#include "libavutil/attributes.h"
#include "libavutil/avassert.h"
#include "libavutil/avstring.h"
#include "libavutil/bswap.h"
#include "libavutil/common.h"
#include "libavutil/dict.h"
#include "libavutil/internal.h"
#include "libavutil/mathematics.h"
#include "libavutil/opt.h"
#include "avformat.h"
#include "avio_internal.h"
#include "avlanguage.h"
#include "id3v2.h"
#include "internal.h"
#include "riff.h"
#include "asf.h"
#include "asfcrypt.h"

#define BMP_HEADER_SIZE 40
#define ASF_TYPE_AUDIO 0x2
#define ASF_TYPE_VIDEO 0x1
#define ASF_STREAM_NUM 0x7F
#define ASF_ERROR_CORRECTION_LENGTH_TYPE 0x60
#define ASF_PACKET_ERROR_CORRECTION_DATA_SIZE 0x2
#define ASF_NUM_OF_PAYLOADS 0x3F

typedef struct GUIDParseTable {
    char name[100];
    ff_asf_guid guid;
    int (*read_object)(AVFormatContext*, struct GUIDParseTable*);
    int is_subobject;
} GUIDParseTable;

typedef struct {
    uint8_t stream_num;
    uint32_t avg_bitrate;
} stream_bitrate;

typedef struct {
    uint8_t stream_index;
    int type;
} ASFStream2;

typedef struct {
    int data_reached;
    int nb_streams;
    uint8_t stream_index;
    int64_t offset; // offset of the current object
    int64_t packet_offset; // offset of the current packet inside Data Object
    uint32_t pad_len; // padding after payload
    uint64_t preroll;
    uint32_t rep_len;
    uint32_t prop_flags; // file properties object flags
    uint64_t num_of_packets;
    uint64_t num_of_packets_left;
    uint64_t num_of_mult_left; // mutiple payloads left
    uint64_t sub_header_offset; // offset of subplayload header
    uint64_t sub_left;  // subpayloads left or not
    int      num_of_sub; // subpayloads get from current ASF packet
    uint16_t mult_sub_len; // total length of subpayloads array inside multiple payload
    uint8_t  pts_delta; // for subpayloads
    uint32_t packet_size;
    uint64_t data_size; // data object size
// presentation time offset, equal for all streams, should be equal to send time, !100-nanosecond units
    uint32_t pts; // presentation time
    uint32_t dts; // send time
    uint64_t time_offset;
    uint16_t bit_num; //number of bitrate records (= number of streams?)
    int nb_packets;
    stream_bitrate *sb;
    ASFStream2 *asf_st[128];
    AVPacket packets_buff[10];
    enum {
        PARSE_PACKET_HEADER,
        READ_SINGLE,
        READ_SUB,
        READ_MULTI,
        READ_MULTI_sub
    } state;
} ASFContext;

static const AVClass asf_class = {
    .class_name = "asf demuxer",
    .version    = LIBAVUTIL_VERSION_INT,
};

static int asf_probe(AVProbeData *pd)
{
    /* check file header */
    if (!ff_guidcmp(pd->buf, &ff_asf_header))
        return AVPROBE_SCORE_MAX;
    else
        return 0;
}

static void swap_guid(ff_asf_guid guid)
{
    FFSWAP(unsigned char, guid[0], guid[3]);
    FFSWAP(unsigned char, guid[1], guid[2]);
    FFSWAP(unsigned char, guid[4], guid[5]);
    FFSWAP(unsigned char, guid[6], guid[7]);
}

static void check_position(AVIOContext *pb,  int64_t offset, uint64_t size)
{

    if (avio_tell(pb) != (offset + size)) {
        printf("Error! Expected position %"PRId64", got %"PRId64". Seeking to the expected postition.\n",
               offset + size, avio_tell(pb));
        avio_seek(pb, offset + size, SEEK_SET);
    }
}


static uint64_t read_obj_size(AVIOContext *pb, GUIDParseTable *g, int64_t offset)
{
    uint64_t size;

    printf("%sASF %s Object, position: %"PRId64" bytes\n",
           g->is_subobject ? "   " : "", g->name, offset);
    size = avio_rl64(pb);
    if (size)
        printf("%sObject size: %"PRIu64" bytes\n", g->is_subobject ? "    " : " ", size);
    else
        printf("    Error: can not read object size\n");

    return size;
}

static int read_unknown(AVFormatContext *s, GUIDParseTable *g)
{
    ASFContext *asf = s->priv_data;
    AVIOContext *pb = s->pb;
    uint64_t size = read_obj_size(pb, g, asf->offset);

    avio_skip(pb, size - 24);

    return 0;
}

// modified GET_STR16 from aviobuf
static int get_asf_string(AVIOContext *pb, int maxlen, char *buf, int buflen)
{
    char* q = buf;
    int ret = 0;
    if (buflen <= 0)
        return AVERROR(EINVAL);
    while (ret + 1 < maxlen) {
        uint8_t tmp;
        uint32_t ch;
        GET_UTF16(ch, (ret += 2) <= maxlen ? avio_rl16(pb) : 0, break;)
        PUT_UTF8(ch, tmp, if (q - buf < buflen - 1) *q++ = tmp;)\
    }
    *q = 0;
    return ret;
}

static int read_unicode(AVIOContext *pb, const char *info)
{
    uint16_t len;

    if (len = avio_rl16(pb)) {
        unsigned char *name;
        len *= 2; // len is number of unicode characters - 2 bytes for each char
        name = av_mallocz(len);
        if (!name)
            return AVERROR(ENOMEM);
        get_asf_string(pb, len, name, len);
        printf("   %s: %s\n", info, name);
        av_free(name);
    }
    return 0;
}

static int read_codec_list(AVFormatContext *s, GUIDParseTable *g)
{
    ASFContext *asf = s->priv_data;
    AVIOContext *pb = s->pb;
    int i, ret;
    uint16_t len, type;
    uint64_t size;
    uint32_t n_codecs;

    size = read_obj_size(pb, g, asf->offset);
    avio_skip(pb, 16); // skip field reserved for guid
    n_codecs = avio_rl32(pb);
    for (i = 0; i < n_codecs; i++) {
        type = avio_rl16(pb);
        if (type == ASF_TYPE_VIDEO)
            printf("   Codec type: Video codec\n");
        else if (type == ASF_TYPE_AUDIO)
            printf("   Codec type: Audio codec\n");
        else
            printf("   Codec type: Unknown codec\n");
        if ((ret = read_unicode(pb, "Codec name")) < 0)
            return ret;
        if ((ret = read_unicode(pb, "Codec description")) < 0)
            return ret;
        if (len = avio_rl16(pb)) {
            unsigned char *in;
            in = av_mallocz(len);
            if (!in)
                return AVERROR(ENOMEM);
            if ((ret = avio_read(pb, in, len)) < 0) {
                av_free(in);
                return ret;
            }
            printf("   Codec information: %s\n", in);
            av_free(in);
        }
    }
    check_position(pb, asf->offset, size);

    return 0;
}

static int read_metadata(AVIOContext *pb, const char *title, uint16_t len, unsigned char *ch, uint16_t max_len)
{
    printf("   %s: ", title);
    memset(ch, 0, max_len);
    avio_get_str16le(pb, len * sizeof(*ch), ch, len * sizeof(*ch));
    printf("   %s\n", ch);

    return 0;
}

static int read_content(AVFormatContext *s, GUIDParseTable *g)
{
    ASFContext *asf = s->priv_data;
    AVIOContext *pb = s->pb;
    int i;
    uint16_t len[5], max_len = 0;
    const char *titles[] = { "Title", "Author", "Copyright", "Description", "Rate" };
    unsigned char *ch;
    uint64_t size = read_obj_size(pb, g, asf->offset);

    for (i = 0; i < 5; i++) {
        len[i] = avio_rl16(pb);
        max_len = FFMAX(max_len, len[i]);
    }
    ch = av_mallocz(max_len * sizeof(*ch));
    if (!ch)
        return(AVERROR(ENOMEM));
    for (i = 0; i < 5; i++)
        read_metadata(pb, titles[i], len[i], ch, max_len);

    av_free(ch);
    check_position(pb, asf->offset, size);

    return 0;
}

static int read_properties(AVFormatContext *s, GUIDParseTable *g)
{
    ASFContext *asf = s->priv_data;
    AVIOContext *pb = s->pb;
    uint64_t size, num_of_packets;
    uint32_t min_packet_size, max_packet_size, max_bitrate;

    size = read_obj_size(pb, g, asf->offset);
    avio_skip(pb, 16); // skip File ID
    size = avio_rl64(pb); //File size
    avio_skip(pb, 8); // skip creation date
    num_of_packets = avio_rl64(pb);
    avio_skip(pb, 16); //skip play and send duration
    asf->preroll    = avio_rl64(pb);
    asf->prop_flags = avio_rl32(pb);
    min_packet_size = avio_rl32(pb);
    max_packet_size = avio_rl32(pb);
    max_bitrate     = avio_rl32(pb);
    asf->num_of_packets = num_of_packets;
    asf->packet_size    = max_packet_size;

    return 0;
}

static int parse_video_info(AVIOContext *pb, AVStream *st)
{
    uint16_t size;

    st->codec->width  = avio_rl32(pb);
    st->codec->height = avio_rl32(pb);
    avio_skip(pb, 1); //skip reserved flags
    size = avio_rl16(pb); // size of the Format Data
    ff_get_bmp_header(pb, st);
    if (size > BMP_HEADER_SIZE) {
        int ret;
        st->codec->extradata_size = size - BMP_HEADER_SIZE;
        st->codec->extradata      = av_mallocz(st->codec->extradata_size +
                                               FF_INPUT_BUFFER_PADDING_SIZE);
        if ((ret = avio_read(pb, st->codec->extradata, st->codec->extradata_size)) < 0)
            return ret;
    }
    return 0;
}

static int read_stream_properties(AVFormatContext *s, GUIDParseTable *g)
{
    ASFContext *asf = s->priv_data;
    AVIOContext *pb = s->pb;
    uint64_t size;
    uint32_t err_data_len, ts_data_len; // time specific data length
    uint16_t flags;
    int8_t *ts_data, *err_data;
    ff_asf_guid stream_type;
    enum AVMediaType type;
    int ret;
    AVStream *st = avformat_new_stream(s, NULL);

    if (!st)
        return AVERROR(ENOMEM);
    avpriv_set_pts_info(st, 32, 1, 1000); // pts should be dword, in milliseconds
    size = read_obj_size(pb, g, asf->offset);
    ff_get_guid(pb, &stream_type);
    if (!ff_guidcmp(&stream_type, &ff_asf_audio_stream))
        type = AVMEDIA_TYPE_AUDIO;
    else if (!ff_guidcmp(&stream_type, &ff_asf_video_stream))
        type = AVMEDIA_TYPE_VIDEO;
    else if (!ff_guidcmp(&stream_type, &ff_asf_jfif_media)) {
        type                = AVMEDIA_TYPE_VIDEO;
        st->codec->codec_id = AV_CODEC_ID_MJPEG;
    } else if (!ff_guidcmp(&stream_type, &ff_asf_command_stream))
        type = AVMEDIA_TYPE_DATA;
    else if (!ff_guidcmp(&stream_type, &ff_asf_ext_stream_embed_stream_header)) { // do ext stream case later
        type = AVMEDIA_TYPE_UNKNOWN;
    } else
        return AVERROR_INVALIDDATA;
    st->codec->codec_type = type;
    ff_get_guid(pb, &stream_type); // error correction type
    asf->time_offset = avio_rl64(pb);
    ts_data_len      = avio_rl32(pb);
    err_data_len     = avio_rl32(pb);
    flags            = avio_rl16(pb); // bit 15 - Encrypted Content
    st->id = flags & ASF_STREAM_NUM;
    asf->nb_streams++;
    asf->asf_st[asf->nb_streams - 1] = av_mallocz(sizeof(ASFStream2*));
    asf->asf_st[asf->nb_streams - 1]->stream_index = st->id;
    avio_skip(pb, 4); // skip reserved field
    if (ts_data_len) {
        ts_data = av_mallocz(ts_data_len);
        if (!ts_data)
            return AVERROR(ENOMEM);
        switch (type) {
        case AVMEDIA_TYPE_AUDIO:
            printf("audio stream\n");
            ret = ff_get_wav_header(pb, st->codec, ts_data_len);
            if (ret < 0) {
                av_free(ts_data);
                return ret;
            }
            break;
        case AVMEDIA_TYPE_VIDEO:
            printf("video stream\n");
            ret = parse_video_info(pb, st);
            if (ret < 0) {
                av_free(ts_data);
                return ret;
            }
            break;
        default:
            if ((ret = avio_read(pb, ts_data, ts_data_len)) < 0) {
                av_free(ts_data);
                return ret;
            }
            break;
        }
        av_free(ts_data);
    } else
        return AVERROR_INVALIDDATA;
    if (err_data_len) {
        err_data = av_mallocz(err_data_len);
        if (!err_data)
            return AVERROR(ENOMEM);
        if ((ret = avio_read(pb, err_data, err_data_len)) < 0) {
            av_free(err_data);
            return ret;
        }
        av_free(err_data);
    }
    check_position(pb, asf->offset, size);
    //printf("nb_streams %d\n", s->nb_streams);

    return 0;
}

static int read_stream_bitrate(AVFormatContext *s, GUIDParseTable *g)
{
    ASFContext *asf = s->priv_data;
    AVIOContext *pb = s->pb;
    int i;
    uint16_t flags;
    uint64_t size = read_obj_size(pb, g, asf->offset);
    stream_bitrate *sb;

    asf->bit_num = avio_rl16(pb); // number of records
    sb = av_mallocz(asf->bit_num * sizeof(stream_bitrate*));
    for (i = 0; i < asf->bit_num; i++) {
        flags = avio_rl16(pb);
        sb[i].stream_num  = flags & ASF_STREAM_NUM;
        sb[i].avg_bitrate = avio_rl32(pb);
    }
    asf->sb = sb;

    check_position(pb, asf->offset, size);
    return 0;
}

static int read_data(AVFormatContext *s, GUIDParseTable *g)
{
    ASFContext *asf = s->priv_data;
    AVIOContext *pb = s->pb;
    stream_bitrate *sb = asf->sb;
    uint64_t size;
    int i, j;

    asf->data_size = read_obj_size(pb, g, asf->offset);
    asf->num_of_packets_left = asf->num_of_packets;
    asf->num_of_mult_left = 0;
    asf->nb_packets       = 0;
    asf->sub_left         = 0;
    asf->data_reached     = 1;
    asf->state            = PARSE_PACKET_HEADER;
    for (i = 0; i < s->nb_streams; i++)
        for (j = 1; j <= asf->bit_num; j++)
            if (s->streams[i]->id == asf->sb[j].stream_num)
                s->streams[i]->codec->bit_rate = asf->sb[j].avg_bitrate;
    avio_skip(pb, 16); // skip File ID
    size = avio_rl64(pb); // Total Data Packets
    printf("   Number of Data packets %"PRIu64"\n", size);
    if (size != asf->num_of_packets)
        av_log(NULL, AV_LOG_WARNING, "Number of Packets from File Properties Object is not equal to Total Data packets value!.\n");
    avio_skip(pb, 2); // skip reserved field

    av_freep(&sb);
    return 0;
}

GUIDParseTable gdef[] = {
    {"Data",                         {0x75, 0xB2, 0x26, 0x36, 0x66, 0x8E, 0x11, 0xCF, 0xA6, 0xD9, 0x00, 0xAA, 0x00, 0x62, 0xCE, 0x6C}, read_data, 0},
    {"Simple Index",                 {0x33, 0x00, 0x08, 0x90, 0xE5, 0xB1, 0x11, 0xCF, 0x89, 0xF4, 0x00, 0xA0, 0xC9, 0x03, 0x49, 0xCB}, read_unknown, 0},
    {"Content Description",          {0x75, 0xB2, 0x26, 0x33, 0x66 ,0x8E, 0x11, 0xCF, 0xA6, 0xD9, 0x00, 0xAA, 0x00, 0x62, 0xCE, 0x6C}, read_content, 1},
    {"Extended Content Description", {0xD2, 0xD0, 0xA4, 0x40, 0xE3, 0x07, 0x11, 0xD2, 0x97, 0xF0, 0x00, 0xA0, 0xC9, 0x5e, 0xA8, 0x50}, read_unknown, 1},
    {"Stream Bitrate Properties",    {0x7B, 0xF8, 0x75, 0xCE, 0x46, 0x8D, 0x11, 0xD1, 0x8D, 0x82, 0x00, 0x60, 0x97, 0xC9, 0xA2, 0xB2}, read_stream_bitrate, 1},
    {"File Properties",              {0x8C, 0xAB, 0xDC, 0xA1, 0xA9, 0x47, 0x11, 0xCF, 0x8E, 0xE4, 0x00, 0xC0, 0x0C, 0x20, 0x53, 0x65}, read_properties, 1},
    {"Header Extension",             {0x5F, 0xBF, 0x03, 0xB5, 0xA9, 0x2E, 0x11, 0xCF, 0x8E, 0xE3, 0x00, 0xC0, 0x0C, 0x20, 0x53, 0x65}, read_unknown, 1},
    {"Stream Properties",            {0xB7, 0xDC, 0x07, 0x91, 0xA9, 0xB7, 0x11, 0xCF, 0x8E, 0xE6, 0x00, 0xC0, 0x0C, 0x20, 0x53, 0x65},
    read_stream_properties, 1},
    {"Codec List",                   {0x86, 0xD1, 0x52, 0x40, 0x31, 0x1D, 0x11, 0xD0, 0xA3, 0xA4, 0x00, 0xA0, 0xC9, 0x03, 0x48, 0xF6}, read_codec_list, 1},
    {"Marker",                       {0xF4, 0x87, 0xCD, 0x01, 0xA9, 0x51, 0x11, 0xCF, 0x8E, 0xE6, 0x00, 0xC0, 0x0C, 0x20, 0x53, 0x65}, read_unknown, 1},
    {"Script Command",               {0x1E, 0xFB, 0x1A, 0x30, 0x0B, 0x62, 0x11, 0xD0, 0xA3, 0x9B, 0x00, 0xA0, 0xC9, 0x03, 0x48, 0xF6}, read_unknown, 1},
    {"Language List",                {0x7C, 0x43, 0x46, 0xa9, 0xef, 0xe0, 0x4B, 0xFC, 0xB2, 0x29, 0x39, 0x3e, 0xde, 0x41, 0x5c, 0x85}, read_unknown, 1},
    {"Padding",                      {0x18, 0x06, 0xD4, 0x74, 0xCA, 0xDF, 0x45, 0x09, 0xA4, 0xBA, 0x9A, 0xAB, 0xCB, 0x96, 0xAA, 0xE8}, read_unknown, 1},
    {"DRMv1 Header",                 {0x22, 0x11, 0xB3, 0xFB, 0xBD, 0x23, 0x11, 0xD2, 0xB4, 0xB7, 0x00, 0xA0, 0xC9, 0x55, 0xFC, 0x6E}, read_unknown, 1},
    {"DRMv2 Header",                 {0x29, 0x8A, 0xE6, 0x14, 0x26, 0x22, 0x4C, 0x17, 0xB9, 0x35, 0xDA, 0xE0, 0x7E, 0xE9, 0x28, 0x9c}, read_unknown, 1},
    {"Index",                        {0xD6, 0xE2, 0x29, 0xD3, 0x35, 0xDA, 0x11, 0xD1, 0x90, 0x34, 0x00, 0xA0, 0xC9, 0x03, 0x49, 0xBE}, read_unknown, 0},
    {"Media Object Index",           {0xFE, 0xB1, 0x03, 0xF8, 0x12, 0xAD, 0x4C, 0x64, 0x84, 0x0F, 0x2A, 0x1D, 0x2F, 0x7A, 0xD4, 0x8C}, read_unknown, 0},
    {"Timecode Index",               {0x3C, 0xB7, 0x3F, 0xD0, 0x0C, 0x4A, 0x48, 0x03, 0x95, 0x3D, 0xED, 0xF7, 0xB6, 0x22, 0x8F, 0x0C}, read_unknown, 0},
    {"Bitrate_Mutual_Exclusion",     {0xD6, 0xE2, 0x29, 0xDC, 0x35, 0xDA, 0x11, 0xD1, 0x90, 0x34, 0x00, 0xA0, 0xC9, 0x03, 0x49, 0xBE}, read_unknown, 1},
    {"Error Correction",             {0x75, 0xB2, 0x26, 0x35, 0x66, 0x8E, 0x11, 0xCF, 0xA6, 0xD9, 0x00, 0xAA, 0x00, 0x62, 0xCE, 0x6C}, read_unknown, 1},
    {"Content Branding",             {0x22, 0x11, 0xB3, 0xFA, 0xBD, 0x23, 0x11, 0xD2, 0xB4, 0xB7, 0x00, 0xA0, 0xC9, 0x55, 0xFC, 0x6E}, read_unknown, 1},
    {"Content Encryption",           {0x22, 0x11, 0xB3, 0xFB, 0xBD, 0x23, 0x11, 0xD2, 0xB4, 0xB7, 0x00, 0xA0, 0xC9, 0x55, 0xFC, 0x6E}, read_unknown, 1},
    {"Extended Content Encryption",  {0x29, 0x8A, 0xE6, 0x14, 0x26, 0x22, 0x4C, 0x17, 0xB9, 0x35, 0xDA, 0xE0, 0x7E, 0xE9, 0x28, 0x9C}, read_unknown, 1},
    {"Digital Signature",            {0x22, 0x11, 0xB3, 0xFC, 0xBD, 0x23, 0x11, 0xD2, 0xB4, 0xB7, 0x00, 0xA0, 0xC9, 0x55, 0xFC, 0x6E}, read_unknown, 1},
    {"Extended Stream Properties",   {0x14, 0xE6, 0xA5, 0xCB, 0xC6, 0x72, 0x43, 0x32, 0x83, 0x99, 0xA9, 0x69, 0x52, 0x06, 0x5B, 0x5A}, read_unknown, 1},
    {"Advanced Mutual Exclusion",    {0xA0, 0x86, 0x49, 0xCF, 0x47, 0x75, 0x46, 0x70, 0x8A, 0x16, 0x6E, 0x35, 0x35, 0x75, 0x66, 0xCD}, read_unknown, 1},
    {"Group Mutual Exclusion",       {0xD1, 0x46, 0x5A, 0x40, 0x5A, 0x79, 0x43, 0x38, 0xB7, 0x1B, 0xE3, 0x6B, 0x8F, 0xD6, 0xC2, 0x49}, read_unknown, 1},
    {"Stream Prioritization",        {0xD4, 0xFE, 0xD1, 0x5B, 0x88, 0xD3, 0x45, 0x4F, 0x81, 0xF0, 0xED, 0x5C, 0x45, 0x99, 0x9E, 0x24}, read_unknown, 1},
    {"Bandwidth Sharing Object",     {0xA6, 0x96, 0x09, 0xE6, 0x51, 0x7B, 0x11, 0xD2, 0xB6, 0xAF, 0x00, 0xC0, 0x4F, 0xD9, 0x08, 0xE9}, read_unknown, 1},
    {"Metadata",                     {0xC5, 0xF8, 0xCB, 0xEA, 0x5B, 0xAF, 0x48, 0x77, 0x84, 0x67, 0xAA, 0x8C, 0x44, 0xFA, 0x4C, 0xCA}, read_unknown, 1},
    {"Audio Spread",                 {0xBF, 0xC3, 0xCD, 0x50, 0x61, 0x8F, 0x11, 0xCF, 0x8B, 0xB2, 0x00, 0xAA, 0x00, 0xB4, 0xE2, 0x20}, read_unknown, 1},
    {"Content Encryption System Windows Media DRM Network Devices",
                                     {0x7A, 0x07, 0x9B, 0xB6, 0xDA, 0XA4, 0x4e, 0x12, 0xA5, 0xCA, 0x91, 0xD3, 0x8D, 0xC1, 0x1A, 0x8D}, read_unknown, 1},
    {"Mutex Language",               {0xD6, 0xE2, 0x2A, 0x00, 0x25, 0xDA, 0x11, 0xD1, 0x90, 0x34, 0x00, 0xA0, 0xC9, 0x03, 0x49, 0xBE}, read_unknown, 1},
    {"Mutex Bitrate",                {0xD6, 0xE2, 0x2A, 0x01, 0x25, 0xDA, 0x11, 0xD1, 0x90, 0x34, 0x00, 0xA0, 0xC9, 0x03, 0x49, 0xBE}, read_unknown, 1},
    {"Mutex Unknown",                {0xD6, 0xE2, 0x2A, 0x02, 0x25, 0xDA, 0x11, 0xD1, 0x90, 0x34, 0x00, 0xA0, 0xC9, 0x03, 0x49, 0xBE}, read_unknown, 1},
    {"Bandwith Sharing Exclusive",   {0xAF, 0x60, 0x60, 0xAA, 0x51, 0x97, 0x11, 0xD2, 0xB6, 0xAF, 0x00, 0xC0, 0x4F, 0xD9, 0x08, 0xE9}, read_unknown, 1},
    {"Bandwith Sharing Partial",     {0xAF, 0x60, 0x60, 0xAB, 0x51, 0x97, 0x11, 0xD2, 0xB6, 0xAF, 0x00, 0xC0, 0x4F, 0xD9, 0x08, 0xE9}, read_unknown, 1},
    {"Payload Extension System Timecode", {0x39, 0x95, 0x95, 0xEC, 0x86, 0x67, 0x4E, 0x2D, 0x8F, 0xDB, 0x98, 0x81, 0x4C, 0xE7, 0x6C, 0x1E}, read_unknown, 1},
    {"Payload Extension System File Name", {0xE1, 0x65, 0xEC, 0x0E, 0x19, 0xED, 0x45, 0xD7, 0xB4, 0xA7, 0x25, 0xCB, 0xD1, 0xE2, 0x8E, 0x9B}, read_unknown, 1},
    {"Payload Extension System Content Type", {0xD5, 0x90, 0xDC, 0x20, 0x07, 0xBC, 0x43, 0x6C, 0x9C, 0xF7, 0xF3, 0xBB, 0xFB, 0xF1, 0xA4, 0xDC}, read_unknown, 1},
    {"Payload Extension System Pixel Aspect Ratio", {0x1, 0x1E, 0xE5, 0x54, 0xF9, 0xEA, 0x4B, 0xC8, 0x82, 0x1A, 0x37, 0x6B, 0x74, 0xE4, 0xC4, 0xB8}, read_unknown, 1},
    {"Payload Extension System Sample Duration", {0xC6, 0xBD, 0x94, 0x50, 0x86, 0x7F, 0x49, 0x07, 0x83, 0xA3, 0xC7, 0x79, 0x21, 0xB7, 0x33, 0xAD}, read_unknown, 1},
    {"Payload Extension System Encryption Sample ID", {0x66, 0x98, 0xB8, 0x4E, 0x0A, 0xFA, 0x43, 0x30, 0xAE, 0xB2, 0x1C, 0x0A, 0x98, 0xD7, 0xA4, 0x4D}, read_unknown, 1},
    {"Payload Extension System Degradable JPEG", {0x00, 0xE1, 0xAF, 0x06, 0x7B, 0xEC, 0x11, 0xD1, 0xA5, 0x82, 0x00, 0xC0, 0x4F, 0xC2, 0x9C, 0xFB}, read_unknown, 1},

};

#define read_length(flag, name, len)       \
    do {                                   \
        if (flag == name ## IS_BYTE)       \
            len = avio_r8(pb);             \
        else if (flag == name ## IS_WORD)  \
            len = avio_rl16(pb);           \
        else if (flag == name ## IS_DWORD) \
            len = avio_rl32(pb);           \
        else                               \
            len = 0;                       \
    } while(0)

static int read_subpayload(AVFormatContext *s, AVPacket *pkt, int is_header)
{
    ASFContext *asf = s->priv_data;
    AVIOContext *pb = s->pb;
    uint8_t sub_len;
    int64_t sub_offset;
    int ret;

    if (is_header) {
        printf("    Sub-Payloads present.\n");
        asf->pts_delta = avio_r8(pb);
        if (asf->num_of_mult_left)
            asf->mult_sub_len = avio_rl16(pb); // total
        asf->sub_header_offset = avio_tell(pb);
        asf->num_of_sub = 0;
    }
    sub_len = avio_r8(pb);
    if (!sub_len)
        return AVERROR_INVALIDDATA;
    sub_offset = avio_tell(pb);
    if ((ret = av_get_packet(pb, pkt, sub_len)) < 0)
        return ret;
    printf("     Sub-Payload at position %"PRId64" of length %"PRIu8" pos %d\n",
           sub_offset, sub_len, avio_tell(pb));
    asf->num_of_sub++;
    pkt->pts = asf->pts + asf->num_of_sub * asf->pts_delta;
    if (asf->num_of_mult_left && (avio_tell(pb) >= (asf->sub_header_offset + asf->mult_sub_len))) {
        asf->sub_left = 0;
        asf->num_of_mult_left--;
    }
    printf("sub offset %d, if %d, asf->packet_offset \n", sub_offset, asf->packet_offset + asf->packet_size - asf->pad_len - sub_len - asf->pad_len);
    if (avio_tell(pb) >= (asf->packet_offset + asf->packet_size - asf->pad_len - asf->rep_len - sub_len)) {
        asf->sub_left = 0;
        if (!asf->num_of_mult_left) {
            avio_skip(pb, asf->pad_len);
            if (avio_tell(pb) != asf->packet_offset + asf->packet_size)
                avio_seek(pb, asf->packet_offset + asf->packet_size, SEEK_SET);
        }
    }

    return 0;
}

static int read_payload(AVFormatContext *s, AVPacket *pkt)
{
    ASFContext *asf = s->priv_data;
    AVIOContext *pb = s->pb;
    uint8_t stream_num;
    uint64_t size;
    int64_t  offset;
    uint32_t off_len, media_len;
    uint16_t pay_len;
    int ret;

    if (!asf->sub_left) {
        stream_num = avio_r8(pb);
        asf->stream_index = stream_num & ASF_STREAM_NUM;
        //printf("header: stream_index %d\n", asf->stream_index);
        if (stream_num >> 7)
            pkt->flags |= AV_PKT_FLAG_KEY;
        read_length((asf->prop_flags & ASF_PL_MASK_MEDIA_OBJECT_NUMBER_LENGTH_FIELD_SIZE),
                    ASF_PL_FLAG_MEDIA_OBJECT_NUMBER_LENGTH_FIELD_, media_len);
        read_length((asf->prop_flags & ASF_PL_MASK_OFFSET_INTO_MEDIA_OBJECT_LENGTH_FIELD_SIZE),
                    ASF_PL_FLAG_OFFSET_INTO_MEDIA_OBJECT_LENGTH_FIELD_, off_len);
        read_length((asf->prop_flags & ASF_PL_MASK_REPLICATED_DATA_LENGTH_FIELD_SIZE),
                    ASF_PL_FLAG_REPLICATED_DATA_LENGTH_FIELD_, asf->rep_len);
        printf("     Media Object Number %"PRIu32", Offset Into Media Object %"PRIu32"\n",
               media_len, off_len);
        if (asf->num_of_mult_left) {
            //printf("if mult left %d\n", asf->num_of_mult_left);
            if (asf->rep_len == 1) {
                asf->sub_left = 1;
                asf->state = READ_MULTI_sub;
                if ((ret = read_subpayload(s, pkt, 1)) < 0)
                    return ret;
            } else {
                avio_skip(pb, 4); // skip media object size
                asf->pts = avio_rl32(pb); // read presentation time
                if ((asf->rep_len - 8) > 0)
                    avio_skip(pb, asf->rep_len - 8); // skip replicated data
                pay_len = avio_rl16(pb); // payload length should be WORD
                offset = avio_tell(pb);
                printf("     packet for stream #%"PRIu8" of size %"PRIu16" at position %"PRId64"\n",
                       asf->stream_index, pay_len, offset);
                if (pay_len > asf->packet_size) {
                    av_log(NULL, AV_LOG_ERROR, "Error: invalid data packet size.\n");
                    return AVERROR_INVALIDDATA;
                }
                if ((ret = av_get_packet(pb, pkt, pay_len)) < 0)
                    return ret;
                asf->num_of_mult_left--;
                pkt->pts = asf->pts;

            }
        } else if (asf->rep_len == 1) {
            asf->sub_left = 1;
            asf->state    = READ_SUB;
            if ((ret = read_subpayload(s, pkt, 1)) < 0)
                return ret;
        } else {
            //printf("offset %d\n", avio_tell(pb));
            avio_skip(pb, 4); // skip media object size
            asf->pts = avio_rl32(pb); // read presentation time
            if ((asf->rep_len - 8) > 0)
                avio_skip(pb, asf->rep_len - 8); // skip replicated data
            offset = avio_tell(pb);
            // size of the payload - size of the packet without header, replicated data and padding
            size = asf->packet_size - offset + asf->packet_offset - asf->pad_len;
            if (size > asf->packet_size) {
                av_log(NULL, AV_LOG_ERROR, "Error: invalid data packet size.\n");
                return AVERROR_INVALIDDATA;
            }
            printf("     packet for stream #%"PRIu8" of size %"PRIu64" at position %"PRId64"\n",
                   asf->stream_index, size, offset);
            if ((ret = av_get_packet(pb, pkt, size)) < 0)
                return ret;
            pkt->pts = asf->pts;
            avio_skip(pb, asf->pad_len); // skip padding
        }
    } else
        if ((ret = read_subpayload(s, pkt, 0)) < 0) // read subpayload without its header
            return ret;

    return 0;
}

static int read_packet_header(AVFormatContext *s)
{
    ASFContext *asf = s->priv_data;
    AVIOContext *pb = s->pb;
    uint64_t size;
    uint32_t packet_len, seq, pts;
    unsigned char error_flags, len_flags, pay_flags;

    asf->packet_offset = avio_tell(pb);
    error_flags = avio_r8(pb); // read Error Correction Flags
    if (error_flags & ASF_PACKET_FLAG_ERROR_CORRECTION_PRESENT)
        if (!(error_flags & ASF_ERROR_CORRECTION_LENGTH_TYPE)) {
            size = error_flags & ASF_PACKET_ERROR_CORRECTION_DATA_SIZE;
            avio_skip(pb, size);
        }
    len_flags = avio_r8(pb);
    asf->prop_flags = avio_r8(pb);
    read_length((len_flags & ASF_PPI_MASK_PACKET_LENGTH_FIELD_SIZE),
                ASF_PPI_FLAG_PACKET_LENGTH_FIELD_, packet_len);
    read_length((len_flags & ASF_PPI_MASK_SEQUENCE_FIELD_SIZE),
                ASF_PPI_FLAG_SEQUENCE_FIELD_, seq);
    read_length((len_flags & ASF_PPI_MASK_PADDING_LENGTH_FIELD_SIZE),
                ASF_PPI_FLAG_PADDING_LENGTH_FIELD_, asf->pad_len );
    asf->dts = avio_rl32(pb); // send time
    avio_skip(pb, 2); // skip duration
    if (len_flags & ASF_PPI_FLAG_MULTIPLE_PAYLOADS_PRESENT) { // Multiple Payloads present
        pay_flags = avio_r8(pb);
        asf->num_of_mult_left = (pay_flags & ASF_NUM_OF_PAYLOADS);
        printf("Multiple payload at one packet present: %d payloads\n", asf->num_of_mult_left + 1);
    }

    return 0;
}

static int asf_read_packet(AVFormatContext *s, AVPacket *pkt)
{
    ASFContext *asf = s->priv_data;
    AVIOContext *pb = s->pb;
    int ret, i, j;

    if (asf->state == PARSE_PACKET_HEADER) {
        if (!asf->num_of_packets_left) {
            ret = AVERROR(EOF);
            goto fail;
        }
        read_packet_header(s);
        if (!asf->num_of_mult_left)
            asf->state = READ_SINGLE;
        else
            asf->state = READ_MULTI;
    }
    switch (asf->state) {
    case READ_SINGLE:
        if ((ret = read_payload(s, pkt)) < 0)
            return ret;
        asf->num_of_packets_left--;
        asf->state = PARSE_PACKET_HEADER;
        break;
    case READ_SUB:
        if ((ret = read_payload(s, pkt)) < 0)
            return ret;
        if (!asf->sub_left) {
            asf->num_of_packets_left--;
            asf->state = PARSE_PACKET_HEADER;
        }
        break;
    case READ_MULTI_sub:
        if ((ret = read_payload(s, pkt)) < 0)
            return ret;
        if (!asf->sub_left && !asf->num_of_mult_left) {
            asf->num_of_packets_left--;
            asf->state = PARSE_PACKET_HEADER;
            avio_skip(pb, asf->pad_len); // skip padding
        } else if (!asf->sub_left)
            asf->state = READ_MULTI;
        break;
    case READ_MULTI:
        if ((ret = read_payload(s, pkt)) < 0)
            return ret;
        if (!asf->num_of_mult_left) {
            asf->num_of_packets_left--;
            asf->state = PARSE_PACKET_HEADER;
            avio_skip(pb, asf->pad_len); // skip padding
        }
        break;
    }

    asf->nb_packets++;
    for (i = 0; i < s->nb_streams; i++)
        for (j = 0; j < asf->nb_streams; j++)
        if (s->streams[i]->id == asf->asf_st[j]->stream_index)
            pkt->stream_index = i;

    //printf("num of packets left %d, stream index %d, nb_packets %d\n", asf->num_of_packets_left, pkt->stream_index, asf->nb_packets);
    return 0;

fail:
    for (i = 0; i < asf->nb_streams; i++)
        av_free(asf->asf_st[i]);
    return ret;
}

static GUIDParseTable *find_guid(ff_asf_guid guid, GUIDParseTable *g)
{
    int j, ret;

    g = gdef;
    for (j = 0; j < FF_ARRAY_ELEMS(g->guid); j++) {
        if (!(ret = memcmp(guid, g->guid, sizeof(g->guid))))
            return g;
        g++;
    }
    return NULL;
}

static int asf_read_header(AVFormatContext *s)
{
    ASFContext *asf = s->priv_data;
    AVIOContext *pb = s->pb;
    GUIDParseTable *g = NULL;
    ff_asf_guid guid;
    int ret;
    int64_t size;

    ff_get_guid(pb, &guid); // return avio_read
    if (ff_guidcmp(&guid, &ff_asf_header))
        return AVERROR_INVALIDDATA;
    size = avio_rl64(pb); // header object size
    printf("Header Object detected, size %d\n", size);
    avio_skip(pb, 6); // skip number of header objects and 2 reserved bytes
    asf->data_reached = 0;
    while (1) {
        asf->offset = avio_tell(pb);
        if ((ret = ff_get_guid(pb, &guid)) < 0)
            return ret;
        swap_guid(&guid);
        g = find_guid(&guid, g);
        if (g) {
            if ((ret = g->read_object(s, g)) < 0) {
                printf(" read object ret %d\n", ret);
                return ret;
            }
            if (asf->data_reached) {
                printf("data detected\n");
                break;
            }
        }
    }
    if (asf->data_reached) {
        asf->nb_streams = 0;

    } else
        av_freep(&asf->sb);

    return 0;
}

AVInputFormat ff_asf_demuxer = {
    .name           = "asf2",
    .long_name      = NULL_IF_CONFIG_SMALL("ASF (Advanced / Active Streaming Format)"),
    .priv_data_size = sizeof(ASFContext),
    .read_probe     = asf_probe,
    .read_header    = asf_read_header,
    .read_packet    = asf_read_packet,
    .flags          = AVFMT_NOBINSEARCH | AVFMT_NOGENSEARCH,
    .priv_class     = &asf_class,
};
