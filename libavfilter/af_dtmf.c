/*
 * Code imported from multimon demod_dtmf.c
 *
 * This file is part of Libav.
 *
 * Libav is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * Libav is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Libav; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * sample format and channel layout conversion audio filter
 */

#include "libavutil/avassert.h"
#include "libavutil/common.h"
#include "libavutil/mathematics.h"
#include "libavutil/channel_layout.h"

#include "audio.h"
#include "avfilter.h"
#include "formats.h"
#include "internal.h"

/*
 *
 * DTMF frequencies
 *
 *      1209 1336 1477 1633
 *  697   1    2    3    A
 *  770   4    5    6    B
 *  852   7    8    9    C
 *  941   *    0    #    D
 *
 */

#define SAMPLE_RATE 22050
#define BLOCKLEN (SAMPLE_RATE/100)  /* 10ms blocks */
#define BLOCKNUM 4

typedef struct DTMFContext {
    unsigned int ph[8];
    float energy[BLOCKNUM];
    float tenergy[BLOCKNUM][16];
    int blkcount;
    int lastch;
} DTMFContext;


#define PHINC(x) ((x) * 0x10000 / SAMPLE_RATE)

static const char *dtmf_transl = "123A456B789C*0#D";

static const unsigned int dtmf_phinc[8] = {
    PHINC(1209), PHINC(1336), PHINC(1477), PHINC(1633),
    PHINC(697), PHINC(770), PHINC(852), PHINC(941)
};

static int find_max_idx(const float *f)
{
    float en = 0;
    int idx = -1, i;

    for (i = 0; i < 4; i++)
        if (f[i] > en) {
            en = f[i];
            idx = i;
        }
    if (idx < 0)
        return -1;
    en *= 0.1;
    for (i = 0; i < 4; i++)
        if (idx != i && f[i] > en)
            return -1;
    return idx;
}

inline float fsqr(float f)
{
        return f*f;
}

static inline int process_block(DTMFContext *dtmf)
{
    float tote;
    float totte[16];
    int i, j;

    tote = 0;
    for (i = 0; i < BLOCKNUM; i++)
        tote += dtmf->energy[i];
    for (i = 0; i < 16; i++) {
        totte[i] = 0;
        for (j = 0; j < BLOCKNUM; j++)
            totte[i] += dtmf->tenergy[j][i];
    }
    for (i = 0; i < 8; i++)
        totte[i] = fsqr(totte[i]) + fsqr(totte[i + 8]);
    memmove(dtmf->energy + 1, dtmf->energy,
        sizeof(dtmf->energy) - sizeof(dtmf->energy[0]));
    dtmf->energy[0] = 0;
    memmove(dtmf->tenergy + 1, dtmf->tenergy,
        sizeof(dtmf->tenergy) - sizeof(dtmf->tenergy[0]));
    memset(dtmf->tenergy, 0, sizeof(dtmf->tenergy[0]));
    tote *= (BLOCKNUM * BLOCKLEN * 0.5);  /* adjust for block lengths */
    av_log(NULL, AV_LOG_VERBOSE,
           "DTMF: Energies: "
           "%8.5f  %8.5f %8.5f %8.5f %8.5f  %8.5f %8.5f %8.5f %8.5f\n",
           tote, totte[0], totte[1], totte[2], totte[3],
                 totte[4], totte[5], totte[6], totte[7]);
    if ((i = find_max_idx(totte)) < 0)
        return -1;
    if ((j = find_max_idx(totte+4)) < 0)
        return -1;
    if ((tote * 0.4) > (totte[i] + totte[j+4]))
        return -1;
    return (i & 3) | ((j << 2) & 0xc);
}

#define COS(x) \
    cos(M_PI * 2.0 * (((x)>>6)&0x3ffu) / 0x400)

#define SIN(x) COS((x)+0xc000)


static void dtmf_guess(DTMFContext *dtmf, float *buffer, int length)
{
    float s_in;
    int i;
    for (; length > 0; length--, buffer++) {
        s_in = (*buffer);
        dtmf->energy[0] += fsqr(s_in);
        for (i = 0; i < 8; i++) {
            dtmf->tenergy[0][i]   += COS(dtmf->ph[i]) * s_in;
            dtmf->tenergy[0][i+8] += SIN(dtmf->ph[i]) * s_in;
            dtmf->ph[i] += dtmf_phinc[i];
        }
        if ((dtmf->blkcount--) <= 0) {
            dtmf->blkcount = BLOCKLEN;
            i = process_block(dtmf);
            if (i != dtmf->lastch && i >= 0)
                av_log(NULL, AV_LOG_INFO, "DTMF: %c\n", dtmf_transl[i]);
            dtmf->lastch = i;
        }
    }
}

static av_cold void uninit(AVFilterContext *ctx)
{

}

static int query_formats(AVFilterContext *ctx)
{
/*
    AVFilterLink *inlink  = ctx->inputs[0];
    AVFilterLink *outlink = ctx->outputs[0];
*/
    AVFilterChannelLayouts *channel_layout = NULL;
    const int format[]         = {AV_SAMPLE_FMT_FLT, -1};
    const int samplerate[]     = {22050, -1};

    ff_set_common_formats(ctx, ff_make_format_list(format));

    ff_set_common_samplerates(ctx, ff_make_format_list(samplerate));

    ff_add_channel_layout(&channel_layout, AV_CH_LAYOUT_MONO);

    ff_set_common_channel_layouts(ctx, channel_layout);

    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    return 0;
}

static int request_frame(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    int ret = 0;

    ret = ff_request_frame(ctx->inputs[0]);

    return ret;
}

static int dtmf_filter_frame(AVFilterLink *inlink, AVFilterBufferRef *buf)
{
    AVFilterContext *ctx  = inlink->dst;
    DTMFContext     *dtmf = ctx->priv;
    AVFilterBufferRefAudioProps *props = buf->audio;
    AVFilterLink *outlink = inlink->dst->outputs[0];

    dtmf_guess(dtmf, buf->extended_data[0], props->nb_samples);

    ff_filter_frame(outlink, buf);

    return 0;
}

static const AVFilterPad avfilter_af_dtmf_inputs[] = {
    {
        .name           = "default",
        .type           = AVMEDIA_TYPE_AUDIO,
        .filter_frame   = dtmf_filter_frame,
        .min_perms      = AV_PERM_READ
    },
    { NULL }
};

static const AVFilterPad avfilter_af_dtmf_outputs[] = {
    {
        .name          = "dummy",
        .type          = AVMEDIA_TYPE_AUDIO,
        .config_props  = config_output,
        .request_frame = request_frame
    },
    { NULL }
};

AVFilter avfilter_af_dtmf = {
    .name          = "dtmf",
    .description   = NULL_IF_CONFIG_SMALL("Audio resampling and conversion."),
    .priv_size     = sizeof(DTMFContext),

    .uninit         = uninit,
    .query_formats  = query_formats,

    .inputs    = avfilter_af_dtmf_inputs,
    .outputs   = avfilter_af_dtmf_outputs,
};
