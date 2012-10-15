/*
 * Copyright (c) 2012 Luca Barbato
 *
 * This file is part of Libav.
 *
 * Libav is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * Libav is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Libav; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * compare two videos.
 */

#include "libavutil/opt.h"
#include "libavutil/common.h"
#include "libavutil/pixdesc.h"
#include "libavutil/avstring.h"
#include "libavutil/avassert.h"
#include "libavutil/parseutils.h"
#include "avfilter.h"
#include "internal.h"
#include "formats.h"
#include "video.h"
#include "compare.h"

#define MAIN 0
#define REF  1

enum {
    CMP_DIFF,
    CMP_DSSIM,
    NB_CMP
};

#define OFFSET(x) offsetof(CmpContext, x)
#define V AV_OPT_FLAG_VIDEO_PARAM
static const AVOption options[] = {
    { "method",    "",  OFFSET(comparator), AV_OPT_TYPE_INT,{ .i64 = CMP_DIFF }, 0, NB_CMP, V, "compare" },
    {   "diff", "Output the different pixels", 0, AV_OPT_TYPE_CONST, {.i64 = CMP_DIFF}, INT_MIN, INT_MAX, V, "compare" },
    {   "dssim","Calculate the dssim and highlight it", 0, AV_OPT_TYPE_CONST, {.i64 = CMP_DSSIM},  INT_MIN, INT_MAX, V, "compare" },
    { NULL },
};

static const AVClass class = {
    .class_name = "compare filter",
    .item_name  = av_default_item_name,
    .option     = options,
    .version    = LIBAVUTIL_VERSION_INT,
};

static int diff_frame(AVFilterContext *ctx,
                      AVFilterBufferRef *dst, AVFilterBufferRef *src)
{
    uint8_t *dp = dst->data[0];
    uint8_t *sp = src->data[0];
    int i, j;
    int width, height;

    width  = FFMIN(dst->video->w, src->video->w);
    height = FFMIN(dst->video->h, src->video->h);
    for (i = 0; i < height; i++) {
        uint8_t *d = dp, *s = sp;
        for (j = 0; j < width; j++) {
            int dr, dg, db;
            dr = d[0]-s[0];
            dg = d[1]-s[1];
            db = d[2]-s[2];
            d[0] = 128 - dr;
            d[1] = 128 - dg;
            d[2] = 128 - db;
            d[3] = 255;
            d += 4;
            s += 4;
        }
        dp += dst->linesize[0];
        sp += src->linesize[0];
    }
    return 0;
}

static av_cold int init(AVFilterContext *ctx, const char *args)
{
    CmpContext *s = ctx->priv;
    int ret;

    s->class = &class;
    av_opt_set_defaults(s);

    if ((ret = av_set_options_string(s, args, "=", ":")) < 0) {
        av_log(ctx, AV_LOG_ERROR, "Error parsing options string %s.\n", args);
        return ret;
    }

    switch (s->comparator) {
    case CMP_DIFF:
        s->compare = diff_frame;
        break;
    case CMP_DSSIM:
        s->compare = ff_dssim_frame;
        break;
    default:
        return AVERROR_INVALIDDATA;
    }

    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    CmpContext *s = ctx->priv;

    if (s->n)
        av_log(NULL, AV_LOG_INFO, "Average metric: %.4f\n", s->avg/s->n);

    avfilter_unref_bufferp(&s->main);
    avfilter_unref_bufferp(&s->ref);
}

static int query_formats(AVFilterContext *ctx)
{
    const enum AVPixelFormat pix_fmts[] = { AV_PIX_FMT_RGB32, AV_PIX_FMT_NONE };

    AVFilterFormats *formats = ff_make_format_list(pix_fmts);

    ff_formats_ref(formats, &ctx->inputs [MAIN]->out_formats);
    ff_formats_ref(formats, &ctx->inputs [REF ]->out_formats);
    ff_formats_ref(formats, &ctx->outputs[MAIN]->in_formats);

    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;

    outlink->w = ctx->inputs[MAIN]->w;
    outlink->h = ctx->inputs[MAIN]->h;
    outlink->time_base = ctx->inputs[MAIN]->time_base;

    return 0;
}

static int filter_frame_main(AVFilterLink *inlink, AVFilterBufferRef *frame)
{
    CmpContext *s = inlink->dst->priv;

    av_assert0(!s->main);
    s->main = frame;

    return 0;
}

static int filter_frame_ref(AVFilterLink *inlink, AVFilterBufferRef *frame)
{
    CmpContext *s = inlink->dst->priv;

    av_assert0(!s->ref);
    s->ref = frame;

    return 0;
}

static int output_frame(AVFilterContext *ctx)
{
    CmpContext *s = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    int ret = ff_filter_frame(outlink, s->main);
    s->main = NULL;

    return ret;
}

static int request_frame(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    CmpContext    *s = ctx->priv;
    int ret = 0;

    /* get a frame on the main input */
    if (!s->main) {
        ret = ff_request_frame(ctx->inputs[MAIN]);
        if (ret < 0)
            return ret;
    }

    /* get a new frame on the reference input */
    if (!s->ref) {
        ret = ff_request_frame(ctx->inputs[REF]);
        if (ret < 0)
            return ret;
    }

    ret = s->compare(ctx, s->main, s->ref);

    avfilter_unref_bufferp(&s->ref);

    if (ret)
        return ret;

    return output_frame(ctx);
}

static const AVFilterPad vf_compare_inputs[] = {
    {
        .name         = "main",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = filter_frame_main,
        .min_perms    = AV_PERM_READ,
        .rej_perms    = AV_PERM_REUSE2 | AV_PERM_PRESERVE,
        .needs_fifo   = 1,
    },
    {
        .name         = "ref",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = filter_frame_ref,
        .min_perms    = AV_PERM_READ,
        .rej_perms    = AV_PERM_REUSE2,
        .needs_fifo   = 1,
    },
    { NULL }
};

static const AVFilterPad vf_compare_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .config_props  = config_output,
        .request_frame = request_frame,
    },
    { NULL }
};

AVFilter avfilter_vf_compare = {
    .name          = "compare",
    .description   = NULL_IF_CONFIG_SMALL("Compare two video sources"),
    .init          = init,
    .uninit        = uninit,
    .priv_size     = sizeof(CmpContext),
    .query_formats = query_formats,
    .inputs        = vf_compare_inputs,
    .outputs       = vf_compare_outputs,
};
