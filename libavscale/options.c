/*
 * Copyright (c) 2014 Luca Barbato
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

#include <stdint.h>

#include "libavutil/mathematics.h"
#include "libavutil/mem.h"
#include "libavutil/opt.h"
#include "avscale.h"
#include "internal.h"

/**
 * @file
 * Options definition for AVAScaleContext.
 */

#define OFFSET(x) offsetof(AVScaleContext, x)
#define P AV_OPT_FLAG_VIDEO_PARAM

static const AVOption avscale_options[] = {
    { "in_width",      "Input Width",     OFFSET(in_width),      AV_OPT_TYPE_INT,  { .i64 = 0              }, 1,            INT_MAX,              P },
    { "in_height",     "Input Height",    OFFSET(in_height),     AV_OPT_TYPE_INT,  { .i64 = 0              }, 1,            INT_MAX,              P },
    { "in_format",     "Input Pixel Format",   OFFSET(in_format),    AV_OPT_TYPE_INT,  { .i64 = 0              }, 0,            INT_MAX,              P },
    { "out_width",      "Input Width",    OFFSET(out_width),     AV_OPT_TYPE_INT,  { .i64 = 0              }, 1,            INT_MAX,              P },
    { "out_height",     "Input Height",   OFFSET(out_height),    AV_OPT_TYPE_INT,  { .i64 = 0              }, 1,            INT_MAX,              P },
    { "out_format",    "Output Pixel Format",   OFFSET(out_format),    AV_OPT_TYPE_INT,  { .i64 = 0              }, 0,            INT_MAX,              P },
};

static const AVClass avscale_context_class = {
    .class_name = "AVScaleContext",
    .item_name  = av_default_item_name,
    .option     = avscale_options,
    .version    = LIBAVUTIL_VERSION_INT,
};

AVScaleContext *avscale_alloc_context(void)
{
    AVScaleContext *avs;

    avs = av_mallocz(sizeof(*avs));
    if (!avs)
        return NULL;

    avs->av_class = &avscale_context_class;
    av_opt_set_defaults(avs);

    return avs;
}

const AVClass *avscale_get_class(void)
{
    return &avscale_context_class;
}
