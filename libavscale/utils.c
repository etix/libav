/*
 * Copyright (C) 2014 Luca Barbato
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

#include "config.h"

#include "avscale.h"
#include "internal.h"

#define SCALER_CONFIGURED(c)                       \
    (c->in_width  && c->in_height  && c->format && \
     c->out_width && c->out_height && c->format)

int select_scaler(AVScaleContext *c)
{
    c->backend = dummy_backend;

    return 0;
}

void avscale_free_context(AVScaleContext *c)
{
    if (c->backend) {
        c->backend->close(c);
        c->backend = NULL;
    }
}

int avscale_init_context(AVScaleContext *c)
{
    int ret;

    if (c->scaler) {
        avscale_free_context(c);
    }

    if (!SCALER_CONFIGURED(c))
        return AVERROR(EINVAL);

    ret = select_scaler(c);

    if (ret < 0)
        return ret;

    return c->backend->init(c);
}

static int needs_reinit(AVScaleContext *c, AVFrame *out, AVFrame *in)
{
    if (c->in_width  != in->width ||
        c->in_height != in->height ||
        c->in_format != in->format ||
        c->out_width  != out->width ||
        c->out_height != out->height ||
        c->out_format != out->format)
        return 1;
    return 0;
}

int avscale_scale_frame(AVScaleContext *c, AVFrame *dst, AVFrame *src)
{

    if (needs_reinit(c, dst, src)) {
        c->in_width   = src->width;
        c->in_height  = src->height;
        c->in_format  = src->format;
        c->out_width  = src->width;
        c->out_height = src->height;
        c->out_format = src->format;
        avscale_init_context(c);
    }

    return c->backend->scale(c,
                             dst->data, dst->linesize,
                             src->data, src->linesize);
}
