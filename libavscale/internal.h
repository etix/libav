/*
 * Copyright (C) 2013 Luca Barbato
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

#ifndef AVSCALE_INTERNAL_H
#define AVSCALE_INTERNAL_H

typedef struct AVScaleContext {
    AVClass *c;
    AVScale *scaler;
    int in_width, in_height;
    int in_format;
    int out_width, out_height;
    int out_format;
    void *priv;
} AVScaleContext;

#endif /* AVSCALE_AVSCALE_H */
