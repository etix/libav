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

#ifndef AVSCALE_AVSCALE_H
#define AVSCALE_AVSCALE_H

/**
 * @file
 * @ingroup libavs
 * Main libavscale public API header
 *
 */

#include <stdint.h>

#include "libavutil/avutil.h"
#include "libavutil/frame.h"
#include "version.h"

/**
 * The structure details are hidden till the API gets stable.
 */
typedef struct AVScaleContext AVScaleContext;

/**
 * Return the LIBAVSCALE_VERSION_INT constant.
 */
unsigned avscale_version(void);

/**
 * Return the libavscale build-time configuration.
 */
const char *avscale_configuration(void);

/**
 * Return the libavscale license.
 */
const char *avscale_license(void);

/**
 * Allocate an empty AVSContext. This must be filled and passed to
 * avs_init_context(). For filling see AVOptions, options.c.
 */
AVScaleContext *avs_alloc_context(void);

/**
 * Initialize the avscaler context by allocating the pixel format conversion
 * chain and the scaling kernel.
 *
 * @return zero or positive value on success, a negative value on
 * error
 */
int avs_init_context(AVScaleContext *c);

/**
 * Get the AVClass for AVSContext. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 */
const AVClass *avs_get_class(void);

/**
 * Free the avscaler context AVSContext.
 * If AVSContext is NULL, then does nothing.
 */
void avs_free_context(AVScaleContext *c);

/**
 * Scale the image provided by an AVFrame in src and put the result
 * in dst.
 *
 * If the scaling context is not finalized (by calling avs_init_context)
 * or the frame pixel format and dimensions do not match the current
 * context the function would reconfigure it before scaling.
 *
 * @param c         The scaling context previously created
 *                  with avs_alloc_context()
 * @param src       The source frame
 * @param dst       The destination frame
 * @return          0 on successo or AVERROR
 */

int avs_scale_frame(AVScaleContext *c, AVFrame *src, AVFrame *dst);


/**
 * Lock the scaling context to a specific source buffer.
 * Useful when using stateful scalers over slices.
 */
int avs_scale_lock(AVScaleContext *c, AVFrame *src);

/**
 * Unlock the scaling context to a specific source buffer.
 * Useful when using stateful scalers over slices.
 */
int avs_scale_unlock(AVScaleContext *c, AVFrame *src);

/**
 * Scale the image provided by an AVFrame in src and put the result
 * in dst, slice by slice.
 *
 * @note A Slice is a number of contiguous rows in a frame, the function
 *       should be called providing contiguous slices.
 *
 * @param c         The locked scaling context.
 * @param src       The source frame
 * @param dst       The destination frame
 * @param start     Starting row
 * @param end       Ending row
 * @return          0 on success, AVERROR otherwise
 */

int avs_scale_slice(AVScaleContext *c, AVFrame *src, AVFrame *dst,
                    int start, int end);

#endif /* AVSCALE_AVSCALE_H */
