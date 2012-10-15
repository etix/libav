/*
 * Copyright (c) 2011 Kornel Lesiński. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials
 * provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include "compare.h"
#include "avfilter.h"
#include "libavutil/common.h"

typedef struct {
    float l, A, b, a;
} LABA;

static const float D65x = 0.9505f, D65y = 1.0f, D65z = 1.089f;

static inline LABA rgba_to_laba(const float gamma, const uint8_t *pix)
{
    float b = powf(pix[0] / 255.0f, 1.0f / gamma),
          g = powf(pix[1] / 255.0f, 1.0f / gamma),
          r = powf(pix[2] / 255.0f, 1.0f / gamma),
          a = pix[3] / 255.0f;

    float fx        = (r * 0.4124f + g * 0.3576f + b * 0.1805f) / D65x;
    float fy        = (r * 0.2126f + g * 0.7152f + b * 0.0722f) / D65y;
    float fz        = (r * 0.0193f + g * 0.1192f + b * 0.9505f) / D65z;
    const float eps = 216.0 / 24389.0;

    fx = (fx > eps) ? powf(fx, 1.0f / 3.0f) : (7.787f * fx + 16.0f / 116.0f);
    fy = (fy > eps) ? powf(fy, 1.0f / 3.0f) : (7.787f * fy + 16.0f / 116.0f);
    fz = (fz > eps) ? powf(fz, 1.0f / 3.0f) : (7.787f * fz + 16.0f / 116.0f);

    return (LABA) {
               (116.0f * fy - 16.0f) / 100.0 * a,
             /* 86 is a fudge to make the value positive */
               (86.2f + 500.0f * (fx - fy)) / 220.0 * a,
             /* 107 is a fudge to make the value positive */
               (107.9f + 200.0f * (fy - fz)) / 220.0 * a,
               a };
}

/* Macros to avoid repeating every line 4 times */

#define LABA_OP(dst, X, op, Y) dst = (LABA) {   \
        (X).l op(Y).l,                          \
        (X).A op(Y).A,                          \
        (X).b op(Y).b,                          \
        (X).a op(Y).a }

#define LABA_OPC(dst, X, op, Y) dst = (LABA) {  \
        (X).l op(Y),                            \
        (X).A op(Y),                            \
        (X).b op(Y),                            \
        (X).a op(Y) }

#define LABA_OP1(dst, op, Y) dst = (LABA) {     \
        dst.l op(Y).l,                          \
        dst.A op(Y).A,                          \
        dst.b op(Y).b,                          \
        dst.a op(Y).a }


typedef void rowcallback (LABA *, int width);

static void square_row(LABA *row, int width)
{
    int i;
    for (i = 0; i < width; i++)
        LABA_OP(row[i], row[i], *, row[i]);
}

/*
 * Blur image horizontally (width 2*size+1) and write it transposed to dst
 * (called twice gives 2d blur)
 * Run callback on every row before blurring
 */
static void transposing_1d_blur(LABA *restrict src,
                                LABA *restrict dst,
                                int width,
                                int height,
                                const int size,
                                rowcallback *const callback)
{
    const float sizef = size;
    int i, j;

    for (j = 0; j < height; j++) {
        LABA *restrict row = src + j * width;
        LABA sum;

        // preprocess line
        if (callback)
            callback(row, width);

        // accumulate sum for pixels outside line
        LABA_OPC(sum, row[0], *, sizef);
        for (i = 0; i < size; i++)
            LABA_OP1(sum, +=, row[i]);

        // blur with left side outside line
        for (i = 0; i < size; i++) {
            LABA_OP1(sum, -=, row[0]);
            LABA_OP1(sum, +=, row[i + size]);

            LABA_OPC(dst[i * height + j], sum, /, sizef * 2.0f);
        }

        for (i = size; i < width - size; i++) {
            LABA_OP1(sum, -=, row[i - size]);
            LABA_OP1(sum, +=, row[i + size]);

            LABA_OPC(dst[i * height + j], sum, /, sizef * 2.0f);
        }

        // blur with right side outside line
        for (i = width - size; i < width; i++) {
            LABA_OP1(sum, -=, row[i - size]);
            LABA_OP1(sum, +=, row[width - 1]);

            LABA_OPC(dst[i * height + j], sum, /, sizef * 2.0f);
        }
    }
}

/*
 * Filter image with callback and blur it (lousy approximate of gaussian)
 */
static void blur(LABA *restrict src, LABA *restrict tmp, LABA *restrict dst,
                 int width, int height, rowcallback *const callback)
{
    int small = 1, big = 1;
    if (FFMIN(height, width) > 100)
        big++;
    if (FFMIN(height, width) > 200)
        big++;
    if (FFMIN(height, width) > 500)
        small++;
    if (FFMIN(height, width) > 800)
        big++;

    transposing_1d_blur(src, tmp, width, height, 1, callback);
    transposing_1d_blur(tmp, dst, height, width, 1, NULL);
    transposing_1d_blur(src, tmp, width, height, small, NULL);
    transposing_1d_blur(tmp, dst, height, width, small, NULL);
    transposing_1d_blur(dst, tmp, width, height, big, NULL);
    transposing_1d_blur(tmp, dst, height, width, big, NULL);
}

/*
 * Conversion is not reversible
 */
static inline LABA convert_pixel(const uint8_t *pix, float gamma, int i, int j)
{
    LABA f1 = rgba_to_laba(gamma, pix);

    // Compose image on coloured background to better judge dissimilarity with
    // various backgrounds
    int n = i ^ j;
    if (n & 4)
        f1.l += 1.0 - f1.a; // using premultiplied alpha
    if (n & 8)
        f1.A += 1.0 - f1.a;
    if (n & 16)
        f1.b += 1.0 - f1.a;

    // Since alpha is already blended with other channels,
    // lower amplitude of alpha to lower score for alpha difference
    f1.a *= 0.75;

    // SSIM is supposed to be applied only to luma,
    // lower amplitude of chroma to lower score for chroma difference
    // (chroma is not ignored completely, because IMHO it also matters)
    f1.A *= 0.75;
    f1.b *= 0.75;

    return f1;
}

/*
 * Algorithm based on Rabah Mehdi's C++ implementation
 */
int ff_dssim_frame(AVFilterContext *ctx,
                   AVFilterBufferRef *dst, AVFilterBufferRef *src)
{
    CmpContext *s = ctx->priv;

    uint8_t *dp = dst->data[0];
    uint8_t *sp = src->data[0];

    float gamma1, gamma2;

    const double c1   = 0.01 * 0.01, c2 = 0.03 * 0.03;
    const float gamma = 1.0 / 2.2;
    double avgminssim = 0;

    int width  = FFMIN(dst->video->w, src->video->w),
        height = FFMIN(dst->video->h, src->video->h);

    LABA *restrict img1;
    LABA *restrict img2;
    LABA *restrict img1_img2;
    LABA *tmp;
    LABA *restrict sigma12;
    LABA *restrict sigma1_sq;
    LABA *restrict sigma2_sq;
    LABA *restrict mu1;
    LABA *restrict mu2;

    int i, j, k = 0;

    gamma1 = gamma2 = 0.45455;

    img1      = av_malloc(width * height * sizeof(LABA));
    img2      = av_malloc(width * height * sizeof(LABA));
    img1_img2 = av_malloc(width * height * sizeof(LABA));
    tmp       = av_malloc(width * height * sizeof(LABA));
    sigma12   = av_malloc(width * height * sizeof(LABA));
    sigma1_sq = av_malloc(width * height * sizeof(LABA));
    sigma2_sq = av_malloc(width * height * sizeof(LABA));

    if (!img1 || !img2 || !img1_img2 || !tmp ||
        !sigma12 || !sigma1_sq || !sigma2_sq)
        return AVERROR(ENOMEM);

    mu1 = img1_img2;
    mu2 = img1;

    for (j = 0; j < height; j++) {
        uint8_t *d = dp, *s = sp;
        for (i = 0; i < width; i++, k++) {
            LABA f1 = convert_pixel(d, gamma1, i, j);
            LABA f2 = convert_pixel(s, gamma2, i, j);

            img1[k] = f1;
            img2[k] = f2;

            // part of computation
            LABA_OP(img1_img2[k], f1, *, f2);
            d += 4;
            s += 4;
        }
        dp += dst->linesize[0];
        sp += src->linesize[0];
    }

    blur(img1_img2, tmp, sigma12, width, height, NULL);

    blur(img1, tmp, mu1, width, height, NULL);

    blur(img1, tmp, sigma1_sq, width, height, square_row);

    blur(img2, tmp, mu2, width, height, NULL);

    blur(img2, tmp, sigma2_sq, width, height, square_row);

    av_free(img2);
    av_freep(&tmp);

#define SSIM(r)                                             \
    ((2.0 * (mu1[k].r * mu2[k].r) + c1) *                   \
     (2.0 * (sigma12[k].r - (mu1[k].r * mu2[k].r)) + c2)) / \
    (((mu1[k].r * mu1[k].r) + (mu2[k].r * mu2[k].r) + c1) * \
     ((sigma1_sq[k].r - (mu1[k].r * mu1[k].r)) +            \
      (sigma2_sq[k].r - (mu2[k].r * mu2[k].r)) + c2))

    k = 0;
    dp = dst->data[0];

    for (j = 0; j < height; j++) {
        uint8_t *d = dp;
        for (i = 0; i < width; i++, k++) {
            LABA ssim = (LABA) {SSIM(l), SSIM(A), SSIM(b), SSIM(a)};

            double minssim = FFMIN(FFMIN(ssim.l, ssim.A),
                                   FFMIN(ssim.b, ssim.a));
            float  max     = 1.0 - FFMIN(FFMIN(ssim.l, ssim.A), ssim.b);
            float maxsq = max * max;
            float r, g, b;

            avgminssim += minssim;

            r = (1.0 - ssim.a) + maxsq;
            g = max + maxsq;
            b = max * 0.5f + (1.0 - ssim.a) * 0.5f + maxsq;

            d[0] = av_clip_uint8(powf(r, gamma) * 256.0f);
            d[1] = av_clip_uint8(powf(g, gamma) * 256.0f);
            d[2] = av_clip_uint8(powf(b, gamma) * 256.0f);
            d[3] = 255;
            d += 4;
        }
        dp += dst->linesize[0];
    }

    av_free(mu1);
    av_free(mu2);
    av_free(sigma12);
    av_free(sigma1_sq);
    av_free(sigma2_sq);

    avgminssim /= (double)width * height;

    s->avg += 1.0 / (avgminssim) - 1.0;
    s->n++;

    av_log(NULL, AV_LOG_INFO, "pts %"PRId64" dssim value %.4f\n",
           dst->pts, 1.0 / (avgminssim) - 1.0);
    return 0;
}
