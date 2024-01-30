/*
 * EXIF metadata parser - internal functions
 * Copyright (c) 2013 Thilo Borgmann <thilo.borgmann _at_ mail.de>
 * Copyright (c) 2024 Leo Izen <leo.izen@gmail.com>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * EXIF metadata parser - internal functions
 * @author Thilo Borgmann <thilo.borgmann _at_ mail.de>
 * @author Leo Izen <leo.izen@gmail.com>
 */

#ifndef AVCODEC_EXIF_INTERNAL_H
#define AVCODEC_EXIF_INTERNAL_H

#include "libavutil/buffer.h"
#include "libavutil/frame.h"
#include "bytestream.h"
#include "exif.h"

int ff_exif_attach(void *logctx, AVFrame *frame, AVBufferRef **data);
int ff_exif_collect_ifd(void *logctx, GetByteContext *gb, int le, AVBufferRef **buffer);

#endif /* AVCODEC_EXIF_INTERNAL_H */
