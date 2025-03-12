/*
 * EXIF metadata parser
 * Copyright (c) 2013 Thilo Borgmann <thilo.borgmann _at_ mail.de>
 * Copyright (c) 2024-2025 Leo Izen <leo.izen@gmail.com>
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
 * EXIF metadata parser
 * @author Thilo Borgmann <thilo.borgmann _at_ mail.de>
 * @author Leo Izen <leo.izen@gmail.com>
 */

#include <inttypes.h>

#include "libavutil/bprint.h"
#include "libavutil/display.h"
#include "libavutil/intreadwrite.h"
#include "libavutil/mem.h"

#include "bytestream.h"
#include "exif_internal.h"
#include "tiff_common.h"

#define EXIF_TAG_NAME_LENGTH   32
#define MAKERNOTE_TAG          0x927c
#define ORIENTATION_TAG        0x112

struct exif_tag {
    char      name[EXIF_TAG_NAME_LENGTH];
    uint16_t  id;
};

static const struct exif_tag tag_list[] = { // JEITA CP-3451 EXIF specification:
    {"GPSVersionID",               0x00}, // <- Table 12 GPS Attribute Information
    {"GPSLatitudeRef",             0x01},
    {"GPSLatitude",                0x02},
    {"GPSLongitudeRef",            0x03},
    {"GPSLongitude",               0x04},
    {"GPSAltitudeRef",             0x05},
    {"GPSAltitude",                0x06},
    {"GPSTimeStamp",               0x07},
    {"GPSSatellites",              0x08},
    {"GPSStatus",                  0x09},
    {"GPSMeasureMode",             0x0A},
    {"GPSDOP",                     0x0B},
    {"GPSSpeedRef",                0x0C},
    {"GPSSpeed",                   0x0D},
    {"GPSTrackRef",                0x0E},
    {"GPSTrack",                   0x0F},
    {"GPSImgDirectionRef",         0x10},
    {"GPSImgDirection",            0x11},
    {"GPSMapDatum",                0x12},
    {"GPSDestLatitudeRef",         0x13},
    {"GPSDestLatitude",            0x14},
    {"GPSDestLongitudeRef",        0x15},
    {"GPSDestLongitude",           0x16},
    {"GPSDestBearingRef",          0x17},
    {"GPSDestBearing",             0x18},
    {"GPSDestDistanceRef",         0x19},
    {"GPSDestDistance",            0x1A},
    {"GPSProcessingMethod",        0x1B},
    {"GPSAreaInformation",         0x1C},
    {"GPSDateStamp",               0x1D},
    {"GPSDifferential",            0x1E},
    {"ImageWidth",                 0x100}, // <- Table 3 TIFF Rev. 6.0 Attribute Information Used in Exif
    {"ImageLength",                0x101},
    {"BitsPerSample",              0x102},
    {"Compression",                0x103},
    {"PhotometricInterpretation",  0x106},
    {"Orientation",                0x112},
    {"SamplesPerPixel",            0x115},
    {"PlanarConfiguration",        0x11C},
    {"YCbCrSubSampling",           0x212},
    {"YCbCrPositioning",           0x213},
    {"XResolution",                0x11A},
    {"YResolution",                0x11B},
    {"ResolutionUnit",             0x128},
    {"StripOffsets",               0x111},
    {"RowsPerStrip",               0x116},
    {"StripByteCounts",            0x117},
    {"JPEGInterchangeFormat",      0x201},
    {"JPEGInterchangeFormatLength",0x202},
    {"TransferFunction",           0x12D},
    {"WhitePoint",                 0x13E},
    {"PrimaryChromaticities",      0x13F},
    {"YCbCrCoefficients",          0x211},
    {"ReferenceBlackWhite",        0x214},
    {"DateTime",                   0x132},
    {"ImageDescription",           0x10E},
    {"Make",                       0x10F},
    {"Model",                      0x110},
    {"Software",                   0x131},
    {"Artist",                     0x13B},
    {"Copyright",                  0x8298},
    {"ExifVersion",                0x9000}, // <- Table 4 Exif IFD Attribute Information (1)
    {"FlashpixVersion",            0xA000},
    {"ColorSpace",                 0xA001},
    {"ComponentsConfiguration",    0x9101},
    {"CompressedBitsPerPixel",     0x9102},
    {"PixelXDimension",            0xA002},
    {"PixelYDimension",            0xA003},
    {"MakerNote",                  0x927C},
    {"UserComment",                0x9286},
    {"RelatedSoundFile",           0xA004},
    {"DateTimeOriginal",           0x9003},
    {"DateTimeDigitized",          0x9004},
    {"SubSecTime",                 0x9290},
    {"SubSecTimeOriginal",         0x9291},
    {"SubSecTimeDigitized",        0x9292},
    {"ImageUniqueID",              0xA420},
    {"ExposureTime",               0x829A}, // <- Table 5 Exif IFD Attribute Information (2)
    {"FNumber",                    0x829D},
    {"ExposureProgram",            0x8822},
    {"SpectralSensitivity",        0x8824},
    {"ISOSpeedRatings",            0x8827},
    {"OECF",                       0x8828},
    {"ShutterSpeedValue",          0x9201},
    {"ApertureValue",              0x9202},
    {"BrightnessValue",            0x9203},
    {"ExposureBiasValue",          0x9204},
    {"MaxApertureValue",           0x9205},
    {"SubjectDistance",            0x9206},
    {"MeteringMode",               0x9207},
    {"LightSource",                0x9208},
    {"Flash",                      0x9209},
    {"FocalLength",                0x920A},
    {"SubjectArea",                0x9214},
    {"FlashEnergy",                0xA20B},
    {"SpatialFrequencyResponse",   0xA20C},
    {"FocalPlaneXResolution",      0xA20E},
    {"FocalPlaneYResolution",      0xA20F},
    {"FocalPlaneResolutionUnit",   0xA210},
    {"SubjectLocation",            0xA214},
    {"ExposureIndex",              0xA215},
    {"SensingMethod",              0xA217},
    {"FileSource",                 0xA300},
    {"SceneType",                  0xA301},
    {"CFAPattern",                 0xA302},
    {"CustomRendered",             0xA401},
    {"ExposureMode",               0xA402},
    {"WhiteBalance",               0xA403},
    {"DigitalZoomRatio",           0xA404},
    {"FocalLengthIn35mmFilm",      0xA405},
    {"SceneCaptureType",           0xA406},
    {"GainControl",                0xA407},
    {"Contrast",                   0xA408},
    {"Saturation",                 0xA409},
    {"Sharpness",                  0xA40A},
    {"DeviceSettingDescription",   0xA40B},
    {"SubjectDistanceRange",       0xA40C},

    /* private EXIF tags */
    {"PrintImageMatching",         0xC4A5}, // <- undocumented meaning

    /* IFD tags */
    {"ExifIFD",                    0x8769}, // <- An IFD pointing to standard Exif metadata
    {"GPSInfo",                    0x8825}, // <- An IFD pointing to GPS Exif Metadata
    {"InteropIFD",                 0xA005}, // <- Table 13 Interoperability IFD Attribute Information
};

static const char *exif_get_tag_name(uint16_t id)
{
    int i;

    for (i = 0; i < FF_ARRAY_ELEMS(tag_list); i++) {
        if (tag_list[i].id == id)
            return tag_list[i].name;
    }

    return NULL;
}

static inline void tput16(PutByteContext *pb, const int le, const uint16_t value)
{
    le ? bytestream2_put_le16(pb, value) : bytestream2_put_be16(pb, value);
}

static inline void tput32(PutByteContext *pb, const int le, const uint32_t value)
{
    le ? bytestream2_put_le32(pb, value) : bytestream2_put_be32(pb, value);
}

static inline void tput64(PutByteContext *pb, const int le, const uint64_t value)
{
    le ? bytestream2_put_le64(pb, value) : bytestream2_put_be64(pb, value);
}

static int exif_read_values(void *logctx, GetByteContext *gb, int le, AVExifEntry *entry)
{
    switch (entry->type) {
        case AV_TIFF_SHORT:
        case AV_TIFF_LONG:
            entry->value.uint = av_calloc(entry->count, sizeof(*entry->value.uint));
            break;
        case AV_TIFF_SSHORT:
        case AV_TIFF_SLONG:
            entry->value.sint = av_calloc(entry->count, sizeof(*entry->value.sint));
            break;
        case AV_TIFF_DOUBLE:
        case AV_TIFF_FLOAT:
            entry->value.dbl = av_calloc(entry->count, sizeof(*entry->value.dbl));
            break;
        case AV_TIFF_RATIONAL:
        case AV_TIFF_SRATIONAL:
            entry->value.rat = av_calloc(entry->count, sizeof(*entry->value.rat));
            break;
        case AV_TIFF_UNDEFINED:
        case AV_TIFF_BYTE:
            entry->value.ubytes = av_mallocz(entry->count);
            break;
        case AV_TIFF_SBYTE:
            entry->value.sbytes = av_mallocz(entry->count);
            break;
        case AV_TIFF_STRING:
            entry->value.str = av_mallocz(entry->count + 1);
            break;
        case AV_TIFF_IFD:
            av_log(logctx, AV_LOG_WARNING, "Bad IFD type for non-IFD tag\n");
            return AVERROR_INVALIDDATA;
    }
    if (!entry->value.ptr)
        return AVERROR(ENOMEM);
    switch (entry->type) {
        case AV_TIFF_SHORT:
            for (size_t i = 0; i < entry->count; i++)
                entry->value.uint[i] = ff_tget_short(gb, le);
            break;
        case AV_TIFF_LONG:
            for (size_t i = 0; i < entry->count; i++)
                entry->value.uint[i] = ff_tget_long(gb, le);
            break;
        case AV_TIFF_SSHORT:
            for (size_t i = 0; i < entry->count; i++)
                entry->value.sint[i] = (int16_t) ff_tget_short(gb, le);
            break;
        case AV_TIFF_SLONG:
            for (size_t i = 0; i < entry->count; i++)
                entry->value.sint[i] = (int32_t) ff_tget_long(gb, le);
            break;
        case AV_TIFF_DOUBLE:
            for (size_t i = 0; i < entry->count; i++)
                entry->value.dbl[i] = ff_tget_double(gb, le);
            break;
        case AV_TIFF_FLOAT:
            for (size_t i = 0; i < entry->count; i++) {
                av_alias32 alias = { .u32 = ff_tget_long(gb, le) };
                entry->value.dbl[i] = alias.f32;
            }
            break;
        case AV_TIFF_RATIONAL:
        case AV_TIFF_SRATIONAL:
            for (size_t i = 0; i < entry->count; i++) {
                int32_t numer = ff_tget_long(gb, le);
                int32_t denom = ff_tget_long(gb, le);
                entry->value.rat[i] = av_make_q(numer, denom);
            }
            break;
        case AV_TIFF_UNDEFINED:
        case AV_TIFF_BYTE:
            bytestream2_get_buffer(gb, entry->value.ubytes, entry->count);
            break;
        case AV_TIFF_SBYTE:
            bytestream2_get_buffer(gb, entry->value.sbytes, entry->count);
            break;
        case AV_TIFF_STRING:
            bytestream2_get_buffer(gb, entry->value.str, entry->count);
            break;
    }

    return 0;
}

static void exif_write_values(PutByteContext *pb, int le, const AVExifEntry *entry) {
    switch (entry->type) {
        case AV_TIFF_SHORT:
            for (size_t i = 0; i < entry->count; i++)
                tput16(pb, le, entry->value.uint[i]);
            break;
        case AV_TIFF_LONG:
            for (size_t i = 0; i < entry->count; i++)
            tput32(pb, le, entry->value.uint[i]);
            break;
        case AV_TIFF_SSHORT:
            for (size_t i = 0; i < entry->count; i++)
                tput16(pb, le, entry->value.sint[i]);
            break;
        case AV_TIFF_SLONG:
            for (size_t i = 0; i < entry->count; i++)
                tput32(pb, le, entry->value.sint[i]);
            break;
        case AV_TIFF_DOUBLE:
            for (size_t i = 0; i < entry->count; i++) {
                const av_alias64 a = { .f64 = entry->value.dbl[i] };
                tput64(pb, le, a.u64);
            }
            break;
        case AV_TIFF_FLOAT:
            for (size_t i = 0; i < entry->count; i++) {
                const av_alias32 a = { .f32 = entry->value.dbl[i] };
                tput32(pb, le, a.u32);
            }
            break;
        case AV_TIFF_RATIONAL:
        case AV_TIFF_SRATIONAL:
            for (size_t i = 0; i < entry->count; i++) {
                tput32(pb, le, entry->value.rat[i].num);
                tput32(pb, le, entry->value.rat[i].den);
            }
            break;
        case AV_TIFF_UNDEFINED:
        case AV_TIFF_BYTE:
            bytestream2_put_buffer(pb, entry->value.ubytes, entry->count);
            break;
        case AV_TIFF_SBYTE:
            bytestream2_put_buffer(pb, entry->value.sbytes, entry->count);
            break;
        case AV_TIFF_STRING:
            bytestream2_put_buffer(pb, entry->value.str, entry->count);
            break;
    }
}

static const uint8_t aoc_header[] = { 'A', 'O', 'C', 0, };
static const uint8_t casio_header[] = { 'Q', 'V', 'C', 0, 0, 0, };
static const uint8_t foveon_header[] = { 'F', 'O', 'V', 'E', 'O', 'N', 0, 0, };
static const uint8_t fuji_header[] = { 'F', 'U', 'J', 'I', };
static const uint8_t nikon_header[] = { 'N', 'i', 'k', 'o', 'n', 0, };
static const uint8_t olympus1_header[] = { 'O', 'L', 'Y', 'M', 'P', 0, };
static const uint8_t olympus2_header[] = { 'O', 'L', 'Y', 'M', 'P', 'U', 'S', 0, 'I', 'I', };
static const uint8_t panasonic_header[] = { 'P', 'a', 'n', 'a', 's', 'o', 'n', 'i', 'c', 0, 0, 0, };
static const uint8_t sigma_header[] = { 'S', 'I', 'G', 'M', 'A', 0, 0, 0, };
static const uint8_t sony_header[] = { 'S', 'O', 'N', 'Y', ' ', 'D', 'S', 'C', ' ', 0, 0, 0, };

/*
 * dervied from Exiv2 MakerNote's article
 * https://exiv2.org/makernote.html or archived at
 * https://web.archive.org/web/20250311155857/https://exiv2.org/makernote.html
 */
static int exif_get_makernote_offset(GetByteContext *gb) {
    if (bytestream2_get_bytes_left(gb) < 12)
        return -1;
    if (!memcmp(gb->buffer, aoc_header, sizeof(aoc_header))) {
        return 6;
    } else if (!memcmp(gb->buffer, casio_header, sizeof(casio_header))) {
        return -1;
    } else if (!memcmp(gb->buffer, foveon_header, sizeof(foveon_header))) {
        return 10;
    } else if (!memcmp(gb->buffer, fuji_header, sizeof(fuji_header))) {
        return -1;
    } else if (!memcmp(gb->buffer, nikon_header, sizeof(nikon_header))) {
        if (bytestream2_get_bytes_left(gb) < 14)
            return -1;
        else if (AV_RB32(gb->buffer + 10) == 0x49492a00 || AV_RB32(gb->buffer + 10) == 0x4d4d002a)
            return -1;
        return 8;
    } else if (!memcmp(gb->buffer, olympus1_header, sizeof(olympus1_header)))  {
        return 8;
    } else if (!memcmp(gb->buffer, olympus2_header, sizeof(olympus2_header))) {
        return -1;
    } else if (!memcmp(gb->buffer, panasonic_header, sizeof(panasonic_header))) {
        return 12;
    } else if (!memcmp(gb->buffer, sigma_header, sizeof(sigma_header))) {
        return 10;
    } else if (!memcmp(gb->buffer, sony_header, sizeof(sony_header))) {
        return 12;
    }

    return 0;
}

static int exif_parse_ifd_list(void *logctx, GetByteContext *gb, int le,
                               int depth, AVExifMetadata *ifd);

static int exif_decode_tag(void *logctx, GetByteContext *gb, int le,
                           int depth, AVExifEntry *entry)
{
    int ret, cur_pos, makernote_offset = -1, tell;
    unsigned id, count;
    enum AVTiffDataType type;

    /* safety check to prevent infinite recursion on malicious IFDs */
    if (depth > 3)
        return AVERROR_INVALIDDATA;

    tell = bytestream2_tell(gb);

    ff_tread_tag(gb, le, &id, &type, &count, &cur_pos);

    entry->id = id;
    entry->type = type;

    av_log(logctx, AV_LOG_DEBUG, "TIFF Tag: id: 0x%04x, type: %d, count: %u, offset: %d\n", id, type, count, tell);

    if (!bytestream2_tell(gb)) {
        bytestream2_seek(gb, cur_pos, SEEK_SET);
        return 0;
    }

    if (id == MAKERNOTE_TAG) {
        tell = bytestream2_tell(gb);
        makernote_offset = exif_get_makernote_offset(gb);
    }
    if (id != MAKERNOTE_TAG && (type == AV_TIFF_IFD || ff_tis_ifd(id)) || makernote_offset >= 0) {
        entry->type = AV_TIFF_IFD;
        entry->count = 1;
        entry->ifd_offset = makernote_offset > 0 ? makernote_offset : 0;
        if (entry->ifd_offset) {
            entry->ifd_lead = av_malloc(entry->ifd_offset);
            if (!entry->ifd_lead)
                return AVERROR(ENOMEM);
            bytestream2_get_buffer(gb, entry->ifd_lead, entry->ifd_offset);
        }
        ret = exif_parse_ifd_list(logctx, gb, le, depth + 1, &entry->value.ifd);
        if (ret < 0 && id == MAKERNOTE_TAG) {
            /*
             * we guessed that MakerNote was an IFD
             * but we were probably incorrect at this
             * point so we try again as a binary blob
             */
            av_exif_free(&entry->value.ifd);
            av_log(logctx, AV_LOG_DEBUG, "unrecognized MakerNote IFD, retrying as blob\n");
            entry->type = type == AV_TIFF_IFD ? AV_TIFF_UNDEFINED : type;
            entry->count = count;
            bytestream2_seek(gb, tell, SEEK_SET);
            ret = exif_read_values(logctx, gb, le, entry);
        }
    } else {
        if (type == AV_TIFF_IFD)
            type = AV_TIFF_UNDEFINED;
        entry->count = count;
        ret = exif_read_values(logctx, gb, le, entry);
    }

    bytestream2_seek(gb, cur_pos, SEEK_SET);

    return ret;
}

static int exif_parse_ifd_list(void *logctx, GetByteContext *gb, int le,
                               int depth, AVExifMetadata *ifd)
{
    uint32_t entries = ff_tget_short(gb, le);
    if (bytestream2_get_bytes_left(gb) < entries * 12)
        return AVERROR_INVALIDDATA;

    ifd->count = entries;
    ifd->entries = av_calloc(ifd->count, sizeof(*ifd->entries));
    if (!ifd->entries)
        return AVERROR(ENOMEM);

    for (uint32_t i = 0; i < entries; i++) {
        int ret = exif_decode_tag(logctx, gb, le, depth, &ifd->entries[i]);
        if (ret < 0)
            return ret;
    }

    // return next IDF offset or 0x000000000 or a value < 0 for failure
    return ff_tget_long(gb, le);
}

void av_exif_free(AVExifMetadata *ifd) {
    if (!ifd->entries)
        return;
    for (size_t i = 0; i < ifd->count; i++) {
        if (ifd->entries[i].type == AV_TIFF_IFD)
            av_exif_free(&ifd->entries[i].value.ifd);
        else
            av_freep(&ifd->entries[i].value.ptr);
        av_freep(&ifd->entries[i].ifd_lead);
    }
    av_freep(&ifd->entries);
    ifd->count = 0;
}

static size_t exif_get_ifd_size(const AVExifMetadata *ifd)
{
    size_t total_size = 2;
    for (size_t i = 0; i < ifd->count; i++) {
        const AVExifEntry *entry = &ifd->entries[i];
        if (entry->type == AV_TIFF_IFD) {
            total_size += 12 + exif_get_ifd_size(&entry->value.ifd) + entry->ifd_offset;
        } else {
            size_t payload_size = entry->type == AV_TIFF_STRING ? entry->count : type_sizes[entry->type] * entry->count;
            if (payload_size > 4)
                total_size += 12 + payload_size;
            else
                total_size += 12;
        }
    }
    return total_size;
}

static int exif_write_ifd(void *logctx, PutByteContext *pb, int le, const AVExifMetadata *ifd)
{
    int offset, ret, tell;
    tell = bytestream2_get_bytes_left_p(pb);
    tput16(pb, le, ifd->count);
    offset = tell + 2 + 12 * (uint32_t) ifd->count;
    for (uint16_t i = 0; i < ifd->count; i++) {
        const AVExifEntry *entry = &ifd->entries[i];
        tput16(pb, le, entry->id);
        tput16(pb, le, entry->type);
        tput32(pb, le, entry->count);
        if (entry->type == AV_TIFF_IFD) {
            int tell = bytestream2_tell_p(pb);
            tput32(pb, le, offset);
            bytestream2_seek_p(pb, offset, SEEK_SET);
            if (entry->ifd_offset)
                bytestream2_put_buffer(pb, entry->ifd_lead, entry->ifd_offset);
            ret = exif_write_ifd(logctx, pb, le, &entry->value.ifd);
            if (ret < 0)
                return ret;
            offset += ret + entry->ifd_offset;
            bytestream2_seek_p(pb, tell + 4, SEEK_SET);
        } else {
            size_t payload_size = entry->type == AV_TIFF_STRING ? entry->count : type_sizes[entry->type] * entry->count;
            if (payload_size > 4) {
                int tell = bytestream2_tell_p(pb);
                tput32(pb, le, offset);
                bytestream2_seek_p(pb, offset, SEEK_SET);
                exif_write_values(pb, le, entry);
                offset += payload_size;
                bytestream2_seek_p(pb, tell + 4, SEEK_SET);
            } else {
                exif_write_values(pb, le, entry);
            }
        }
    }

    return offset - tell;
}

int av_exif_write(void *logctx, const AVExifMetadata *ifd, AVBufferRef **buffer)
{
    AVBufferRef *buf = NULL;
    size_t size;
    PutByteContext pb;
    int ret;

    if (*buffer)
        return AVERROR(EINVAL);

    size = 8 + exif_get_ifd_size(ifd);
    buf = av_buffer_alloc(size);
    if (!buf)
        return AVERROR(ENOMEM);

    bytestream2_init_writer(&pb, buf->data, buf->size);

    bytestream2_put_le16(&pb, AV_RL16("II"));
    bytestream2_put_le16(&pb, 42);
    bytestream2_put_le32(&pb, 8);

    ret = exif_write_ifd(logctx, &pb, 1, ifd);
    if (ret < 0) {
        av_buffer_unref(&buf);
        return ret;
    }

    *buffer = buf;

    return 0;
}

int av_exif_parse_buffer(void *logctx, const uint8_t *buf, size_t size,
                         AVExifMetadata *ifd, enum AVExifParseMode parse_mode)
{
    int ret, le;
    GetByteContext gbytes;
    if (size > INT_MAX)
        return AVERROR(EINVAL);
    bytestream2_init(&gbytes, buf, size);
    if (parse_mode == AV_EXIF_PARSE_TIFF_HEADER) {
        int ifd_offset;
        // read TIFF header
        ret = ff_tdecode_header(&gbytes, &le, &ifd_offset);
        if (ret < 0) {
            av_log(logctx, AV_LOG_ERROR, "invalid TIFF header in EXIF data\n");
            return ret;
        }
        bytestream2_seek(&gbytes, ifd_offset, SEEK_SET);
    } else {
        le = parse_mode == AV_EXIF_ASSUME_LE;
    }

    // read 0th IFD and store the metadata
    // (return values > 0 indicate the presence of subimage metadata)
    ret = exif_parse_ifd_list(logctx, &gbytes, le, 0, ifd);
    if (ret < 0) {
        av_exif_free(ifd);
        av_log(logctx, AV_LOG_ERROR, "error decoding EXIF data\n");
        return ret;
    }

    return bytestream2_tell(&gbytes);
}

static int attach_displaymatrix(void *logctx, AVFrame *frame, int orientation)
{
    AVFrameSideData *sd;
    int32_t *matrix;
    /* invalid orientation */
    if (orientation < 2 || orientation > 8)
        return 0;
    sd = av_frame_new_side_data(frame, AV_FRAME_DATA_DISPLAYMATRIX, sizeof(int32_t) * 9);
    if (!sd) {
        av_log(logctx, AV_LOG_ERROR, "Could not allocate frame side data\n");
        return AVERROR(ENOMEM);
    }
    matrix = (int32_t *) sd->data;

    switch (orientation) {
    case 2:
        av_display_rotation_set(matrix, 0.0);
        av_display_matrix_flip(matrix, 1, 0);
        break;
    case 3:
        av_display_rotation_set(matrix, 180.0);
        break;
    case 4:
        av_display_rotation_set(matrix, 180.0);
        av_display_matrix_flip(matrix, 1, 0);
        break;
    case 5:
        av_display_rotation_set(matrix, 90.0);
        av_display_matrix_flip(matrix, 1, 0);
        break;
    case 6:
        av_display_rotation_set(matrix, 90.0);
        break;
    case 7:
        av_display_rotation_set(matrix, -90.0);
        av_display_matrix_flip(matrix, 1, 0);
        break;
    case 8:
        av_display_rotation_set(matrix, -90.0);
        break;
    default:
        av_assert0(0);
    }

    return 0;
}

static inline const char *column_sep(uint32_t i, uint32_t c)
{
    return i ? (i % c ? ", " : "\n") : "";
}

static int exif_ifd_to_dict(void *logctx, const char *prefix, const AVExifMetadata *ifd, AVDictionary **metadata)
{
    AVBPrint bp;
    int ret = 0;
    char *key = NULL;
    char *value = NULL;

    if (!prefix)
        prefix = "";

    for (uint16_t i = 0; i < ifd->count; i++) {
        const AVExifEntry *entry = &ifd->entries[i];
        const char *name = exif_get_tag_name(entry->id);
        char buf[7];
        if (!name) {
            snprintf(buf, sizeof(buf), "0x%04X", entry->id);
            name = buf;
        }
        av_bprint_init(&bp, entry->count * 10, AV_BPRINT_SIZE_UNLIMITED);
        av_bprintf(&bp, "%s%s%s", prefix, *prefix ? "/" : "", name);
        ret = av_bprint_finalize(&bp, &key);
        if (ret < 0)
            goto end;
        av_bprint_init(&bp, entry->count * 10, AV_BPRINT_SIZE_UNLIMITED);
        switch (entry->type) {
            case AV_TIFF_IFD:
                ret = exif_ifd_to_dict(logctx, key, &entry->value.ifd, metadata);
                if (ret < 0)
                    goto end;
                break;
            case AV_TIFF_SHORT:
            case AV_TIFF_LONG:
                for (uint32_t j = 0; j < entry->count; j++)
                    av_bprintf(&bp, "%s%7" PRIu32, column_sep(j, 8), (uint32_t)entry->value.uint[j]);
                break;
            case AV_TIFF_SSHORT:
            case AV_TIFF_SLONG:
                for (uint32_t j = 0; j < entry->count; j++)
                    av_bprintf(&bp, "%s%7" PRId32, column_sep(j, 8), (int32_t)entry->value.sint[j]);
                break;
            case AV_TIFF_RATIONAL:
            case AV_TIFF_SRATIONAL:
                for (uint32_t j = 0; j < entry->count; j++)
                    av_bprintf(&bp, "%s%7i:%-7i", column_sep(j, 4), entry->value.rat[j].num, entry->value.rat[j].den);
                break;
            case AV_TIFF_DOUBLE:
            case AV_TIFF_FLOAT:
                for (uint32_t j = 0; j < entry->count; j++)
                    av_bprintf(&bp, "%s%.15g", column_sep(j, 4), entry->value.dbl[j]);
                break;
            case AV_TIFF_STRING:
                av_bprintf(&bp, "%s", entry->value.str);
                break;
            case AV_TIFF_UNDEFINED:
            case AV_TIFF_BYTE:
                for (uint32_t j = 0; j < entry->count; j++)
                    av_bprintf(&bp, "%s%3i", column_sep(j, 16), entry->value.ubytes[j]);
                break;
            case AV_TIFF_SBYTE:
                for (uint32_t j = 0; j < entry->count; j++)
                    av_bprintf(&bp, "%s%3i", column_sep(j, 16), entry->value.sbytes[j]);
                break;
        }
        if (entry->type != AV_TIFF_IFD) {
            if (!av_bprint_is_complete(&bp)) {
                av_bprint_finalize(&bp, NULL);
                ret = AVERROR(ENOMEM);
                goto end;
            }
            ret = av_bprint_finalize(&bp, &value);
            if (ret < 0)
                goto end;
            ret = av_dict_set(metadata, key, value, AV_DICT_DONT_STRDUP_KEY | AV_DICT_DONT_STRDUP_VAL);
            if (ret < 0)
                goto end;
            key = NULL;
            value = NULL;
        } else {
            av_freep(&key);
        }
    }

end:
    av_freep(&key);
    av_freep(&value);
    return ret;
}

int av_exif_ifd_to_dict(void *logctx, const AVExifMetadata *ifd, AVDictionary **metadata)
{
    return exif_ifd_to_dict(logctx, "", ifd, metadata);
}

int avpriv_exif_decode_ifd(void *logctx, const uint8_t *buf, int size,
                           int le, int depth, AVDictionary **metadata)
{
    AVExifMetadata ifd = { 0 };
    GetByteContext gb;
    int ret;
    bytestream2_init(&gb, buf, size);
    ret = exif_parse_ifd_list(logctx, &gb, le, depth, &ifd);
    if (ret < 0)
        return ret;
    ret = av_exif_ifd_to_dict(logctx, &ifd, metadata);
    av_exif_free(&ifd);
    return ret;
}

static int exif_attach_ifd(void *logctx, AVFrame *frame, AVExifMetadata *ifd, AVBufferRef *og)
{
    AVExifEntry *orient = NULL;
    AVBufferRef *buffer = og;
    AVFrameSideData *sd;
    int ret;

    for (size_t i = 0; i < ifd->count; i++) {
        if (ifd->entries[i].id == ORIENTATION_TAG && ifd->entries[i].count > 0) {
            orient = &ifd->entries[i];
            break;
        }
    }

    if (orient && orient->value.uint[0] > 1) {
        ret = attach_displaymatrix(logctx, frame, orient->value.uint[0]);
        if (ret < 0) {
            av_log(logctx, AV_LOG_WARNING, "Unable to attach displaymatrix from EXIF\n");
        } else {
            orient->value.uint[0] = 1;
            buffer = NULL;
        }
    }

    ret = av_exif_ifd_to_dict(logctx, ifd, &frame->metadata);
    if (ret < 0)
        return ret;

    if (!buffer) {
        ret = av_exif_write(logctx, ifd, &buffer);
        if (ret < 0)
            goto end;
    }

    sd = av_frame_new_side_data_from_buf(frame, AV_FRAME_DATA_EXIF, buffer);
    if (!sd) {
        if (buffer != og)
            av_buffer_unref(&buffer);
        ret = AVERROR(ENOMEM);
        goto end;
    }

    ret = 0;

end:
    if (og && ret >= 0 && buffer != og)
        av_buffer_unref(&og);
    return ret;
}

int ff_exif_attach_ifd(void *logctx, AVFrame *frame, AVExifMetadata *ifd)
{
    return exif_attach_ifd(logctx, frame, ifd, NULL);
}

int ff_exif_attach_buffer(void *logctx, AVFrame *frame, AVBufferRef *data)
{
    int ret;
    AVExifMetadata ifd = { 0 };

    ret = av_exif_parse_buffer(logctx, data->data, data->size, &ifd, AV_EXIF_PARSE_TIFF_HEADER);
    if (ret < 0)
        goto end;

    ret = exif_attach_ifd(logctx, frame, &ifd, data);

end:
    av_exif_free(&ifd);
    return ret;
}
