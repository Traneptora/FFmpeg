/*
 * EXIF metadata parser
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
 * EXIF metadata parser
 * @author Thilo Borgmann <thilo.borgmann _at_ mail.de>
 * @author Leo Izen <leo.izen@gmail.com>
 */

#include "libavutil/display.h"

#include "exif_internal.h"
#include "tiff_common.h"

#define EXIF_TAG_NAME_LENGTH   32
#define MAKERNOTE_TAG 0x927c

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
    {"SubjectDistanceRange",       0xA40C}
//    {"InteroperabilityIndex",      0x1}, // <- Table 13 Interoperability IFD Attribute Information
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


static int exif_add_metadata(void *logctx, int count, int type,
                             const char *name, const char *sep,
                             GetByteContext *gb, int le,
                             AVDictionary **metadata)
{
    switch(type) {
    case TIFF_DOUBLE   : return ff_tadd_doubles_metadata(count, name, sep, gb, le, metadata);
    case TIFF_SSHORT   : return ff_tadd_shorts_metadata(count, name, sep, gb, le, 1, metadata);
    case TIFF_SHORT    : return ff_tadd_shorts_metadata(count, name, sep, gb, le, 0, metadata);
    case TIFF_SBYTE    : return ff_tadd_bytes_metadata(count, name, sep, gb, le, 1, metadata);
    case TIFF_BYTE     :
    case TIFF_UNDEFINED: return ff_tadd_bytes_metadata(count, name, sep, gb, le, 0, metadata);
    case TIFF_STRING   : return ff_tadd_string_metadata(count, name, gb, le, metadata);
    case TIFF_SRATIONAL:
    case TIFF_RATIONAL : return ff_tadd_rational_metadata(count, name, sep, gb, le, metadata);
    case TIFF_SLONG    :
    case TIFF_LONG     : return ff_tadd_long_metadata(count, name, sep, gb, le, metadata);
    default:
        av_log(logctx, AV_LOG_WARNING,
            "Invalid TIFF tag type %d found for %s with size %d\n", type, name, count);
        return 0;
    };
}

static int exif_parse_ifd_list(void *logctx, GetByteContext *gb, int le,
                               int depth, AVDictionary **metadata);

static int exif_decode_tag(void *logctx, GetByteContext *gbytes, int le,
                           int depth, AVDictionary **metadata)
{
    int ret, cur_pos;
    unsigned id, count;
    enum TiffTypes type;

    if (depth > 2) {
        return 0;
    }

    ff_tread_tag(gbytes, le, &id, &type, &count, &cur_pos);

    if (!bytestream2_tell(gbytes)) {
        bytestream2_seek(gbytes, cur_pos, SEEK_SET);
        return 0;
    }

    // read count values and add it metadata
    // store metadata or proceed with next IFD
    ret = ff_tis_ifd(id);
    if (ret) {
        ret = exif_parse_ifd_list(logctx, gbytes, le, depth + 1, metadata);
    } else {
        const char *name = exif_get_tag_name(id);
        char buf[7];

        if (!name) {
            name = buf;
            snprintf(buf, sizeof(buf), "0x%04X", id);
        }

        ret = exif_add_metadata(logctx, count, type, name, NULL,
                                gbytes, le, metadata);
    }

    bytestream2_seek(gbytes, cur_pos, SEEK_SET);

    return ret;
}

static const uint8_t casio_header[] = {
    'Q', 'V', 'C', 0, 0, 0,
};

static const uint8_t fuji_header[] = {
    'F', 'U', 'J', 'I',
};

static const uint8_t nikon_header[] = {
    'N', 'i', 'k', 'o', 'n', 0,
};

static const uint8_t olympus1_header[] = {
    'O', 'L', 'Y', 'M', 'P', 0,
};

static const uint8_t olympus2_header[] = {
    'O', 'L', 'Y', 'M', 'P', 'U', 'S', 0, 'I', 'I',
};

static const uint8_t panosonic_header[] = {
    'P', 'a', 'n', 'o', 's', 'o', 'n',  'i', 'c', 0, 0, 0,
};

static const uint8_t aoc_header[] = {
    'A', 'O', 'C', 0,
};

static const uint8_t sigma_header[] = {
    'S', 'I', 'G', 'M', 'A', 0, 0, 0,
};

static const uint8_t foveon_header[] = {
    'F', 'O', 'V', 'E', 'O', 'N', 0, 0,
};

static const uint8_t sony_header[] = {
    'S', 'O', 'N', 'Y', ' ', 'D', 'S', 'C', ' ', 0, 0, 0,
};


static int exif_get_makernote_offset(GetByteContext *gb) {
    if (!memcmp(gb->buffer, casio_header, sizeof(casio_header))) {
        return -1;
    } else if (!memcmp(gb->buffer, fuji_header, sizeof(fuji_header))) {
        return -1;
    } else if (!memcmp(gb->buffer, olympus2_header, sizeof(olympus2_header))) {
        return -1;
    } else if (!memcmp(gb->buffer, olympus1_header, sizeof(olympus1_header)))  {
        return 8;
    } else if (!memcmp(gb->buffer, nikon_header, sizeof(nikon_header))) {
        if (bytestream2_get_bytes_left(gb) < 14)
            return -1;
        else if (AV_RB32(gb->buffer + 10) == 0x49492a00 || AV_RB32(gb->buffer + 10) == 0x4d4d002a)
            return -1;
        return 8;
    } else if (!memcmp(gb->buffer, panosonic_header, sizeof(panosonic_header))) {
        return 12;
    } else if (!memcmp(gb->buffer, aoc_header, sizeof(aoc_header))) {
        return 6;
    } else if (!memcmp(gb->buffer, sigma_header, sizeof(sigma_header))) {
        return 10;
    } else if (!memcmp(gb->buffer, foveon_header, sizeof(foveon_header))) {
        return 10;
    } else if (!memcmp(gb->buffer, sony_header, sizeof(sony_header))) {
        return 12;
    }

    return 0;
}

static int exif_get_collect_size(void *logctx, GetByteContext *gb, int le, int depth)
{
    int entries, total_size = 2;
    GetByteContext gbytes;

    if (depth > 2)
        return 0;

    gbytes = *gb;
    entries = ff_tget_short(&gbytes, le);
    if (bytestream2_get_bytes_left(&gbytes) < entries * 12)
        return AVERROR_INVALIDDATA;

    for (int i = 0; i < entries; i++) {
        int cur_pos, makernote_ifd = -1;
        unsigned id, count;
        enum TiffTypes type;
        ff_tread_tag(&gbytes, le, &id, &type, &count, &cur_pos);
        if (!bytestream2_tell(&gbytes)) {
            bytestream2_seek(&gbytes, cur_pos, SEEK_SET);
            continue;
        }
        if (id == MAKERNOTE_TAG)
            makernote_ifd = exif_get_makernote_offset(&gbytes);
        if (id != MAKERNOTE_TAG && ff_tis_ifd(id) || makernote_ifd >= 0) {
            int makernote_off = makernote_ifd >= 0 ? makernote_ifd : 0;
            bytestream2_seek(&gbytes, makernote_off, SEEK_CUR);
            int ret = exif_get_collect_size(logctx, &gbytes, le, depth + 1);
            if (ret < 0)
                return ret;
            total_size += ret + 12 + makernote_off;
        } else {
            int payload_size = type == TIFF_STRING ? count : count * type_sizes[type];
            if (payload_size > 4)
                total_size += 12 + payload_size;
            else
                total_size += 12;
        }
        bytestream2_seek(&gbytes, cur_pos, SEEK_SET);
    }

    return total_size;
}

static inline void tput16(PutByteContext *pb, const int le, const uint16_t value)
{
    le ? bytestream2_put_le16(pb, value) : bytestream2_put_be16(pb, value);
}

static inline void tput32(PutByteContext *pb, const int le, const uint32_t value)
{
    le ? bytestream2_put_le32(pb, value) : bytestream2_put_be32(pb, value);
}

static int exif_collect_ifd_list(void *logctx, GetByteContext *gb, int le, int depth, PutByteContext *pb)
{
    int entries, ret = 0, offset;
    GetByteContext gbytes;

    if (depth > 2)
        return 0;

    gbytes = *gb;
    entries = ff_tget_short(&gbytes, le);
    if (bytestream2_get_bytes_left(&gbytes) < entries * 12)
        return AVERROR_INVALIDDATA;

    tput16(pb, le, entries);
    offset = bytestream2_tell_p(pb) + entries * 12;
    for (int i = 0; i < entries; i++) {
        int cur_pos, makernote_ifd = -1;
        unsigned id, count;
        enum TiffTypes type;
        ff_tread_tag(&gbytes, le, &id, &type, &count, &cur_pos);
        if (!bytestream2_tell(&gbytes)) {
            bytestream2_seek(&gbytes, cur_pos, SEEK_SET);
            continue;
        }
        if (bytestream2_get_bytes_left_p(pb) < 12)
            return AVERROR_BUFFER_TOO_SMALL;
        tput16(pb, le, id);
        tput16(pb, le, type);
        tput32(pb, le, count);
        if (id == MAKERNOTE_TAG)
            makernote_ifd = exif_get_makernote_offset(&gbytes);
        if (id != MAKERNOTE_TAG && ff_tis_ifd(id) || makernote_ifd >= 0) {
            int tell = bytestream2_tell_p(pb);
            int makernote_off = makernote_ifd >= 0 ? makernote_ifd : 0;
            tput32(pb, le, offset);
            bytestream2_seek_p(pb, offset, SEEK_SET);
            if (makernote_off)
                bytestream2_copy_buffer(pb, &gbytes, makernote_off);
            ret = exif_collect_ifd_list(logctx, &gbytes, le, depth + 1, pb);
            if (ret < 0)
                return ret;
            offset += ret + makernote_off;
            bytestream2_seek_p(pb, tell + 4, SEEK_SET);
        } else  {
            int payload_size = type == TIFF_STRING ? count : count * type_sizes[type];
            if (payload_size > 4) {
                int tell = bytestream2_tell_p(pb);
                tput32(pb, le, offset);
                bytestream2_seek_p(pb, offset, SEEK_SET);
                if (bytestream2_get_bytes_left(&gbytes) < payload_size)
                    return AVERROR_INVALIDDATA;
                bytestream2_put_buffer(pb, gbytes.buffer, payload_size);
                offset += payload_size;
                bytestream2_seek_p(pb, tell + 4, SEEK_SET);
            } else {
                bytestream2_put_ne32(pb, bytestream2_get_ne32(&gbytes));
            }
        }
        bytestream2_seek(&gbytes, cur_pos, SEEK_SET);
    }

    return offset;
}

int ff_exif_collect_ifd(void *logctx, GetByteContext *gb, int le, AVBufferRef **buffer)
{
    AVBufferRef *ref = NULL;
    int total_size, ret;
    PutByteContext pb;
    if (!buffer)
        return 0;

    total_size = exif_get_collect_size(logctx, gb, le, 0);
    if (total_size <= 0)
        return total_size;
    total_size += 8;
    ref = av_buffer_alloc(total_size);
    if (!ref)
        return AVERROR(ENOMEM);
    bytestream2_init_writer(&pb, ref->data, total_size);
    bytestream2_put_be32(&pb, le ? 0x49492a00 : 0x4d4d002a);
    tput32(&pb, le, 8);

    ret = exif_collect_ifd_list(logctx, gb, le, 0, &pb);
    if (ret < 0)
        av_buffer_unref(&ref);

    *buffer = ref;
    return ret;
}

static int exif_parse_ifd_list(void *logctx, GetByteContext *gb, int le,
                               int depth, AVDictionary **metadata)
{
    int entries = ff_tget_short(gb, le);
    if (bytestream2_get_bytes_left(gb) < entries * 12)
        return AVERROR_INVALIDDATA;

    for (int i = 0; i < entries; i++) {
        int ret = exif_decode_tag(logctx, gb, le, depth, metadata);
        if (ret < 0)
            return ret;
    }

    // return next IDF offset or 0x000000000 or a value < 0 for failure
    return ff_tget_long(gb, le);
}

int av_exif_parse_buffer(void *logctx, const uint8_t *buf, size_t size,
                         AVDictionary **metadata, enum AVExifParseMode parse_mode)
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
    ret = exif_parse_ifd_list(logctx, &gbytes, le, 0, metadata);
    if (ret < 0) {
        av_log(logctx, AV_LOG_ERROR, "error decoding EXIF data\n");
        return ret;
    }

    return bytestream2_tell(&gbytes);
}

static int attach_displaymatrix(void *logctx, AVFrame *frame, const char *value)
{
    char *endptr;
    AVFrameSideData *sd;
    long orientation = strtol(value, &endptr, 0);
    int32_t *matrix;
    /* invalid string */
    if (*endptr || endptr == value)
        return 0;
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

int ff_exif_attach(void *logctx, AVFrame *frame, AVBufferRef **data)
{
    const AVDictionaryEntry *e = NULL;
    int ret;
    AVDictionary *m = NULL;
    AVBufferRef *buffer = *data;
    AVFrameSideData *sd = av_frame_new_side_data_from_buf(frame, AV_FRAME_DATA_EXIF, buffer);
    if (!sd)
        return AVERROR(ENOMEM);
    *data = NULL;
    ret = av_exif_parse_buffer(logctx, buffer->data, buffer->size, &m, AV_EXIF_PARSE_TIFF_HEADER);
    if (ret < 0)
        return ret;

    if ((e = av_dict_get(m, "Orientation", e, AV_DICT_IGNORE_SUFFIX))) {
        ret = attach_displaymatrix(logctx, frame, e->value);
        if (ret < 0)
            return ret;
    }

    return 0;
}
