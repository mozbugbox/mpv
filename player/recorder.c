/*
 * This file is part of mpv.
 *
 * mpv is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * mpv is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with mpv.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <libavformat/avformat.h>

#include "common/av_common.h"
#include "common/common.h"
#include "common/global.h"
#include "common/msg.h"
#include "demux/packet.h"
#include "demux/stheader.h"

#include "recorder.h"

struct mp_recorder {
    struct mpv_global *global;
    struct mp_log *log;

    struct mp_recstream **streams;
    int num_streams;

    bool opened;

    // The start timestamp of the currently recorded segment (the timestamp of
    // the first packet of the incoming packet stream).
    double base_ts;
    // The output packet timestamp corresponding to base_ts. It's the timestamp
    // of the first packet of the current segment written to the output.
    double rebase_ts;

    AVFormatContext *mux;
};

struct mp_recorder_sink {
    struct mp_recorder *owner;
    struct mp_recstream *stream;
};

struct mp_recstream {
    struct sh_stream *sh;
    AVStream *av_stream;
    double last_out_dts;
    struct mp_recorder_sink sink;
};

static enum AVMediaType mp_to_av_stream_type(int type)
{
    switch (type) {
    case STREAM_VIDEO: return AVMEDIA_TYPE_VIDEO;
    case STREAM_AUDIO: return AVMEDIA_TYPE_AUDIO;
    case STREAM_SUB:   return AVMEDIA_TYPE_SUBTITLE;
    default:           return AVMEDIA_TYPE_UNKNOWN;
    }
}

static int add_stream(struct mp_recorder *priv, struct sh_stream *sh)
{
    enum AVMediaType av_type = mp_to_av_stream_type(sh->type);
    if (av_type == AVMEDIA_TYPE_UNKNOWN)
        return -1;

    struct mp_recstream *rst = talloc(priv, struct mp_recstream);
    *rst = (struct mp_recstream) {
        .sh = sh,
        .av_stream = avformat_new_stream(priv->mux, NULL),
        .last_out_dts = MP_NOPTS_VALUE,
        .sink = {
            .owner = priv,
            .stream = rst,
        },
    };

    if (!rst->av_stream)
        return -1;

    AVCodecParameters *avp = rst->av_stream->codecpar;
    struct mp_codec_params *c = sh->codec;

    if (c->lav_codecpar) {
        // Just use what demux_lavf.c gives us.
        if (avcodec_parameters_copy(avp, c->lav_codecpar) < 0)
            return -1;
    } else {
        avp->codec_type = av_type;
        avp->codec_id = mp_codec_to_av_codec_id(c->codec);
        avp->codec_tag = c->codec_tag;
        avp->extradata =
            av_mallocz(c->extradata_size + FF_INPUT_BUFFER_PADDING_SIZE);
        if (!avp->extradata)
            return -1;
        avp->extradata_size = c->extradata_size;
        memcpy(avp->extradata, c->extradata, c->extradata_size);

        avp->width = c->disp_w;
        avp->height = c->disp_h;
        avp->bits_per_coded_sample = c->bits_per_coded_sample;

        avp->sample_rate = c->samplerate;
        avp->bit_rate = c->bitrate;
        avp->block_align = c->block_align;
        avp->bits_per_coded_sample = c->bits_per_coded_sample;
        avp->channels = c->channels.num;
    }

    if (avp->codec_id == AV_CODEC_ID_NONE)
        return -1;

    rst->av_stream->time_base = mp_get_codec_timebase(c);

    MP_TARRAY_APPEND(priv, priv->streams, priv->num_streams, rst);
    return 0;
}

struct mp_recorder *mp_recorder_create(struct mpv_global *global,
                                       const char *target_file,
                                       struct sh_stream **streams,
                                       int num_streams)
{
    struct mp_recorder *priv = talloc_zero(NULL, struct mp_recorder);

    priv->global = global;
    priv->log = mp_log_new(priv, global->log, "recorder");

    if (!num_streams) {
        MP_ERR(priv, "No streams.\n");
        goto error;
    }

    priv->mux = avformat_alloc_context();
    if (!priv->mux)
        goto error;

    priv->mux->oformat = av_guess_format(NULL, target_file, NULL);
    if (!priv->mux->oformat) {
        MP_ERR(priv, "Output format not found.\n");
        goto error;
    }

    if (avio_open2(&priv->mux->pb, target_file, AVIO_FLAG_WRITE, NULL, NULL) < 0) {
        MP_ERR(priv, "Failed opening output file.\n");
        goto error;
    }

    for (int n = 0; n < num_streams; n++) {
        if (add_stream(priv, streams[n]) < 0) {
            MP_ERR(priv, "Can't mux one of the input streams.\n");
            goto error;
        }
    }

    if (avformat_write_header(priv->mux, NULL) < 0) {
        MP_ERR(priv, "Write header failed.\n");
        goto error;
    }

    priv->opened = true;

    priv->base_ts = MP_NOPTS_VALUE;
    priv->rebase_ts = 0;

    return priv;

error:
    mp_recorder_destroy(priv);
    return NULL;
}

void mp_recorder_destroy(struct mp_recorder *priv)
{
    // TODO: delete possibly created file if it's broken?

    if (priv->opened) {
        if (av_write_trailer(priv->mux) < 0)
            MP_ERR(priv, "Writing trailer failed.\n");
    }

    if (priv->mux) {
        if (avio_closep(&priv->mux->pb) < 0)
            MP_ERR(priv, "Closing file failed\n");

        avformat_free_context(priv->mux);
    }

    talloc_free(priv);
}

// This is called on a seek, or when recording was started mid-stream.
void mp_recorder_mark_discontinuity(struct mp_recorder *priv)
{
    double max_ts = MP_NOPTS_VALUE;
    for (int n = 0; n < priv->num_streams; n++) {
        struct mp_recstream *rst = priv->streams[n];
        if (rst->last_out_dts > max_ts)
            max_ts = rst->last_out_dts;
    }
    priv->base_ts = MP_NOPTS_VALUE;
    priv->rebase_ts = max_ts == MP_NOPTS_VALUE ? 0 : max_ts;
    MP_WARN(priv, "discontinuity at %f\n", priv->rebase_ts);
}

// Get a stream for writing. The pointer is valid until mp_recorder is
// destroyed. The stream is the index referencing the stream passed to
// mp_recorder_create().
struct mp_recorder_sink *mp_recorder_get_sink(struct mp_recorder *r, int stream)
{
    assert(stream >= 0 && stream < r->num_streams);
    return &r->streams[stream]->sink;
}

// Pass a packet to the given stream. The function does not own the packet, but
// can create a new reference to it if it needs to retain it. Can be NULL to
// signal proper end of stream.
void mp_recorder_feed_packet(struct mp_recorder_sink *s,
                             struct demux_packet *pkt)
{
    struct mp_recorder *priv = s->owner;
    struct mp_recstream *rst = s->stream;

    if (!pkt)
        return;

    struct demux_packet mpkt = *pkt;

    if (priv->base_ts == MP_NOPTS_VALUE) {
        priv->base_ts = mpkt.dts;
        MP_WARN(priv, "Rebasing stream %p to %f\n", rst, priv->base_ts);
    }

    double diff = priv->rebase_ts - priv->base_ts;
    mpkt.pts += diff;
    mpkt.dts += diff;

    rst->last_out_dts = mpkt.dts;

    AVPacket avpkt;
    mp_set_av_packet(&avpkt, &mpkt, &rst->av_stream->time_base);

    avpkt.stream_index = rst->av_stream->index;

    AVPacket *new_packet = av_packet_clone(&avpkt);
    if (!new_packet) {
        MP_ERR(priv, "Failed to allocate packet.\n");
        return;
    }

    if (av_interleaved_write_frame(priv->mux, new_packet) < 0)
        MP_ERR(priv, "Failed writing packet.\n");
}
