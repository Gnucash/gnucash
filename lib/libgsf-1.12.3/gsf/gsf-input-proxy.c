/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-input-proxy.c: proxy object (with its own current position)
 *
 * Copyright (C) 2004 Morten Welinder (terra@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <gsf-config.h>
#include <gsf/gsf-input-proxy.h>
#include <gsf/gsf-input-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>

static GObjectClass *parent_class;

struct _GsfInputProxy {
	GsfInput input;

	GsfInput *source;
	gsf_off_t offset;
};

typedef struct {
	GsfInputClass input_class;
} GsfInputProxyClass;


/**
 * gsf_input_proxy_new_section :
 * @source : The underlying data source.
 * @offset : Offset into source for start of section.
 * @size : Length of section.
 *
 * This creates a new proxy to a section of the given source.  The new
 * object will have its own current position, but any operation on it
 * can change the source's position.
 *
 * If a proxy to a proxy is created, the intermediate proxy is short-
 * circuited.
 *
 * This function will ref the source.
 *
 * Returns a new input object.
 **/
GsfInput *
gsf_input_proxy_new_section (GsfInput *source,
			     gsf_off_t offset,
			     gsf_off_t size)
{
	GsfInputProxy *proxy;
	gsf_off_t source_size;

	g_return_val_if_fail (GSF_IS_INPUT (source), NULL);
	g_return_val_if_fail (offset >= 0, NULL);

	source_size = gsf_input_size (source);
	g_return_val_if_fail (offset <= source_size, NULL);
	g_return_val_if_fail (size <= source_size - offset, NULL);

	proxy = g_object_new (GSF_INPUT_PROXY_TYPE, NULL);
	proxy->offset = offset;
	gsf_input_set_size (GSF_INPUT (proxy), size);

	/* Short-circuit multiple proxies.  */
	if (GSF_IS_INPUT_PROXY (source)) {
		GsfInputProxy *proxy_source = GSF_INPUT_PROXY (source);
		proxy->offset += proxy_source->offset;
		source = proxy_source->source;
	}

	proxy->source = g_object_ref (source);
	return GSF_INPUT (proxy);
}

/**
 * gsf_input_proxy_new :
 * @source : The underlying data source.
 *
 * This creates a new proxy to the entire, given input source.  See
 * gsf_input_proxy_new_section for details.
 *
 * Returns a new input object.
 **/
GsfInput *
gsf_input_proxy_new (GsfInput *source)
{
	return gsf_input_proxy_new_section (source, 0, gsf_input_size (source));
}

static void
gsf_input_proxy_finalize (GObject *obj)
{
	GsfInputProxy *proxy = (GsfInputProxy *)obj;

	if (proxy->source != NULL) {
		g_object_unref (G_OBJECT (proxy->source));
		proxy->source = NULL;
	}

	parent_class->finalize (obj);
}

static GsfInput *
gsf_input_proxy_dup (GsfInput *src_input, G_GNUC_UNUSED GError **err)
{
	return gsf_input_proxy_new (src_input);
}

static guint8 const *
gsf_input_proxy_read (GsfInput *input, size_t num_bytes, guint8 *buffer)
{
	GsfInputProxy *proxy = GSF_INPUT_PROXY (input);

	/* Seek to our position in the source.  */
	if (gsf_input_seek (proxy->source,
			    proxy->offset + gsf_input_tell (input),
			    G_SEEK_SET))
		return NULL;

	/* Read the data.  */
	return gsf_input_read (proxy->source, num_bytes, buffer);
}

static gboolean
gsf_input_proxy_seek (G_GNUC_UNUSED GsfInput *input,
		      G_GNUC_UNUSED gsf_off_t offset,
		      G_GNUC_UNUSED GSeekType whence)
{
	return FALSE;
}

static void
gsf_input_proxy_init (GObject *obj)
{
	GsfInputProxy *proxy = GSF_INPUT_PROXY (obj);

	proxy->source = NULL;
	proxy->offset = 0;
}

static void
gsf_input_proxy_class_init (GObjectClass *gobject_class)
{
	GsfInputClass *input_class = GSF_INPUT_CLASS (gobject_class);

	gobject_class->finalize = gsf_input_proxy_finalize;
	input_class->Dup	= gsf_input_proxy_dup;
	input_class->Read	= gsf_input_proxy_read;
	input_class->Seek	= gsf_input_proxy_seek;

	parent_class = g_type_class_peek_parent (gobject_class);
}

GSF_CLASS (GsfInputProxy, gsf_input_proxy,
	   gsf_input_proxy_class_init, gsf_input_proxy_init, GSF_INPUT_TYPE)
