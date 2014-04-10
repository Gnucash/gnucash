/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-structured_blob.c : Utility storage to blob in/out a tree of data
 *
 * Copyright (C) 2002-2004 Jody Goldberg (jody@gnome.org)
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
#include <gsf/gsf-structured-blob.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-infile-impl.h>
#include <gsf/gsf-input.h>
#include <gsf/gsf-outfile.h>
#include <gsf/gsf-output.h>
#include <gsf/gsf-shared-memory.h>
#include <string.h>

static GObjectClass *parent_class;

struct _GsfStructuredBlob {
	GsfInfile base;

	GsfSharedMemory *data;
	GPtrArray *children;
}; 
typedef struct {
	GsfInfileClass base;
} GsfStructuredBlobClass;

static void
blob_finalize (GObject *obj)
{
	unsigned i;
	GsfStructuredBlob *blob = GSF_STRUCTURED_BLOB (obj);

	if (blob->data != NULL) {
		g_object_unref (G_OBJECT (blob->data));
		blob->data = NULL;
	}

	if (blob->children != NULL) {
		for (i = 0; i < blob->children->len ; i++)
			g_object_unref (g_ptr_array_index (blob->children, i));
		g_ptr_array_free (blob->children, TRUE);
		blob->children = NULL;
	}

	parent_class->finalize (obj);
}

static GsfInput *
blob_dup (GsfInput *input, G_GNUC_UNUSED GError **err)
{
	GsfStructuredBlob const *src = (GsfStructuredBlob *) input;
	GsfStructuredBlob *dst = g_object_new (GSF_STRUCTURED_BLOB_TYPE, NULL);

	if (src->data != NULL) {
		dst->data = src->data;
		g_object_ref (G_OBJECT (dst->data));
	}
	if (src->children != NULL) {
		unsigned i;
		gpointer child;

		dst->children = g_ptr_array_sized_new (src->children->len);
		g_ptr_array_set_size  (dst->children, src->children->len);
		for (i = 0; i < src->children->len ; i++) {
			child = g_ptr_array_index (src->children, i);
			g_ptr_array_index (dst->children, i) = child;
			g_object_ref (child);
		}
	}

	return GSF_INPUT (dst);
}

static guint8 const *
blob_read (GsfInput *input, size_t num_bytes, guint8 *optional_buffer)
{
	GsfStructuredBlob *blob = (GsfStructuredBlob *) input;
	guchar const *src = blob->data->buf;

	if (src == NULL)
		return NULL;
	if (optional_buffer) {
		memcpy (optional_buffer, src + input->cur_offset, num_bytes);
		return optional_buffer;
	} else
		return src + input->cur_offset;
}

static gboolean
blob_seek (G_GNUC_UNUSED GsfInput *input,
	   G_GNUC_UNUSED gsf_off_t offset,
	   G_GNUC_UNUSED GSeekType whence)
{
	return FALSE;
}

static int
blob_num_children (GsfInfile *infile)
{
	GsfStructuredBlob const *blob = (GsfStructuredBlob *) infile;

	if (blob->children != NULL)
		return blob->children->len;
	return -1;
}

static char const *
blob_name_by_index (GsfInfile *infile, int i)
{
	GsfStructuredBlob const *blob = (GsfStructuredBlob *) infile;
	if (blob->children != NULL) {
		g_return_val_if_fail (i < 0 || (unsigned)i >= blob->children->len, NULL);
		return gsf_input_name (g_ptr_array_index (blob->children, i));
	}
	return NULL;
}

static GsfInput   *
blob_child_by_index (GsfInfile *infile, int i, GError **err)
{
	GsfStructuredBlob const *blob = (GsfStructuredBlob *) infile;
	if (blob->children != NULL) {
		g_return_val_if_fail (i < 0 || (unsigned)i >= blob->children->len, NULL);
		return gsf_input_dup (g_ptr_array_index (blob->children, i), err);
	}
	return NULL;
}

static GsfInput   *
blob_child_by_name (GsfInfile *infile, char const *name, GError **err)
{
	GsfStructuredBlob const *blob = (GsfStructuredBlob *) infile;
	if (blob->children != NULL) {
		unsigned i;
		GsfInput *child;

		for (i = 0 ; i < blob->children->len ;) {
			child = g_ptr_array_index (blob->children, i);
			if (!strcmp (gsf_input_name (child), name))
				return gsf_input_dup (child, err);
		}
	}
	return NULL;
}

static void
gsf_structured_blob_init (GObject *obj)
{
	GsfStructuredBlob *blob = GSF_STRUCTURED_BLOB (obj);

	blob->data = NULL;
	blob->children = NULL;
}

static void
gsf_structured_blob_class_init (GObjectClass *gobject_class)
{
	GsfInputClass  *input_class  = GSF_INPUT_CLASS (gobject_class);
	GsfInfileClass *infile_class = GSF_INFILE_CLASS (gobject_class);

	gobject_class->finalize		= blob_finalize;
	input_class->Dup		= blob_dup;
	input_class->Read		= blob_read;
	input_class->Seek		= blob_seek;
	infile_class->num_children	= blob_num_children;
	infile_class->name_by_index	= blob_name_by_index;
	infile_class->child_by_index	= blob_child_by_index;
	infile_class->child_by_name	= blob_child_by_name;

	parent_class = g_type_class_peek_parent (gobject_class);
}

GSF_CLASS (GsfStructuredBlob, gsf_structured_blob,
	   gsf_structured_blob_class_init, gsf_structured_blob_init,
	   GSF_INFILE_TYPE)

/**
 * gsf_structured_blob_read :
 * @input : An input (potentially a GsfInfile) holding the blob
 *
 * Returns a freshly created tree of blobs
 **/
GsfStructuredBlob *
gsf_structured_blob_read (GsfInput *input)
{
	GsfStructuredBlob *blob;
	gsf_off_t content_size;
	int i = 0;

	g_return_val_if_fail (GSF_IS_INPUT (input), NULL);

	blob = g_object_new (GSF_STRUCTURED_BLOB_TYPE, NULL);

	content_size = gsf_input_remaining (input);
	if (content_size > 0) {
		guint8 *buf = (guint8*)g_try_malloc (content_size);

		if (buf == NULL) {
			g_warning ("Failed attempting to allocate %" GSF_OFF_T_FORMAT " bytes",
				   content_size);

			g_object_unref (G_OBJECT (blob));
			return NULL;
		}

		gsf_input_read (input, content_size, buf);
		blob->data = gsf_shared_memory_new (buf, content_size, TRUE);
	}

	gsf_input_set_name (GSF_INPUT (blob), gsf_input_name (input));

	if (GSF_IS_INFILE (input))
		i = gsf_infile_num_children (GSF_INFILE (input));
	if (i > 0) {
		GsfInput	  *child;
		GsfStructuredBlob *child_blob;

		blob->children = g_ptr_array_sized_new (i);
		g_ptr_array_set_size  (blob->children, i);
		while (i-- > 0) {
			child = gsf_infile_child_by_index (GSF_INFILE (input), i);
			child_blob = gsf_structured_blob_read (child);
			g_object_unref (G_OBJECT (child));

			g_ptr_array_index (blob->children, i) = child_blob;
#if 0
			/*
			 * We don't need this, and setting it causes circular
			 * links.
			 */
			gsf_input_set_container (GSF_INPUT (child_blob),
						 GSF_INFILE (blob));
#endif
		}
	}

	return blob;
}

/**
 * gsf_structured_blob_write :
 * @blob :
 * @container :
 *
 * Dumps structured blob @blob onto the @container.  Will fail if the output is
 * not an Outfile and blob has multiple streams.
 *
 * Returns : TRUE on success.
 **/
gboolean
gsf_structured_blob_write (GsfStructuredBlob *blob, GsfOutfile *container)
{
	GsfOutput *output;
	gboolean has_kids;

	g_return_val_if_fail (GSF_IS_STRUCTURED_BLOB (blob), FALSE);
	g_return_val_if_fail (GSF_IS_OUTFILE (container), FALSE);

	has_kids = (blob->children != NULL && blob->children->len > 0);

	output = gsf_outfile_new_child  (GSF_OUTFILE (container),
		gsf_input_name (GSF_INPUT (blob)),
		has_kids);
	if (has_kids) {
		GsfStructuredBlob *child_blob;
		unsigned i;

		for (i = 0 ; i < blob->children->len ; i++) {
			child_blob = g_ptr_array_index (blob->children, i);
			if (!gsf_structured_blob_write (child_blob, GSF_OUTFILE (output)))
				return FALSE;
		}
	}

	if (blob->data != NULL)
		gsf_output_write (output, blob->data->size, blob->data->buf);
	gsf_output_close (output);
	g_object_unref (G_OBJECT (output));

	return TRUE;
}
