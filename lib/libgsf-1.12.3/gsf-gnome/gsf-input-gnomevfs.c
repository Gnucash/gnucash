/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-input-gnomevfs.c: 
 *
 * Copyright (C) 2002-2004 Dom Lachowicz (cinamod@hotmail.com)
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
#include <string.h>

#include <gsf-gnome/gsf-input-gnomevfs.h>
#include <gsf/gsf-input-memory.h>
#include <gsf/gsf-input-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <libgnomevfs/gnome-vfs-method.h>

struct _GsfInputGnomeVFS {
	GsfInput input;

	GnomeVFSHandle *handle;
	GnomeVFSURI    *uri;

        guint8   *buf;
        size_t   buf_size;
};

typedef GsfInputClass GsfInputGnomeVFSClass;

/**
 * gsf_input_gnomevfs_new_uri:
 * @uri : uri you wish to open.
 * @err	: optionally NULL.
 *
 * Returns a new input or NULL.
 **/
GsfInput *
gsf_input_gnomevfs_new_uri (GnomeVFSURI *uri, GError **error)
{
	GnomeVFSHandle	 *handle;
	GnomeVFSFileInfo *info;
	GnomeVFSResult    res;
	GnomeVFSFileType  type;
	gsf_off_t	  size;
	gboolean          is_local;

	if (uri == NULL) {
		g_set_error (error, gsf_input_error_id (), 0,
			     "Filename/URI cannot be NULL");
		return NULL;
	}

	if (!VFS_METHOD_HAS_FUNC (uri->method, seek))
		goto make_local_copy;

	info = gnome_vfs_file_info_new ();
	res = gnome_vfs_get_file_info_uri (uri, info, GNOME_VFS_FILE_INFO_DEFAULT | GNOME_VFS_FILE_INFO_FOLLOW_LINKS);

	size = (gsf_off_t)info->size;
	type = info->type;
	is_local = GNOME_VFS_FILE_INFO_LOCAL (info);
	gnome_vfs_file_info_unref (info);

	switch (res) {
	case GNOME_VFS_ERROR_NOT_SUPPORTED:
		goto make_local_copy;
	default:
		g_set_error (error, gsf_input_error_id (), (gint) res,
			     gnome_vfs_result_to_string (res));
		return NULL;
	case GNOME_VFS_OK: /* Nothing */ ;
	}

	if (type != GNOME_VFS_FILE_TYPE_REGULAR) {
#if 0
		g_print ("uri=%s\n", gnome_vfs_uri_to_string (uri, GNOME_VFS_URI_HIDE_NONE));
		g_print ("uri.text=%s\n", uri->text);
		g_print ("parent=%s\n", uri->parent ? gnome_vfs_uri_to_string (uri->parent, GNOME_VFS_URI_HIDE_NONE) : "(null)");
		g_print ("method=%s\n", uri->method_string);
		g_print ("fragment=%s\n", uri->fragment_id ? uri->fragment_id : "(null)");
#endif
		if (type == GNOME_VFS_FILE_TYPE_DIRECTORY && uri->parent) {
			/* Reported for "file:///.../foo.zip#zip:beta.gnumeric" */
			goto make_local_copy;
		}

		g_set_error (error, gsf_input_error_id (), 0,
			     "Not a regular file");
		return NULL;
	}

	/* Make copies of small files.  */
	if (!is_local && size < (256 << 10))
		goto make_local_copy;

	res = gnome_vfs_open_uri (&handle, uri,
				  GNOME_VFS_OPEN_READ | GNOME_VFS_OPEN_RANDOM);
	if (res != GNOME_VFS_OK) {
		g_set_error (error, gsf_input_error_id (), (gint) res,
			     gnome_vfs_result_to_string (res));
		return NULL;
	}

	{
		char *name;
		GsfInputGnomeVFS *input = g_object_new (GSF_INPUT_GNOMEVFS_TYPE, NULL);

		input->handle = handle;
		input->uri = gnome_vfs_uri_ref (uri);
		input->buf  = NULL;
		input->buf_size = 0;
		gsf_input_set_size (GSF_INPUT (input), size);
		name = gnome_vfs_uri_to_string (uri, 0);
		gsf_input_set_name (GSF_INPUT (input), name);
		g_free (name);
		return GSF_INPUT (input);
	}

 make_local_copy:
	{
		char *buffer;
		int file_size;
		char *uri_text, *name;
		GsfInput *mem;

		uri_text = gnome_vfs_uri_to_string (uri, GNOME_VFS_URI_HIDE_NONE);
		res = gnome_vfs_read_entire_file (uri_text, &file_size, &buffer);
		g_free (uri_text);
		if (res != GNOME_VFS_OK) {
			g_set_error (error, gsf_input_error_id (), (gint)res,
				     "Read error while creating local copy.");
			return NULL;
		}

		mem = gsf_input_memory_new (buffer, file_size, TRUE);
		if (!mem) {
			g_set_error (error, gsf_input_error_id (), 0,
				     "Failed to create local memory stream");
			g_free (buffer);
			return NULL;
		}

		name = gnome_vfs_uri_to_string (uri, 0);
		gsf_input_set_name (mem, name);
		g_free (name);

		return mem;
	}
}

/**
 * gsf_input_gnomevfs_new :
 * @uri : uri you wish to open.
 * @err	: optionally NULL.
 *
 * Returns a new file or NULL.
 **/
GsfInput *
gsf_input_gnomevfs_new (char const *text_uri, GError **error)
{
	GnomeVFSURI *uri = gnome_vfs_uri_new (text_uri);
	if (!uri) {
		g_set_error (error, gsf_input_error_id (), 0,
			     "Invalid URI");
		return NULL;
	} else {
		GsfInput *res = gsf_input_gnomevfs_new_uri (uri, error);
		gnome_vfs_uri_unref (uri);
		return res;
	}
}

static void
gsf_input_gnomevfs_finalize (GObject *obj)
{
	GObjectClass *parent_class;
	GsfInputGnomeVFS *input = (GsfInputGnomeVFS *)obj;

	if (input->handle != NULL) {
		gnome_vfs_close (input->handle);
		input->handle = NULL;
	}
	if (input->uri != NULL) {
		gnome_vfs_uri_unref (input->uri);
		input->uri = NULL;
	}

	g_free (input->buf);
	input->buf = NULL;
	input->buf_size = 0;

	parent_class = g_type_class_peek (GSF_INPUT_TYPE);
	if (parent_class && parent_class->finalize)
		parent_class->finalize (obj);
}

static GsfInput *
gsf_input_gnomevfs_dup (GsfInput *src_input, GError **err)
{
	GsfInputGnomeVFS const *src = (GsfInputGnomeVFS *)src_input;
	return gsf_input_gnomevfs_new (src->input.name, err);
}

static guint8 const *
gsf_input_gnomevfs_read (GsfInput *input, size_t num_bytes,
			 guint8 *buffer)
{
	GsfInputGnomeVFS *vfs = GSF_INPUT_GNOMEVFS (input);
	GnomeVFSResult res = GNOME_VFS_OK;
	GnomeVFSFileSize nread = 0, total_read = 0;

	g_return_val_if_fail (vfs != NULL, NULL);
	g_return_val_if_fail (vfs->handle != NULL, NULL);

	if (buffer == NULL) {
		if (vfs->buf_size < num_bytes) {
			vfs->buf_size = num_bytes;
			g_free (vfs->buf);
			vfs->buf = g_new (guint8, vfs->buf_size);
		}
		buffer = vfs->buf;
	}

	while ((res == GNOME_VFS_OK) && (total_read < num_bytes)) {
		res = gnome_vfs_read (vfs->handle,
			(gpointer)(buffer + total_read),
			(GnomeVFSFileSize) (num_bytes - total_read), &nread);
		total_read += nread;
	}

	if (res != GNOME_VFS_OK || total_read != num_bytes)
		return NULL;

	return buffer;
}

static gboolean
gsf_input_gnomevfs_seek (GsfInput *input, gsf_off_t offset, GSeekType whence)
{
	GsfInputGnomeVFS const *vfs     = GSF_INPUT_GNOMEVFS (input);
	GnomeVFSSeekPosition vfs_whence;

	if (vfs->handle == NULL)
		return TRUE;

	switch (whence) {
	default:
	case G_SEEK_SET : vfs_whence = GNOME_VFS_SEEK_START;	break;
	case G_SEEK_CUR : vfs_whence = GNOME_VFS_SEEK_CURRENT;	break;
	case G_SEEK_END : vfs_whence = GNOME_VFS_SEEK_END;	break;
	}

	/* Work around http://bugzilla.gnome.org/show_bug.cgi?id=152844  */
	if (whence == G_SEEK_SET && offset > 0 && offset == gsf_input_size (input)) {
		if (gsf_input_gnomevfs_seek (input, offset - 1, whence))
			return TRUE;
		if (gsf_input_gnomevfs_read (input, 1, NULL) == NULL)
			return TRUE;
		return FALSE;
	}

	if (GNOME_VFS_OK == gnome_vfs_seek (vfs->handle,vfs_whence,
					    (GnomeVFSFileOffset) offset))
		return FALSE;
	return TRUE;
}

static void
gsf_input_gnomevfs_init (GObject *obj)
{
	GsfInputGnomeVFS *vfs = GSF_INPUT_GNOMEVFS (obj);

	vfs->handle   = NULL;
	vfs->buf      = NULL;
	vfs->buf_size = 0;
}

static void
gsf_input_gnomevfs_class_init (GObjectClass *gobject_class)
{
	GsfInputClass *input_class = GSF_INPUT_CLASS (gobject_class);

	gobject_class->finalize = gsf_input_gnomevfs_finalize;
	input_class->Dup	    = gsf_input_gnomevfs_dup;
	input_class->Read	    = gsf_input_gnomevfs_read;
	input_class->Seek	    = gsf_input_gnomevfs_seek;
}

GSF_CLASS (GsfInputGnomeVFS, gsf_input_gnomevfs,
	   gsf_input_gnomevfs_class_init, gsf_input_gnomevfs_init,
	   GSF_INPUT_TYPE)
