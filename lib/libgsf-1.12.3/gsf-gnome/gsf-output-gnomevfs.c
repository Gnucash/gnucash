/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-output-gnomevfs.c: gnomevfs based output
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
#include <gsf-gnome/gsf-output-gnomevfs.h>
#include <gsf/gsf-output-impl.h>
#include <gsf/gsf-impl-utils.h>

struct _GsfOutputGnomeVFS {
    GsfOutput output;

    GnomeVFSHandle *handle;
};

typedef struct {
    GsfOutputClass output_class;
} GsfOutputGnomeVFSClass;

/**
* gsf_output_gnomevfs_new :
 * @text_uri : in utf8.
 * @err	     : optionally NULL.
 *
 * Returns a new file or NULL.
 **/
GsfOutput *
gsf_output_gnomevfs_new (char const *text_uri, GError **err)
{
	GnomeVFSURI *uri = gnome_vfs_uri_new (text_uri);
	GsfOutput *res = gsf_output_gnomevfs_new_uri (uri, err);
	gnome_vfs_uri_unref (uri);
	return res;
}

/**
 * gsf_output_gnomevfs_new_uri :
 * @uri      : resource indicator
 * @err	     : optionally NULL.
 *
 * Returns a new file or NULL.
 **/
GsfOutput *
gsf_output_gnomevfs_new_uri (GnomeVFSURI * uri, GError **err)
{
	GsfOutputGnomeVFS *output;
	GnomeVFSHandle *handle;
	GnomeVFSResult res;
	int perms = -1;

	if (uri == NULL) {
		g_set_error (err, gsf_output_error_id (), 0,
			     "Filename/URI cannot be NULL");
		return NULL;
	}

	if (gnome_vfs_uri_exists (uri)) {
		/* see bug 159442 - if the file exists, we want to do our best to preserve existing 
		 * pemissions AND truncate the file. that is, we want to emulate truncate() in case 
		 * a gnomevfs backend doesn't support it */
		GnomeVFSFileInfo *info;

		info = gnome_vfs_file_info_new ();
		res = gnome_vfs_get_file_info_uri (uri,
						   info,
						   GNOME_VFS_FILE_INFO_FOLLOW_LINKS|GNOME_VFS_FILE_INFO_GET_ACCESS_RIGHTS);

		if ((res == GNOME_VFS_OK) && (info->valid_fields & GNOME_VFS_FILE_INFO_FIELDS_PERMISSIONS)) {
			perms = info->permissions;
		} 

		gnome_vfs_file_info_unref (info);
	}

	if (perms == -1) {
		/* we didn't get the permissions, but calling open_uri() with OPEN_WRITE set will create the file for us.
		 * if the uri_exists(), let's hope that truncate() works. */
		res = gnome_vfs_open_uri (&handle, uri, GNOME_VFS_OPEN_WRITE|GNOME_VFS_OPEN_RANDOM);	
	} else {
		/* we got the permissions, so let's call create() with the existing permissions instead of open() since 
		 * create() will truncate the file for us. */
		res = gnome_vfs_create_uri (&handle, uri, GNOME_VFS_OPEN_WRITE|GNOME_VFS_OPEN_RANDOM, FALSE, perms);

		if (res != GNOME_VFS_OK) {
			/* create() failed. let's see if we can open_uri() instead and hope that truncate works. */
			res = gnome_vfs_open_uri (&handle, uri, GNOME_VFS_OPEN_WRITE|GNOME_VFS_OPEN_RANDOM);
		}
	}

	if (res != GNOME_VFS_OK) {
		g_set_error (err, gsf_output_error_id (), (gint) res,
			     gnome_vfs_result_to_string (res));
		return NULL;
	}

	/* truncate the file to length 0 so if we overwrite a file smaller than
	 * it was before, it doesn't show the rest of the old file (Bug: 159442).
	 * for many gnomevfs backends, this might actually be a noop */
	gnome_vfs_truncate_handle(handle, 0);

	output = g_object_new (GSF_OUTPUT_GNOMEVFS_TYPE, NULL);
	output->handle = handle;

	return GSF_OUTPUT (output);
}

static gboolean
gsf_output_gnomevfs_close (GsfOutput *output)
{
    GsfOutputGnomeVFS *vfs = GSF_OUTPUT_GNOMEVFS (output);
    gboolean res = FALSE;

    if (vfs->handle != NULL) {
        res = (GNOME_VFS_OK == gnome_vfs_close (vfs->handle));
        vfs->handle = NULL;
    }

    return res;
}

static void
gsf_output_gnomevfs_finalize (GObject *obj)
{
    GObjectClass *parent_class;
    GsfOutput *output = (GsfOutput *)obj;

    gsf_output_gnomevfs_close (output);

    parent_class = g_type_class_peek (GSF_OUTPUT_TYPE);
    if (parent_class && parent_class->finalize)
        parent_class->finalize (obj);
}

static gboolean
gsf_output_gnomevfs_seek (GsfOutput *output, gsf_off_t offset,
			  GSeekType whence)
{
	GsfOutputGnomeVFS const *vfs = GSF_OUTPUT_GNOMEVFS (output);
	GnomeVFSSeekPosition	vfs_whence = 0; /* make compiler shut up */
	GnomeVFSResult	 	res;

	g_return_val_if_fail (vfs->handle != NULL, 
		gsf_output_set_error (output, 0, "missing handle"));

	switch (whence) {
	case G_SEEK_SET : vfs_whence = GNOME_VFS_SEEK_START;	break;
	case G_SEEK_CUR : vfs_whence = GNOME_VFS_SEEK_CURRENT;	break;
	case G_SEEK_END : vfs_whence = GNOME_VFS_SEEK_END;	break;
	default :
		break; /*checked in GsfOutput wrapper */
	}

	res = gnome_vfs_seek (vfs->handle, vfs_whence,
			      (GnomeVFSFileOffset) offset);
	if (GNOME_VFS_OK == res)
		return TRUE;
	return gsf_output_set_error (output, 0,
		gnome_vfs_result_to_string (res));
}

static gboolean
gsf_output_gnomevfs_write (GsfOutput *output,
			   size_t num_bytes,
			   guint8 const *buffer)
{
    GsfOutputGnomeVFS *vfs = GSF_OUTPUT_GNOMEVFS (output);
    GnomeVFSFileSize nwritten = 0, total_written = 0;
    GnomeVFSResult res = GNOME_VFS_OK;

    g_return_val_if_fail (vfs != NULL, FALSE);
    g_return_val_if_fail (vfs->handle != NULL, FALSE);

    while ((res == GNOME_VFS_OK) && (total_written < num_bytes))
	    {
		    res = gnome_vfs_write (vfs->handle, (gconstpointer)(buffer + total_written),
					   (GnomeVFSFileSize)(num_bytes - total_written), &nwritten);
		    total_written += nwritten;
	    }
    return (res == GNOME_VFS_OK && total_written == num_bytes);
}

static void
gsf_output_gnomevfs_init (GObject *obj)
{
    GsfOutputGnomeVFS *vfs = GSF_OUTPUT_GNOMEVFS (obj);

    vfs->handle = NULL;
}

static void
gsf_output_gnomevfs_class_init (GObjectClass *gobject_class)
{
    GsfOutputClass *output_class = GSF_OUTPUT_CLASS (gobject_class);

    gobject_class->finalize = gsf_output_gnomevfs_finalize;
    output_class->Close	= gsf_output_gnomevfs_close;
    output_class->Seek	= gsf_output_gnomevfs_seek;
    output_class->Write	= gsf_output_gnomevfs_write;
}

GSF_CLASS (GsfOutputGnomeVFS, gsf_output_gnomevfs,
           gsf_output_gnomevfs_class_init, gsf_output_gnomevfs_init, GSF_OUTPUT_TYPE)
