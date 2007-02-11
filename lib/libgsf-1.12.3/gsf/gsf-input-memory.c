/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-input-memory.c:
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
#include <string.h>
#include <gsf/gsf-input-memory.h>
#include <gsf/gsf-input-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-shared-memory.h>
#include <glib/gstdio.h>

#ifdef HAVE_MMAP

#if defined(FREEBSD) || defined(__FreeBSD__)
/* We must keep the file open while pages are mapped.  */
/* http://www.freebsd.org/cgi/query-pr.cgi?pr=48291 */
#define HAVE_BROKEN_MMAP
#endif /* defined(FREEBSD) || defined(__FreeBSD__) */

#elif defined(G_OS_WIN32)

#include <windows.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <io.h>
#include <fcntl.h>

#define MAP_FAILED NULL
#endif /* HAVE_MMAP */

#ifndef O_BINARY
#define O_BINARY 0
#endif

static GObjectClass *parent_class;

struct _GsfInputMemory {
	GsfInput parent;
	GsfSharedMemory *shared;
#ifdef HAVE_BROKEN_MMAP
	int fd;
#endif
};
typedef GsfInputClass GsfInputMemoryClass;

/**
 * gsf_input_memory_new:
 * @buf: The input bytes
 * @length: The length of @buf
 * @needs_free: Whether you want this memory to be free'd at object destruction
 *
 * Returns: A new #GsfInputMemory
 */
GsfInput *
gsf_input_memory_new (guint8 const *buf, gsf_off_t length, gboolean needs_free)
{
	GsfInputMemory *mem = g_object_new (GSF_INPUT_MEMORY_TYPE, NULL);
	mem->shared = gsf_shared_memory_new ((void *)buf, length, needs_free);
	gsf_input_set_size (GSF_INPUT (mem), length);
	return GSF_INPUT (mem);
}

/**
 * gsf_input_memory_new_clone:
 * @buf: The input bytes
 * @length: The length of @buf
 *
 * Returns: A new #GsfInputMemory
 */
GsfInput *
gsf_input_memory_new_clone (guint8 const *buf, gsf_off_t length)
{
	GsfInputMemory *mem = NULL;
	guint8 * cpy = g_try_malloc (length * sizeof (guint8));
	if (cpy == NULL)
		return NULL;

	memcpy (cpy, buf, length);
	mem = g_object_new (GSF_INPUT_MEMORY_TYPE, NULL);
	mem->shared = gsf_shared_memory_new ((void *)cpy, length, TRUE);
	gsf_input_set_size (GSF_INPUT (mem), length);
	return GSF_INPUT (mem);
}

static void
gsf_input_memory_finalize (GObject *obj)
{
	GsfInputMemory *mem = (GsfInputMemory *) (obj);

	if (mem->shared)
		g_object_unref (G_OBJECT (mem->shared));

#ifdef HAVE_BROKEN_MMAP
	if (mem->fd != -1)
		close (mem->fd);
#endif

	parent_class->finalize (obj);
}

static GsfInput *
gsf_input_memory_dup (GsfInput *src_input, G_GNUC_UNUSED GError **err)
{
	GsfInputMemory const *src = (GsfInputMemory *) (src_input);
	GsfInputMemory *dst = g_object_new (GSF_INPUT_MEMORY_TYPE, NULL);

	dst->shared = src->shared;
	g_object_ref (G_OBJECT (dst->shared));
	gsf_input_set_size (GSF_INPUT (dst), src->shared->size);

#ifdef HAVE_BROKEN_MMAP
	if (src->fd != -1)
		dst->fd = dup (src->fd);
#endif

	return GSF_INPUT (dst);
}

static guint8 const *
gsf_input_memory_read (GsfInput *input, size_t num_bytes, guint8 *optional_buffer)
{
	GsfInputMemory *mem = (GsfInputMemory *) (input);
	guchar const *src = mem->shared->buf;

	if (src == NULL)
		return NULL;
	if (optional_buffer) {
		memcpy (optional_buffer, src + input->cur_offset, num_bytes);
		return optional_buffer;
	} else
		return src + input->cur_offset;
}

static gboolean
gsf_input_memory_seek (G_GNUC_UNUSED GsfInput *input,
		       G_GNUC_UNUSED gsf_off_t offset,
		       G_GNUC_UNUSED GSeekType whence)
{
	return FALSE;
}

static void
gsf_input_memory_init (GObject *obj)
{
	GsfInputMemory *mem = (GsfInputMemory *) (obj);
	mem->shared = NULL;
#ifdef HAVE_BROKEN_MMAP
	mem->fd = -1;
#endif
}

static void
gsf_input_memory_class_init (GObjectClass *gobject_class)
{
	GsfInputClass *input_class = GSF_INPUT_CLASS (gobject_class);

	gobject_class->finalize = gsf_input_memory_finalize;
	input_class->Dup	= gsf_input_memory_dup;
	input_class->Read	= gsf_input_memory_read;
	input_class->Seek	= gsf_input_memory_seek;

	parent_class = g_type_class_peek_parent (gobject_class);
}

GSF_CLASS (GsfInputMemory, gsf_input_memory,
	   gsf_input_memory_class_init, gsf_input_memory_init,
	   GSF_INPUT_TYPE)

/***************************************************************************/

#ifdef HAVE_MMAP

#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#ifndef PROT_READ
#define PROT_READ 0x1
#endif /* PROT_READ */

#if !defined(MAP_FAILED) || defined(__osf__)
/* Someone needs their head examined - BSD ? */
#	define MAP_FAILED ((void *)-1)
#endif /* !defined(MAP_FAILED) || defined(__osf__) */

#endif /* HAVE_MMAP */

/**
 * gsf_input_mmap_new:
 * @filename: The file on disk that you want to mmap
 * @err: A #GError, or optionally %null
 *
 * Returns: A new #GsfInputMemory
 */
GsfInput *
gsf_input_mmap_new (char const *filename, GError **err)
{
#if defined(HAVE_MMAP) || defined(G_OS_WIN32)
	GsfInputMemory *mem;
	guint8 *buf = NULL;
	struct stat st;
	int fd;
	size_t size;

	fd = g_open (filename, O_RDONLY | O_BINARY, 0);
	if (fd < 0 || fstat (fd, &st) < 0) {
		if (err != NULL) {
			int save_errno = errno;
			char *utf8name = g_filename_display_name (filename);
			*err = g_error_new (gsf_input_error_id (), 0,
					    "%s: %s",
					    utf8name, g_strerror (save_errno));
			g_free (utf8name);
		}
		if (fd >= 0) close (fd);
		return NULL;
	}

	if (!S_ISREG (st.st_mode)) {
		if (err != NULL) {
			char *utf8name = g_filename_display_name (filename);
			*err = g_error_new (gsf_input_error_id (), 0,
					    "%s: Is not a regular file",
					    utf8name);
			g_free (utf8name);
		}
		close (fd);
		return NULL;
	}

	size = (size_t) st.st_size;
	if ((off_t) size != st.st_size) { /* Check for overflow */
		if (err != NULL) {
			char *utf8name = g_filename_display_name (filename);
			*err = g_error_new (gsf_input_error_id (), 0,
					    "%s: File too large to be memory mapped",
					    utf8name);
			g_free (utf8name);
		}
		close (fd);
		return NULL;
	}

#ifdef G_OS_WIN32
	{
		HANDLE handle = CreateFileMapping ((HANDLE)_get_osfhandle (fd), NULL, PAGE_READONLY, 0, 0, NULL);
		buf = MapViewOfFile (handle, FILE_MAP_READ, 0, 0, 0);
		CloseHandle (handle);
	}
#else
	buf = mmap (NULL, size, PROT_READ, MAP_SHARED, fd, (off_t) 0);
#endif

	if (buf == MAP_FAILED) {
		if (err != NULL) {
			int save_errno = errno;
			char *utf8name = g_filename_display_name (filename);
			*err = g_error_new (gsf_input_error_id (), 0,
					    "%s: %s",
					    utf8name, g_strerror (save_errno));
			g_free (utf8name);
		}
		close (fd);
		return NULL;
	}

	mem = g_object_new (GSF_INPUT_MEMORY_TYPE, NULL);
	mem->shared = gsf_shared_memory_mmapped_new (buf, (gsf_off_t) size);
	gsf_input_set_size (GSF_INPUT (mem), (gsf_off_t) size);
	gsf_input_set_name (GSF_INPUT (mem), filename);

#ifdef HAVE_BROKEN_MMAP
	mem->fd = fd;
#else
	close (fd);
#endif

	return GSF_INPUT (mem);
#else
	(void)filename;
	if (err != NULL)
		*err = g_error_new (gsf_input_error_id (), 0,
				    "mmap not supported");
	return NULL;
#endif
}
