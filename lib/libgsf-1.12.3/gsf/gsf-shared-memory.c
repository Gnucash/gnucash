/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-shared-memory.c: 
 *
 * Copyright (C) 2002-2004 Morten Welinder (terra@diku.dk)
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
#include <gsf/gsf-shared-memory.h>
#include <gsf/gsf-impl-utils.h>

#ifdef HAVE_MMAP
#include <sys/types.h>
#include <sys/mman.h>
#elif defined(G_OS_WIN32)
#include <windows.h>
#endif

typedef struct {
	GObjectClass g_object_class;
} GsfSharedMemoryClass;

static GObjectClass *parent_class;

GsfSharedMemory *
gsf_shared_memory_new (void *buf, gsf_off_t size, gboolean needs_free)
{
	GsfSharedMemory *mem = g_object_new (GSF_SHARED_MEMORY_TYPE, NULL);
	mem->buf = buf;
	mem->size = size;
	mem->needs_free = needs_free;
	mem->needs_unmap = FALSE;
	return mem;
}

GsfSharedMemory *
gsf_shared_memory_mmapped_new (void *buf, gsf_off_t size)
{
#if defined(HAVE_MMAP) || defined(G_OS_WIN32)
	size_t msize = size;
	if ((gsf_off_t)msize != size) {
		g_warning ("memory buffer size too large");
		return NULL;
	} else {
		GsfSharedMemory *mem = gsf_shared_memory_new (buf, size, FALSE);
		mem->needs_unmap = TRUE;
		return mem;
	}
#else
	return NULL;
#endif
}

static void
gsf_shared_memory_finalize (GObject *obj)
{
	GsfSharedMemory *mem = (GsfSharedMemory *) (obj);
	
	if (mem->buf != NULL) {
		if (mem->needs_free)
			g_free (mem->buf);
		else if (mem->needs_unmap) {
#ifdef HAVE_MMAP
			munmap (mem->buf, mem->size);
#elif defined(G_OS_WIN32)
			UnmapViewOfFile (mem->buf);
#else
			g_assert_not_reached ();
#endif
		}
	}

	G_OBJECT_CLASS (parent_class)->finalize (obj);
}

static void
gsf_shared_memory_init (GObject *obj)
{
	GsfSharedMemory *mem = (GsfSharedMemory *) (obj);
	mem->buf = NULL;
}

static void
gsf_shared_memory_class_init (GObjectClass *gobject_class)
{
	parent_class = g_type_class_peek_parent (gobject_class);

	gobject_class->finalize = gsf_shared_memory_finalize;
}

GSF_CLASS (GsfSharedMemory, gsf_shared_memory,
	   gsf_shared_memory_class_init, gsf_shared_memory_init,
	   G_TYPE_OBJECT)
