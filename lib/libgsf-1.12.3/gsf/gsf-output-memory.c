/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-output-memory.c:
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
#include <gsf/gsf-output-memory.h>
#include <gsf/gsf-output-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <string.h>

#define MIN_BLOCK 512
#define MAX_STEP  (MIN_BLOCK * 128)

static GsfOutputClass *parent_class;

struct _GsfOutputMemory {
	GsfOutput output;
	guint8 *buffer;
	size_t capacity;
};

typedef struct {
	GsfOutputClass output_class;
} GsfOutputMemoryClass;

/**
 * gsf_output_memory_new :
 *
 * Returns a new file or NULL.
 **/
GsfOutput *
gsf_output_memory_new (void)
{
	return g_object_new (GSF_OUTPUT_MEMORY_TYPE, NULL);	
}

static gboolean
gsf_output_memory_close (GsfOutput *output)
{
	return (parent_class->Close == NULL) ||
		parent_class->Close (output);
}

static void
gsf_output_memory_finalize (GObject *obj)
{
	GsfOutputMemory *mem = GSF_OUTPUT_MEMORY (obj);
	
	g_free (mem->buffer);
	mem->buffer = NULL;

	G_OBJECT_CLASS (parent_class)->finalize (obj);
}

static gboolean
gsf_output_memory_seek (G_GNUC_UNUSED GsfOutput *output,
			G_GNUC_UNUSED gsf_off_t offset,
			G_GNUC_UNUSED GSeekType whence)
{
	/* let parent implementation handle maneuvering cur_offset */
	return TRUE;
}

static gboolean
gsf_output_memory_expand (GsfOutputMemory *mem, gsf_off_t needed)
{
	gsf_off_t capacity = MAX (mem->capacity, MIN_BLOCK);
	gsize lcapacity;
	
	/* If we need >= MAX_STEP, align to a next multiple of MAX_STEP.
	 * Since MAX_STEP is probably a power of two, this computation
	 * should reduce to "dec, shr, inc, shl", which is probably
	 * quicker then branching.
	 */
	if (needed < MAX_STEP)
		while (capacity < needed)
			capacity *= 2;
	else
		capacity = ((needed - 1) / MAX_STEP + 1) * MAX_STEP;

	/* Check for overflow: g_renew() casts its parameters to gsize. */
	lcapacity = capacity;
	if ((gsf_off_t) lcapacity != capacity || capacity < 0) {
		g_warning ("overflow in gsf_output_memory_expand");
		return FALSE;
	}
	mem->buffer   = g_renew (guint8, mem->buffer, lcapacity);
	mem->capacity = capacity;
	
	return TRUE;
}

static gboolean
gsf_output_memory_write (GsfOutput *output,
			 size_t num_bytes,
			 guint8 const *buffer)
{
	GsfOutputMemory *mem = GSF_OUTPUT_MEMORY (output);
	
	g_return_val_if_fail (mem != NULL, FALSE);
	
	if (!mem->buffer) {
		mem->buffer   = g_new (guint8, MIN_BLOCK);
		mem->capacity = MIN_BLOCK;
	}
	if (num_bytes + output->cur_offset > mem->capacity) {
		if (!gsf_output_memory_expand (mem, output->cur_offset + num_bytes))
			return FALSE;
	}
	
	memcpy (mem->buffer + output->cur_offset, buffer, num_bytes);	
	return TRUE;
}

static gsf_off_t gsf_output_memory_vprintf (GsfOutput *output,
	char const *format, va_list args) G_GNUC_PRINTF (2, 0);

static gsf_off_t
gsf_output_memory_vprintf (GsfOutput *output, char const *format, va_list args)
{
	GsfOutputMemory *mem = (GsfOutputMemory *)output;

	if (mem->buffer) {
		va_list args2;
		gsf_off_t len;

		/*
		 * We need to make a copy as args will become unusable after
		 * the g_vsnprintf call.
		 */
		G_VA_COPY (args2, args);

		len =
			g_vsnprintf (mem->buffer + output->cur_offset,
				     mem->capacity - output->cur_offset,
				     format, args);

		if (len < mem->capacity - output->cur_offset)
			return len; /* There was sufficient space */

		return parent_class->Vprintf (output, format, args2);
	}
	return parent_class->Vprintf (output, format, args);
}

static void
gsf_output_memory_init (GObject *obj)
{
	GsfOutputMemory *mem = GSF_OUTPUT_MEMORY (obj);

	mem->buffer   = NULL;
	mem->capacity = 0;
}

static void
gsf_output_memory_class_init (GObjectClass *gobject_class)
{
	GsfOutputClass *output_class = GSF_OUTPUT_CLASS (gobject_class);
	
	gobject_class->finalize = gsf_output_memory_finalize;
	output_class->Close     = gsf_output_memory_close;
	output_class->Seek      = gsf_output_memory_seek;
	output_class->Write     = gsf_output_memory_write;
	output_class->Vprintf   = gsf_output_memory_vprintf;

	parent_class = GSF_OUTPUT_CLASS (g_type_class_peek_parent (gobject_class));
}

/**
 * gsf_output_memory_get_bytes :
 * @mem : the output device.
 * 
 * Returns: The data that has been written to @mem, or %null
 **/
const guint8 *
gsf_output_memory_get_bytes (GsfOutputMemory * mem)
{
	g_return_val_if_fail (mem != NULL, NULL);
	return mem->buffer;
}

GSF_CLASS (GsfOutputMemory, gsf_output_memory,
           gsf_output_memory_class_init, gsf_output_memory_init, GSF_OUTPUT_TYPE)

