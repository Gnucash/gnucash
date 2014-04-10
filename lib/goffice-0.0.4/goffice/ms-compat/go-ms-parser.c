/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* 
 * ms-escher-types.h - 
 * Copyright (C) 2003-2004, Christopher James Lahey
 *
 * Authors:
 *   Christopher James Lahey <clahey@ximian.com>
 *
 * This file is free software; you can redistribute it and/or modify
 * it under the terms of version 2 of the GNU Library General Public
 * License as published by the Free Software Foundation.
 *
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this file; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA.
 */

#include <goffice/goffice-config.h>
#include <goffice/ms-compat/go-ms-parser.h>
#include <glib/gmacros.h>
#include <gsf/gsf-input.h>
#include <gsf/gsf-utils.h>

#define d(x) x

#define CVS_VERSION "$Id: go-ms-parser.c,v 1.8 2005/08/08 08:57:01 jdassen Exp $"
#define ERROR_STRING(cond,str) G_STRLOC "\n<" CVS_VERSION ">\n" str " (" #cond ")"
#define ERROR(cond,str) { \
	if (!(cond)) { \
		if (err) \
			g_set_error (err, domain, code, ERROR_STRING(cond,str)); \
		else \
			g_warning (ERROR_STRING(cond,str)); \
		goto error_cleanup; \
	} \
}

#define STACK_TOP GO_MS_PARSER_STACK_TOP(stack)

static GQuark domain;
static gint code;

GOMSParserRecordType unknown_type =
	{	0,			"Unknown",			FALSE,	FALSE,	-1,	-1	};


void
go_ms_parser_read (GsfInput   *input,
		   gsf_off_t   length,
		   const GOMSParserRecordType *types,
		   guint type_count,
		   GOMSParserCallbacks *callbacks,
		   gpointer user_data,
		   GError    **err)
{
	GSList *stack = NULL;
	const guint8 *data;
	gsf_off_t position;

	while (length > 0) {
		const GOMSParserRecordType *type;
		GOMSParserRecord record;
		guint i;

		ERROR (length >= 8, "Length Error");
		data = gsf_input_read (input, 8, NULL);
		ERROR (data != NULL, "Length Error");

		record.vers   = data[0] & 0xf;
		record.inst   = ((data[0] & 0xf0) >> 4) | (data[1] << 4);
		record.opcode = GSF_LE_GET_GUINT16 (data+2);
		record.length = GSF_LE_GET_GUINT32 (data+4);

		record.length_remaining = record.length;
		record.parse_state = NULL;

		data = NULL;

		/* Find type */
		type = &unknown_type;

		for (i = 0; i < type_count; i++) {
			if (types[i].typecode == record.opcode) {
				type = &types[i];
				break;
			}
		}

		record.type = type;

		ERROR (record.length + 8 <= (stack ? ((GOMSParserRecord *) (stack->data))->length_remaining : length), "Length Error");
		ERROR (type->min_record_size == -1 || ((guint32)type->min_record_size) <= record.length, "Length Error");
		ERROR (type->max_record_size == -1 || ((guint32)type->max_record_size) >= record.length, "Length Error");

		position = gsf_input_tell (input);

		if (type->is_container) {
			GOMSParserRecord *stack_item;
			stack_item = g_new (GOMSParserRecord, 1);
			*stack_item = record;
			stack = g_slist_prepend (stack, stack_item);

			if (callbacks && callbacks->start_container) {
				callbacks->start_container (stack, input, err, user_data);
			}

			gsf_input_seek (input, position, G_SEEK_SET);
		} else {
			if (callbacks && callbacks->handle_atom) {
				if (type->do_read) {
					data = gsf_input_read (input, record.length, NULL);
					ERROR (record.length == 0 || data, "Length Error");
				}

				callbacks->handle_atom (&record, stack, data, input, err, user_data);
			}

			if (STACK_TOP) {
				STACK_TOP->length_remaining -= record.length + 8;
			} else {
				length -= record.length + 8;
			}
			gsf_input_seek (input, position + record.length, G_SEEK_SET);
		}

		while (STACK_TOP && STACK_TOP->length_remaining == 0) {
			int item_length = STACK_TOP->length;

			if (callbacks && callbacks->end_container) {
				callbacks->end_container (stack, input, err, user_data);
			}

			g_free (stack->data);
			stack = g_slist_delete_link (stack, stack);
			if (stack) {
				STACK_TOP->length_remaining -= item_length + 8;
			} else {
				length -= item_length + 8;
			}
		}
	}

 error_cleanup:
	while (stack) {
		g_free (stack->data);
		stack = g_slist_delete_link (stack, stack);
	}
}
