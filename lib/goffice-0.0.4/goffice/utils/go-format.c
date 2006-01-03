/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-format.c :
 *
 * Copyright (C) 2003-2005 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <goffice/goffice-config.h>
#include "go-format.h"
#include "go-math.h"
#include "datetime.h"
#include "format-impl.h"
#include <string.h>

static gboolean
go_style_format_condition (GOFormatElement const *entry, double val)
{
	if (entry->restriction_type == '*')
		return TRUE;

	switch (entry->restriction_type) {
	case '<': return val < entry->restriction_value;
	case '>': return val > entry->restriction_value;
	case '=': return val == entry->restriction_value;
	case ',': return val <= entry->restriction_value;
	case '.': return val >= entry->restriction_value;
	case '+': return val != entry->restriction_value;
	default:
		return FALSE;
	}
}

void
go_format_value_gstring (GOFormat const *format, GString *res, double val,
			 int col_width, GODateConventions const *date_conv)
{
	GOFormatElement const *entry = NULL; /* default to General */
	GSList const *list = NULL;
	gboolean need_abs = FALSE;

	if (format != NULL) {
		for (list = format->entries; list; list = list->next)
			if (go_style_format_condition (list->data, val))
				break;
		if (list == NULL)
			list = format->entries;
	}

	/* If nothing matches treat it as General */
	if (list != NULL) {
		entry = list->data;

		/* Empty formats should be ignored */
		if (entry->format[0] == '\0')
			return;

#if 0
		if (go_color && entry->go_color != 0)
			*go_color = entry->go_color;
#endif

		if (strcmp (entry->format, "@") == 0) {
			/* FIXME : Formatting a value as a text returns
			 * the entered text.  We need access to the
			 * parse format */
			entry = NULL;

		/* FIXME : Just containing General is enough to be
		 * general for now.  We'll ignore prefixes and suffixes
		 * for the time being */
		} else if (strstr (entry->format, "General") != NULL)
			entry = NULL;
	}

	/* More than one format? -- abs the value.  */
	need_abs = entry && format->entries->next;

	if (INT_MAX >= val && val >= INT_MIN && val == floor (val)) {
		int i_val = (int)val;
		if (need_abs)
			i_val = ABS (i_val);

		if (entry == NULL)
			go_fmt_general_int (res, i_val, col_width);
		else
			go_format_number (res, i_val, col_width, entry, date_conv);
	} else {
		if (need_abs)
			val = fabs (val);

		if (entry == NULL)
			go_fmt_general_float (res, val, col_width);
		else
			go_format_number (res, val, col_width, entry, date_conv);
	}
}

char *
go_format_value (GOFormat const *fmt, double val)
{
	GString *res;

	if (!go_finite (val))
		return g_strdup ("#VALUE!");

	res = g_string_sized_new (20);
	go_format_value_gstring (fmt, res, val, -1, NULL);
	return g_string_free (res, FALSE);
}
