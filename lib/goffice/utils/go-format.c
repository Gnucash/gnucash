/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-format.c :
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#include <goffice/goffice-config.h>
#include "go-format.h"
//#include <src/format.h>
//#include <src/value.h>
//#include <src/datetime.h>
#include <format.h>
#include <value.h>
#include <datetime.h>

GOFormat *
go_format_new_from_XL (char const *descriptor_string, gboolean delocalize)
{
	return style_format_new_XL (descriptor_string, delocalize);
}

char *
go_format_as_XL	(GOFormat const *fmt, gboolean localized)
{
	return style_format_as_XL (fmt, localized);
}

GOFormat *
go_format_ref (GOFormat *fmt)
{
	style_format_ref (fmt);
	return fmt;
}

void
go_format_unref (GOFormat *fmt)
{
	style_format_unref (fmt);
}

char *
go_format_value (GOFormat const *fmt, double val)
{
	static GnmValueFloat	  tmp  = { VALUE_FLOAT, NULL, 0. };
	static GnmDateConventions conv = { FALSE };
	tmp.val = val;
	return format_value (fmt, (GnmValue *)&tmp, NULL, -1, &conv);
}

gboolean
go_format_eq (GOFormat const *a, GOFormat const *b)
{
	if (a == NULL)
		return b == NULL;
	if (b == NULL)
		return FALSE;
	return style_format_equal (a, b);
}

/**
 * go_format_general :
 * 
 * Returns the 'General' #GOFormat but does not add a reference
 **/
GOFormat *
go_format_general (void)
{
	return style_format_general ();
}

/**
 * go_format_default_date :
 * 
 * Returns the default date #GOFormat but does not add a reference
 **/
GOFormat *
go_format_default_date (void)
{
	return style_format_default_date ();
}

/**
 * go_format_default_time :
 * 
 * Returns the default time #GOFormat but does not add a reference
 **/
GOFormat *
go_format_default_time (void)
{
	return style_format_default_time ();
}

/**
 * go_format_default_percentage :
 * 
 * Returns the default percentage #GOFormat but does not add a reference
 **/
GOFormat *
go_format_default_percentage (void)
{
	return style_format_default_percentage ();
}

/**
 * go_format_default_money :
 * 
 * Returns the default money #GOFormat but does not add a reference
 **/
GOFormat *
go_format_default_money	(void)
{
	return style_format_default_money ();
}
