/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */

/*
 * Global Gnome Font data structures.  To avoid duplicating this across
 * workbooks.
 *
 * Author:
 *   Miguel de Icaza (miguel@gnu.org)
 */
#include "gnumeric.h"
#include "global-gnome-font.h"

GList *gnumeric_font_family_list = NULL;
GList *gnumeric_point_size_list = NULL;

int const gnumeric_point_sizes [] = {
	4, 8, 9, 10, 11, 12, 14, 16, 18,
	20, 22, 24, 26, 28, 36, 48, 72,
	0
};

