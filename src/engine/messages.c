/********************************************************************\
 * messages.c -- national-language messages for GnuCash             *
 * Copyright (C) 2001 Christian Stimming                            *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include "messages.h"
#include <string.h>
#include <glib.h>

/* Prefix marker and end-of-prefix marker for i18n messages with
   qualifying prefix, see
   e.g. http://mail.gnome.org/archives/gnome-i18n/2001-April/msg00058.html. 

   Adapted from a patch posted to gnome-devel-list on Dec 6, 2000, by
   Zbigniew Chyla <cyba@piast.t19.ds.pwr.wroc.pl>
*/
#define Q_PREFIX_START '|'
#define Q_PREFIX_END   '|'

/***
 * gnc_qualifier_prefix_gettext
 **/
gchar *
gnc_qualifier_prefix_gettext (const gchar *string)
{
	g_assert (string != NULL);

	if (*string != Q_PREFIX_START) {
		return gettext (string);
	} else {
		gchar *translation;

		translation = gettext (string);
		if (translation != string) {
			if (*translation != Q_PREFIX_START) {
				return translation;
			} else {
				gchar *real_translation;

				real_translation = strchr (translation + 1, Q_PREFIX_END);
				if (real_translation != NULL) {
					return real_translation + 1;
				} else {
					g_warning ("Ivalid Q_() translation: \"%s\"", translation);
					return translation;
				}
			}
		} else {
			gchar *real_string;

			real_string = strchr (string + 1, Q_PREFIX_END);
			if (real_string != NULL) {
				return gettext (real_string + 1);
			} else {
				g_warning ("Invalid Q_() string: \"%s\"", string);
				return (gchar *) string;
			}
		}
	}
}


/***
 * gnc_qualifier_prefix_noop
 **/
gchar *
gnc_qualifier_prefix_noop (const gchar *string)
{
	g_assert (string != NULL);

	if (*string != Q_PREFIX_START) {
		return (gchar *) string;
	} else {
		gchar *real_string;

		real_string = strchr (string + 1, Q_PREFIX_END);
		if (real_string != NULL) {
			return real_string + 1;
		} else {
			g_warning ("Invalid Q_() string: \"%s\"", string);
			return (gchar *) string;
		}
	}
}
