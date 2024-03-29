/*   utilities for GTimeTracker - a time tracker
 *   Copyright (C) 2001 Linas Vepstas
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *   02110-1301 USA
 */

#ifndef __GTT_UTIL_H__
#define __GTT_UTIL_H__

#ifdef __cplusplus
extern "C" {
#endif

/* Some gtk-like utilities */
void xxxgtk_textview_set_text (GtkTextView *text, const char *str);
char * xxxgtk_textview_get_text (GtkTextView *text);

#ifdef __cplusplus
}
#endif

#endif /* __GTT_UTIL_H__ */
