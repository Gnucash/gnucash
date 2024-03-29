/********************************************************************\
 * gnc-splash.h -- splash screen for GnuCash                        *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef GNC_SPLASH_H
#define GNC_SPLASH_H
#include <gtk/gtk.h>

#ifdef __cplusplus
extern "C" {
#endif

void gnc_show_splash_screen (void);
void gnc_destroy_splash_screen (void);
void gnc_update_splash_screen (const gchar *string, double percentage);
GtkWindow *gnc_get_splash_screen (void);

#ifdef __cplusplus
}
#endif

#define GNC_SPLASH_PERCENTAGE_UNKNOWN 101

#endif
