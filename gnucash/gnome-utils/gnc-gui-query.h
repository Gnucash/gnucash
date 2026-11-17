/********************************************************************\
 * gnc-gui-query.h -- functions for creating dialogs for GnuCash    *
 * Copyright (C) 1998, 1999, 2000 Linas Vepstas                     *
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
 * along with this program; if not, see                             *
 * <https://www.gnu.org/licenses/>                                  *
\********************************************************************/

#ifndef QUERY_USER_H
#define QUERY_USER_H

#ifdef __cplusplus
extern "C" {
#endif

extern void
gnc_info_dialog (GtkWindow *parent,
                 const char *format, ...) G_GNUC_PRINTF (2, 3);


void gnc_error_dialog (GtkWindow* parent, const char* format, ...) G_GNUC_PRINTF (2, 3);

#ifdef __cplusplus
}
#endif


#endif
