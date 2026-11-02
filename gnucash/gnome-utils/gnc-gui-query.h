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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef QUERY_USER_H
#define QUERY_USER_H

#include <gtk/gtk.h>
#include "gnc-ui.h"

#ifdef __cplusplus
extern "C" {
#endif

extern void
gnc_info_dialog (GtkWindow *parent,
                 const char *format, ...) G_GNUC_PRINTF (2, 3);


void gnc_error_dialog (GtkWindow* parent, const char* format, ...) G_GNUC_PRINTF (2, 3);

/* Attach the shared exactly-once response/destroy/parent lifecycle to an
 * existing dialog without presenting it. The caller may present it afterward
 * or emit a response immediately for an already remembered answer. */
void gnc_gui_query_bind_dialog_response (GtkDialog *dialog,
                                        GncGuiQueryResponseCallback completed,
                                        gpointer user_data);

void gnc_choose_radio_option_dialog_async (GtkWidget *parent,
                                           const char *title,
                                           const char *msg,
                                           const char *button_name,
                                           int default_value,
                                           GList *radio_list,
                                           GncGuiQueryResponseCallback completed,
                                           gpointer user_data);

#ifdef __cplusplus
}
#endif


#endif
