/********************************************************************\
 * dialog-print-check.h : dialog to control check printing          *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef __DIALOG_PRINT_CHECK_H_
#define __DIALOG_PRINT_CHECK_H_

#include "config.h"

#include <gnome.h>
#include <guile/gh.h>

#include "print-session.h"

typedef struct {
  GtkWidget * dialog;

  GtkWidget * format_picker;
  GtkWidget * position_picker;
  GtkWidget * dformat_picker;
  GtkWidget * payee_x,  * payee_y;
  GtkWidget * date_x,   * date_y;
  GtkWidget * words_x,  * words_y;
  GtkWidget * number_x, * number_y;
  GtkWidget * memo_x,   * memo_y;

  GtkWidget * check_position;
  GtkWidget * format_entry;

  GtkWidget * units_picker;
  
  SCM callback;

} PrintCheckDialog;

PrintCheckDialog * gnc_ui_print_check_dialog_create(SCM callback);
void gnc_ui_print_check_dialog_destroy(PrintCheckDialog * pcd);
void gnc_ui_print_check_dialog_ok_cb(GtkButton * button,
                                     gpointer  user_data);

void gnc_ui_print_check_dialog_cancel_cb(GtkButton * button,
                                         gpointer user_data);

void gnc_ui_print_check_dialog_help_cb(GtkButton * button,
                                       gpointer user_data);

#endif
