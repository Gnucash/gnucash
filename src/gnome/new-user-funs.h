/********************************************************************\
 * new-user-funs.h -- new user functionality for GnuCash            *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef _NEW_USER_FUNS_H_
#define _NEW_USER_FUNS_H_

#include <glib.h>

#include "gnc-commodity-edit.h"
#include "Group.h"

int gnc_ui_show_new_user_window(void);
int gnc_ui_delete_new_user_window(void);

int gnc_ui_show_nu_cancel_dialog(void);
int gnc_ui_delete_nu_cancel_dialog(void);

int gnc_ui_show_nu_account_list(void);
int gnc_ui_delete_nu_account_list(void);

GtkCList* gnc_new_user_get_clist(void);
GtkWidget* gnc_new_user_get_widget(const char *name);
AccountGroup* gnc_new_user_merge_groups(GSList *dalist);
GNCCommodityEdit * gnc_get_new_user_commodity_editor(void);

#endif /* NEW_USER_FUNS_H */
