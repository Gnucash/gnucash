/********************************************************************\
 * new-user-callbacks.h - new user functionality for GnuCash        *
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

#ifndef _NEW_USER_CALLBACKS_H_
#define _NEW_USER_CALLBACKS_H_

#include <gnome.h>
#include <glib.h>

gboolean
on_newUserStartPage_next               (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

gboolean
on_chooseAccountTypesPage_next         (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
on_newUserDruidFinishPage_finish       (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
on_accountChooseDruidPage_cancel       (GnomeDruid      *gnomedruid,
                                        gpointer         user_data);

void
on_newAccountCancelDialog_OKButton_clicked
                                        (GtkButton       *button,
                                         gpointer         user_data);

void
on_newAccountCurrencyChoosePage_prepare
                                        (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                         gpointer         user_data);

void
on_chooseAccountTypesPage_prepare      (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
on_newUserDruidFinishPage_prepare      (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
on_newAccountTypesList_select_row      (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data);

void
on_newAccountTree_select_row           (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data);


#endif /* _NEW_USER_CALLBACKS_H_ */    
