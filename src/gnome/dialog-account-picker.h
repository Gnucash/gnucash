/********************************************************************\
 * dialog-account-picker.h -- window for picking a GNUcash account  *
 *                       (GnuCash)                                  *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef __DIALOG_ACCOUNT_PICKER_H_
#define __DIALOG_ACCOUNT_PICKER_H_

#include "glade-gnc-dialogs.h"
#include "glade-cb-gnc-dialogs.h"

#include <guile/gh.h>

SCM accountPickerBox(char *initial_pick, int initial_type);

typedef struct _accountpickerdialog {
  
  GtkWidget * dialog;
  GtkWidget * treeview;
  GtkWidget * acct_entry;
  GtkWidget * descript_entry;
  GtkWidget * type_picker;

  SCM scm_acct_info;

} QIFAccountPickerDialog;


#endif
