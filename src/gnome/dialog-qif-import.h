/********************************************************************\
 * dialog-qif-import.h -- window for controlling import of QIF data *
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

#ifndef __DIALOG_QIF_IMPORT_H_
#define __DIALOG_QIF_IMPORT_H_

#include <guile/gh.h>

#include "glade-qif-import.h"
#include "glade-cb-qif-import.h"

typedef struct _qifimportwindow
{

  GtkWidget * parent;
  GtkWidget * dialog;

  SCM       imported_files;
  SCM       selected_file;
  SCM       mapping_info; 
  SCM       cat_display_info;
  SCM       acct_display_info;

} QIFImportWindow;

QIFImportWindow * gnc_ui_qif_import_dialog_make(GtkWidget * parent);
void gnc_ui_qif_import_dialog_destroy(QIFImportWindow * window);

#endif
