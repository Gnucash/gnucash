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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef DIALOG_QIF_IMPORT_H
#define DIALOG_QIF_IMPORT_H

#include <libguile.h>
#include <gtk/gtk.h> /* For GtkWidget */

/* QIF Import Windows ***********************************************/

typedef struct _qifimportwindow QIFImportWindow;

QIFImportWindow * gnc_ui_qif_import_druid_make(void);
void              gnc_ui_qif_import_druid_destroy (QIFImportWindow * window);
SCM               gnc_ui_qif_import_druid_get_mappings(QIFImportWindow * w);

/* The gnc_file_qif_import() routine will pop up a standard file
 *     selection dialogue asking the user to pick a QIF file. If one
 *     is selected the the QIF file is opened and read. It's contents
 *     are merged into the existing session (if any). The current
 *     session continues to remain open for editing. */
void              gnc_file_qif_import (void);
void              gnc_ui_qif_import_create_plugin(void);

#endif
