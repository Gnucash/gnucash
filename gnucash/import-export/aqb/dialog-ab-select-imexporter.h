/*
 * dialog-ab-select-imexporter.h --
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/**
 * @addtogroup Import_Export
 * @{
 * @addtogroup AqBanking
 * @{
 * @file dialog-ab-select-imexporter.h
 * @brief Dialog to select AQBanking importer/exporter and format profile.
 * @author Copyright (C) 2022 John Ralls <jralls@ceridwen.us>
 */

#ifndef DIALOG_AB_SELECT_IMEXPORTER_H
#define DIALOG_AB_SELECT_IMEXPORTER_H

#include "gnc-ab-utils.h"
#include <gtk/gtk.h>
#include <aqbanking/banking.h>

typedef struct _GncABSelectImExDlg GncABSelectImExDlg;

G_BEGIN_DECLS

/**
 * Create an AQBanking importer/exporter selection dialog.
 * @param parent A widget to center the dialog over.
 * @param abi The AQBanking instance to query.
 * @return a GncABSelectImExDlg.
 */
GncABSelectImExDlg* gnc_ab_select_imex_dlg_new (GtkWidget* parent,
                                                AB_BANKING* abi);

/**
 * Destroy an AQBanking importer/exporter selection dialog.
 * @param imexd the dialog.
 */
void gnc_ab_select_imex_dlg_destroy (GncABSelectImExDlg* imexd);

/**
 * Run an AQBanking importer/exporter selection dialog.
 * @param imexd the dialog.
 * @return A GTK_RESPONSE status.
 */
int gnc_ab_select_imex_dlg_run (GncABSelectImExDlg* imexd);

/**
 * Get the selected importer/exporter name
 * @param imexd the dialog with the selection
 * @return the selected importer/exporter name, free with g_free.
 */
char*
gnc_ab_select_imex_dlg_get_imexporter_name (GncABSelectImExDlg* imexd);

/**
 * Get the selected importer/exporter name
 * @param imexd the dialog with the selection
 * @para  name the importer/exporter name to select.
 */
void
gnc_ab_select_imex_dlg_set_imexporter_name (GncABSelectImExDlg* imexd, const char* name);

/**
 * Get the selected file format profile name
 * @param imexd the dialog with the selection
 * @return the selected format profile name, free with g_free.
 */
char*
gnc_ab_select_imex_dlg_get_profile_name (GncABSelectImExDlg* imexd);

/**
 * Get the selected file format profile name
 * @param imexd the dialog with the selection
 * @param name the profile to select.
 */
void
gnc_ab_select_imex_dlg_set_profile_name (GncABSelectImExDlg* imexd, const char* name);

G_END_DECLS

#endif //DIALOG_AB_SELECT_IMEXPORTER_H
/**
 * @}
 * @}
 */
