/********************************************************************\
 * import-account-matcher.h - flexible account picker/matcher       *
 *                                                                  *
 * Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>        *
 * Copyright (C) 2012 Robert Fewell                                 *
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
/** @addtogroup Import_Export
    @{ */
/**@file import-account-matcher.h
  @brief  Generic and very flexible account matcher/picker
 @author Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>
 */
#ifndef IMPORT_ACCOUNT_MATCHER_H
#define IMPORT_ACCOUNT_MATCHER_H

#include "Account.h"
#include <gtk/gtk.h>

#include "gnc-tree-view-account.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
    GtkWidget           *dialog;                         /* Dialog Widget */
    GtkWidget           *ok_button;                      /* ok button Widget */
    GncTreeViewAccount  *account_tree;                   /* Account tree */
    GtkWidget           *account_tree_sw;                /* Scroll Window for Account tree */
    const gchar         *account_human_description;      /* description for on line id, incoming */
    const gnc_commodity *new_account_default_commodity;  /* new account default commodity, incoming */
    GNCAccountType       new_account_default_type;       /* new account default type, incoming */
    GtkWidget           *whbox;                          /* Warning HBox */
    GtkWidget           *warning;                        /* Warning Label */
} AccountPickerDialog;

/** Find an account by its online identifier without showing user interface. */
Account *gnc_import_find_account_by_online_id(const gchar *online_id,
                                               GNCAccountType default_type);

typedef void (*GncImportAccountCallback)(Account *account, gboolean accepted,
                                         gpointer user_data);
void gnc_import_select_account_async(GtkWidget *parent,
                                     const gchar *account_online_id_value,
                                     gboolean prompt_on_no_match,
                                     const gchar *account_human_description,
                                     const gnc_commodity *new_account_default_commodity,
                                     GNCAccountType new_account_default_type,
                                     Account *default_selection,
                                     GncImportAccountCallback callback,
                                     gpointer user_data);

#ifdef __cplusplus
}
#endif

#endif
/**@}*/
