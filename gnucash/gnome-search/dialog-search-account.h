/*
 * dialog-search-account.h -- Account selection dialog for search criteria
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
 * along with this program; if not, see
 * <https://www.gnu.org/licenses/>.
 */

#ifndef _DIALOG_SEARCH_ACCOUNT_H
#define _DIALOG_SEARCH_ACCOUNT_H

#include <gtk/gtk.h>
#include "search-account.h"

G_BEGIN_DECLS

#define GNC_TYPE_SEARCH_ACCOUNT_DIALOG (gnc_search_account_dialog_get_type ())
G_DECLARE_FINAL_TYPE (GncSearchAccountDialog, gnc_search_account_dialog, GNC, SEARCH_ACCOUNT_DIALOG, GtkDialog)

struct _GncSearchAccountDialog
{
    GtkDialog parent_instance;

    GNCSearchAccount *search_account;
    GtkWidget *account_tree;
};

GtkWidget *gnc_search_account_dialog_new (GtkWindow *parent, GNCSearchAccount *search_account);

GList *gnc_search_account_dialog_get_selected_accounts (GncSearchAccountDialog *dialog);

G_END_DECLS

#endif /* _DIALOG_SEARCH_ACCOUNT_H */
