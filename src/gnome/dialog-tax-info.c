/********************************************************************\
 * dialog-tax-info.c -- tax information dialog                      *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
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

#include "config.h"

#include <gnome.h>

#include "account-tree.h"
#include "glade-gnc-dialogs.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "messages.h"


#define DIALOG_TAX_INFO_CM_CLASS "dialog-tax-info"

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

typedef struct
{
  GtkWidget * dialog;

  GtkWidget * account_tree;

  GtkWidget * tax_related_button;
  GtkWidget * txf_category_clist;
  GtkWidget * txf_help_text;
  GtkWidget * current_account_button;
} TaxInfoDialog;


static int
window_destroy_cb (GtkObject *object, gpointer data)
{
  TaxInfoDialog *ti_dialog = data;

  gnc_unregister_gui_component_by_data (DIALOG_TAX_INFO_CM_CLASS, ti_dialog);

  g_free (ti_dialog);
}

static void
gnc_tax_info_dialog_create (GtkWidget * parent, TaxInfoDialog *ti_dialog)
{
  GtkWidget *dialog;
  GtkObject *tido;

  dialog = create_Tax_Information_Dialog ();
  ti_dialog->dialog = dialog;
  tido = GTK_OBJECT (dialog);

  gtk_signal_connect(tido, "destroy",
                     GTK_SIGNAL_FUNC (window_destroy_cb), ti_dialog);

  /* parent */
  if (parent != NULL)
    gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (parent));

  /* default to ok */
  gnome_dialog_set_default (GNOME_DIALOG(dialog), 0);

  /* account tree */
  {
    GNCAccountTree *tree;
    AccountViewInfo info;
    GNCAccountType type;
    GtkWidget *scroll;

    ti_dialog->account_tree = gnc_account_tree_new ();
    tree = GNC_ACCOUNT_TREE (ti_dialog->account_tree);

    gtk_clist_column_titles_hide (GTK_CLIST (ti_dialog->account_tree));
    gnc_account_tree_hide_all_but_name (tree);

    gnc_account_tree_get_view_info (tree, &info);

    for (type = 0; type < NUM_ACCOUNT_TYPES; type++)
      info.include_type[type] = (type == INCOME) || (type == EXPENSE);

    gnc_account_tree_set_view_info (tree, &info);

    gnc_account_tree_refresh (tree);

    gtk_widget_show (ti_dialog->account_tree);

    scroll = gtk_object_get_data (tido, "account_scroll");
    gtk_container_add (GTK_CONTAINER (scroll), ti_dialog->account_tree);
  }
}

static void
close_handler (gpointer user_data)
{
  TaxInfoDialog *ti_dialog = user_data;

  gnome_dialog_close (GNOME_DIALOG (ti_dialog->dialog));
}

/********************************************************************\
 * gnc_tax_info_dialog                                              *
 *   opens up a window to set account tax information               *
 *                                                                  * 
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_tax_info_dialog (GtkWidget * parent)
{
  TaxInfoDialog *ti_dialog;

  ti_dialog = g_new0 (TaxInfoDialog, 1);

  gnc_tax_info_dialog_create (parent, ti_dialog);

  gnc_register_gui_component (DIALOG_TAX_INFO_CM_CLASS,
                              NULL, close_handler, ti_dialog);

  gtk_widget_grab_focus (ti_dialog->account_tree);

  gtk_widget_show (ti_dialog->dialog);
}
