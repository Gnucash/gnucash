/********************************************************************\
 * dialog-budget.c : dialog for entering a budget                   *
 * Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>      *
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

#include "config.h"

#include "dialog-budget.h"
#include "glade-gnc-dialogs.h"
#include "guile-util.h"


struct _BudgetDialog
{
  GtkWidget *dialog;

  GtkWidget *entry_tree;

  GtkWidget *entry_description_entry;
  GtkWidget *entry_type_menu;

  GtkWidget *sub_description_entry;
  GtkWidget *sub_amount_entry;
  GtkWidget *sub_mechanism_menu;

  SCM apply_func;
  SCM apply_func_id;
};

typedef struct _BudgetEntry
{
  SCM entry;
  SCM entry_id;
} BudgetEntry;


static struct
{
  SCM entry_description;
} getters;

static gboolean getters_initialized = FALSE;


static void
initialize_getters()
{
  if (getters_initialized)
    return;

  getters.entry_description = gh_eval_str("budget-entry-get-description");

  getters_initialized = TRUE;
}


static void
destroy_entry(gpointer data)
{
  BudgetEntry *be = data;

  if (be == NULL) return;

  gnc_unregister_c_side_scheme_ptr_id(be->entry_id);

  g_free(be);
}

static void
fill_entry_tree(GtkWidget *entry_tree, SCM budget)
{
  GtkCTreeNode *node;
  GtkCTree *ctree;
  gchar *text[2];
  BudgetEntry *be;
  SCM entry;
  SCM value;

  text[1] = NULL;

  ctree = GTK_CTREE(entry_tree);

  gtk_clist_freeze(GTK_CLIST(ctree));

  while (gh_list_p(budget) && !gh_null_p(budget))
  {
    entry  = gh_car(budget);
    budget = gh_cdr(budget);

    value = gh_call1(getters.entry_description, entry);
    text[0] = gh_scm2newstr(value, NULL);
    if (text[0] == NULL)
      continue;

    node = gtk_ctree_insert_node(ctree, NULL, NULL, text, 0,
                                 NULL, NULL, NULL, NULL, FALSE, FALSE);

    be = g_new0(BudgetEntry, 1);

    be->entry = entry;
    be->entry_id = gnc_register_c_side_scheme_ptr(entry);

    gtk_ctree_node_set_row_data_full(ctree, node, be, destroy_entry);
  }

  gtk_clist_thaw(GTK_CLIST(ctree));
}

BudgetDialog *
gnc_ui_budget_dialog_create(SCM budget, SCM apply_func)
{
  BudgetDialog *bd;
  GtkObject *bdo;
  GtkWidget *button;
  GtkWidget *arrow;

  initialize_getters();

  bd = g_new0(BudgetDialog, 1);

  bd->dialog = create_Budget_Dialog();
  bdo = GTK_OBJECT(bd->dialog);

  bd->apply_func = apply_func;
  bd->apply_func_id = gnc_register_c_side_scheme_ptr(apply_func);

  bd->entry_tree = gtk_object_get_data(bdo, "entry_tree");
  fill_entry_tree(bd->entry_tree, budget);

  bd->entry_description_entry =
    gtk_object_get_data(bdo, "entry_description_entry");
  bd->entry_type_menu =
    gtk_object_get_data(bdo, "entry_type_menu");

  bd->sub_description_entry =
    gtk_object_get_data(bdo, "sub_description_entry");
  bd->sub_amount_entry =
    gtk_object_get_data(bdo, "sub_amount_entry");
  bd->sub_mechanism_menu =
    gtk_object_get_data(bdo, "sub_mechanism_menu");

  button = gtk_object_get_data(bdo, "up_button");
  arrow = gtk_arrow_new(GTK_ARROW_UP, GTK_SHADOW_IN);
  gtk_container_remove(GTK_CONTAINER(button), GTK_BIN(button)->child);
  gtk_container_add(GTK_CONTAINER(button), arrow);

  button = gtk_object_get_data(bdo, "down_button");
  arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_IN);
  gtk_container_remove(GTK_CONTAINER(button), GTK_BIN(button)->child);
  gtk_container_add(GTK_CONTAINER(button), arrow);

  gtk_object_set_data(bdo, "budget_dialog_structure", bd);

  gtk_widget_show_all(bd->dialog);

  return bd;
}

void
gnc_ui_budget_dialog_destroy(BudgetDialog *bd)
{
  if (bd == NULL)
    return;

  if (bd->dialog != NULL)
    gnome_dialog_close(GNOME_DIALOG(bd->dialog));

  bd->dialog = NULL;
}

void
on_Budget_Dialog_destroy(GtkObject       *object,
                         gpointer         user_data)
{
  BudgetDialog *bd;

  if (object == NULL) return;

  bd = gtk_object_get_data(object, "budget_dialog_structure");
  if (bd == NULL)
    return;

  gnc_unregister_c_side_scheme_ptr_id(bd->apply_func_id);

  g_free(bd);

  gtk_object_set_data(object, "budget_dialog_structure", NULL);
}
