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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include "dialog-budget.h"
#include "glade-gnc-dialogs.h"
#include "gnc-datedelta.h"
#include "guile-util.h"
#include "messages.h"


struct _BudgetDialog
{
  GtkWidget *dialog;

  GtkWidget *entry_tree;

  GtkWidget *budget_name_entry;

  GtkWidget *entry_description_entry;
  GtkWidget *entry_type_menu;

  GtkWidget *sub_description_entry;
  GtkWidget *sub_amount_entry;
  GtkWidget *sub_period_delta;
  GtkWidget *sub_mechanism_menu;
  GtkWidget *sub_grace_delta;

  GtkWidget *apply_button;

  SCM apply_func;
  SCM apply_func_id;

  SCM current_entry;
  SCM current_subentry;

  gboolean budget_changed;
  gboolean entry_changed;
  gboolean subentry_changed;
};

typedef enum
{
  BUDGET_ENTRY,
  BUDGET_SUBENTRY
} EntryType;

typedef struct _BudgetEntry
{
  EntryType type;
  SCM entry;
  SCM entry_id;
} BudgetEntry;

typedef struct _BudgetSubEntry
{
  EntryType type;
  SCM subentry;
  SCM subentry_id;
} BudgetSubEntry;

typedef union
{
  EntryType type;
  BudgetEntry entry_s;
  BudgetSubEntry subentry_s;
} BudgetItem;

static struct
{
  SCM entry_description;
  SCM entry_action;
  SCM entry_subentries;

  SCM subentry_description;
} getters;

static gboolean getters_initialized = FALSE;


static void
initialize_getters()
{
  if (getters_initialized)
    return;

  getters.entry_description = gh_eval_str("budget-entry-get-description");
  getters.entry_action      = gh_eval_str("budget-entry-get-action");
  getters.entry_subentries  = gh_eval_str("budget-entry-get-subentries");

  getters.subentry_description =
    gh_eval_str("budget-subentry-get-description");

  getters_initialized = TRUE;
}


static void
destroy_subentry(gpointer data)
{
  BudgetSubEntry *bse = data;

  if (bse == NULL) return;

  gnc_unregister_c_side_scheme_ptr_id(bse->subentry_id);

  g_free(bse);
}

static void
destroy_entry(gpointer data)
{
  BudgetEntry *be = data;

  if (be == NULL) return;

  gnc_unregister_c_side_scheme_ptr_id(be->entry_id);

  g_free(be);
}

static char *
SCM_to_description(SCM value)
{
  if (gh_string_p(value))
  {
    char * string;

    string = gh_scm2newstr(value, NULL);
    if (string != NULL)
    {
      char *temp;

      temp = string;
      string = g_strdup(temp);
      free(temp);

      return string;
    }
  }

  return g_strconcat("<", NO_DESC_STR, ">", NULL);
}

static void
load_entry(BudgetDialog *bd)
{
  char *string;
  SCM entry;

  entry = bd->current_entry;

  if (entry != SCM_UNDEFINED)
  {
    SCM value;

    value = gh_call1(getters.entry_description, entry);
    string = SCM_to_description(value);
    gtk_entry_set_text(GTK_ENTRY(bd->entry_description_entry), string);
    g_free(string);

    value = gh_call1(getters.entry_action, entry);
    gtk_option_menu_set_history(GTK_OPTION_MENU(bd->entry_type_menu),
                                gh_scm2bool(value) ? 0 : 1);
  }
}

static void
fill_subentries(GtkCTree *ctree, GtkCTreeNode *entry_node, SCM entry)
{
  BudgetSubEntry *bse;
  GtkCTreeNode *node;
  gchar *text[2];
  SCM subentries;
  SCM subentry;
  SCM value;

  text[1] = NULL;

  subentries = gh_call1(getters.entry_subentries, entry);

  while (gh_list_p(subentries) && !gh_null_p(subentries))
  {
    subentry   = gh_car(subentries);
    subentries = gh_cdr(subentries);

    value = gh_call1(getters.subentry_description, subentry);
    text[0] = SCM_to_description(value);

    node = gtk_ctree_insert_node(ctree, entry_node, NULL, text, 0,
                                 NULL, NULL, NULL, NULL, TRUE, FALSE);

    bse = g_new0(BudgetSubEntry, 1);

    bse->type = BUDGET_SUBENTRY;
    bse->subentry = subentry;
    bse->subentry_id = gnc_register_c_side_scheme_ptr(subentry);

    gtk_ctree_node_set_row_data_full(ctree, node, bse, destroy_subentry);

    g_free(text[0]);
  }
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

  gtk_clist_clear(GTK_CLIST(ctree));

  while (gh_list_p(budget) && !gh_null_p(budget))
  {
    entry  = gh_car(budget);
    budget = gh_cdr(budget);

    value = gh_call1(getters.entry_description, entry);
    text[0] = SCM_to_description(value);

    node = gtk_ctree_insert_node(ctree, NULL, NULL, text, 0, NULL,
                                 NULL, NULL, NULL, FALSE, FALSE);

    be = g_new0(BudgetEntry, 1);

    be->type = BUDGET_ENTRY;
    be->entry = entry;
    be->entry_id = gnc_register_c_side_scheme_ptr(entry);

    gtk_ctree_node_set_row_data_full(ctree, node, be, destroy_entry);

    fill_subentries(ctree, node, entry);

    g_free(text[0]);
  }

  gtk_clist_thaw(GTK_CLIST(ctree));
}

static void
load_budget(BudgetDialog *bd, SCM budget)
{
  fill_entry_tree(bd->entry_tree, budget);
}

static void
budget_changed(BudgetDialog *bd)
{
  bd->budget_changed = TRUE;

  gtk_widget_set_sensitive(bd->apply_button, TRUE);
}

static void
budget_name_entry_changed(GtkEditable *editable, BudgetDialog *bd)
{
  budget_changed(bd);
}

static void
allow_edits(BudgetDialog *bd, gboolean allow_edits)
{
  GtkWidget *widget;

  widget = gtk_object_get_data(GTK_OBJECT(bd->dialog), "entry_frame");
  gtk_widget_set_sensitive(widget, allow_edits);

  widget = gtk_object_get_data(GTK_OBJECT(bd->dialog), "subentry_frame");
  gtk_widget_set_sensitive(widget, allow_edits);
}

void
on_budget_entry_tree_tree_select_row   (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data)
{
  GtkObject *obj = GTK_OBJECT(user_data);
  GtkCTreeRow *row;

  BudgetSubEntry *bse;
  BudgetDialog *bd;
  BudgetEntry *be;
  BudgetItem *bi;

  if (column < 0)
    return;

  row = GTK_CTREE_ROW(node);

  bd = gtk_object_get_data(obj, "budget_dialog_structure");
  if (bd == NULL)
    return;

  bd->current_entry    = SCM_UNDEFINED;
  bd->current_subentry = SCM_UNDEFINED;

  bi = gtk_ctree_node_get_row_data(ctree, GTK_CTREE_NODE(node));
  switch (bi->type)
  {
    case BUDGET_ENTRY: {
      be = &bi->entry_s;

      bd->current_entry = be->entry;

      if (row->children == NULL)
        break;

      bse = gtk_ctree_node_get_row_data(ctree, row->children);
      if ((bse == NULL) || (bse->type != BUDGET_SUBENTRY))
        break;

      bd->current_subentry = bse->subentry;
    }
    break;

    case BUDGET_SUBENTRY: {
      bse = &bi->subentry_s;

      bd->current_subentry = bse->subentry;

      if (row->parent == NULL)
        break;

      be = gtk_ctree_node_get_row_data(ctree, row->parent);
      if ((be == NULL) || (be->type != BUDGET_ENTRY))
        break;

      bd->current_entry = be->entry;
    }
    break;

    default:
      g_warning("on_budget_entry_tree_tree_select_row: bad type");
      break;
  }

  load_entry(bd);
}

void
on_budget_entry_tree_tree_unselect_row (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data)
{
  GtkObject *obj = GTK_OBJECT(user_data);
  BudgetDialog *bd;

  if (column < 0)
    return;

  bd = gtk_object_get_data(obj, "budget_dialog_structure");
  if (bd == NULL)
    return;

  bd->current_entry    = SCM_UNDEFINED;
  bd->current_subentry = SCM_UNDEFINED;
}

BudgetDialog *
gnc_ui_budget_dialog_create(SCM budget, SCM apply_func)
{
  BudgetDialog *bd;
  GtkObject *bdo;
  GtkWidget *button;
  GtkWidget *arrow;
  GtkWidget *box;

  initialize_getters();

  bd = g_new0(BudgetDialog, 1);

  bd->current_entry = SCM_UNDEFINED;
  bd->current_subentry = SCM_UNDEFINED;

  bd->dialog = create_Budget_Dialog();
  bdo = GTK_OBJECT(bd->dialog);

  bd->apply_func = apply_func;
  bd->apply_func_id = gnc_register_c_side_scheme_ptr(apply_func);

  bd->budget_name_entry = gtk_object_get_data(bdo, "budget_name_entry");
  gtk_signal_connect(GTK_OBJECT(bd->budget_name_entry), "changed",
                     GTK_SIGNAL_FUNC(budget_name_entry_changed), bd);

  bd->entry_tree = gtk_object_get_data(bdo, "entry_tree");

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

  box = gtk_object_get_data(bdo, "period_box");
  bd->sub_period_delta = gnc_date_delta_new(FALSE);
  gtk_box_pack_start(GTK_BOX(box), bd->sub_period_delta, FALSE, FALSE, 0);
  gtk_widget_show(bd->sub_period_delta);

  box = gtk_object_get_data(bdo, "grace_box");
  bd->sub_grace_delta = gnc_date_delta_new(FALSE);
  gtk_box_pack_start(GTK_BOX(box), bd->sub_grace_delta, FALSE, FALSE, 0);
  gtk_widget_show(bd->sub_grace_delta);

  bd->apply_button = gtk_object_get_data(bdo, "apply_button");
  gtk_widget_set_sensitive(bd->apply_button, FALSE);

  gtk_object_set_data(bdo, "budget_dialog_structure", bd);

  load_budget(bd, budget);

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

  bd->dialog = NULL;
  gnc_unregister_c_side_scheme_ptr_id(bd->apply_func_id);

  g_free(bd);

  gtk_object_set_data(object, "budget_dialog_structure", NULL);
}

void
on_budget_cancel_button_clicked(GtkButton       *button,
                                gpointer         user_data)
{
  GnomeDialog *dialog = user_data;
  BudgetDialog *bd;

  if (dialog == NULL)
    return;

  bd = gtk_object_get_data(GTK_OBJECT(user_data), "budget_dialog_structure");
  gnc_ui_budget_dialog_destroy(bd);
}
