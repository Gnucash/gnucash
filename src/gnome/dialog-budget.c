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
#include "dialog-utils.h"
#include "glade-gnc-dialogs.h"
#include "gnc-datedelta.h"
#include "guile-util.h"
#include "messages.h"
#include "util.h"


struct _BudgetDialog
{
  GtkWidget *dialog;

  GtkWidget *entry_tree;

  GtkWidget *budget_name_entry;

  GtkWidget *entry_description_entry;
  GtkWidget *entry_type_menu;
  GtkWidget *entry_frame;

  GtkWidget *subentry_description_entry;
  GtkWidget *subentry_amount_entry;
  GtkWidget *subentry_period_delta;
  GtkWidget *subentry_mechanism_menu;
  GtkWidget *subentry_grace_delta;
  GtkWidget *subentry_frame;

  GtkWidget *apply_button;

  SCM apply_func;
  SCM apply_func_id;

  SCM current_entry;
  SCM current_subentry;

  gboolean budget_changed;
  gboolean entry_changed;
  gboolean subentry_changed;

  gint ignore_changes;
  gint num_entries;
};

typedef enum
{
  BUDGET_ENTRY,
  BUDGET_SUBENTRY,
  BUDGET_ANY
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
  SCM subentry_amount;
  SCM subentry_period;
  SCM subentry_period_type;
  SCM subentry_mechanism;

  SCM bill_get_start;
  SCM bill_get_end;

} getters;

static struct
{
  SCM is_bill_mechanism;
  SCM is_recurring_mechanism;
  SCM is_contingency_mechanism;
  SCM is_nominal_mechanism;
} preds;

typedef enum
{
  MECHANISM_NOMINAL,
  MECHANISM_BILL,
  MECHANISM_RECURRING,
  MECHANISM_CONTINGENCY,
  MECHANISM_NONE
} MechanismType;

static gboolean getters_initialized = FALSE;
static gboolean preds_initialized = FALSE;


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
  getters.subentry_amount = gh_eval_str("budget-subentry-get-amount");
  getters.subentry_period = gh_eval_str("budget-subentry-get-period");
  getters.subentry_period_type =
    gh_eval_str("budget-subentry-get-period-type");
  getters.subentry_mechanism = gh_eval_str("budget-subentry-get-mechanism");

  getters.bill_get_start = gh_eval_str("budget-bill-get-window-start-day");
  getters.bill_get_end   = gh_eval_str("budget-bill-get-window-end-day");

  getters_initialized = TRUE;
}

static void
initialize_preds()
{
  if (preds_initialized)
    return;

  preds.is_bill_mechanism = gh_eval_str("budget-bill-pred");
  preds.is_recurring_mechanism = gh_eval_str("budget-recurring-pred");
  preds.is_contingency_mechanism = gh_eval_str("budget-contingency-pred");
  preds.is_nominal_mechanism = gh_eval_str("budget-nominal-pred");

  preds_initialized = TRUE;
}

static MechanismType
mechanism_type(SCM mechanism)
{
  SCM result;

  result = gh_call1(preds.is_nominal_mechanism, mechanism);
  if (gh_scm2bool(result))
    return MECHANISM_NOMINAL;

  result = gh_call1(preds.is_contingency_mechanism, mechanism);
  if (gh_scm2bool(result))
    return MECHANISM_CONTINGENCY;

  result = gh_call1(preds.is_recurring_mechanism, mechanism);
  if (gh_scm2bool(result))
    return MECHANISM_RECURRING;

  result = gh_call1(preds.is_bill_mechanism, mechanism);
  if (gh_scm2bool(result))
    return MECHANISM_BILL;

  g_warning("mechanism_type: bad mechanism");

  return MECHANISM_NONE;
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
string_to_description(char *string, gboolean no_blank)
{
  if ((string == NULL) || (*string == '\0'))
  {
    if (no_blank)
      return g_strconcat("<", NO_DESC_STR, ">", NULL);
    else
      return g_strdup("");
  }

  return g_strdup(string);
}

static char *
SCM_to_description(SCM value, gboolean no_blank)
{
  if (gh_string_p(value))
  {
    char * string;

    string = gh_scm2newstr(value, NULL);
    if (string != NULL)
    {
      char *temp;

      temp = string;
      string = string_to_description(string, no_blank);
      free(temp);

      return string;
    }
  }

  return string_to_description(NULL, no_blank);
}

static void
load_entry(BudgetDialog *bd)
{
  char *string;
  SCM entry;

  entry = bd->current_entry;
  bd->ignore_changes++;

  if (entry != SCM_UNDEFINED)
  {
    SCM value;

    value = gh_call1(getters.entry_description, entry);
    string = SCM_to_description(value, FALSE);
    gtk_entry_set_text(GTK_ENTRY(bd->entry_description_entry), string);
    g_free(string);

    value = gh_call1(getters.entry_action, entry);
    gtk_option_menu_set_history(GTK_OPTION_MENU(bd->entry_type_menu),
                                gh_scm2bool(value) ? 0 : 1);
  }
  else
  {
  }

  bd->ignore_changes--;
}

static GNCDateDeltaUnits
period_units(SCM period_type)
{
  GNCDateDeltaUnits units;
  char *string;

  if (!gh_symbol_p(period_type))
    return GNC_DATE_DELTA_DAYS;

  string = gh_symbol2newstr(period_type, NULL);

  if (safe_strcmp(string, "gnc:budget-day") == 0)
    units = GNC_DATE_DELTA_DAYS;
  else if (safe_strcmp(string, "gnc:budget-week") == 0)
    units = GNC_DATE_DELTA_WEEKS;
  else if (safe_strcmp(string, "gnc:budget-month") == 0)
    units = GNC_DATE_DELTA_MONTHS;
  else if (safe_strcmp(string, "gnc:budget-year") == 0)
    units = GNC_DATE_DELTA_YEARS;
  else
  {
    g_warning("period_units: bad period type");
    units = GNC_DATE_DELTA_DAYS;
  }

  if (string != NULL)
    free(string);

  return units;
}

static void
allow_mechanism_edits(BudgetDialog *bd, gboolean allow)
{
  GtkWidget *box;

  box = gtk_object_get_data(GTK_OBJECT(bd->dialog), "bill_day_box");
  gtk_widget_set_sensitive(box, allow);

  box = gtk_object_get_data(GTK_OBJECT(bd->dialog), "grace_box");
  gtk_widget_set_sensitive(box, allow);
}

static void
load_subentry(BudgetDialog *bd)
{
  char *string;
  SCM subentry;

  subentry = bd->current_subentry;
  bd->ignore_changes++;

  if (subentry != SCM_UNDEFINED)
  {
    GNCDateDeltaUnits units;
    MechanismType mech_type;
    double amount;
    int period;
    SCM mechanism;
    SCM value;

    value = gh_call1(getters.subentry_description, subentry);
    string = SCM_to_description(value, FALSE);
    gtk_entry_set_text(GTK_ENTRY(bd->subentry_description_entry), string);
    g_free(string);

    value = gh_call1(getters.subentry_amount, subentry);
    amount = gh_scm2double(value);
    string = xaccPrintAmount(amount, PRTSEP, NULL);
    gtk_entry_set_text(GTK_ENTRY(bd->subentry_amount_entry), string);

    value = gh_call1(getters.subentry_period, subentry);
    period = gh_scm2int(value);
    gnc_date_delta_set_value(GNC_DATE_DELTA(bd->subentry_period_delta),
                             period);

    value = gh_call1(getters.subentry_period_type, subentry);
    units = period_units(value);
    gnc_date_delta_set_units(GNC_DATE_DELTA(bd->subentry_period_delta), units);

    mechanism = gh_call1(getters.subentry_mechanism, subentry);
    mech_type = mechanism_type(mechanism);
    gtk_option_menu_set_history(GTK_OPTION_MENU(bd->subentry_mechanism_menu),
                                mech_type);
    switch (mech_type)
    {
      case MECHANISM_BILL:
        value = gh_call1(getters.bill_get_start, mechanism);
        period = gh_scm2int(value);
        gnc_date_delta_set_value(GNC_DATE_DELTA(bd->subentry_grace_delta),
                                 period);

        allow_mechanism_edits(bd, TRUE);
        break;
      default:
        allow_mechanism_edits(bd, FALSE);
        break;
    }
  }
  else
  {
  }

  bd->ignore_changes--;
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
    text[0] = SCM_to_description(value, TRUE);

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

static int
fill_entry_tree(GtkWidget *entry_tree, SCM budget)
{
  GtkCTreeNode *node;
  GtkCTree *ctree;
  gchar *text[2];
  BudgetEntry *be;
  SCM entry;
  SCM value;
  int count = 0;

  text[1] = NULL;

  ctree = GTK_CTREE(entry_tree);

  gtk_clist_freeze(GTK_CLIST(ctree));

  gtk_clist_clear(GTK_CLIST(ctree));

  while (gh_list_p(budget) && !gh_null_p(budget))
  {
    entry  = gh_car(budget);
    budget = gh_cdr(budget);

    value = gh_call1(getters.entry_description, entry);
    text[0] = SCM_to_description(value, TRUE);

    node = gtk_ctree_insert_node(ctree, NULL, NULL, text, 0, NULL,
                                 NULL, NULL, NULL, FALSE, FALSE);

    be = g_new0(BudgetEntry, 1);

    be->type = BUDGET_ENTRY;
    be->entry = entry;
    be->entry_id = gnc_register_c_side_scheme_ptr(entry);

    gtk_ctree_node_set_row_data_full(ctree, node, be, destroy_entry);

    fill_subentries(ctree, node, entry);

    g_free(text[0]);

    count++;
  }

  gtk_clist_thaw(GTK_CLIST(ctree));

  return count;
}

static void
load_budget(BudgetDialog *bd, SCM budget)
{
  bd->ignore_changes++;
  bd->num_entries = fill_entry_tree(bd->entry_tree, budget);
  bd->ignore_changes--;
}

static void
budget_changed(BudgetDialog *bd)
{
  if (bd->ignore_changes)
    return;

  bd->budget_changed = TRUE;

  gtk_widget_set_sensitive(bd->apply_button, TRUE);
}

static GtkCTreeNode *
get_selected_node(BudgetDialog *bd, EntryType type)
{
  GtkCTree *ctree;
  GtkCTreeRow *row;
  GtkCTreeNode *node;
  BudgetItem *item;
  int r;

  ctree = GTK_CTREE(bd->entry_tree);
  r = GTK_CLIST(ctree)->focus_row;
  if (r < 0)
    return NULL;

  node = gtk_ctree_node_nth(ctree, r);
  if (node == NULL)
    return NULL;

  item = gtk_ctree_node_get_row_data(ctree, node);
  if ((item->type == type) || (type == BUDGET_ANY))
    return node;

  row = GTK_CTREE_ROW(node);

  switch (type)
  {
    case BUDGET_ENTRY:
      return row->parent;
      break;
    case BUDGET_SUBENTRY:
      return row->children;
      break;
    default:
      g_warning("budget_dialog_get_selected_node: bad entry type");
      return NULL;
      break;
  }
}

static void
entry_description_entry_changed(GtkEditable *editable, BudgetDialog *bd)
{
  GtkCTreeNode *node;
  BudgetEntry *be;
  gchar *text;

  if (bd->ignore_changes)
    return;

  budget_changed(bd);
  bd->entry_changed = TRUE;

  node = get_selected_node(bd, BUDGET_ENTRY);
  if (node == NULL)
    return;

  be = gtk_ctree_node_get_row_data(GTK_CTREE(bd->entry_tree), node);
  if ((be == NULL) || (be->type != BUDGET_ENTRY))
    return;

  text = gtk_entry_get_text(GTK_ENTRY(bd->entry_description_entry));
  text = string_to_description(text, TRUE);

  gtk_ctree_node_set_text(GTK_CTREE(bd->entry_tree), node, 0, text);

  g_free(text);
}

static void
subentry_description_entry_changed(GtkEditable *editable, BudgetDialog *bd)
{
  GtkCTreeNode *node;
  BudgetSubEntry *bse;
  gchar *text;

  if (bd->ignore_changes)
    return;

  budget_changed(bd);
  bd->subentry_changed = TRUE;

  node = get_selected_node(bd, BUDGET_SUBENTRY);
  if (node == NULL)
    return;

  bse = gtk_ctree_node_get_row_data(GTK_CTREE(bd->entry_tree), node);
  if ((bse == NULL) || (bse->type != BUDGET_SUBENTRY))
    return;

  text = gtk_entry_get_text(GTK_ENTRY(bd->subentry_description_entry));
  text = string_to_description(text, TRUE);

  gtk_ctree_node_set_text(GTK_CTREE(bd->entry_tree), node, 0, text);

  g_free(text);
}

static void
entry_up_button_clicked(GtkButton *button, BudgetDialog *bd)
{
  GtkCTreeRow *row;
  GtkCTreeNode *node;
  GtkCTreeNode *parent;
  GtkCTreeNode *sibling;
  GtkCTree *ctree;
  BudgetItem *bi;

  if (bd->num_entries == 0)
    return;

  node = get_selected_node(bd, BUDGET_ANY);
  if (node == NULL)
    return;

  ctree = GTK_CTREE(bd->entry_tree);
  row   = GTK_CTREE_ROW(node);

  parent  = row->parent;
  sibling = GTK_CTREE_NODE_PREV(node);

  if (sibling == NULL)
    return;

  bi = gtk_ctree_node_get_row_data(ctree, node);
  switch (bi->type)
  {
    case BUDGET_SUBENTRY:
      if (parent == sibling)
        return;
      break;
    case BUDGET_ENTRY:
      bi = gtk_ctree_node_get_row_data(ctree, sibling);
      while (bi->type != BUDGET_ENTRY)
      {
        sibling = GTK_CTREE_NODE_PREV(sibling);
        if (sibling == NULL)
          return;
        bi = gtk_ctree_node_get_row_data(ctree, sibling);
      }
      break;
    default:
      g_warning("entry_up_button_clicked: bad item type");
      return;
  }

  gtk_ctree_move(ctree, node, parent, sibling);
  if (gtk_ctree_node_is_visible(ctree, node) != GTK_VISIBILITY_FULL)
    gtk_ctree_node_moveto(ctree, node, 0, 0.5, 0);
}

static void
subentry_amount_entry_changed(GtkEditable *editable, BudgetDialog *bd)
{
  if (bd->ignore_changes)
    return;

  budget_changed(bd);
  bd->subentry_changed = TRUE;
}

static void
subentry_period_delta_changed(GNCDateDelta *delta, BudgetDialog *bd)
{
  if (bd->ignore_changes)
    return;

  budget_changed(bd);
  bd->subentry_changed = TRUE;
}

static void
subentry_grace_delta_changed(GNCDateDelta *delta, BudgetDialog *bd)
{
  if (bd->ignore_changes)
    return;

  budget_changed(bd);
  bd->subentry_changed = TRUE;
}

static gboolean
subentry_amount_entry_focus_out(GtkWidget *widget, GdkEventFocus *event,
                                BudgetDialog *bd)
{
  GtkEntry *entry = GTK_ENTRY(widget);
  gchar *new_string;
  gchar *string;
  double value;

  string = gtk_entry_get_text(entry);

  if ((string == NULL) || (*string == '\0'))
    return FALSE;

  value = xaccParseAmount(string, GNC_T);

  new_string = xaccPrintAmount(value, PRTSEP, NULL);

  if (safe_strcmp(string, new_string) == 0)
    return FALSE;

  gtk_entry_set_text(entry, new_string);

  return FALSE;
}

static void
budget_name_entry_changed(GtkEditable *editable, BudgetDialog *bd)
{
  if (bd->ignore_changes)
    return;

  budget_changed(bd);
}

static void
entry_type_menu_changed(GtkButton *button, BudgetDialog *bd)
{
  if (bd->ignore_changes)
    return;

  budget_changed(bd);
  bd->entry_changed = TRUE;
}

static void
mechanism_menu_changed(GtkButton *button, BudgetDialog *bd)
{
  MechanismType mech_type;

  if (bd->ignore_changes)
    return;

  budget_changed(bd);
  bd->entry_changed = TRUE;

  mech_type = gnc_option_menu_get_active(bd->subentry_mechanism_menu);
  switch (mech_type)
  {
    case MECHANISM_BILL:
      allow_mechanism_edits(bd, TRUE);
      break;
    default:
      allow_mechanism_edits(bd, FALSE);
      break;
  }
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
select_node (BudgetDialog *bd, GtkCTreeNode *node)
{
  GtkCTreeRow *row;
  GtkCTree *ctree;

  BudgetSubEntry *bse;
  BudgetEntry *be;
  BudgetItem *bi;

  ctree = GTK_CTREE(bd->entry_tree);

  row = GTK_CTREE_ROW(node);

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

  allow_edits(bd, ((bd->current_entry != SCM_UNDEFINED) ||
                   (bd->current_subentry != SCM_UNDEFINED)));
  load_entry(bd);
  load_subentry(bd);
}

void
on_budget_entry_tree_tree_select_row   (GtkCTree        *ctree,
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

  select_node(bd, GTK_CTREE_NODE(node));
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

  allow_edits(bd, FALSE);
  load_entry(bd);
  load_subentry(bd);
}

void
on_budget_entry_tree_scroll_vertical   (GtkCList        *clist,
                                        GtkScrollType    scroll_type,
                                        gfloat           position,
                                        gpointer         user_data)
{
  GtkObject *obj = GTK_OBJECT(user_data);
  GtkCTreeNode *node;
  BudgetDialog *bd;
  int row;

  bd = gtk_object_get_data(obj, "budget_dialog_structure");
  if (bd == NULL)
    return;

  row = GTK_CLIST(bd->entry_tree)->focus_row;
  if (row < 0)
  {
    allow_edits(bd, FALSE);
    return;
  }

  node = gtk_ctree_node_nth(GTK_CTREE(bd->entry_tree), row);
  if (node == NULL)
  {
    allow_edits(bd, FALSE);
    return;
  }

  select_node(bd, node);
}

static void
connect_entry_type_menu_item(GtkWidget *item, gpointer data)
{
  gtk_signal_connect(GTK_OBJECT(item), "activate",
                     GTK_SIGNAL_FUNC(entry_type_menu_changed), data);
}

static void
connect_mechanism_menu_item(GtkWidget *item, gpointer data)
{
  gtk_signal_connect(GTK_OBJECT(item), "activate",
                     GTK_SIGNAL_FUNC(mechanism_menu_changed), data);
}

BudgetDialog *
gnc_ui_budget_dialog_create(SCM budget, SCM apply_func)
{
  BudgetDialog *bd;
  GtkObject *bdo;
  GtkWidget *button;
  GtkWidget *arrow;
  GtkWidget *box;
  GtkWidget *menu;

  initialize_getters();
  initialize_preds();

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
  gtk_signal_connect(GTK_OBJECT(bd->entry_description_entry), "changed",
                     GTK_SIGNAL_FUNC(entry_description_entry_changed), bd);

  bd->entry_type_menu = gtk_object_get_data(bdo, "entry_type_menu");
  menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(bd->entry_type_menu));
  gtk_container_forall(GTK_CONTAINER(menu), connect_entry_type_menu_item, bd);

  bd->entry_frame = gtk_object_get_data(bdo, "entry_frame");

  bd->subentry_description_entry =
    gtk_object_get_data(bdo, "subentry_description_entry");
  gtk_signal_connect(GTK_OBJECT(bd->subentry_description_entry), "changed",
                     GTK_SIGNAL_FUNC(subentry_description_entry_changed), bd);

  bd->subentry_amount_entry =
    gtk_object_get_data(bdo, "subentry_amount_entry");
  gtk_signal_connect(GTK_OBJECT(bd->subentry_amount_entry), "changed",
                     GTK_SIGNAL_FUNC(subentry_amount_entry_changed), bd);
  gtk_signal_connect(GTK_OBJECT(bd->subentry_amount_entry), "focus-out-event",
                     GTK_SIGNAL_FUNC(subentry_amount_entry_focus_out), bd);

  bd->subentry_mechanism_menu =
    gtk_object_get_data(bdo, "subentry_mechanism_menu");
  menu = bd->subentry_mechanism_menu;
  gnc_option_menu_init(menu);
  menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(menu));
  gtk_container_forall(GTK_CONTAINER(menu), connect_mechanism_menu_item, bd);

  bd->entry_frame = gtk_object_get_data(bdo, "entry_frame");

  button = gtk_object_get_data(bdo, "entry_up_button");
  arrow = gtk_arrow_new(GTK_ARROW_UP, GTK_SHADOW_IN);
  gtk_container_remove(GTK_CONTAINER(button), GTK_BIN(button)->child);
  gtk_container_add(GTK_CONTAINER(button), arrow);
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(entry_up_button_clicked), bd);

  button = gtk_object_get_data(bdo, "entry_down_button");
  arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_IN);
  gtk_container_remove(GTK_CONTAINER(button), GTK_BIN(button)->child);
  gtk_container_add(GTK_CONTAINER(button), arrow);

  box = gtk_object_get_data(bdo, "period_box");
  bd->subentry_period_delta = gnc_date_delta_new(FALSE);
  gtk_box_pack_start(GTK_BOX(box), bd->subentry_period_delta, FALSE, FALSE, 0);
  gtk_signal_connect(GTK_OBJECT(bd->subentry_period_delta), "delta_changed",
                     GTK_SIGNAL_FUNC(subentry_period_delta_changed), bd);
  gtk_widget_show(bd->subentry_period_delta);

  box = gtk_object_get_data(bdo, "grace_box");
  bd->subentry_grace_delta = gnc_date_delta_new(FALSE);
  gtk_box_pack_start(GTK_BOX(box), bd->subentry_grace_delta, FALSE, FALSE, 0);
  gtk_signal_connect(GTK_OBJECT(bd->subentry_grace_delta), "delta_changed",
                     GTK_SIGNAL_FUNC(subentry_grace_delta_changed), bd);
  gtk_widget_show(bd->subentry_grace_delta);

  bd->apply_button = gtk_object_get_data(bdo, "apply_button");
  gtk_widget_set_sensitive(bd->apply_button, FALSE);

  gtk_object_set_data(bdo, "budget_dialog_structure", bd);

  load_budget(bd, budget);

  if (bd->num_entries > 0)
  {
    gtk_clist_select_row(GTK_CLIST(bd->entry_tree), 0, 0);
    gtk_widget_grab_focus(bd->entry_tree);
  }
  else
  {
    allow_edits(bd, FALSE);
    button = gtk_object_get_data(bdo, "add_entry_button");
    gtk_widget_grab_focus(button);
  }

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
