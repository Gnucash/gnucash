/********************************************************************\
 * reconcile-list.c -- A list of accounts to be reconciled for      *
 *                     GnuCash.                                     *
 * Copyright (C) 1998,1999 Jeremy Collins	                    *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-date.h"
#include "QueryCore.h"
#include "QueryNew.h"
#include "Transaction.h"
#include "gnc-ui-util.h"
#include "gnc-gconf-utils.h"
#include "reconcile-list.h"
#include "search-param.h"
#include "gnc-component-manager.h"

/* Signal codes */
enum
{
  TOGGLE_RECONCILED,
  DOUBLE_CLICK_SPLIT,
  LAST_SIGNAL
};


/** Static Globals ****************************************************/
static GNCQueryListClass *parent_class = NULL;
static guint reconcile_list_signals[LAST_SIGNAL] = {0};


/** Static function declarations **************************************/
static void gnc_reconcile_list_init(GNCReconcileList *list);
static void gnc_reconcile_list_class_init(GNCReconcileListClass *klass);
static void gnc_reconcile_list_finalize (GObject *object);
static gpointer gnc_reconcile_list_is_reconciled(gpointer item, gpointer user_data);
static void gnc_reconcile_list_line_toggled (GNCQueryList *list, gpointer item,
					     gpointer user_data);
static void gnc_reconcile_list_double_click_entry (GNCQueryList *list,
						   gpointer item,
						   gpointer user_data);


GType
gnc_reconcile_list_get_type (void)
{
  static GType gnc_reconcile_list_type = 0;

  if (gnc_reconcile_list_type == 0)
  {
    static const GTypeInfo gnc_reconcile_list_info = {
      sizeof (GNCReconcileListClass),
      NULL,
      NULL,
      (GClassInitFunc) gnc_reconcile_list_class_init,
      NULL,
      NULL,
      sizeof (GNCReconcileList),
      0,
      (GInstanceInitFunc) gnc_reconcile_list_init
    };

    gnc_reconcile_list_type = g_type_register_static (GNC_TYPE_QUERY_LIST,
						      "GncReconcileList",
						      &gnc_reconcile_list_info, 0);
  }

  return gnc_reconcile_list_type;
}


/********************************************************************\
 * gnc_reconcile_list_new                                           *
 *   creates the account tree                                       *
 *                                                                  *
 * Args: account - the account to use in filling up the splits.     *
 *       type    - the type of list, RECLIST_DEBIT or RECLIST_CREDIT*
 * Returns: the account tree widget, or NULL if there was a problem.*
\********************************************************************/
static void
gnc_reconcile_list_construct(GNCReconcileList *list, Query *query)
{
  GNCQueryList *qlist = GNC_QUERY_LIST (list);
  gboolean inv_sort = FALSE;

  if (list->list_type == RECLIST_CREDIT)
    inv_sort = TRUE;

  gnc_query_list_construct (qlist, list->column_list, query);
  gnc_query_list_set_numerics (qlist, TRUE, inv_sort);

  /* Now set up the signals for the QueryList */
  g_signal_connect (G_OBJECT (qlist), "line_toggled",
		    G_CALLBACK(gnc_reconcile_list_line_toggled), list);
  g_signal_connect (G_OBJECT (qlist), "double_click_entry",
		    G_CALLBACK(gnc_reconcile_list_double_click_entry), list);
}

GtkWidget *
gnc_reconcile_list_new(Account *account, GNCReconcileListType type,
                       time_t statement_date)
{
  GNCReconcileList *list;
  gboolean include_children, auto_check;
  GList *accounts = NULL;
  GList *splits;
  Query *query;

  g_return_val_if_fail(account, NULL);
  g_return_val_if_fail((type == RECLIST_DEBIT) ||
                       (type == RECLIST_CREDIT), NULL);

  list = g_object_new (GNC_TYPE_RECONCILE_LIST,
		       "n-columns", 5,
		       NULL);

  list->account = account;
  list->list_type = type;
  list->statement_date = statement_date;

  query = xaccMallocQuery();
  xaccQuerySetBook(query, gnc_get_current_book ());

  include_children = xaccAccountGetReconcileChildrenStatus(account);
  if (include_children)
    accounts = gnc_account_get_descendants(account);

  /* match the account */
  accounts = g_list_prepend (accounts, account);

  xaccQueryAddAccountMatch (query, accounts, GUID_MATCH_ANY, QUERY_AND);

  g_list_free (accounts);

  /* limit the matches to CREDITs and DEBITs only, depending on the type */
  if (type == RECLIST_CREDIT)
    xaccQueryAddValueMatch(query, gnc_numeric_zero (),
			   NUMERIC_MATCH_CREDIT,
			   COMPARE_GTE, QUERY_AND);
  else
    xaccQueryAddValueMatch(query, gnc_numeric_zero (),
			   NUMERIC_MATCH_DEBIT,
			   COMPARE_GTE, QUERY_AND);

  /* limit the matches only to Cleared and Non-reconciled splits */
  xaccQueryAddClearedMatch(query, CLEARED_NO | CLEARED_CLEARED, QUERY_AND);

  /* initialize the QueryList */
  gnc_reconcile_list_construct (list, query);

  /* find the list of splits to auto-reconcile */
  auto_check = gnc_gconf_get_bool(GCONF_RECONCILE_SECTION,
				  "check_cleared", NULL);

  if (auto_check) {
    for (splits = xaccQueryGetSplits(query); splits; splits = splits->next) {
      Split *split = splits->data;
      char recn = xaccSplitGetReconcile(split);
      time_t trans_date = xaccTransGetDate(xaccSplitGetParent(split));
    
      /* Just an extra verification that our query is correct ;) */
      g_assert (recn == NREC || recn == CREC);

      if (recn == CREC &&
          difftime(trans_date, statement_date) <= 0)
	g_hash_table_insert (list->reconciled, split, split);
    }
  }

  /* Free the query -- we don't need it anymore */
  xaccFreeQuery(query);

  return GTK_WIDGET(list);
}

static void
gnc_reconcile_list_init (GNCReconcileList *list)
{
  GNCSearchParam *param;
  GList *columns = NULL;

  list->reconciled = g_hash_table_new (NULL, NULL);
  list->account = NULL;
  list->sibling = NULL;

  param = gnc_search_param_new();
  gnc_search_param_set_param_fcn (param, QUERYCORE_BOOLEAN,
				  gnc_reconcile_list_is_reconciled, list);
  gnc_search_param_set_title (param, _("Reconciled:R")+11);
  gnc_search_param_set_justify (param, GTK_JUSTIFY_CENTER);
  gnc_search_param_set_passive (param, TRUE);
  gnc_search_param_set_non_resizeable (param, TRUE);
  columns = g_list_prepend (columns, param);
  columns = gnc_search_param_prepend_with_justify (columns, _("Amount"),
						   GTK_JUSTIFY_RIGHT,
						   NULL, GNC_ID_SPLIT,
						   SPLIT_AMOUNT, NULL);
  columns = gnc_search_param_prepend (columns, _("Description"), NULL,
				      GNC_ID_SPLIT, SPLIT_TRANS,
				      TRANS_DESCRIPTION, NULL);
  columns = gnc_search_param_prepend_with_justify (columns, _("Num"),
						   GTK_JUSTIFY_CENTER,
						   NULL, GNC_ID_SPLIT,
						   SPLIT_TRANS, TRANS_NUM, NULL);
  columns = gnc_search_param_prepend (columns, _("Date"), NULL, GNC_ID_SPLIT,
				      SPLIT_TRANS, TRANS_DATE_POSTED, NULL);

  list->column_list = columns;
}


static void
gnc_reconcile_list_class_init (GNCReconcileListClass *klass)
{
  GObjectClass    *object_class;
  GtkCListClass   *clist_class;

  object_class =  G_OBJECT_CLASS (klass);
  clist_class = GTK_CLIST_CLASS (klass);

  parent_class = g_type_class_peek_parent (klass);

  reconcile_list_signals[TOGGLE_RECONCILED] =
    g_signal_new("toggle_reconciled",
		 G_OBJECT_CLASS_TYPE (object_class),
		 G_SIGNAL_RUN_FIRST,
		 G_STRUCT_OFFSET (GNCReconcileListClass,
				  toggle_reconciled),
		 NULL, NULL,
		 g_cclosure_marshal_VOID__POINTER,
		 G_TYPE_NONE, 1,
		 G_TYPE_POINTER);

  reconcile_list_signals[DOUBLE_CLICK_SPLIT] =
    g_signal_new("double_click_split",
		 G_OBJECT_CLASS_TYPE (object_class),
		 G_SIGNAL_RUN_FIRST,
		 G_STRUCT_OFFSET (GNCReconcileListClass,
				  double_click_split),
		 NULL, NULL,
		 g_cclosure_marshal_VOID__POINTER,
		 G_TYPE_NONE, 1,
		 G_TYPE_POINTER);

  object_class->finalize = gnc_reconcile_list_finalize;

  klass->toggle_reconciled = NULL;
  klass->double_click_split = NULL;
}

static void
gnc_reconcile_list_toggle_split(GNCReconcileList *list, Split *split)
{
  Split *current;

  g_return_if_fail (GNC_IS_RECONCILE_LIST(list));
  g_return_if_fail (list->reconciled != NULL);

  current = g_hash_table_lookup (list->reconciled, split);

  if (current == NULL)
    g_hash_table_insert (list->reconciled, split, split);
  else
    g_hash_table_remove (list->reconciled, split);

  gnc_query_list_refresh_item (GNC_QUERY_LIST(list), split);
}

static void
gnc_reconcile_list_toggle_children(Account *account, GNCReconcileList *list, Split *split)
{
  GList *child_accounts, *node;
  Transaction *transaction;

  /*
   * Need to get all splits in this transaction and identify any that are
   * in the same heirarchy as the account being reconciled (not necessarily
   * the account this split is from.)
   *
   * For each of these splits toggle them all to the same state.
   */
  child_accounts = gnc_account_get_descendants(account);
  child_accounts = g_list_prepend(child_accounts, account);
  transaction = xaccSplitGetParent(split);
  for(node = xaccTransGetSplitList(transaction); node; node = node->next)
  {
    Split *other_split;
    Account *other_account;
    GNCReconcileList *current_list;

    other_split = node->data;
    other_account = xaccSplitGetAccount(other_split);
    if(other_split == split)
      continue;
    /* Check this 'other' account in in the same heirarchy */
    if(!g_list_find(child_accounts,other_account))
      continue;
    /* Search our sibling list for this split first.  We search the 
     * sibling list first because that it where it is most likely to be.
     */
    current_list = list->sibling;
    if (!gnc_query_list_item_in_list (GNC_QUERY_LIST(current_list), other_split)) {
      /* Not in the sibling list, try this list */
      current_list = list;
      if (!gnc_query_list_item_in_list (GNC_QUERY_LIST(current_list), other_split))
	/* We can't find it, nothing more I can do about it */
	continue;
    }
    gnc_reconcile_list_toggle_split(current_list, other_split);
  }
  g_list_free(child_accounts);
}

static void
gnc_reconcile_list_toggle (GNCReconcileList *list, Split *split)
{
  gboolean include_children;

  g_return_if_fail (GNC_IS_RECONCILE_LIST(list));
  g_return_if_fail (list->reconciled != NULL);

  gnc_reconcile_list_toggle_split(list, split);

  include_children = xaccAccountGetReconcileChildrenStatus(list->account);
  if(include_children)
    gnc_reconcile_list_toggle_children(list->account, list, split);

  g_signal_emit (G_OBJECT (list),
                 reconcile_list_signals[TOGGLE_RECONCILED], 0, split);
}

static void
gnc_reconcile_list_line_toggled (GNCQueryList *list, gpointer item,
				 gpointer user_data)
{
  GNCReconcileList *rlist = user_data;

  g_return_if_fail(item);
  g_return_if_fail(user_data);
  g_return_if_fail(GNC_IS_RECONCILE_LIST(rlist));

  gnc_reconcile_list_toggle (rlist, item);
}

static void gnc_reconcile_list_double_click_entry (GNCQueryList *list,
						   gpointer item,
						   gpointer user_data)
{
  GNCReconcileList *rlist = user_data;

  g_return_if_fail(item);
  g_return_if_fail(user_data);
  g_return_if_fail(GNC_IS_RECONCILE_LIST(rlist));

  g_signal_emit(G_OBJECT(rlist),
		reconcile_list_signals[DOUBLE_CLICK_SPLIT], 0, item);
}

static void
gnc_reconcile_list_finalize (GObject *object)
{
  GNCReconcileList *list = GNC_RECONCILE_LIST(object);

  if (list->reconciled != NULL) {
    g_hash_table_destroy(list->reconciled);
    list->reconciled = NULL;
  }

  G_OBJECT_CLASS (parent_class)->finalize (object);
}

gint
gnc_reconcile_list_get_needed_height (GNCReconcileList *list, gint num_rows)
{
  g_return_val_if_fail (list != NULL, 0);
  g_return_val_if_fail (GNC_IS_RECONCILE_LIST(list), 0);

  if (!GTK_WIDGET_REALIZED (list))
    return 0;

  return gnc_query_list_get_needed_height (GNC_QUERY_LIST(list), num_rows);
}

gint
gnc_reconcile_list_get_num_splits (GNCReconcileList *list)
{
  g_return_val_if_fail (list != NULL, 0);
  g_return_val_if_fail (GNC_IS_RECONCILE_LIST(list), 0);

  return gnc_query_list_get_num_entries(GNC_QUERY_LIST(list));
}

Split *
gnc_reconcile_list_get_current_split (GNCReconcileList *list)
{
  g_return_val_if_fail (list != NULL, NULL);
  g_return_val_if_fail (GNC_IS_RECONCILE_LIST(list), NULL);

  return gnc_query_list_get_current_entry(GNC_QUERY_LIST(list));
}

/********************************************************************\
 * gnc_reconcile_list_is_reconciled                                 *
 *   Is the item a reconciled split?                                *
 *                                                                  *
 * Args: item - the split to be checked                             *
 *       user_data - a pointer to the GNCReconcileList              *
 * Returns: whether the split is to be reconciled.                  *
\********************************************************************/
static gpointer
gnc_reconcile_list_is_reconciled(gpointer item, gpointer user_data)
{
  GNCReconcileList *list = user_data;
  Split *current;

  g_return_val_if_fail(item, NULL);
  g_return_val_if_fail(list, NULL);
  g_return_val_if_fail(GNC_IS_RECONCILE_LIST(list), NULL);

  if (!list->reconciled)
    return NULL;

  current = g_hash_table_lookup(list->reconciled, item);
  return GINT_TO_POINTER(current != NULL);
}

static void
grl_refresh_helper (gpointer key, gpointer value, gpointer user_data)
{
  GNCReconcileList *list = user_data;
  GNCQueryList *qlist = GNC_QUERY_LIST(list);

  if (!gnc_query_list_item_in_list(qlist, key))
    g_hash_table_remove(list->reconciled, key);
}

/********************************************************************\
 * gnc_reconcile_list_refresh                                       *
 *   refreshes the list                                             *
 *                                                                  *
 * Args: list - list to refresh                                     *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_reconcile_list_refresh (GNCReconcileList *list)
{
  GNCQueryList *qlist;

  g_return_if_fail (list != NULL);
  g_return_if_fail (GNC_IS_RECONCILE_LIST(list));

  qlist = GNC_QUERY_LIST(list);
  gnc_query_list_refresh(qlist);

  /* Now verify that everything in the reconcile hash is still in qlist */
  if (list->reconciled)
    g_hash_table_foreach(list->reconciled, grl_refresh_helper, list);
}


/********************************************************************\
 * gnc_reconcile_list_reconciled_balance                            *
 *   returns the reconciled balance of the list                     *
 *                                                                  *
 * Args: list - list to get reconciled balance of                   *
 * Returns: reconciled balance (gnc_numeric)                        *
\********************************************************************/
static void
grl_balance_hash_helper (gpointer key, gpointer value, gpointer user_data)
{
  Split *split = key;
  gnc_numeric *total = user_data;

  *total = gnc_numeric_add_fixed (*total, xaccSplitGetAmount (split));
}

gnc_numeric
gnc_reconcile_list_reconciled_balance (GNCReconcileList *list)
{
  gnc_numeric total = gnc_numeric_zero ();

  g_return_val_if_fail (list != NULL, total);
  g_return_val_if_fail (GNC_IS_RECONCILE_LIST(list), total);

  if (list->reconciled == NULL)
    return total;

  g_hash_table_foreach (list->reconciled, grl_balance_hash_helper, &total);

  return gnc_numeric_abs (total);
}


/********************************************************************\
 * gnc_reconcile_list_commit                                        *
 *   Commit the reconcile information in the list. only change the  *
 *   state of those items marked as reconciled.  All others should  *
 *   retain their previous state (none, cleared, voided, etc.).     *
 *                                                                  *
 * Args: list - list to commit                                      *
 *       date - date to set as the reconcile date                   *
 * Returns: nothing                                                 *
\********************************************************************/
static void
grl_commit_hash_helper (gpointer key, gpointer value, gpointer user_data)
{
  Split *split = key;
  time_t *date = user_data;

  xaccSplitSetReconcile (split, YREC);
  xaccSplitSetDateReconciledSecs (split, *date);
}

void
gnc_reconcile_list_commit (GNCReconcileList *list, time_t date)
{
  g_return_if_fail (list != NULL);
  g_return_if_fail (GNC_IS_RECONCILE_LIST(list));

  if (list->reconciled == NULL)
    return;

  gnc_suspend_gui_refresh();
  g_hash_table_foreach (list->reconciled, grl_commit_hash_helper, &date);
  gnc_resume_gui_refresh();
}


/********************************************************************\
 * gnc_reconcile_list_postpone                                      *
 *   postpone the reconcile information in the list by setting      *
 *   reconciled splits to cleared status                            *
 *                                                                  *
 * Args: list - list to commit                                      *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_reconcile_list_postpone (GNCReconcileList *list)
{
  GtkCList *clist = GTK_CLIST(list); /* This is cheating! */
  Split *split;
  int num_splits;
  int i;

  g_return_if_fail(list != NULL);
  g_return_if_fail(GNC_IS_RECONCILE_LIST(list));

  if (list->reconciled == NULL)
    return;

  num_splits = gnc_query_list_get_num_entries(GNC_QUERY_LIST(list));
  gnc_suspend_gui_refresh();
  for (i = 0; i < num_splits; i++)
  {
    char recn;

    split = gtk_clist_get_row_data (clist, i);

    // Don't change splits past reconciliation date that haven't been
    // set to be reconciled
    if ( difftime(list->statement_date,
		  xaccTransGetDate(xaccSplitGetParent(split))) >= 0 ||
	 g_hash_table_lookup(list->reconciled, split))
    {
      recn = g_hash_table_lookup (list->reconciled, split) ? CREC : NREC;

      xaccSplitSetReconcile (split, recn);
    }
  }
  gnc_resume_gui_refresh();
}


/********************************************************************\
 * gnc_reconcile_list_unselect_all                                  *
 *   unselect all splits in the list                                *
 *                                                                  *
 * Args: list - list to unselect all                                *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_reconcile_list_unselect_all(GNCReconcileList *list)
{
  g_return_if_fail (list != NULL);
  g_return_if_fail (GNC_IS_RECONCILE_LIST(list));

  gnc_query_list_unselect_all (GNC_QUERY_LIST(list));
}


/********************************************************************\
 * gnc_reconcile_list_changed                                       *
 *   returns true if any splits have been reconciled                *
 *                                                                  *
 * Args: list - list to get changed status for                      *
 * Returns: true if any reconciled splits                           *
\********************************************************************/
gboolean
gnc_reconcile_list_changed (GNCReconcileList *list)
{
  g_return_val_if_fail (list != NULL, FALSE);
  g_return_val_if_fail (GNC_IS_RECONCILE_LIST(list), FALSE);

  return g_hash_table_size (list->reconciled) != 0;
}

#if 0
static void
gnc_reconcile_list_fill(GNCReconcileList *list)
{
  const gchar *strings[list->num_columns + 1];
  GNCPrintAmountInfo print_info;
  Transaction *trans;
  gboolean auto_check;
  GList *splits;
  Split *split;

  auto_check = gnc_gconf_get_bool(GCONF_RECONCILE_SECTION,
				  "check_cleared", NULL);

  strings[5] = NULL;

  print_info = gnc_account_print_info (list->account, FALSE);

  for (splits = xaccQueryGetSplits (list->query); splits; splits=splits->next)
  {
    gnc_numeric amount;
    Timespec ts;
    const gchar *trans_num;
    char recn;
    int row, len;

    split = splits->data;
    trans = xaccSplitGetParent (split);
    trans_num = xaccTransGetNum (trans);

    recn = xaccSplitGetReconcile(split);
    if ((recn != NREC) && (recn != CREC))
      continue;

    amount = xaccSplitGetAmount (split);

    if (gnc_numeric_negative_p (amount)) {
      if (list->list_type == RECLIST_DEBIT) {
	continue;
      }
    } else if (gnc_numeric_positive_p (amount)) {
      if (list->list_type == RECLIST_CREDIT) {
	continue;
      }
    } else {
      len = trans_num ? strlen(trans_num) : 0;
      if ((len  && (list->list_type == RECLIST_DEBIT)) ||
	  (!len && (list->list_type == RECLIST_CREDIT)))
	continue;
    }

    xaccTransGetDatePostedTS (trans, &ts);

    strings[0] = gnc_print_date (ts);
    strings[1] = trans_num;
    strings[2] = xaccTransGetDescription (trans);
    strings[3] = xaccPrintAmount (gnc_numeric_abs (amount), print_info);
    strings[4] = "";

    if (list->first_fill && auto_check && recn == CREC)
      g_hash_table_insert (list->reconciled, split, split);

    row = gtk_clist_append (GTK_CLIST(list), (gchar **) strings);
    gtk_clist_set_row_data (GTK_CLIST(list), row, split);

    update_toggle (GTK_CLIST (list), row);

    list->num_splits++;
  }

  list->first_fill = FALSE;
}
#endif
