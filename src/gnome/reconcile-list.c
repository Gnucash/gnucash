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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gnome.h>

#include "FileDialog.h"
#include "date.h"
#include "dialog-utils.h"
#include "gnc-ui-util.h"
#include "messages.h"
#include "reconcile-list.h"


/* Signal codes */
enum
{
  TOGGLE_RECONCILED,
  DOUBLE_CLICK_SPLIT,
  LAST_SIGNAL
};


/** Static Globals ****************************************************/
static GtkCListClass *parent_class = NULL;
static guint reconcile_list_signals[LAST_SIGNAL] = {0};


/** Static function declarations **************************************/
static void gnc_reconcile_list_init(GNCReconcileList *list);
static void gnc_reconcile_list_class_init(GNCReconcileListClass *klass);
static void gnc_reconcile_list_select_row(GtkCList *clist, gint row,
					  gint column, GdkEvent *event);
static void gnc_reconcile_list_unselect_row(GtkCList *clist, gint row,
					    gint column, GdkEvent *event);
static void gnc_reconcile_list_destroy(GtkObject *object);
static void gnc_reconcile_list_fill(GNCReconcileList *list);


GtkType
gnc_reconcile_list_get_type()
{
  static GtkType gnc_reconcile_list_type = 0;

  if (!gnc_reconcile_list_type)
  {
    static const GtkTypeInfo gnc_reconcile_list_info =
    {
      "GNCReconcileList",
      sizeof (GNCReconcileList),
      sizeof (GNCReconcileListClass),
      (GtkClassInitFunc) gnc_reconcile_list_class_init,
      (GtkObjectInitFunc) gnc_reconcile_list_init,
      /* reserved_1 */ NULL,
      /* reserved_2 */ NULL,
      (GtkClassInitFunc) NULL
    };

    gnc_reconcile_list_type = gtk_type_unique(GTK_TYPE_CLIST,
					      &gnc_reconcile_list_info);
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
GtkWidget *
gnc_reconcile_list_new(Account *account, GNCReconcileListType type)
{
  GNCReconcileList *list;

  g_assert(account != NULL);
  g_assert((type == RECLIST_DEBIT) || (type == RECLIST_CREDIT));

  list = GNC_RECONCILE_LIST(gtk_type_new(gnc_reconcile_list_get_type()));

  list->account = account;
  list->list_type = type;

  list->query = xaccMallocQuery();

  xaccQuerySetGroup(list->query, gncGetCurrentGroup());

  /* match the account */
  xaccQueryAddSingleAccountMatch(list->query, account, QUERY_OR);

  if (type == RECLIST_CREDIT)
    DxaccQueryAddAmountMatch(list->query, 0.0, AMT_SGN_MATCH_CREDIT,
                             AMT_MATCH_ATLEAST, QUERY_AND);
  else
    DxaccQueryAddAmountMatch(list->query, 0.0, AMT_SGN_MATCH_DEBIT,
                             AMT_MATCH_ATLEAST, QUERY_AND);

  return GTK_WIDGET(list);
}

static void
gnc_reconcile_list_init (GNCReconcileList *list)
{
  GtkCList *clist = GTK_CLIST (list);
  GtkStyle *style;
  gint font_height;
  gchar * titles[] =
    {
      _("Date"),
      _("Num"),
      _("Description"),
      _("Amount"),
      _("Reconciled:R") + 11,
      NULL
    };

  list->num_splits = 0;
  list->num_columns = 0;
  list->reconciled = g_hash_table_new (NULL, NULL);
  list->current_row = -1;
  list->current_split = NULL;
  list->no_toggle = FALSE;
  list->always_unselect = FALSE;
  list->first_fill = TRUE;
  list->query = NULL;

  while (titles[list->num_columns] != NULL)
    list->num_columns++;

  gtk_clist_construct (clist, list->num_columns, titles);
  gtk_clist_set_shadow_type (clist, GTK_SHADOW_IN);
  gtk_clist_set_column_justification (clist, 3, GTK_JUSTIFY_RIGHT);
  gtk_clist_set_column_justification (clist, 4, GTK_JUSTIFY_CENTER);
  gtk_clist_column_titles_passive (clist);

  style = gtk_widget_get_style (GTK_WIDGET(list));

  {
    GdkFont *font = NULL;
    gint width;
    gint i;

    font = style->font;
    if (font != NULL)
    {
      for (i = 0; i < list->num_columns; i++)
      {
	width = gdk_string_width (font, titles[i]);
	gtk_clist_set_column_min_width (GTK_CLIST(list), i, width + 5);
	if (i == 4)
	  gtk_clist_set_column_max_width (GTK_CLIST(list), i, width + 5);
      }

      font_height = font->ascent + font->descent;
    }
    else
      font_height = 0;
  }

  list->check_size = (font_height > 0) ? font_height - 3 : 9;

  list->mask = NULL;
  list->off_pixmap = NULL;
  list->on_pixmap = NULL;
}

static void
set_toggle (GNCReconcileList *list, gint row)
{
  gboolean reconciled;
  GdkPixmap *pixmap;
  Split *split;

  if (!GTK_WIDGET_REALIZED (GTK_WIDGET (list)))
    return;

  split = gtk_clist_get_row_data (GTK_CLIST (list), row);
  reconciled = g_hash_table_lookup (list->reconciled, split) != NULL;

  pixmap = reconciled ? list->on_pixmap : list->off_pixmap;

  gtk_clist_set_pixmap (GTK_CLIST (list), row, 4, pixmap, list->mask);
}

static void
widget_realize (GtkWidget *widget)
{
  GNCReconcileList *list = GNC_RECONCILE_LIST (widget);
  GdkGCValues gc_values;
  GtkStyle *style;
  GdkGC *gc;
  gint i;

  if (GTK_WIDGET_CLASS (parent_class)->realize)
    (GTK_WIDGET_CLASS (parent_class)->realize) (widget);

  if (list->mask)
    return;

  style = gtk_widget_get_style (widget);

  list->mask = gdk_pixmap_new (NULL, list->check_size, list->check_size, 1);
  list->on_pixmap = gdk_pixmap_new (widget->window,
                                    list->check_size, list->check_size, -1);
  list->off_pixmap = gdk_pixmap_new (widget->window,
                                     list->check_size, list->check_size, -1);

  gc_values.foreground = style->white;
  gc = gtk_gc_get (1, gtk_widget_get_colormap (widget),
                   &gc_values, GDK_GC_FOREGROUND);

  gdk_draw_rectangle (list->mask, gc, TRUE, 0, 0,
                      list->check_size, list->check_size);

  gtk_gc_release (gc);

  gc = style->base_gc[GTK_STATE_NORMAL];

  gdk_draw_rectangle (list->on_pixmap, gc, TRUE, 0, 0,
                      list->check_size, list->check_size);
  gdk_draw_rectangle (list->off_pixmap, gc, TRUE, 0, 0,
                      list->check_size, list->check_size);

  gc = style->text_gc[GTK_STATE_NORMAL];

  gdk_draw_line (list->on_pixmap, gc,
                 1, list->check_size / 2,
                 list->check_size / 3, list->check_size - 5);
  gdk_draw_line (list->on_pixmap, gc,
                 1, list->check_size / 2 + 1,
                 list->check_size / 3, list->check_size - 4);
        
  gdk_draw_line (list->on_pixmap, gc,
                 list->check_size / 3, list->check_size - 5,
                 list->check_size - 3, 2);
  gdk_draw_line (list->on_pixmap, gc,
                 list->check_size / 3, list->check_size - 4,
                 list->check_size - 3, 1);

  for (i = 0; i < GTK_CLIST (list)->rows; i++)
    set_toggle (list, i);
}

static void
widget_unrealize (GtkWidget *widget)
{
  GNCReconcileList *list = GNC_RECONCILE_LIST (widget);

  if (list->mask)
    gdk_bitmap_unref (list->mask);

  if (list->on_pixmap)
    gdk_pixmap_unref (list->on_pixmap);

  if (list->off_pixmap)
    gdk_pixmap_unref (list->off_pixmap);

  list->mask = NULL;
  list->on_pixmap = NULL;
  list->off_pixmap = NULL;

  if (GTK_WIDGET_CLASS (parent_class)->unrealize)
    (GTK_WIDGET_CLASS (parent_class)->unrealize) (widget);
}

static void
gnc_reconcile_list_class_init (GNCReconcileListClass *klass)
{
  GtkObjectClass    *object_class;
  GtkWidgetClass    *widget_class;
  GtkContainerClass *container_class;
  GtkCListClass     *clist_class;

  object_class =    (GtkObjectClass*) klass;
  widget_class =    (GtkWidgetClass*) klass;
  container_class = (GtkContainerClass*) klass;
  clist_class =     (GtkCListClass*) klass;

  parent_class = gtk_type_class(GTK_TYPE_CLIST);

  reconcile_list_signals[TOGGLE_RECONCILED] =
    gtk_signal_new("toggle_reconciled",
		   GTK_RUN_FIRST,
		   object_class->type,
		   GTK_SIGNAL_OFFSET(GNCReconcileListClass,
				     toggle_reconciled),
		   gtk_marshal_NONE__POINTER,
		   GTK_TYPE_NONE, 1,
		   GTK_TYPE_POINTER);

  reconcile_list_signals[DOUBLE_CLICK_SPLIT] =
    gtk_signal_new("double_click_split",
		   GTK_RUN_FIRST,
		   object_class->type,
		   GTK_SIGNAL_OFFSET(GNCReconcileListClass,
				     double_click_split),
		   gtk_marshal_NONE__POINTER,
		   GTK_TYPE_NONE, 1,
		   GTK_TYPE_POINTER);

  gtk_object_class_add_signals(object_class,
			       reconcile_list_signals,
			       LAST_SIGNAL);

  object_class->destroy = gnc_reconcile_list_destroy;

  widget_class->realize = widget_realize;
  widget_class->unrealize = widget_unrealize;

  clist_class->select_row = gnc_reconcile_list_select_row;
  clist_class->unselect_row = gnc_reconcile_list_unselect_row;

  klass->toggle_reconciled = NULL;
  klass->double_click_split = NULL;
}

static void
gnc_reconcile_list_toggle (GNCReconcileList *list)
{
  Split *split, *current;
  gint row;

  g_assert (IS_GNC_RECONCILE_LIST(list));
  g_assert (list->reconciled != NULL);

  if (list->no_toggle)
    return;

  row = list->current_row;
  split = gtk_clist_get_row_data (GTK_CLIST(list), row);
  current = g_hash_table_lookup (list->reconciled, split);

  list->current_split = split;

  if (current == NULL)
    g_hash_table_insert (list->reconciled, split, split);
  else
    g_hash_table_remove (list->reconciled, split);

  set_toggle (list, row);

  gtk_signal_emit (GTK_OBJECT (list),
                   reconcile_list_signals[TOGGLE_RECONCILED], split);
}

static void
gnc_reconcile_list_select_row (GtkCList *clist, gint row, gint column,
			       GdkEvent *event)
{
  GNCReconcileList *list = GNC_RECONCILE_LIST(clist);

  list->current_row = row;

  gnc_reconcile_list_toggle (list);

  GTK_CLIST_CLASS(parent_class)->select_row (clist, row, column, event);

  if (event && (event->type == GDK_2BUTTON_PRESS))
  {
    Split *split;

    split = gtk_clist_get_row_data (clist, row);

    gtk_signal_emit(GTK_OBJECT(list),
                    reconcile_list_signals[DOUBLE_CLICK_SPLIT], split);
  }
}

static void
gnc_reconcile_list_unselect_row (GtkCList *clist, gint row, gint column,
                                 GdkEvent *event)
{
  GNCReconcileList *list = GNC_RECONCILE_LIST(clist);

  if (row == list->current_row)
  {
    gnc_reconcile_list_toggle (list);
    if (!list->always_unselect)
      return;
  }

  GTK_CLIST_CLASS(parent_class)->unselect_row (clist, row, column, event);

  if (event && (event->type == GDK_2BUTTON_PRESS))
  {
    Split *split;

    split = gtk_clist_get_row_data (clist, row);

    gtk_signal_emit (GTK_OBJECT(list),
                     reconcile_list_signals[DOUBLE_CLICK_SPLIT], split);
  }
}

static void
gnc_reconcile_list_destroy (GtkObject *object)
{
  GNCReconcileList *list = GNC_RECONCILE_LIST(object);

  if (list->reconciled != NULL)
  {
    g_hash_table_destroy(list->reconciled);
    list->reconciled = NULL;
  }

  if (list->query != NULL)
  {
    xaccFreeQuery(list->query);
    list->query = NULL;
  }

  if (GTK_OBJECT_CLASS(parent_class)->destroy)
    GTK_OBJECT_CLASS(parent_class)->destroy (object);
}

gint
gnc_reconcile_list_get_needed_height (GNCReconcileList *list, gint num_rows)
{
  GtkCList *clist;
  gint list_height;
  gint title_height;

  g_return_val_if_fail (list != NULL, 0);
  g_return_val_if_fail (IS_GNC_RECONCILE_LIST(list), 0);

  if (!GTK_WIDGET_REALIZED (list))
    return 0;

  clist = GTK_CLIST (list);

  /* sync with gtkclist.c */
  title_height = (clist->column_title_area.height +
                  (GTK_WIDGET(list)->style->klass->ythickness +
                   GTK_CONTAINER(list)->border_width) * 2);
  list_height = (clist->row_height * num_rows) + (num_rows + 1);

  return title_height + list_height;
}

gint
gnc_reconcile_list_get_num_splits (GNCReconcileList *list)
{
  g_return_val_if_fail (list != NULL, 0);
  g_return_val_if_fail (IS_GNC_RECONCILE_LIST(list), 0);

  return list->num_splits;
}

Split *
gnc_reconcile_list_get_current_split (GNCReconcileList *list)
{
  g_return_val_if_fail (list != NULL, NULL);
  g_return_val_if_fail (IS_GNC_RECONCILE_LIST(list), NULL);

  return list->current_split;
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
  GtkCList *clist = GTK_CLIST(list);
  GtkAdjustment *adjustment;
  gfloat save_value = 0.0;
  Split *old_split;
  gint new_row;

  g_return_if_fail (list != NULL);
  g_return_if_fail (IS_GNC_RECONCILE_LIST(list));

  adjustment = gtk_clist_get_vadjustment (GTK_CLIST(list));
  if (adjustment != NULL)
    save_value = adjustment->value;

  gtk_clist_freeze (clist);

  gtk_clist_clear (clist);

  old_split = list->current_split;
  list->num_splits = 0;
  list->current_row = -1;
  list->current_split = NULL;

  gnc_reconcile_list_fill (list);

  gtk_clist_columns_autosize (clist);

  if (adjustment != NULL)
  {
    save_value = CLAMP (save_value, adjustment->lower, adjustment->upper);
    gtk_adjustment_set_value (adjustment, save_value);
  }

  if (old_split != NULL)
  {
    new_row = gtk_clist_find_row_from_data (clist, old_split);
    if (new_row >= 0)
    {
      list->no_toggle = TRUE;
      gtk_clist_select_row (clist, new_row, 0);
      list->no_toggle = FALSE;
      list->current_split = old_split;
    }
  }

  gtk_clist_thaw (clist);
}


/********************************************************************\
 * gnc_reconcile_list_reconciled_balance                            *
 *   returns the reconciled balance of the list                     *
 *                                                                  *
 * Args: list - list to get reconciled balance of                   *
 * Returns: reconciled balance (gnc_numeric)                        *
\********************************************************************/
gnc_numeric
gnc_reconcile_list_reconciled_balance (GNCReconcileList *list)
{
  GtkCList *clist = GTK_CLIST(list);
  Split *split;
  gnc_numeric total;
  int account_type;
  int i;

  total = gnc_numeric_zero ();

  g_return_val_if_fail (list != NULL, total);
  g_return_val_if_fail (IS_GNC_RECONCILE_LIST(list), total);

  if (list->reconciled == NULL)
    return total;

  account_type = xaccAccountGetType (list->account);

  for (i = 0; i < list->num_splits; i++)
  {
    split = gtk_clist_get_row_data (clist, i);

    if (g_hash_table_lookup (list->reconciled, split) == NULL)
      continue;

    if ((account_type == STOCK) || (account_type == MUTUAL) ||
        (account_type == CURRENCY))
      total = gnc_numeric_add_fixed (total, xaccSplitGetShareAmount(split));
    else
      total = gnc_numeric_add_fixed (total, xaccSplitGetValue(split));
  }

  return gnc_numeric_abs (total);
}


/********************************************************************\
 * gnc_reconcile_list_commit                                        *
 *   commit the reconcile information in the list                   *
 *                                                                  *
 * Args: list - list to commit                                      *
 *       date - date to set as the reconcile date                   *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_reconcile_list_commit (GNCReconcileList *list, time_t date)
{
  GtkCList *clist = GTK_CLIST(list);
  Split *split;
  int i;

  g_return_if_fail (list != NULL);
  g_return_if_fail (IS_GNC_RECONCILE_LIST(list));

  if (list->reconciled == NULL)
    return;

  for (i = 0; i < list->num_splits; i++)
  {
    char recn;

    split = gtk_clist_get_row_data (clist, i);

    recn = g_hash_table_lookup (list->reconciled, split) ? YREC : NREC;

    xaccSplitSetReconcile (split, recn);
    if (recn == YREC)
      xaccSplitSetDateReconciledSecs (split, date);
  }
}


/********************************************************************\
 * gnc_reconcile_list_commit                                        *
 *   postpone the reconcile information in the list by setting      *
 *   reconciled splits to cleared status                            *
 *                                                                  *
 * Args: list - list to commit                                      *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_reconcile_list_postpone (GNCReconcileList *list)
{
  GtkCList *clist = GTK_CLIST(list);
  Split *split;
  int i;

  g_return_if_fail(list != NULL);
  g_return_if_fail(IS_GNC_RECONCILE_LIST(list));

  if (list->reconciled == NULL)
    return;

  for (i = 0; i < list->num_splits; i++)
  {
    char recn;

    split = gtk_clist_get_row_data (clist, i);

    recn = g_hash_table_lookup (list->reconciled, split) ? CREC : NREC;

    xaccSplitSetReconcile (split, recn);
  }
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
  g_return_if_fail (IS_GNC_RECONCILE_LIST(list));

  list->no_toggle = TRUE;
  list->always_unselect = TRUE;

  gtk_clist_unselect_all (GTK_CLIST(list));

  list->always_unselect = FALSE;
  list->no_toggle = FALSE;

  list->current_split = NULL;
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
  g_return_val_if_fail (IS_GNC_RECONCILE_LIST(list), FALSE);

  return g_hash_table_size (list->reconciled) != 0;
}


/********************************************************************\
 * gnc_reconcile_list_set_sort_order                                *
 *   sets the sorting order of splits in the list                   *
 *                                                                  *
 * Args: list - list to change the sort order for                   *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_reconcile_list_set_sort_order (GNCReconcileList *list, sort_type_t key)
{
  g_return_if_fail (list != NULL);
  g_return_if_fail (IS_GNC_RECONCILE_LIST(list));
  g_return_if_fail (list->query != NULL);

  xaccQuerySetSortOrder (list->query, key,
                         (key == BY_STANDARD) ? BY_NONE : BY_STANDARD,
                         BY_NONE);

  if (list->list_type == RECLIST_DEBIT)
    return;

  xaccQuerySetSortIncreasing (list->query, !(key == BY_AMOUNT));
}

static void
gnc_reconcile_list_fill(GNCReconcileList *list)
{
  const gchar *strings[list->num_columns + 1];
  GNCPrintAmountInfo print_info;
  GNCAccountType account_type;
  Transaction *trans;
  GList *splits;
  Split *split;

  account_type = xaccAccountGetType (list->account);
  strings[5] = NULL;

  if ((account_type == STOCK) ||
      (account_type == MUTUAL) ||
      (account_type == CURRENCY))
    print_info = gnc_account_quantity_print_info (list->account, FALSE);
  else
    print_info = gnc_account_value_print_info (list->account, FALSE);

  for (splits = xaccQueryGetSplits (list->query); splits; splits=splits->next)
  {
    gnc_numeric amount;
    Timespec ts;
    char recn;
    int row;

    split = splits->data;

    recn = xaccSplitGetReconcile(split);
    if ((recn != NREC) && (recn != CREC))
      continue;

    if((account_type == STOCK) || (account_type == MUTUAL))
      amount = xaccSplitGetShareAmount (split);
    else
      amount = xaccSplitGetValue (split);

    if (gnc_numeric_negative_p (amount) && list->list_type == RECLIST_DEBIT)
      continue;
    if (!gnc_numeric_negative_p (amount) && list->list_type == RECLIST_CREDIT)
      continue;

    trans = xaccSplitGetParent (split);

    xaccTransGetDateTS (trans, &ts);

    strings[0] = gnc_print_date (ts);
    strings[1] = xaccTransGetNum (trans);
    strings[2] = xaccTransGetDescription (trans);
    strings[3] = xaccPrintAmount (gnc_numeric_abs (amount), print_info);
    strings[4] = "";

    if (list->first_fill && recn == CREC)
      g_hash_table_insert (list->reconciled, split, split);

    row = gtk_clist_append (GTK_CLIST(list), (gchar **) strings);
    gtk_clist_set_row_data (GTK_CLIST(list), row, split);

    set_toggle (list, row);

    list->num_splits++;
  }

  list->first_fill = FALSE;
}
