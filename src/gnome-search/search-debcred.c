/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gnome.h>

#include "gnc-amount-edit.h"
#include "QueryCore.h"

#include "search-debcred.h"

#define d(x)

static GNCSearchCoreType *clone(GNCSearchCoreType *fe);
static gboolean validate (GNCSearchCoreType *fe);
static GtkWidget *get_widget(GNCSearchCoreType *fe);
static QueryPredData_t get_predicate (GNCSearchCoreType *fe);

static void gnc_search_debcred_class_init	(GNCSearchDebcredClass *class);
static void gnc_search_debcred_init	(GNCSearchDebcred *gspaper);
static void gnc_search_debcred_finalise	(GtkObject *obj);

#define _PRIVATE(x) (((GNCSearchDebcred *)(x))->priv)

struct _GNCSearchDebcredPrivate {
};

static GNCSearchCoreTypeClass *parent_class;

enum {
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

guint
gnc_search_debcred_get_type (void)
{
  static guint type = 0;
	
  if (!type) {
    GtkTypeInfo type_info = {
      "GNCSearchDebcred",
      sizeof(GNCSearchDebcred),
      sizeof(GNCSearchDebcredClass),
      (GtkClassInitFunc)gnc_search_debcred_class_init,
      (GtkObjectInitFunc)gnc_search_debcred_init,
      (GtkArgSetFunc)NULL,
      (GtkArgGetFunc)NULL
    };
		
    type = gtk_type_unique(gnc_search_core_type_get_type (), &type_info);
  }
	
  return type;
}

static void
gnc_search_debcred_class_init (GNCSearchDebcredClass *class)
{
  GtkObjectClass *object_class;
  GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)class;

  object_class = (GtkObjectClass *)class;
  parent_class = gtk_type_class(gnc_search_core_type_get_type ());

  object_class->finalize = gnc_search_debcred_finalise;

  /* override methods */
  gnc_search_core_type->validate = validate;
  gnc_search_core_type->get_widget = get_widget;
  gnc_search_core_type->get_predicate = get_predicate;
  gnc_search_core_type->clone = clone;

  /* signals */

  gtk_object_class_add_signals(object_class, signals, LAST_SIGNAL);
}

static void
gnc_search_debcred_init (GNCSearchDebcred *o)
{
  o->priv = g_malloc0 (sizeof (*o->priv));
  o->value = gnc_numeric_zero ();
  o->how = COMPARE_LT;
  o->option = NUMERIC_MATCH_ANY;
}

static void
gnc_search_debcred_finalise (GtkObject *obj)
{
  GNCSearchDebcred *o = (GNCSearchDebcred *)obj;
  g_assert (IS_GNCSEARCH_DEBCRED (o));

  g_free(o->priv);
	
  ((GtkObjectClass *)(parent_class))->finalize(obj);
}

/**
 * gnc_search_debcred_new:
 *
 * Create a new GNCSearchDebcred object.
 * 
 * Return value: A new #GNCSearchDebcred object.
 **/
GNCSearchDebcred *
gnc_search_debcred_new (void)
{
  GNCSearchDebcred *o = (GNCSearchDebcred *)gtk_type_new(gnc_search_debcred_get_type ());
  return o;
}

void
gnc_search_debcred_set_value (GNCSearchDebcred *fi, gnc_numeric value)
{
  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_DEBCRED (fi));
	
  fi->value = value;
}

void
gnc_search_debcred_set_how (GNCSearchDebcred *fi, query_compare_t how)
{
  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_DEBCRED (fi));
  fi->how = how;
}

void
gnc_search_debcred_set_option (GNCSearchDebcred *fi, numeric_match_t option)
{
  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_DEBCRED (fi));
  fi->option = option;
}

static gboolean
validate (GNCSearchCoreType *fe)
{
  GNCSearchDebcred *fi = (GNCSearchDebcred *)fe;
  gboolean valid = TRUE;

  g_return_val_if_fail (fi, FALSE);
  g_return_val_if_fail (IS_GNCSEARCH_DEBCRED (fi), FALSE);
	
  /* XXX */

  return valid;
}

static void
how_option_changed (GtkWidget *widget, GNCSearchDebcred *fe)
{
  fe->how = (query_compare_t)
    gtk_object_get_data (GTK_OBJECT (widget), "option");
}

static void
option_changed (GtkWidget *widget, GNCSearchDebcred *fe)
{
  fe->option = (numeric_match_t)
    gtk_object_get_data (GTK_OBJECT (widget), "option");
}

static void
entry_changed (GNCAmountEdit *entry, GNCSearchDebcred *fe)
{
  fe->value = gnc_amount_edit_get_amount (entry);
}

static GtkWidget *
add_menu_item (GtkWidget *menu, gpointer user_data, char *label,
	       query_compare_t option, GtkSignalFunc fcn)
{
  GtkWidget *item = gtk_menu_item_new_with_label (label);
  gtk_object_set_data (GTK_OBJECT (item), "option", (gpointer) option);
  gtk_signal_connect (GTK_OBJECT (item), "activate", fcn, user_data);
  gtk_menu_append (GTK_MENU (menu), item);
  gtk_widget_show (item);
  return item;
}

#define ADD_MENU_ITEM(cmp,str,op,fcn) { \
	item = add_menu_item (menu, fe, str, op, fcn); \
	if (cmp == op) { current = index; first = item; } \
	index++; \
} 

static GtkWidget *
make_how_menu (GNCSearchCoreType *fe)
{
  GNCSearchDebcred *fi = (GNCSearchDebcred *)fe;
  GtkWidget *menu, *item, *first, *opmenu;
  int current = 0, index = 0;

  menu = gtk_menu_new ();

  ADD_MENU_ITEM (fi->how, _("less than"), COMPARE_LT, how_option_changed);
  first = item;			/* Force one */
  ADD_MENU_ITEM (fi->how, _("less than or equal to"), COMPARE_LTE,
		 how_option_changed);
  ADD_MENU_ITEM (fi->how, _("equal to"), COMPARE_EQUAL, how_option_changed);
  ADD_MENU_ITEM (fi->how, _("not equal to"), COMPARE_NEQ,
		 how_option_changed);
  ADD_MENU_ITEM (fi->how, _("greater than"), COMPARE_GT,
		 how_option_changed);
  ADD_MENU_ITEM (fi->how, _("greater than or equal to"), COMPARE_GTE,
		 how_option_changed);

  opmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu (GTK_OPTION_MENU (opmenu), menu);

  gtk_signal_emit_by_name (GTK_OBJECT (first), "activate", fe);
  gtk_option_menu_set_history (GTK_OPTION_MENU (opmenu), current);

  return opmenu;
}

static GtkWidget *
make_option_menu (GNCSearchCoreType *fe)
{
  GNCSearchDebcred *fi = (GNCSearchDebcred *)fe;
  GtkWidget *menu, *item, *first, *opmenu;
  int current = 0, index = 0;

  menu = gtk_menu_new ();

  ADD_MENU_ITEM (fi->option, _("has credits or debits"), NUMERIC_MATCH_ANY,
		 option_changed);
  first = item;			/* Force one */
  ADD_MENU_ITEM (fi->option, _("has debits"), NUMERIC_MATCH_DEBIT,
		 option_changed);
  ADD_MENU_ITEM (fi->option, _("has credits"), NUMERIC_MATCH_CREDIT,
		 option_changed);

  opmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu (GTK_OPTION_MENU (opmenu), menu);

  gtk_signal_emit_by_name (GTK_OBJECT (first), "activate", fe);
  gtk_option_menu_set_history (GTK_OPTION_MENU (opmenu), current);

  return opmenu;
}

static GtkWidget *
get_widget (GNCSearchCoreType *fe)
{
  GtkWidget *entry, *menu, *box;
  GNCSearchDebcred *fi = (GNCSearchDebcred *)fe;
	
  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_DEBCRED (fi), NULL);

  box = gtk_hbox_new (FALSE, 3);

  /* Build and connect the option menus */
  menu = make_option_menu (fe);
  gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

  menu = make_how_menu (fe);
  gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

  /* Build and connect the entry window */
  entry = gnc_amount_edit_new ();
  gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (entry), fi->value);
  gtk_signal_connect (GTK_OBJECT (entry), "amount_changed", entry_changed, fe);
  gtk_box_pack_start (GTK_BOX (box), entry, FALSE, FALSE, 3);

  /* And return the box */
  return box;
}

static QueryPredData_t get_predicate (GNCSearchCoreType *fe)
{
  GNCSearchDebcred *fi = (GNCSearchDebcred *)fe;

  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_DEBCRED (fi), NULL);

  return gncQueryNumericPredicate (fi->how, fi->option, fi->value);
}

static GNCSearchCoreType *clone(GNCSearchCoreType *fe)
{
  GNCSearchDebcred *se, *fse = (GNCSearchDebcred *)fe;

  g_return_val_if_fail (fse, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_DEBCRED (fse), NULL);

  se = gnc_search_debcred_new ();
  gnc_search_debcred_set_value (se, fse->value);
  gnc_search_debcred_set_how (se, fse->how);
  gnc_search_debcred_set_option (se, fse->option);

  return (GNCSearchCoreType *)se;
}
