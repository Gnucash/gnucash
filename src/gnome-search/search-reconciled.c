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

#include "QueryCore.h"
#include "Transaction.h"	/* for ?REC */

#include "search-reconciled.h"

#define d(x)

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QueryPredData_t gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_reconciled_class_init	(GNCSearchReconciledClass *class);
static void gnc_search_reconciled_init	(GNCSearchReconciled *gspaper);
static void gnc_search_reconciled_finalise	(GtkObject *obj);

#define _PRIVATE(x) (((GNCSearchReconciled *)(x))->priv)

struct _GNCSearchReconciledPrivate {
};

static GNCSearchCoreTypeClass *parent_class;

enum {
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

guint
gnc_search_reconciled_get_type (void)
{
  static guint type = 0;
	
  if (!type) {
    GtkTypeInfo type_info = {
      "GNCSearchReconciled",
      sizeof(GNCSearchReconciled),
      sizeof(GNCSearchReconciledClass),
      (GtkClassInitFunc)gnc_search_reconciled_class_init,
      (GtkObjectInitFunc)gnc_search_reconciled_init,
      (GtkArgSetFunc)NULL,
      (GtkArgGetFunc)NULL
    };
		
    type = gtk_type_unique(gnc_search_core_type_get_type (), &type_info);
  }
	
  return type;
}

static void
gnc_search_reconciled_class_init (GNCSearchReconciledClass *class)
{
  GtkObjectClass *object_class;
  GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)class;

  object_class = (GtkObjectClass *)class;
  parent_class = gtk_type_class(gnc_search_core_type_get_type ());

  object_class->finalize = gnc_search_reconciled_finalise;

  /* override methods */
  gnc_search_core_type->validate = gncs_validate;
  gnc_search_core_type->get_widget = gncs_get_widget;
  gnc_search_core_type->get_predicate = gncs_get_predicate;
  gnc_search_core_type->clone = gncs_clone;

  /* signals */

  gtk_object_class_add_signals(object_class, signals, LAST_SIGNAL);
}

static void
gnc_search_reconciled_init (GNCSearchReconciled *o)
{
  o->priv = g_malloc0 (sizeof (*o->priv));
  o->how = COMPARE_EQUAL;
  o->value = CLEARED_NO;
}

static void
gnc_search_reconciled_finalise (GtkObject *obj)
{
  GNCSearchReconciled *o = (GNCSearchReconciled *)obj;
  g_assert (IS_GNCSEARCH_RECONCILED (o));

  g_free(o->priv);
	
  ((GtkObjectClass *)(parent_class))->finalize(obj);
}

/**
 * gnc_search_reconciled_new:
 *
 * Create a new GNCSearchReconciled object.
 * 
 * Return value: A new #GNCSearchReconciled object.
 **/
GNCSearchReconciled *
gnc_search_reconciled_new (void)
{
  GNCSearchReconciled *o = (GNCSearchReconciled *)gtk_type_new(gnc_search_reconciled_get_type ());
  return o;
}

void
gnc_search_reconciled_set_value (GNCSearchReconciled *fi, cleared_match_t value)
{
  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_RECONCILED (fi));
	
  fi->value = value;
}

void
gnc_search_reconciled_set_how (GNCSearchReconciled *fi, char_match_t how)
{
  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_RECONCILED (fi));
  fi->how = how;
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
  GNCSearchReconciled *fi = (GNCSearchReconciled *)fe;
  gboolean valid = TRUE;

  g_return_val_if_fail (fi, FALSE);
  g_return_val_if_fail (IS_GNCSEARCH_RECONCILED (fi), FALSE);
	
  /* XXX */

  return valid;
}

static void
option_changed (GtkWidget *widget, GNCSearchReconciled *fe)
{
  fe->how = (char_match_t)
    gtk_object_get_data (GTK_OBJECT (widget), "option");
}

static void
toggle_changed (GtkToggleButton *button, GNCSearchReconciled *fe)
{
  gboolean is_on = gtk_toggle_button_get_active (button);
  cleared_match_t value =
    (cleared_match_t) gtk_object_get_data (GTK_OBJECT (button), "button-value");

  if (is_on)
    fe->value |= value;
  else
    fe->value &= ~value;
}

static GtkWidget *
add_menu_item (GtkWidget *menu, gpointer user_data, char *label,
	       char_match_t option)
{
  GtkWidget *item = gtk_menu_item_new_with_label (label);
  gtk_object_set_data (GTK_OBJECT (item), "option", (gpointer) option);
  gtk_signal_connect (GTK_OBJECT (item), "activate", option_changed, user_data);
  gtk_menu_append (GTK_MENU (menu), item);
  gtk_widget_show (item);
  return item;
}

#define ADD_MENU_ITEM(str,op) { \
	item = add_menu_item (menu, fe, str, op); \
	if (fi->how == op) { current = index; first = item; } \
	index++; \
} 

static GtkWidget *
make_menu (GNCSearchCoreType *fe)
{
  GNCSearchReconciled *fi = (GNCSearchReconciled *)fe;
  GtkWidget *menu, *item, *first, *opmenu;
  int current = 0, index = 0;

  menu = gtk_menu_new ();

  ADD_MENU_ITEM (_("is"), CHAR_MATCH_ANY);
  first = item;			/* Force one */ 
  ADD_MENU_ITEM (_("is not"), CHAR_MATCH_NONE);

  opmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu (GTK_OPTION_MENU (opmenu), menu);

  gtk_signal_emit_by_name (GTK_OBJECT (first), "activate", fe);
  gtk_option_menu_set_history (GTK_OPTION_MENU (opmenu), current);

  return opmenu;
}

static GtkWidget *
make_toggle (GNCSearchReconciled *fi, char *label, char_match_t option)
{
  GtkWidget *toggle;

  toggle = gtk_toggle_button_new_with_label (label);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle), (fi->value & option));
  gtk_object_set_data (GTK_OBJECT (toggle), "button-value", (gpointer) option);
  gtk_signal_connect (GTK_OBJECT (toggle), "toggled", toggle_changed, fi);

  return toggle;
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
  GtkWidget *toggle, *menu, *box;
  GNCSearchReconciled *fi = (GNCSearchReconciled *)fe;
	
  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_RECONCILED (fi), NULL);

  box = gtk_hbox_new (FALSE, 3);

  /* Build and connect the option menu */
  menu = make_menu (fe);
  gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

  /* Build and connect the toggles */
  toggle = make_toggle (fi, _("Not Cleared"), CLEARED_NO);
  gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

  toggle = make_toggle (fi, _("Cleared"), CLEARED_CLEARED);
  gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

  toggle = make_toggle (fi, _("Reconciled"), CLEARED_RECONCILED);
  gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

  toggle = make_toggle (fi, _("Frozen"), CLEARED_FROZEN);
  gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

  toggle = make_toggle (fi, _("Voided"), CLEARED_VOIDED);
  gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

  /* And return the box */
  return box;
}

static QueryPredData_t gncs_get_predicate (GNCSearchCoreType *fe)
{
  GNCSearchReconciled *fi = (GNCSearchReconciled *)fe;
  char chars[6];
  cleared_match_t value;
  int i;

  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_RECONCILED (fi), NULL);

  /* This code should look a lot like xaccQueryAddClearedMatch() */

  value = fi->value;
  i = 0;

  if (value & CLEARED_CLEARED)
    chars[i++] = CREC;
  if (value & CLEARED_RECONCILED)
    chars[i++] = YREC;
  if (value & CLEARED_FROZEN)
    chars[i++] = FREC;
  if (value & CLEARED_NO)
    chars[i++] = NREC;
  if (value & CLEARED_VOIDED)
    chars[i++] = VREC;
  chars[i] = '\0';

  return gncQueryCharPredicate (fi->how, chars);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
  GNCSearchReconciled *se, *fse = (GNCSearchReconciled *)fe;

  g_return_val_if_fail (fse, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_RECONCILED (fse), NULL);

  se = gnc_search_reconciled_new ();
  gnc_search_reconciled_set_value (se, fse->value);
  gnc_search_reconciled_set_how (se, fse->how);

  return (GNCSearchCoreType *)se;
}
