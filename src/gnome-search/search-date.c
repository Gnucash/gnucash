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

#include "date.h"
#include "gnc-date-edit.h"
#include "QueryCore.h"

#include "search-date.h"

#define d(x)

static GNCSearchCoreType *clone(GNCSearchCoreType *fe);
static gboolean validate (GNCSearchCoreType *fe);
static GtkWidget *get_widget(GNCSearchCoreType *fe);
static QueryPredData_t get_predicate (GNCSearchCoreType *fe);

static void gnc_search_date_class_init	(GNCSearchDateClass *class);
static void gnc_search_date_init	(GNCSearchDate *gspaper);
static void gnc_search_date_finalise	(GtkObject *obj);

#define _PRIVATE(x) (((GNCSearchDate *)(x))->priv)

struct _GNCSearchDatePrivate {
};

static GNCSearchCoreTypeClass *parent_class;

enum {
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

guint
gnc_search_date_get_type (void)
{
  static guint type = 0;
	
  if (!type) {
    GtkTypeInfo type_info = {
      "GNCSearchDate",
      sizeof(GNCSearchDate),
      sizeof(GNCSearchDateClass),
      (GtkClassInitFunc)gnc_search_date_class_init,
      (GtkObjectInitFunc)gnc_search_date_init,
      (GtkArgSetFunc)NULL,
      (GtkArgGetFunc)NULL
    };
		
    type = gtk_type_unique(gnc_search_core_type_get_type (), &type_info);
  }
	
  return type;
}

static void
gnc_search_date_class_init (GNCSearchDateClass *class)
{
  GtkObjectClass *object_class;
  GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)class;

  object_class = (GtkObjectClass *)class;
  parent_class = gtk_type_class(gnc_search_core_type_get_type ());

  object_class->finalize = gnc_search_date_finalise;

  /* override methods */
  gnc_search_core_type->validate = validate;
  gnc_search_core_type->get_widget = get_widget;
  gnc_search_core_type->get_predicate = get_predicate;
  gnc_search_core_type->clone = clone;

  /* signals */

  gtk_object_class_add_signals(object_class, signals, LAST_SIGNAL);
}

static void
gnc_search_date_init (GNCSearchDate *o)
{
  o->priv = g_malloc0 (sizeof (*o->priv));
  o->ts.tv_sec = time(NULL);
  o->how = COMPARE_LT;
}

static void
gnc_search_date_finalise (GtkObject *obj)
{
  GNCSearchDate *o = (GNCSearchDate *)obj;
  g_assert (IS_GNCSEARCH_DATE (o));

  g_free(o->priv);
	
  ((GtkObjectClass *)(parent_class))->finalize(obj);
}

/**
 * gnc_search_date_new:
 *
 * Create a new GNCSearchDate object.
 * 
 * Return value: A new #GNCSearchDate object.
 **/
GNCSearchDate *
gnc_search_date_new (void)
{
  GNCSearchDate *o = (GNCSearchDate *)gtk_type_new(gnc_search_date_get_type ());
  return o;
}

void
gnc_search_date_set_date (GNCSearchDate *fi, Timespec ts)
{
  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_DATE (fi));
	
  fi->ts = ts;
}

void
gnc_search_date_set_how (GNCSearchDate *fi, query_compare_t how)
{
  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_DATE (fi));
  fi->how = how;
}

static gboolean
validate (GNCSearchCoreType *fe)
{
  GNCSearchDate *fi = (GNCSearchDate *)fe;
  gboolean valid = TRUE;

  g_return_val_if_fail (fi, FALSE);
  g_return_val_if_fail (IS_GNCSEARCH_DATE (fi), FALSE);
	
  /* XXX */

  return valid;
}

static void
option_changed (GtkWidget *widget, GNCSearchDate *fe)
{
  fe->how = (query_compare_t)
    gtk_object_get_data (GTK_OBJECT (widget), "option");
}

static void
date_changed (GNCDateEdit *date_edit, GNCSearchDate *fe)
{
  fe->ts = gnc_date_edit_get_date_ts (date_edit);
}

static GtkWidget *
add_menu_item (GtkWidget *menu, gpointer user_data, char *label,
	       query_compare_t option)
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
  GNCSearchDate *fi = (GNCSearchDate *)fe;
  GtkWidget *menu, *item, *first, *opmenu;
  int current = 0, index = 0;

  menu = gtk_menu_new ();

  ADD_MENU_ITEM (_("is before"), COMPARE_LT);
  first = item;			/* Force one */
  ADD_MENU_ITEM (_("is before or on"), COMPARE_LTE);
  ADD_MENU_ITEM (_("is on"), COMPARE_EQUAL);
  ADD_MENU_ITEM (_("is not on"), COMPARE_NEQ);
  ADD_MENU_ITEM (_("is after"), COMPARE_GT);
  ADD_MENU_ITEM (_("is on or after"), COMPARE_GTE);

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
  GNCSearchDate *fi = (GNCSearchDate *)fe;
	
  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_DATE (fi), NULL);

  box = gtk_hbox_new (FALSE, 3);

  /* Build and connect the option menu */
  menu = make_menu (fe);
  gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

  /* Build and connect the date entry window */
  entry = gnc_date_edit_new_ts (fi->ts, FALSE, FALSE);
  gtk_signal_connect (GTK_OBJECT (entry), "date_changed", date_changed, fe);
  gtk_box_pack_start (GTK_BOX (box), entry, FALSE, FALSE, 3);

  /* And return the box */
  return box;
}

static QueryPredData_t get_predicate (GNCSearchCoreType *fe)
{
  GNCSearchDate *fi = (GNCSearchDate *)fe;

  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_DATE (fi), NULL);

  return gncQueryDatePredicate (fi->how, DATE_MATCH_NORMAL, fi->ts);
}

static GNCSearchCoreType *clone(GNCSearchCoreType *fe)
{
  GNCSearchDate *se, *fse = (GNCSearchDate *)fe;

  g_return_val_if_fail (fse, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_DATE (fse), NULL);

  se = gnc_search_date_new ();
  gnc_search_date_set_date (se, fse->ts);
  gnc_search_date_set_how (se, fse->how);

  return (GNCSearchCoreType *)se;
}
