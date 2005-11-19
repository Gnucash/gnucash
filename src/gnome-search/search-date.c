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
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "QueryCore.h"

#include "search-date.h"

#define d(x)

static void editable_enters (GNCSearchCoreType *fe);
static void grab_focus (GNCSearchCoreType *fe);
static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QueryPredData_t gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_date_class_init	(GNCSearchDateClass *class);
static void gnc_search_date_init	(GNCSearchDate *gspaper);
static void gnc_search_date_finalize	(GObject *obj);

typedef struct _GNCSearchDatePrivate GNCSearchDatePrivate;

struct _GNCSearchDatePrivate {
  GtkWidget *entry;
};

#define _PRIVATE(o) \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_SEARCH_DATE, GNCSearchDatePrivate))

static GNCSearchCoreTypeClass *parent_class;

guint
gnc_search_date_get_type (void)
{
  static guint type = 0;
	
  if (!type) {
    GTypeInfo type_info = {
      sizeof(GNCSearchDateClass),       /* class_size */
      NULL,   				/* base_init */
      NULL,				/* base_finalize */
      (GClassInitFunc)gnc_search_date_class_init,
      NULL,				/* class_finalize */
      NULL,				/* class_data */
      sizeof(GNCSearchDate),		/* */
      0,				/* n_preallocs */
      (GInstanceInitFunc)gnc_search_date_init,
    };
		
    type = g_type_register_static (GNC_TYPE_SEARCH_CORE_TYPE,
				   "GNCSearchDate",
				   &type_info, 0);
  }
	
  return type;
}

static void
gnc_search_date_class_init (GNCSearchDateClass *class)
{
  GObjectClass *object_class;
  GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)class;

  object_class = G_OBJECT_CLASS (class);
  parent_class = g_type_class_peek_parent (class);

  object_class->finalize = gnc_search_date_finalize;

  /* override methods */
  gnc_search_core_type->editable_enters = editable_enters;
  gnc_search_core_type->grab_focus = grab_focus;
  gnc_search_core_type->validate = gncs_validate;
  gnc_search_core_type->get_widget = gncs_get_widget;
  gnc_search_core_type->get_predicate = gncs_get_predicate;
  gnc_search_core_type->clone = gncs_clone;

  g_type_class_add_private(class, sizeof(GNCSearchDatePrivate));
}

static void
gnc_search_date_init (GNCSearchDate *o)
{
  o->ts.tv_sec = time(NULL);
  o->how = COMPARE_LT;
}

static void
gnc_search_date_finalize (GObject *obj)
{
  GNCSearchDate *o;
  GNCSearchDatePrivate *priv;

  g_assert (IS_GNCSEARCH_DATE (obj));

  o = GNCSEARCH_DATE(obj);
  priv = _PRIVATE(o);
  if (priv->entry)
    gtk_widget_destroy (priv->entry);

  G_OBJECT_CLASS (parent_class)->finalize(obj);
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
  GNCSearchDate *o = g_object_new(GNC_TYPE_SEARCH_DATE, NULL);
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
gncs_validate (GNCSearchCoreType *fe)
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
    g_object_get_data (G_OBJECT (widget), "option");
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
  g_object_set_data (G_OBJECT (item), "option", (gpointer) option);
  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK(option_changed), user_data);
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

  g_signal_emit_by_name (G_OBJECT (first), "activate", fe);
  gtk_option_menu_set_history (GTK_OPTION_MENU (opmenu), current);

  return opmenu;
}

static void
grab_focus (GNCSearchCoreType *fe)
{
  GNCSearchDate *fi = (GNCSearchDate *)fe;
  GNCSearchDatePrivate *priv;

  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_DATE (fi));

  priv = _PRIVATE(fi);
  if (priv->entry)
    gtk_widget_grab_focus (GNC_DATE_EDIT(priv->entry)->date_entry);
}

static void
editable_enters (GNCSearchCoreType *fe)
{
  GNCSearchDate *fi = (GNCSearchDate *)fe;
  GNCSearchDatePrivate *priv;

  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_DATE (fi));

  priv = _PRIVATE(fi);
  if (priv->entry)
    gnc_date_editable_enters (GNC_DATE_EDIT (priv->entry), TRUE);
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
  GtkWidget *entry, *menu, *box;
  GNCSearchDate *fi = (GNCSearchDate *)fe;
  GNCSearchDatePrivate *priv;
	
  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_DATE (fi), NULL);

  priv = _PRIVATE(fi);
  box = gtk_hbox_new (FALSE, 3);

  /* Build and connect the option menu */
  menu = make_menu (fe);
  gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

  /* Build and connect the date entry window */
  entry = gnc_date_edit_new_ts (fi->ts, FALSE, FALSE);
  g_signal_connect (G_OBJECT (entry), "date_changed", G_CALLBACK (date_changed), fe);
  gtk_box_pack_start (GTK_BOX (box), entry, FALSE, FALSE, 3);
  g_object_ref (entry);
  priv->entry = entry;

  /* And return the box */
  return box;
}

static QueryPredData_t gncs_get_predicate (GNCSearchCoreType *fe)
{
  GNCSearchDate *fi = (GNCSearchDate *)fe;
  GNCSearchDatePrivate *priv;

  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_DATE (fi), NULL);

  /* Make sure we actually use the currently-entered date */
  priv = _PRIVATE(fi);
  if (priv->entry)
    fi->ts = gnc_date_edit_get_date_ts (GNC_DATE_EDIT (priv->entry));

  return gncQueryDatePredicate (fi->how, DATE_MATCH_NORMAL, fi->ts);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
  GNCSearchDate *se, *fse = (GNCSearchDate *)fe;

  g_return_val_if_fail (fse, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_DATE (fse), NULL);

  se = gnc_search_date_new ();
  gnc_search_date_set_date (se, fse->ts);
  gnc_search_date_set_how (se, fse->how);

  return (GNCSearchCoreType *)se;
}
