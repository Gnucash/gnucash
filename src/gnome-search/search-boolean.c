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

#include "QueryCore.h"

#include "search-boolean.h"

#define d(x)

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QueryPredData_t gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_boolean_class_init	(GNCSearchBooleanClass *class);
static void gnc_search_boolean_init	(GNCSearchBoolean *gspaper);
static void gnc_search_boolean_finalize	(GObject *obj);

typedef struct _GNCSearchBooleanPrivate GNCSearchBooleanPrivate;

struct _GNCSearchBooleanPrivate {
  gpointer dummy;
};

#define _PRIVATE(o) \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_SEARCH_BOOLEAN, GNCSearchBooleanPrivate))

static GNCSearchCoreTypeClass *parent_class;

guint
gnc_search_boolean_get_type (void)
{
  static guint type = 0;
	
  if (!type) {
    GTypeInfo type_info = {
      sizeof(GNCSearchBooleanClass),    /* class_size */
      NULL,   				/* base_init */
      NULL,				/* base_finalize */
      (GClassInitFunc)gnc_search_boolean_class_init,
      NULL,				/* class_finalize */
      NULL,				/* class_data */
      sizeof(GNCSearchBoolean),		/* */
      0,				/* n_preallocs */
      (GInstanceInitFunc)gnc_search_boolean_init,
    };
		
    type = g_type_register_static (GNC_TYPE_SEARCH_CORE_TYPE,
				   "GNCSearchBoolean",
				   &type_info, 0);
  }
	
  return type;
}

static void
gnc_search_boolean_class_init (GNCSearchBooleanClass *class)
{
  GObjectClass *object_class;
  GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)class;

  object_class = G_OBJECT_CLASS (class);
  parent_class = g_type_class_peek_parent (class);

  object_class->finalize = gnc_search_boolean_finalize;

  /* override methods */
  gnc_search_core_type->validate = gncs_validate;
  gnc_search_core_type->get_widget = gncs_get_widget;
  gnc_search_core_type->get_predicate = gncs_get_predicate;
  gnc_search_core_type->clone = gncs_clone;

  g_type_class_add_private(class, sizeof(GNCSearchBooleanPrivate));
}

static void
gnc_search_boolean_init (GNCSearchBoolean *o)
{
  o->how = COMPARE_EQUAL;
  o->value = TRUE;
}

static void
gnc_search_boolean_finalize (GObject *obj)
{
  GNCSearchBoolean *o = (GNCSearchBoolean *)obj;
  g_assert (IS_GNCSEARCH_BOOLEAN (o));

  G_OBJECT_CLASS (parent_class)->finalize(obj);
}

/**
 * gnc_search_boolean_new:
 *
 * Create a new GNCSearchBoolean object.
 * 
 * Return value: A new #GNCSearchBoolean object.
 **/
GNCSearchBoolean *
gnc_search_boolean_new (void)
{
  GNCSearchBoolean *o = g_object_new(GNC_TYPE_SEARCH_BOOLEAN, NULL);
  return o;
}

void
gnc_search_boolean_set_value (GNCSearchBoolean *fi, gboolean value)
{
  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_BOOLEAN (fi));
	
  fi->value = value;
}

void
gnc_search_boolean_set_how (GNCSearchBoolean *fi, query_compare_t how)
{
  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_BOOLEAN (fi));
  fi->how = how;
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
  GNCSearchBoolean *fi = (GNCSearchBoolean *)fe;
  gboolean valid = TRUE;

  g_return_val_if_fail (fi, FALSE);
  g_return_val_if_fail (IS_GNCSEARCH_BOOLEAN (fi), FALSE);
	
  /* XXX */

  return valid;
}

static void
option_changed (GtkWidget *widget, GNCSearchBoolean *fe)
{
  fe->how = (query_compare_t)
    g_object_get_data (G_OBJECT (widget), "option");
}

static void
toggle_changed (GtkToggleButton *button, GNCSearchBoolean *fe)
{
  fe->value = gtk_toggle_button_get_active (button);
}

static GtkWidget *
add_menu_item (GtkWidget *menu, gpointer user_data, char *label,
	       query_compare_t option)
{
  GtkWidget *item = gtk_menu_item_new_with_label (label);
  g_object_set_data (G_OBJECT (item), "option", (gpointer) option);
  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (option_changed), user_data);
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
  GNCSearchBoolean *fi = (GNCSearchBoolean *)fe;
  GtkWidget *menu, *item, *first, *opmenu;
  int current = 0, index = 0;

  menu = gtk_menu_new ();

  ADD_MENU_ITEM (_("is"), COMPARE_EQUAL);
  first = item;			/* Force one */ 
  ADD_MENU_ITEM (_("is not"), COMPARE_NEQ);

  opmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu (GTK_OPTION_MENU (opmenu), menu);

  g_signal_emit_by_name (G_OBJECT (first), "activate", fe);
  gtk_option_menu_set_history (GTK_OPTION_MENU (opmenu), current);

  return opmenu;
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
  GtkWidget *toggle, *menu, *box;
  GNCSearchBoolean *fi = (GNCSearchBoolean *)fe;
	
  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_BOOLEAN (fi), NULL);

  box = gtk_hbox_new (FALSE, 3);

  /* Build and connect the option menu */
  menu = make_menu (fe);
  gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

  /* Build and connect the toggle */
  toggle = gtk_toggle_button_new_with_label (_("set true"));
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle), fi->value);
  g_signal_connect (G_OBJECT (toggle), "toggled", G_CALLBACK (toggle_changed), fe);
  gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

  /* And return the box */
  return box;
}

static QueryPredData_t gncs_get_predicate (GNCSearchCoreType *fe)
{
  GNCSearchBoolean *fi = (GNCSearchBoolean *)fe;

  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_BOOLEAN (fi), NULL);

  return gncQueryBooleanPredicate (fi->how, fi->value);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
  GNCSearchBoolean *se, *fse = (GNCSearchBoolean *)fe;

  g_return_val_if_fail (fse, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_BOOLEAN (fse), NULL);

  se = gnc_search_boolean_new ();
  gnc_search_boolean_set_value (se, fse->value);
  gnc_search_boolean_set_how (se, fse->how);

  return (GNCSearchCoreType *)se;
}
