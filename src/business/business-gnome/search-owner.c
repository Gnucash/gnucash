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
#include "gnc-ui-util.h"
#include "gnc-gui-query.h"
#include "gncOwner.h"

#include "business-utils.h"
#include "search-owner.h"

#define d(x)

static GNCSearchCoreType *clone(GNCSearchCoreType *fe);
static gboolean validate (GNCSearchCoreType *fe);
static GtkWidget *get_widget(GNCSearchCoreType *fe);
static QueryPredData_t get_predicate (GNCSearchCoreType *fe);

static void gnc_search_owner_class_init	(GNCSearchOwnerClass *class);
static void gnc_search_owner_init	(GNCSearchOwner *gspaper);
static void gnc_search_owner_finalise	(GtkObject *obj);

#define _PRIVATE(x) (((GNCSearchOwner *)(x))->priv)

struct _GNCSearchOwnerPrivate {
  GncOwner	owner;
  GtkWidget *	owner_box;
  GtkWidget *	owner_choice;
};

static GNCSearchCoreTypeClass *parent_class;

enum {
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

guint
gnc_search_owner_get_type (void)
{
  static guint type = 0;
	
  if (!type) {
    GtkTypeInfo type_info = {
      "GNCSearchOwner",
      sizeof(GNCSearchOwner),
      sizeof(GNCSearchOwnerClass),
      (GtkClassInitFunc)gnc_search_owner_class_init,
      (GtkObjectInitFunc)gnc_search_owner_init,
      (GtkArgSetFunc)NULL,
      (GtkArgGetFunc)NULL
    };
		
    type = gtk_type_unique(gnc_search_core_type_get_type (), &type_info);
  }
	
  return type;
}

static void
gnc_search_owner_class_init (GNCSearchOwnerClass *class)
{
  GtkObjectClass *object_class;
  GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)class;

  object_class = (GtkObjectClass *)class;
  parent_class = gtk_type_class(gnc_search_core_type_get_type ());

  object_class->finalize = gnc_search_owner_finalise;

  /* override methods */
  gnc_search_core_type->validate = validate;
  gnc_search_core_type->get_widget = get_widget;
  gnc_search_core_type->get_predicate = get_predicate;
  gnc_search_core_type->clone = clone;

  /* signals */

  gtk_object_class_add_signals(object_class, signals, LAST_SIGNAL);
}

static void
gnc_search_owner_init (GNCSearchOwner *o)
{
  o->priv = g_malloc0 (sizeof (*o->priv));
}

static void
gnc_search_owner_finalise (GtkObject *obj)
{
  GNCSearchOwner *o = (GNCSearchOwner *)obj;
  g_assert (IS_GNCSEARCH_OWNER (o));

  g_free(o->priv);
	
  ((GtkObjectClass *)(parent_class))->finalize(obj);
}

/**
 * gnc_search_owner_new:
 *
 * Create a new GNCSearchOwner object.
 * 
 * Return value: A new #GNCSearchOwner object.
 **/
GNCSearchOwner *
gnc_search_owner_new (void)
{
  GNCSearchOwner *o = (GNCSearchOwner *)gtk_type_new(gnc_search_owner_get_type ());
  return o;
}

static gboolean
validate (GNCSearchCoreType *fe)
{
  GNCSearchOwner *fi = (GNCSearchOwner *)fe;
  gboolean valid = TRUE;

  g_return_val_if_fail (fi, FALSE);
  g_return_val_if_fail (IS_GNCSEARCH_OWNER (fi), FALSE);
	
  if (fi->priv->owner.owner.undefined == NULL) {
    valid = FALSE;
    gnc_error_dialog (_("You have not selected an owner"));
  }

  /* XXX */

  return valid;
}

static int
owner_changed_cb (GtkWidget *widget, gpointer data)
{
  GNCSearchOwner *fe = data;
  gnc_owner_get_owner (fe->priv->owner_choice, &(fe->priv->owner));
  return FALSE;
}

static void
set_owner_widget (GNCSearchOwner *fe)
{
  /* Remove the old choice widget */
  if (fe->priv->owner_choice)
    gtk_container_remove (GTK_CONTAINER (fe->priv->owner_box),
			  fe->priv->owner_choice);

  /* Create a new choice widget */
  fe->priv->owner_choice =
    gnc_owner_select_create (NULL, fe->priv->owner_box,
			     gnc_get_current_book(), &(fe->priv->owner));

  /* Setup the "changed" callback */
  gtk_signal_connect (GTK_OBJECT (fe->priv->owner_choice), "changed",
		      GTK_SIGNAL_FUNC (owner_changed_cb), fe);

  gtk_widget_show_all (fe->priv->owner_choice);
}

static void
type_option_changed (GtkWidget *widget, GNCSearchOwner *fe)
{
  GncOwnerType type = (GncOwnerType)
    gtk_object_get_data (GTK_OBJECT (widget), "option");

  /* If the type changed or if we don't have a type create the owner_choice */
  if (type != gncOwnerGetType (&(fe->priv->owner))) {
    fe->priv->owner.type = type;
    fe->priv->owner.owner.undefined = NULL;
    set_owner_widget (fe);
  } else if (fe->priv->owner_choice == NULL)
    set_owner_widget (fe);
}

static GtkWidget *
add_type_menu_item (GtkWidget *menu, gpointer user_data, char *label,
		    GncOwnerType type)
{
  GtkWidget *item = gtk_menu_item_new_with_label (label);
  gtk_object_set_data (GTK_OBJECT (item), "option", (gpointer) type);
  gtk_signal_connect (GTK_OBJECT (item), "activate", type_option_changed,
		      user_data);
  gtk_menu_append (GTK_MENU (menu), item);
  gtk_widget_show (item);
  return item;
}

#define ADD_TYPE_MENU_ITEM(str,tp) { \
	item = add_type_menu_item (menu, fe, str, tp); \
	if (type == tp) { current = index; first = item; } \
	index++; \
} 

static GtkWidget *
make_type_menu (GNCSearchCoreType *fe)
{
  GNCSearchOwner *fi = (GNCSearchOwner *)fe;
  GtkWidget *menu, *item, *first, *opmenu;
  int current = 0, index = 0;
  GncOwnerType type;

  menu = gtk_menu_new ();

  type = gncOwnerGetType (&(fi->priv->owner));

  ADD_TYPE_MENU_ITEM (_("Customer"), GNC_OWNER_CUSTOMER);
  first = item;
  ADD_TYPE_MENU_ITEM (_("Vendor"), GNC_OWNER_VENDOR);
  ADD_TYPE_MENU_ITEM (_("Job"), GNC_OWNER_JOB);

  opmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu (GTK_OPTION_MENU (opmenu), menu);

  gtk_signal_emit_by_name (GTK_OBJECT (first), "activate", fe);
  gtk_option_menu_set_history (GTK_OPTION_MENU (opmenu), current);

  return opmenu;
}

static void
how_option_changed (GtkWidget *widget, GNCSearchOwner *fe)
{
  fe->how = (query_compare_t)
    gtk_object_get_data (GTK_OBJECT (widget), "option");
}

static GtkWidget *
add_how_menu_item (GtkWidget *menu, gpointer user_data, char *label,
	       query_compare_t option)
{
  GtkWidget *item = gtk_menu_item_new_with_label (label);
  gtk_object_set_data (GTK_OBJECT (item), "option", (gpointer) option);
  gtk_signal_connect (GTK_OBJECT (item), "activate", how_option_changed,
		      user_data);
  gtk_menu_append (GTK_MENU (menu), item);
  gtk_widget_show (item);
  return item;
}

#define ADD_HOW_MENU_ITEM(str,op) { \
	item = add_how_menu_item (menu, fe, str, op); \
	if (fi->how == op) { current = index; first = item; } \
	index++; \
} 

static GtkWidget *
make_how_menu (GNCSearchCoreType *fe)
{
  GNCSearchOwner *fi = (GNCSearchOwner *)fe;
  GtkWidget *menu, *item, *first, *opmenu;
  int current = 0, index = 0;

  menu = gtk_menu_new ();

  ADD_HOW_MENU_ITEM (_("is"), GUID_MATCH_ANY);
  first = item;			/* Force one */
  ADD_HOW_MENU_ITEM (_("is not"), GUID_MATCH_NONE);

  opmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu (GTK_OPTION_MENU (opmenu), menu);

  gtk_signal_emit_by_name (GTK_OBJECT (first), "activate", fe);
  gtk_option_menu_set_history (GTK_OPTION_MENU (opmenu), current);

  return opmenu;
}

static GtkWidget *
get_widget (GNCSearchCoreType *fe)
{
  GtkWidget *how_menu, *type_menu, *box;
  GNCSearchOwner *fi = (GNCSearchOwner *)fe;
	
  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_OWNER (fi), NULL);

  box = gtk_hbox_new (FALSE, 3);

  /* Build and connect the "how" option menu. */
  how_menu = make_how_menu (fe);
  gtk_box_pack_start (GTK_BOX (box), how_menu, FALSE, FALSE, 3);

  /* Create the owner box */
  fi->priv->owner_box = gtk_hbox_new (FALSE, 0);

  /* Build and connect the "type" option menu.
   * Note that this will build the owner_choice and
   * put it in the owner_box we just created.
   */
  type_menu = make_type_menu (fe);
  gtk_box_pack_start (GTK_BOX (box), type_menu, FALSE, FALSE, 3);

  /* connect the owner box */
  gtk_box_pack_start (GTK_BOX (box), fi->priv->owner_box, FALSE, FALSE, 3);

  /* And return the box */
  return box;
}

static QueryPredData_t get_predicate (GNCSearchCoreType *fe)
{
  GNCSearchOwner *fi = (GNCSearchOwner *)fe;
  const GUID *guid;
  GList *l = NULL;

  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_OWNER (fi), NULL);

  guid = gncOwnerGetGUID (&(fi->priv->owner));
  l = g_list_prepend (l, (gpointer)guid);

  return gncQueryGUIDPredicate (fi->how, l);
}

static GNCSearchCoreType *clone(GNCSearchCoreType *fe)
{
  GNCSearchOwner *se, *fse = (GNCSearchOwner *)fe;

  g_return_val_if_fail (fse, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_OWNER (fse), NULL);

  se = gnc_search_owner_new ();
  se->how = fse->how;
  gncOwnerCopy (&(fse->priv->owner), &(se->priv->owner));

  return (GNCSearchCoreType *)se;
}
