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

#include "Account.h"
#include "QueryCore.h"
#include "gnc-account-tree.h"
#include "gnc-gui-query.h"

#include "search-account.h"

#define d(x)

static GNCSearchCoreType *clone(GNCSearchCoreType *fe);
static gboolean validate (GNCSearchCoreType *fe);
static GtkWidget *get_widget(GNCSearchCoreType *fe);
static QueryPredData_t get_predicate (GNCSearchCoreType *fe);

static void gnc_search_account_class_init	(GNCSearchAccountClass *class);
static void gnc_search_account_init	(GNCSearchAccount *gspaper);
static void gnc_search_account_finalise	(GtkObject *obj);

#define _PRIVATE(x) (((GNCSearchAccount *)(x))->priv)

struct _GNCSearchAccountPrivate {
  gboolean	match_all;
  GList *	selected_accounts;
};

static GNCSearchCoreTypeClass *parent_class;

enum {
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

guint
gnc_search_account_get_type (void)
{
  static guint type = 0;
	
  if (!type) {
    GtkTypeInfo type_info = {
      "GNCSearchAccount",
      sizeof(GNCSearchAccount),
      sizeof(GNCSearchAccountClass),
      (GtkClassInitFunc)gnc_search_account_class_init,
      (GtkObjectInitFunc)gnc_search_account_init,
      (GtkArgSetFunc)NULL,
      (GtkArgGetFunc)NULL
    };
		
    type = gtk_type_unique(gnc_search_core_type_get_type (), &type_info);
  }
	
  return type;
}

static void
gnc_search_account_class_init (GNCSearchAccountClass *class)
{
  GtkObjectClass *object_class;
  GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)class;

  object_class = (GtkObjectClass *)class;
  parent_class = gtk_type_class(gnc_search_core_type_get_type ());

  object_class->finalize = gnc_search_account_finalise;

  /* override methods */
  gnc_search_core_type->validate = validate;
  gnc_search_core_type->get_widget = get_widget;
  gnc_search_core_type->get_predicate = get_predicate;
  gnc_search_core_type->clone = clone;

  /* signals */

  gtk_object_class_add_signals(object_class, signals, LAST_SIGNAL);
}

static void
gnc_search_account_init (GNCSearchAccount *o)
{
  o->priv = g_malloc0 (sizeof (*o->priv));
  o->how = GUID_MATCH_ANY;
}

static void
gnc_search_account_finalise (GtkObject *obj)
{
  GNCSearchAccount *o = (GNCSearchAccount *)obj;
  g_assert (IS_GNCSEARCH_ACCOUNT (o));

  g_free(o->priv);
	
  ((GtkObjectClass *)(parent_class))->finalize(obj);
}

/**
 * gnc_search_account_new:
 *
 * Create a new GNCSearchAccount object.
 * 
 * Return value: A new #GNCSearchAccount object.
 **/
GNCSearchAccount *
gnc_search_account_new (void)
{
  GNCSearchAccount *o = (GNCSearchAccount *)gtk_type_new(gnc_search_account_get_type ());
  return o;
}

/**
 * gnc_search_account_matchall_new:
 *
 * Create a new GNCSearchAccount object.
 * 
 * Return value: A new #GNCSearchAccount object.
 **/
GNCSearchAccount *
gnc_search_account_matchall_new (void)
{
  GNCSearchAccount *o = (GNCSearchAccount *)gtk_type_new(gnc_search_account_get_type ());
  o->priv->match_all = TRUE;
  o->how = GUID_MATCH_ALL;
  return o;
}

static gboolean
validate (GNCSearchCoreType *fe)
{
  GNCSearchAccount *fi = (GNCSearchAccount *)fe;
  gboolean valid = TRUE;

  g_return_val_if_fail (fi, FALSE);
  g_return_val_if_fail (IS_GNCSEARCH_ACCOUNT (fi), FALSE);
	
  if (fi->priv->selected_accounts == NULL && fi->how ) {
    valid = FALSE;
    gnc_error_dialog (_("You have not selected any accounts"));
  }

  /* XXX */

  return valid;
}

static void
option_changed (GtkWidget *widget, GNCSearchAccount *fe)
{
  fe->how = (query_compare_t)
    gtk_object_get_data (GTK_OBJECT (widget), "option");
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
  GNCSearchAccount *fi = (GNCSearchAccount *)fe;
  GtkWidget *menu, *item, *first, *opmenu;
  int current = 0, index = 0;

  menu = gtk_menu_new ();

  if (fi->priv->match_all) {
    ADD_MENU_ITEM (_("matches all accounts"), GUID_MATCH_ALL);
    first = item;
  } else {
    ADD_MENU_ITEM (_("matches any account"), GUID_MATCH_ANY);
    first = item;			/* Force one */
    ADD_MENU_ITEM (_("matches no accounts"), GUID_MATCH_NONE);
  }

  opmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu (GTK_OPTION_MENU (opmenu), menu);

  gtk_signal_emit_by_name (GTK_OBJECT (first), "activate", fe);
  gtk_option_menu_set_history (GTK_OPTION_MENU (opmenu), current);

  return opmenu;
}

static char *
describe_button (GNCSearchAccount *fi)
{
  if (fi->priv->selected_accounts)
    return (_("Selected Accounts"));
  return (_("Choose Accounts"));
}

static void
button_clicked (GtkButton *button, GNCSearchAccount *fi)
{
  GnomeDialog *dialog;
  GtkWidget *account_tree;
  GtkWidget *accounts_scroller;
  GtkWidget *label;
  char *desc;

  /* Create the account tree */
  account_tree = gnc_account_tree_new ();
  gtk_clist_column_titles_hide(GTK_CLIST(account_tree));
  gnc_account_tree_hide_all_but_name(GNC_ACCOUNT_TREE(account_tree));
  gnc_account_tree_refresh(GNC_ACCOUNT_TREE(account_tree));
  gtk_clist_set_selection_mode(GTK_CLIST(account_tree),
			       GTK_SELECTION_MULTIPLE);

  /* Select the currently-selected accounts */
  if (fi->priv->selected_accounts)
    gnc_account_tree_select_accounts (GNC_ACCOUNT_TREE(account_tree),
				      fi->priv->selected_accounts, FALSE);

  /* Create the account scroller and put the tree in it */
  accounts_scroller = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add(GTK_CONTAINER(accounts_scroller), account_tree);
  gtk_widget_set_usize(GTK_WIDGET(accounts_scroller), 300, 300);

  /* Create the label */
  label = gtk_label_new (_("Select Accounts to Match"));

  /* Create the dialog */
  dialog =
    (GnomeDialog *) gnome_dialog_new (_("Select the Accounts to Compare"),
				      GNOME_STOCK_BUTTON_OK,
				      GNOME_STOCK_BUTTON_CANCEL,
				      NULL);
  gnome_dialog_close_hides (dialog, TRUE);

  /* Put the dialog together */
  gtk_box_pack_start ((GtkBox *)dialog->vbox, label,
		      TRUE, TRUE, 3);
  gtk_box_pack_start ((GtkBox *)dialog->vbox, accounts_scroller,
		      TRUE, TRUE, 3);

  gtk_widget_show_all (GTK_WIDGET (dialog));

  /* Now run the dialog */
  switch (gnome_dialog_run (dialog)) {
  case -1:			/* wm close */
  case 0:			/* ok */
    if (fi->priv->selected_accounts)
      g_list_free (fi->priv->selected_accounts);

    fi->priv->selected_accounts =
      gnc_account_tree_get_current_accounts (GNC_ACCOUNT_TREE (account_tree));

    desc = describe_button (fi);
    gtk_label_set_text (GTK_LABEL (GTK_BIN (button)->child), desc);
    break;

  case 1:			/* cancel */
    break;
  }

  gtk_widget_destroy (dialog);
}

static GtkWidget *
get_widget (GNCSearchCoreType *fe)
{
  GtkWidget *button, *label, *menu, *box;
  GNCSearchAccount *fi = (GNCSearchAccount *)fe;
  char *desc;
	
  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_ACCOUNT (fi), NULL);

  box = gtk_hbox_new (FALSE, 3);

  /* Build and connect the option menu */
  menu = make_menu (fe);
  gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

  /* Build and connect the account entry window */
  desc = describe_button (fi);
  label = gtk_label_new (desc);
  gtk_misc_set_alignment (GTK_MISC (label), 0.5, 0.5);

  button = gtk_button_new ();
  gtk_container_add (GTK_CONTAINER (button), label);
  gtk_signal_connect (GTK_OBJECT (button), "clicked", button_clicked, fe);
  gtk_box_pack_start (GTK_BOX (box), button, FALSE, FALSE, 3);

  /* And return the box */
  return box;
}

static QueryPredData_t get_predicate (GNCSearchCoreType *fe)
{
  GNCSearchAccount *fi = (GNCSearchAccount *)fe;
  GList *l = NULL, *node;

  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_ACCOUNT (fi), NULL);

  for (node = fi->priv->selected_accounts; node; node = node->next) {
    Account *acc = node->data;
    const GUID *guid = xaccAccountGetGUID (acc);
    l = g_list_prepend (l, (gpointer)guid);
  }
  l = g_list_reverse (l);

  return gncQueryGUIDPredicate (fi->how, l);
}

static GNCSearchCoreType *clone(GNCSearchCoreType *fe)
{
  GNCSearchAccount *se, *fse = (GNCSearchAccount *)fe;

  g_return_val_if_fail (fse, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_ACCOUNT (fse), NULL);

  se = gnc_search_account_new ();
  se->how = fse->how;
  se->priv->match_all = fse->priv->match_all;
  se->priv->selected_accounts = g_list_copy (fse->priv->selected_accounts);

  return (GNCSearchCoreType *)se;
}
