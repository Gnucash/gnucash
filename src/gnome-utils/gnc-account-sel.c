/**
 * gnc-account-sel.c -- combobox style account selection widget
 *
 * Copyright (C) 2002 Joshua Sled <jsled@asynchronous.org>
 * All rights reserved.
 * Copyright (C) 2006 David Hampton <hampton@employees.org>
 *
 * Gnucash is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 **/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-account.h"
#include "GNCId.h"
#include "gnc-account-sel.h"
#include "gnc-commodity.h"
#include "gnc-exp-parser.h"
#include "gnc-gtk-utils.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnc-session.h"

#define ACCT_DATA_TAG "gnc-account-sel_acct"

/* Signal codes */
enum
{
        ACCOUNT_SEL_CHANGED,
        LAST_SIGNAL
};

enum account_cols {
	ACCT_COL_NAME = 0,
	ACCT_COL_PTR,
	NUM_ACCT_COLS
};

static guint account_sel_signals [LAST_SIGNAL] = { 0 };

static void gnc_account_sel_init (GNCAccountSel *gas);
static void gnc_account_sel_class_init (GNCAccountSelClass *klass);
static void gnc_account_sel_finalize (GObject *object);
static void gnc_account_sel_dispose (GObject *object);

static void gas_filter_accounts (gpointer data, gpointer user_data);

static void gas_populate_list (GNCAccountSel *gas);

static void gas_new_account_click (GtkButton *b, gpointer ud);

static GtkHBox *parent_class;

GType
gnc_account_sel_get_type (void)
{
        static GType account_sel_type = 0;

        if (account_sel_type == 0) {
                GTypeInfo account_sel_info = {
                        sizeof (GNCAccountSelClass),
			NULL,
			NULL,
                        (GClassInitFunc) gnc_account_sel_class_init,
			NULL,
			NULL,
			sizeof (GNCAccountSel),
			0,
                        (GInstanceInitFunc) gnc_account_sel_init
                };

                account_sel_type = g_type_register_static (GTK_TYPE_HBOX,
							   "GNCAccountSel",
							   &account_sel_info, 0);
        }

        return account_sel_type;
}

static
void
gnc_account_sel_event_cb( QofInstance *entity,
                          QofEventId event_type,
                          gpointer user_data,
			  gpointer event_data )
{
        if ( ! ( event_type == QOF_EVENT_CREATE
                 || event_type == QOF_EVENT_MODIFY
                 || event_type == QOF_EVENT_DESTROY )
             || !GNC_IS_ACCOUNT(entity) ) {
                return;
        }
        gas_populate_list( (GNCAccountSel*)user_data );
}

static
void
gnc_account_sel_class_init (GNCAccountSelClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_account_sel_finalize;
	object_class->dispose = gnc_account_sel_dispose;

        account_sel_signals [ACCOUNT_SEL_CHANGED] =
                g_signal_new ("account_sel_changed",
                              G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GNCAccountSelClass, account_sel_changed),
			      NULL,
			      NULL,
			      g_cclosure_marshal_VOID__VOID,
                              G_TYPE_NONE,
			      0);
}

static void
combo_changed_cb(GNCAccountSel *gas, gpointer combo)
{
        g_signal_emit_by_name(gas, "account_sel_changed");
}

static void
gnc_account_sel_init (GNCAccountSel *gas)
{
	GtkWidget *widget;

        gas->initDone = FALSE;
        gas->acctTypeFilters = FALSE;
        gas->newAccountButton = NULL;

        g_object_set(gas, "spacing", 2, (gchar*)NULL);

	gas->store = gtk_list_store_new(NUM_ACCT_COLS, G_TYPE_STRING, G_TYPE_POINTER);
        widget =
	  gtk_combo_box_entry_new_with_model(GTK_TREE_MODEL(gas->store), ACCT_COL_NAME);
        gas->combo = GTK_COMBO_BOX_ENTRY(widget);
	gtk_combo_box_set_model(GTK_COMBO_BOX(widget),
				GTK_TREE_MODEL(gas->store));
	g_object_unref(gas->store);
        g_signal_connect_swapped(gas->combo, "changed",
                                 G_CALLBACK(combo_changed_cb), gas);
        gtk_container_add( GTK_CONTAINER(gas), widget );

        /* Add completion. */
	gnc_cbe_require_list_item(GTK_COMBO_BOX_ENTRY(widget));

        /* Get the accounts, place into combo list */
        gas_populate_list( gas );

        gas->eventHandlerId =
                qof_event_register_handler( gnc_account_sel_event_cb, gas );

        gas->initDone = TRUE;
}

typedef struct {
        GNCAccountSel *gas;
        GList **outList;
} account_filter_data;

static
void
gas_populate_list( GNCAccountSel *gas )
{
        account_filter_data atnd;
        Account *root;
	Account *acc;
	GtkTreeIter iter;
	GtkEntry *entry;
	gint i, active = -1;
        GList *accts, *ptr, *filteredAccts;
        gchar *currentSel, *name;

	entry = GTK_ENTRY(gtk_bin_get_child(GTK_BIN(gas->combo)));
        currentSel = gtk_editable_get_chars(
                GTK_EDITABLE(entry), 0, -1 );

        root = gnc_book_get_root_account( gnc_get_current_book() );
        accts = gnc_account_get_descendants_sorted( root );

        filteredAccts   = NULL;
        atnd.gas        = gas;
        atnd.outList    = &filteredAccts;

        g_list_foreach( accts, gas_filter_accounts, (gpointer)&atnd );
        g_list_free( accts );

	gtk_list_store_clear(gas->store);
	for (ptr = filteredAccts, i = 0; ptr; ptr = g_list_next(ptr), i++) {
	  acc = ptr->data;
	  name = xaccAccountGetFullName(acc);
	  gtk_list_store_append(gas->store, &iter);
	  gtk_list_store_set(gas->store, &iter,
			     ACCT_COL_NAME, name,
			     ACCT_COL_PTR,  acc,
			     -1);
	  if (g_utf8_collate(name, currentSel) == 0) {
	    active = i;
	  g_free(name);
	  }
	}

        /* If the account which was in the text box before still exists, then
         * reset to it. */
	if (active != -1)
	  gtk_combo_box_set_active(GTK_COMBO_BOX(gas->combo), active);

        g_list_free( filteredAccts );
        if ( currentSel ) {
                g_free( currentSel );
        }
}

static
void
gas_filter_accounts( gpointer data, gpointer user_data )
{
        account_filter_data *atnd;
        Account *a;

        atnd = (account_filter_data*)user_data;
        a = (Account*)data;
        /* Filter as we've been configured to do. */
        if ( atnd->gas->acctTypeFilters ) {
                /* g_list_find is the poor-mans '(member ...)', especially
                 * easy when the data pointers in the list are just casted
                 * account type identifiers. */
                if ( g_list_find( atnd->gas->acctTypeFilters,
                                  GINT_TO_POINTER(xaccAccountGetType( a )) )
                     == NULL ) {
                        return;
                }
        }

        if ( atnd->gas->acctCommodityFilters ) {
                if ( g_list_find_custom( atnd->gas->acctCommodityFilters,
                                  GINT_TO_POINTER(xaccAccountGetCommodity( a )),
                                  gnc_commodity_compare) 
                     == NULL ) {
                        return;
                }
        }

        
        *atnd->outList = g_list_append( *atnd->outList, a );
}


GtkWidget *
gnc_account_sel_new (void)
{
        GNCAccountSel *gas;

        gas = g_object_new (GNC_TYPE_ACCOUNT_SEL, NULL);

        return GTK_WIDGET (gas);
}

typedef struct {
  GNCAccountSel *gas;
  Account *acct;
} gas_find_data;

static
gboolean
gnc_account_sel_find_account (GtkTreeModel *model,
			      GtkTreePath *path,
			      GtkTreeIter *iter,
			      gas_find_data *data)
{
  Account *model_acc;

  gtk_tree_model_get(model, iter, ACCT_COL_PTR, &model_acc, -1);
  if (data->acct != model_acc)
    return FALSE;

  gtk_combo_box_set_active_iter(GTK_COMBO_BOX(data->gas->combo), iter);
  return TRUE;
}
void
gnc_account_sel_set_account( GNCAccountSel *gas, Account *acct, gboolean set_default_acct )
{
	gas_find_data data;

        if (set_default_acct)
        {
          gtk_combo_box_set_active(GTK_COMBO_BOX(gas->combo), 0);
        }
        else
        {
	  gtk_combo_box_set_active( GTK_COMBO_BOX(gas->combo), -1 );
        }
        if ( acct == NULL )
                return;

	data.gas = gas;
	data.acct = acct;
	gtk_tree_model_foreach(GTK_TREE_MODEL(gas->store),
			       (GtkTreeModelForeachFunc)gnc_account_sel_find_account,
			       &data);
}

Account*
gnc_account_sel_get_account( GNCAccountSel *gas )
{
	GtkTreeIter iter;
        Account *acc;

	if (!gtk_combo_box_get_active_iter(GTK_COMBO_BOX(gas->combo), &iter))
	  return NULL;

	gtk_tree_model_get(GTK_TREE_MODEL(gas->store), &iter,
			   ACCT_COL_PTR, &acc,
			   -1);
        return acc;
}


void
gnc_account_sel_set_acct_filters( GNCAccountSel *gas, GList *typeFilters, GList *commodityFilters )
{
        GList *src=NULL;
        GList *dest=NULL;
        gnc_commodity* commClone=NULL;

        if ( gas->acctTypeFilters != NULL ) {
                g_list_free( gas->acctTypeFilters );
                gas->acctTypeFilters = NULL;
        }

        if ( gas->acctCommodityFilters != NULL) {
                g_list_free( gas->acctCommodityFilters );
                gas->acctCommodityFilters = NULL;
        }

        /* If both filters are null, then no filters exist. */
        if (( ! typeFilters ) && ( ! commodityFilters)) {
                return;
        }

        /* This works because the GNCAccountTypes in the list are
         * ints-casted-as-pointers. */
        if (typeFilters)
        {
            gas->acctTypeFilters = g_list_copy( typeFilters );
        }

        if (commodityFilters)
        {
            src = commodityFilters;

            while (src->data != NULL)
            {
                //gnc_commodity_clone would have been nice but it expects me to 
                //insert the clone into a book, which I don't want to do.
                commClone = gnc_commodity_new(qof_session_get_book(gnc_get_current_session()), "","","","",1);
                gnc_commodity_copy(commClone, src->data);
                dest = g_list_prepend(dest, commClone);
                if (src->next == NULL)
                {
                    break;
                }
                src = src->next;
            }
            gas->acctCommodityFilters = dest;
        }

        gas_populate_list( gas );
}

static void
gnc_account_sel_finalize (GObject *object)
{
	GNCAccountSel *gas;
	
	g_return_if_fail (object != NULL);
        g_return_if_fail (GNC_IS_ACCOUNT_SEL (object));

	gas = GNC_ACCOUNT_SEL (object);

        if (gas->acctTypeFilters) {
                g_list_free (gas->acctTypeFilters);
        }

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
gnc_account_sel_dispose (GObject *object)
{
	GNCAccountSel *gas;
	
	g_return_if_fail (object != NULL);
        g_return_if_fail (GNC_IS_ACCOUNT_SEL (object));

	gas = GNC_ACCOUNT_SEL (object);

        if (gas->eventHandlerId) {
		qof_event_unregister_handler (gas->eventHandlerId);
		gas->eventHandlerId = 0;
	}

	G_OBJECT_CLASS (parent_class)->dispose (object);
}

void
gnc_account_sel_set_new_account_ability( GNCAccountSel *gas,
                                         gboolean state )
{
        g_return_if_fail (gas != NULL);

        if ( state == (gas->newAccountButton != NULL) ) {
                /* We're already in that state; don't do anything. */
                return;
        }

        if ( gas->newAccountButton ) {
                g_assert( state == TRUE );
                /* destroy the existing button. */
                gtk_container_remove( GTK_CONTAINER(gas),
                                      gas->newAccountButton );
                gtk_widget_destroy( gas->newAccountButton );
                gas->newAccountButton = NULL;
                return;
        }
        
        /* create the button. */
        gas->newAccountButton = gtk_button_new_with_label( _("New...") );
        g_signal_connect( gas->newAccountButton,
			  "clicked",
			  G_CALLBACK( gas_new_account_click ),
			  gas );
        gtk_box_pack_start( GTK_BOX(gas), gas->newAccountButton,
                            FALSE, FALSE, 0 );
}

void
gnc_account_sel_set_new_account_modal( GNCAccountSel *gas,
				       gboolean state )
{
	g_return_if_fail (gas != NULL);
	gas->isModal = state;
}

static void
gas_new_account_click( GtkButton *b, gpointer ud )
{
        GNCAccountSel *gas = (GNCAccountSel*)ud;
	if (gas->isModal)
	  gnc_ui_new_accounts_from_name_window_with_types ( NULL,
							    gas->acctTypeFilters );
	else
	  gnc_ui_new_account_with_types( gnc_get_current_book(), gas->acctTypeFilters );
}

gint
gnc_account_sel_get_num_account( GNCAccountSel *gas )
{
  if (NULL == gas)
    return 0;
  return gtk_tree_model_iter_n_children( GTK_TREE_MODEL(gas->store), NULL );
}

void
gnc_account_sel_purge_account( GNCAccountSel *gas,
			       Account *target,
			       gboolean recursive)
{
  GtkTreeModel *model = GTK_TREE_MODEL(gas->store);
  GtkTreeIter iter;
  Account *acc;
  gboolean more;

  if (!gtk_tree_model_get_iter_first(model, &iter))
    return;

  if (!recursive) {
    do {
      gtk_tree_model_get(model, &iter, ACCT_COL_PTR, &acc, -1);
      if (acc == target) {
	gtk_list_store_remove(gas->store, &iter);
	break;
      }
    } while (gtk_tree_model_iter_next(model, &iter));
  } else {
    do {
      gtk_tree_model_get(model, &iter, ACCT_COL_PTR, &acc, -1);
      while (acc) {
	if (acc == target)
	  break;
	acc = gnc_account_get_parent(acc);
      }

      if (acc == target)
	more = gtk_list_store_remove(gas->store, &iter);
      else
	more = gtk_tree_model_iter_next(model, &iter);
    } while (more);
  }

  gtk_combo_box_set_active(GTK_COMBO_BOX(gas->combo), 0);
}


