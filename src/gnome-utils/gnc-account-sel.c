/**
 * gnc-account-sel.c -- combobox style account selection widget
 *
 * Copyright (C) 2002 Joshua Sled <jsled@asynchronous.org>
 * All rights reserved.
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 **/

#include "config.h"

#include "dialog-account.h"
#include "GNCId.h"
#include "gnc-account-sel.h"
#include "gnc-exp-parser.h"
#include "messages.h"
#include "gnc-ui-util.h"
#include "gnc-engine-util.h"
#include "gnc-event.h"

#define ACCT_DATA_TAG "gnc-account-sel_acct"

/* Signal codes */
enum
{
        ACCOUNT_SEL_CHANGED,
        LAST_SIGNAL
};

static gint account_sel_signals [LAST_SIGNAL] = { 0 };

static void gnc_account_sel_init         (GNCAccountSel      *gas);
static void gnc_account_sel_class_init   (GNCAccountSelClass *class);
static void gas_accounts_to_names( gpointer data, gpointer user_data );

static void gas_populate_list( GNCAccountSel *gas );
static void gas_strcmp_adapter( gpointer a, gpointer b );
static void gnc_account_sel_event_cb( GUID *entity, QofIdType id_type,
                                      GNCEngineEventType event_type,
                                      gpointer user_data );

static void gas_new_account_click( GtkButton *b, gpointer ud );

static void gas_destroy( GtkObject *o, gpointer user_data );

#if 0 /* completion not implemented */
static void gnc_account_sel_changed( GtkEditable *entry, gpointer ud );
static void gnc_account_sel_list_clicked( GtkButton *b, gpointer ud );
static gint gnc_account_sel_key_press( GtkWidget          *widget,
                                       GdkEventKey        *event );
#endif /* 0 -- completion not implemented */

static GtkHBox *parent_class;

guint
gnc_account_sel_get_type (void)
{
        static guint account_sel_type = 0;

        if (!account_sel_type){
                GtkTypeInfo account_sel_info = {
                        "GNCAccountSel",
                        sizeof (GNCAccountSel),
                        sizeof (GNCAccountSelClass),
                        (GtkClassInitFunc) gnc_account_sel_class_init,
                        (GtkObjectInitFunc) gnc_account_sel_init,
                        NULL,
                        NULL,
                        (GtkClassInitFunc) NULL,
                };

                account_sel_type = gtk_type_unique (GTK_TYPE_HBOX, &account_sel_info);
        }

        return account_sel_type;
}

static
void
gnc_account_sel_event_cb( GUID *entity, QofIdType type,
                          GNCEngineEventType event_type,
                          gpointer user_data )
{
        if ( ! ( event_type == GNC_EVENT_CREATE
                 || event_type == GNC_EVENT_MODIFY
                 || event_type == GNC_EVENT_DESTROY )
             || strcmp( type, GNC_ID_ACCOUNT ) != 0 ) {
                return;
        }
        gas_populate_list( (GNCAccountSel*)user_data );
}

static
void
gnc_account_sel_class_init (GNCAccountSelClass *class)
{
        GtkObjectClass *object_class;
        GtkWidgetClass *widget_class;
        GtkHBoxClass *hbox_class;

        object_class = (GtkObjectClass*) class;
        widget_class = (GtkWidgetClass*) class;
        hbox_class = (GtkHBoxClass*) class;

        parent_class = gtk_type_class (gtk_entry_get_type ());

        account_sel_signals [ACCOUNT_SEL_CHANGED] =
                gtk_signal_new ("account_sel_changed",
                                GTK_RUN_FIRST, object_class->type, 
                                GTK_SIGNAL_OFFSET (GNCAccountSelClass,
                                                   account_sel_changed),
                                gtk_signal_default_marshaller,
                                GTK_TYPE_NONE, 0);
	
        gtk_object_class_add_signals (object_class,
                                      account_sel_signals,
                                      LAST_SIGNAL);

        class->account_sel_changed = NULL;
}

static void
gnc_account_sel_init (GNCAccountSel *gas)
{
        gas->initDone = FALSE;
        gas->acctTypeFilters = FALSE;
        gas->newAccountButton = NULL;

        gas->combo = GTK_COMBO(gtk_combo_new());
        gtk_combo_set_value_in_list( gas->combo, TRUE, TRUE );
        gtk_container_add( GTK_CONTAINER(gas), GTK_WIDGET(gas->combo) );

        /* This is only because completion cannot be implemented. */
        gtk_editable_set_editable( GTK_EDITABLE(gas->combo->entry), FALSE );
        gtk_signal_connect( GTK_OBJECT(gas), "destroy",
                            GTK_SIGNAL_FUNC( gas_destroy ),
                            gas );

#if 0 /* completion not implemented. */
        gtk_signal_connect( GTK_OBJECT(gas->combo->entry), "changed",
                            GTK_SIGNAL_FUNC( gnc_account_sel_changed ),
                            gas );
#endif /* 0 -- completion not implemented. */

        /* Get the accounts, place into combo list */
        gas_populate_list( gas );

        gas->eventHandlerId =
                gnc_engine_register_event_handler( gnc_account_sel_event_cb, gas );

        gas->initDone = TRUE;
}

typedef struct {
        GNCAccountSel *gas;
        GList **outList;
        char accountSep;
} accounts_to_names_data;

/**
 * Used in the g_list_foreach call in gas_populate_list to see if the given
 * string is in [via strcmp] the list, and to set the flag as necessary.
 **/
typedef struct {
        char *str;
        gboolean flag;
} findData;

static
void
gas_strcmp_adapter( gpointer listElt, gpointer ud )
{
        findData *fd = (findData*)ud;
        fd->flag |= ( strcmp( (char*)listElt, (char*)fd->str ) == 0 );
}

static
void
gas_populate_list( GNCAccountSel *gas )
{
        accounts_to_names_data atnd;
        AccountGroup *ag;
        GList *accts, *nameList;
        gchar *currentSel;

        currentSel = gtk_editable_get_chars(
                GTK_EDITABLE(gas->combo->entry), 0, -1 );

        ag = gnc_book_get_group( gnc_get_current_book() );
        accts = (GList*)xaccGroupGetSubAccounts( ag );

        nameList        = NULL;
        atnd.gas        = gas;
        atnd.outList    = &nameList;
        atnd.accountSep = gnc_get_account_separator();

        g_list_foreach( accts, gas_accounts_to_names,
                        (gpointer)&atnd );
        g_list_free( accts );

	/* Make sure we have a list of something... */
	if ( nameList == NULL )
		nameList = g_list_prepend( NULL, "" );

	gtk_combo_set_popdown_strings( gas->combo, nameList );

        /* If the account which was in the text box before still exists, then
         * reset to it. */
        {
                findData tmpfd;
                gint pos;
                tmpfd.str = currentSel;
                tmpfd.flag = FALSE;

                g_list_foreach( nameList, gas_strcmp_adapter, &tmpfd );

                if ( tmpfd.flag ) {
                        gtk_editable_delete_text(
                                GTK_EDITABLE(gas->combo->entry), 0, -1 );
                        pos = 0;
                        gtk_editable_insert_text(
                                GTK_EDITABLE(gas->combo->entry),
                                currentSel, strlen(currentSel), &pos );
                }
        }
        g_list_free( nameList );
        if ( currentSel ) {
                g_free( currentSel );
        }

#if 0 /* completion not implemented */
        gas->completion = g_completion_new( NULL );
        g_completion_add_items( gas->completion, nameList );
#endif /* 0 -- completion not implemented */
}

static
void
gas_accounts_to_names( gpointer data, gpointer user_data )
{
        accounts_to_names_data *atnd;
        Account *a;

        atnd = (accounts_to_names_data*)user_data;
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
        *atnd->outList =
                g_list_append( *atnd->outList,
                               xaccAccountGetFullName(a, atnd->accountSep) );
}

#if 0 /* completion not implemented  */
/*
 * There is apparently no way -- in GTK 1.x -- to programatically select a
 * region in the way we would like... so we've disallowed manual editing of
 * the Account string.
 */
static
void
gnc_account_sel_changed( GtkEditable *entry, gpointer ud )
{
        gchar *s, *prefix;
        GNCAccountSel *gas = (GNCAccountSel*)ud;

        if ( !gas->initDone ) {
                return;
        }
        s = gtk_editable_get_chars( entry, 0, -1 );
        g_completion_complete( gas->completion, s, &prefix );
        if ( prefix && (strlen(prefix) > 0) ) {
                printf( "changed into \"%s\"; longest completion: \"%s\"\n", s, prefix );
                gtk_signal_handler_block_by_func( GTK_OBJECT(gas->combo->entry),
                                                  GTK_SIGNAL_FUNC(gnc_account_sel_changed),
                                                  ud );
                gtk_entry_set_text( GTK_ENTRY(gas->combo->entry), prefix );
                gtk_editable_select_region( GTK_EDITABLE(gas->combo->entry),
                                            strlen(s), -1 );
                {
                        GdkEventKey k;
                        gboolean ret;
                        
                        k.type = GDK_KEY_RELEASE;
                        k.send_event = TRUE;
                        k.state = GDK_SHIFT_MASK;
                        k.keyval = GDK_End;
                        k.length = 0;
                        k.string = "";
                        printf( "foo [%d : \"%s\"]\n", k.length, k.string );
                        //gtk_widget_event( GTK_WIDGET(gas->combo->entry), &e );
                        gtk_signal_emit_by_name( GTK_OBJECT(gas->combo->entry),
                                                 "key-press-event",
                                                 gas->combo->entry, &k, NULL, &ret );
                        printf( "bar\n" );
                }
                gtk_editable_set_position( GTK_EDITABLE(gas->combo->entry),
                                           strlen(s) );
                gtk_signal_handler_unblock_by_func( GTK_OBJECT(gas->combo->entry),
                                                    GTK_SIGNAL_FUNC(gnc_account_sel_changed),
                                                    ud );
                g_free( prefix );
        }
        g_free( s );
}

static
gint
gnc_account_sel_key_press(GtkWidget *widget, GdkEventKey *event)
{
        GNCAccountSel *gas = GNC_ACCOUNT_SEL(widget);
        gint result;

        result = (* GTK_WIDGET_CLASS (parent_class)->key_press_event)(widget, event);

        switch (event->keyval)
        {
        case GDK_Tab:
                /* FIXME: += equivalent keys. */
                break;
                if (event->state & (GDK_CONTROL_MASK | GDK_MOD1_MASK | GDK_SHIFT_MASK))
                        break;
                return result;
        case GDK_KP_Enter:
                break;
        default:
                return result;
        }

        return TRUE;
}
#endif /* 0 -- completion not implemented */

GtkWidget *
gnc_account_sel_new (void)
{
        GNCAccountSel *gas;

        gas = gtk_type_new (gnc_account_sel_get_type ());

        return GTK_WIDGET (gas);
}

GtkWidget *
gnc_account_sel_gtk_entry (GNCAccountSel *gas)
{
        g_return_val_if_fail(gas != NULL, NULL);
        g_return_val_if_fail(GNC_IS_ACCOUNT_SEL(gas), NULL);

        return (GtkWidget *)gas->combo->entry;
}

void
gnc_account_sel_set_account( GNCAccountSel *gas, Account *acct )
{
        gchar *acctStr;

        if ( acct == NULL ) {
                gtk_list_select_item( GTK_LIST(gas->combo->list), 0 );
                return;
        }
        acctStr = xaccAccountGetFullName( acct,
                                          gnc_get_account_separator() );
        gtk_entry_set_text( GTK_ENTRY(gas->combo->entry), acctStr );
        g_free( acctStr );
}

Account*
gnc_account_sel_get_account( GNCAccountSel *gas )
{
        AccountGroup *ag;
        Account *ret;
        gchar *txt;

        ret = NULL;
        txt = gtk_editable_get_chars( GTK_EDITABLE(gas->combo->entry), 0, -1 );
        g_assert( txt != NULL );
        if ( strlen(txt) == 0 ) {
                goto cleanup;
        }
        ag = gnc_book_get_group( gnc_get_current_book() );
        ret = xaccGetAccountFromFullName( ag, txt, gnc_get_account_separator() );
 cleanup:
        g_free( txt );
        return ret;
}


void
gnc_account_sel_set_acct_filters( GNCAccountSel *gas, GList *filters )
{
        if ( gas->acctTypeFilters != NULL ) {
                g_list_free( gas->acctTypeFilters );
                gas->acctTypeFilters = NULL;
        }
        /* If it's null, then no filters exist. */
        if ( ! filters ) {
                return;
        }
        /* This works because the GNCAccountTypes in the list are
         * ints-casted-as-pointers. */
        gas->acctTypeFilters = g_list_copy( filters );
        gas_populate_list( gas );
}

static
void
gas_destroy( GtkObject *o, gpointer user_data )
{
        GNCAccountSel *gas = (GNCAccountSel*)user_data;
        if ( gas->acctTypeFilters ) {
                g_list_free( gas->acctTypeFilters );
        }
        gnc_engine_unregister_event_handler( gas->eventHandlerId );
}

void
gnc_account_sel_set_new_account_ability( GNCAccountSel *gas,
                                         gboolean state )
{
        g_assert( gas );
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
        gtk_signal_connect( GTK_OBJECT(gas->newAccountButton),
                            "clicked",
                            GTK_SIGNAL_FUNC( gas_new_account_click ),
                            gas );
        gtk_box_pack_start( GTK_BOX(gas), gas->newAccountButton,
                            TRUE, FALSE, 2 );
}

void
gnc_account_sel_set_new_account_modal( GNCAccountSel *gas,
				       gboolean state )
{
	g_assert ( gas );
	gas->isModal = state;
}

static
void
gas_new_account_click( GtkButton *b, gpointer ud )
{
        GNCAccountSel *gas = (GNCAccountSel*)ud;
	if (gas->isModal)
	  gnc_ui_new_accounts_from_name_window_with_types ( NULL,
							    gas->acctTypeFilters );
	else
	  gnc_ui_new_account_with_types( NULL, gas->acctTypeFilters );
}
