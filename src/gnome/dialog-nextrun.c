/********************************************************************\
 * dialog-nextrun.c - "since last run" dialog.                      *
 * Copyright (c) 2001 Joshua Sled <jsled@asynchronous.org>          *
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
#include <glib.h>

#include "gnc-engine-util.h"
#include "FileDialog.h"
#include "gnc-book.h"
#include "Transaction.h"
#include "Group.h"
#include "gnc-numeric.h"
#include "SchedXaction.h"
#include "gnc-component-manager.h"
#include "dialog-nextrun.h"
#include "SplitLedger.h"
#include "gnc-ui-util.h"
#include "gnc-exp-parser.h"
#include "dialog-utils.h"

#define DIALOG_NEXTRUN_CM_CLASS "dialog-nextrun"

static short module = MOD_SX;

typedef struct _toCreateTransaction {
        SchedXaction        *sx;
        GDate                *date;

        gint                clistRow;
} toCreateTransaction;

typedef struct _sxSinceLastData {
        GtkWidget                                *nextrunDlg;
        GladeXML                                *gxml;
        GList /* <toCreateTransaction*> */        *transList;
} sxSinceLastData;

static void nextrun_init( sxSinceLastData *sxsld );
static void nextrun_close_handler( gpointer ud );
static void nr_ok_clicked( GtkButton *b, gpointer ud );
static void nr_next_clicked( GtkButton *b, gpointer ud );
static void nr_prev_xaction_clicked( GtkButton *b, gpointer ud );
static void nr_next_xaction_clicked( GtkButton *b, gpointer ud );

static void nextrun_destroy( GtkObject *o, gpointer ud );

static void slr_create_transactions( SchedXaction *sx, GDate *gd );

//int parse_vars_from_formula( const char *formula, GHashTable *varHash );

void
gnc_ui_nextrun_dialog_create(void)
{
        sxSinceLastData        *sxsld = g_new0( sxSinceLastData, 1 );
        sxsld->gxml = gnc_glade_xml_new( "sched-xact.glade", "Since-Last-Run Instantiation" );
        sxsld->nextrunDlg = glade_xml_get_widget( sxsld->gxml, "Since-Last-Run Instantiation" );

        nextrun_init( sxsld );
}

static void
nextrun_init( sxSinceLastData *sxsld )
{
        GtkWidget        *o;
        int                i;
        struct widgetNameSignalHandlerTuple {
                char *name;
                char *signal;
                void (*handlerFn)();
        } widgets[] = {
                { "ok",           "clicked", nr_ok_clicked },
                { "next",         "clicked", nr_next_clicked },
                { "prev_xaction", "clicked", nr_prev_xaction_clicked },
                { "next_xaction", "clicked", nr_next_xaction_clicked },
                { NULL,           NULL,      NULL }
        };

        gnc_register_gui_component( DIALOG_NEXTRUN_CM_CLASS,
                                    NULL,
                                    nextrun_close_handler,
                                    sxsld->nextrunDlg );
        
        gtk_signal_connect( GTK_OBJECT(sxsld->nextrunDlg), "destroy",
                            GTK_SIGNAL_FUNC( nextrun_destroy ), sxsld );

        for ( i=0; widgets[i].name != NULL ; i++ ) {
                o = glade_xml_get_widget( sxsld->gxml, widgets[i].name );
                gtk_signal_connect( GTK_OBJECT(o), widgets[i].signal,
                                    GTK_SIGNAL_FUNC(widgets[i].handlerFn),
                                    sxsld );
                
        }

        o = glade_xml_get_widget( sxsld->gxml, "next" );
        gtk_signal_connect( GTK_OBJECT(o), "clicked",
                            GTK_SIGNAL_FUNC(nr_next_clicked),
                            sxsld );

        o = glade_xml_get_widget( sxsld->gxml, "prev_xaction" );
        gtk_signal_connect( GTK_OBJECT(o), "clicked",
                            GTK_SIGNAL_FUNC(nr_prev_xaction_clicked),
                            sxsld );
        o = glade_xml_get_widget( sxsld->gxml, "next_xaction" );
        gtk_signal_connect( GTK_OBJECT(o), "clicked",
                            GTK_SIGNAL_FUNC(nr_next_xaction_clicked),
                            sxsld );

        gtk_widget_show_all( sxsld->nextrunDlg );
}

static void
nextrun_close_handler( gpointer ud )
{
        gnome_dialog_close( GNOME_DIALOG( ((sxSinceLastData*)ud)->nextrunDlg ) );
}

static void
nr_ok_clicked( GtkButton *b, gpointer ud )
{
        sxSinceLastData                *sxsld;
        GList                        *tctList;
        toCreateTransaction        *tct;

        sxsld = (sxSinceLastData*)ud;

        tctList = sxsld->transList;
        if ( tctList == NULL ) {
                PERR( "no transactions to create\n" );
        }
        do {
                tct = (toCreateTransaction*)tctList->data;
                slr_create_transactions( tct->sx, tct->date );
        } while ( (tctList = tctList->next) );

        nextrun_close_handler( ud );
}

static void
free_elts( gpointer data, gpointer user_data )
{
        g_free( data );
}

static void
nr_next_clicked( GtkButton *b, gpointer ud )
{
        sxSinceLastData *sxsld;
        GtkWidget        *dlg;
        GtkWidget        *o;
        GtkCList        *cl;
        time_t                gdeDate;
        GList                *sxList;
        GNCBook                *book;
        SchedXaction        *sx;
        GDate                gd, *endDate;
        gchar                buf[1024];
        gint                row;
        char                *rowText[2];
        toCreateTransaction        *tct;

        sxsld = (sxSinceLastData*)ud;
        o = glade_xml_get_widget( sxsld->gxml, "next_date" );
        gdeDate = gnome_date_edit_get_date( GNOME_DATE_EDIT(o) );

        DEBUG( "Okay... I should run with a date of: %s", ctime(&gdeDate) );

        // destroy the previous transactions
        // destroy all the toCreateTransactions
        if ( sxsld->transList != NULL ) {
                g_list_foreach( sxsld->transList, free_elts, NULL );
                g_list_free( sxsld->transList );
                sxsld->transList = NULL;
        }

        book = gncGetCurrentBook();
        sxList = gnc_book_get_schedxactions( book );
        if ( sxList == NULL ) {
                PERR( "No scheduled transactions to play with\n" );
                return;
        }
        
        endDate = g_date_new();
        g_date_set_time( endDate, gdeDate );

        o = glade_xml_get_widget( sxsld->gxml, "replace_with_register" );
        cl = GTK_CLIST(o);
        gtk_clist_clear( cl );

        gd = *g_date_new();
        row = 0;
        do {
                sx = (SchedXaction*)sxList->data;
                g_date_set_time( &gd, time(NULL) );
                while ( g_date_compare( &gd, endDate ) <= 0 ) {
                        g_date_strftime( buf, 1023, "%c", &gd );
                        // add to clist [ahem... register... ahem]
                        rowText[0] = xaccSchedXactionGetName( sx );
                        rowText[1] = malloc( sizeof(char) * 256 ); // FIXME
                        g_date_strftime( rowText[1], 255, "%c", &gd );

                        tct = g_new0( toCreateTransaction, 1 );
                        tct->sx = sx;
                        tct->date = g_date_new();
                        *tct->date = gd;
                        tct->clistRow = row;
                        sxsld->transList = g_list_append( sxsld->transList, tct );

                        gtk_clist_insert( cl, row, rowText );
                        row += 1;

                        gd = xaccSchedXactionGetInstanceAfter( sx, &gd );
                }
        } while ( (sxList = sxList->next) );

#if 0
        sxList = sxsld->transList;
        do {
                char buf[128];
                g_date_strftime( buf, 127, "%c",
                                 ((toCreateTransaction*)sxList->data)->date );
                DEBUG( "List contains sx \"%s\" on date \"%s\"\n",
                        xaccSchedXactionGetName( ((toCreateTransaction*)sxList->data)->sx ),
                        buf );
        } while ( (sxList = sxList->next) );
#endif // 0
}

static void
nextrun_destroy( GtkObject *o, gpointer ud )
{
        DEBUG( "nuttin' doin...\n" );
}

static void
nr_prev_xaction_clicked( GtkButton *b, gpointer ud )
{
}

static void
nr_next_xaction_clicked( GtkButton *b, gpointer ud )
{
}

static gboolean
create_each_transaction( Transaction *t, void *d )
{
        Transaction        *newT;
        GDate                *gd;
        GList                *sList;
        GList                *osList;
        Split                *split;
        kvp_frame        *split_kvpf;
        kvp_value        *kvp_val;
        gboolean        errFlag;

        errFlag = FALSE;

        DEBUG( "I'm seeing Transaction \"%s\"\n",
                xaccTransGetDescription( t ) );

        gd = (GDate*)d;

        newT = xaccMallocTransaction();
        xaccTransBeginEdit( newT );
        // the action and description/memo are in the template
        gnc_copy_trans_onto_trans( t, newT, FALSE, FALSE );

        // the date is new [gd]
        xaccTransSetDate( newT,
                          g_date_day( gd ),
                          g_date_month( gd ),
                          g_date_year( gd ) );

        // the accounts and amounts are in the kvp_frames of the splits.
        osList = xaccTransGetSplitList( t );
        sList = xaccTransGetSplitList( newT );
        if ( (osList == NULL) || (sList == NULL) ) {
                PERR( "\tseen transaction w/o splits. :(\n" );
                return FALSE;
        }
        do {
                split = (Split*)sList->data;
                // Ick.  This assumes that the split lists will be
                // ordered identically. :( I think it's fair to say
                // they will, but I'd rather not have to count on
                // it. --jsled
                split_kvpf = xaccSplitGetSlots( (Split*)osList->data );

                DEBUG( "\tProcessing Split \"%s\"\n",
                        xaccSplitGetMemo( split ) );

                DEBUG( "\tkvp_frame: %s\n",
                        kvp_frame_to_string( split_kvpf ) );


                // from-transaction of splits
                {
                        GUID                *acct_guid;
                        Account                *acct;
                        // contains the guid of the split's actual account.
                        kvp_val = kvp_frame_get_slot( split_kvpf, "sched-xaction/xfrm" );
                        if ( kvp_val == NULL ) {
                                PERR( "Null kvp_val for xfrm\n" );
                        }
                        acct_guid = kvp_value_get_guid( kvp_val );
                        acct = xaccAccountLookup( acct_guid );
                        DEBUG( "Got account with name \"%s\"\n",
                                xaccAccountGetName( acct ) );
                        //xaccSplitSetAccount( split, acct );
                        xaccAccountInsertSplit( acct, split );
                }
                // credit/debit formulas
                {
                        char                *str;
                        gnc_numeric        credit_num;
                        gnc_numeric        debit_num;
                        gnc_numeric        final;
                        int                gncn_error;

                        kvp_val = kvp_frame_get_slot( split_kvpf, "sched-xaction/credit_formula" );
                        str = kvp_value_get_string( kvp_val );
                        credit_num = gnc_numeric_create( 0, 1 );
                        if ( str != NULL ) {
                                
                                printf( "---------------\n" );
                                printf( "Parsing formula:\n" );
                                //parse_vars_from_formula( str, NULL );
                                printf( "---------------\n" );

                                xaccParseAmount( str, TRUE, &credit_num, NULL );
                                //string_to_gnc_numeric( str, &credit_num );
                                printf( "gnc_numeric::credit: \"%s\" -> \"%s\"\n",
                                        str, gnc_numeric_to_string( credit_num ) );
                        }
                        
                        kvp_val = kvp_frame_get_slot( split_kvpf, "sched-xaction/debit_formula" );
                        str = kvp_value_get_string( kvp_val );

                        debit_num = gnc_numeric_create( 0, 1 );
                        if ( str != NULL ) {

                                printf( "---------------\n" );
                                printf( "Parsing formula:\n" );
                                //parse_vars_from_formula( str, NULL );
                                printf( "---------------\n" );

                                xaccParseAmount( str, TRUE, &debit_num, NULL );
                                //string_to_gnc_numeric( str, &debit_num );
                                printf( "gnc_numeric::debit: \"%s\" -> \"%s\"\n",
                                        str, gnc_numeric_to_string( debit_num ) );
                        }
                        
                        final = gnc_numeric_sub_fixed( debit_num,
                                                       credit_num );
                        
                        gncn_error = gnc_numeric_check( final );
                        if ( gncn_error != GNC_ERROR_OK ) {
                                printf( "Error %d in final gnc_numeric value\n", gncn_error );
                                errFlag = TRUE;
                                break;
                        }
                        printf( "gnc_numeric::final: \"%s\"\n",
                                gnc_numeric_to_string( final ) );
                        xaccSplitSetValue( split, final );
                }
#if 0
                kvp_val = kvp_frame_get_slot( split_kvpf, "sched-xaction/shares" );
                kvp_val = kvp_frame_get_slot( split_kvpf, "sched-xaction/amnt" );
#endif // 0
        } while ( (sList = sList->next) && (osList = osList->next) );

        if ( errFlag ) {
                printf( "Some error in newT creation\n" );
                xaccTransRollbackEdit( newT );
        } else {
                xaccTransCommitEdit( newT );
        }

        return TRUE;
        
}

static void
slr_create_transactions( SchedXaction *sx, GDate *gd )
{
        AccountGroup        *ag;
        Account                *acct;
        char                *id;

        // get template account group
        ag = gnc_book_get_template_group( gncGetCurrentBook() );
        id = guid_to_string( xaccSchedXactionGetGUID(sx) );
        acct = xaccGetAccountFromName( ag, id );
        printf( "Got account \"%s\"\n",
                xaccAccountGetName( acct ) );
        g_free( id );

        xaccAccountForEachTransaction( acct,
                                       create_each_transaction,
                                       gd );

}

#if 0

/**
 * Parses in-fix mathematical formulas using the standard operators
 * [+-/%*], and '(', ')' grouping.
 *
 * Any strings are placed in the GHashTable as variables. The value of
 * each key in the hash-table is a struct of the form:
 * { const char *varStr;
 *   gint       idx;
 *   gint       len; };
 *
 **/
int
parse_vars_from_formula( const char *formula, GHashTable *varHash )
{
        gnc_numeric num_foo;
        char        *foo;
        GList        *list;

        gnc_exp_parser_init();
        if ( ! gnc_exp_parser_parse( formula, &num_foo, &foo ) ) {
                printf( "Error parsing at \"%s\": %s\n",
                        foo, gnc_exp_parser_error_string() );
        }
        printf( "Successful parse...\n" );
        list = gnc_exp_parser_get_variable_names();
        if ( list == NULL ) {
                printf( "NULL variable list\n" );
        } else {
                do {
                        printf( "Variable \"%s\"\n",
                                list->data );
                } while ( (list = list->next) );
        }
        
        gnc_exp_parser_shutdown();
}
#endif // 0

#if 0
        GScanner        *varScanner;
        GTokenType        tok;

        varScanner = g_scanner_new( NULL );
        g_scanner_set_scope( varScanner, 0 );
        g_scanner_freeze_symbol_table( varScanner );
        g_scanner_scope_add_symbol( varScanner, 0, "(", (gpointer)"left-paren" );
        g_scanner_scope_add_symbol( varScanner, 0, ")", (gpointer)"right-paren" );
        g_scanner_scope_add_symbol( varScanner, 0, "+", (gpointer)"plus" );
        g_scanner_scope_add_symbol( varScanner, 0, "-", (gpointer)"minus" );
        g_scanner_scope_add_symbol( varScanner, 0, "/", (gpointer)"div" );
        g_scanner_scope_add_symbol( varScanner, 0, "*", (gpointer)"mult" );
        g_scanner_thaw_symbol_table( varScanner );
        
        g_scanner_input_text( varScanner, formula, strlen( formula ) );
        
        do {
                tok = g_scanner_get_next_token( varScanner );
                printf( "tok: " );
                switch ( varScanner->token ) {
                case G_TOKEN_EOF:
                        printf( "EOF" ); break;
                case G_TOKEN_LEFT_PAREN:
                        printf( "(" ); break;
                case G_TOKEN_RIGHT_PAREN:
                        printf( ")" ); break;
                case G_TOKEN_LEFT_CURLY:
                        printf( "{" ); break;
                case G_TOKEN_RIGHT_CURLY:
                        printf( "}" ); break;
                case G_TOKEN_LEFT_BRACE:
                        printf( "[" ); break;
                case G_TOKEN_RIGHT_BRACE:
                        printf( "]" ); break;
                case G_TOKEN_EQUAL_SIGN:
                        printf( "=" ); break;
                case G_TOKEN_COMMA:
                        printf( "," ); break;
                case G_TOKEN_NONE:
                        printf( "NONE" ); break;
                case G_TOKEN_ERROR:
                        printf( "ERROR(%d)", varScanner->value.v_error ); break;
                case G_TOKEN_CHAR:
                        printf( "CHAR(%c)", varScanner->value.v_char ); break;
                case G_TOKEN_BINARY:
                        printf( "BINARY" ); break;
                case G_TOKEN_OCTAL:
                        printf( "OCTAL(%ul)", varScanner->value.v_octal ); break;
                case G_TOKEN_INT:
                        printf( "INT(%ul)", varScanner->value.v_int ); break;
                case G_TOKEN_HEX:
                        printf( "HEX(%ul)", varScanner->value.v_hex ); break;
                case G_TOKEN_FLOAT:
                        printf( "FLOAT(%f)", varScanner->value.v_float ); break;
                case G_TOKEN_STRING:
                        printf( "STRING(%s)", varScanner->value.v_string ); break;
                case G_TOKEN_SYMBOL:
                        printf( "SYMBOL(%s)", (gchar*)varScanner->value.v_symbol ); break;
                case G_TOKEN_IDENTIFIER:
                        printf( "IDENT(%s)", varScanner->value.v_identifier ); break;
                case G_TOKEN_IDENTIFIER_NULL:
                        printf( "NULL_IDENT" ); break;
                case G_TOKEN_COMMENT_SINGLE:
                case G_TOKEN_COMMENT_MULTI:
                        printf( "COMMENT(%s)", varScanner->value.v_comment ); break;
                case G_TOKEN_LAST:
                        printf( "END" ); break;
                default:
                        printf( "UNK" ); break;
                };
                printf( "\n" );
        } while ( (varScanner->token != G_TOKEN_LAST) &&
                  (varScanner->token != G_TOKEN_EOF) &&
                  (varScanner->token != G_TOKEN_NONE) );
        return 1;
        /*
"0.33 * ( base + ld ) + (0.25 * internet)":

tok: FLOAT(0.330000)
tok: UNK
tok: )
tok: IDENT(base)
tok: UNK
tok: IDENT(ld)
tok: (
tok: UNK
tok: )
tok: FLOAT(0.250000)
tok: UNK
tok: IDENT(internet)
tok: (
tok: EOF
        */
          
#endif // 0
