/********************************************************************\
 * druid-loan.c : A Gnome Druid for setting up loan-repayment       *
 *     scheduled transactions.                                      *
 * Copyright (C) 2002 Joshua Sled <jsled@asynchronous.org>          *
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
#include <string.h>
#include <gtk/gtk.h>
#include <glib.h>
#include <glade/glade.h>
#include <gnome.h>

#include "druid-loan.h"

#include "gnc-component-manager.h"
#include "dialog-utils.h"
#include "Account.h"
#include "FreqSpec.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "gnc-frequency.h"

#define DIALOG_LOAN_DRUID_CM_CLASS "druid-loan-setup"

#define SX_GLADE_FILE "sched-xact.glade"
#define LOAN_DRUID_WIN_GLADE_NAME "loan_druid_win"
#define LOAN_DRUID_GLADE_NAME     "loan_druid"

#define PG_INTRO "loan_intro_pg"
#define PG_INFO "loan_info_pg"
#  define PARAM_TABLE      "param_table"
#  define ORIG_PRINC_GNE   "orig_princ_gne"
#  define ORIG_PRINC_ENTRY "orig_princ_ent"
#  define CUR_PRINC_GNE    "cur_princ_gne"
#  define CUR_PRINC_ENTRY  "cur_princ_ent"
#  define IRATE_SPIN       "irate_spin"
#  define TYPE_OPT         "type_opt"
#  define VAR_CONTAINER    "type_freq_frame"
#  define START_DATE       "start_gde"
#  define LENGTH_SPIN      "len_spin"
#  define LENGTH_OPT       "len_opt"
#  define REMAIN_SPIN      "rem_spin"
#define PG_OPTS "loan_opts_pg"
#  define OPT_CONTAINER        "opt_vbox"
#  define OPT_ESCROW           "opt_escrow"
#  define OPT_ESCROW_CONTAINER "opt_escrow_hbox"
#define PG_REPAYMENT "repayment_pg"
#  define TXN_NAME       "txn_title"
#  define REPAY_TABLE    "repay_table"
#  define AMOUNT_GNE     "amount_gne"
#  define AMOUNT_ENTRY   "amount_ent"
#  define REMAINDER_OPT  "remain_opt"
#  define FREQ_CONTAINER "freq_frame"
#define PG_PAYMENT "payment_pg"
#  define PAY_TXN_TITLE      "pay_txn_title"
#  define PAY_AMT_GNE        "pay_amt_gne"
#  define PAY_AMT_ENTRY      "pay_amt_ent"
#  define PAY_TABLE          "pay_table"
#  define PAY_TXN_PART_RB    "pay_txn_part_rb"
#  define PAY_UNIQ_FREQ_RB   "pay_uniq_freq_rb"
#  define PAY_FREQ_CONTAINER "pay_freq_align"
#define PG_FINISH "finish_pg"

#define OPT_VBOX_SPACING 2

/**
 * TODO/fixme:
 * . param account selection should fill in orig/cur principal amounts from
 *   the books.
 * . initialize type freq to monthly.
 * . if LoanType <- !FIXED
 *   . Frequency <- sensitive
 **/

/**
 * The data relating to a single "repayment option" -- a potential
 * [sub-]transaction in the repayment.
 **/
typedef struct RepayOptData_ {
        gboolean enabled;
        char *name; // { "insurance", "pmi", "taxes", ... }
        char *txnMemo;
        float amount;
        gboolean throughEscrowP;
        Account *to;
        Account *from; // If NULL { If throughEscrowP, then through escrowP;
                       //   else: undefined.
        FreqSpec *fs; // If NULL, part of repayment; otherwise: defined here.
        GDate *startDate;
} RepayOptData;

/**
 * The default repayment options data.
 **/
typedef struct RepayOptDataDefault_ {
        char *name;
        char *defaultTxnMemo;
        gboolean escrowDefault;
} RepayOptDataDefault;

static RepayOptDataDefault REPAY_DEFAULTS[] = {
     /* { name, default txn memo, throughEscrowP } */
     { "Taxes",         "Tax Payment",           FALSE },
     { "Insurance",     "Insurance Payment",     FALSE },
     { "PMI",           "PMI Payment",           FALSE },
     { "Other Expense", "Miscellaneous Payment", FALSE },
     { NULL }
};

/**
 * The UI-side storage of the repayment options.
 **/
typedef struct RepayOptUI_ {
        GtkCheckButton *optCb;
        GtkCheckButton *escrowCb;
        RepayOptData *optData;
} RepayOptUIData;

typedef enum {
        FIXED = 0,
        VARIABLE,
        VARIABLE_5_1 = VARIABLE,
        VARIABLE_7_1,
        VARIABLE_10_1,
        /* ... FIXME */
} LoanType;

typedef enum {
        MONTHS = 0,
        YEARS
} PeriodSize;

/**
 * Data about a loan repayment.
 **/
typedef struct LoanData_ {
        Account *primaryAcct;
        float principal;
        float principalLeft;
        float interestRate;
        LoanType type;
        FreqSpec *loanFreq;
        GDate *startDate;
        GDate *varStartDate;
        int numPer;
        int numPerRemain;
        PeriodSize perSize;

        int repayOptCount;
        RepayOptData **repayOpts;
} LoanData;

/**
 * The UI-side storage of the loan druid data.
 **/
typedef struct LoanDruidData_ {
        GladeXML *gxml;
        GtkWidget *dialog;
        GnomeDruid *druid;

        LoanData ld;
        /* The UI-side storage of repayment data; this is 1:1 with the array
         * in LoanData */
        RepayOptUIData **repayOptsUI;

        /* Current index of the payment opt for multiplexing the 'payment'
         * page. */
        int currentIdx;
        
        /* widgets */
        /* prm = params */
        GtkTable *prmTable;
        GnomeNumberEntry *prmOrigPrincGNE;
        GtkEntry *prmOrigPrincEntry;
        GnomeNumberEntry *prmCurPrincGNE;
        GtkEntry *prmCurPrincEntry;
        GtkSpinButton *prmIrateSpin;
        GtkOptionMenu *prmType;
        GtkFrame *prmVarFrame;
        GNCFrequency *prmVarGncFreq;
        GnomeDateEdit *prmStartDateGDE;
        GtkSpinButton *prmLengthSpin;
        GtkOptionMenu *prmLengthType;
        GtkSpinButton *prmRemainSpin;

        /* opt = options */
        GtkVBox *optVBox;
        GtkCheckButton *optEscrowOpt;
        GtkHBox *optEscrowHBox;

        /* rep = repayment */
        GtkEntry *repTxnName;
        GtkTable *repTable;
        GnomeNumberEntry *repAmtGNE;
        GtkEntry *repAmtEntry;
        GtkOptionMenu *repRemainderOpt;
        GtkFrame *repFreqFrame;
        GNCFrequency *repGncFreq;

        /* pay = payment[s] */
        GtkEntry *payTxnName;
        GnomeNumberEntry *payAmtGNE;
        GtkEntry *payAmtEntry;
        GtkTable *payTable;
        GtkRadioButton *payTxnFreqPartRb;
        GtkRadioButton *payTxnFreqUniqRb;
        GtkAlignment *payFreqAlign;
        GNCFrequency *payGncFreq;
} LoanDruidData;

static void gnc_loan_druid_data_init( LoanDruidData *ldd );
static void gnc_loan_druid_get_widgets( LoanDruidData *ldd );

static void ld_close_handler( LoanDruidData *ldd );
static void ld_destroy( GtkObject *o, gpointer ud );

static void ld_cancel_check( GnomeDruid *gd, LoanDruidData *ldd );

static void ld_opt_toggled( GtkToggleButton *tb, gpointer ud );
static void ld_opt_consistency( GtkToggleButton *tb, gpointer ud );
static void ld_escrow_tog( GtkToggleButton *tb, gpointer ud );

static void ld_pay_freq_toggle( GtkToggleButton *tb, gpointer ud );

static gboolean ld_info_save( GnomeDruidPage *gdp, gpointer arg1, gpointer ud );
static void     ld_info_prep( GnomeDruidPage *gdp, gpointer arg1, gpointer ud );
static gboolean ld_opts_next( GnomeDruidPage *gdp, gpointer arg1, gpointer ud );
static void     ld_opts_prep( GnomeDruidPage *gdp, gpointer arg1, gpointer ud );
static gboolean ld_rep_next ( GnomeDruidPage *gdp, gpointer arg1, gpointer ud );
static void     ld_rep_prep ( GnomeDruidPage *gdp, gpointer arg1, gpointer ud );
static gboolean ld_pay_next ( GnomeDruidPage *gdp, gpointer arg1, gpointer ud );
static void     ld_pay_prep ( GnomeDruidPage *gdp, gpointer arg1, gpointer ud );
static gboolean ld_pay_back ( GnomeDruidPage *gdp, gpointer arg1, gpointer ud );
static gboolean ld_fin_back ( GnomeDruidPage *gdp, gpointer arg1, gpointer ud );

struct LoanDruidData_*
gnc_ui_sx_loan_druid_create()
{
        static struct {
                char *pageName;
                gboolean (*nextFn)();
                void     (*prepFn)();
                gboolean (*backFn)();
                void     (*finishFn)();
                /* cancel is handled by the druid itself. */
        } DRUID_HANDLERS[] = {
                { PG_INFO,      ld_info_save, ld_info_prep, ld_info_save, NULL },
                { PG_OPTS,      ld_opts_next, ld_opts_prep, NULL,         NULL },
                { PG_REPAYMENT, ld_rep_next,  ld_rep_prep },
                { PG_PAYMENT,   ld_pay_next,  ld_pay_prep, ld_pay_back, NULL },
                { PG_FINISH,    NULL,         NULL,        ld_fin_back, NULL },
                { NULL }
        };
        int i;
        LoanDruidData *ldd;

        ldd = g_new0( LoanDruidData, 1 );

        gnc_loan_druid_data_init( ldd );

        ldd->gxml   = gnc_glade_xml_new( SX_GLADE_FILE, LOAN_DRUID_WIN_GLADE_NAME );
        ldd->dialog = glade_xml_get_widget( ldd->gxml, LOAN_DRUID_WIN_GLADE_NAME );
        ldd->druid  = GNOME_DRUID(glade_xml_get_widget( ldd->gxml,
                                                        LOAN_DRUID_GLADE_NAME ));

        /* get pointers to the various widgets */
        gnc_loan_druid_get_widgets( ldd );
        
        /* FIXME: non-gladeable widget setup */
        {
                {
                        GtkAdjustment *a;

                        /* 8.0 [%], range of 0.05..100.0 with ticks at 0.05[%]. */
                        a = GTK_ADJUSTMENT(gtk_adjustment_new( 8.0, 0.05,
                                                               100.0, 0.05,
                                                               1.0, 1.0 ));
                        gtk_spin_button_set_adjustment( ldd->prmIrateSpin, a );
                        gtk_spin_button_set_value( ldd->prmIrateSpin, 8.00 );
                        gtk_spin_button_set_snap_to_ticks( ldd->prmIrateSpin,
                                                           TRUE );

                        a = GTK_ADJUSTMENT(gtk_adjustment_new( 360, 1,
                                                               9999, 1,
                                                               12, 12 ));
                        gtk_spin_button_set_adjustment( ldd->prmLengthSpin, a );

                        a = GTK_ADJUSTMENT(gtk_adjustment_new( 360, 1,
                                                               9999, 1,
                                                               12, 12 ));
                        gtk_spin_button_set_adjustment( ldd->prmRemainSpin, a );
                }
               
                gnc_option_menu_init( GTK_WIDGET(ldd->prmType) );
                gnc_option_menu_init( GTK_WIDGET(ldd->prmLengthType) );
                gnc_option_menu_init( GTK_WIDGET(ldd->repRemainderOpt) );

                /* PARAM_TABLE(0,1) += account sel */
                /* OPT_ESCROW_CONTAINER += account sel */
                {
                }

                /* OPT_CONTAINER += option options. */
                /* . Each RepayOpt gets an entry in the optContainer.
                 * . Each "entry" is a 2-line vbox containing:
                 *   . The checkbox for the option itself
                 *   . an alignment-contained sub-checkbox for "through the
                 *     escrow account".
                 *   . Hook up each to bit-twiddling the appropriate line.
                 */

                /* FIXME : too deep, factor out. */
                {
                        RepayOptUIData *rouid;
                        GtkVBox *vb;
                        GtkAlignment *optAlign, *subOptAlign;
                        GString *str;

                        str = g_string_sized_new( 32 );

                        for ( i=0; i<ldd->ld.repayOptCount; i++ ) {
                                rouid = ldd->repayOptsUI[i];
                                vb = GTK_VBOX(gtk_vbox_new( FALSE, OPT_VBOX_SPACING ));

                                /* Add payment checkbox. */
                                g_string_sprintf( str, "... pay \"%s\"?", rouid->optData->name );
                                rouid->optCb =
                                        GTK_CHECK_BUTTON(gtk_check_button_new_with_label( str->str ));
                                gtk_box_pack_start( GTK_BOX(vb), GTK_WIDGET(rouid->optCb),
                                                    FALSE, FALSE, 2 );
                                rouid->escrowCb =
                                        GTK_CHECK_BUTTON(gtk_check_button_new_with_label( _("via Escrow account?") ));
                                gtk_widget_set_sensitive( GTK_WIDGET(rouid->escrowCb), FALSE );
                                subOptAlign = GTK_ALIGNMENT(gtk_alignment_new( 0.5, 0.5, 0.75, 1.0 ));
                                gtk_container_add( GTK_CONTAINER(subOptAlign),
                                                   GTK_WIDGET(rouid->escrowCb) );
                                gtk_box_pack_start( GTK_BOX(vb), GTK_WIDGET(subOptAlign),
                                                    FALSE, FALSE, 2 );

                                gtk_signal_connect( GTK_OBJECT( rouid->optCb ),
                                                    "toggled",
                                                    GTK_SIGNAL_FUNC(ld_opt_toggled),
                                                    (gpointer)rouid );
                                gtk_signal_connect( GTK_OBJECT( rouid->optCb ),
                                                    "toggled",
                                                    GTK_SIGNAL_FUNC(ld_opt_consistency),
                                                    (gpointer)rouid->escrowCb );
                                gtk_signal_connect( GTK_OBJECT( rouid->escrowCb ),
                                                    "toggled",
                                                    GTK_SIGNAL_FUNC(ld_escrow_tog),
                                                    (gpointer)rouid );

                                optAlign = GTK_ALIGNMENT(gtk_alignment_new( 0.5, 0.5, 0.75, 1.0 ));
                                gtk_container_add( GTK_CONTAINER(optAlign),
                                                   GTK_WIDGET(vb) );
                                gtk_box_pack_start( GTK_BOX(ldd->optVBox), GTK_WIDGET(optAlign),
                                                    FALSE, FALSE, 2 );
                                gtk_widget_show_all( GTK_WIDGET(optAlign) );
                        }
                        g_string_free( str, TRUE );
                }

                gtk_signal_connect( GTK_OBJECT(ldd->payTxnFreqPartRb), "toggled",
                                    GTK_SIGNAL_FUNC(ld_pay_freq_toggle), (gpointer)ldd );
                gtk_signal_connect( GTK_OBJECT(ldd->payTxnFreqUniqRb), "toggled",
                                    GTK_SIGNAL_FUNC(ld_pay_freq_toggle), (gpointer)ldd );

                ldd->prmVarGncFreq =
                        GNC_FREQUENCY(gnc_frequency_new( NULL, NULL ));
                gtk_container_add( GTK_CONTAINER(ldd->prmVarFrame),
                                   GTK_WIDGET(ldd->prmVarGncFreq) );

                ldd->repGncFreq =
                        GNC_FREQUENCY(gnc_frequency_new( NULL, NULL ));
                gtk_container_add( GTK_CONTAINER(ldd->repFreqFrame),
                                   GTK_WIDGET(ldd->repGncFreq) );

                ldd->payGncFreq =
                        GNC_FREQUENCY(gnc_frequency_new( NULL, NULL ));
                gtk_container_add( GTK_CONTAINER(ldd->payFreqAlign),
                                   GTK_WIDGET(ldd->payGncFreq) );
                /* FIXME: Repayment/Payment[s] pages += account sel */
        }

        gnc_register_gui_component( DIALOG_LOAN_DRUID_CM_CLASS,
                                    NULL, /* no refresh handler */
                                    ld_close_handler,
                                    ldd );
        gtk_signal_connect( GTK_OBJECT(ldd->dialog), "destroy",
                            GTK_SIGNAL_FUNC(ld_destroy),
                            ldd );

        /* setup page-transition handlers */
        /* setup druid-global handler for cancel */
        gtk_signal_connect( GTK_OBJECT(ldd->druid), "cancel",
                            GTK_SIGNAL_FUNC(ld_cancel_check),
                            (gpointer)ldd );
        /* FIXME: this is substantially similar to the code in
         * dialog-sxsincelast.c ... it should probably be factored out
         * somewhere. */
        for ( i=0; DRUID_HANDLERS[i].pageName != NULL; i++ ) {
                GtkWidget *pg;

                pg = glade_xml_get_widget( ldd->gxml,
                                           DRUID_HANDLERS[i].pageName );
                if ( DRUID_HANDLERS[i].prepFn ) {
                        gtk_signal_connect( GTK_OBJECT(pg), "prepare",
                                            GTK_SIGNAL_FUNC(DRUID_HANDLERS[i].
                                                            prepFn),
                                            ldd);
                }
                if ( DRUID_HANDLERS[i].backFn ) {
                        gtk_signal_connect( GTK_OBJECT(pg), "back",
                                            GTK_SIGNAL_FUNC(DRUID_HANDLERS[i].
                                                            backFn),
                                            ldd);
                }
                if ( DRUID_HANDLERS[i].nextFn ) {
                        gtk_signal_connect( GTK_OBJECT(pg), "next",
                                            GTK_SIGNAL_FUNC(DRUID_HANDLERS[i].
                                                            nextFn),
                                            ldd);
                }
                if ( DRUID_HANDLERS[i].finishFn ) {
                        gtk_signal_connect( GTK_OBJECT(pg), "finish",
                                            GTK_SIGNAL_FUNC(DRUID_HANDLERS[i].
                                                            finishFn),
                                            ldd);
                }
        }

        gtk_widget_show_all( ldd->dialog );
        return ldd;
}

static
void
gnc_loan_druid_data_init( LoanDruidData *ldd )
{
        int i, optCount;
        RepayOptData *optData;

        /* get the count of repayment defaults. */
        for ( optCount=0; REPAY_DEFAULTS[optCount].name != NULL; optCount++ )
                ;

        ldd->currentIdx = -1;

        ldd->ld.startDate = g_date_new();
        ldd->ld.varStartDate = g_date_new();
        g_date_set_time( ldd->ld.startDate, time(NULL) );
        ldd->ld.loanFreq = xaccFreqSpecMalloc( gnc_get_current_book() );
        ldd->ld.repayOptCount = optCount;
        ldd->ld.repayOpts = g_new0( RepayOptData*, optCount );
        /* copy all the default lines into the LDD */
        ldd->repayOptsUI = g_new0( RepayOptUIData*, optCount );
        for ( i=0; i < optCount; i++ ) {
                g_assert( REPAY_DEFAULTS[i].name != NULL );

                ldd->repayOptsUI[i] = g_new0( RepayOptUIData, 1 );
                optData = ldd->ld.repayOpts[i]
                        = ldd->repayOptsUI[i]->optData
                        = g_new0( RepayOptData, 1 );

                optData->enabled        = FALSE;
                optData->name           = g_strdup( REPAY_DEFAULTS[i].name );
                optData->txnMemo        = g_strdup( REPAY_DEFAULTS[i].
                                                    defaultTxnMemo );
                optData->amount         = 0.0;
                optData->throughEscrowP = REPAY_DEFAULTS[i].escrowDefault;
                optData->fs             = NULL;
                optData->startDate      = NULL;
        }
}

static
void
gnc_loan_druid_get_widgets( LoanDruidData *ldd )
{
        g_assert( ldd != NULL );
        g_assert( ldd->gxml != NULL );

        /* Get all widgets */

#define GET_CASTED_WIDGET( cast, name ) \
	(cast( glade_xml_get_widget( ldd->gxml, name ) ))

        /* prm = params */
        ldd->prmTable =
                GET_CASTED_WIDGET( GTK_TABLE,          PARAM_TABLE );
        ldd->prmOrigPrincGNE =
                GET_CASTED_WIDGET( GNOME_NUMBER_ENTRY, ORIG_PRINC_GNE );
        ldd->prmOrigPrincEntry =
                GET_CASTED_WIDGET( GTK_ENTRY,          ORIG_PRINC_ENTRY );
        ldd->prmCurPrincGNE =
                GET_CASTED_WIDGET( GNOME_NUMBER_ENTRY, CUR_PRINC_GNE );
        ldd->prmCurPrincEntry =
                GET_CASTED_WIDGET( GTK_ENTRY,          CUR_PRINC_ENTRY );
        ldd->prmIrateSpin =
                GET_CASTED_WIDGET( GTK_SPIN_BUTTON,    IRATE_SPIN );
        ldd->prmType =
                GET_CASTED_WIDGET( GTK_OPTION_MENU,    TYPE_OPT );
        ldd->prmVarFrame =
                GET_CASTED_WIDGET( GTK_FRAME,          VAR_CONTAINER );
        ldd->prmStartDateGDE =
                GET_CASTED_WIDGET( GNOME_DATE_EDIT,    START_DATE );
        ldd->prmLengthSpin =
                GET_CASTED_WIDGET( GTK_SPIN_BUTTON,    LENGTH_SPIN );
        ldd->prmLengthType =
                GET_CASTED_WIDGET( GTK_OPTION_MENU,    LENGTH_OPT );
        ldd->prmRemainSpin =
                GET_CASTED_WIDGET( GTK_SPIN_BUTTON,    REMAIN_SPIN );
        
        /* opt = options */
        ldd->optVBox =
                GET_CASTED_WIDGET( GTK_VBOX,         OPT_CONTAINER );
        ldd->optEscrowOpt =
                GET_CASTED_WIDGET( GTK_CHECK_BUTTON, OPT_ESCROW );
        ldd->optEscrowHBox =
                GET_CASTED_WIDGET( GTK_HBOX,         OPT_ESCROW_CONTAINER );

        /* rep = repayment */
        ldd->repTxnName =
                GET_CASTED_WIDGET( GTK_ENTRY,          TXN_NAME );
        ldd->repTable =
                GET_CASTED_WIDGET( GTK_TABLE,          REPAY_TABLE );
        ldd->repAmtGNE =
                GET_CASTED_WIDGET( GNOME_NUMBER_ENTRY, AMOUNT_GNE );
        ldd->repAmtEntry =
                GET_CASTED_WIDGET( GTK_ENTRY,          AMOUNT_ENTRY );
        ldd->repRemainderOpt =
                GET_CASTED_WIDGET( GTK_OPTION_MENU,    REMAINDER_OPT );
        ldd->repFreqFrame =
                GET_CASTED_WIDGET( GTK_FRAME,          FREQ_CONTAINER );

        /* pay = payment[s] */
        ldd->payTxnName =
                GET_CASTED_WIDGET( GTK_ENTRY,          PAY_TXN_TITLE );
        ldd->payAmtGNE =
                GET_CASTED_WIDGET( GNOME_NUMBER_ENTRY, PAY_AMT_GNE );
        ldd->payAmtEntry =
                GET_CASTED_WIDGET( GTK_ENTRY,          PAY_AMT_ENTRY );
        ldd->payTable =
                GET_CASTED_WIDGET( GTK_TABLE,          PAY_TABLE );
        ldd->payTxnFreqPartRb =
                GET_CASTED_WIDGET( GTK_RADIO_BUTTON,   PAY_TXN_PART_RB );
        ldd->payTxnFreqUniqRb =
                GET_CASTED_WIDGET( GTK_RADIO_BUTTON,   PAY_UNIQ_FREQ_RB );
        ldd->payFreqAlign =
                GET_CASTED_WIDGET( GTK_ALIGNMENT,      PAY_FREQ_CONTAINER );
}

static
void
ld_close_handler( LoanDruidData *ldd )
{
        gtk_widget_hide( ldd->dialog );
        gtk_widget_destroy( ldd->dialog );
}

static
void
ld_destroy( GtkObject *o, gpointer ud )
{
        LoanDruidData *ldd;

        ldd = (LoanDruidData*)ud;

        g_assert( ldd );

        gnc_unregister_gui_component_by_data
          (DIALOG_LOAN_DRUID_CM_CLASS, ldd);

        /* FIXME: free alloc'd mem; cleanup */

        g_free( ldd );
}


static
void
ld_cancel_check( GnomeDruid *gd, LoanDruidData *ldd )
{
        const char *cancelMsg = _( "Are you sure you want to close "
                                   "the Mortgage/Loan Setup Druid?" );
        if ( gnc_verify_dialog_parented( ldd->dialog, FALSE, cancelMsg ) ) {
                gnc_close_gui_component_by_data( DIALOG_LOAN_DRUID_CM_CLASS,
                                                 ldd );
        }
}

static
void
ld_opt_toggled( GtkToggleButton *tb, gpointer ud )
{
        RepayOptUIData *rouid;

        rouid = (RepayOptUIData*)ud;
        rouid->optData->enabled = gtk_toggle_button_get_active(tb);
}

static
void
ld_opt_consistency( GtkToggleButton *tb, gpointer ud )
{
        GtkToggleButton *escrowCb;

        escrowCb = GTK_TOGGLE_BUTTON(ud);
        /* make sure the escrow option is only selected if we're active. */
        gtk_toggle_button_set_state( escrowCb,
                                     gtk_toggle_button_get_active(escrowCb)
                                     & gtk_toggle_button_get_active(tb) );
        /* make sure the escrow option is only sensitive if we're active. */
        gtk_widget_set_sensitive( GTK_WIDGET(escrowCb),
                                  gtk_toggle_button_get_active(tb) );
}

static
void
ld_escrow_tog( GtkToggleButton *tb, gpointer ud )
{
        RepayOptUIData *rouid;

        rouid = (RepayOptUIData*)ud;
        rouid->optData->throughEscrowP = gtk_toggle_button_get_active( tb );
}

static
gboolean
ld_info_save( GnomeDruidPage *gdp, gpointer arg1, gpointer ud )
{
        LoanDruidData *ldd;
        float amt;
        gchar *txt;

        ldd = (LoanDruidData*)ud;

        /* FIXME: account */
        txt = gtk_editable_get_chars( GTK_EDITABLE(ldd->prmOrigPrincEntry),
                                      0, -1 );
        amt = -1.0;
        amt = (float)strtod( txt, NULL );
        if ( amt < 0 ) {
                gnc_error_dialog( _("The original principal must "
                                    "be a valid number.") );
                return TRUE;
        }
        ldd->ld.principal = amt;
        g_free( txt );
        txt = gtk_editable_get_chars( GTK_EDITABLE(ldd->prmCurPrincEntry),
                                      0, -1 );
        amt = -1.0;
        amt = (float)strtod( txt, NULL );
        if ( amt < 0 ) {
                gnc_error_dialog( _("The current principal must "
                                    "be a valid number.") );
                return TRUE;
        }
        ldd->ld.principalLeft = amt;
        ldd->ld.interestRate =
                gtk_spin_button_get_value_as_float( ldd->prmIrateSpin );
        ldd->ld.type = gnc_option_menu_get_active( GTK_WIDGET(ldd->prmType) );
        if ( ldd->ld.type != FIXED ) {
                gnc_frequency_save_state( ldd->prmVarGncFreq,
                                          ldd->ld.loanFreq,
                                          ldd->ld.varStartDate );
        }

        /* start date */
        {
                time_t tmpTT;
                struct tm *tmpTm;

                tmpTT = gnome_date_edit_get_date( ldd->prmStartDateGDE );
                tmpTm = localtime( &tmpTT );
                g_date_set_dmy( ldd->ld.startDate,
                                1, (tmpTm->tm_mon+1), tmpTm->tm_mday );
        }

        /* len / periods */
        {
                ldd->ld.perSize =
                        (gnc_option_menu_get_active( GTK_WIDGET(ldd->prmLengthType) )
                         == MONTHS) ? MONTHS : YEARS;
                ldd->ld.numPer =
                        gtk_spin_button_get_value_as_int( ldd->prmLengthSpin );
                ldd->ld.numPerRemain =
                        gtk_spin_button_get_value_as_int( ldd->prmRemainSpin );
        }
        return FALSE;
}

static
void
ld_info_prep( GnomeDruidPage *gdp, gpointer arg1, gpointer ud )
{
        LoanDruidData *ldd;
        GString *str;

        ldd = (LoanDruidData*)ud;
        str = g_string_sized_new( 16 );
        /* FIXME: account. */
        g_string_sprintf( str, "%0.2f", ldd->ld.principal );
        gtk_entry_set_text( ldd->prmOrigPrincEntry, str->str );
        g_string_sprintf( str, "%0.2f", ldd->ld.principalLeft );
        gtk_entry_set_text( ldd->prmCurPrincEntry, str->str );
        gtk_spin_button_set_value( ldd->prmIrateSpin, ldd->ld.interestRate );
        gtk_option_menu_set_history( ldd->prmType, ldd->ld.type );
        if ( ldd->ld.type != FIXED ) {
                gnc_frequency_setup( ldd->prmVarGncFreq,
                                     ldd->ld.loanFreq,
                                     ldd->ld.varStartDate );
        }

        /* start date */
        {
                struct tm *tmpTm;

                tmpTm = g_new0( struct tm, 1 );

                g_date_to_struct_tm( ldd->ld.startDate, tmpTm );
                gnome_date_edit_set_time( ldd->prmStartDateGDE,
                                          mktime(tmpTm) );
                g_free( tmpTm );
        }

        /* length: total and remaining */
        {
                // prmLengthSpin
                gtk_spin_button_set_value( ldd->prmLengthSpin, ldd->ld.numPer );
                // prmLengthType
                gtk_option_menu_set_history( ldd->prmLengthType, ldd->ld.perSize );
                // prmRemainSpin
                gtk_spin_button_set_value( ldd->prmRemainSpin, ldd->ld.numPerRemain );
        }

        g_string_free( str, TRUE );
}

static
gboolean
ld_opts_next( GnomeDruidPage *gdp, gpointer arg1, gpointer ud )
{
        /* FIXME: do anything here? */
        return FALSE;
}

static
void
ld_opts_prep( GnomeDruidPage *gdp, gpointer arg1, gpointer ud )
{
        /* FIXME: setup the options */
}

static
gboolean
ld_rep_next( GnomeDruidPage *gdp, gpointer arg1, gpointer ud )
{
        LoanDruidData *ldd;

        ldd = (LoanDruidData*)ud;
        if ( (ldd->currentIdx < 0)
             || (ldd->currentIdx >= ldd->ld.repayOptCount)
             || !ldd->ld.repayOpts[ldd->currentIdx]->enabled ) {
                int i;
                for ( i=0;
                      (i < ldd->ld.repayOptCount)
                      && !ldd->ld.repayOpts[i]->enabled;
                      i++ )
                        ;
                if ( i == ldd->ld.repayOptCount ) {
                        /* transition to final page. */
                        GtkWidget *pg;
                        pg = glade_xml_get_widget( ldd->gxml, PG_FINISH );
                        gnome_druid_set_page( ldd->druid, GNOME_DRUID_PAGE(pg) );
                        return TRUE;
                }
                ldd->currentIdx = i;
        }
        return FALSE;
}

static
void
ld_rep_prep( GnomeDruidPage *gdp, gpointer arg1, gpointer ud )
{
        /* do anything? */
}

static
void
ld_pay_prep( GnomeDruidPage *gdp, gpointer arg1, gpointer ud )
{
        LoanDruidData *ldd;
        RepayOptData *rod;
        GString *str;
        gboolean uniq;

        ldd = (LoanDruidData*)ud;
        g_assert( ldd->currentIdx >= 0 );
        g_assert( ldd->currentIdx <= ldd->ld.repayOptCount );

        rod = ldd->ld.repayOpts[ldd->currentIdx];
        str = g_string_sized_new( 32 );
        g_string_sprintf( str, "Payment: \"%s\"", rod->name );
        gnome_druid_page_standard_set_title( GNOME_DRUID_PAGE_STANDARD(gdp),
                                             str->str );
        /* FIXME: copy in the relevant data from the currently-indexed
         * option. */
        gtk_entry_set_text( ldd->payTxnName, rod->txnMemo );
        g_string_sprintf( str, "%0.2f", rod->amount );
        gtk_entry_set_text( ldd->payAmtEntry, str->str );

        uniq = (rod->fs != NULL);
        gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(ldd->payTxnFreqPartRb),
                                      !uniq );
        gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(ldd->payTxnFreqUniqRb),
                                      uniq );
        gtk_widget_set_sensitive( GTK_WIDGET(ldd->payFreqAlign),
                                  uniq );
        if ( uniq ) {
                gnc_frequency_setup( ldd->payGncFreq,
                                     rod->fs, rod->startDate );
        }

        g_string_free( str, TRUE );
}

static
void
ld_pay_save_current( LoanDruidData *ldd )
{
        gchar *tmpStr;
        RepayOptData *rod;

        g_assert( ldd->currentIdx >= 0 );
        g_assert( ldd->currentIdx <= ldd->ld.repayOptCount );
        rod = ldd->ld.repayOpts[ ldd->currentIdx ];

        tmpStr = gtk_editable_get_chars( GTK_EDITABLE(ldd->payTxnName),
                                         0, -1 );
        if ( rod->txnMemo != NULL ) {
                g_free( rod->txnMemo );
        }
        rod->txnMemo = tmpStr;
        tmpStr = NULL;

        tmpStr = gtk_editable_get_chars( GTK_EDITABLE(ldd->payAmtEntry),
                                         0, -1 );
        rod->amount = (float)strtod( tmpStr, NULL );
        g_free( tmpStr );

        /* if ( rb toggled )
         *   ensure freqspec/startdate setup
         *   save
         * else
         *   if (freqspec setup)
         *     remove freqspec/startdate
         */

        /* neither of these should happen. */
        g_assert( ! (rod->fs && !rod->startDate) );
        g_assert( ! (!rod->fs && rod->startDate) );

        if ( gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ldd->payTxnFreqUniqRb)) ) {
                if ( rod->fs == NULL ) {
                        rod->fs = xaccFreqSpecMalloc( gnc_get_current_book() );
                }
                if ( rod->startDate == NULL ) {
                        rod->startDate = g_date_new();
                }
                gnc_frequency_save_state( ldd->payGncFreq,
                                          rod->fs, rod->startDate );
        } else {
                if ( rod->fs ) {
                        xaccFreqSpecFree( rod->fs );
                        rod->fs = NULL;
                }
                if ( rod->startDate ) {
                        g_date_free( rod->startDate );
                        rod->startDate = NULL;
                }
        }
}

static
gboolean
ld_pay_next( GnomeDruidPage *gdp, gpointer arg1, gpointer ud )
{
        int i;
        LoanDruidData *ldd;

        ldd = (LoanDruidData*)ud;
        /* save current data */
        ld_pay_save_current( ldd );

        /* Go through opts list and select next enabled option. */
        for ( i=(++ldd->currentIdx);
              (i < ldd->ld.repayOptCount)
              && !ldd->ld.repayOpts[i]->enabled; i++ )
                ;
        if ( i != ldd->ld.repayOptCount ) {
                ldd->currentIdx = i;
                ld_pay_prep( gdp, arg1, ud );
                return TRUE;
        }
        return FALSE;
}

static
gboolean
ld_pay_back( GnomeDruidPage *gdp, gpointer arg1, gpointer ud )
{
        int i;
        LoanDruidData *ldd;

        ldd = (LoanDruidData*)ud;

        /* save current data */
        ld_pay_save_current( ldd );

        for ( i=(--ldd->currentIdx);
              (i > -1) && !ldd->ld.repayOpts[i]->enabled;
              i-- ) {
        }
        if ( i != -1 ) {
                ldd->currentIdx = i;
                ld_pay_prep( gdp, arg1, ud );
                return TRUE;
        }
        return FALSE;
}

static
gboolean
ld_fin_back ( GnomeDruidPage *gdp, gpointer arg1, gpointer ud )
{
        LoanDruidData *ldd;

        ldd = (LoanDruidData*)ud;
        if ( (ldd->currentIdx < 0)
             || (ldd->currentIdx >= ldd->ld.repayOptCount)
             || !ldd->ld.repayOpts[ldd->currentIdx]->enabled ) {
                int i;
                for ( i=ldd->ld.repayOptCount-1;
                      (i > -1)
                      && !ldd->ld.repayOpts[i]->enabled;
                      i-- )
                        ;
                if ( i == -1 ) {
                        /* transition to Repayment page. */
                        GtkWidget *pg;
                        pg = glade_xml_get_widget( ldd->gxml, PG_REPAYMENT );
                        gnome_druid_set_page( ldd->druid, GNOME_DRUID_PAGE(pg) );
                        return TRUE;
                }
                ldd->currentIdx = i;
        }
        return FALSE;
}

static
void
ld_pay_freq_toggle( GtkToggleButton *tb, gpointer ud )
{
        LoanDruidData *ldd;
        gboolean uniq;

        ldd = (LoanDruidData*)ud;

        g_assert( ldd->currentIdx >= 0 );
        g_assert( ldd->currentIdx <= ldd->ld.repayOptCount );
                  
        uniq = gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(ldd->payTxnFreqUniqRb) );
        gtk_widget_set_sensitive( GTK_WIDGET(ldd->payFreqAlign), uniq );
}
