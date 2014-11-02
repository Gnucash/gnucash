/********************************************************************\
 * assistant-loan.c : An Assistant for setting up loan-repayment    *
 *     scheduled transactions.                                      *
 * Copyright (C) 2002,2007 Joshua Sled <jsled@asynchronous.org>     *
 * Copyright (C) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (C) 2011 Robert Fewell                                 *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "assistant-loan.h"
#include "SchedXaction.h"
#include "SX-book.h"
#include "SX-ttinfo.h"
#include "assistant-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-account-sel.h"
#include "gnc-date.h"
#include "gnc-exp-parser.h"
#include "gnc-component-manager.h"
#include "dialog-utils.h"
#include "Account.h"
#include "gnc-ui.h"
#include "gnc-gdate-utils.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "gnc-frequency.h"
#include "gnc-engine.h"

#ifndef HAVE_STRFMON
# include "strfmon.h"
#else
# include <monetary.h>
#endif
#define DIALOG_LOAN_ASSISTANT_CM_CLASS "assistant-loan-setup"

static QofLogModule log_module = GNC_MOD_ASSISTANT;

enum loan_cols
{
    LOAN_COL_DATE = 0,
    LOAN_COL_PAYMENT,
    LOAN_COL_PRINCIPAL,
    LOAN_COL_INTEREST,
    NUM_LOAN_COLS
};

typedef enum
{
    CURRENT_YEAR,
    NOW_PLUS_ONE,
    WHOLE_LOAN,
    CUSTOM
} REV_RANGE_OPTS;

struct LoanAssistantData_;

/**
 * The data relating to a single "repayment option" -- a potential
 * [sub-]transaction in the repayment.
 **/
typedef struct RepayOptData_
{
    gboolean enabled;
    gboolean optValid;
    gboolean FreqUniq;
    char *name; /* { "insurance", "pmi", "taxes", ... } */
    char *txnMemo;
    float amount;
    gboolean throughEscrowP;
    gboolean specSrcAcctP;
    Account *to;
    Account *from; /* If NULL { If throughEscrowP, then through escrowAcct };
                        * else: undefined. */
    GList *schedule;
    /* If NULL, part of repayment; otherwise: defined
     * here. */
    GDate *startDate;
} RepayOptData;

/**
 * The default repayment options data.
 **/
typedef struct RepayOptDataDefault_
{
    char *name;
    char *defaultTxnMemo;
    gboolean escrowDefault;
    gboolean specSrcAcctDefault;
} RepayOptDataDefault;

static RepayOptDataDefault REPAY_DEFAULTS[] =
{
    /* { name, default txn memo, throughEscrowP, specSrcAcctP } */
    { N_("Taxes"),         N_("Tax Payment"),           TRUE,  FALSE },
    { N_("Insurance"),     N_("Insurance Payment"),     TRUE,  FALSE  },
    /* Translators: PMI stands for Private Mortgage Insurance. */
    { N_("PMI"),           N_("PMI Payment"),           TRUE,  FALSE  },
    { N_("Other Expense"), N_("Miscellaneous Payment"), FALSE, FALSE },
    { NULL }
};

/**
 * The UI-side storage of the repayment options.
 **/
typedef struct RepayOptUI_
{
    /* must be stated this way [instead of 'LoanAssistantData*'] because of
     * forward decl. */
    struct LoanAssistantData_ *ldd;
    GtkCheckButton *optCb;
    GtkCheckButton *escrowCb;
    RepayOptData *optData;
} RepayOptUIData;

typedef enum
{
    GNC_FIXED = 0,
    GNC_VARIABLE,
    GNC_VARIABLE_3_1 = GNC_VARIABLE,
    GNC_VARIABLE_5_1,
    GNC_VARIABLE_7_1,
    GNC_VARIABLE_10_1,
    /* ... FIXME */
} LoanType;

typedef enum
{
    GNC_MONTHS = 0,
    GNC_YEARS
} PeriodSize;

/*type of interest rate entered*/
typedef enum
{
    GNC_IRATE_SIMPLE,
    GNC_IRATE_APR_DAILY,
    GNC_IRATE_APR_WEEKLY,
    GNC_IRATE_APR_MONTHLY,
    GNC_IRATE_APR_QUARTERLY,
    GNC_IRATE_APR_ANNUALLY
} IRateType;

/**
 * A transient struct used to collate the GDate and the gnc_numeric row-data
 * for the repayment review schedule.  numCells is an array of gnc_numerics,
 * with a length of the LoanData.revNumPmts.
 **/
typedef struct rev_repayment_row
{
    GDate date;
    gnc_numeric *numCells;
} RevRepaymentRow;

/**
 * Data about a loan repayment.
 **/
typedef struct LoanData_
{
    Account *primaryAcct;
    gnc_numeric principal;
    float interestRate;
    IRateType rateType;
    LoanType type;
    GList *loan_schedule;
    GDate *startDate;
    GDate *varStartDate;
    int numPer;
    PeriodSize perSize;
    int numMonRemain;

    char *repMemo;
    char *repAmount;
    Account *repFromAcct;
    Account *repPriAcct;
    Account *repIntAcct;
    Account *escrowAcct;
    GList *repayment_schedule;
    GDate *repStartDate;

    int repayOptCount;
    RepayOptData **repayOpts;

    /* Data concerning the review of repayments. */
    int revNumPmts;
    int revRepayOptToColMap[ (sizeof(REPAY_DEFAULTS)
                              / sizeof(RepayOptDataDefault))
                             - 1 ];
    GList *revSchedule;
} LoanData;

/**
 * The UI-side storage of the loan assistant data.
 **/
typedef struct LoanAssistantData_
{
    GtkWidget *window;
    GtkWidget *assistant;

    LoanData ld;
    /* The UI-side storage of repayment data; this is 1:1 with the array
     * in LoanData */
    RepayOptUIData **repayOptsUI;

    /* Current index of the payment opt for multiplexing the 'payment'
     * page. */
    int currentIdx;

    /* widgets */
    /* prm = params */
    GtkTable      *prmTable;
    GNCAccountSel *prmAccountGAS;
    GNCAmountEdit *prmOrigPrincGAE;
    GtkSpinButton *prmIrateSpin;
    GtkComboBox   *prmType;
    GtkFrame      *prmVarFrame;
    GncFrequency  *prmVarGncFreq;
    GNCDateEdit   *prmStartDateGDE;
    GtkSpinButton *prmLengthSpin;
    GtkComboBox   *prmLengthType;
    GtkSpinButton *prmRemainSpin;
    GtkComboBox   *prmIrateType;

    /* opt = options */
    GtkVBox        *optVBox;
    GtkCheckButton *optEscrowCb;
    GtkHBox        *optEscrowHBox;
    GNCAccountSel  *optEscrowGAS;

    /* rep = repayment */
    GtkEntry      *repTxnName;
    GtkTable      *repTable;
    GtkEntry      *repAmtEntry;
    GNCAccountSel *repAssetsFromGAS;
    GNCAccountSel *repPrincToGAS;
    GNCAccountSel *repIntToGAS;
    GtkFrame      *repFreqFrame;
    GncFrequency  *repGncFreq;

    /* pay = payment[s] */
    GtkEntry         *payTxnName;
    GtkEntry         *payAmtEntry;
    GNCAccountSel    *payAcctFromGAS;
    GNCAccountSel    *payAcctEscToGAS;
    GNCAccountSel    *payAcctEscFromGAS;
    GNCAccountSel    *payAcctToGAS;
    GtkTable         *payTable;
    GtkCheckButton   *payUseEscrow;
    GtkCheckButton   *paySpecSrcAcct;
    GtkLabel         *payAcctFromLabel;
    GtkLabel         *payEscToLabel;
    GtkLabel         *payEscFromLabel;
    GtkRadioButton   *payTxnFreqPartRb;
    GtkRadioButton   *payTxnFreqUniqRb;
    GtkAlignment     *payFreqAlign;
    GncFrequency     *payGncFreq;

    /* rev = review */
    GtkComboBox       *revRangeOpt;
    GtkFrame          *revDateFrame;
    GtkTable          *revTable;
    GNCDateEdit       *revStartDate;
    GNCDateEdit       *revEndDate;
    GtkScrolledWindow *revScrollWin;
    GtkTreeView       *revView;
} LoanAssistantData;

/**
 * A transient structure to contain SX details during the creation process.
 **/
typedef struct toCreateSX_
{
    /** The name of the SX */
    gchar *name;
    /** The start, last-occurred and end dates. */
    GDate start, last, end;
    /** The SX schedule */
    GList *schedule;
    /** The current 'instance-num' count. */
    gint instNum;
    /** The main/source transaction being created. */
    TTInfo *mainTxn;
    /** The optional escrow transaction being created. */
    TTInfo *escrowTxn;
} toCreateSX;

/**************************************************************************/

static void loan_assistant_window_destroy_cb( GtkObject *object, gpointer user_data );
static void loan_assistant_close_handler( gpointer user_data );
static void loan_assistant_data_init( LoanAssistantData *ldd );

static void loan_info_prep( GtkAssistant *assistant, gpointer user_data );
static void loan_info_prm_type_cb( GtkWidget *w, gpointer user_data );
static void loan_info_calc_update_cb( GtkWidget *widget, gpointer user_data );
void loan_info_page_valid_cb( GtkWidget *widget, gpointer user_data );
static gboolean loan_info_page_complete( GtkAssistant *assistant, gpointer user_data );
static void loan_info_page_save( GtkAssistant *assistant, gpointer user_data );

static void loan_opt_prep( GtkAssistant *assistant, gpointer user_data );
static void loan_opt_toggled_cb( GtkToggleButton *tb, gpointer user_data );
static void loan_opt_consistency_cb( GtkToggleButton *tb, gpointer user_data );
static void loan_opt_escrow_toggle_cb( GtkToggleButton *tb, gpointer user_data );
static void loan_opt_escrow_toggled_cb( GtkToggleButton *tb, gpointer user_data );
void loan_opt_page_valid_cb (GtkWidget *widget, gpointer user_data );
static gboolean loan_opt_page_complete( GtkAssistant *assistant, gpointer user_data );

static void loan_rep_prep ( GtkAssistant *assistant, gpointer user_data );
void loan_rep_page_valid_cb (GtkWidget *widget, gpointer user_data );
static gboolean loan_rep_page_complete( GtkAssistant *assistant, gpointer user_data );
static void loan_rep_page_save( GtkAssistant *assistant, gpointer user_data );

static void loan_pay_prep ( GtkAssistant *assistant, gpointer user_data );
static void loan_pay_use_esc_setup( LoanAssistantData *ldd, gboolean newState );
static void loan_pay_use_esc_toggle_cb( GtkToggleButton *tb, gpointer user_data );
static void loan_pay_spec_src_setup( LoanAssistantData *ldd, gboolean newState );
static void loan_pay_spec_src_toggle_cb( GtkToggleButton *tb, gpointer user_data );
static void loan_pay_freq_toggle_cb( GtkToggleButton *tb, gpointer user_data );
static void loan_pay_page_valid_cb (GtkWidget *widget, gpointer user_data );
static gboolean loan_pay_complete( GtkAssistant *assistant, gpointer user_data );
static gboolean loan_pay_all_opt_valid ( GtkAssistant *assistant, gpointer user_data );
static void loan_pay_back_button_cb( GtkButton *button, gpointer user_data );
static void loan_pay_next_button_cb( GtkButton *button, gpointer user_data );

static void loan_rev_prep ( GtkAssistant *assistant, gpointer user_data );
static void loan_rev_recalc_schedule( LoanAssistantData *ldd );
static void loan_rev_range_opt_changed_cb( GtkComboBox *combo, gpointer user_data );
static void loan_rev_range_changed_cb( GNCDateEdit *gde, gpointer user_data );
static void loan_rev_get_loan_range( LoanAssistantData *ldd, GDate *start, GDate *end );
static void loan_rev_get_dates( LoanAssistantData *ldd, GDate *start, GDate *end );
static void loan_rev_update_view( LoanAssistantData *ldd, GDate *start, GDate *end );
static void loan_rev_sched_list_free( gpointer data, gpointer user_data );
static void loan_rev_hash_to_list( gpointer key, gpointer val, gpointer user_data );
static void loan_rev_hash_free_date_keys( gpointer key, gpointer val, gpointer user_data );

static void loan_get_pmt_formula( LoanAssistantData *ldd, GString *gstr );
static void loan_get_ppmt_formula( LoanAssistantData *ldd, GString *gstr );
static void loan_get_ipmt_formula( LoanAssistantData *ldd, GString *gstr );
static float loan_apr_to_simple_formula (float rate, float compounding_periods);

static void loan_create_sxes( LoanAssistantData *ldd );
static gint loan_find_ttsplit_with_acct( gconstpointer elt, gconstpointer crit );
static void loan_create_sx_from_tcSX( LoanAssistantData *ldd, toCreateSX *tcSX );
static void loan_tcSX_free( gpointer data, gpointer user_data );

void loan_assistant_prepare( GtkAssistant *assistant, GtkWidget *page, gpointer user_data );
void loan_assistant_finish( GtkAssistant *gtkassistant, gpointer user_data );
void loan_assistant_cancel( GtkAssistant *gtkassistant, gpointer user_data );
void loan_assistant_close( GtkAssistant *gtkassistant, gpointer user_data );

/*****************************************************************************/


static
void
loan_assistant_close_handler( gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    gtk_widget_destroy( ldd->window );
}


static
void
loan_assistant_window_destroy_cb( GtkObject *object, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;

    g_assert( ldd );

    gnc_unregister_gui_component_by_data (DIALOG_LOAN_ASSISTANT_CM_CLASS, ldd);

    /* free alloc'd mem; cleanup */

    /* repay opts */
    {
        int i;

        g_date_free( ldd->ld.startDate );
        g_date_free( ldd->ld.varStartDate );
        recurrenceListFree(&ldd->ld.loan_schedule);

        if ( ldd->ld.repMemo )
            g_free( ldd->ld.repMemo );

        for ( i = 0; i < ldd->ld.repayOptCount; i++ )
        {
            RepayOptData *rod = ldd->ld.repayOpts[i];
            if ( rod->name )
                g_free( rod->name );
            if ( rod->txnMemo )
                g_free( rod->txnMemo );

            if ( rod->startDate )
                g_date_free( rod->startDate );

            if (rod->schedule != NULL)
                recurrenceListFree(&rod->schedule);

            g_free( ldd->ld.repayOpts[i] );
            g_free( ldd->repayOptsUI[i] );
        }
        g_free( ldd->ld.repayOpts );
        g_free( ldd->repayOptsUI );

        if ( ldd->ld.repAmount )
            g_free( ldd->ld.repAmount );

        g_date_free( ldd->ld.repStartDate );
    }

    /* review */
    {
        if ( ldd->ld.revSchedule )
        {
            g_list_foreach( ldd->ld.revSchedule,
                            loan_rev_sched_list_free,
                            NULL );
            g_list_free( ldd->ld.revSchedule );
            ldd->ld.revSchedule = NULL;
        }
    }

    g_free( ldd );
}


static GtkWidget *
gnc_loan_assistant_create( LoanAssistantData *ldd )
{
    GtkBuilder *builder;
    GtkWidget *window;

    loan_assistant_data_init( ldd );

    builder = gtk_builder_new();

    gnc_builder_add_from_file  (builder , "assistant-loan.glade", "len_liststore");
    gnc_builder_add_from_file  (builder , "assistant-loan.glade", "range_liststore");
    gnc_builder_add_from_file  (builder , "assistant-loan.glade", "type_liststore");
    gnc_builder_add_from_file  (builder , "assistant-loan.glade", "rate_liststore");

    gnc_builder_add_from_file  (builder , "assistant-loan.glade", "Loan-Mortgage Assistant");
    window = GTK_WIDGET(gtk_builder_get_object (builder, "Loan-Mortgage Assistant"));
    ldd->window = window;


    /* Set the assistant colors */
    gnc_assistant_set_colors (GTK_ASSISTANT (ldd->window));

    /* Enable buttons on complete pages. */
    gtk_assistant_set_page_complete (GTK_ASSISTANT (window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "loan_intro_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT (window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "loan_options_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT (window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "loan_review_page")),
                                     TRUE);

    /* Information Page */
    {
        ldd->prmTable = GTK_TABLE(gtk_builder_get_object(builder, "param_table"));
        ldd->prmVarFrame = GTK_FRAME(gtk_builder_get_object(builder, "type_freq_frame"));
        ldd->prmIrateSpin = GTK_SPIN_BUTTON (gtk_builder_get_object(builder, "irate_spin"));
        ldd->prmType = GTK_COMBO_BOX (gtk_builder_get_object(builder, "type_combobox"));
        gtk_combo_box_set_active( GTK_COMBO_BOX( ldd->prmType), 0 );
        ldd->prmLengthSpin = GTK_SPIN_BUTTON (gtk_builder_get_object(builder, "len_spin"));
        ldd->prmLengthType = GTK_COMBO_BOX (gtk_builder_get_object(builder, "len_opt"));
        gtk_combo_box_set_active( GTK_COMBO_BOX( ldd->prmLengthType), 0 );
        ldd->prmRemainSpin = GTK_SPIN_BUTTON (gtk_builder_get_object(builder, "rem_spin"));
        ldd->prmIrateType = GTK_COMBO_BOX (gtk_builder_get_object(builder, "irate_type_combobox"));
        gtk_combo_box_set_active( GTK_COMBO_BOX( ldd->prmIrateType), 0 );
        /* ldd->prmStartDateGDE */
    }
    /* Repayment Page */
    {
        ldd->repTable = GTK_TABLE(gtk_builder_get_object(builder, "repay_table"));
        ldd->repTxnName = GTK_ENTRY(gtk_builder_get_object(builder, "txn_title"));
        ldd->repAmtEntry = GTK_ENTRY(gtk_builder_get_object(builder, "amount_ent"));
        ldd->repFreqFrame = GTK_FRAME(gtk_builder_get_object(builder, "freq_frame"));
    }
    /* Options Page */
    {
        ldd->optVBox = GTK_VBOX(gtk_builder_get_object(builder, "loan_options_page"));
        ldd->optEscrowCb = GTK_CHECK_BUTTON(gtk_builder_get_object(builder, "opt_escrow_cb"));
        ldd->optEscrowHBox = GTK_HBOX(gtk_builder_get_object(builder, "opt_escrow_hbox"));
    }
    /* Payment Page */
    {
        ldd->payTable = GTK_TABLE(gtk_builder_get_object(builder, "pay_table"));
        ldd->payTxnName = GTK_ENTRY(gtk_builder_get_object(builder, "pay_txn_title"));
        ldd->payAmtEntry = GTK_ENTRY(gtk_builder_get_object(builder, "pay_amt_ent"));
        ldd->payUseEscrow = GTK_CHECK_BUTTON(gtk_builder_get_object(builder, "pay_use_escrow"));
        ldd->paySpecSrcAcct = GTK_CHECK_BUTTON(gtk_builder_get_object(builder, "pay_specify_source"));
        ldd->payAcctFromLabel = GTK_LABEL(gtk_builder_get_object(builder, "pay_from_account_label"));
        ldd->payEscToLabel = GTK_LABEL(gtk_builder_get_object(builder, "pay_escrow_to_label"));
        ldd->payEscFromLabel = GTK_LABEL(gtk_builder_get_object(builder, "pay_escrow_from_label"));
        ldd->payTxnFreqPartRb = GTK_RADIO_BUTTON(gtk_builder_get_object(builder, "pay_txn_part_rb"));
        ldd->payTxnFreqUniqRb = GTK_RADIO_BUTTON(gtk_builder_get_object(builder, "pay_uniq_freq_rb"));
        ldd->payFreqAlign = GTK_ALIGNMENT(gtk_builder_get_object(builder, "pay_freq_align"));
    }
    /* Review Page */
    {
        ldd->revTable = GTK_TABLE(gtk_builder_get_object(builder, "rev_date_range_table"));
        ldd->revRangeOpt = GTK_COMBO_BOX(gtk_builder_get_object(builder, "rev_range_opt"));
        ldd->revDateFrame = GTK_FRAME(gtk_builder_get_object(builder, "rev_date_frame"));
        ldd->revScrollWin = GTK_SCROLLED_WINDOW(gtk_builder_get_object(builder, "rev_scrollwin"));
        /* GNCDateEdit       *revStartDate */
        /* GNCDateEdit       *revEndDate   */
    }

    /* non-buildable widget setup */
    {
        GtkWidget *butt;
        int i;
        // ACCT_TYPE_LIABILITY
        GList *liabilityAcct;
        // ACCT_TYPE_BANK, ACCT_TYPE_CASH, ACCT_TYPE_CREDIT,
        // ACCT_TYPE_ASSET + ACCT_TYPE_LIABILITY
        GList *paymentFromAccts;
        // ACCT_TYPE_EXPENSE, ACCT_TYPE_LIABILITY, + payment-froms.
        GList *paymentToAccts;
        int fromLen = 5;
        GNCAccountType paymentFroms[] = { ACCT_TYPE_BANK, ACCT_TYPE_CASH,
                                          ACCT_TYPE_CREDIT, ACCT_TYPE_ASSET,
                                          ACCT_TYPE_LIABILITY
                                        };
        int toLen = 1;
        GNCAccountType paymentTos[] = { ACCT_TYPE_EXPENSE };

        liabilityAcct = NULL;
        paymentFromAccts = NULL;
        paymentToAccts = NULL;

        liabilityAcct = g_list_append( liabilityAcct,
                                       GINT_TO_POINTER( ACCT_TYPE_LIABILITY ) );
        for ( i = 0; i < fromLen; i++ )
        {
            paymentFromAccts
            = g_list_append( paymentFromAccts,
                             GINT_TO_POINTER( paymentFroms[i] ) );
            paymentToAccts
            = g_list_append( paymentToAccts,
                             GINT_TO_POINTER( paymentFroms[i] ) );
        }

        for ( i = 0; i < toLen; i++ )
        {
            paymentToAccts = g_list_append( paymentToAccts,
                                            GINT_TO_POINTER( paymentTos[i] ) );
        }

        /* All of the GncAccountSel[ectors]... */
        {
            int i;
            GtkAlignment *a;
            /* "gas" == GncAccountSel */
            struct gas_in_tables_data
            {
                GNCAccountSel **loc;
                GtkTable *table;
                gboolean newAcctAbility;
                int left, right, top, bottom;
                GList *allowableAccounts;
            } gas_data[] =
            {
                /* These ints are the GtkTable boundries */
                { &ldd->prmAccountGAS,     ldd->prmTable, TRUE,  1, 4, 0, 1, liabilityAcct },
                { &ldd->repAssetsFromGAS,  ldd->repTable, TRUE,  1, 4, 2, 3, paymentFromAccts },
                { &ldd->repPrincToGAS,     ldd->repTable, TRUE,  1, 2, 3, 4, paymentToAccts  },
                { &ldd->repIntToGAS,       ldd->repTable, TRUE,  3, 4, 3, 4, paymentToAccts },
                { &ldd->payAcctFromGAS,    ldd->payTable, TRUE,  1, 2, 4, 5, paymentFromAccts },
                { &ldd->payAcctEscToGAS,   ldd->payTable, FALSE, 3, 4, 4, 5, paymentToAccts },
                { &ldd->payAcctEscFromGAS, ldd->payTable, FALSE, 1, 2, 5, 6, paymentFromAccts },
                { &ldd->payAcctToGAS,      ldd->payTable, TRUE,  3, 4, 5, 6, paymentToAccts },
                { NULL }
            };

            /* left-aligned, 25%-width */
            a = GTK_ALIGNMENT(gtk_alignment_new( 0.0, 0.5, 0.25, 1.0 ));
            ldd->prmOrigPrincGAE = GNC_AMOUNT_EDIT(gnc_amount_edit_new());
            gtk_container_add( GTK_CONTAINER(a), GTK_WIDGET(ldd->prmOrigPrincGAE) );
            gtk_table_attach( ldd->prmTable, GTK_WIDGET(a),
                              1, 4, 1, 2,
                              GTK_EXPAND | GTK_FILL,
                              GTK_EXPAND | GTK_FILL, 2, 2 );

            for ( i = 0; gas_data[i].loc != NULL; i++ )
            {
                GNCAccountSel *gas;

                a = GTK_ALIGNMENT(gtk_alignment_new( 0.0, 0.5, 0.25, 1.0 ));
                gas = GNC_ACCOUNT_SEL(gnc_account_sel_new());
                gnc_account_sel_set_new_account_ability(
                    gas, gas_data[i].newAcctAbility );
                if ( gas_data[i].allowableAccounts != NULL )
                {
                    gnc_account_sel_set_acct_filters(
                        gas, gas_data[i].allowableAccounts, NULL );
                }
                gtk_container_add( GTK_CONTAINER(a),
                                   GTK_WIDGET(gas) );
                gtk_table_attach( gas_data[i].table,
                                  GTK_WIDGET(a),
                                  gas_data[i].left,
                                  gas_data[i].right,
                                  gas_data[i].top,
                                  gas_data[i].bottom,
                                  GTK_EXPAND | GTK_FILL,
                                  GTK_EXPAND | GTK_FILL, 2, 2 );
                *(gas_data[i].loc) = gas;
            }
        }

        /* Setup the payment page always-insensitive GASes */
        gtk_widget_set_sensitive( GTK_WIDGET(ldd->payAcctEscToGAS),   FALSE );
        gtk_widget_set_sensitive( GTK_WIDGET(ldd->payAcctEscFromGAS), FALSE );

        /* The GNCDateEdit[s] */
        {
            /* "gde" == GNCDateEdit */
            struct gde_in_tables_data
            {
                GNCDateEdit **loc;
                GtkTable *table;
                int left, right, top, bottom;
            } gde_data[] =
            {
                /* These ints are the GtkTable boundries */
                { &ldd->prmStartDateGDE, ldd->prmTable, 1, 2, 4, 5 },
                { &ldd->revStartDate,    ldd->revTable, 1, 2, 0, 1 },
                { &ldd->revEndDate,      ldd->revTable, 1, 2, 1, 2 },
                { NULL }
            };

            for ( i = 0; gde_data[i].loc != NULL; i++ )
            {
                *gde_data[i].loc =
                    GNC_DATE_EDIT(
                        gnc_date_edit_new( gnc_time (NULL),
                                           FALSE, FALSE ) );
                gtk_table_attach( gde_data[i].table,
                                  GTK_WIDGET( *gde_data[i].loc ),
                                  gde_data[i].left,
                                  gde_data[i].right,
                                  gde_data[i].top,
                                  gde_data[i].bottom,
                                  (GTK_EXPAND | GTK_FILL),
                                  GTK_FILL, 0, 0 );
            }

        }

        gtk_widget_set_sensitive( GTK_WIDGET(ldd->prmVarFrame), FALSE );
        {
            g_signal_connect( ldd->prmType, "changed",
                              G_CALLBACK( loan_info_prm_type_cb ),
                              ldd );
        }

        {
            GtkAdjustment *a;

            /* 8.0 [%], range of 0.005..100.0 with ticks at 0.001[%]. */
            a = GTK_ADJUSTMENT(gtk_adjustment_new( 8.0, 0.001,
                                                   100.0, 0.001,
                                                   1.0, 0.0 ));
            gtk_spin_button_set_adjustment( ldd->prmIrateSpin, a );
            gtk_spin_button_set_value( ldd->prmIrateSpin, 8.00 );
            gtk_spin_button_set_snap_to_ticks( ldd->prmIrateSpin, TRUE );

            a = GTK_ADJUSTMENT(gtk_adjustment_new( 360, 1,
                                                   9999, 1,
                                                   12, 0.0 ));
            gtk_spin_button_set_adjustment( ldd->prmLengthSpin, a );
            g_signal_connect( ldd->prmLengthSpin, "changed",
                              G_CALLBACK( loan_info_calc_update_cb ),
                              ldd );
            g_signal_connect( ldd->prmStartDateGDE, "date-changed",
                              G_CALLBACK( loan_info_calc_update_cb ),
                              ldd );
            g_signal_connect( ldd->prmLengthSpin, "value-changed",
                              G_CALLBACK( loan_info_calc_update_cb ),
                              ldd );
            g_signal_connect( ldd->prmLengthType, "changed",
                              G_CALLBACK( loan_info_calc_update_cb ),
                              ldd );

            a = GTK_ADJUSTMENT(gtk_adjustment_new( 360, 1,
                                                   9999, 1,
                                                   12, 0.0 ));
            gtk_spin_button_set_adjustment( ldd->prmRemainSpin, a );
        }

        g_signal_connect( ldd->optEscrowCb, "toggled",
                          G_CALLBACK(loan_opt_escrow_toggle_cb), ldd );
        gtk_widget_set_sensitive( GTK_WIDGET(ldd->optEscrowHBox), FALSE );
        ldd->optEscrowGAS = GNC_ACCOUNT_SEL(gnc_account_sel_new());
        gnc_account_sel_set_new_account_ability( ldd->optEscrowGAS, TRUE );
        gtk_container_add( GTK_CONTAINER(ldd->optEscrowHBox),
                           GTK_WIDGET(ldd->optEscrowGAS) );

        {
            /* . Each RepayOpt gets an "entry" in the optContainer.
             * . Each "entry" is a 2-line vbox containing:
             *   . The checkbox for the option itself
             *   . an alignment-contained sub-checkbox for "through the
             *     escrow account".
             *   . Hook up each to bit-twiddling the appropriate line.
             */

            RepayOptUIData *rouid;
            GtkVBox *vb;
            GtkAlignment *optAlign, *subOptAlign;
            GString *str;

            str = g_string_sized_new( 32 );

            for ( i = 0; i < ldd->ld.repayOptCount; i++ )
            {
                rouid = ldd->repayOptsUI[i];
                vb = GTK_VBOX(gtk_vbox_new( FALSE, 2 ));

                /* Add payment checkbox. */

                /* Translators: %s is "Taxes",
                 * "Insurance", or similar. */
                g_string_printf( str, _("... pay \"%s\"?"),
                                 rouid->optData->name );
                rouid->optCb =
                    GTK_CHECK_BUTTON(
                        gtk_check_button_new_with_label(
                            str->str ));
                gtk_box_pack_start( GTK_BOX(vb),
                                    GTK_WIDGET(rouid->optCb),
                                    FALSE, FALSE, 2 );
                rouid->escrowCb =
                    GTK_CHECK_BUTTON(
                        gtk_check_button_new_with_label(
                            _("via Escrow account?") ));
                gtk_widget_set_sensitive(
                    GTK_WIDGET(rouid->escrowCb),
                    FALSE );
                subOptAlign =
                    GTK_ALIGNMENT(
                        gtk_alignment_new(
                            0.5, 0.5, 0.75, 1.0 ));
                gtk_container_add( GTK_CONTAINER(subOptAlign),
                                   GTK_WIDGET(rouid->escrowCb) );
                gtk_box_pack_start( GTK_BOX(vb), GTK_WIDGET(subOptAlign),
                                    FALSE, FALSE, 2 );

                g_signal_connect( rouid->optCb, "toggled",
                                  G_CALLBACK(loan_opt_toggled_cb),
                                  rouid );
                g_signal_connect( rouid->optCb, "toggled",
                                  G_CALLBACK(loan_opt_consistency_cb),
                                  rouid );
                g_signal_connect( rouid->escrowCb, "toggled",
                                  G_CALLBACK(loan_opt_escrow_toggled_cb),
                                  rouid );

                optAlign = GTK_ALIGNMENT(gtk_alignment_new( 0.5, 0.5, 0.75, 1.0 ));
                gtk_container_add( GTK_CONTAINER(optAlign),
                                   GTK_WIDGET(vb) );
                gtk_box_pack_start( GTK_BOX(ldd->optVBox), GTK_WIDGET(optAlign),
                                    FALSE, FALSE, 2 );
                gtk_widget_show_all( GTK_WIDGET(optAlign) );
            }
            g_string_free( str, TRUE );
        }

        g_signal_connect( ldd->payUseEscrow, "toggled",
                          G_CALLBACK(loan_pay_use_esc_toggle_cb), ldd );
        g_signal_connect( ldd->paySpecSrcAcct, "toggled",
                          G_CALLBACK(loan_pay_spec_src_toggle_cb), ldd );
        g_signal_connect( ldd->payTxnFreqUniqRb, "toggled",
                          G_CALLBACK(loan_pay_freq_toggle_cb), ldd );

        {
            GtkHBox *hbox;
            hbox = GTK_HBOX(gtk_builder_get_object(builder, "type_freq_hbox"));
            ldd->prmVarGncFreq = GNC_FREQUENCY(gnc_frequency_new( NULL, NULL ));
            gtk_box_pack_start( GTK_BOX(hbox) , GTK_WIDGET(ldd->prmVarGncFreq), TRUE, FALSE, 0 );
            g_signal_connect (ldd->prmVarGncFreq, "changed",
                              G_CALLBACK (loan_info_page_valid_cb), ldd);
        }
        {
            GtkHBox *hbox;
            hbox = GTK_HBOX(gtk_builder_get_object(builder, "freq_frame_hbox"));
            ldd->repGncFreq = GNC_FREQUENCY(gnc_frequency_new( NULL, NULL ));
            gtk_box_pack_start( GTK_BOX(hbox) , GTK_WIDGET(ldd->repGncFreq), TRUE, FALSE, 0 );
            g_signal_connect (ldd->repGncFreq, "changed",
                              G_CALLBACK (loan_rep_page_valid_cb), ldd);
        }

        ldd->payGncFreq = GNC_FREQUENCY(gnc_frequency_new( NULL, NULL ));
        gtk_container_add( GTK_CONTAINER(ldd->payFreqAlign), GTK_WIDGET(ldd->payGncFreq) );
        g_signal_connect (ldd->payGncFreq, "changed",
                          G_CALLBACK (loan_pay_page_valid_cb), ldd);

        butt = GTK_WIDGET(gtk_builder_get_object(builder, "pay_back_button"));
        g_signal_connect (butt, "clicked",
                          G_CALLBACK (loan_pay_back_button_cb), ldd);

        butt = GTK_WIDGET(gtk_builder_get_object(builder, "pay_next_button"));
        g_signal_connect (butt, "clicked",
                          G_CALLBACK (loan_pay_next_button_cb), ldd);

    }

    /* Info page Call Back */
    {
        g_signal_connect (ldd->prmAccountGAS, "account_sel_changed",
                          G_CALLBACK (loan_info_page_valid_cb), ldd);
        g_signal_connect( ldd->prmIrateType, "changed",
                          G_CALLBACK( loan_info_page_valid_cb ), ldd );
    }
    /* Opts page Call Back */
    {
        g_signal_connect (ldd->optEscrowGAS, "account_sel_changed",
                          G_CALLBACK (loan_opt_page_valid_cb), ldd);
    }
    /* Rep page Call Back */
    {
        g_signal_connect (ldd->repAssetsFromGAS, "account_sel_changed",
                          G_CALLBACK (loan_rep_page_valid_cb), ldd);
        g_signal_connect (ldd->repIntToGAS, "account_sel_changed",
                          G_CALLBACK (loan_rep_page_valid_cb), ldd);
        g_signal_connect (ldd->repPrincToGAS, "account_sel_changed",
                          G_CALLBACK (loan_rep_page_valid_cb), ldd);
    }
    /* Pay page Call Back */
    {
        g_signal_connect (ldd->payAcctFromGAS, "account_sel_changed",
                          G_CALLBACK (loan_pay_page_valid_cb), ldd);
        g_signal_connect (ldd->payAcctToGAS, "account_sel_changed",
                          G_CALLBACK (loan_pay_page_valid_cb), ldd);
        g_signal_connect (ldd->payAcctEscFromGAS, "account_sel_changed",
                          G_CALLBACK (loan_pay_page_valid_cb), ldd);
        g_signal_connect (ldd->payAcctEscToGAS, "account_sel_changed",
                          G_CALLBACK (loan_pay_page_valid_cb), ldd);
    }
    /* Review page Call Back */
    {
        gtk_combo_box_set_active( ldd->revRangeOpt, 0 );
        g_signal_connect( ldd->revRangeOpt, "changed",
                          G_CALLBACK( loan_rev_range_opt_changed_cb ),
                          ldd );
        g_signal_connect( ldd->revStartDate, "date-changed",
                          G_CALLBACK( loan_rev_range_changed_cb ),
                          ldd );
        g_signal_connect( ldd->revEndDate, "date-changed",
                          G_CALLBACK( loan_rev_range_changed_cb ),
                          ldd );
    }

    g_signal_connect( ldd->window, "destroy",
                      G_CALLBACK(loan_assistant_window_destroy_cb),
                      ldd );

    gtk_builder_connect_signals(builder, ldd);
    g_object_unref(G_OBJECT(builder));

    gtk_widget_show_all( ldd->window );
    return window;
}


static
void
loan_assistant_data_init( LoanAssistantData *ldd )
{
    int i, optCount;
    RepayOptData *optData;

    /* get the count of repayment defaults. */
    for ( optCount = 0; REPAY_DEFAULTS[optCount].name != NULL; optCount++ )
        ;

    ldd->currentIdx = -1;

    ldd->ld.principal = gnc_numeric_zero();
    ldd->ld.startDate = g_date_new();
    ldd->ld.varStartDate = g_date_new();
    gnc_gdate_set_time64( ldd->ld.startDate, gnc_time (NULL) );
    ldd->ld.loan_schedule = NULL;
    ldd->ld.repayment_schedule = NULL;
    {
        Recurrence *r = g_new0(Recurrence, 1);
        recurrenceSet(r, 1, PERIOD_MONTH, ldd->ld.startDate, WEEKEND_ADJ_NONE);
        ldd->ld.repayment_schedule = g_list_append(ldd->ld.repayment_schedule, r);
    }

    ldd->ld.repMemo = g_strdup( _("Loan") );
    ldd->ld.repAmount = NULL;
    ldd->ld.repStartDate = g_date_new();
    ldd->ld.repayOptCount = optCount;
    ldd->ld.repayOpts = g_new0( RepayOptData*, optCount );
    /* copy all the default lines into the LDD */
    ldd->repayOptsUI = g_new0( RepayOptUIData*, optCount );
    for ( i = 0; i < optCount; i++ )
    {
        g_assert( REPAY_DEFAULTS[i].name != NULL );

        ldd->repayOptsUI[i] = g_new0( RepayOptUIData, 1 );
        ldd->repayOptsUI[i]->ldd = ldd;

        optData = ldd->ld.repayOpts[i]
                  = ldd->repayOptsUI[i]->optData
                    = g_new0( RepayOptData, 1 );

        optData->enabled        = FALSE;
        optData->optValid	= FALSE;
        optData->FreqUniq	= FALSE;
        optData->name           = g_strdup( _(REPAY_DEFAULTS[i].name) );
        optData->txnMemo        = g_strdup( _(REPAY_DEFAULTS[i].
                                              defaultTxnMemo) );
        optData->amount         = 0.0;
        optData->throughEscrowP = REPAY_DEFAULTS[i].escrowDefault;
        optData->specSrcAcctP   = REPAY_DEFAULTS[i].specSrcAcctDefault;
        optData->schedule       = NULL;
        optData->startDate      = NULL;
    }
}

/************************ Page functions in order ***********************/

static
void
loan_info_prep( GtkAssistant *assistant, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;

    gnc_amount_edit_set_amount( ldd->prmOrigPrincGAE, ldd->ld.principal );
    gtk_spin_button_set_value( ldd->prmIrateSpin, ldd->ld.interestRate );
    gtk_combo_box_set_active( ldd->prmIrateType, ldd->ld.rateType );
    gtk_combo_box_set_active( ldd->prmType, ldd->ld.type );
    if ( ldd->ld.type != GNC_FIXED )
    {
        g_signal_handlers_block_by_func( GNC_FREQUENCY( ldd->prmVarGncFreq), loan_info_page_valid_cb , ldd );
        gnc_frequency_setup_recurrence(ldd->prmVarGncFreq, ldd->ld.loan_schedule, ldd->ld.varStartDate);
        g_signal_handlers_unblock_by_func( GNC_FREQUENCY( ldd->prmVarGncFreq), loan_info_page_valid_cb , ldd );
    }

    /* start date */
    {
        struct tm *tmpTm;

        tmpTm = g_new0( struct tm, 1 );

        g_date_to_struct_tm (ldd->ld.startDate, tmpTm);
        gnc_date_edit_set_time (ldd->prmStartDateGDE,
                                gnc_mktime (tmpTm));
        g_free (tmpTm);
    }

    /* length: total and remaining */
    {
        gtk_spin_button_set_value( ldd->prmLengthSpin, ldd->ld.numPer );
        gtk_combo_box_set_active( ldd->prmLengthType, ldd->ld.perSize );
        gtk_spin_button_set_value( ldd->prmRemainSpin, ldd->ld.numMonRemain );
    }
}


static
void
loan_info_prm_type_cb( GtkWidget *w, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    gint index;

    index = gtk_combo_box_get_active( ldd->prmType );
    gtk_widget_set_sensitive( GTK_WIDGET(ldd->prmVarFrame),
                              index != GNC_FIXED );
}


static
void
loan_info_calc_update_cb( GtkWidget *w, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    GDate start, now;
    int i, totalVal, total, remain;

    g_date_clear( &start, 1 );
    g_date_clear( &now, 1 );
    gnc_gdate_set_time64( &start, gnc_date_edit_get_date( ldd->prmStartDateGDE ) );
    gnc_gdate_set_time64( &now, gnc_time (NULL) );
    for ( i = 0; g_date_compare( &start, &now ) < 0; i++ )
    {
        g_date_add_months( &start, 1 );
    }

    /* Get the correct, current value of the length spin. */
    {
        gchar *valueStr = gtk_editable_get_chars( GTK_EDITABLE(ldd->prmLengthSpin),
                          0, -1 );
        totalVal = strtol( valueStr, NULL, 10 );
        g_free( valueStr );
    }
    total = totalVal
            * ( gtk_combo_box_get_active( ldd->prmLengthType )
                == 1 ? 12 : 1 );
    remain = total - i;
    gtk_spin_button_set_value( ldd->prmRemainSpin, remain );
    gtk_widget_show( GTK_WIDGET(ldd->prmRemainSpin) );
}


void
loan_info_page_valid_cb (GtkWidget *widget, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    GtkAssistant *assistant = GTK_ASSISTANT(ldd->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    gtk_assistant_set_page_complete (assistant, page,
                                     loan_info_page_complete (assistant, ldd));
}


static
gboolean
loan_info_page_complete( GtkAssistant *assistant, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;

    ldd->ld.primaryAcct = gnc_account_sel_get_account( ldd->prmAccountGAS );
    /* Test for valid Account */
    if ( ldd->ld.primaryAcct == NULL )
        return FALSE;

    return TRUE;
}


static
void
loan_info_page_save( GtkAssistant *assistant, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;

    ldd->ld.primaryAcct = gnc_account_sel_get_account( ldd->prmAccountGAS );

    if ( ! ldd->ld.repPriAcct )
    {
        ldd->ld.repPriAcct = ldd->ld.primaryAcct;
    }
    ldd->ld.principal = gnc_amount_edit_get_amount( ldd->prmOrigPrincGAE );
    ldd->ld.interestRate = gtk_spin_button_get_value( ldd->prmIrateSpin );
    ldd->ld.rateType = gtk_combo_box_get_active (ldd->prmIrateType );
    ldd->ld.type = gtk_combo_box_get_active( ldd->prmType );

    if ( ldd->ld.type != GNC_FIXED )
    {
        recurrenceListFree(&ldd->ld.loan_schedule);
        gnc_frequency_save_to_recurrence(ldd->prmVarGncFreq,
                                         &ldd->ld.loan_schedule,
                                         ldd->ld.varStartDate);
    }

    /* start date */
    {
        time64 tmpTT;
        struct tm *tmpTm;

        tmpTT = gnc_date_edit_get_date( ldd->prmStartDateGDE );
        tmpTm = gnc_localtime ( &tmpTT );
        g_date_set_dmy( ldd->ld.startDate,
                        tmpTm->tm_mday,
                        (tmpTm->tm_mon + 1),
                        (1900 + tmpTm->tm_year) );
	gnc_tm_free (tmpTm);
    }

    /* len / periods */
    {
        ldd->ld.perSize =
            (gtk_combo_box_get_active( ldd->prmLengthType )
             == GNC_MONTHS) ? GNC_MONTHS : GNC_YEARS;
        ldd->ld.numPer =
            gtk_spin_button_get_value_as_int( ldd->prmLengthSpin );
        ldd->ld.numMonRemain =
            gtk_spin_button_get_value_as_int( ldd->prmRemainSpin );
    }
}

/************************************************************************/

static
void
loan_opt_prep( GtkAssistant *assistant, gpointer user_data )
{
    int i;
    RepayOptUIData *rouid;
    LoanAssistantData *ldd = user_data;

    /* Save Previous Page ( Information ) */
    loan_info_page_save(assistant, ldd);

    if ( ldd->ld.escrowAcct )
    {
        gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(ldd->optEscrowCb), TRUE );
        gnc_account_sel_set_account( ldd->optEscrowGAS, ldd->ld.escrowAcct, FALSE );
    }
    for ( i = 0; i < ldd->ld.repayOptCount; i++ )
    {
        rouid = ldd->repayOptsUI[i];
        gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(rouid->optCb),
                                      rouid->optData->enabled );
        gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(rouid->escrowCb),
                                      rouid->optData->throughEscrowP
                                      && rouid->optData->enabled
                                      && ldd->ld.escrowAcct );
        gtk_widget_set_sensitive( GTK_WIDGET(rouid->escrowCb),
                                  rouid->optData->enabled
                                  && ldd->ld.escrowAcct );
    }
}


static
void
loan_opt_toggled_cb( GtkToggleButton *tb, gpointer user_data )
{
    RepayOptUIData *rouid;

    rouid = (RepayOptUIData*)user_data;
    rouid->optData->enabled = gtk_toggle_button_get_active(tb);
}


static
void
loan_opt_consistency_cb( GtkToggleButton *tb, gpointer user_data )
{
    GtkToggleButton *escrowCb;
    RepayOptUIData *rouid;

    rouid = (RepayOptUIData*)user_data;
    escrowCb = GTK_TOGGLE_BUTTON(rouid->escrowCb);
    /* make sure the escrow option is only selected if we're active. */
    gtk_toggle_button_set_active( escrowCb,
                                  gtk_toggle_button_get_active(
                                      GTK_TOGGLE_BUTTON(
                                          rouid->ldd->optEscrowCb) )
                                  && rouid->optData->throughEscrowP
                                  && gtk_toggle_button_get_active(tb) );
    /* make sure the escrow option is only sensitive if we're active, and
     * the escrow account is enabled  */
    gtk_widget_set_sensitive( GTK_WIDGET(escrowCb),
                              gtk_toggle_button_get_active(tb)
                              && gtk_toggle_button_get_active(
                                  GTK_TOGGLE_BUTTON(rouid->ldd->optEscrowCb)) );
}


static
void
loan_opt_escrow_toggle_cb( GtkToggleButton *tb, gpointer user_data )
{
    int i;
    gboolean newState;
    RepayOptUIData *rouid;
    LoanAssistantData *ldd = user_data;
    GtkAssistant *assistant = GTK_ASSISTANT(ldd->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    newState = gtk_toggle_button_get_active(tb);
    gtk_widget_set_sensitive( GTK_WIDGET(ldd->optEscrowHBox), newState );
    /* Check for Valid Account if enabled */
    if (newState)
    {
        if (GNC_ACCOUNT_SEL( ldd->ld.escrowAcct) == NULL)
            gtk_assistant_set_page_complete (assistant, page, FALSE);
    }
    else
    {
        ldd->ld.escrowAcct = NULL;
        gnc_account_sel_set_account( GNC_ACCOUNT_SEL( ldd->optEscrowGAS), NULL , FALSE );
        gtk_assistant_set_page_complete (assistant, page, TRUE);
    }


    /* deal with escrow options. */
    for ( i = 0; i < ldd->ld.repayOptCount; i++ )
    {
        rouid = ldd->repayOptsUI[i];
        /* If we're going off, then uncheck and desensitize all escrow opts. */
        /* If we're going on, then sensitize all escrow opts. */

        /* prevent the toggle handler from running and "trashing" the
         * state of the throughEscrowP selection */
        g_signal_handlers_block_by_func( rouid->escrowCb,
                                         loan_opt_escrow_toggled_cb,
                                         rouid );
        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(rouid->escrowCb),
            ( newState
              && gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(rouid->optCb) )
              && rouid->optData->throughEscrowP ) );
        gtk_widget_set_sensitive(
            GTK_WIDGET(rouid->escrowCb),
            newState
            && gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(rouid->optCb) ) );
        g_signal_handlers_unblock_by_func( rouid->escrowCb,
                                           loan_opt_escrow_toggled_cb,
                                           rouid );
        if ( newState )
        {
            rouid->optData->from = ldd->ld.escrowAcct;
        }
        else
        {
            rouid->optData->from = NULL;
        }
    }
}


static
void
loan_opt_escrow_toggled_cb( GtkToggleButton *tb, gpointer user_data )
{
    RepayOptUIData *rouid;

    rouid = (RepayOptUIData*)user_data;
    rouid->optData->throughEscrowP = gtk_toggle_button_get_active( tb );
}


void
loan_opt_page_valid_cb (GtkWidget *widget, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;

    GtkAssistant *assistant = GTK_ASSISTANT(ldd->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    gtk_assistant_set_page_complete (assistant, page,
                                     loan_opt_page_complete (assistant, ldd));
}


static
gboolean
loan_opt_page_complete( GtkAssistant *assistant, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;

    if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(ldd->optEscrowCb) ) )
    {
        ldd->ld.escrowAcct =
            gnc_account_sel_get_account( ldd->optEscrowGAS );
        /* Test for valid Account */
        if ( ldd->ld.escrowAcct == NULL )
            return FALSE;
    }
    else
    {
        ldd->ld.escrowAcct = NULL;
    }
    return TRUE;
}

/************************************************************************/

static
void
loan_rep_prep( GtkAssistant *assistant, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    GString *str;

    if ( ldd->ld.repAmount )
    {
        g_free( ldd->ld.repAmount );
    }

    str = g_string_sized_new( 64 );
    loan_get_pmt_formula( ldd, str);
    ldd->ld.repAmount = str->str;
    g_string_free( str, FALSE );

    if ( ldd->ld.repMemo )
        gtk_entry_set_text( ldd->repTxnName, ldd->ld.repMemo );

    if ( ldd->ld.repAmount )
        gtk_entry_set_text( ldd->repAmtEntry, ldd->ld.repAmount );

    gnc_account_sel_set_account( ldd->repAssetsFromGAS, ldd->ld.repFromAcct, FALSE );
    gnc_account_sel_set_account( ldd->repPrincToGAS, ldd->ld.repPriAcct, FALSE );
    gnc_account_sel_set_account( ldd->repIntToGAS, ldd->ld.repIntAcct, FALSE );

    g_signal_handlers_block_by_func( ldd->repGncFreq, loan_rep_page_valid_cb , ldd );
    gnc_frequency_setup_recurrence(ldd->repGncFreq, ldd->ld.repayment_schedule, ldd->ld.repStartDate);
    g_signal_handlers_unblock_by_func( ldd->repGncFreq, loan_rep_page_valid_cb , ldd );

    /* Find the first enabled option */
    {
        int i;
        for ( i = 0; // we can always start at 0, here.
                (i < ldd->ld.repayOptCount)
                && !ldd->ld.repayOpts[i]->enabled;
                i++ )
            ;
        if ( i < ldd->ld.repayOptCount )
            ldd->currentIdx = i;
        else
            ldd->currentIdx = -1;
    }
}


void
loan_rep_page_valid_cb (GtkWidget *widget, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    GtkAssistant *assistant = GTK_ASSISTANT(ldd->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    gtk_assistant_set_page_complete (assistant, page,
                                     loan_rep_page_complete (assistant, ldd));
}


static
gboolean
loan_rep_page_complete( GtkAssistant *assistant, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;

    ldd->ld.repFromAcct =
        gnc_account_sel_get_account( ldd->repAssetsFromGAS );
    /* Test for valid Account */
    if ( ldd->ld.repFromAcct == NULL )
        return FALSE;

    ldd->ld.repPriAcct =
        gnc_account_sel_get_account( ldd->repPrincToGAS );
    /* Test for valid Account */
    if ( ldd->ld.repPriAcct == NULL )
        return FALSE;

    ldd->ld.repIntAcct =
        gnc_account_sel_get_account( ldd->repIntToGAS );
    /* Test for valid Account */
    if ( ldd->ld.repIntAcct == NULL )
        return FALSE;

    return TRUE;
}


static
void
loan_rep_page_save( GtkAssistant *assistant, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;

    if ( ldd->ld.repMemo )
        g_free( ldd->ld.repMemo );
    ldd->ld.repMemo =
        gtk_editable_get_chars( GTK_EDITABLE(ldd->repTxnName), 0, -1 );

    if ( ldd->ld.repAmount )
        g_free( ldd->ld.repAmount );
    ldd->ld.repAmount =
        gtk_editable_get_chars( GTK_EDITABLE(ldd->repAmtEntry), 0, -1 );

    ldd->ld.repFromAcct =
        gnc_account_sel_get_account( ldd->repAssetsFromGAS );

    ldd->ld.repPriAcct =
        gnc_account_sel_get_account( ldd->repPrincToGAS );

    ldd->ld.repIntAcct =
        gnc_account_sel_get_account( ldd->repIntToGAS );

    recurrenceListFree(&ldd->ld.repayment_schedule);
    gnc_frequency_save_to_recurrence(ldd->repGncFreq,
                                     &ldd->ld.repayment_schedule,
                                     ldd->ld.repStartDate);
}

/************************************************************************/

static
void
loan_pay_prep( GtkAssistant *assistant, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    RepayOptData *rod;
    GString *str;


    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    /* Save Previous Page (Repayment) */
    loan_rep_page_save(assistant, ldd);

    /* Step over page if no options enabled */
    if (ldd->currentIdx == -1 )
    {
        gtk_assistant_set_current_page (assistant, num + 1);
    }
    else
    {
        g_assert( ldd->currentIdx >= 0 );
        g_assert( ldd->currentIdx <= ldd->ld.repayOptCount );

        rod = ldd->ld.repayOpts[ldd->currentIdx];
        str = g_string_sized_new( 32 );
        /* Translators: %s is "Taxes", or "Insurance", or similar */
        g_string_printf( str, _("Loan Repayment Option: \"%s\""), rod->name );
        gtk_assistant_set_page_title (assistant, page, str->str );

        /* copy in the relevant data from the currently-indexed option. */
        gtk_entry_set_text( ldd->payTxnName, rod->txnMemo );
        g_string_printf( str, "%0.2f", rod->amount );
        gtk_entry_set_text( ldd->payAmtEntry, str->str );

        gtk_widget_set_sensitive( GTK_WIDGET(ldd->payUseEscrow),
                                  (ldd->ld.escrowAcct != NULL) );

        {
            g_signal_handlers_block_by_func( ldd->payUseEscrow,
                                             loan_pay_use_esc_toggle_cb,
                                             ldd );

            loan_pay_use_esc_setup( ldd,
                                    (ldd->ld.escrowAcct != NULL)
                                    && rod->throughEscrowP );
            gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(ldd->payUseEscrow),
                                          (rod->throughEscrowP
                                           && ldd->ld.escrowAcct != NULL) );

            g_signal_handlers_unblock_by_func( ldd->payUseEscrow,
                                               loan_pay_use_esc_toggle_cb,
                                               ldd );
        }

        {
            g_signal_handlers_block_by_func( ldd->paySpecSrcAcct,
                                             loan_pay_spec_src_toggle_cb,
                                             ldd );
            loan_pay_spec_src_setup( ldd, rod->specSrcAcctP );
            gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(ldd->paySpecSrcAcct),
                                          rod->specSrcAcctP );

            g_signal_handlers_unblock_by_func( ldd->paySpecSrcAcct,
                                               loan_pay_spec_src_toggle_cb,
                                               ldd );
        }

        g_signal_handlers_block_by_func(ldd->payAcctToGAS, loan_pay_page_valid_cb, ldd );
        gnc_account_sel_set_account( ldd->payAcctToGAS,   rod->to, FALSE );
        g_signal_handlers_unblock_by_func(ldd->payAcctToGAS, loan_pay_page_valid_cb, ldd );


        g_signal_handlers_block_by_func(ldd->payTxnFreqUniqRb, loan_pay_freq_toggle_cb, ldd );
        gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(ldd->payTxnFreqPartRb), !rod->FreqUniq );
        gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(ldd->payTxnFreqUniqRb), rod->FreqUniq );
        g_signal_handlers_unblock_by_func(ldd->payTxnFreqUniqRb, loan_pay_freq_toggle_cb, ldd );

        gtk_widget_set_sensitive( GTK_WIDGET(ldd->payFreqAlign), rod->FreqUniq );

        if ( rod->FreqUniq )
        {
            g_signal_handlers_disconnect_by_func( ldd->payGncFreq, loan_pay_page_valid_cb, ldd );
            gtk_container_remove( GTK_CONTAINER(ldd->payFreqAlign), GTK_WIDGET(ldd->payGncFreq) );
            ldd->payGncFreq = NULL;
            ldd->payGncFreq = GNC_FREQUENCY(gnc_frequency_new_from_recurrence( rod->schedule, rod->startDate ));
            gtk_container_add( GTK_CONTAINER(ldd->payFreqAlign), GTK_WIDGET(ldd->payGncFreq) );
            g_signal_connect (ldd->payGncFreq, "changed",
                              G_CALLBACK (loan_pay_page_valid_cb), ldd);
        }
        g_string_free( str, TRUE );
        loan_pay_page_valid_cb(GTK_WIDGET(ldd->window), ldd);
    }
}


void
loan_pay_page_valid_cb (GtkWidget *widget, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    GtkAssistant *assistant = GTK_ASSISTANT(ldd->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    gtk_assistant_set_page_complete (assistant, page,
                                     ( loan_pay_complete (assistant, ldd) &&
                                       loan_pay_all_opt_valid (assistant, ldd )));
}


static
void
loan_pay_use_esc_setup( LoanAssistantData *ldd, gboolean newState )
{
    gtk_widget_set_sensitive( GTK_WIDGET(ldd->payEscToLabel), newState );
    gtk_widget_set_sensitive( GTK_WIDGET(ldd->payEscFromLabel), newState );
    if ( newState )
    {
        g_signal_handlers_block_by_func( ldd->payAcctEscToGAS, loan_pay_page_valid_cb, ldd );
        g_signal_handlers_block_by_func( ldd->payAcctEscFromGAS, loan_pay_page_valid_cb, ldd );
        gnc_account_sel_set_account( ldd->payAcctEscToGAS, ldd->ld.escrowAcct, FALSE );
        gnc_account_sel_set_account( ldd->payAcctEscFromGAS, ldd->ld.escrowAcct, FALSE );
        g_signal_handlers_unblock_by_func( ldd->payAcctEscToGAS, loan_pay_page_valid_cb, ldd );
        g_signal_handlers_unblock_by_func( ldd->payAcctEscFromGAS, loan_pay_page_valid_cb, ldd );
    }
}


static
void
loan_pay_use_esc_toggle_cb( GtkToggleButton *tb, gpointer user_data )
{
    gboolean newState;
    LoanAssistantData *ldd = user_data;

    newState = gtk_toggle_button_get_active( tb );
    loan_pay_use_esc_setup( ldd, newState );
}


static
void
loan_pay_spec_src_setup( LoanAssistantData *ldd, gboolean newState )
{
    gtk_widget_set_sensitive( GTK_WIDGET(ldd->payAcctFromLabel), newState );
    gtk_widget_set_sensitive( GTK_WIDGET(ldd->payAcctFromGAS), newState );
    if ( newState )
    {
        g_signal_handlers_block_by_func( ldd->payAcctFromGAS, loan_pay_page_valid_cb, ldd );
        gnc_account_sel_set_account( ldd->payAcctFromGAS, ldd->ld.repayOpts[ldd->currentIdx]->from, FALSE );
        g_signal_handlers_unblock_by_func( ldd->payAcctFromGAS, loan_pay_page_valid_cb, ldd );
    }
    else
    {
        g_signal_handlers_block_by_func( ldd->payAcctFromGAS, loan_pay_page_valid_cb, ldd );
        gnc_account_sel_set_account( ldd->payAcctFromGAS, NULL, FALSE );
        ldd->ld.repayOpts[ldd->currentIdx]->from = NULL;
        g_signal_handlers_unblock_by_func( ldd->payAcctFromGAS, loan_pay_page_valid_cb, ldd );
    }
}


static
void
loan_pay_spec_src_toggle_cb( GtkToggleButton *tb, gpointer user_data )
{
    gboolean newState;
    LoanAssistantData *ldd = user_data;

    newState = gtk_toggle_button_get_active( tb );
    loan_pay_spec_src_setup( ldd, newState );
}


static
void
loan_pay_freq_toggle_cb( GtkToggleButton *tb, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    RepayOptData *rod;

    g_assert( ldd->currentIdx >= 0 );
    g_assert( ldd->currentIdx <= ldd->ld.repayOptCount );

    rod = ldd->ld.repayOpts[ldd->currentIdx];

    rod->FreqUniq = gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(ldd->payTxnFreqUniqRb) );
    gtk_widget_set_sensitive( GTK_WIDGET(ldd->payFreqAlign), rod->FreqUniq );

    if ( rod->FreqUniq )
    {
        if ( rod->schedule == NULL )
        {
            Recurrence *r = g_new0(Recurrence, 1);

            recurrenceSet(r, 1, PERIOD_MONTH, ldd->ld.startDate, WEEKEND_ADJ_NONE);
            rod->schedule = g_list_append(rod->schedule, r);
        }
        if ( rod->startDate == NULL )
        {
            rod->startDate = g_date_new();
            *rod->startDate = *ldd->ld.startDate;
        }
        g_signal_handlers_block_by_func( ldd->payGncFreq, loan_pay_page_valid_cb, ldd );
        gnc_frequency_setup_recurrence(ldd->payGncFreq, rod->schedule, rod->startDate);
        g_signal_handlers_unblock_by_func( ldd->payGncFreq, loan_pay_page_valid_cb, ldd );
    }
    else
    {
        if (rod->schedule)
        {
            recurrenceListFree(&rod->schedule);
        }
        if ( rod->startDate )
        {
            g_date_free( rod->startDate );
            rod->startDate = NULL;
        }
    }
}


static
void
loan_pay_next_button_cb( GtkButton *button, gpointer user_data )
{
    int i;
    LoanAssistantData *ldd = user_data;

    /* save current data */
    if ( loan_pay_complete ( GTK_ASSISTANT(ldd->window), user_data ) != FALSE )
    {
        /* Go through opts list and select next enabled option. */
        for ( i = ldd->currentIdx + 1;
                (i < ldd->ld.repayOptCount)
                && !ldd->ld.repayOpts[i]->enabled; i++ )
            ;
        if ( i < ldd->ld.repayOptCount )
        {
            ldd->currentIdx = i;
            loan_pay_prep( GTK_ASSISTANT(ldd->window), user_data );
        }
    }
}


static
void
loan_pay_back_button_cb( GtkButton *button, gpointer user_data )
{
    int i;
    LoanAssistantData *ldd = user_data;

    /* save current data */
    if ( loan_pay_complete ( GTK_ASSISTANT(ldd->window), user_data ) != FALSE)
    {
        /* go back through opts list and select next enabled options. */
        for ( i = ldd->currentIdx - 1;
                (i > -1) && !ldd->ld.repayOpts[i]->enabled;
                i-- )
            ;
        if ( i >= 0 )
        {
            ldd->currentIdx = i;
            loan_pay_prep( GTK_ASSISTANT(ldd->window), user_data );
        }
    }
}


static
gboolean
loan_pay_all_opt_valid ( GtkAssistant *assistant, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    int i;
    gboolean all_valid;
    all_valid = FALSE;

    /* Go through all option pages checking for valid enabled pages */
    for ( i = 0; (i < ldd->ld.repayOptCount); i++ )
    {
        if (ldd->ld.repayOpts[i]->enabled)
        {
            if (ldd->ld.repayOpts[i]->optValid)
                all_valid = TRUE;
            else
                all_valid = FALSE;
        }
    }
    return all_valid;
}


static
gboolean
loan_pay_complete( GtkAssistant *assistant, gpointer user_data )
{
    gchar *tmpStr;
    LoanAssistantData *ldd = user_data;
    RepayOptData *rod;

    g_assert( ldd->currentIdx >= 0 );
    g_assert( ldd->currentIdx <= ldd->ld.repayOptCount );
    rod = ldd->ld.repayOpts[ ldd->currentIdx ];

    tmpStr = gtk_editable_get_chars( GTK_EDITABLE(ldd->payTxnName),
                                     0, -1 );
    if ( rod->txnMemo != NULL )
    {
        g_free( rod->txnMemo );
    }
    rod->txnMemo = tmpStr;
    tmpStr = NULL;

    tmpStr = gtk_editable_get_chars( GTK_EDITABLE(ldd->payAmtEntry),
                                     0, -1 );
    rod->amount = (float)strtod( tmpStr, NULL );
    g_free( tmpStr );

    rod->specSrcAcctP =
        gtk_toggle_button_get_active(
            GTK_TOGGLE_BUTTON(ldd->paySpecSrcAcct) );

    /* Test for Valid From Account */
    if ( rod->specSrcAcctP )
    {
        rod->from = gnc_account_sel_get_account( ldd->payAcctFromGAS );
        if ( rod->from == NULL )
            return FALSE;
    }

    /* Test for Valid To Account */
    rod->to = gnc_account_sel_get_account( ldd->payAcctToGAS );
    if ( rod->to == NULL )
        return FALSE;

    /* Set Page Valid */
    rod->optValid = TRUE;

    /* If Uniq Freq, then save to recurrence */
    if ( rod->FreqUniq )
    {
        if ( rod->startDate == NULL )
        {
            rod->startDate = g_date_new();
        }
        recurrenceListFree(&rod->schedule);
        gnc_frequency_save_to_recurrence(ldd->payGncFreq, &rod->schedule, rod->startDate);

        if (! rod->schedule )
        {
            return FALSE;
        }
    }
    return TRUE;
}

/************************************************************************/

static
void
loan_rev_prep( GtkAssistant *assistant, gpointer user_data )
{
    /* 3, here, does not include the Date column. */
    static const int BASE_COLS = 3;
    LoanAssistantData *ldd = user_data;
    GtkListStore *store;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GType *types;
    int i;
    int col = 1;

    /* Make sure we saved last Payment Option */
    if (ldd->currentIdx != -1)
        loan_pay_complete(assistant, ldd);

    /* Cleanup old view */
    if ( ldd->revView != NULL )
    {
        gtk_widget_destroy( GTK_WIDGET(ldd->revView) );
        ldd->revView = NULL;
    }

    ldd->ld.revNumPmts = BASE_COLS;
    /* Get the correct number of repayment columns. */
    for ( i = 0; i < ldd->ld.repayOptCount; i++ )
    {
        ldd->ld.revRepayOptToColMap[i] = -1;
        if ( ! ldd->ld.repayOpts[i]->enabled )
        {
            continue;
        }
        /* not '+1' = there is no date column to be accounted for in
         * the mapping. */
        ldd->ld.revRepayOptToColMap[i] = ldd->ld.revNumPmts;
        ldd->ld.revNumPmts += 1;
    }
    /* '+1' for leading date col */
    types = g_new( GType, ldd->ld.revNumPmts + 1 );
    for ( i = 0; i < ldd->ld.revNumPmts + 1; i++ )
        types[i] = G_TYPE_STRING;
    store = gtk_list_store_newv(ldd->ld.revNumPmts + 1, types);
    g_free(types);

    ldd->revView = GTK_TREE_VIEW(
                       gtk_tree_view_new_with_model( GTK_TREE_MODEL(store) ));
    g_object_unref(store);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Date"), renderer,
             "text", LOAN_COL_DATE,
             NULL);
    gtk_tree_view_append_column(ldd->revView, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Payment"), renderer,
             "text", LOAN_COL_PAYMENT,
             NULL);
    gtk_tree_view_append_column(ldd->revView, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Principal"), renderer,
             "text", LOAN_COL_PRINCIPAL,
             NULL);
    gtk_tree_view_append_column(ldd->revView, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Interest"), renderer,
             "text", LOAN_COL_INTEREST,
             NULL);
    gtk_tree_view_append_column(ldd->revView, column);

    /* move the appropriate names over into the title array */
    {
        for ( i = 0; i < ldd->ld.repayOptCount; i++ )
        {
            if ( ldd->ld.revRepayOptToColMap[i] == -1 )
            {
                continue;
            }
            renderer = gtk_cell_renderer_text_new();
            column = gtk_tree_view_column_new_with_attributes
                     (ldd->ld.repayOpts[i]->name, renderer,
                      "text", LOAN_COL_INTEREST + col,
                      NULL);
            gtk_tree_view_append_column(ldd->revView, column);
            col++;
        }
    }

    gtk_container_add( GTK_CONTAINER(ldd->revScrollWin),
                       GTK_WIDGET(ldd->revView) );
    gtk_widget_show( GTK_WIDGET(ldd->revView) );

    loan_rev_recalc_schedule( ldd );

    {
        GDate start, end;
        g_date_clear( &start, 1 );
        g_date_clear( &end, 1 );
        loan_rev_get_dates( ldd, &start, &end );
        loan_rev_update_view( ldd, &start, &end );
    }
}


static
void
loan_rev_range_opt_changed_cb( GtkComboBox *combo, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    int opt;

    opt = gtk_combo_box_get_active( ldd->revRangeOpt );
    gtk_widget_set_sensitive( GTK_WIDGET(ldd->revDateFrame),
                              (opt == CUSTOM) );
    {
        GDate start, end;
        g_date_clear( &start, 1 );
        g_date_clear( &end, 1 );
        loan_rev_get_dates( ldd, &start, &end );
        loan_rev_update_view( ldd, &start, &end );
    }
}


static
void
loan_rev_range_changed_cb( GNCDateEdit *gde, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    {
        GDate start, end;
        g_date_clear( &start, 1 );
        g_date_clear( &end, 1 );
        loan_rev_get_dates( ldd, &start, &end );
        loan_rev_update_view( ldd, &start, &end );
    }
}


static
void
loan_rev_get_loan_range( LoanAssistantData *ldd, GDate *start, GDate *end )
{
    int monthsTotal;
    struct tm *endDateMath;

    *start = *ldd->ld.startDate;

    endDateMath = g_new0( struct tm, 1 );
    g_date_to_struct_tm (ldd->ld.startDate, endDateMath);
    monthsTotal = ( (ldd->ld.numPer - 1)
                    * ( ldd->ld.perSize == GNC_MONTHS ? 1 : 12 ) );
    endDateMath->tm_mon += monthsTotal;
    gnc_gdate_set_time64 (end, gnc_mktime (endDateMath));
    g_free (endDateMath);
}


static
void
loan_rev_get_dates( LoanAssistantData *ldd, GDate *start, GDate *end )
{
    int range = gtk_combo_box_get_active( ldd->revRangeOpt );
    switch ( range )
    {
    case CURRENT_YEAR:
        gnc_gdate_set_time64( start, gnc_time (NULL) );
        g_date_set_dmy( start, 1, G_DATE_JANUARY, g_date_get_year( start ) );
        g_date_set_dmy( end, 31, G_DATE_DECEMBER, g_date_get_year( start ) );
        break;
    case NOW_PLUS_ONE:
        gnc_gdate_set_time64( start, gnc_time (NULL) );
        *end = *start;
        g_date_add_years( end, 1 );
        break;
    case WHOLE_LOAN:
        loan_rev_get_loan_range( ldd, start, end );
        break;
    case CUSTOM:
        gnc_gdate_set_time64( start,
                           gnc_date_edit_get_date( ldd->revStartDate ) );
        gnc_gdate_set_time64( end,
                           gnc_date_edit_get_date( ldd->revEndDate ) );
        break;
    default:
        PERR( "Unknown review date range option %d", range );
        break;
    }

}


static
void
loan_rev_sched_list_free( gpointer data, gpointer user_data )
{
    RevRepaymentRow *rrr = (RevRepaymentRow*)data;
    g_free( rrr->numCells );
    g_free( rrr );
}


static
void
loan_rev_hash_to_list( gpointer key, gpointer val, gpointer user_data )
{
    GList **l = (GList**)user_data;
    RevRepaymentRow *rrr = g_new0( RevRepaymentRow, 1 );
    if ( !key || !val )
    {
        DEBUG( "%.8x, %.8x",
               GPOINTER_TO_UINT(key),
               GPOINTER_TO_UINT(val));
        return;
    }
    rrr->date = *(GDate*)key;
    rrr->numCells = (gnc_numeric*)val;
    *l = g_list_append( *l, (gpointer)rrr );
}


static
void
loan_rev_hash_free_date_keys( gpointer key, gpointer val, gpointer user_data )
{
    g_free( (GDate*)key );
}


static
void
loan_rev_recalc_schedule( LoanAssistantData *ldd )
{
    GDate start, end;
    gnc_numeric *rowNumData;
    GHashTable *repayment_schedule;

    g_date_clear( &start, 1 );
    g_date_clear( &end, 1 );
    loan_rev_get_loan_range( ldd, &start, &end );

    /* The repayment_schedule is a hash of GDates to
     * row-of-gnc_numeric[N] data, where N is the number of columns as
     * determined by the _prep function, and stored in
     * LoanData::revNumPmts. */
    repayment_schedule = g_hash_table_new(gnc_gdate_hash, gnc_gdate_equal);

    /* Do the master repayment */
    {
        GDate curDate, nextDate;
        GString *pmtFormula, *ppmtFormula, *ipmtFormula;
        int i;
        GHashTable *ivar;

        pmtFormula = g_string_sized_new( 64 );
        loan_get_pmt_formula( ldd, pmtFormula );
        ppmtFormula = g_string_sized_new( 64 );
        loan_get_ppmt_formula( ldd, ppmtFormula );
        ipmtFormula = g_string_sized_new( 64 );
        loan_get_ipmt_formula( ldd, ipmtFormula );

        ivar = g_hash_table_new( g_str_hash, g_str_equal );
        g_date_clear( &curDate, 1 );
        curDate = start;
        g_date_subtract_days( &curDate, 1 );
        g_date_clear(&nextDate, 1);
        recurrenceListNextInstance(ldd->ld.repayment_schedule, &curDate, &nextDate);
        for ( i = 1;
                g_date_valid( &nextDate )
                && g_date_compare( &nextDate, &end ) <= 0 ;
                i++,
                curDate = nextDate,
                recurrenceListNextInstance(ldd->ld.repayment_schedule,
                                           &curDate, &nextDate))
        {
            gnc_numeric ival;
            gnc_numeric val;
            char *eloc;
            rowNumData =
                (gnc_numeric*)g_hash_table_lookup( repayment_schedule,
                                                   &curDate );
            if ( rowNumData == NULL)
            {
                int j;
                GDate *dateKeyCopy = g_date_new();

                *dateKeyCopy = curDate;
                rowNumData = g_new0( gnc_numeric, ldd->ld.revNumPmts );
                g_assert( rowNumData != NULL );
                for ( j = 0; j < ldd->ld.revNumPmts; j++ )
                {
                    rowNumData[j] = gnc_numeric_error( GNC_ERROR_ARG );
                }
                g_hash_table_insert( repayment_schedule,
                                     (gpointer)dateKeyCopy,
                                     (gpointer)rowNumData );
            }

            /* evaluate the expressions given the correct
             * sequence number i */
            ival = gnc_numeric_create( i, 1 );
            g_hash_table_insert( ivar, "i", &ival );

            if ( ! gnc_exp_parser_parse_separate_vars(
                        pmtFormula->str, &val, &eloc, ivar ) )
            {
                PERR( "pmt Parsing error at %s", eloc );
                continue;
            }
            val = gnc_numeric_convert( val, 100, GNC_HOW_RND_ROUND_HALF_UP );
            rowNumData[0] = val;

            if ( ! gnc_exp_parser_parse_separate_vars(
                        ppmtFormula->str, &val, &eloc, ivar ) )
            {
                PERR( "ppmt Parsing error at %s", eloc );
                continue;
            }
            val = gnc_numeric_convert( val, 100, GNC_HOW_RND_ROUND_HALF_UP );
            rowNumData[1] = val;

            if ( ! gnc_exp_parser_parse_separate_vars(
                        ipmtFormula->str, &val, &eloc, ivar ) )
            {
                PERR( "ipmt Parsing error at %s", eloc );
                continue;
            }
            val = gnc_numeric_convert( val, 100, GNC_HOW_RND_ROUND_HALF_UP );
            rowNumData[2] = val;
        }

        g_string_free( ipmtFormula, TRUE );
        g_string_free( ppmtFormula, TRUE );
        g_string_free( pmtFormula, TRUE );

        g_hash_table_destroy( ivar );
    }

    /* Process any other enabled payments. */
    {
        int i;
        GDate curDate, nextDate;
        GList *schedule;

        for ( i = 0; i < ldd->ld.repayOptCount; i++ )
        {
            if ( ! ldd->ld.repayOpts[i]->enabled )
                continue;

            schedule
            = ( ldd->ld.repayOpts[i]->schedule != NULL
                ? ldd->ld.repayOpts[i]->schedule
                : ldd->ld.repayment_schedule );

            g_date_clear( &curDate, 1 );
            curDate = start;
            g_date_subtract_days( &curDate, 1 );
            g_date_clear(&nextDate, 1);
            recurrenceListNextInstance(schedule, &curDate, &nextDate );
            for ( ; g_date_valid( &nextDate )
                    && g_date_compare( &nextDate, &end ) <= 0;
                    curDate = nextDate,
                    recurrenceListNextInstance(
                        schedule, &curDate, &nextDate ) )
            {
                gint gncn_how =
                    GNC_HOW_DENOM_SIGFIGS(2)
                    | GNC_HOW_RND_ROUND_HALF_UP;
                gnc_numeric val;
                rowNumData = (gnc_numeric*)g_hash_table_lookup( repayment_schedule,
                             &curDate );
                if ( rowNumData == NULL )
                {
                    int j;
                    GDate *dateKeyCopy = g_date_new();

                    *dateKeyCopy = curDate;
                    rowNumData = g_new0( gnc_numeric, ldd->ld.revNumPmts );
                    g_assert( rowNumData != NULL );
                    for ( j = 0; j < ldd->ld.revNumPmts; j++ )
                    {
                        rowNumData[j] = gnc_numeric_error( GNC_ERROR_ARG );
                    }
                    g_hash_table_insert( repayment_schedule,
                                         (gpointer)dateKeyCopy,
                                         (gpointer)rowNumData );
                }

                val = double_to_gnc_numeric( (double)ldd->ld
                                             .repayOpts[i]
                                             ->amount,
                                             100, gncn_how );
                rowNumData[ ldd->ld.revRepayOptToColMap[i] ]
                = val;
            }
        }
    }

    /* Convert the GHashTable into a sorted GList in the LoanData */
    {
        if ( ldd->ld.revSchedule != NULL )
        {
            g_list_foreach( ldd->ld.revSchedule,
                            loan_rev_sched_list_free,
                            NULL );
            g_list_free( ldd->ld.revSchedule );
            ldd->ld.revSchedule = NULL;
        }
        g_hash_table_foreach( repayment_schedule, loan_rev_hash_to_list,
                              &ldd->ld.revSchedule );
        g_hash_table_foreach( repayment_schedule, loan_rev_hash_free_date_keys,
                              NULL );
        g_hash_table_destroy( repayment_schedule );
        ldd->ld.revSchedule =
            g_list_sort( ldd->ld.revSchedule, (GCompareFunc)g_date_compare );
    }
}


static
void
loan_rev_update_view( LoanAssistantData *ldd, GDate *start, GDate *end )
{
    static gchar *NO_AMT_CELL_TEXT = " ";
    GList *l;
    GNCPrintAmountInfo pai;
    GtkListStore *store;
    GtkTreeIter iter;

    pai = gnc_default_price_print_info();
    pai.min_decimal_places = 2;

    store = GTK_LIST_STORE(gtk_tree_view_get_model( ldd->revView ));

    gtk_list_store_clear( store );

    for ( l = ldd->ld.revSchedule; l != NULL; l = l->next )
    {
        int i;
        gchar tmpBuf[50];
        RevRepaymentRow *rrr = (RevRepaymentRow*)l->data;

        if ( g_date_compare( &rrr->date, start ) < 0 )
            continue;
        if ( g_date_compare( &rrr->date, end ) > 0 )
            continue; /* though we can probably return, too. */

        gtk_list_store_append(store, &iter);

        qof_print_gdate( tmpBuf, MAX_DATE_LENGTH, &rrr->date );
        gtk_list_store_set( store, &iter, LOAN_COL_DATE, tmpBuf, -1 );

        for ( i = 0; i < ldd->ld.revNumPmts; i++ )
        {
            int numPrinted;
            if ( gnc_numeric_check( rrr->numCells[i] )
                    == GNC_ERROR_ARG )
            {
                /* '+1' for the date cell */
                gtk_list_store_set( store, &iter,
                                    i + 1, NO_AMT_CELL_TEXT,
                                    -1);
                continue;
            }

            numPrinted = xaccSPrintAmount( tmpBuf, rrr->numCells[i], pai );
            g_assert( numPrinted < 50 );
            /* '+1' for the date cell */
            gtk_list_store_set( store, &iter,
                                i + 1, tmpBuf,
                                -1);
        }

    }
}

/************************* Worker functions *****************************/

/* convert APR rate to simple rate based on formula r=q((1+i)^(1/q)-1) (r=interest rate, i=apr, q=compounding periods) */

gfloat loan_apr_to_simple_formula (gfloat rate, gfloat compounding_periods)
{
    /* float percent_to_frac; - redundant */
    gfloat simple_rate;
    /* percent_to_frac= compounding_periods/100; - redundant */
    simple_rate = compounding_periods * ((pow((1 + rate), (1 / compounding_periods))) - 1);
    return (simple_rate);
}

#define MAX_FORMULA 1024

static
void
loan_get_formula_internal( LoanAssistantData *ldd, GString *gstr, const gchar* template )
{
    gint rate_case;
    gfloat pass_thru_rate, period_rate;
    gfloat periods;
    gfloat principal;
    gchar formula[MAX_FORMULA];

    g_assert( ldd != NULL );
    g_assert( gstr != NULL );

    pass_thru_rate = ldd->ld.interestRate / 100;
    periods = (ldd->ld.numPer * (ldd->ld.perSize == GNC_MONTHS ? 1 : 12)) * 1.;
    principal = gnc_numeric_to_double(ldd->ld.principal);

    rate_case = ldd->ld.rateType;
    switch (rate_case)
    {
    case GNC_IRATE_SIMPLE:
        period_rate = pass_thru_rate;
        break;
    case GNC_IRATE_APR_DAILY:
        period_rate = loan_apr_to_simple_formula (pass_thru_rate, 365);
        break;
    case GNC_IRATE_APR_WEEKLY:
        period_rate = loan_apr_to_simple_formula (pass_thru_rate, 52);
        break;
    case GNC_IRATE_APR_MONTHLY:
        period_rate = loan_apr_to_simple_formula (pass_thru_rate, 12);
        break;
    case GNC_IRATE_APR_QUARTERLY:
        period_rate = loan_apr_to_simple_formula (pass_thru_rate, 4);
        break;
    case GNC_IRATE_APR_ANNUALLY:
        period_rate = loan_apr_to_simple_formula (pass_thru_rate, 1);
        break;
    default:
        period_rate = ldd->ld.interestRate / 100;
        break;
    }

    if (0 < strfmon (formula, MAX_FORMULA, template,
                     period_rate, 12.0, periods, principal ))
        g_string_append (gstr, formula);
}


static
void
loan_get_pmt_formula( LoanAssistantData *ldd, GString *gstr )
{
    loan_get_formula_internal (ldd, gstr, "pmt( %!.5i / %!0.2i : %!0.2i : %!0.2i : 0 : 0 )");
}


static
void
loan_get_ppmt_formula( LoanAssistantData *ldd, GString *gstr )
{
    loan_get_formula_internal (ldd, gstr, "ppmt( %!.5i / %!0.2i : i : %!0.2i : %!0.2i : 0 : 0 )");
}


static
void
loan_get_ipmt_formula( LoanAssistantData *ldd, GString *gstr )
{
    loan_get_formula_internal (ldd, gstr, "ipmt( %!.5i / %!0.2i : i : %!0.2i : %!0.2i : 0 : 0 )");
}

/******************* Scheduled Transaction Functions ********************/

static int
ld_calc_sx_instance_num(GDate *start_date, GList *schedule)
{
    int instance_count;
    GDate next_date, today;

    g_date_clear(&next_date, 1);
    g_date_clear(&today, 1);
    gnc_gdate_set_time64 (&today, gnc_time (NULL));

    if (g_date_compare(start_date, &today) > 0)
        return 0;

    instance_count = -1;
    do
    {
        instance_count++;
        recurrenceListNextInstance(schedule, start_date, &next_date);
    }
    while (g_date_compare(&next_date, &today) < 0);

    return instance_count;
}


static
void
loan_tcSX_free( gpointer data, gpointer user_data )
{
    toCreateSX *tcSX = (toCreateSX*)data;
    g_free( tcSX->name );
    if ( tcSX->mainTxn )
        gnc_ttinfo_free( tcSX->mainTxn );
    if ( tcSX->escrowTxn )
        gnc_ttinfo_free( tcSX->escrowTxn );
    g_free( tcSX );
}


/**
 * Custom GCompareFunc to find the element of a GList of TTSplitInfo's which
 * has the given [Account*] criteria.
 * @return 0 if match, as per GCompareFunc in the g_list_find_custom context.
 **/
static
gint
loan_find_ttsplit_with_acct( gconstpointer elt,
                             gconstpointer crit )
{
    TTSplitInfo *ttsi = (TTSplitInfo*)elt;
    return ( (gnc_ttsplitinfo_get_account( ttsi )
              == (Account*)crit) ? 0 : 1 );
}


/**
 * Enters into the books a Scheduled Transaction from the given toCreateSX.
 **/
static
void
loan_create_sx_from_tcSX( LoanAssistantData *ldd, toCreateSX *tcSX )
{
    SchedXaction *sx;
    SchedXactions *sxes;
    GList *ttxnList;

    sx = xaccSchedXactionMalloc( gnc_get_current_book() );
    xaccSchedXactionSetName( sx, tcSX->name );
    gnc_sx_set_schedule(sx, tcSX->schedule);
    xaccSchedXactionSetStartDate( sx, &tcSX->start );
    xaccSchedXactionSetLastOccurDate( sx, &tcSX->last );
    xaccSchedXactionSetEndDate( sx, &tcSX->end );
    gnc_sx_set_instance_count( sx, tcSX->instNum );

    ttxnList = NULL;
    if ( tcSX->mainTxn )
        ttxnList = g_list_append( ttxnList, tcSX->mainTxn );
    if ( tcSX->escrowTxn )
        ttxnList = g_list_append( ttxnList, tcSX->escrowTxn );

    g_assert( ttxnList != NULL );

    xaccSchedXactionSetTemplateTrans( sx, ttxnList,
                                      gnc_get_current_book() );

    sxes = gnc_book_get_schedxactions(gnc_get_current_book());
    gnc_sxes_add_sx(sxes, sx);
    g_list_free( ttxnList );
    ttxnList = NULL;
}


/**
 * Does the work to setup the given toCreateSX structure for a specific
 * repayment.  Note that if the RepayOptData doesn't specify a unique
 * schedule, the paymentSX and the tcSX parameters will be the same.
 **/
static
void
ld_setup_repayment_sx( LoanAssistantData *ldd,
                       RepayOptData *rod,
                       toCreateSX *paymentSX,
                       toCreateSX *tcSX )
{
    /* In DoubleEntryAccounting-ease, this is what we're going to do,
     * below...
     *
     * if ( rep->escrow ) {
     *   if ( rep->from ) {
     *      a: paymentSX.main.splits += split( rep->fromAcct, repAmt )
     *      b: paymentSX.main.split( ldd->ld.escrowAcct ).debCred += repAmt
     *         tcSX.escrow.split( rep->escrow ).debCred += repAmt
     *     c1: tcSX.escrow.splits += split( rep->toAcct, +repAmt )
     *   } else {
     *      d: paymentSX.main.split( ldd->ld.repFromAcct ).debcred += -repAmt
     *      b: paymentSX.main.split( ldd->ld.escrowAcct ).debCred += repAmt
     *         tcSX.escrow.splits += split( rep->escrow, -repAmt )
     *     c1: tcSX.escrow.splits += split( rep->toAcct, +repAmt )
     *   }
     * } else {
     *   if ( rep->from ) {
     *      a: paymentSX.main.splits += split( rep->fromAcct, -repAmt )
     *     c2: paymentSX.main.splits += split( rep->toAcct,   +repAmt )
     *   } else {
     *      d: paymentSX.main.split( ldd->ld.payFromAcct ).debcred += -repAmt
     *     c2: paymentSX.main.splits += split( rep->toAcct, +repAmt )
     *   }
     * }
     */

    /* Now, we refactor the common operations from the above out...
     *
     * fromSplit = NULL;
     * if ( rep->escrow ) {
     *   b: paymentSX.main.split( ldd->ld.escrowAcct ).debCred += repAmt
     *  c1: ( toTTI = tcSX.escrow )
     *   if ( rep->from ) {
     *     a1: (fromSplit = NULL) paymentSX.main.splits += split( rep->fromAcct, repAmt )
     *      b:
     *         tcSX.escrow.split( rep->escrow ).debCred += repAmt
     *     c1:
     *   } else {
     *     a2: (fromSplit = paymentSX.main.split( ldd->ld.repFromAcct )) .debcred += -repAmt
     *      b:
     *         tcSX.escrow.splits += split( rep->escrow, -repAmt )
     *     c1:
     *   }
     * } else {
     *   c2: ( toTTI = paymentSX.main )
     *   if ( rep->from ) {
     *     a1: (fromSplit = NULL) paymentSX.main.splits += split( rep->fromAcct, -repAmt )
     *     c2:
     *   } else {
     *     a2: (fromSplit = paymentSX.main.split( ldd->ld.payFromAcct )).debcred += -repAmt
     *     c2:
     *   }
     * }
     * if ( fromSplit ) {
     *   fromSplit.debCred += (-repAmt);
     * } else {
     *   fromSplit = split( rep->fromAcct, -repAmt )
     *   paymentSX.main.splits += fromSplit
     * }
     * toTTI.splits += split( rep->toAcct, +repAmt );
     */

    /** Now, the actual implementation... */

    GString *gstr;
    GList *elt;
    TTSplitInfo *fromSplit = NULL;
    TTSplitInfo *ttsi;
    TTInfo *toTxn = NULL;
    GNCPrintAmountInfo pricePAI = gnc_default_price_print_info();
#define AMTBUF_LEN 64
    gchar amtBuf[AMTBUF_LEN];
    gint GNCN_HOW = (GNC_HOW_DENOM_SIGFIGS(2) | GNC_HOW_RND_ROUND_HALF_UP);

    /* We're going to use this a lot, below, so just create it once. */
    xaccSPrintAmount( amtBuf,
                      double_to_gnc_numeric( rod->amount, 100,
                              GNCN_HOW ),
                      pricePAI );

    if ( rod->throughEscrowP && ldd->ld.escrowAcct )
    {
        toTxn = tcSX->escrowTxn;

        /* Add the repayment amount into the string of the existing
         * ttsplit. */
        {
            elt = g_list_find_custom(
                      gnc_ttinfo_get_template_splits( paymentSX->mainTxn ),
                      ldd->ld.escrowAcct,
                      loan_find_ttsplit_with_acct );
            g_assert( elt );
            ttsi = (TTSplitInfo*)elt->data;
            g_assert( ttsi );
            gstr = g_string_new( gnc_ttsplitinfo_get_debit_formula( ttsi ) );
            g_string_append_printf( gstr, " + %s", amtBuf );
            gnc_ttsplitinfo_set_debit_formula( ttsi, gstr->str );
            g_string_free( gstr, TRUE );
            gstr = NULL;
            ttsi = NULL;
        }

        if ( rod->from != NULL )
        {
            gchar *str;

            fromSplit = NULL;

            /* tcSX.escrow.split( rep->escrow ).debCred += repAmt */
            elt = g_list_find_custom(
                      gnc_ttinfo_get_template_splits( tcSX->escrowTxn ),
                      ldd->ld.escrowAcct,
                      loan_find_ttsplit_with_acct );
            ttsi = NULL;
            if ( elt )
            {
                ttsi = (TTSplitInfo*)elt->data;
            }
            if ( !ttsi )
            {
                /* create split */
                ttsi = gnc_ttsplitinfo_malloc();
                gnc_ttsplitinfo_set_memo( ttsi, rod->txnMemo );
                gnc_ttsplitinfo_set_account( ttsi, ldd->ld.escrowAcct );
                gnc_ttinfo_append_template_split( tcSX->escrowTxn, ttsi );
            }
            if ( (str = (gchar*)gnc_ttsplitinfo_get_credit_formula( ttsi ))
                    == NULL )
            {
                gstr = g_string_sized_new( 16 );
            }
            else
            {
                /* If we did get a split/didn't have to
                 * create a split, then we need to add our
                 * amount in rather than replace. */
                gstr = g_string_new( str );
                g_string_append_printf( gstr, " + " );
            }
            g_string_append_printf( gstr, "%s", amtBuf );
            gnc_ttsplitinfo_set_credit_formula( ttsi, gstr->str );
            g_string_free( gstr, TRUE );
            gstr = NULL;
            ttsi = NULL;
        }
        else
        {
            /* (fromSplit = paymentSX.main.split( ldd->ld.repFromAcct )) */
            elt = g_list_find_custom(
                      gnc_ttinfo_get_template_splits( paymentSX->mainTxn ),
                      ldd->ld.repFromAcct,
                      loan_find_ttsplit_with_acct );
            g_assert( elt );
            fromSplit = (TTSplitInfo*)elt->data;

            /* tcSX.escrow.splits += split( rep->escrow, -repAmt ) */
            ttsi = gnc_ttsplitinfo_malloc();
            gnc_ttsplitinfo_set_memo( ttsi, rod->txnMemo );
            gnc_ttsplitinfo_set_account( ttsi, ldd->ld.escrowAcct );
            gnc_ttsplitinfo_set_credit_formula( ttsi, amtBuf );
            gnc_ttinfo_append_template_split( tcSX->escrowTxn, ttsi );
            ttsi = NULL;
        }
    }
    else
    {
        toTxn = tcSX->mainTxn;

        if ( rod->from != NULL )
        {
            fromSplit = NULL;
        }
        else
        {
            /* (fromSplit = paymentSX.main.split( ldd->ld.repFromAcct )) */
            elt = g_list_find_custom(
                      gnc_ttinfo_get_template_splits( tcSX->mainTxn ),
                      ldd->ld.repFromAcct,
                      loan_find_ttsplit_with_acct );
            fromSplit = NULL;
            if ( elt )
            {
                /* This is conditionally true in the case of
                 * a repayment on it's own schedule. */
                fromSplit = (TTSplitInfo*)elt->data;
            }
        }
    }

    if ( fromSplit != NULL )
    {
        /* Update the existing from-split. */
        gstr = g_string_new( gnc_ttsplitinfo_get_credit_formula( fromSplit ) );
        g_string_append_printf( gstr, " + %s", amtBuf );
        gnc_ttsplitinfo_set_credit_formula( fromSplit, gstr->str );
        g_string_free( gstr, TRUE );
        gstr = NULL;

    }
    else
    {
        TTInfo *tti;
        /* Create a new from-split. */
        ttsi = gnc_ttsplitinfo_malloc();
        gnc_ttsplitinfo_set_memo( ttsi, rod->txnMemo );
        if ( rod->from )
        {
            gnc_ttsplitinfo_set_account( ttsi, rod->from );
        }
        else
        {
            gnc_ttsplitinfo_set_account( ttsi, ldd->ld.repFromAcct );
        }
        gnc_ttsplitinfo_set_credit_formula( ttsi, amtBuf );
        tti = tcSX->mainTxn;
        if ( rod->throughEscrowP )
        {
            tti = paymentSX->mainTxn;
        }
        gnc_ttinfo_append_template_split( tti, ttsi );
        ttsi = NULL;
        tti  = NULL;
    }

    /* Add to-account split. */
    {
        ttsi = gnc_ttsplitinfo_malloc();
        gnc_ttsplitinfo_set_memo( ttsi, rod->txnMemo );
        gnc_ttsplitinfo_set_account( ttsi, rod->to );
        gnc_ttsplitinfo_set_debit_formula( ttsi, amtBuf );
        gnc_ttinfo_append_template_split( toTxn, ttsi );
        ttsi = NULL;
    }
}


/**
 * Actually does the heavy-lifting of creating the SXes from the
 * LoanAssistantData.
 *
 * Rules:
 * - There is at least one SX created, with at least one txn, for the loan
 *   payment itself.
 * - A new SX is created for each repayment with a different frequency.
 * - Non-unique repayment From-accounts cause a "summed (src-)split", unique
 *   repayment From-accounts cause new (src-)splits.
 * - Each repayment causes a new (dst-)split [the To-account].
 * - Escrow-diverted repayments cause new Txns w/in their
 *   SX. [Assets->Escrow, Escrow->(Expense|Liability)]
 **/
static
void
loan_create_sxes( LoanAssistantData *ldd )
{
    /* The main loan-payment SX.*/
    toCreateSX *paymentSX = NULL;
    /* A GList of any other repayment SXes with different schedule. */
    GList *repaySXes = NULL;
    /* The currently-being-referenced toCreateSX. */
    toCreateSX *tcSX;
    int i;
    TTInfo *ttxn;
    TTSplitInfo *ttsi;
    GString *gstr;

    paymentSX = g_new0( toCreateSX, 1 );
    paymentSX->name  = g_strdup(ldd->ld.repMemo);
    paymentSX->start = *ldd->ld.startDate;
    paymentSX->last  = *ldd->ld.repStartDate;
    g_date_subtract_months( &paymentSX->last, 1 );
    {
        paymentSX->end = *ldd->ld.repStartDate;
        g_date_add_months( &paymentSX->end, ldd->ld.numMonRemain - 1);
    }

    paymentSX->schedule = ldd->ld.repayment_schedule;
    /* Figure out the correct current instance-count for the txns in the
     * SX. */
    paymentSX->instNum =
        (ldd->ld.numPer * ( ldd->ld.perSize == GNC_YEARS ? 12 : 1 ))
        - ldd->ld.numMonRemain + 1;

    paymentSX->mainTxn = gnc_ttinfo_malloc();
    gnc_ttinfo_set_currency( paymentSX->mainTxn,
                             gnc_default_currency() );

    {
        GString *payMainTxnDesc = g_string_sized_new( 32 );
        g_string_printf( payMainTxnDesc,
                         "%s - %s",
                         ldd->ld.repMemo,
                         ( ldd->ld.escrowAcct == NULL
                           ? _("Payment")
                           : _("Escrow Payment") )
                       );

        gnc_ttinfo_set_description( paymentSX->mainTxn,
                                    payMainTxnDesc->str );
        g_string_free( payMainTxnDesc, TRUE );
    }

    /* Create the basic txns and splits...
     *
     * ttxn <- mainTxn
     * srcAcct <- assets
     * if ( escrow ) {
     *  realSrcAcct <- srcAcct
     *  srcAcct     <- escrow;
     *  ttxn <- escrowTxn
     *  main.splits += split( realSrcAcct, -pmt )
     *  main.splits += split( escrow,       pmt )
     * }
     * ttxn.splits += split( escrow,            -pmt)
     * ttxn.splits += split( liability,          ppmt )
     * ttxn.splits += split( expenses:interest,  ipmt ) */

    {
        Account *srcAcct;

        ttxn = paymentSX->mainTxn;
        srcAcct = ldd->ld.repFromAcct;
        if ( ldd->ld.escrowAcct != NULL )
        {
            Account *realSrcAcct = srcAcct;
            srcAcct = ldd->ld.escrowAcct;
            gstr = g_string_sized_new( 32 );
            loan_get_pmt_formula( ldd, gstr );
            /* ttxn.splits += split( realSrcAcct, -pmt ); */
            {
                ttsi = gnc_ttsplitinfo_malloc();
                gnc_ttsplitinfo_set_memo( ttsi, ldd->ld.repMemo );
                gnc_ttsplitinfo_set_account( ttsi, realSrcAcct );
                gnc_ttsplitinfo_set_credit_formula( ttsi, gstr->str );
                gnc_ttinfo_append_template_split( ttxn, ttsi );
                ttsi = NULL;
            }
            /* ttxn.splits += split( escrowAcct, +pmt ); */
            {
                ttsi = gnc_ttsplitinfo_malloc();
                gnc_ttsplitinfo_set_memo( ttsi, ldd->ld.repMemo );
                gnc_ttsplitinfo_set_account( ttsi, ldd->ld.escrowAcct );
                gnc_ttsplitinfo_set_debit_formula( ttsi, gstr->str );
                gnc_ttinfo_append_template_split( ttxn, ttsi );
                ttsi = NULL;
            }
            g_string_free( gstr, TRUE );
            gstr = NULL;
            paymentSX->escrowTxn = gnc_ttinfo_malloc();
            gnc_ttinfo_set_currency( paymentSX->escrowTxn,
                                     gnc_default_currency() );

            {
                GString *escrowTxnDesc;
                escrowTxnDesc = g_string_new( ldd->ld.repMemo );
                g_string_append_printf( escrowTxnDesc, " - %s", _("Payment") );
                gnc_ttinfo_set_description( paymentSX->escrowTxn,
                                            escrowTxnDesc->str );
                g_string_free( escrowTxnDesc, TRUE );
            }
            ttxn = paymentSX->escrowTxn;
        }
        /* ttxn.splits += split( srcAcct, -pmt ); */
        {
            ttsi = gnc_ttsplitinfo_malloc();
            {
                gstr = g_string_new( ldd->ld.repMemo );
                g_string_append_printf( gstr, " - %s",
                                        _("Payment") );
                gnc_ttsplitinfo_set_memo( ttsi, gstr->str );
                g_string_free( gstr, TRUE );
                gstr = NULL;
            }
            gnc_ttsplitinfo_set_account( ttsi, srcAcct );
            gstr = g_string_sized_new( 32 );
            loan_get_pmt_formula( ldd, gstr );
            gnc_ttsplitinfo_set_credit_formula( ttsi, gstr->str );
            gnc_ttinfo_append_template_split( ttxn, ttsi );
            g_string_free( gstr, TRUE );
            gstr = NULL;
            ttsi = NULL;
        }
        /* ttxn.splits += split( ldd->ld.repPriAcct, +ppmt ); */
        {
            ttsi = gnc_ttsplitinfo_malloc();
            {
                gstr = g_string_new( ldd->ld.repMemo );
                g_string_append_printf( gstr, " - %s",
                                        _("Principal") );
                gnc_ttsplitinfo_set_memo( ttsi, gstr->str );
                g_string_free( gstr, TRUE );
                gstr = NULL;
            }
            gnc_ttsplitinfo_set_account( ttsi, ldd->ld.repPriAcct );
            gstr = g_string_sized_new( 32 );
            loan_get_ppmt_formula( ldd, gstr );
            gnc_ttsplitinfo_set_debit_formula( ttsi, gstr->str );
            gnc_ttinfo_append_template_split( ttxn, ttsi );
            g_string_free( gstr, TRUE );
            gstr = NULL;
            ttsi = NULL;
        }
        /* ttxn.splits += split( ldd->ld.repIntAcct, +ipmt ); */
        {
            ttsi = gnc_ttsplitinfo_malloc();
            {
                gstr = g_string_new( ldd->ld.repMemo );
                g_string_append_printf( gstr, " - %s",
                                        _("Interest") );
                gnc_ttsplitinfo_set_memo( ttsi, gstr->str );
                g_string_free( gstr, TRUE );
                gstr = NULL;
            }
            gnc_ttsplitinfo_set_account( ttsi, ldd->ld.repIntAcct );
            gstr = g_string_sized_new( 32 );
            loan_get_ipmt_formula( ldd, gstr );
            gnc_ttsplitinfo_set_debit_formula( ttsi, gstr->str );
            gnc_ttinfo_append_template_split( ttxn, ttsi );
            g_string_free( gstr, TRUE );
            gstr = NULL;
            ttsi = NULL;
        }
    }
    for ( i = 0; i < ldd->ld.repayOptCount; i++ )
    {
        RepayOptData *rod = ldd->ld.repayOpts[i];
        if ( ! rod->enabled )
            continue;

        tcSX = paymentSX;
        if ( rod->schedule != NULL )
        {
            tcSX = g_new0( toCreateSX, 1 );
            gstr = g_string_new( ldd->ld.repMemo );
            g_string_append_printf( gstr, " - %s",
                                    rod->name );
            tcSX->name    = g_strdup(gstr->str);
            tcSX->start   = *ldd->ld.startDate;
            tcSX->last    = *ldd->ld.repStartDate;
            {
                tcSX->end = tcSX->last;
                g_date_add_months( &tcSX->end, ldd->ld.numMonRemain );
            }
            tcSX->schedule = rod->schedule;
            /* So it won't get destroyed when the close the
             * Assistant. */
            tcSX->instNum =
                ld_calc_sx_instance_num(&tcSX->start, rod->schedule);
            rod->schedule = NULL;
            tcSX->mainTxn = gnc_ttinfo_malloc();
            gnc_ttinfo_set_currency( tcSX->mainTxn,
                                     gnc_default_currency() );
            gnc_ttinfo_set_description( tcSX->mainTxn,
                                        gstr->str );
            tcSX->escrowTxn = gnc_ttinfo_malloc();
            gnc_ttinfo_set_currency( tcSX->escrowTxn,
                                     gnc_default_currency() );
            gnc_ttinfo_set_description( tcSX->escrowTxn,
                                        gstr->str );

            g_string_free( gstr, TRUE );
            gstr = NULL;

            repaySXes = g_list_append( repaySXes, tcSX );

        }

        /* repayment */
        ld_setup_repayment_sx( ldd, rod, paymentSX, tcSX );
    }
    /* Create the SXes */
    {
        GList *l;

        loan_create_sx_from_tcSX( ldd, paymentSX );

        for ( l = repaySXes; l; l = l->next )
        {
            loan_create_sx_from_tcSX( ldd, (toCreateSX*)l->data );
        }
    }
    /* Clean up. */
    loan_tcSX_free( paymentSX, NULL );
    g_list_foreach( repaySXes, loan_tcSX_free, NULL );
    g_list_free( repaySXes );
}

/************************ Assistant Functions ***************************/

void
loan_assistant_finish ( GtkAssistant *gtkassistant, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    loan_create_sxes( ldd );

}


void
loan_assistant_cancel( GtkAssistant *gtkassistant, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    gnc_close_gui_component_by_data( DIALOG_LOAN_ASSISTANT_CM_CLASS, ldd );
}


void
loan_assistant_close( GtkAssistant *gtkassistant, gpointer user_data )
{
    LoanAssistantData *ldd = user_data;
    gnc_close_gui_component_by_data( DIALOG_LOAN_ASSISTANT_CM_CLASS, ldd );
}


void
loan_assistant_prepare (GtkAssistant  *assistant, GtkWidget *page,
                        gpointer user_data)
{
    gint currentpage = gtk_assistant_get_current_page(assistant);

    switch (currentpage)
    {
    case 1:
        /* Current page is info page */
        loan_info_prep (assistant, user_data);
        break;
    case 2:
        /* Current page is Options page */
        loan_opt_prep (assistant, user_data);
        break;
    case 3:
        /* Current page is Repayments page */
        loan_rep_prep (assistant, user_data);
        break;
    case 4:
        /* Current page is Repayments Options page */
        loan_pay_prep (assistant, user_data);
        break;
    case 5:
        /* Current page is Review page */
        loan_rev_prep (assistant, user_data);
        break;
    }
}


/********************************************************************\
 * gnc_ui_sx_loan_assistant_create                                  *
 *   opens up a window to start the loan Assistant                   *
 *                                                                  *
\********************************************************************/
void
gnc_ui_sx_loan_assistant_create (void)
{
    LoanAssistantData *ldd;
    gint component_id;

    ldd = g_new0 (LoanAssistantData, 1);

    gnc_loan_assistant_create (ldd);

    component_id = gnc_register_gui_component (DIALOG_LOAN_ASSISTANT_CM_CLASS,
                   NULL, loan_assistant_close_handler,
                   ldd);

    gnc_gui_component_watch_entity_type (component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    gtk_widget_show_all (ldd->window);

    gnc_window_adjust_for_screen (GTK_WINDOW(ldd->window));
}

