/********************************************************************\
 * dialog-hbcitrans.c -- dialog for hbci transaction                *
 * Copyright (C) 2002 Christian Stimming                            *
 * Copyright (C) 2004 Bernd Wagner (changes for                     *
 *                     online transaction templates)                *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <gnome.h>
#include <glib/gi18n.h>
#include <aqbanking/version.h>
#include <aqbanking/account.h>
#include <aqbanking/jobsingletransfer.h>
#include <aqbanking/jobsingledebitnote.h>
#include <aqbanking/jobinternaltransfer.h>
#include <iconv.h>

#include "dialog-utils.h"
#include "gnc-glib-utils.h"
#include "gnc-ui.h"
#include "gnc-amount-edit.h"
#include "dialog-transfer.h"

#include "gnc-hbci-utils.h"
#include "gnc-hbci-trans-templ.h"
#include "dialog-hbcitrans.h"
#if HAVE_KTOBLZCHECK_H
#  include <ktoblzcheck.h>
#endif

#define TEMPLATE_LABEL "template"

typedef enum
{
    TEMPLATE_NAME,
    TEMPLATE_POINTER,
    TEMPLATE_NUM_COLUMNS
} TemplateListColumns;

/* -------------------------------------- */
/* Data structure */
/* -------------------------------------- */
struct _trans_data
{
    /* The dialog itself */
    GtkWidget *dialog;
    GtkWidget *parent;

    /* Whether this is a transfer or a direct debit */
    GNC_HBCI_Transtype trans_type;

    /* Recipient */
    GtkWidget *recp_name_entry;
    GtkWidget *recp_account_entry;
    GtkWidget *recp_bankcode_entry;

    /* Amount */
    GtkWidget *amount_edit;

    /* Purpose, description */
    GtkWidget *purpose_entry;
    GtkWidget *purpose_cont_entry;
    GtkWidget *purpose_cont2_entry;
    GtkWidget *purpose_cont3_entry;

    /* Recipient's bank name (may be filled in automatically sometime later) */
    GtkWidget *recp_bankname_label;

    /* The template choosing GtkTreeView/GtkListStore */
    GtkTreeView *template_gtktreeview;
    GtkListStore *template_list_store;

    /* Flag, if template list has been changed */
    gboolean templ_changed;

    /* The HBCI transaction that got created here */
    AB_TRANSACTION *hbci_trans;

    /* The gnucash transaction dialog where the user specifies the gnucash transaction. */
    XferDialog *gnc_trans_dialog;

    /* The Gnucash transaction that got created here */
    Transaction *gnc_trans;

#if HAVE_KTOBLZCHECK_H
    /* object for Account number checking */
    AccountNumberCheck *blzcheck;
#endif
};


void gnc_hbci_dialog_delete(HBCITransDialog *td)
{
    if (!td) return;
    /* Unregister handler for transaction creation callback */
    if (td->gnc_trans_dialog)
        gnc_xfer_dialog_set_txn_cb(td->gnc_trans_dialog, NULL, NULL);
    if (td->hbci_trans)
        AB_Transaction_free (td->hbci_trans);

    if (td->dialog)
        gtk_widget_destroy (GTK_WIDGET (td->dialog));
#if HAVE_KTOBLZCHECK_H
    AccountNumberCheck_delete(td->blzcheck);
#endif
}

static gboolean
get_templ_helper (GtkTreeModel *model,
                  GtkTreePath *path,
                  GtkTreeIter *iter,
                  gpointer data)
{
    GList **list = data;
    GNCTransTempl *templ;

    gtk_tree_model_get (model, iter,
                        TEMPLATE_POINTER, &templ,
                        -1);
    *list = g_list_append(*list, templ);
    return FALSE; /* continue */
}

GList *gnc_hbci_dialog_get_templ(const HBCITransDialog *td)
{
    GList *list = NULL;

    g_assert(td);
    gtk_tree_model_foreach(GTK_TREE_MODEL(td->template_list_store),
                           get_templ_helper, &list);
    return list;
}
GtkWidget *gnc_hbci_dialog_get_parent(const HBCITransDialog *td)
{
    g_assert(td);
    return td->parent;
}
const AB_TRANSACTION *gnc_hbci_dialog_get_htrans(const HBCITransDialog *td)
{
    g_assert(td);
    return td->hbci_trans;
}
Transaction *gnc_hbci_dialog_get_gtrans(const HBCITransDialog *td)
{
    g_assert(td);
    return td->gnc_trans;
}
gboolean gnc_hbci_dialog_get_templ_changed(const HBCITransDialog *td)
{
    g_assert(td);
    return td->templ_changed;
}
void gnc_hbci_dialog_hide(HBCITransDialog *td)
{
    g_assert(td);
    gtk_widget_hide_all (td->dialog);
}
void gnc_hbci_dialog_show(HBCITransDialog *td)
{
    g_assert(td);
    gtk_widget_show_all (td->dialog);
}


/* -------------------------------------- */
/* Prototypes; callbacks for dialog function */
/* -------------------------------------- */

AB_TRANSACTION *
hbci_trans_fill_values(const AB_ACCOUNT *h_acc, HBCITransDialog *td);
gboolean
check_ktoblzcheck(GtkWidget *parent, const HBCITransDialog *td,
                  const AB_TRANSACTION *trans);

void on_template_list_selection_changed(GtkTreeSelection *selection, gpointer user_data);

void template_selection_cb(GtkButton *b, gpointer user_data);
void add_template_cb(GtkButton *b, gpointer user_data);
void moveup_template_cb(GtkButton *button, gpointer user_data);
void movedown_template_cb(GtkButton *button, gpointer user_data);
void sort_template_cb(GtkButton *button, gpointer user_data);
void del_template_cb(GtkButton *button, gpointer user_data);

void blz_changed_cb(GtkEditable *e, gpointer user_data);



/* -------------------------------------- */
/* Main dialog function */
/* -------------------------------------- */

/* doesn't exist any longer */

/* ************************************************************
 * constructor
 */

static void fill_template_list_func(gpointer data, gpointer user_data)
{
    GNCTransTempl *templ = data;
    GtkListStore *list_store = user_data;
    GtkTreeIter iter;

    g_assert(templ);
    g_assert(list_store);

    gtk_list_store_append(list_store, &iter);
    gtk_list_store_set(list_store, &iter,
                       TEMPLATE_NAME, gnc_trans_templ_get_name(templ),
                       TEMPLATE_POINTER, templ,
                       -1);
}

HBCITransDialog *
gnc_hbci_dialog_new (GtkWidget *parent,
                     const AB_ACCOUNT *h_acc,
                     Account *gnc_acc,
                     GNC_HBCI_Transtype trans_type,
                     GList *templates)
{
    GladeXML *xml;
    const char *hbci_bankid;
    HBCITransDialog *td;
    GtkTreeSelection *selection;
    GtkTreeViewColumn *column;
    GtkCellRenderer *renderer;

    td = g_new0(HBCITransDialog, 1);

    td->parent = parent;
    td->trans_type = trans_type;
    g_assert (h_acc);
    hbci_bankid = AB_Account_GetBankCode(h_acc);
#if HAVE_KTOBLZCHECK_H
    td->blzcheck = AccountNumberCheck_new();
#endif

    xml = gnc_glade_xml_new ("hbci.glade", "HBCI_trans_dialog");

    td->dialog = glade_xml_get_widget (xml, "HBCI_trans_dialog");

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW (td->dialog),
                                      GTK_WINDOW (parent));

    {
        gchar *hbci_bankname, *hbci_ownername;
        GtkWidget *heading_label;
        GtkWidget *recp_name_heading;
        GtkWidget *recp_account_heading;
        GtkWidget *recp_bankcode_heading;
        GtkWidget *amount_hbox;
        GtkWidget *orig_name_label;
        GtkWidget *orig_account_label;
        GtkWidget *orig_bankname_label;
        GtkWidget *orig_bankcode_label;
        GtkWidget *orig_name_heading;
        GtkWidget *orig_account_heading;
        GtkWidget *orig_bankname_heading;
        GtkWidget *orig_bankcode_heading;
        GtkWidget *exec_later_button;
        GtkWidget *add_templ_button;
        GtkWidget *moveup_templ_button;
        GtkWidget *movedown_templ_button;
        GtkWidget *sort_templ_button;
        GtkWidget *del_templ_button;

        g_assert
        ((heading_label = glade_xml_get_widget (xml, "heading_label")) != NULL);
        g_assert
        ((td->recp_name_entry = glade_xml_get_widget (xml, "recp_name_entry")) != NULL);
        g_assert
        ((recp_name_heading = glade_xml_get_widget (xml, "recp_name_heading")) != NULL);
        g_assert
        ((td->recp_account_entry = glade_xml_get_widget (xml, "recp_account_entry")) != NULL);
        g_assert
        ((recp_account_heading = glade_xml_get_widget (xml, "recp_account_heading")) != NULL);
        g_assert
        ((td->recp_bankcode_entry = glade_xml_get_widget (xml, "recp_bankcode_entry")) != NULL);
        g_assert
        ((recp_bankcode_heading = glade_xml_get_widget (xml, "recp_bankcode_heading")) != NULL);
        g_assert
        ((td->recp_bankname_label = glade_xml_get_widget (xml, "recp_bankname_label")) != NULL);
        g_assert
        ((amount_hbox = glade_xml_get_widget (xml, "amount_hbox")) != NULL);
        g_assert
        ((td->purpose_entry = glade_xml_get_widget (xml, "purpose_entry")) != NULL);
        g_assert
        ((td->purpose_cont_entry = glade_xml_get_widget (xml, "purpose_cont_entry")) != NULL);
        g_assert
        ((td->purpose_cont2_entry = glade_xml_get_widget (xml, "purpose_cont2_entry")) != NULL);
        g_assert
        ((td->purpose_cont3_entry = glade_xml_get_widget (xml, "purpose_cont3_entry")) != NULL);
        g_assert
        ((orig_name_label = glade_xml_get_widget (xml, "orig_name_label")) != NULL);
        g_assert
        ((orig_account_label = glade_xml_get_widget (xml, "orig_account_label")) != NULL);
        g_assert
        ((orig_bankname_label = glade_xml_get_widget (xml, "orig_bankname_label")) != NULL);
        g_assert
        ((orig_bankcode_label = glade_xml_get_widget (xml, "orig_bankcode_label")) != NULL);
        g_assert
        ((orig_name_heading = glade_xml_get_widget (xml, "orig_name_heading")) != NULL);
        g_assert
        ((orig_account_heading = glade_xml_get_widget (xml, "orig_account_heading")) != NULL);
        g_assert
        ((orig_bankname_heading = glade_xml_get_widget (xml, "orig_bankname_heading")) != NULL);
        g_assert
        ((orig_bankcode_heading = glade_xml_get_widget (xml, "orig_bankcode_heading")) != NULL);
        g_assert
        ((exec_later_button = glade_xml_get_widget (xml, "exec_later_button")) != NULL);
        g_assert
        ((td->template_gtktreeview = GTK_TREE_VIEW(glade_xml_get_widget (xml, "template_list"))) != NULL);
        g_assert
        ((add_templ_button = glade_xml_get_widget (xml, "add_templ_button")) != NULL);
        g_assert
        ((moveup_templ_button = glade_xml_get_widget (xml, "moveup_templ_button")) != NULL);
        g_assert
        ((movedown_templ_button = glade_xml_get_widget (xml, "movedown_templ_button")) != NULL);
        g_assert
        ((sort_templ_button = glade_xml_get_widget (xml, "sort_templ_button")) != NULL);
        g_assert
        ((del_templ_button = glade_xml_get_widget (xml, "del_templ_button")) != NULL);

        td->amount_edit = gnc_amount_edit_new();
        gtk_box_pack_start_defaults(GTK_BOX(amount_hbox), td->amount_edit);
        gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (td->amount_edit),
                                               TRUE);
        gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (td->amount_edit),
                                      xaccAccountGetCommoditySCU (gnc_acc));

        /* Check for what kind of transaction this should be, and change
           the labels accordingly. */
        switch (trans_type)
        {
        case SINGLE_TRANSFER:
        case SINGLE_INTERNAL_TRANSFER:
            /* all labels are already set */
            break;
        case SINGLE_DEBITNOTE:
            gtk_label_set_text (GTK_LABEL (heading_label),
                                /* Translators: Strings from this file are needed only in
                                 * countries that have one of aqbanking's Online Banking
                                 * techniques available. This is 'OFX DirectConnect'
                                 * (U.S. and others), 'HBCI' (in Germany), or 'YellowNet'
                                 * (Switzerland). If none of these techniques are available
                                 * in your country, you may safely ignore strings from the
                                 * import-export/hbci subdirectory. */
                                _("Enter an Online Direct Debit Note"));

            gtk_label_set_text (GTK_LABEL (recp_name_heading),
                                _("Debited Account Owner"));
            gtk_label_set_text (GTK_LABEL (recp_account_heading),
                                _("Debited Account Number"));
            gtk_label_set_text (GTK_LABEL (recp_bankcode_heading),
                                _("Debited Account Bank Code"));

            gtk_label_set_text (GTK_LABEL (orig_name_heading),
                                _("Credited Account Owner"));
            gtk_label_set_text (GTK_LABEL (orig_account_heading),
                                _("Credited Account Number"));
            gtk_label_set_text (GTK_LABEL (orig_bankcode_heading),
                                _("Credited Account Bank Code"));
            break;

        default:
            g_critical("dialog-hbcitrans: Oops, unknown GNC_HBCI_Transtype %d.\n",
                       trans_type);
        }

        /* Make this button insensitive since it's still unimplemented. */
        gtk_widget_destroy (exec_later_button);

        /* aqbanking up to 2.3.0 did not guarantee the following strings
           to be correct utf8; mentioned in bug#351371. */
        hbci_bankname =
            gnc_utf8_strip_invalid_strdup (AB_Account_GetBankName(h_acc));
        hbci_ownername =
            gnc_utf8_strip_invalid_strdup (AB_Account_GetOwnerName(h_acc));

        /* Fill in the values from the objects */
        gtk_label_set_text (GTK_LABEL (orig_name_label),
                            hbci_ownername);
        gtk_label_set_text (GTK_LABEL (orig_account_label),
                            AB_Account_GetAccountNumber (h_acc));
        gtk_label_set_text (GTK_LABEL (orig_bankname_label),
                            (hbci_bankname && (strlen(hbci_bankname) > 0) ?
                             hbci_bankname :
                             _("(unknown)")));
        gtk_label_set_text (GTK_LABEL (orig_bankcode_label),
                            hbci_bankid);
        g_free (hbci_ownername);
        g_free (hbci_bankname);

        /* fill list for choosing a transaction template */
        gtk_tree_view_set_headers_visible(td->template_gtktreeview, FALSE);
        td->template_list_store = gtk_list_store_new(TEMPLATE_NUM_COLUMNS,
                                  G_TYPE_STRING,
                                  G_TYPE_POINTER);
        gtk_tree_view_set_model(td->template_gtktreeview,
                                GTK_TREE_MODEL(td->template_list_store));
        g_object_unref(td->template_list_store);
        g_list_foreach(templates, fill_template_list_func, td->template_list_store);

        renderer = gtk_cell_renderer_text_new();
        column = gtk_tree_view_column_new_with_attributes ("Template Name",
                 renderer,
                 "text", TEMPLATE_NAME,
                 NULL);
        gtk_tree_view_append_column(td->template_gtktreeview, column);

        td->templ_changed = FALSE;

        /* Connect signals */
        selection = gtk_tree_view_get_selection(td->template_gtktreeview);
        g_signal_connect (selection, "changed",
                          G_CALLBACK (on_template_list_selection_changed),
                          td);

        g_signal_connect (add_templ_button, "clicked",
                          G_CALLBACK(add_template_cb), td);
        g_signal_connect (moveup_templ_button, "clicked",
                          G_CALLBACK (moveup_template_cb),
                          td);

        g_signal_connect (movedown_templ_button, "clicked",
                          G_CALLBACK (movedown_template_cb),
                          td);

        g_signal_connect (sort_templ_button, "clicked",
                          G_CALLBACK (sort_template_cb),
                          td);

        g_signal_connect (del_templ_button, "clicked",
                          G_CALLBACK (del_template_cb),
                          td);

        g_signal_connect (td->recp_bankcode_entry, "changed",
                          G_CALLBACK(blz_changed_cb), td);

    } /* GtkWidget declarations/definitions */

    return td;
}


/* ************************************************************
 * Now all the functions where the action happens.
 */

int gnc_hbci_dialog_run_until_ok(HBCITransDialog *td,
                                 const AB_ACCOUNT *h_acc)
{
    gint result;
    int max_purpose_lines;
    gboolean values_ok;

    {
        AB_JOB *job = AB_JobSingleTransfer_new((AB_ACCOUNT *)h_acc);
        if (AB_Job_CheckAvailability(job))
        {
            g_warning("gnc_hbci_trans_dialog_enqueue: Oops, job not available. Aborting.\n");
            return GTK_RESPONSE_CANCEL;
        }
        {
            const AB_TRANSACTION_LIMITS *joblimits = AB_JobSingleTransfer_GetFieldLimits(job);
            max_purpose_lines = (joblimits ?
                                 AB_TransactionLimits_GetMaxLinesPurpose (joblimits) :
                                 2);
        }
        /* these are the number of fields, 27 characters each. */
        AB_Job_free(job);
    }
    /* gtk_widget_set_sensitive (GTK_WIDGET (td->purpose_entry), max_purpose_lines > 0); */
    gtk_widget_set_sensitive (GTK_WIDGET (td->purpose_cont_entry), max_purpose_lines > 1);
    gtk_widget_set_sensitive (GTK_WIDGET (td->purpose_cont2_entry), max_purpose_lines > 2);
    gtk_widget_set_sensitive (GTK_WIDGET (td->purpose_cont3_entry), max_purpose_lines > 3);

    /* Repeat until entered values make sense */
    do
    {

        /* Make sure to show the dialog here */
        gtk_widget_show_all (td->dialog);

        /* Now run the dialog until it gets closed by a button press. */
        result = gtk_dialog_run (GTK_DIALOG (td->dialog));
        /* g_message("hbci_trans: result button was %d.\n", result); */

        /* The dialog gets hidden anyway as soon as any button is pressed. */
        gtk_widget_hide_all (td->dialog);

        /* Was cancel pressed or dialog closed?
         *  GNC_RESPONSE_NOW == execute now
         *  GNC_RESPONSE_LATER == scheduled for later execution (currently unimplemented)
         *  GTK_RESPONSE_CANCEL == cancel
         *  GTK_RESPONSE_DELETE_EVENT == window destroyed */
        if ((result != GNC_RESPONSE_NOW) && (result != GNC_RESPONSE_LATER))
        {
            gtk_widget_destroy (GTK_WIDGET (td->dialog));
            td->dialog = NULL;
            return GTK_RESPONSE_CANCEL;
        }

        /* Now fill in the values from the entry fields into a new
           AB_TRANSACTION. */
        td->hbci_trans = hbci_trans_fill_values(h_acc, td);
        values_ok = TRUE;

        /*printf("dialog-hbcitrans: Got value as %s .\n",
          AB_VALUE_toReadableString (AB_TRANSACTION_value (trans)));*/
        if (AB_Value_GetValue (AB_Transaction_GetValue (td->hbci_trans)) == 0.0)
        {
            gtk_widget_show_all (td->dialog);
            values_ok = !gnc_verify_dialog
                        (td->dialog,
                         TRUE,
                         "%s",
                         _("The amount is zero or the amount field could not be "
                           "interpreted correctly. You might have mixed up decimal "
                           "point and comma, compared to your locale settings. "
                           "This does not result in a valid online transfer job. \n"
                           "\n"
                           "Do you want to enter the job again?"));
            if (values_ok)
            {
                AB_Transaction_free (td->hbci_trans);
                td->hbci_trans = NULL;
                return GTK_RESPONSE_CANCEL;
            }
            continue;
        } /* check Transaction_value */

        {
            char *purpose = gnc_hbci_getpurpose (td->hbci_trans);
            if (strlen(purpose) == 0)
            {
                gtk_widget_show_all (td->dialog);
                values_ok = !gnc_verify_dialog
                            (GTK_WIDGET (td->dialog),
                             TRUE,
                             "%s",
                             _("You did not enter any transaction purpose. A purpose is "
                               "required for an online transfer.\n"
                               "\n"
                               "Do you want to enter the job again?"));
                if (values_ok)
                {
                    AB_Transaction_free (td->hbci_trans);
                    td->hbci_trans = NULL;
                    return GTK_RESPONSE_CANCEL;
                }
                continue;
            } /* check Transaction_purpose */
        }

        {
            char *othername = gnc_hbci_getremotename (td->hbci_trans);
            if (!othername || (strlen (othername) == 0))
            {
                gtk_widget_show_all (td->dialog);
                values_ok = !gnc_verify_dialog
                            (GTK_WIDGET (td->dialog),
                             TRUE,
                             "%s",
                             _("You did not enter a recipient name.  A recipient name is "
                               "required for an online transfer.\n"
                               "\n"
                               "Do you want to enter the job again?"));
                if (othername)
                    g_free (othername);
                if (values_ok)
                {
                    AB_Transaction_free (td->hbci_trans);
                    td->hbci_trans = NULL;
                    return GTK_RESPONSE_CANCEL;
                }
                continue;
            } /* check Recipient Name (in aqbanking: Remote Name) */
        }

        /* FIXME: If this is a direct debit, set the textkey/ "Textschluessel"/
           transactionCode according to some GUI selection here!! */
        /*if (td->trans_type == SINGLE_DEBITNOTE)
          AB_TRANSACTION_setTextKey (td->hbci_trans, 05);*/

        /* And finally check the account code, if ktoblzcheck is available. */
        values_ok = check_ktoblzcheck(GTK_WIDGET (td->dialog), td, td->hbci_trans);

    }
    while (!values_ok);

    return result;
}

/** Create a new AB_TRANSACTION, fill the values from the entry
    fields into it and return it. The caller must
    AB_TRANSACTION_free() it when finished. */
AB_TRANSACTION *
hbci_trans_fill_values(const AB_ACCOUNT *h_acc, HBCITransDialog *td)
{
    /* Fill in the user-entered values */
    AB_TRANSACTION *trans = AB_Transaction_new();
    gchar *tmpchar;

    /* The internal source encoding is returned by
       gnc_hbci_book_encoding(), which is hard-coded so far. This needs
       to be fixed for the gnome2 version; the source encoding is then
       probably utf-8 as well. iconv is also used in
       gnc_AB_BANKING_interactors() in hbci-interaction.c. */
    GIConv gnc_iconv_handler =
        g_iconv_open(gnc_hbci_AQBANKING_encoding(), gnc_hbci_book_encoding());
    g_assert(gnc_iconv_handler != (GIConv)(-1));

    /* OpenHBCI newer than 0.9.8: use account's bankCode values
     * instead of the bank's ones since this is what some banks
     * require. */
    AB_Transaction_SetLocalBankCode (trans,
                                     AB_Account_GetBankCode (h_acc));
    AB_Transaction_SetLocalAccountNumber (trans, AB_Account_GetAccountNumber (h_acc));
    AB_Transaction_SetLocalCountry (trans, "DE");

    AB_Transaction_SetRemoteBankCode
    (trans, gtk_entry_get_text (GTK_ENTRY (td->recp_bankcode_entry)));
    /* g_message("Got otherBankCode %s.\n",
       AB_Transaction_otherBankCode (trans)); */
    AB_Transaction_SetRemoteAccountNumber
    (trans, gtk_entry_get_text (GTK_ENTRY (td->recp_account_entry)));
    /* g_message("Got otherAccountId %s.\n",
       AB_Transaction_otherAccountId (trans)); */
    AB_Transaction_SetRemoteCountry (trans, "DE");

    /* Convert the result of GTK_ENTRY into UTF-8 */
    tmpchar = gnc_call_iconv(gnc_iconv_handler,
                             gtk_entry_get_text (GTK_ENTRY (td->recp_name_entry)));
    AB_Transaction_AddRemoteName (trans, tmpchar, FALSE);
    g_free (tmpchar);

    /* The last argument means: If TRUE, then the string will be only be
       appended if it doesn't exist yet. */
    /* Convert the result of GTK_ENTRY into UTF-8 */
    tmpchar = gnc_call_iconv(gnc_iconv_handler,
                             gtk_entry_get_text (GTK_ENTRY (td->purpose_entry)));
    AB_Transaction_AddPurpose (trans, tmpchar, FALSE);
    g_free (tmpchar);
    tmpchar = gnc_call_iconv(gnc_iconv_handler,
                             gtk_entry_get_text (GTK_ENTRY (td->purpose_cont_entry)));
    if (strlen(tmpchar) > 0)
        AB_Transaction_AddPurpose (trans, tmpchar, FALSE);
    g_free (tmpchar);
    tmpchar = gnc_call_iconv(gnc_iconv_handler,
                             gtk_entry_get_text (GTK_ENTRY (td->purpose_cont2_entry)));
    if (strlen(tmpchar) > 0)
        AB_Transaction_AddPurpose (trans, tmpchar, FALSE);
    g_free (tmpchar);
    tmpchar = gnc_call_iconv(gnc_iconv_handler,
                             gtk_entry_get_text (GTK_ENTRY (td->purpose_cont3_entry)));
    if (strlen(tmpchar) > 0)
        AB_Transaction_AddPurpose (trans, tmpchar, FALSE);
    g_free (tmpchar);

    /* FIXME: Replace "EUR" by account-dependent string here. */
    AB_Transaction_SetValue
    (trans, AB_Value_new
     (gnc_amount_edit_get_damount (GNC_AMOUNT_EDIT (td->amount_edit)), "EUR"));

    /* If this is a direct debit, a textkey/ "Textschluessel"/
       transactionCode different from the default has to be set. */
    switch (td->trans_type)
    {
    case SINGLE_DEBITNOTE:
        /* AB_Transaction_SetTransactionCode (trans, 05); */
        AB_Transaction_SetTextKey (trans, 05);
        break;
    default:
        /* AB_Transaction_SetTransactionCode (trans, 51); */
        AB_Transaction_SetTextKey (trans, 51);
    }

    g_iconv_close(gnc_iconv_handler);
    return trans;
}

/** Checks the account code in the AB_TRANSACTION, if the
    ktoblzcheck package is available. Returns TRUE if everything is
    fine, or FALSE if this transaction should be entered again. */
gboolean
check_ktoblzcheck(GtkWidget *parent, const HBCITransDialog *td,
                  const AB_TRANSACTION *trans)
{
#if HAVE_KTOBLZCHECK_H
    int blzresult;
    const char *blztext;
    gboolean values_ok = TRUE;

    blzresult = AccountNumberCheck_check
                (td->blzcheck,
                 AB_Transaction_GetRemoteBankCode (trans),
                 AB_Transaction_GetRemoteAccountNumber (trans));
    switch (blzresult)
    {
    case 2:
        gtk_widget_show_all (parent);
        values_ok = gnc_verify_dialog
                    (parent,
                     TRUE,
                     _("The internal check of the destination account number '%s' "
                       "at the specified bank with bank code '%s' failed. This means "
                       "the account number might contain an error. Should the online "
                       "transfer job be sent with this account number anyway?"),
                     AB_Transaction_GetRemoteAccountNumber (trans),
                     AB_Transaction_GetRemoteBankCode (trans));
        blztext = "Kontonummer wahrscheinlich falsch";
        break;
    case 0:
        blztext = "Kontonummer ok";
        break;
    case 3:
        blztext = "bank unbekannt";
        break;
    default:
    case 1:
        blztext = "unbekannt aus unbekanntem grund";
        break;
    }

    /*printf("gnc_hbci_trans: KtoBlzCheck said check is %d = %s\n",
      blzresult, blztext);*/
    return values_ok;
#else
    return TRUE;
#endif
}

AB_JOB *
gnc_hbci_trans_dialog_enqueue(const AB_TRANSACTION *hbci_trans, AB_BANKING *api,
                              AB_ACCOUNT *h_acc,
                              GNC_HBCI_Transtype trans_type)
{
    AB_JOB *job;

    /* Create a Do-Transaction (Transfer) job. */
    switch (trans_type)
    {
    case SINGLE_DEBITNOTE:
        job = AB_JobSingleDebitNote_new(h_acc);
        break;
    case SINGLE_INTERNAL_TRANSFER:
        job = AB_JobInternalTransfer_new(h_acc);
        break;
    default:
    case SINGLE_TRANSFER:
        job = AB_JobSingleTransfer_new(h_acc);
    };
    if (AB_Job_CheckAvailability(job))
    {
        g_warning("gnc_hbci_trans_dialog_enqueue: Oops, job not available. Aborting.\n");
        return NULL;
    }

    switch (trans_type)
    {
    case SINGLE_DEBITNOTE:
        AB_JobSingleDebitNote_SetTransaction(job, hbci_trans);
        break;
    case SINGLE_INTERNAL_TRANSFER:
        AB_JobInternalTransfer_SetTransaction(job, hbci_trans);
        break;
    default:
    case SINGLE_TRANSFER:
        AB_JobSingleTransfer_SetTransaction(job, hbci_trans);
    };

    /* Add job to queue */
    AB_Banking_EnqueueJob(api, job);

    return job;
}

gboolean
gnc_hbci_trans_dialog_execute(HBCITransDialog *td, AB_BANKING *api,
                              AB_JOB *job, GNCInteractor *interactor)
{
    gboolean successful;
    g_assert(td);
    g_assert(api);
    g_assert(job);

    successful = gnc_AB_BANKING_execute (td->parent, api, job, interactor);

    /*printf("dialog-hbcitrans: Ok, result of api_execute was %d.\n",
      successful);*/

    if (!successful)
    {
        /* AB_BANKING_executeOutbox failed. */
        if ((AB_Job_GetStatus (job) == AB_Job_StatusPending) ||
                (AB_Job_GetStatus (job) == AB_Job_StatusError))
            successful = !gnc_verify_dialog
                         (td->parent,
                          FALSE,
                          "%s",
                          _("The job was sent to the bank successfully, but the "
                            "bank is refusing to execute the job. Please check "
                            "the log window for the exact error message of the "
                            "bank. The line with the error message contains a "
                            "code number that is greater than 9000.\n"
                            "\n"
                            "Do you want to enter the job again?"));

        if (AB_Job_GetStatus (job) == AB_Job_StatusPending)
            AB_Banking_DelPendingJob(api, job);

        AB_Transaction_free (td->hbci_trans);
        td->hbci_trans = NULL;
    }
    /* Watch out! The job *has* to be removed from the queue
       here because otherwise it might be executed again. */
    /* FIXME: need to do AB_Banking_DequeueJob(api, job); */
    return successful;
}


/* -------------------------------------- */
/* Callbacks */
/* -------------------------------------- */
static void fill_entry(const char *str, GtkWidget *entry)
{
    gtk_entry_set_text (GTK_ENTRY (entry), str ? str : "");
}


void
on_template_list_selection_changed (GtkTreeSelection *selection,
                                    gpointer          user_data)
{
    HBCITransDialog *td = user_data;
    GNCTransTempl *templ;
    GtkTreeModel *model;
    GtkTreeIter iter;

    g_assert(td);

    if (!gtk_tree_selection_get_selected(selection, &model, &iter))
        return;
    gtk_tree_model_get(model, &iter, TEMPLATE_POINTER, &templ, -1);

    fill_entry(gnc_trans_templ_get_recp_name(templ), td->recp_name_entry);
    fill_entry(gnc_trans_templ_get_recp_account(templ), td->recp_account_entry);
    fill_entry(gnc_trans_templ_get_recp_bankcode(templ), td->recp_bankcode_entry);
    fill_entry(gnc_trans_templ_get_purpose(templ), td->purpose_entry);
    fill_entry(gnc_trans_templ_get_purpose_cont(templ), td->purpose_cont_entry);

    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (td->amount_edit),
                                gnc_trans_templ_get_amount (templ));

}


void blz_changed_cb(GtkEditable *e, gpointer user_data)
{
#if HAVE_KTOBLZCHECK_H
    HBCITransDialog *td = user_data;
    const AccountNumberCheck_Record *record;
    g_assert(td);

    record = AccountNumberCheck_findBank
             (td->blzcheck,
              gtk_entry_get_text (GTK_ENTRY (td->recp_bankcode_entry)));

    if (record)
    {
        const char *bankname = AccountNumberCheck_Record_bankName (record);
        GError *error = NULL;
        const char *ktoblzcheck_encoding =
#ifdef KTOBLZCHECK_VERSION_MAJOR
            /* This version number macro has been added in
            	 ktoblzcheck-1.10, but this function exists already since
            	 ktoblzcheck-1.7, so we're on the safe side. */
            AccountNumberCheck_stringEncoding()
#else
            /* Every ktoblzcheck release before 1.10 is guaranteed to
            	 return strings only in ISO-8859-15. */
            "ISO-8859-15"
#endif
            ;
        gchar *utf8_bankname = g_convert (bankname, strlen(bankname),
                                          "UTF-8", ktoblzcheck_encoding,
                                          NULL, NULL, &error);
        if (error != NULL)
        {
            g_critical ("Error converting bankname \"%s\" to UTF-8\n", bankname);
            g_error_free (error);
            /* Conversion was erroneous, so don't use the string */
            utf8_bankname = g_strdup (_("(unknown)"));
        }
        gtk_label_set_text (GTK_LABEL (td->recp_bankname_label),
                            (strlen(utf8_bankname) > 0 ?
                             utf8_bankname : _("(unknown)")));
        g_free (utf8_bankname);
        gtk_widget_show_all (td->recp_bankname_label);

        /*printf("blz_changed_cb: KtoBlzCheck said check is bank is '%s' at '%s'.\n",
          bankname,
          AccountNumberCheck_Record_location (record));*/

    }
    else
    {
        gtk_label_set_text (GTK_LABEL (td->recp_bankname_label),
                            _("(unknown)"));
        gtk_widget_show_all (td->recp_bankname_label);
    }
#endif
}

/* -------------------------------------- */
/* -------------------------------------- */

void add_template_cb(GtkButton *b,
                     gpointer user_data)
{
    HBCITransDialog *td = user_data;
    GtkWidget *dlg;
    const gchar *name;
    int retval = -1;
    GladeXML *xml;
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeIter cur_iter, new_iter;
    GtkWidget *entry;

    g_assert(td);

    xml = gnc_glade_xml_new ("hbci.glade", "HBCI_template_name_dialog");

    dlg = glade_xml_get_widget (xml, "HBCI_template_name_dialog");
    entry = glade_xml_get_widget (xml, "template_name");
    gtk_entry_set_text(GTK_ENTRY(entry),
                       gtk_entry_get_text(GTK_ENTRY(td->recp_name_entry)));

    retval = gtk_dialog_run(GTK_DIALOG(dlg));

    if (retval == GTK_RESPONSE_OK)
    {
        name = gtk_entry_get_text(GTK_ENTRY(entry));

        if (name && (strlen(name) > 0))
        {
            GNCTransTempl *r;
            /*printf("add_template_cb: adding template '%s'\n", name);*/
            r = gnc_trans_templ_new_full
                (name,
                 gtk_entry_get_text (GTK_ENTRY (td->recp_name_entry)),
                 gtk_entry_get_text (GTK_ENTRY (td->recp_account_entry)),
                 gtk_entry_get_text (GTK_ENTRY (td->recp_bankcode_entry)),
                 gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (td->amount_edit)),
                 gtk_entry_get_text (GTK_ENTRY (td->purpose_entry)),
                 gtk_entry_get_text (GTK_ENTRY (td->purpose_cont_entry)));

            selection = gtk_tree_view_get_selection(td->template_gtktreeview);
            if (gtk_tree_selection_get_selected(selection, &model, &cur_iter))
            {
                gtk_list_store_insert_after(td->template_list_store,
                                            &new_iter, &cur_iter);
            }
            else
            {
                gtk_list_store_append(GTK_LIST_STORE(model), &new_iter);
            }
            gtk_list_store_set(td->template_list_store, &new_iter,
                               TEMPLATE_NAME, name,
                               TEMPLATE_POINTER, r,
                               -1);
            td->templ_changed = TRUE;
        }
    }
    gtk_widget_destroy(dlg);
    //  g_object_unref(xml);
}


void
moveup_template_cb(GtkButton       *button,
                   gpointer         user_data)
{
    HBCITransDialog *td = user_data;
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeIter iter, prev_iter;
    GtkTreePath *prev_path;

    g_assert(td);

    selection = gtk_tree_view_get_selection(td->template_gtktreeview);
    if (!gtk_tree_selection_get_selected (selection, &model, &iter))
        return;

    prev_path = gtk_tree_model_get_path(model, &iter);
    if (gtk_tree_path_prev(prev_path))
    {
        if (gtk_tree_model_get_iter(model, &prev_iter, prev_path))
        {
            gtk_list_store_move_before(GTK_LIST_STORE(model), &iter, &prev_iter);
            td->templ_changed = TRUE;
        }
    }

    gtk_tree_path_free(prev_path);
}


void
movedown_template_cb(GtkButton       *button,
                     gpointer         user_data)
{
    HBCITransDialog *td = user_data;
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeIter iter, next_iter;

    g_assert(td);

    selection = gtk_tree_view_get_selection(td->template_gtktreeview);
    if (!gtk_tree_selection_get_selected (selection, &model, &iter))
        return;

    next_iter = iter;
    if (gtk_tree_model_iter_next(model, &next_iter))
    {
        gtk_list_store_move_after(GTK_LIST_STORE(model), &iter, &next_iter);
        td->templ_changed = TRUE;
    }
}

void
sort_template_cb(GtkButton       *button,
                 gpointer         user_data)
{
    HBCITransDialog *td = user_data;
    g_assert(td);

    gtk_tree_sortable_set_sort_column_id
    (GTK_TREE_SORTABLE(td->template_list_store),
     TEMPLATE_NAME, GTK_SORT_ASCENDING);
    gtk_tree_sortable_set_sort_column_id
    (GTK_TREE_SORTABLE(td->template_list_store),
     GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID,
     GTK_SORT_ASCENDING);
    td->templ_changed = TRUE;
}



void
del_template_cb(GtkButton       *button,
                gpointer         user_data)
{
    HBCITransDialog *td = user_data;
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeIter iter;

    g_assert(td);

    selection = gtk_tree_view_get_selection(td->template_gtktreeview);
    if (!gtk_tree_selection_get_selected (selection, &model, &iter))
        return;

    gtk_list_store_remove(GTK_LIST_STORE(model), &iter);
    td->templ_changed = TRUE;
}



void gnc_hbci_dialog_xfer_cb(Transaction *trans, gpointer user_data)
{
    HBCITransDialog *td = user_data;
    g_assert(td);
    if (trans)
    {
        td->gnc_trans = trans;
        /* Unregister handler for transaction creation callback */
        if (td->gnc_trans_dialog)
            gnc_xfer_dialog_set_txn_cb(td->gnc_trans_dialog, NULL, NULL);
        td->gnc_trans_dialog = NULL;
    }
    else
    {
        if (td->gnc_trans_dialog)
        {
            gnc_xfer_dialog_set_txn_cb(td->gnc_trans_dialog, NULL, NULL);
            td->gnc_trans_dialog = NULL;
        }
    }
    return;
}
