/*
 * dialog-ab-trans.c --
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/**
 * @internal
 * @file dialog-ab-trans.c
 * @brief Templates for AqBanking transactions
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2004 Bernd Wagner
 * @author Copyright (C) 2006 David Hampton <hampton@employees.org>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include "config.h"

#include <glib/gi18n.h>
#if HAVE_KTOBLZCHECK_H
#    include <ktoblzcheck.h>
#endif
#include <aqbanking/jobsingletransfer.h>
#include <aqbanking/jobsingledebitnote.h>
#include <aqbanking/jobinternaltransfer.h>

#include "dialog-ab-trans.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "gnc-ab-trans-templ.h"
#include "gnc-ab-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-ui.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

static void fill_templ_helper(gpointer data, gpointer user_data);
static AB_TRANSACTION *ab_trans_fill_values(GncABTransDialog *td);
static gboolean check_ktoblzcheck(GtkWidget *parent, const GncABTransDialog *td,
                                  const AB_TRANSACTION *trans);
static gboolean clear_templ_helper(GtkTreeModel *model, GtkTreePath *path,
                                   GtkTreeIter *iter, gpointer user_data);
static gboolean get_templ_helper(GtkTreeModel *model, GtkTreePath *path,
                                 GtkTreeIter *iter, gpointer data);
static AB_JOB *get_available_empty_job(AB_ACCOUNT *ab_acc,
                                       GncABTransType trans_type);

void dat_bankcode_changed_cb(GtkEditable *editable, gpointer user_data);
void templ_list_row_activated_cb(GtkTreeView *view, GtkTreePath *path,
                                 GtkTreeViewColumn *column, gpointer user_data);
static gboolean find_templ_helper(GtkTreeModel *model, GtkTreePath *path,
                                  GtkTreeIter *iter, gpointer user_data);
void dat_add_templ_cb(GtkButton *button, gpointer user_data);
void dat_moveup_templ_cb(GtkButton *button, gpointer user_data);
void dat_movedown_templ_cb(GtkButton *button, gpointer user_data);
void dat_sort_templ_cb(GtkButton *button, gpointer user_data);
void dat_del_templ_cb(GtkButton *button, gpointer user_data);

enum
{
    TEMPLATE_NAME,
    TEMPLATE_POINTER,
    TEMPLATE_NUM_COLUMNS
};

struct _GncABTransDialog
{
    /* The dialog itself */
    GtkWidget *dialog;
    GtkWidget *parent;
    AB_ACCOUNT *ab_acc;

    /* Whether this is a transfer or a direct debit */
    GncABTransType trans_type;

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

    /* The aqbanking transaction that got created here */
    AB_TRANSACTION *ab_trans;

    /* The gnucash transaction that got created here */
    Transaction *gnc_trans;

#if HAVE_KTOBLZCHECK_H
    /* object for Account number checking */
    AccountNumberCheck *blzcheck;
#endif
};

static void
fill_templ_helper(gpointer data, gpointer user_data)
{
    GncABTransTempl *templ = data;
    GtkListStore *store = user_data;
    GtkTreeIter iter;

    g_return_if_fail(templ && store);
    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter,
                       TEMPLATE_NAME, gnc_ab_trans_templ_get_name(templ),
                       TEMPLATE_POINTER, templ,
                       -1);
}

/**
 * Create a new AB_TRANSACTION, fill the values from the entry fields into it
 * and return it.  The caller must AB_TRANSACTION_free() it when finished.
 */
static AB_TRANSACTION *
ab_trans_fill_values(GncABTransDialog *td)
{
    /* Fill in the user-entered values */
    AB_TRANSACTION *trans = AB_Transaction_new();
    AB_VALUE *value;

    AB_Transaction_SetLocalBankCode(trans, AB_Account_GetBankCode(td->ab_acc));
    AB_Transaction_SetLocalAccountNumber(
        trans, AB_Account_GetAccountNumber(td->ab_acc));
    AB_Transaction_SetLocalCountry(trans, "DE");

    AB_Transaction_SetRemoteBankCode(
        trans, gtk_entry_get_text(GTK_ENTRY(td->recp_bankcode_entry)));
    AB_Transaction_SetRemoteAccountNumber(
        trans, gtk_entry_get_text(GTK_ENTRY(td->recp_account_entry)));
    AB_Transaction_SetRemoteCountry(trans, "DE");
    AB_Transaction_AddRemoteName(
        trans, gtk_entry_get_text(GTK_ENTRY(td->recp_name_entry)), FALSE);

    AB_Transaction_AddPurpose(
        trans, gtk_entry_get_text(GTK_ENTRY(td->purpose_entry)), FALSE);
    AB_Transaction_AddPurpose(
        trans, gtk_entry_get_text(GTK_ENTRY(td->purpose_cont_entry)), FALSE);
    AB_Transaction_AddPurpose(
        trans, gtk_entry_get_text(GTK_ENTRY(td->purpose_cont2_entry)), FALSE);
    AB_Transaction_AddPurpose(
        trans, gtk_entry_get_text(GTK_ENTRY(td->purpose_cont3_entry)), FALSE);

    value = AB_Value_fromDouble(gnc_amount_edit_get_damount(
                                    GNC_AMOUNT_EDIT(td->amount_edit)));
    /* FIXME: Replace "EUR" by account-dependent string here. */
    AB_Value_SetCurrency(value, "EUR");
    AB_Transaction_SetValue(trans, value);
    AB_Value_free(value);

    /* If this is a direct debit, a textkey/ "Textschluessel"/transactionCode
     * different from the default has to be set. */
    switch (td->trans_type)
    {
    case SINGLE_DEBITNOTE:
        /* AB_Transaction_SetTransactionCode (trans, 05); */
        AB_Transaction_SetTextKey(trans, 05);
        break;
    default:
        /* AB_Transaction_SetTransactionCode (trans, 51); */
        AB_Transaction_SetTextKey (trans, 51);
    }

    return trans;
}

GncABTransDialog *
gnc_ab_trans_dialog_new(GtkWidget *parent, AB_ACCOUNT *ab_acc,
                        gint commodity_scu, GncABTransType trans_type,
                        GList *templates)
{
    GncABTransDialog *td;
    GladeXML *xml;
    const gchar *ab_ownername;
    const gchar *ab_accountnumber;
    const gchar *ab_bankname;
    const gchar *ab_bankcode;
    GtkWidget *heading_label;
    GtkWidget *recp_name_heading;
    GtkWidget *recp_account_heading;
    GtkWidget *recp_bankcode_heading;
    GtkWidget *amount_hbox;
    GtkWidget *orig_name_heading;
    GtkWidget *orig_name_label;
    GtkWidget *orig_account_heading;
    GtkWidget *orig_account_label;
    GtkWidget *orig_bankname_heading;
    GtkWidget *orig_bankname_label;
    GtkWidget *orig_bankcode_heading;
    GtkWidget *orig_bankcode_label;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    g_return_val_if_fail(ab_acc, NULL);

    ab_ownername = AB_Account_GetOwnerName(ab_acc);
    if (!ab_ownername)
        ab_ownername = "";
    ab_accountnumber = AB_Account_GetAccountNumber(ab_acc);
    ab_bankcode = AB_Account_GetBankCode(ab_acc);
    ab_bankname = AB_Account_GetBankName(ab_acc);
    if (!ab_bankname || !*ab_bankname)
        ab_bankname = _("(unknown)");

    td = g_new0(GncABTransDialog, 1);
    td->parent = parent;
    td->ab_acc = ab_acc;
    td->trans_type = trans_type;

#if HAVE_KTOBLZCHECK_H
    td->blzcheck = AccountNumberCheck_new();
#endif

    xml = gnc_glade_xml_new("aqbanking.glade", "Transaction Dialog");
    td->dialog = glade_xml_get_widget(xml, "Transaction Dialog");
    g_object_set_data_full(G_OBJECT(td->dialog), "xml", xml, g_object_unref);
    glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, td);

    if (parent)
        gtk_window_set_transient_for(GTK_WINDOW(td->dialog), GTK_WINDOW(parent));

    /* Extract widgets */
    heading_label = glade_xml_get_widget(xml, "heading_label");
    recp_name_heading = glade_xml_get_widget(xml, "recp_name_heading");
    td->recp_name_entry = glade_xml_get_widget(xml, "recp_name_entry");
    recp_account_heading = glade_xml_get_widget(xml, "recp_account_heading");
    td->recp_account_entry = glade_xml_get_widget(xml, "recp_account_entry");
    recp_bankcode_heading = glade_xml_get_widget(xml, "recp_bankcode_heading");
    td->recp_bankcode_entry = glade_xml_get_widget(xml, "recp_bankcode_entry");
    td->recp_bankname_label = glade_xml_get_widget(xml, "recp_bankname_label");
    amount_hbox = glade_xml_get_widget(xml, "amount_hbox");
    td->purpose_entry = glade_xml_get_widget(xml, "purpose_entry");
    td->purpose_cont_entry = glade_xml_get_widget(xml, "purpose_cont_entry");
    td->purpose_cont2_entry = glade_xml_get_widget(xml, "purpose_cont2_entry");
    td->purpose_cont3_entry = glade_xml_get_widget(xml, "purpose_cont3_entry");
    orig_name_heading = glade_xml_get_widget(xml, "orig_name_heading");
    orig_name_label = glade_xml_get_widget(xml, "orig_name_label");
    orig_account_heading = glade_xml_get_widget(xml, "orig_account_heading");
    orig_account_label = glade_xml_get_widget(xml, "orig_account_label");
    orig_bankname_heading = glade_xml_get_widget(xml, "orig_bankname_heading");
    orig_bankname_label = glade_xml_get_widget(xml, "orig_bankname_label");
    orig_bankcode_heading = glade_xml_get_widget(xml, "orig_bankcode_heading");
    orig_bankcode_label = glade_xml_get_widget(xml, "orig_bankcode_label");
    td->template_gtktreeview =
        GTK_TREE_VIEW(glade_xml_get_widget(xml, "template_list"));

    /* Amount edit */
    td->amount_edit = gnc_amount_edit_new();
    gtk_box_pack_start_defaults(GTK_BOX(amount_hbox), td->amount_edit);
    gnc_amount_edit_set_evaluate_on_enter(GNC_AMOUNT_EDIT(td->amount_edit),
                                          TRUE);
    gnc_amount_edit_set_fraction(GNC_AMOUNT_EDIT(td->amount_edit),
                                 commodity_scu);

    /* Check for what kind of transaction this should be, and change the
     * labels accordingly */
    switch (trans_type)
    {
    case SINGLE_TRANSFER:
    case SINGLE_INTERNAL_TRANSFER:
        /* all labels are already set */
        break;
    case SINGLE_DEBITNOTE:
        gtk_label_set_text(GTK_LABEL (heading_label),
                           /* Translators: Strings from this file are
                             * needed only in countries that have one of
                             * aqbanking's Online Banking techniques
                             * available. This is 'OFX DirectConnect'
                             * (U.S. and others), 'HBCI' (in Germany),
                             * or 'YellowNet' (Switzerland). If none of
                             * these techniques are available in your
                             * country, you may safely ignore strings
                             * from the import-export/hbci
                             * subdirectory. */
                           _("Enter an Online Direct Debit Note"));

        gtk_label_set_text(GTK_LABEL(recp_name_heading),
                           _("Debited Account Owner"));
        gtk_label_set_text(GTK_LABEL(recp_account_heading),
                           _("Debited Account Number"));
        gtk_label_set_text(GTK_LABEL(recp_bankcode_heading),
                           _("Debited Account Bank Code"));

        gtk_label_set_text(GTK_LABEL(orig_name_heading),
                           _("Credited Account Owner"));
        gtk_label_set_text(GTK_LABEL(orig_account_heading),
                           _("Credited Account Number"));
        gtk_label_set_text(GTK_LABEL(orig_bankcode_heading),
                           _("Credited Account Bank Code"));
        break;

    default:
        g_critical("gnc_ab_trans_dialog_new: Oops, unknown GncABTransType %d",
                   trans_type);
    }

    gtk_label_set_text(GTK_LABEL(orig_name_label), ab_ownername);
    gtk_label_set_text(GTK_LABEL(orig_account_label), ab_accountnumber);
    gtk_label_set_text(GTK_LABEL(orig_bankname_label), ab_bankname);
    gtk_label_set_text (GTK_LABEL (orig_bankcode_label), ab_bankcode);

    /* Fill list for choosing a transaction template */
    td->template_list_store = gtk_list_store_new(TEMPLATE_NUM_COLUMNS,
                              G_TYPE_STRING, G_TYPE_POINTER);
    g_list_foreach(templates, fill_templ_helper, td->template_list_store);
    gtk_tree_view_set_model(td->template_gtktreeview,
                            GTK_TREE_MODEL(td->template_list_store));
    td->templ_changed = FALSE;
    /* Keep a reference to the store */

    /* Show this list */
    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(
                 "Template Name", renderer, "text", TEMPLATE_NAME, NULL);
    gtk_tree_view_append_column(td->template_gtktreeview, column);

    return td;
}

static gboolean
check_ktoblzcheck(GtkWidget *parent, const GncABTransDialog *td,
                  const AB_TRANSACTION *trans)
{
#ifndef HAVE_KTOBLZCHECK_H
    return TRUE;
#else
    gint blzresult;
    const char *blztext;
    gboolean values_ok = TRUE;

    ENTER(" ");

    blzresult = AccountNumberCheck_check(
                    td->blzcheck,
                    AB_Transaction_GetRemoteBankCode(trans),
                    AB_Transaction_GetRemoteAccountNumber(trans));
    switch (blzresult)
    {
    case 2:
        gtk_widget_show(parent);
        values_ok = gnc_verify_dialog(
                        parent, TRUE,
                        _("The internal check of the destination account number '%s' "
                          "at the specified bank with bank code '%s' failed. This means "
                          "the account number might contain an error. Should the online "
                          "transfer job be sent with this account number anyway?"),
                        AB_Transaction_GetRemoteAccountNumber(trans),
                        AB_Transaction_GetRemoteBankCode(trans));
        blztext = "Kontonummer wahrscheinlich falsch";
        break;
    case 0:
        blztext = "Kontonummer ok";
        break;
    case 3:
        blztext = "bank unbekannt";
        break;
    case 1:
    default:
        blztext = "unbekannt aus unbekanntem grund";
        break;
    }

    LEAVE("KtoBlzCheck said check is %d = %s",
          blzresult, blztext ? blztext : "(none)");

    return values_ok;
#endif
}

gint
gnc_ab_trans_dialog_run_until_ok(GncABTransDialog *td)
{
    gint result;
    AB_JOB *job;
    const AB_TRANSACTION_LIMITS *joblimits;
    guint8 max_purpose_lines;
    gboolean values_ok;
    gchar *purpose;
    gchar *othername;

    /* Check whether the account supports this job */
    job = get_available_empty_job(td->ab_acc, td->trans_type);
    if (!job)
    {
        g_warning("gnc_ab_trans_dialog_run_until_ok: Oops, job not available");
        return GTK_RESPONSE_CANCEL;
    }

    /* Activate as many purpose entries as available for the job */
    joblimits = AB_JobSingleTransfer_GetFieldLimits(job);
    max_purpose_lines = joblimits ?
                        AB_TransactionLimits_GetMaxLinesPurpose(joblimits) : 2;
    gtk_widget_set_sensitive(td->purpose_cont_entry, max_purpose_lines > 1);
    gtk_widget_set_sensitive(td->purpose_cont2_entry, max_purpose_lines > 2);
    gtk_widget_set_sensitive(td->purpose_cont3_entry, max_purpose_lines > 3);

    /* Show the dialog */
    gtk_widget_show(td->dialog);

    /* Repeat until entered values make sense */
    do
    {
        /* Now run the dialog until it gets closed by a button press */
        result = gtk_dialog_run (GTK_DIALOG (td->dialog));

        /* Was cancel pressed or dialog closed?
         *  GNC_RESPONSE_NOW == execute now
         *  GNC_RESPONSE_LATER == scheduled for later execution (unimplemented)
         *  GTK_RESPONSE_CANCEL == cancel
         *  GTK_RESPONSE_DELETE_EVENT == window destroyed */
        if (result != GNC_RESPONSE_NOW && result != GNC_RESPONSE_LATER)
        {
            gtk_widget_destroy(td->dialog);
            td->dialog = NULL;
            break;
        }

        /* Now fill in the values from the entry fields into a new
         * AB_TRANSACTION */
        td->ab_trans = ab_trans_fill_values(td);
        values_ok = TRUE;

        /* Check transaction value */
        values_ok =
            AB_Value_GetValueAsDouble(AB_Transaction_GetValue(td->ab_trans))
            != 0.0;
        if (!values_ok)
        {
            gtk_widget_show(td->dialog);
            if (gnc_verify_dialog(
                        td->dialog, TRUE, "%s",
                        _("The amount is zero or the amount field could not be "
                          "interpreted correctly. You might have mixed up decimal "
                          "point and comma, compared to your locale settings. "
                          "This does not result in a valid online transfer job. \n"
                          "\n"
                          "Do you want to enter the job again?")))
            {
                continue;
            }
            else
            {
                AB_Transaction_free(td->ab_trans);
                td->ab_trans = NULL;
                result = GTK_RESPONSE_CANCEL;
                break;
            }
        }

        /* Check transaction purpose */
        purpose = gnc_ab_get_purpose(td->ab_trans);
        values_ok = *purpose;
        g_free(purpose);
        if (!values_ok)
        {
            gtk_widget_show(td->dialog);
            if (gnc_verify_dialog(
                        td->dialog, TRUE, "%s",
                        _("You did not enter any transaction purpose. A purpose is "
                          "required for an online transfer.\n"
                          "\n"
                          "Do you want to enter the job again?")))
            {
                continue;
            }
            else
            {
                AB_Transaction_free(td->ab_trans);
                td->ab_trans = NULL;
                result = GTK_RESPONSE_CANCEL;
                break;
            }
        }

        /* Check recipient / remote name */
        othername = gnc_ab_get_remote_name(td->ab_trans);
        values_ok = othername && *othername;
        g_free(othername);
        if (!values_ok)
        {
            gtk_widget_show(td->dialog);
            if (gnc_verify_dialog(
                        td->dialog, TRUE, "%s",
                        _("You did not enter a recipient name.  A recipient name is "
                          "required for an online transfer.\n"
                          "\n"
                          "Do you want to enter the job again?")))
            {
                continue;
            }
            else
            {
                AB_Transaction_free(td->ab_trans);
                td->ab_trans = NULL;
                result = GTK_RESPONSE_CANCEL;
                break;
            }
        }

        /* FIXME: If this is a direct debit, set the textkey/ "Textschluessel"/
         * transactionCode according to some GUI selection here!! */
        /*if (td->trans_type == SINGLE_DEBITNOTE)
          AB_TRANSACTION_setTextKey (td->hbci_trans, 05); */

        /* And finally check the account code, if ktoblzcheck is available */
        values_ok = check_ktoblzcheck(td->dialog, td, td->ab_trans);

    }
    while (!values_ok);

    /* Hide the dialog */
    if (td->dialog)
        gtk_widget_hide(td->dialog);

    return result;
}

static gboolean
clear_templ_helper(GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter,
                   gpointer user_data)
{
    GncABTransTempl *templ;

    g_return_val_if_fail(model && iter, TRUE);

    gtk_tree_model_get(model, iter, TEMPLATE_POINTER, &templ, -1);
    gnc_ab_trans_templ_free(templ);
    return FALSE;
}

void
gnc_ab_trans_dialog_free(GncABTransDialog *td)
{
    if (!td) return;
    if (td->ab_trans)
        AB_Transaction_free(td->ab_trans);
    if (td->dialog)
        gtk_widget_destroy(td->dialog);
    if (td->template_list_store)
    {
        gtk_tree_model_foreach(GTK_TREE_MODEL(td->template_list_store),
                               clear_templ_helper, NULL);
        g_object_unref(td->template_list_store);
    }
#if HAVE_KTOBLZCHECK_H
    AccountNumberCheck_delete(td->blzcheck);
#endif
    g_free(td);
}

static gboolean
get_templ_helper(GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter,
                 gpointer data)
{
    GList **list = data;
    GncABTransTempl *templ;

    g_return_val_if_fail(model && iter, TRUE);

    gtk_tree_model_get(model, iter, TEMPLATE_POINTER, &templ, -1);
    *list = g_list_prepend(*list, templ);
    return FALSE;
}

GList *
gnc_ab_trans_dialog_get_templ(const GncABTransDialog *td, gboolean *changed)
{
    GList *list = NULL;

    g_return_val_if_fail(td, NULL);

    if (changed)
    {
        *changed = td->templ_changed;
        if (!*changed)
            return NULL;
    }

    gtk_tree_model_foreach(GTK_TREE_MODEL(td->template_list_store),
                           get_templ_helper, &list);
    list = g_list_reverse(list);
    return list;
}

GtkWidget *
gnc_ab_trans_dialog_get_parent(const GncABTransDialog *td)
{
    g_return_val_if_fail(td, NULL);
    return td->parent;
}

const AB_TRANSACTION *
gnc_ab_trans_dialog_get_ab_trans(const GncABTransDialog *td)
{
    g_return_val_if_fail(td, NULL);
    return td->ab_trans;
}

static AB_JOB *
get_available_empty_job(AB_ACCOUNT *ab_acc, GncABTransType trans_type)
{
    AB_JOB *job;

    switch (trans_type)
    {
    case SINGLE_DEBITNOTE:
        job = AB_JobSingleDebitNote_new(ab_acc);
        break;
    case SINGLE_INTERNAL_TRANSFER:
        job = AB_JobInternalTransfer_new(ab_acc);
        break;
    case SINGLE_TRANSFER:
    default:
        job = AB_JobSingleTransfer_new(ab_acc);
    };

    if (!job || AB_Job_CheckAvailability(job, 0))
    {
        if (job) AB_Job_free(job);
        return NULL;
    }
    return job;
}

AB_JOB *
gnc_ab_trans_dialog_get_job(const GncABTransDialog *td)
{
    g_return_val_if_fail(td, NULL);
    return gnc_ab_get_trans_job(td->ab_acc, td->ab_trans, td->trans_type);
}

AB_JOB *
gnc_ab_get_trans_job(AB_ACCOUNT *ab_acc, const AB_TRANSACTION *ab_trans,
                     GncABTransType trans_type)
{
    AB_JOB *job;

    g_return_val_if_fail(ab_acc && ab_trans, NULL);

    job = get_available_empty_job(ab_acc, trans_type);
    if (job)
    {
        switch (trans_type)
        {
        case SINGLE_DEBITNOTE:
            AB_JobSingleDebitNote_SetTransaction(job, ab_trans);
            break;
        case SINGLE_INTERNAL_TRANSFER:
            AB_JobInternalTransfer_SetTransaction(job, ab_trans);
            break;
        case SINGLE_TRANSFER:
        default:
            AB_JobSingleTransfer_SetTransaction(job, ab_trans);
        };
    }
    return job;
}

void
templ_list_row_activated_cb(GtkTreeView *view, GtkTreePath *path,
                            GtkTreeViewColumn *column, gpointer user_data)
{
    GncABTransDialog *td = user_data;
    GtkTreeModel *model;
    GtkTreeIter iter;
    GncABTransTempl *templ;
    const gchar *old_name, *new_name;
    const gchar *old_account, *new_account;
    const gchar *old_bankcode, *new_bankcode;
    const gchar *old_purpose, *new_purpose;
    const gchar *old_purpose_cont, *new_purpose_cont;
    GtkWidget *amount_widget;
    const gchar *old_amount_text;
    gnc_numeric old_amount, new_amount;

    g_return_if_fail(td);

    ENTER("td=%p", td);
    if (!gtk_tree_model_get_iter(GTK_TREE_MODEL(td->template_list_store), &iter,
                                 path))
    {
        LEAVE("Could not get iter");
        return;
    }
    gtk_tree_model_get(GTK_TREE_MODEL(td->template_list_store), &iter,
                       TEMPLATE_POINTER, &templ, -1);

    /* Get old values */
    old_name = gtk_entry_get_text(GTK_ENTRY(td->recp_name_entry));
    old_account = gtk_entry_get_text(GTK_ENTRY(td->recp_account_entry));
    old_bankcode = gtk_entry_get_text(GTK_ENTRY(td->recp_bankcode_entry));
    old_purpose = gtk_entry_get_text(GTK_ENTRY(td->purpose_entry));
    old_purpose_cont = gtk_entry_get_text(GTK_ENTRY(td->purpose_cont_entry));
    amount_widget = gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(td->amount_edit));
    old_amount_text = gtk_entry_get_text(GTK_ENTRY(amount_widget));
    old_amount = gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(td->amount_edit));

    /* Get new values */
    new_name = gnc_ab_trans_templ_get_recp_name(templ);
    new_account = gnc_ab_trans_templ_get_recp_account(templ);
    new_bankcode = gnc_ab_trans_templ_get_recp_bankcode(templ);
    new_purpose = gnc_ab_trans_templ_get_purpose(templ);
    new_purpose_cont = gnc_ab_trans_templ_get_purpose_cont(templ);
    new_amount = gnc_ab_trans_templ_get_amount(templ);
    if (!new_name) new_name = "";
    if (!new_account) new_account = "";
    if (!new_bankcode) new_bankcode = "";
    if (!new_purpose) new_purpose = "";
    if (!new_purpose_cont) new_purpose_cont = "";

    /* Check for differences to avoid overwriting entered text */
    if ((*old_name && strcmp(old_name, new_name))
            || (*old_account && strcmp(old_account, new_account))
            || (*old_bankcode && strcmp(old_bankcode, new_bankcode))
            || (*old_purpose && strcmp(old_purpose, new_purpose))
            || (*old_purpose_cont && strcmp(old_purpose_cont, new_purpose_cont))
            || (*old_amount_text && !gnc_numeric_equal(old_amount, new_amount)))
    {
        if (!gnc_verify_dialog(
                    td->parent, FALSE,
                    _("Do you really want to overwrite your changes with the "
                      "contents of the template \"%s\"?"),
                    gnc_ab_trans_templ_get_name(templ)))
        {

            LEAVE("aborted");
            return;
        }
    }

    /* Fill in */
    gtk_entry_set_text(GTK_ENTRY(td->recp_name_entry), new_name);
    gtk_entry_set_text(GTK_ENTRY(td->recp_account_entry), new_account);
    gtk_entry_set_text(GTK_ENTRY(td->recp_bankcode_entry), new_bankcode);
    gtk_entry_set_text(GTK_ENTRY(td->purpose_entry), new_purpose);
    gtk_entry_set_text(GTK_ENTRY(td->purpose_cont_entry), new_purpose_cont);
    gnc_amount_edit_set_amount(GNC_AMOUNT_EDIT(td->amount_edit), new_amount);
    LEAVE(" ");
}

void
dat_bankcode_changed_cb(GtkEditable *editable, gpointer user_data)
{
#if HAVE_KTOBLZCHECK_H
    GncABTransDialog *td = user_data;
    const AccountNumberCheck_Record *record;
    const gchar *input = gtk_entry_get_text(GTK_ENTRY(td->recp_bankcode_entry));

    g_return_if_fail(td);

    ENTER("td=%p, input=%s", td, input);
    record = AccountNumberCheck_findBank(td->blzcheck, input);

    if (record)
    {
        const char *bankname = AccountNumberCheck_Record_bankName(record);
        GError *error = NULL;
        const char *ktoblzcheck_encoding =
#ifdef KTOBLZCHECK_VERSION_MAJOR
            /* This version number macro has been added in ktoblzcheck-1.10, but
             * this function exists already since ktoblzcheck-1.7, so we're on
             * the safe side. */
            AccountNumberCheck_stringEncoding()
#else
            /* Every ktoblzcheck release before 1.10 is guaranteed to return
             * strings only in ISO-8859-15. */
            "ISO-8859-15"
#endif
            ;
        gchar *utf8_bankname = g_convert(bankname, strlen(bankname), "UTF-8",
                                         ktoblzcheck_encoding, NULL, NULL,
                                         &error);

        if (error)
        {
            g_critical("Error converting bankname \"%s\" to UTF-8", bankname);
            g_error_free (error);
            /* Conversion was erroneous, so don't use the string */
            utf8_bankname = g_strdup(_("(unknown)"));
        }
        gtk_label_set_text(GTK_LABEL(td->recp_bankname_label),
                           *utf8_bankname ? utf8_bankname : _("(unknown)"));
        DEBUG("Found: %s", utf8_bankname);
        g_free(utf8_bankname);
    }
    else
    {
        gtk_label_set_text(GTK_LABEL(td->recp_bankname_label), _("(unknown)"));
    }
    LEAVE(" ");
#endif
}

struct _FindTemplData
{
    const gchar *name;
    const GncABTransTempl *pointer;
};

static gboolean
find_templ_helper(GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter,
                  gpointer user_data)
{
    struct _FindTemplData *data = user_data;
    gchar *name;
    GncABTransTempl *templ;
    gboolean match;

    g_return_val_if_fail(model && data, TRUE);
    gtk_tree_model_get(model, iter,
                       TEMPLATE_NAME, &name,
                       TEMPLATE_POINTER, &templ,
                       -1);
    if (data->name)
    {
        /* Search for the template by name */
        g_return_val_if_fail(!data->pointer, TRUE);
        match = strcmp(name, data->name) == 0;
        if (match) data->pointer = templ;
    }
    else
    {
        /* Search for the template by template pointer */
        g_return_val_if_fail(!data->name, TRUE);
        match = templ == data->pointer;
        if (match) data->name = g_strdup(name);
    }
    g_free(name);
    return match;
}

void
dat_add_templ_cb(GtkButton *button, gpointer user_data)
{
    GncABTransDialog *td = user_data;
    GladeXML *xml;
    GtkWidget *dialog;
    GtkWidget *entry;
    gint retval;
    const gchar *name;
    GncABTransTempl *templ;
    struct _FindTemplData data;
    GtkTreeSelection *selection;
    GtkTreeIter cur_iter;
    GtkTreeIter new_iter;

    g_return_if_fail(td);

    ENTER("td=%p", td);
    xml = gnc_glade_xml_new ("aqbanking.glade", "Template Name Dialog");
    dialog = glade_xml_get_widget(xml, "Template Name Dialog");
    g_object_set_data_full(G_OBJECT(dialog), "xml", xml, g_object_unref);
    entry = glade_xml_get_widget(xml, "template_name");

    /* Suggest recipient name as name of the template */
    gtk_entry_set_text(GTK_ENTRY(entry),
                       gtk_entry_get_text(GTK_ENTRY(td->recp_name_entry)));

    do
    {
        retval = gtk_dialog_run(GTK_DIALOG(dialog));
        if (retval != GTK_RESPONSE_OK)
            break;

        name = gtk_entry_get_text(GTK_ENTRY(entry));
        if (!*name)
            break;

        data.name = name;
        data.pointer = NULL;
        gtk_tree_model_foreach(GTK_TREE_MODEL(td->template_list_store),
                               find_templ_helper, &data);
        if (data.pointer)
        {
            gnc_error_dialog(dialog, "%s",
                             _("A template with the given name already exists.  "
                               "Please enter another name."));
            continue;
        }

        /* Create a new template */
        templ = gnc_ab_trans_templ_new_full(
                    name,
                    gtk_entry_get_text(GTK_ENTRY(td->recp_name_entry)),
                    gtk_entry_get_text(GTK_ENTRY(td->recp_account_entry)),
                    gtk_entry_get_text(GTK_ENTRY(td->recp_bankcode_entry)),
                    gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(td->amount_edit)),
                    gtk_entry_get_text(GTK_ENTRY(td->purpose_entry)),
                    gtk_entry_get_text (GTK_ENTRY(td->purpose_cont_entry)));

        /* Insert it, either after the selected one or at the end */
        selection = gtk_tree_view_get_selection(td->template_gtktreeview);
        if (gtk_tree_selection_get_selected(selection, NULL, &cur_iter))
        {
            gtk_list_store_insert_after(td->template_list_store,
                                        &new_iter, &cur_iter);
        }
        else
        {
            gtk_list_store_append(td->template_list_store, &new_iter);
        }
        gtk_list_store_set(td->template_list_store, &new_iter,
                           TEMPLATE_NAME, name,
                           TEMPLATE_POINTER, templ,
                           -1);
        td->templ_changed = TRUE;
        DEBUG("Added template with name %s", name);
        break;
    }
    while (TRUE);

    gtk_widget_destroy(dialog);

    LEAVE(" ");
}

void
dat_moveup_templ_cb(GtkButton *button, gpointer user_data)
{
    GncABTransDialog *td = user_data;
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeIter iter;
    GtkTreePath *prev_path;
    GtkTreeIter prev_iter;

    g_return_if_fail(td);

    selection = gtk_tree_view_get_selection(td->template_gtktreeview);
    if (!gtk_tree_selection_get_selected(selection, &model, &iter))
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
dat_movedown_templ_cb(GtkButton *button, gpointer user_data)
{
    GncABTransDialog *td = user_data;
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeIter iter;
    GtkTreeIter next_iter;

    g_return_if_fail(td);

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
dat_sort_templ_cb(GtkButton *button, gpointer user_data)
{
    GncABTransDialog *td = user_data;

    g_return_if_fail(td);

    ENTER("td=%p", td);
    gtk_tree_sortable_set_sort_column_id(
        GTK_TREE_SORTABLE(td->template_list_store),
        TEMPLATE_NAME, GTK_SORT_ASCENDING);
    gtk_tree_sortable_set_sort_column_id(
        GTK_TREE_SORTABLE(td->template_list_store),
        GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID,
        GTK_SORT_ASCENDING);
    td->templ_changed = TRUE;
    LEAVE(" ");
}

void
dat_del_templ_cb(GtkButton *button, gpointer user_data)
{
    GncABTransDialog *td = user_data;
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeIter iter;
    gchar *name;

    g_return_if_fail(td);

    ENTER("td=%p", td);
    selection = gtk_tree_view_get_selection(td->template_gtktreeview);
    if (!gtk_tree_selection_get_selected (selection, &model, &iter))
    {
        LEAVE("None selected");
        return;
    }

    gtk_tree_model_get(model, &iter, TEMPLATE_NAME, &name, -1);
    if (gnc_verify_dialog(
                td->parent, FALSE,
                _("Do you really want to delete the template with the name \"%s\"?"),
                name))
    {
        gtk_list_store_remove(GTK_LIST_STORE(model), &iter);
        td->templ_changed = TRUE;
        DEBUG("Deleted template with name %s", name);
    }
    g_free(name);
    LEAVE(" ");
}
