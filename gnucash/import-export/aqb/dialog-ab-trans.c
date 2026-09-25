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

#include <config.h>

#include <glib/gi18n.h>
#include "gnc-ab-utils.h"
#if (AQBANKING_VERSION_INT >= 60400)
#include <aqbanking/types/transaction.h>

#include <gnc-aqbanking-templates.h>
#endif
#include "dialog-ab-trans.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-ui.h"
#include "import-selection-model.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

#if (AQBANKING_VERSION_INT >= 60400)
/* Template handling */
static void gnc_ab_trans_dialog_fill_templ_helper(gpointer data, gpointer user_data);
static GtkStringObject *gnc_ab_trans_dialog_template_row_new (GncABTransTempl *templ);
static void gnc_ab_trans_dialog_template_factory_setup (GtkListItemFactory *factory,
                                                         GtkListItem *list_item,
                                                         gpointer user_data);
static void gnc_ab_trans_dialog_template_factory_bind (GtkListItemFactory *factory,
                                                        GtkListItem *list_item,
                                                        gpointer user_data);
#endif
static AB_TRANSACTION *gnc_ab_trans_dialog_fill_values(GncABTransDialog *td);
static GNC_AB_JOB *gnc_ab_trans_dialog_get_available_empty_job(GNC_AB_ACCOUNT_SPEC *ab_acc,
        GncABTransType trans_type);

static void gnc_ab_trans_dialog_check_iban(const GncABTransDialog *td,
        const AB_TRANSACTION *trans);

/* Callbacks - connected with GtkBuilder - so should not be static */
void gnc_ab_trans_dialog_add_templ_cb(GtkButton *button, gpointer user_data);
void gnc_ab_trans_dialog_moveup_templ_cb(GtkButton *button, gpointer user_data);
void gnc_ab_trans_dialog_movedown_templ_cb(GtkButton *button, gpointer user_data);
void gnc_ab_trans_dialog_sort_templ_cb(GtkButton *button, gpointer user_data);
void gnc_ab_trans_dialog_del_templ_cb(GtkButton *button, gpointer user_data);
void gnc_ab_trans_dialog_ibanentry_filter_cb (GtkEditable *editable,
        const gchar *text,
        gint         length,
        gint        *position,
        gpointer     user_data);
void gnc_ab_trans_dialog_bicentry_filter_cb (GtkEditable *editable,
        const gchar *text,
        gint         length,
        gint        *position,
        gpointer     user_data);
void gnc_ab_trans_dialog_templ_list_row_activated_cb(GtkColumnView *view,
        guint position, gpointer user_data);

typedef struct _GncABTransDialogRunData GncABTransDialogRunData;
typedef struct _TemplateDeleteRequest TemplateDeleteRequest;

struct _TemplateDeleteRequest
{
    GncABTransDialog *dialog;
    GtkStringObject *row;
};

static void gnc_ab_trans_dialog_verify_values(GncABTransDialog *td);
static gboolean gnc_ab_trans_dialog_prepare (GncABTransDialog *td);
static void gnc_ab_trans_dialog_complete (GncABTransDialogRunData *data,
                                          gint response);
static void gnc_ab_trans_dialog_window_destroyed (GtkWidget *widget,
                                                  gpointer user_data);

struct _GncABTransDialogRunData
{
    GTask *task;
    GncABTransDialog *dialog;
    gulong now_handler;
    gulong later_handler;
    gulong cancel_handler;
    gulong close_handler;
    GtkEventController *shortcuts;
};


struct _GncABTransDialog
{
    /* The dialog itself */
    GtkWidget *dialog;
    GtkWidget *parent;
    GncABTransDialogRunData *run_data;
    GNC_AB_ACCOUNT_SPEC *ab_acc;

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

    /* Originator's name (might have to be edited by the user) */
    GtkWidget *orig_name_entry;

    /* The template choosing GtkColumnView/GListStore */
    GtkColumnView *template_view;
    GListStore *template_store;
    GtkSingleSelection *template_selection;

    /* Execution controls */
    GtkWidget *exec_button;
    GtkWidget *exec_later_button;
    GtkWidget *cancel_button;

    /* A pending confirmation owns itself; td_free() only invalidates td. */
    TemplateDeleteRequest *template_delete_request;

    /* Flag, if template list has been changed */
    gboolean templ_changed;

    /* The aqbanking transaction that got created here */
    AB_TRANSACTION *ab_trans;

    /* The gnucash transaction that got created here */
    Transaction *gnc_trans;
};

gboolean gnc_ab_trans_isSEPA(GncABTransType t)
{
    switch (t)
    {
    case SEPA_TRANSFER:
#if (AQBANKING_VERSION_INT >= 60400)
    case SEPA_INTERNAL_TRANSFER:
#endif
    case SEPA_DEBITNOTE:
        return TRUE;
    default:
        return FALSE;
    }
}

#if (AQBANKING_VERSION_INT >= 60400)
#define TEMPLATE_ROW_POINTER "template-pointer"

static GtkStringObject *
gnc_ab_trans_dialog_template_row_new (GncABTransTempl *templ)
{
    GtkStringObject *row;

    g_return_val_if_fail (templ, NULL);
    row = gtk_string_object_new (gnc_ab_trans_templ_get_name (templ));
    g_object_set_data_full (G_OBJECT (row), TEMPLATE_ROW_POINTER, templ,
                            (GDestroyNotify)gnc_ab_trans_templ_free);
    return row;
}

static void
gnc_ab_trans_dialog_template_factory_setup (GtkListItemFactory *factory,
                                            GtkListItem *list_item,
                                            gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    (void)factory;
    (void)user_data;
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (list_item, label);
}

static void
gnc_ab_trans_dialog_template_factory_bind (GtkListItemFactory *factory,
                                           GtkListItem *list_item,
                                           gpointer user_data)
{
    GtkStringObject *row = GTK_STRING_OBJECT (gtk_list_item_get_item (list_item));

    (void)factory;
    (void)user_data;
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (list_item)),
                        gtk_string_object_get_string (row));
}

static void
gnc_ab_trans_dialog_fill_templ_helper(gpointer data, gpointer user_data)
{
    GncABTransTempl *templ = data;
    GListStore *store = user_data;
    GtkStringObject *row;

    g_return_if_fail(templ && store);
    row = gnc_ab_trans_dialog_template_row_new (templ);
    g_list_store_append (store, row);
    g_object_unref (row);
}
#endif
/**
 * Create a new AB_TRANSACTION, fill the values from the entry fields into it
 * and return it.  The caller must AB_TRANSACTION_free() it when finished.
 */
static AB_TRANSACTION *
gnc_ab_trans_dialog_fill_values(GncABTransDialog *td)
{
    /* Fill in the user-entered values */
    AB_TRANSACTION *trans = AB_Transaction_new();
    AB_VALUE *value;

    AB_Banking_FillTransactionFromAccountSpec(trans, td->ab_acc);

    if (gnc_ab_trans_isSEPA(td->trans_type))
    {
        AB_Transaction_SetRemoteBic(
                    trans, gnc_entry_get_text(GTK_ENTRY(td->recp_bankcode_entry)));
        AB_Transaction_SetRemoteIban(
                    trans, gnc_entry_get_text(GTK_ENTRY(td->recp_account_entry)));
        AB_Transaction_SetLocalName(
                    trans, gnc_entry_get_text(GTK_ENTRY(td->orig_name_entry)));
    }
    else
    {
        AB_Transaction_SetRemoteBankCode(
                    trans, gnc_entry_get_text(GTK_ENTRY(td->recp_bankcode_entry)));
        AB_Transaction_SetRemoteAccountNumber(
                    trans, gnc_entry_get_text(GTK_ENTRY(td->recp_account_entry)));
    }
    AB_Transaction_SetRemoteCountry(trans, "DE");
    AB_Transaction_SetRemoteName(
        trans, gnc_entry_get_text(GTK_ENTRY(td->recp_name_entry)));

    AB_Transaction_AddPurposeLine(
        trans, gnc_entry_get_text(GTK_ENTRY(td->purpose_entry)));
    AB_Transaction_AddPurposeLine(
        trans, gnc_entry_get_text(GTK_ENTRY(td->purpose_cont_entry)));
    AB_Transaction_AddPurposeLine(
        trans, gnc_entry_get_text(GTK_ENTRY(td->purpose_cont2_entry)));
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
        break;
    }

    return trans;
}

GncABTransDialog *
gnc_ab_trans_dialog_new(GtkWidget *parent, GNC_AB_ACCOUNT_SPEC *ab_acc,
                        gint commodity_scu, GncABTransType trans_type,
                        GList *templates)
{
    GncABTransDialog *td;
    GtkBuilder  *builder;
    const gchar *ab_ownername;
    const gchar *ab_accountnumber;
    const gchar *ab_bankname;
    const gchar *ab_bankcode;
    G_GNUC_UNUSED GtkWidget *trans_vbox;
    GtkWidget *heading_label;
    GtkWidget *recp_name_heading;
    GtkWidget *recp_account_heading;
    GtkWidget *recp_bankcode_heading;
    GtkWidget *amount_hbox, *amount_label;
    GtkWidget *orig_name_heading;
    GtkWidget *orig_account_heading;
    GtkWidget *orig_account_label;
    G_GNUC_UNUSED GtkWidget *orig_bankname_heading;
    GtkWidget *orig_bankname_label;
    GtkWidget *orig_bankcode_heading;
    GtkWidget *orig_bankcode_label;
#if (AQBANKING_VERSION_INT >= 60400)
    GtkExpander *template_expander;
    GtkWidget *template_label;
    GtkWidget *add_templ_button;
    GtkWidget *del_templ_button;
    GtkScrolledWindow *template_scrolledwindow;
    GtkListItemFactory *template_factory;
    GtkColumnViewColumn *template_column;
#endif

    g_return_val_if_fail(ab_acc, NULL);

    ab_ownername = AB_AccountSpec_GetOwnerName(ab_acc);
    if (!ab_ownername)
        ab_ownername = "";
    ab_accountnumber = AB_AccountSpec_GetAccountNumber(ab_acc);
    ab_bankcode = AB_AccountSpec_GetBankCode(ab_acc);
    ab_bankname = _("(unknown)");

    td = g_new0(GncABTransDialog, 1);
    td->parent = parent;
    td->ab_acc = ab_acc;
    td->trans_type = trans_type;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-ab.glade", "aqbanking_transaction_dialog");
    td->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "aqbanking_transaction_dialog"));

    if (parent)
        gtk_window_set_transient_for(GTK_WINDOW(td->dialog), GTK_WINDOW(parent));

    /* Extract widgets */
    trans_vbox = GTK_WIDGET(gtk_builder_get_object (builder, "trans_vbox"));
    heading_label = GTK_WIDGET(gtk_builder_get_object (builder, "heading_label"));
    recp_name_heading = GTK_WIDGET(gtk_builder_get_object (builder, "recp_name_heading"));
    td->recp_name_entry = GTK_WIDGET(gtk_builder_get_object (builder, "recp_name_entry"));
    recp_account_heading = GTK_WIDGET(gtk_builder_get_object (builder, "recp_account_heading"));
    td->recp_account_entry = GTK_WIDGET(gtk_builder_get_object (builder, "recp_account_entry"));
    recp_bankcode_heading = GTK_WIDGET(gtk_builder_get_object (builder, "recp_bankcode_heading"));
    td->recp_bankcode_entry = GTK_WIDGET(gtk_builder_get_object (builder, "recp_bankcode_entry"));
    td->recp_bankname_label = GTK_WIDGET(gtk_builder_get_object (builder, "recp_bankname_label"));
    amount_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "amount_hbox"));
    amount_label = GTK_WIDGET(gtk_builder_get_object (builder, "amount_label"));
    td->purpose_entry = GTK_WIDGET(gtk_builder_get_object (builder, "purpose_entry"));
    td->purpose_cont_entry = GTK_WIDGET(gtk_builder_get_object (builder, "purpose_cont_entry"));
    td->purpose_cont2_entry = GTK_WIDGET(gtk_builder_get_object (builder, "purpose_cont2_entry"));
    td->purpose_cont3_entry = GTK_WIDGET(gtk_builder_get_object (builder, "purpose_cont3_entry"));
    td->exec_button = GTK_WIDGET(gtk_builder_get_object(builder, "exec_now_button"));
    td->exec_later_button = GTK_WIDGET(gtk_builder_get_object(
        builder, "exec_later_button"));
    td->cancel_button = GTK_WIDGET(gtk_builder_get_object(builder, "cancel_button"));
    orig_name_heading = GTK_WIDGET(gtk_builder_get_object (builder, "orig_name_heading"));
    td->orig_name_entry = GTK_WIDGET(gtk_builder_get_object (builder, "orig_name_label"));
    orig_account_heading = GTK_WIDGET(gtk_builder_get_object (builder, "orig_account_heading"));
    orig_account_label = GTK_WIDGET(gtk_builder_get_object (builder, "orig_account_label"));
    orig_bankname_heading = GTK_WIDGET(gtk_builder_get_object (builder, "orig_bankname_heading"));
    orig_bankname_label = GTK_WIDGET(gtk_builder_get_object (builder, "orig_bankname_label"));
    orig_bankcode_heading = GTK_WIDGET(gtk_builder_get_object (builder, "orig_bankcode_heading"));
    orig_bankcode_label = GTK_WIDGET(gtk_builder_get_object (builder, "orig_bankcode_label"));
#if (AQBANKING_VERSION_INT >= 60400)
    template_expander = GTK_EXPANDER(gtk_builder_get_object (builder, "expander1"));
    template_label = GTK_WIDGET(gtk_builder_get_object (builder, "label1"));
    add_templ_button= GTK_WIDGET(gtk_builder_get_object(builder, "add_templ_button"));
    del_templ_button= GTK_WIDGET(gtk_builder_get_object(builder, "del_templ_button"));
    template_scrolledwindow = GTK_SCROLLED_WINDOW(gtk_builder_get_object (builder,
                                                  "template_scrolledwindow"));
#endif

    /* Amount edit */
    td->amount_edit = gnc_amount_edit_new();
    gtk_box_append (GTK_BOX(amount_hbox), GTK_WIDGET(td->amount_edit));
    gnc_amount_edit_make_mnemonic_target(GNC_AMOUNT_EDIT(td->amount_edit), amount_label);
    gnc_amount_edit_set_evaluate_on_enter(GNC_AMOUNT_EDIT(td->amount_edit),
                                          TRUE);
    gnc_amount_edit_set_fraction(GNC_AMOUNT_EDIT(td->amount_edit),
                                 commodity_scu);

    /* Amount changes are evaluated on Enter. Evaluate again when focus leaves
     * the entry with GTK4's event controller. */
    {
        GtkEventController *focus = gtk_event_controller_focus_new ();

        g_signal_connect_swapped (focus, "leave",
                                  G_CALLBACK (gnc_ab_trans_dialog_verify_values),
                                  td);
        gtk_widget_add_controller (
            gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (td->amount_edit)), focus);
    }

    /* Check for what kind of transaction this should be, and change the
     * labels accordingly */
    switch (trans_type)
    {
    case SINGLE_TRANSFER:
    case SINGLE_INTERNAL_TRANSFER:
        /* all labels are already set */
        break;
    case SEPA_TRANSFER:
        gtk_label_set_text(GTK_LABEL (heading_label),
                           /* Translators: Strings from this file are
                              needed only in countries that have one of
                              aqbanking's Online Banking techniques
                              available. This is 'OFX DirectConnect'
                              (U.S. and others), 'HBCI' (Germany),
                              or 'YellowNet' (Switzerland). If none of
                              these techniques are available in your
                              country, you may safely ignore strings
                              from the import-export/hbci
                              subdirectory. */
                           _("Enter a SEPA Online Transfer"));
        gtk_label_set_text(GTK_LABEL(recp_account_heading),
                           _("Recipient IBAN (International Account Number)"));
        gtk_label_set_text(GTK_LABEL(recp_bankcode_heading),
                           _("Recipient BIC (Bank Code)"));

        gtk_label_set_text(GTK_LABEL(orig_account_heading),
                           _("Originator IBAN (International Account Number)"));
        gtk_label_set_text(GTK_LABEL(orig_bankcode_heading),
                           _("Originator BIC (Bank Code)"));
        break;

#if (AQBANKING_VERSION_INT >= 60400)
    case SEPA_INTERNAL_TRANSFER:
        gtk_label_set_text(GTK_LABEL (heading_label),
                           _("Enter a SEPA Internal Transfer"));
        gtk_label_set_text(GTK_LABEL(recp_account_heading),
                           _("Recipient IBAN (International Account Number)"));
        gtk_label_set_text(GTK_LABEL(recp_bankcode_heading),
                           _("Recipient BIC (Bank Code)"));

        gtk_label_set_text(GTK_LABEL(orig_account_heading),
                           _("Originator IBAN (International Account Number)"));
        gtk_label_set_text(GTK_LABEL(orig_bankcode_heading),
                           _("Originator BIC (Bank Code)"));
	/* Disable target account entry for SEPA internal transfers, but only let choose from templates */
    	gtk_widget_set_sensitive(td->recp_name_entry, FALSE);
    	gtk_widget_set_sensitive(td->recp_account_entry, FALSE);
    	gtk_widget_set_sensitive(td->recp_bankcode_entry, FALSE);
    	gtk_widget_set_sensitive(add_templ_button, FALSE);
    	gtk_widget_set_visible(add_templ_button, FALSE);
        gtk_widget_set_focusable (add_templ_button, FALSE);
    	gtk_widget_set_sensitive(del_templ_button, FALSE);
    	gtk_widget_set_visible(del_templ_button, FALSE);
        gtk_widget_set_focusable (del_templ_button, FALSE);
        gtk_label_set_text(GTK_LABEL(template_label),
                           _("Target Accounts"));
        gtk_expander_set_expanded(template_expander, TRUE);
        break;
#endif

    case SINGLE_DEBITNOTE:
        /* this case is no longer in use; don't introduce extra strings */
        break;

    case SEPA_DEBITNOTE:
        gtk_label_set_text(GTK_LABEL (heading_label),
                           _("Enter a SEPA Online Direct Debit Note"));

        gtk_label_set_text(GTK_LABEL(recp_name_heading),
                           _("Debited Account Owner"));
        gtk_label_set_text(GTK_LABEL(recp_account_heading),
                           _("Debited IBAN (International Account Number)"));
        gtk_label_set_text(GTK_LABEL(recp_bankcode_heading),
                           _("Debited BIC (Bank Code)"));

        gtk_label_set_text(GTK_LABEL(orig_name_heading),
                           _("Credited Account Owner"));
        gtk_label_set_text(GTK_LABEL(orig_account_heading),
                           _("Credited IBAN (International Account Number)"));
        gtk_label_set_text(GTK_LABEL(orig_bankcode_heading),
                           _("Credited BIC (Bank Code)"));
        break;

    default:
        g_critical("gnc_ab_trans_dialog_new: Oops, unknown GncABTransType %d",
                   trans_type);
        break;
    }

    /* Additionally change the labels for the European (SEPA) transactions */
    if (gnc_ab_trans_isSEPA(trans_type))
    {
        // Also, SEPA might have much longer IBAN (up to 34 chars) and BIC (11)
        gtk_entry_set_max_length(GTK_ENTRY(td->recp_bankcode_entry), 11);
        gtk_entry_set_max_length(GTK_ENTRY(td->recp_account_entry), 34);
    }

    gnc_entry_set_text(GTK_ENTRY(td->orig_name_entry), ab_ownername);
    gtk_label_set_text(GTK_LABEL(orig_bankname_label), ab_bankname);
    if (gnc_ab_trans_isSEPA(trans_type))
    {
        gtk_widget_set_sensitive(GTK_WIDGET(td->orig_name_entry), TRUE);
        ab_accountnumber = AB_AccountSpec_GetIban(ab_acc);
        ab_bankcode = AB_AccountSpec_GetBic(ab_acc);
        gtk_label_set_text(GTK_LABEL(orig_account_label), ab_accountnumber);
        gtk_label_set_text (GTK_LABEL (orig_bankcode_label), ab_bankcode);
    }
    else
    {
        gtk_widget_set_sensitive(GTK_WIDGET(td->orig_name_entry), FALSE);
        gtk_label_set_text(GTK_LABEL(orig_account_label), ab_accountnumber);
        gtk_label_set_text (GTK_LABEL (orig_bankcode_label), ab_bankcode);
    }

#if (AQBANKING_VERSION_INT >= 60400)
    /* Fill list for choosing a transaction template */
    td->template_store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    g_list_foreach (templates, gnc_ab_trans_dialog_fill_templ_helper, td->template_store);
    td->template_selection = gnc_import_single_selection_new (G_LIST_MODEL (td->template_store));
    td->template_view = GTK_COLUMN_VIEW (gtk_column_view_new (GTK_SELECTION_MODEL
                                                               (g_object_ref (td->template_selection))));
    template_factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (template_factory, "setup",
                      G_CALLBACK (gnc_ab_trans_dialog_template_factory_setup), NULL);
    g_signal_connect (template_factory, "bind",
                      G_CALLBACK (gnc_ab_trans_dialog_template_factory_bind), NULL);
    template_column = gtk_column_view_column_new (_("Template Name"), template_factory);
    gtk_column_view_column_set_expand (template_column, TRUE);
    gtk_column_view_append_column (td->template_view, template_column);
    g_object_unref (template_column);
    gtk_scrolled_window_set_child (template_scrolledwindow, GTK_WIDGET (td->template_view));
    g_signal_connect (td->template_view, "activate",
                      G_CALLBACK (gnc_ab_trans_dialog_templ_list_row_activated_cb), td);
    td->templ_changed = FALSE;
#endif

    /* Connect the Signals */
    gnc_builder_connect_signals_full(builder, gnc_builder_connect_full_func, td);
    g_signal_connect (td->dialog, "destroy",
                      G_CALLBACK (gnc_ab_trans_dialog_window_destroyed), td);
    gtk_window_set_default_widget (GTK_WINDOW (td->dialog), td->exec_button);

    g_object_unref(G_OBJECT(builder));

    /* Disabled OK button until suitable values are filled */
    gnc_ab_trans_dialog_verify_values(td);

    return td;
}

static void
gnc_ab_trans_dialog_entry_set (GtkWidget* entry,
                               const gchar* message,
                               const gchar* icon_name)
{
    g_object_set (entry,
                  "secondary-icon-name", icon_name,
                  "secondary-icon-tooltip-text", message,
                  NULL);
}

static void
gnc_ab_trans_dialog_check_iban(const GncABTransDialog *td,
                               const AB_TRANSACTION *trans)
{
    if (gnc_ab_trans_isSEPA(td->trans_type))
    {
        /* Verify the correct IBAN bank code */
        int rv = AB_Banking_CheckIban(AB_Transaction_GetRemoteIban(trans));
        if (rv != 0) {
            gchar *message = g_strdup_printf(_("The internal check of the destination IBAN '%s' "
                                               "failed. This means "
                                               "the account number might contain an error."),
                                             AB_Transaction_GetRemoteIban(trans));
            gnc_ab_trans_dialog_entry_set (td->recp_account_entry, message,
                                           "dialog-warning");
        }
        else
        {
            gnc_ab_trans_dialog_entry_set (td->recp_account_entry, "",
                                           NULL);
            gnc_ab_trans_dialog_entry_set (td->recp_bankcode_entry, "",
                                           NULL);
        }
    }
    else
    {
        /* this case is no longer in use */
        return;
    }
}

static void
gnc_ab_trans_dialog_clear_transaction(GncABTransDialog *td)
{
    AB_Transaction_free(td->ab_trans);
    td->ab_trans = NULL;
}

void
gnc_ab_trans_dialog_verify_values(GncABTransDialog *td)
{
    gchar* purpose;
    gchar* othername;
    const gchar* account;
    const gchar* bankcode;

    gboolean values_ok = TRUE;

    GtkWidget *amount_entry = gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(td->amount_edit));

    /* Fill in the values from the entry fields into a new
     * AB_TRANSACTION */
    td->ab_trans = gnc_ab_trans_dialog_fill_values(td);

    // Verify that we have a local IBAN and BIC
    if (gnc_ab_trans_isSEPA(td->trans_type))
    {
        const char* localIBAN = AB_Transaction_GetLocalIban(td->ab_trans);
        if (!localIBAN || (strlen(localIBAN) == 0))
        {
            const char* localBankCode = AB_Transaction_GetLocalBankCode(td->ab_trans);
            const char* localAccountCode = AB_Transaction_GetLocalAccountNumber(td->ab_trans);
            values_ok = FALSE;
            gnc_error_dialog(GTK_WINDOW (td->dialog),
                             _("Your local bank account does not yet have the SEPA account information stored."
                               " We are sorry, but in this development version one additional step is necessary "
                               "which has not yet been implemented directly in gnucash. "
                               "Please execute the command line program \"aqhbci-tool\" for your account, as follows: "
                               "aqhbci-tool4 getaccsepa -b %s -a %s"),
                             (localBankCode ? localBankCode : ""),
                             (localAccountCode ? localAccountCode : ""));
        }
    }

    /* Check recipient / remote name */
    othername = gnc_ab_get_remote_name(td->ab_trans);
    if (!othername || !strlen(othername))
    {
        gnc_ab_trans_dialog_entry_set (td->recp_name_entry,
                                       _("You did not enter a recipient name. A recipient name is "
                                         "required for an online transfer.\n"),
                                       "process-stop");

        g_free (othername);
        values_ok = FALSE;
    }
    else
    {
        gnc_ab_trans_dialog_entry_set (td->recp_name_entry, "", NULL);
        g_free(othername);
    }

    /* Check account */
    account = gnc_ab_trans_isSEPA(td->trans_type)
            ? AB_Transaction_GetRemoteIban(td->ab_trans)
            : AB_Transaction_GetRemoteAccountNumber(td->ab_trans);
    if (!account || !strlen(account))
    {
        gnc_ab_trans_dialog_entry_set (td->recp_account_entry,
                                       _("You did not enter a recipient account. A recipient account is "
                                         "required for an online transfer.\n"),
                                       "process-stop");
        values_ok = FALSE;
    }
    else
    {
        gnc_ab_trans_dialog_entry_set (td->recp_account_entry, "", NULL);
    }
    /* Check bank */
    bankcode = gnc_ab_trans_isSEPA(td->trans_type)
            ? AB_Transaction_GetRemoteBic(td->ab_trans)
            : AB_Transaction_GetRemoteBankCode(td->ab_trans);
    if (!gnc_ab_trans_isSEPA(td->trans_type) && (!bankcode || !strlen(bankcode)))
    {
        gnc_ab_trans_dialog_entry_set (td->recp_bankcode_entry,
                                       _("You did not enter a recipient bank. A recipient bank is "
                                         "required for an online transfer.\n"),
                                       "process-stop");
        values_ok = FALSE;
    }
    else
    {
        gnc_ab_trans_dialog_entry_set (td->recp_bankcode_entry, "", NULL);
    }
    /* Check if account details are correct - gives warning only */
    if (values_ok)
        gnc_ab_trans_dialog_check_iban(td, td->ab_trans);

    /* Check transaction value */
    if (AB_Value_GetValueAsDouble(AB_Transaction_GetValue(td->ab_trans))
            == 0.0)
    {
        gnc_ab_trans_dialog_entry_set (amount_entry,
                                       _("The amount is zero or the amount field could not be "
                                         "interpreted correctly. You might have mixed up decimal "
                                         "point and comma, compared to your locale settings. "
                                         "This does not result in a valid online transfer job."),
                                       "process-stop");
        values_ok = FALSE;
    }
    else
    {
        gnc_ab_trans_dialog_entry_set (amount_entry, "", NULL);
    }

    /* Check transaction purpose. OFX doesn't do transfers. */
    purpose = gnc_ab_get_purpose(td->ab_trans, FALSE);
    if (!purpose || !strlen(purpose))
    {
        gnc_ab_trans_dialog_entry_set (td->purpose_entry,
                                       _("You did not enter any transaction purpose. A purpose is "
                                         "required for an online transfer.\n"),
                                       "process-stop");
        g_free (purpose);
        values_ok = FALSE;
    }
    else
    {
        gnc_ab_trans_dialog_entry_set (td->purpose_entry, "", NULL);
        g_free(purpose);
    }

    gtk_widget_set_sensitive(td->exec_button, values_ok);
    gnc_ab_trans_dialog_clear_transaction(td);
}

static gboolean
gnc_ab_trans_dialog_prepare (GncABTransDialog *td)
{
    GNC_AB_JOB *job;
    const AB_TRANSACTION_LIMITS *joblimits;
    guint8 max_purpose_lines;

    job = gnc_ab_trans_dialog_get_available_empty_job (td->ab_acc,
                                                        td->trans_type);
    if (!job)
    {
        g_warning ("gnc_ab_trans_dialog_run_async: Oops, job not available");
        return FALSE;
    }

    joblimits = AB_AccountSpec_GetTransactionLimitsForCommand (
        td->ab_acc, AB_Transaction_GetCommand (job));
    max_purpose_lines = joblimits
        ? AB_TransactionLimits_GetMaxLinesPurpose (joblimits) : 2;
    gtk_widget_set_sensitive (td->purpose_cont_entry, max_purpose_lines > 1);
    gtk_widget_set_sensitive (td->purpose_cont2_entry, max_purpose_lines > 2);
    gtk_widget_set_sensitive (td->purpose_cont3_entry, max_purpose_lines > 3);
    if (joblimits)
    {
        gtk_entry_set_max_length (GTK_ENTRY (td->purpose_entry),
                                  AB_TransactionLimits_GetMaxLenPurpose (joblimits));
        gtk_entry_set_max_length (GTK_ENTRY (td->purpose_cont_entry),
                                  AB_TransactionLimits_GetMaxLenPurpose (joblimits));
        gtk_entry_set_max_length (GTK_ENTRY (td->purpose_cont2_entry),
                                  AB_TransactionLimits_GetMaxLenPurpose (joblimits));
        gtk_entry_set_max_length (GTK_ENTRY (td->purpose_cont3_entry),
                                  AB_TransactionLimits_GetMaxLenPurpose (joblimits));
        gtk_entry_set_max_length (GTK_ENTRY (td->recp_name_entry),
                                  AB_TransactionLimits_GetMaxLenRemoteName (joblimits));
    }
    AB_Transaction_free (job);
    return TRUE;
}

static void
gnc_ab_trans_dialog_now_clicked (GtkButton *button, gpointer user_data)
{
    (void)button;
    gnc_ab_trans_dialog_complete (user_data, GNC_RESPONSE_NOW);
}

static void
gnc_ab_trans_dialog_later_clicked (GtkButton *button, gpointer user_data)
{
    (void)button;
    gnc_ab_trans_dialog_complete (user_data, GNC_RESPONSE_LATER);
}

static void
gnc_ab_trans_dialog_cancel_clicked (GtkButton *button, gpointer user_data)
{
    (void)button;
    gnc_ab_trans_dialog_complete (user_data, GTK_RESPONSE_CANCEL);
}

static gboolean
gnc_ab_trans_dialog_close_requested (GtkWindow *window, gpointer user_data)
{
    (void)window;
    gnc_ab_trans_dialog_complete (user_data, GTK_RESPONSE_CANCEL);
    return TRUE;
}

static gboolean
gnc_ab_trans_dialog_escape_pressed (GtkWidget *widget, GVariant *args,
                                    gpointer user_data)
{
    (void)widget;
    (void)args;
    gnc_ab_trans_dialog_complete (user_data, GTK_RESPONSE_CANCEL);
    return TRUE;
}

static void
gnc_ab_trans_dialog_add_shortcuts (GncABTransDialogRunData *data)
{
    GtkShortcutController *controller = GTK_SHORTCUT_CONTROLLER (
        gtk_shortcut_controller_new ());

    gtk_shortcut_controller_set_scope (controller, GTK_SHORTCUT_SCOPE_MANAGED);
    gtk_shortcut_controller_add_shortcut (
        controller,
        gtk_shortcut_new (
            gtk_keyval_trigger_new (GDK_KEY_Escape, 0),
            gtk_callback_action_new (gnc_ab_trans_dialog_escape_pressed, data, NULL)));
    data->shortcuts = GTK_EVENT_CONTROLLER (controller);
    gtk_widget_add_controller (data->dialog->dialog, data->shortcuts);
}

static void
gnc_ab_trans_dialog_window_destroyed (GtkWidget *widget, gpointer user_data)
{
    GncABTransDialog *td = user_data;

    if (td->dialog == widget)
        td->dialog = NULL;
    if (td->run_data)
        gnc_ab_trans_dialog_complete (td->run_data, GTK_RESPONSE_CANCEL);
}

static void
gnc_ab_trans_dialog_complete (GncABTransDialogRunData *data, gint response)
{
    GncABTransDialog *td;
    GTask *task;

    if (!data || !data->dialog || data->dialog->run_data != data)
        return;

    td = data->dialog;
    td->run_data = NULL;
    task = data->task;
    if (td->dialog)
    {
        if (data->now_handler)
            g_signal_handler_disconnect (td->exec_button, data->now_handler);
        if (data->later_handler)
            g_signal_handler_disconnect (td->exec_later_button, data->later_handler);
        if (data->cancel_handler)
            g_signal_handler_disconnect (td->cancel_button, data->cancel_handler);
        if (data->close_handler)
            g_signal_handler_disconnect (td->dialog, data->close_handler);
        if (data->shortcuts)
        {
            gtk_widget_remove_controller (td->dialog, data->shortcuts);
            data->shortcuts = NULL;
        }
    }

    if (response == GNC_RESPONSE_NOW || response == GNC_RESPONSE_LATER)
    {
        g_clear_pointer (&td->ab_trans, AB_Transaction_free);
        td->ab_trans = gnc_ab_trans_dialog_fill_values (td);
        if (td->dialog)
            gtk_widget_set_visible (td->dialog, FALSE);
    }
    else if (td->dialog)
    {
        gtk_window_destroy (GTK_WINDOW (td->dialog));
        td->dialog = NULL;
    }

    g_task_return_int (task, response);
    g_object_unref (task);
    g_free (data);
}

void
gnc_ab_trans_dialog_run_async (GncABTransDialog *td,
                               GCancellable *cancellable,
                               GAsyncReadyCallback callback,
                               gpointer user_data)
{
    GncABTransDialogRunData *data;
    GTask *task;

    g_return_if_fail (td);
    if (td->run_data || !td->dialog)
    {
        task = g_task_new (NULL, cancellable, callback, user_data);
        g_task_return_new_error (task, G_IO_ERROR, G_IO_ERROR_FAILED,
                                 "The AqBanking transaction dialog is not available");
        g_object_unref (task);
        return;
    }

    if (!gnc_ab_trans_dialog_prepare (td))
    {
        task = g_task_new (NULL, cancellable, callback, user_data);
        g_task_return_int (task, GTK_RESPONSE_CANCEL);
        g_object_unref (task);
        return;
    }

    data = g_new0 (GncABTransDialogRunData, 1);
    data->task = g_task_new (NULL, cancellable, callback, user_data);
    data->dialog = td;
    td->run_data = data;
    g_task_set_source_tag (data->task, gnc_ab_trans_dialog_run_async);
    data->now_handler = g_signal_connect (
        td->exec_button, "clicked", G_CALLBACK (gnc_ab_trans_dialog_now_clicked), data);
    data->later_handler = g_signal_connect (
        td->exec_later_button, "clicked",
        G_CALLBACK (gnc_ab_trans_dialog_later_clicked), data);
    data->cancel_handler = g_signal_connect (
        td->cancel_button, "clicked",
        G_CALLBACK (gnc_ab_trans_dialog_cancel_clicked), data);
    data->close_handler = g_signal_connect (
        td->dialog, "close-request",
        G_CALLBACK (gnc_ab_trans_dialog_close_requested), data);
    gnc_ab_trans_dialog_add_shortcuts (data);
    gtk_window_present (GTK_WINDOW (td->dialog));
}

gboolean
gnc_ab_trans_dialog_run_finish (GAsyncResult *result, gint *response,
                                GError **error)
{
    GTask *task;

    g_return_val_if_fail (g_task_is_valid (result, NULL), FALSE);
    g_return_val_if_fail (response, FALSE);

    task = G_TASK (result);
    if (g_task_had_error (task))
    {
        g_task_propagate_int (task, error);
        return FALSE;
    }

    *response = g_task_propagate_int (task, error);
    return TRUE;
}
void
gnc_ab_trans_dialog_free(GncABTransDialog *td)
{
    if (!td) return;
    if (td->ab_trans)
        AB_Transaction_free(td->ab_trans);
    if (td->template_delete_request)
        td->template_delete_request->dialog = NULL;
    if (td->dialog)
        gtk_window_destroy (GTK_WINDOW(td->dialog));

#if (AQBANKING_VERSION_INT >= 60400)
    g_clear_object (&td->template_selection);
    g_clear_object (&td->template_store);
#endif
    g_free(td);
}

#if (AQBANKING_VERSION_INT >= 60400)
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

    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (td->template_store));
         position++)
    {
        GtkStringObject *row = g_list_model_get_item (G_LIST_MODEL (td->template_store),
                                                       position);
        list = g_list_prepend (list, g_object_get_data (G_OBJECT (row),
                                                        TEMPLATE_ROW_POINTER));
        g_object_unref (row);
    }
    list = g_list_reverse (list);
    return list;
}
#endif

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

static GNC_AB_JOB *
gnc_ab_trans_dialog_get_available_empty_job(GNC_AB_ACCOUNT_SPEC *ab_acc, GncABTransType trans_type)
{
    GNC_AB_JOB *job;
    AB_TRANSACTION_COMMAND cmd = AB_Transaction_CommandUnknown;

     switch (trans_type)
     {
     case SINGLE_DEBITNOTE: /* no longer in use */
         cmd=AB_Transaction_CommandDebitNote;
         break;
     case SINGLE_INTERNAL_TRANSFER:
         cmd=AB_Transaction_CommandInternalTransfer;
         break;
     case SEPA_TRANSFER:
         cmd=AB_Transaction_CommandSepaTransfer;
         break;
#if (AQBANKING_VERSION_INT >= 60400)
     case SEPA_INTERNAL_TRANSFER:
         cmd=AB_Transaction_CommandSepaInternalTransfer;
         break;
#endif
     case SEPA_DEBITNOTE:
         cmd=AB_Transaction_CommandSepaDebitNote;
         break;
     default:
        cmd=AB_Transaction_CommandTransfer; /* no longer in use */
         break;
     };
     if (!AB_AccountSpec_GetTransactionLimitsForCommand(ab_acc, cmd))
         return NULL;

     job = AB_Transaction_new();
     AB_Transaction_SetCommand(job, cmd);
     AB_Transaction_SetUniqueAccountId(job, AB_AccountSpec_GetUniqueId(ab_acc));
    return job;
}

GNC_AB_JOB *
gnc_ab_trans_dialog_get_job(const GncABTransDialog *td)
{
    g_return_val_if_fail(td, NULL);
    return gnc_ab_get_trans_job(td->ab_acc, td->ab_trans, td->trans_type);
}

GNC_AB_JOB *
gnc_ab_get_trans_job(GNC_AB_ACCOUNT_SPEC *ab_acc,
                     const AB_TRANSACTION *ab_trans,
                     GncABTransType trans_type)
{
    GNC_AB_JOB *job;

    g_return_val_if_fail(ab_acc && ab_trans, NULL);

    job = gnc_ab_trans_dialog_get_available_empty_job(ab_acc, trans_type);
    if (job)
    {
        AB_TRANSACTION *new_job;

        /* merge transactions */
        new_job=AB_Transaction_dup(ab_trans);
        AB_Transaction_SetCommand(new_job, AB_Transaction_GetCommand(job));
        AB_Transaction_SetUniqueAccountId(new_job,
                                          AB_Transaction_GetUniqueAccountId(job));

        AB_Transaction_free(job);
        return new_job;
    }
    return NULL;

}

#if (AQBANKING_VERSION_INT >= 60400)
void
gnc_ab_trans_dialog_templ_list_row_activated_cb(GtkColumnView *view,
                                                 guint position,
                                                 gpointer user_data)
{
    GncABTransDialog *td = user_data;
    GtkStringObject *row;
    GncABTransTempl *templ;
    const gchar *new_name;
    const gchar *new_account;
    const gchar *new_bankcode;
    const gchar *new_purpose;
    const gchar *new_purpose_cont;
    gnc_numeric new_amount;

    g_return_if_fail(td);
    (void)view;

    ENTER("td=%p", td);
    row = g_list_model_get_item (G_LIST_MODEL (td->template_store), position);
    if (!row)
    {
        LEAVE("Could not get row");
        return;
    }
    templ = g_object_get_data (G_OBJECT (row), TEMPLATE_ROW_POINTER);

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

    /* Fill in */
    gnc_entry_set_text(GTK_ENTRY(td->recp_name_entry), new_name);
    gnc_entry_set_text(GTK_ENTRY(td->recp_account_entry), new_account);
    gnc_entry_set_text(GTK_ENTRY(td->recp_bankcode_entry), new_bankcode);
    gnc_entry_set_text(GTK_ENTRY(td->purpose_entry), new_purpose);
    gnc_entry_set_text(GTK_ENTRY(td->purpose_cont_entry), new_purpose_cont);
    gnc_amount_edit_set_amount(GNC_AMOUNT_EDIT(td->amount_edit), new_amount);
    g_object_unref (row);
    LEAVE(" ");
}

static gboolean
gnc_ab_trans_dialog_template_name_exists (const GncABTransDialog *td,
                                          const gchar *name)
{
    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (td->template_store));
         position++)
    {
        GtkStringObject *row = g_list_model_get_item (G_LIST_MODEL (td->template_store),
                                                       position);
        gboolean exists = g_strcmp0 (gtk_string_object_get_string (row), name) == 0;
        g_object_unref (row);
        if (exists)
            return TRUE;
    }
    return FALSE;
}

typedef struct
{
    GtkBuilder *builder;
    GtkWidget *dialog;
    GtkWidget *entry;
    GncABTransDialog *td;
} TemplateNameDialog;

static void
template_name_dialog_free (TemplateNameDialog *info)
{
    if (!info)
        return;

    if (info->dialog)
    {
        g_signal_handlers_disconnect_by_data (info->dialog, info);
        gtk_window_destroy (GTK_WINDOW (info->dialog));
        info->dialog = NULL;
    }
    g_clear_object (&info->builder);
    g_free (info);
}

static void
template_name_dialog_destroyed (GtkWidget *widget, gpointer user_data)
{
    TemplateNameDialog *info = user_data;

    if (info->dialog == widget)
        info->dialog = NULL;
    g_clear_object (&info->builder);
    g_free (info);
}

static void
template_name_dialog_accept_clicked (GtkButton *button, gpointer user_data)
{
    TemplateNameDialog *info = user_data;
    const gchar *name;
    GncABTransTempl *templ;
    GtkStringObject *row;
    guint position;

    (void)button;
    name = gnc_entry_get_text (GTK_ENTRY (info->entry));
    if (!*name)
    {
        template_name_dialog_free (info);
        return;
    }

    if (gnc_ab_trans_dialog_template_name_exists (info->td, name))
    {
        gnc_error_dialog (GTK_WINDOW (info->dialog), "%s",
                          _("A template with the given name already exists. "
                            "Please enter another name."));
        return;
    }

    templ = gnc_ab_trans_templ_new_full (
        name, gnc_entry_get_text (GTK_ENTRY (info->td->recp_name_entry)),
        gnc_entry_get_text (GTK_ENTRY (info->td->recp_account_entry)),
        gnc_entry_get_text (GTK_ENTRY (info->td->recp_bankcode_entry)),
        gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (info->td->amount_edit)),
        gnc_entry_get_text (GTK_ENTRY (info->td->purpose_entry)),
        gnc_entry_get_text (GTK_ENTRY (info->td->purpose_cont_entry)));

    position = gnc_import_single_selection_get_insert_after_position (
        info->td->template_selection);
    row = gnc_ab_trans_dialog_template_row_new (templ);
    g_list_store_insert (info->td->template_store, position, row);
    gtk_single_selection_set_selected (info->td->template_selection, position);
    g_object_unref (row);
    info->td->templ_changed = TRUE;
    DEBUG ("Added template with name %s", name);
    template_name_dialog_free (info);
}

static void
template_name_dialog_cancel_clicked (GtkButton *button, gpointer user_data)
{
    (void)button;
    template_name_dialog_free (user_data);
}

static gboolean
template_name_dialog_close_requested (GtkWindow *window, gpointer user_data)
{
    (void)window;
    template_name_dialog_free (user_data);
    return TRUE;
}

static gboolean
template_name_dialog_escape_pressed (GtkWidget *widget, GVariant *args,
                                     gpointer user_data)
{
    (void)widget;
    (void)args;
    template_name_dialog_free (user_data);
    return TRUE;
}

static void
template_name_dialog_add_shortcuts (TemplateNameDialog *info)
{
    GtkShortcutController *controller = GTK_SHORTCUT_CONTROLLER (
        gtk_shortcut_controller_new ());

    gtk_shortcut_controller_set_scope (controller, GTK_SHORTCUT_SCOPE_MANAGED);
    gtk_shortcut_controller_add_shortcut (
        controller,
        gtk_shortcut_new (
            gtk_keyval_trigger_new (GDK_KEY_Escape, 0),
            gtk_callback_action_new (template_name_dialog_escape_pressed, info, NULL)));
    gtk_widget_add_controller (info->dialog, GTK_EVENT_CONTROLLER (controller));
}

void
gnc_ab_trans_dialog_add_templ_cb (GtkButton *button, gpointer user_data)
{
    GncABTransDialog *td = user_data;
    TemplateNameDialog *info;

    (void)button;
    g_return_if_fail (td);

    ENTER ("td=%p", td);
    info = g_new0 (TemplateNameDialog, 1);
    info->td = td;
    info->builder = gtk_builder_new ();
    gnc_builder_add_from_file (info->builder, "dialog-ab.glade",
                               "aqbanking_template_name_dialog");
    info->dialog = GTK_WIDGET (gtk_builder_get_object (
        info->builder, "aqbanking_template_name_dialog"));
    info->entry = GTK_WIDGET (gtk_builder_get_object (info->builder,
                                                       "template_name"));
    if (!info->dialog || !info->entry)
    {
        template_name_dialog_free (info);
        LEAVE ("Could not create template dialog");
        return;
    }

    gnc_entry_set_text (GTK_ENTRY (info->entry),
                        gnc_entry_get_text (GTK_ENTRY (td->recp_name_entry)));
    if (td->dialog)
        gtk_window_set_transient_for (GTK_WINDOW (info->dialog),
                                      GTK_WINDOW (td->dialog));
    g_signal_connect (gtk_builder_get_object (info->builder, "okbutton1"),
                      "clicked", G_CALLBACK (template_name_dialog_accept_clicked), info);
    g_signal_connect (gtk_builder_get_object (info->builder, "cancelbutton1"),
                      "clicked", G_CALLBACK (template_name_dialog_cancel_clicked), info);
    g_signal_connect (info->dialog, "close-request",
                      G_CALLBACK (template_name_dialog_close_requested), info);
    g_signal_connect (info->dialog, "destroy",
                      G_CALLBACK (template_name_dialog_destroyed), info);
    gtk_window_set_default_widget (
        GTK_WINDOW (info->dialog),
        GTK_WIDGET (gtk_builder_get_object (info->builder, "okbutton1")));
    template_name_dialog_add_shortcuts (info);
    gtk_window_present (GTK_WINDOW (info->dialog));
    LEAVE (" ");
}
void
gnc_ab_trans_dialog_moveup_templ_cb(GtkButton *button, gpointer user_data)
{
    GncABTransDialog *td = user_data;
    guint position;
    GtkStringObject *row;

    g_return_if_fail(td);

    position = gtk_single_selection_get_selected (td->template_selection);
    if (position == GTK_INVALID_LIST_POSITION || position == 0)
        return;

    row = g_list_model_get_item (G_LIST_MODEL (td->template_store), position);
    g_list_store_remove (td->template_store, position);
    g_list_store_insert (td->template_store, position - 1, row);
    gtk_single_selection_set_selected (td->template_selection, position - 1);
    g_object_unref (row);
    td->templ_changed = TRUE;
}

void
gnc_ab_trans_dialog_movedown_templ_cb(GtkButton *button, gpointer user_data)
{
    GncABTransDialog *td = user_data;
    guint position;
    guint count;
    GtkStringObject *row;

    g_return_if_fail(td);

    position = gtk_single_selection_get_selected (td->template_selection);
    count = g_list_model_get_n_items (G_LIST_MODEL (td->template_store));
    if (position == GTK_INVALID_LIST_POSITION || position + 1 >= count)
        return;

    row = g_list_model_get_item (G_LIST_MODEL (td->template_store), position);
    g_list_store_remove (td->template_store, position);
    g_list_store_insert (td->template_store, position + 1, row);
    gtk_single_selection_set_selected (td->template_selection, position + 1);
    g_object_unref (row);
    td->templ_changed = TRUE;
}

static gint
gnc_ab_trans_dialog_template_compare (gconstpointer left, gconstpointer right,
                                      gpointer user_data)
{
    (void)user_data;
    return g_utf8_collate (gtk_string_object_get_string ((GtkStringObject *)left),
                           gtk_string_object_get_string ((GtkStringObject *)right));
}

void
gnc_ab_trans_dialog_sort_templ_cb(GtkButton *button, gpointer user_data)
{
    GncABTransDialog *td = user_data;
    GtkStringObject *selected_row = NULL;
    guint selected_position;

    g_return_if_fail(td);

    ENTER("td=%p", td);
    selected_position = gtk_single_selection_get_selected (td->template_selection);
    if (selected_position != GTK_INVALID_LIST_POSITION)
        selected_row = g_list_model_get_item (G_LIST_MODEL (td->template_store),
                                              selected_position);
    g_list_store_sort (td->template_store, gnc_ab_trans_dialog_template_compare, NULL);
    if (selected_row)
    {
        for (guint position = 0;
             position < g_list_model_get_n_items (G_LIST_MODEL (td->template_store));
             position++)
        {
            GtkStringObject *row = g_list_model_get_item (G_LIST_MODEL (td->template_store),
                                                           position);
            gboolean is_selected = row == selected_row;
            g_object_unref (row);
            if (is_selected)
            {
                gtk_single_selection_set_selected (td->template_selection, position);
                break;
            }
        }
        g_object_unref (selected_row);
    }
    td->templ_changed = TRUE;
    LEAVE(" ");
}

static void
template_delete_request_free (TemplateDeleteRequest *request)
{
    if (!request)
        return;

    g_clear_object (&request->row);
    g_free (request);
}

static void
template_delete_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    TemplateDeleteRequest *request = user_data;
    GncABTransDialog *td = request->dialog;

    (void)parent;
    if (td && td->template_delete_request == request)
        td->template_delete_request = NULL;

    if (response == GTK_RESPONSE_YES && td)
    {
        guint n_items = g_list_model_get_n_items (G_LIST_MODEL (td->template_store));

        for (guint position = 0; position < n_items; position++)
        {
            GtkStringObject *candidate = g_list_model_get_item (
                G_LIST_MODEL (td->template_store), position);
            gboolean is_target = candidate == request->row;

            g_object_unref (candidate);
            if (!is_target)
                continue;

            g_list_store_remove (td->template_store, position);
            td->templ_changed = TRUE;
            DEBUG ("Deleted template with name %s",
                   gtk_string_object_get_string (request->row));
            break;
        }
    }

    template_delete_request_free (request);
}

void
gnc_ab_trans_dialog_del_templ_cb(GtkButton *button, gpointer user_data)
{
    GncABTransDialog *td = user_data;
    guint position;
    GtkStringObject *row;
    TemplateDeleteRequest *request;

    (void)button;
    g_return_if_fail (td);
    if (td->template_delete_request)
        return;

    ENTER ("td=%p", td);
    position = gtk_single_selection_get_selected (td->template_selection);
    if (position == GTK_INVALID_LIST_POSITION)
    {
        LEAVE ("None selected");
        return;
    }

    row = g_list_model_get_item (G_LIST_MODEL (td->template_store), position);
    if (!row)
    {
        LEAVE ("Could not get selected template");
        return;
    }

    request = g_new0 (TemplateDeleteRequest, 1);
    request->dialog = td;
    request->row = row;
    td->template_delete_request = request;
    gnc_verify_dialog_async (
        GTK_WINDOW (td->dialog), FALSE, template_delete_finished, request,
        _("Do you really want to delete the template with the name \"%s\"?"),
        gtk_string_object_get_string (row));
    LEAVE (" ");
}

#endif

void
gnc_ab_trans_dialog_ibanentry_filter_cb (GtkEditable *editable,
                                     const gchar *text,
                                     gint         length,
                                     gint        *position,
                                     gpointer     data)
{
    GString* result = g_string_new(NULL);
    gint i;
    GncABTransDialog *td = data;

    if (length == -1)
        length = strlen(text);
    g_assert(position);

    /* Filter digits / non digits as needed */
    for (i = 0; i < length; i++)
    {
        gchar c = text[i];

        if (gnc_ab_trans_isSEPA(td->trans_type))
        {
            enum {
                ALPHA
                , ALNUM
                , NUMERIC
            } allowed_characterclass;

            // SEPA: Only alphas in the first two places (at index 0, 1)
            if (*position + i < 2)
            {
                allowed_characterclass = ALPHA;
            }
            // SEPA: Next two places are digits only (index 2, 3)
            else if (*position + i < 4)
            {
                allowed_characterclass = NUMERIC;
            }
            // SEPA: The rest depends on the country code: Either Alpha-numeric or numeric only
            else
            {
                const gchar* acct_text = gnc_entry_get_text(GTK_ENTRY(td->recp_account_entry));
                // Special case for German ("DE") IBAN: Numeric only. Otherwise allow alpha-numeric
                if (acct_text[0] == 'D' && acct_text[1] == 'E')
                {
                    allowed_characterclass = NUMERIC;
                }
                else
                {
                    allowed_characterclass = ALNUM;
                }
            }

            // Do the actual character class check. Alphas are only allowed in
            // uppercase, though.
            switch (allowed_characterclass)
            {
            case ALPHA:
                if (g_ascii_isalpha(c))
                    g_string_append_c(result, g_ascii_toupper(c));
                break;
            case ALNUM:
                if (g_ascii_isalnum(c))
                    g_string_append_c(result, g_ascii_toupper(c));
                break;
            case NUMERIC:
                if (g_ascii_isdigit(c))
                    g_string_append_c(result, c);
                break;
            }
        }
        else
        {
            // Non-SEPA: Only accept digits.
            if (g_ascii_isdigit(c))
            {
                g_string_append_c(result, c);
            }
        }
    }

    g_signal_handlers_block_by_func (editable,
                                     (gpointer) gnc_ab_trans_dialog_ibanentry_filter_cb, data);
    gtk_editable_insert_text (editable, result->str, result->len, position);
    g_signal_handlers_unblock_by_func (editable,
                                       (gpointer) gnc_ab_trans_dialog_ibanentry_filter_cb, data);
    g_signal_stop_emission_by_name (editable, "insert_text");
    g_string_free (result, TRUE);
}

void
gnc_ab_trans_dialog_bicentry_filter_cb (GtkEditable *editable,
                                     const gchar *text,
                                     gint         length,
                                     gint        *position,
                                     gpointer     data)
{
    GString* result = g_string_new(NULL);
    gint i;
    GncABTransDialog *td = data;

    if (length == -1)
        length = strlen(text);
    g_assert(position);

    /* Filter non digits */
    for (i = 0; i < length; i++)
    {
        gchar c = text[i];

        if (gnc_ab_trans_isSEPA(td->trans_type))
        {
            // SEPA: Only alphas in the first 6 places (only upper case, though), then both upper-case alphas and digits
            if (*position + i < 6)
            {
                if (g_ascii_isalpha(c))
                    g_string_append_c(result, g_ascii_toupper(c));
            }
            else
            {
                if (g_ascii_isalnum(c))
                    g_string_append_c(result, g_ascii_toupper(c));
            }
        }
        else
        {
            // Non-SEPA: Only digits accepted.
            if (g_ascii_isdigit(c))
            {
                g_string_append_c(result, c);
            }
        }
    }

    g_signal_handlers_block_by_func (editable,
                                     (gpointer) gnc_ab_trans_dialog_bicentry_filter_cb, data);
    gtk_editable_insert_text (editable, result->str, result->len, position);
    g_signal_handlers_unblock_by_func (editable,
                                       (gpointer) gnc_ab_trans_dialog_bicentry_filter_cb, data);
    g_signal_stop_emission_by_name (editable, "insert_text");
    g_string_free (result, TRUE);
}
