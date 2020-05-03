/*
 * dialog-employee.c -- Dialog for Employee entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-currency-edit.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "dialog-search.h"
#include "search-param.h"
#include "gnc-account-sel.h"

#include "gncAddress.h"
#include "gncEmployee.h"
#include "gncEmployeeP.h"
#include "gncOwner.h"

#include "dialog-employee.h"
#include "dialog-invoice.h"
#include "dialog-payment.h"
#include "business-gnome-utils.h"

#define DIALOG_NEW_EMPLOYEE_CM_CLASS "dialog-new-employee"
#define DIALOG_EDIT_EMPLOYEE_CM_CLASS "dialog-edit-employee"

#define GNC_PREFS_GROUP_SEARCH "dialogs.business.employee-search"

void gnc_employee_window_ok_cb (GtkWidget *widget, gpointer data);
void gnc_employee_window_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_employee_window_help_cb (GtkWidget *widget, gpointer data);
void gnc_employee_window_destroy_cb (GtkWidget *widget, gpointer data);
void gnc_employee_name_changed_cb (GtkWidget *widget, gpointer data);
void gnc_employee_ccard_acct_toggled_cb (GtkToggleButton *button, gpointer data);

typedef enum
{
    NEW_EMPLOYEE,
    EDIT_EMPLOYEE
} EmployeeDialogType;

struct _employee_select_window
{
    QofBook  *book;
    QofQuery *q;
};

struct _employee_window
{
    GtkWidget *	dialog;

    GtkWidget *	id_entry;
    GtkWidget *	username_entry;

    GtkWidget *	name_entry;
    GtkWidget *	addr1_entry;
    GtkWidget *	addr2_entry;
    GtkWidget *	addr3_entry;
    GtkWidget *	addr4_entry;
    GtkWidget *	phone_entry;
    GtkWidget *	fax_entry;
    GtkWidget *	email_entry;

    GtkWidget *	language_entry;

    GtkWidget *	workday_amount;
    GtkWidget *	rate_amount;
    GtkWidget *	currency_edit;
    GtkWidget *	ccard_acct_check;
    GtkWidget *	ccard_acct_sel;

    GtkWidget *	active_check;

    /* ACL? */

    EmployeeDialogType	dialog_type;
    GncGUID		employee_guid;
    gint		component_id;
    QofBook *	book;
    GncEmployee *	created_employee;
};

static GncEmployee *
ew_get_employee (EmployeeWindow *ew)
{
    if (!ew)
        return NULL;

    return gncEmployeeLookup (ew->book, &ew->employee_guid);
}

static void gnc_ui_to_employee (EmployeeWindow *ew, GncEmployee *employee)
{
    GncAddress *addr;

    addr = gncEmployeeGetAddr (employee);

    gnc_suspend_gui_refresh ();

    gncEmployeeBeginEdit (employee);

    if (ew->dialog_type == NEW_EMPLOYEE)
        qof_event_gen(QOF_INSTANCE(employee), QOF_EVENT_ADD, NULL);

    gncEmployeeSetID (employee, gtk_editable_get_chars
                      (GTK_EDITABLE (ew->id_entry), 0, -1));
    gncEmployeeSetUsername (employee, gtk_editable_get_chars
                            (GTK_EDITABLE (ew->username_entry), 0, -1));

    gncAddressSetName (addr, gtk_editable_get_chars
                       (GTK_EDITABLE (ew->name_entry), 0, -1));
    gncAddressSetAddr1 (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (ew->addr1_entry), 0, -1));
    gncAddressSetAddr2 (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (ew->addr2_entry), 0, -1));
    gncAddressSetAddr3 (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (ew->addr3_entry), 0, -1));
    gncAddressSetAddr4 (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (ew->addr4_entry), 0, -1));
    gncAddressSetPhone (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (ew->phone_entry), 0, -1));
    gncAddressSetFax (addr, gtk_editable_get_chars
                      (GTK_EDITABLE (ew->fax_entry), 0, -1));
    gncAddressSetEmail (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (ew->email_entry), 0, -1));

    gncEmployeeSetActive (employee, gtk_toggle_button_get_active
                          (GTK_TOGGLE_BUTTON (ew->active_check)));
    gncEmployeeSetLanguage (employee, gtk_editable_get_chars
                            (GTK_EDITABLE (ew->language_entry), 0, -1));

    /* Parse and set the workday and rate amounts */
    gncEmployeeSetWorkday (employee, gnc_amount_edit_get_amount
                           (GNC_AMOUNT_EDIT (ew->workday_amount)));
    gncEmployeeSetRate (employee, gnc_amount_edit_get_amount
                        (GNC_AMOUNT_EDIT (ew->rate_amount)));
    gncEmployeeSetCurrency (employee, gnc_currency_edit_get_currency
                            (GNC_CURRENCY_EDIT (ew->currency_edit)));

    /* Fill in the CCard Acct */
    gncEmployeeSetCCard (employee,
                         (gtk_toggle_button_get_active
                          (GTK_TOGGLE_BUTTON (ew->ccard_acct_check)) ?
                          gnc_account_sel_get_account
                          (GNC_ACCOUNT_SEL (ew->ccard_acct_sel)) : NULL));

    gncEmployeeCommitEdit (employee);
    gnc_resume_gui_refresh ();
}

static gboolean check_entry_nonempty (GtkWidget *entry,
                                      const char * error_message)
{
    const char *res = gtk_entry_get_text (GTK_ENTRY (entry));
    if (g_strcmp0 (res, "") == 0)
    {
        if (error_message)
            gnc_error_dialog (gnc_ui_get_gtk_window(entry), "%s", error_message);
        return TRUE;
    }
    return FALSE;
}

void
gnc_employee_window_ok_cb (GtkWidget *widget, gpointer data)
{
    EmployeeWindow *ew = data;
    gchar *string;

    /* Check for valid username */
    if (check_entry_nonempty (ew->username_entry,
                              _("You must enter a username.")))
        return;

    /* Check for valid username */
    if (check_entry_nonempty (ew->name_entry,
                              _("You must enter the employee's name.")))
        return;

    /* Make sure we have an address */
    if (check_entry_nonempty (ew->addr1_entry, NULL) &&
            check_entry_nonempty (ew->addr2_entry, NULL) &&
            check_entry_nonempty (ew->addr3_entry, NULL) &&
            check_entry_nonempty (ew->addr4_entry, NULL))
    {
        const char *msg = _("You must enter an address.");
        gnc_error_dialog (gnc_ui_get_gtk_window (widget), "%s", msg);
        return;
    }

    /* Set the employee id if one has not been chosen */
    if (g_strcmp0 (gtk_entry_get_text (GTK_ENTRY (ew->id_entry)), "") == 0)
    {
        string = gncEmployeeNextID (ew->book);
        gtk_entry_set_text (GTK_ENTRY (ew->id_entry), string);
        g_free(string);
    }

    /* Now save it off */
    {
        GncEmployee *employee = ew_get_employee (ew);
        if (employee)
        {
            gnc_ui_to_employee (ew, employee);
        }
        ew->created_employee = employee;
        ew->employee_guid = *guid_null ();
    }

    gnc_close_gui_component (ew->component_id);
}

void
gnc_employee_window_cancel_cb (GtkWidget *widget, gpointer data)
{
    EmployeeWindow *ew = data;

    gnc_close_gui_component (ew->component_id);
}

void
gnc_employee_window_help_cb (GtkWidget *widget, gpointer data)
{
    gnc_gnome_help(HF_HELP, HL_USAGE_EMPLOYEE);
}

void
gnc_employee_window_destroy_cb (GtkWidget *widget, gpointer data)
{
    EmployeeWindow *ew = data;
    GncEmployee *employee = ew_get_employee (ew);

    gnc_suspend_gui_refresh ();

    if (ew->dialog_type == NEW_EMPLOYEE && employee != NULL)
    {
        gncEmployeeBeginEdit (employee);
        gncEmployeeDestroy (employee);
        ew->employee_guid = *guid_null ();
    }

    gnc_unregister_gui_component (ew->component_id);
    gnc_resume_gui_refresh ();

    g_free (ew);
}

void
gnc_employee_name_changed_cb (GtkWidget *widget, gpointer data)
{
    EmployeeWindow *ew = data;
    char *fullname, *title;
    const char *name, *id;

    if (!ew)
        return;

    name = gtk_entry_get_text (GTK_ENTRY (ew->name_entry));
    if (!name || *name == '\0')
        name = _("<No name>");

    id = gtk_entry_get_text (GTK_ENTRY (ew->id_entry));

    fullname = g_strconcat (name, " (", id, ")", (char *)NULL);

    if (ew->dialog_type == EDIT_EMPLOYEE)
        title = g_strconcat (_("Edit Employee"), " - ", fullname, (char *)NULL);
    else
        title = g_strconcat (_("New Employee"), " - ", fullname, (char *)NULL);

    gtk_window_set_title (GTK_WINDOW (ew->dialog), title);

    g_free (fullname);
    g_free (title);
}

void
gnc_employee_ccard_acct_toggled_cb (GtkToggleButton *button, gpointer data)
{
    EmployeeWindow *ew = data;

    if (!ew)
        return;

    if (gtk_toggle_button_get_active (button))
    {
        gtk_widget_set_sensitive (ew->ccard_acct_sel, TRUE);
        gtk_widget_show (ew->ccard_acct_sel);
    }
    else
    {
        gtk_widget_set_sensitive (ew->ccard_acct_sel, TRUE);
        gtk_widget_hide (ew->ccard_acct_sel);
    }
}

static void
gnc_employee_window_close_handler (gpointer user_data)
{
    EmployeeWindow *ew = user_data;

    gtk_widget_destroy (ew->dialog);
}

static void
gnc_employee_window_refresh_handler (GHashTable *changes, gpointer user_data)
{
    EmployeeWindow *ew = user_data;
    const EventInfo *info;
    GncEmployee *employee = ew_get_employee (ew);

    /* If there isn't a employee behind us, close down */
    if (!employee)
    {
        gnc_close_gui_component (ew->component_id);
        return;
    }

    /* Next, close if this is a destroy event */
    if (changes)
    {
        info = gnc_gui_get_entity_events (changes, &ew->employee_guid);
        if (info && (info->event_mask & QOF_EVENT_DESTROY))
        {
            gnc_close_gui_component (ew->component_id);
            return;
        }
    }
}

static gboolean
find_handler (gpointer find_data, gpointer user_data)
{
    const GncGUID *employee_guid = find_data;
    EmployeeWindow *ew = user_data;

    return(ew && guid_equal(&ew->employee_guid, employee_guid));
}

static EmployeeWindow *
gnc_employee_new_window (GtkWindow *parent,
                         QofBook *bookp,
                         GncEmployee *employee)
{
    EmployeeWindow *ew;
    GtkBuilder *builder;
    GtkWidget *hbox, *edit;
    gnc_commodity *currency;
    GNCPrintAmountInfo print_info;
    GList *acct_types;
    Account *ccard_acct;

    /*
     * Find an existing window for this employee.  If found, bring it to
     * the front.
     */
    if (employee)
    {
        GncGUID employee_guid;

        employee_guid = *gncEmployeeGetGUID (employee);
        ew = gnc_find_first_gui_component (DIALOG_EDIT_EMPLOYEE_CM_CLASS,
                                           find_handler, &employee_guid);
        if (ew)
        {
            gtk_window_set_transient_for (GTK_WINDOW(ew->dialog), parent);
            gtk_window_present (GTK_WINDOW(ew->dialog));
            return(ew);
        }
    }

    /* Find the default currency */
    if (employee)
        currency = gncEmployeeGetCurrency (employee);
    else
        currency = gnc_default_currency ();

    /*
     * No existing employee window found.  Build a new one.
     */
    ew = g_new0 (EmployeeWindow, 1);

    ew->book = bookp;

    /* Find the dialog */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-employee.glade", "employee_dialog");
    ew->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "employee_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW(ew->dialog), parent);

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(ew->dialog), "GncEmployeeDialog");

    g_object_set_data (G_OBJECT (ew->dialog), "dialog_info", ew);

    /* Get entry points */
    ew->id_entry = GTK_WIDGET(gtk_builder_get_object (builder, "id_entry"));
    ew->username_entry = GTK_WIDGET(gtk_builder_get_object (builder, "username_entry"));

    ew->name_entry = GTK_WIDGET(gtk_builder_get_object (builder, "name_entry"));
    ew->addr1_entry = GTK_WIDGET(gtk_builder_get_object (builder, "addr1_entry"));
    ew->addr2_entry = GTK_WIDGET(gtk_builder_get_object (builder, "addr2_entry"));
    ew->addr3_entry = GTK_WIDGET(gtk_builder_get_object (builder, "addr3_entry"));
    ew->addr4_entry = GTK_WIDGET(gtk_builder_get_object (builder, "addr4_entry"));
    ew->phone_entry = GTK_WIDGET(gtk_builder_get_object (builder, "phone_entry"));
    ew->fax_entry = GTK_WIDGET(gtk_builder_get_object (builder, "fax_entry"));
    ew->email_entry = GTK_WIDGET(gtk_builder_get_object (builder, "email_entry"));

    ew->language_entry = GTK_WIDGET(gtk_builder_get_object (builder, "language_entry"));
    ew->active_check = GTK_WIDGET(gtk_builder_get_object (builder, "active_check"));

    /* Currency */
    edit = gnc_currency_edit_new();
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(edit), currency);
    ew->currency_edit = edit;

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "currency_box"));
    gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

    /* WORKDAY: Value */
    edit = gnc_amount_edit_new();
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
    print_info = gnc_integral_print_info ();
    print_info.max_decimal_places = 5;
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit), 100000);
    ew->workday_amount = edit;
    gtk_widget_show (edit);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "hours_hbox"));
    gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

    /* RATE: Monetary Value */
    edit = gnc_amount_edit_new();
    print_info = gnc_commodity_print_info (currency, FALSE);
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit),
                                  gnc_commodity_get_fraction (currency));
    ew->rate_amount = edit;
    gtk_widget_show (edit);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "rate_hbox"));
    gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

    /* CCard Account Selection */
    ew->ccard_acct_check = GTK_WIDGET(gtk_builder_get_object (builder, "ccard_check"));

    edit = gnc_account_sel_new();
    acct_types = g_list_prepend(NULL, (gpointer)ACCT_TYPE_CREDIT);
    gnc_account_sel_set_acct_filters (GNC_ACCOUNT_SEL(edit), acct_types, NULL);
    g_list_free (acct_types);

    ew->ccard_acct_sel = edit;
    gtk_widget_show (edit);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "ccard_acct_hbox"));
    gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, ew);

    /* Setup initial values */
    if (employee != NULL)
    {
        GncAddress *addr;

        ew->dialog_type = EDIT_EMPLOYEE;
        ew->employee_guid = *gncEmployeeGetGUID (employee);

        addr = gncEmployeeGetAddr (employee);

        gtk_entry_set_text (GTK_ENTRY (ew->id_entry), gncEmployeeGetID (employee));
        gtk_entry_set_text (GTK_ENTRY (ew->username_entry), gncEmployeeGetUsername (employee));

        /* Setup Address */
        gtk_entry_set_text (GTK_ENTRY (ew->name_entry), gncAddressGetName (addr));
        gtk_entry_set_text (GTK_ENTRY (ew->addr1_entry), gncAddressGetAddr1 (addr));
        gtk_entry_set_text (GTK_ENTRY (ew->addr2_entry), gncAddressGetAddr2 (addr));
        gtk_entry_set_text (GTK_ENTRY (ew->addr3_entry), gncAddressGetAddr3 (addr));
        gtk_entry_set_text (GTK_ENTRY (ew->addr4_entry), gncAddressGetAddr4 (addr));
        gtk_entry_set_text (GTK_ENTRY (ew->phone_entry), gncAddressGetPhone (addr));
        gtk_entry_set_text (GTK_ENTRY (ew->fax_entry), gncAddressGetFax (addr));
        gtk_entry_set_text (GTK_ENTRY (ew->email_entry), gncAddressGetEmail (addr));

        gtk_entry_set_text (GTK_ENTRY (ew->language_entry),
                            gncEmployeeGetLanguage (employee));

        /* Set toggle buttons */
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ew->active_check),
                                      gncEmployeeGetActive (employee));

        ew->component_id =
            gnc_register_gui_component (DIALOG_EDIT_EMPLOYEE_CM_CLASS,
                                        gnc_employee_window_refresh_handler,
                                        gnc_employee_window_close_handler,
                                        ew);
    }
    else
    {
        employee = gncEmployeeCreate (bookp);
        ew->employee_guid = *gncEmployeeGetGUID (employee);

        ew->dialog_type = NEW_EMPLOYEE;
        ew->component_id =
            gnc_register_gui_component (DIALOG_NEW_EMPLOYEE_CM_CLASS,
                                        gnc_employee_window_refresh_handler,
                                        gnc_employee_window_close_handler,
                                        ew);
    }


    /* I know that employee exists here -- either passed in or just created */
    /* Set the workday and rate values */
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (ew->workday_amount),
                                gncEmployeeGetWorkday (employee));
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (ew->rate_amount),
                                gncEmployeeGetRate (employee));


    ccard_acct = gncEmployeeGetCCard (employee);
    if (ccard_acct == NULL)
    {
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ew->ccard_acct_check), FALSE);
        gtk_widget_set_sensitive (ew->ccard_acct_sel, FALSE);
    }
    else
    {
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ew->ccard_acct_check), TRUE);
        gnc_account_sel_set_account (GNC_ACCOUNT_SEL (ew->ccard_acct_sel), ccard_acct, FALSE);
    }

    /* XXX: Set the ACL */

    gnc_gui_component_watch_entity_type (ew->component_id,
                                         GNC_EMPLOYEE_MODULE_NAME,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    gtk_widget_show_all (ew->dialog);

    if (ccard_acct == NULL)
        gtk_widget_hide (ew->ccard_acct_sel);

    g_object_unref(G_OBJECT(builder));

    return ew;
}

EmployeeWindow *
gnc_ui_employee_new (GtkWindow *parent, QofBook *bookp)
{
    EmployeeWindow *ew;

    /* Make sure required options exist */
    if (!bookp) return NULL;

    ew = gnc_employee_new_window (parent, bookp, NULL);

    return ew;
}

EmployeeWindow *
gnc_ui_employee_edit (GtkWindow *parent, GncEmployee *employee)
{
    EmployeeWindow *ew;

    if (!employee) return NULL;

    ew = gnc_employee_new_window (parent, gncEmployeeGetBook(employee), employee);

    return ew;
}

/* Functions for employee selection widgets */

static void
invoice_employee_cb (GtkWindow *dialog, gpointer *employee_p, gpointer user_data)
{
    struct _employee_select_window *sw = user_data;
    GncOwner owner;
    GncEmployee *employee;

    g_return_if_fail (employee_p && user_data);

    employee = *employee_p;

    if (!employee)
        return;

    gncOwnerInitEmployee (&owner, employee);
    gnc_invoice_search (dialog, NULL, &owner, sw->book);
    return;
}

static void
payment_employee_cb (GtkWindow *dialog, gpointer *employee_p, gpointer user_data)
{
    struct _employee_select_window *sw = user_data;
    GncOwner owner;
    GncEmployee *employee;

    g_return_if_fail (employee_p && user_data);

    employee = *employee_p;

    if (!employee)
        return;

    gncOwnerInitEmployee (&owner, employee);
    gnc_ui_payment_new (dialog, &owner, sw->book);
    return;
}

static void
edit_employee_cb (GtkWindow *dialog, gpointer *employee_p, gpointer user_data)
{
    GncEmployee *employee;

    g_return_if_fail (employee_p && user_data);

    employee = *employee_p;

    if (!employee)
        return;

    gnc_ui_employee_edit (dialog, employee);
    return;
}

static gpointer
new_employee_cb (GtkWindow *dialog, gpointer user_data)
{
    struct _employee_select_window *sw = user_data;
    EmployeeWindow *ew;

    g_return_val_if_fail (user_data, NULL);

    ew = gnc_ui_employee_new (dialog, sw->book);
    return ew_get_employee (ew);
}

static void
free_employee_cb (gpointer user_data)
{
    struct _employee_select_window *sw = user_data;

    g_return_if_fail (sw);

    qof_query_destroy (sw->q);
    g_free (sw);
}

GNCSearchWindow *
gnc_employee_search (GtkWindow *parent, GncEmployee *start, QofBook *book)
{
    QofIdType type = GNC_EMPLOYEE_MODULE_NAME;
    struct _employee_select_window *sw;
    QofQuery *q, *q2 = NULL;
    static GList *params = NULL;
    static GList *columns = NULL;
    static GNCSearchCallbackButton buttons[] =
    {
        { N_("View/Edit Employee"), edit_employee_cb, NULL, TRUE},
        { N_("Expense Vouchers"), invoice_employee_cb, NULL, TRUE},
        { N_("Process Payment"), payment_employee_cb, NULL, FALSE},
        { NULL },
    };

    g_return_val_if_fail (book, NULL);

    /* Build parameter list in reverse order */
    if (params == NULL)
    {
        params = gnc_search_param_prepend (params, _("Employee ID"), NULL, type,
                                           EMPLOYEE_ID, NULL);
        params = gnc_search_param_prepend (params, _("Employee Username"), NULL,
                                           type, EMPLOYEE_USERNAME, NULL);
        params = gnc_search_param_prepend (params, _("Employee Name"), NULL,
                                           type, EMPLOYEE_ADDR, ADDRESS_NAME, NULL);
    }

    /* Build the column list in reverse order */
    if (columns == NULL)
    {
        columns = gnc_search_param_prepend (columns, _("Username"), NULL, type,
                                            EMPLOYEE_USERNAME, NULL);
        columns = gnc_search_param_prepend (columns, _("ID #"), NULL, type,
                                            EMPLOYEE_ID, NULL);
        columns = gnc_search_param_prepend (columns, _("Name"), NULL, type,
                                            EMPLOYEE_ADDR, ADDRESS_NAME, NULL);
    }

    /* Build the queries */
    q = qof_query_create_for (type);
    qof_query_set_book (q, book);

#if 0
    if (start)
    {
        q2 = qof_query_copy (q);
        qof_query_add_guid_match (q2, g_slist_prepend (NULL, QOF_PARAM_GUID),
                                  gncEmployeeGetGUID (start), QOF_QUERY_AND);
    }
#endif

    /* launch select dialog and return the result */
    sw = g_new0 (struct _employee_select_window, 1);
    sw->book = book;
    sw->q = q;

    return gnc_search_dialog_create (parent, type, _("Find Employee"),
                                     params, columns, q, q2,
                                     buttons, NULL, new_employee_cb,
                                     sw, free_employee_cb,
                                     GNC_PREFS_GROUP_SEARCH, NULL,
                                     "GncFindEmployeeDialog");
}

GNCSearchWindow *
gnc_employee_search_select (GtkWindow *parent, gpointer start, gpointer book)
{
    if (!book) return NULL;

    return gnc_employee_search (parent, start, book);
}

GNCSearchWindow *
gnc_employee_search_edit (GtkWindow *parent, gpointer start, gpointer book)
{
    if (start)
        gnc_ui_employee_edit (parent, start);

    return NULL;
}
