/*
 * dialog-filter-invoices.cpp -- Filter dialog for invoices 
 * Copyright (C) 2026 Roy Hansen 
 * Author: Roy Hansen <roy@royhansen.no>
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
 * along with this program; if not, see
 * <https://www.gnu.org/licenses/>.
 */

#include <memory>
#include <string>
#include <gtk/gtk.h>
#include "gnc-plugin-page.h"
#include <gnc-date-edit.h>
#include "gncInvoice.h"
#include "gnc-ui-util.h"
#include "dialog-utils.h"
#include "dialog-filter-invoices.h"

static const char * const UI_FILE = "dialog-filter-invoices.glade";
static QofLogModule log_module = GNC_MOD_GUI;

/************************************************************/
/*       GncFilterInvoicesDialogInternal Class              */
/************************************************************/

enum Preset {
    CUSTOM,
    OVERDUE
};

class GncFilterInvoicesDialogInternal {
    GncOwnerType owner_type;
    GncPluginPage *plugin_page = nullptr;
    GtkWidget *dialog = nullptr;

    // State
    Preset        preset = Preset::CUSTOM;
    int           overdue_days = 0;
    bool          show_paid = true;
    bool          show_unpaid = true;
    bool          show_posted = true;
    bool          show_unposted = true;
    bool          show_invoices = true;
    bool          show_creditnotes = true;
    bool          use_start_date = false;
    bool          use_end_date = false;
    time64        start_date = 0;
    time64        end_date = 0;
    std::string   search_term;
    // State end

    GtkToggleButton *custom_toggle = nullptr;
    GtkToggleButton *overdue_toggle = nullptr;
    GtkWidget       *custom_controls = nullptr;
    GtkWidget       *overdue_controls = nullptr;

    GtkToggleButton *overdue_days_zero = nullptr;
    GtkToggleButton *overdue_days_seven = nullptr;
    GtkToggleButton *overdue_days_fourteen = nullptr;
    GtkToggleButton *overdue_days_thirty = nullptr;
    GtkToggleButton *overdue_days_sixty = nullptr;
    GtkToggleButton *overdue_days_ninety = nullptr;

    GtkToggleButton *paid_toggle = nullptr;
    GtkToggleButton *unpaid_toggle = nullptr;
    GtkToggleButton *posted_toggle = nullptr;
    GtkToggleButton *unposted_toggle = nullptr;
    GtkToggleButton *invoices_toggle = nullptr;
    GtkToggleButton *creditnotes_toggle = nullptr;
    GtkToggleButton *start_toggle = nullptr;
    GtkToggleButton *end_toggle = nullptr;
    GNCDateEdit     *start_picker = nullptr;
    GNCDateEdit     *end_picker = nullptr;
    GtkEntry        *search_entry = nullptr;

public:
    std::function<void()> apply_filter = [](){
        g_warn_if_fail (false);
    };

    GncFilterInvoicesDialogInternal (GncOwnerType &owner, GncPluginPage &page,
                                     std::function<void()> apply_cb) :
                                    owner_type(owner), plugin_page(&page)
    {
        if (apply_cb)
            apply_filter = apply_cb;
        else
            g_warn_if_fail (false);
    }
 
    ~GncFilterInvoicesDialogInternal ()
    {
        if (dialog)
            gtk_widget_destroy (GTK_WIDGET (dialog));
    }

    void dialog_open();

    QofQuery *create_qof_query ();

    void dialog_close ();

    void set_preset (Preset p);

    void set_overdue_days (GtkToggleButton *button);

    void set_paid (bool status);

    void set_unpaid (bool status);

    void set_posted (bool status);

    void set_unposted (bool status);

    void set_invoices (bool status);

    void set_creditnotes (bool status);

    void set_dates (bool init);

    void set_use_start_date (bool status);

    void set_use_end_date (bool status);

    void apply_filter_if_start_or_end_date ()
    {
        if (use_start_date || use_end_date)
            apply_filter ();
    }

    void set_search_term ();

    void set_search_term (const char *term);

    bool reset ();

private:
    void resize_window ();

    void reset_dates ();
};


/************************************************************/
/*                     Command callbacks                    */
/************************************************************/

static void
dialog_destroy (GtkWidget *w, gpointer user_data)
{
    static_cast<GncFilterInvoicesDialogInternal *> (user_data)->dialog_close();
}

static gboolean
action_custom_toggle (GtkToggleButton *button, gpointer user_data)
{
    if (!gtk_toggle_button_get_active(button))
        return false;

    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_preset (Preset::CUSTOM);
    filter->apply_filter ();

    return false;
}

static gboolean
action_overdue_toggle (GtkToggleButton *button, gpointer user_data)
{
    if (!gtk_toggle_button_get_active(button))
        return false;

    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_preset (Preset::OVERDUE);
    filter->apply_filter ();

    return false;
}

static gboolean
action_overdue_days_toggle (GtkToggleButton *button, gpointer user_data)
{
    if (!gtk_toggle_button_get_active(button))
        return false;

    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_overdue_days (button);
    filter->apply_filter ();

    return false;
}

static gboolean
action_paid_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_paid (state);
    filter->apply_filter ();

    return false;
}

static gboolean
action_unpaid_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_unpaid (state);
    filter->apply_filter ();

    return false;
}

static gboolean
action_posted_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_posted (state);
    filter->apply_filter ();

    return false;
}

static gboolean
action_unposted_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_unposted (state);
    filter->apply_filter ();

    return false;
}

static gboolean
action_invoices_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_invoices (state);
    filter->apply_filter ();

    return false;
}

static gboolean
action_creditnotes_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_creditnotes (state);
    filter->apply_filter ();

    return false;
}

static void
action_start_picker_activate (GtkWidget *date, gpointer user_data)
{
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_dates (false);
    filter->apply_filter ();
}

static gboolean
action_start_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_use_start_date (state);
    filter->apply_filter ();

    return false;
}

static void
action_end_picker_activate (GtkWidget *date, gpointer user_data)
{
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_dates (false);
    filter->apply_filter ();
}

static gboolean
action_end_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_use_end_date (state);
    filter->apply_filter ();

    return false;
}

static void 
action_apply_dates (GtkToggleButton *button, gpointer user_data)
{
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_dates (false);
    filter->apply_filter_if_start_or_end_date ();
}

static void
action_search_entry_activate (GtkEditable *editable, gpointer user_data)
{
    const gchar *text = gtk_entry_get_text(GTK_ENTRY(editable));
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_search_term (text);
    filter->apply_filter ();
}

static void
action_apply_search_entry (GtkButton *button, gpointer user_data)
{
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_search_term ();
    filter->apply_filter ();
}

static void
action_reset (GtkButton *button, gpointer user_data)
{
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);
    if (filter->reset ())
        filter->apply_filter ();
}

static void
action_close (GtkButton *button, gpointer user_data)
{
    gtk_widget_destroy (GTK_WIDGET (user_data));
}


/************************************************************/
/*                     Implementation                       */
/************************************************************/

void
GncFilterInvoicesDialogInternal::dialog_close ()
{
    dialog = nullptr;

    custom_toggle = nullptr;
    overdue_toggle = nullptr;
    custom_controls = nullptr;
    overdue_controls = nullptr;

    overdue_days_zero = nullptr;
    overdue_days_seven = nullptr;
    overdue_days_fourteen = nullptr;
    overdue_days_thirty = nullptr;
    overdue_days_sixty = nullptr;
    overdue_days_ninety = nullptr;

    paid_toggle = nullptr;
    unpaid_toggle = nullptr;
    posted_toggle = nullptr;
    unposted_toggle = nullptr;
    invoices_toggle = nullptr;
    creditnotes_toggle = nullptr;
    start_toggle = nullptr;
    end_toggle = nullptr;
    start_picker = nullptr;
    end_picker = nullptr;
    search_entry = nullptr;
}

bool 
GncFilterInvoicesDialogInternal::reset ()
{
    if (preset == Preset::CUSTOM
        && show_paid && show_unpaid
        && show_posted && show_unposted
        && show_invoices && show_creditnotes
        && search_term == ""
        && !use_start_date && !use_end_date)
    {
        return false;
    }
    else if (preset == Preset::OVERDUE
             && overdue_days == 0
             && search_term == "")
    {
        return false;
    }

    if (preset == Preset::CUSTOM)
    {
        set_paid (true);
        set_unpaid (true);
        set_posted (true);
        set_unposted (true);
        set_invoices (true);
        set_creditnotes (true);
        set_use_start_date (false);
        set_use_end_date (false);
        reset_dates ();
    }
    else if (preset == Preset::OVERDUE)
    {
        set_overdue_days (overdue_days_zero);
    }

    set_search_term ("");

    g_warn_if_fail (dialog);

    return true;
}

void
GncFilterInvoicesDialogInternal::resize_window ()
{
    g_return_if_fail (dialog);

    GtkRequisition requisition;
    gtk_widget_get_preferred_size (GTK_WIDGET(dialog),
                                   nullptr,
                                   &requisition);

    gtk_window_resize (GTK_WINDOW (dialog),
                       requisition.width,
                       requisition.height);
}

void
GncFilterInvoicesDialogInternal::set_preset (Preset p)
{
    preset = p;
    GtkToggleButton *toggle;

    switch (p)
    {
    case Preset::CUSTOM:
      toggle = custom_toggle;
      gtk_widget_hide (overdue_controls);
      gtk_widget_show (custom_controls);
      break;
    case Preset::OVERDUE:
        toggle = overdue_toggle;
        gtk_widget_hide (custom_controls);
        gtk_widget_show (overdue_controls);
        break;
    }

    if (toggle)
        gtk_toggle_button_set_active (toggle, true);
    else
        g_warn_if_fail (false);

    resize_window ();
}

void
GncFilterInvoicesDialogInternal::set_overdue_days (GtkToggleButton *button)
{
    int days = 0;

    g_return_if_fail (button);

    gtk_toggle_button_set_active (button, true);

    if (button == overdue_days_seven)
        days = 7;
    else if (button == overdue_days_fourteen)
        days = 14;
    else if (button == overdue_days_thirty)
        days = 30;
    else if (button == overdue_days_sixty)
        days = 60;
    else if (button == overdue_days_ninety)
        days = 90;

    overdue_days = days;
}

void
GncFilterInvoicesDialogInternal::set_paid (bool status)
{
    show_paid = status;

    if (paid_toggle)
        gtk_toggle_button_set_active(paid_toggle, show_paid);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_unpaid (bool status)
{
    show_unpaid = status;

    if (unpaid_toggle)
        gtk_toggle_button_set_active(unpaid_toggle, show_unpaid);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_posted (bool status)
{
    show_posted = status;

    if (posted_toggle)
        gtk_toggle_button_set_active(posted_toggle, show_posted);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_unposted (bool status)
{
    show_unposted = status;

    if (unposted_toggle)
        gtk_toggle_button_set_active(unposted_toggle, show_unposted);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_invoices (bool status)
{
    show_invoices = status;

    if (invoices_toggle)
        gtk_toggle_button_set_active(invoices_toggle, show_invoices);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_creditnotes (bool status)
{
    show_creditnotes = status;

    if (creditnotes_toggle)
        gtk_toggle_button_set_active(creditnotes_toggle, show_creditnotes);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_search_term (const char *term)
{
    search_term = term;

    if (search_entry)
        gtk_entry_set_text(search_entry, search_term.c_str());
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_search_term ()
{
    const gchar *text = gtk_entry_get_text(search_entry);

    set_search_term (text);
}

void
GncFilterInvoicesDialogInternal::set_use_start_date (bool status)
{
    use_start_date = status;

    if (start_toggle)
        gtk_toggle_button_set_active(start_toggle, use_start_date);
    else
        g_warn_if_fail(false);

    if (start_picker)
        gtk_widget_set_sensitive (GTK_WIDGET (start_picker), use_start_date);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_use_end_date (bool status)
{
    use_end_date = status;

    if (end_toggle)
        gtk_toggle_button_set_active(end_toggle, use_end_date);
    else
        g_warn_if_fail(false);

    if (end_picker)
        gtk_widget_set_sensitive (GTK_WIDGET (end_picker), use_end_date);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_dates (bool init)
{
    if (start_picker)
    {
        if (init && start_date != 0)
        {
            gnc_date_edit_set_time(start_picker, start_date);
        }
        else
        {
            start_date = gnc_date_edit_get_date(start_picker);
        }
    }
    else g_warn_if_fail(false);

    if (end_picker)
    {
        if (init && end_date != 0)
        {
            gnc_date_edit_set_time(end_picker, end_date);
        }
        else
        {
            end_date = gnc_date_edit_get_date_end(end_picker);
        }
    }
    else
    {
        g_warn_if_fail(false);
    }
}

void
GncFilterInvoicesDialogInternal::reset_dates ()
{
    if (start_picker)
        gnc_date_edit_set_time(start_picker, time(NULL));
    else
        g_warn_if_fail(false);

    if (end_picker)
        gnc_date_edit_set_time(end_picker, time(NULL));
    else
        g_warn_if_fail(false);

    set_dates(false);
}

QofQuery *
GncFilterInvoicesDialogInternal::create_qof_query ()
{
    auto *book = gnc_get_current_book ();

    auto *query = qof_query_create_for (GNC_ID_INVOICE);
    qof_query_set_book (query, book);

    {
        bool filter_empty = preset == Preset::CUSTOM && (!show_invoices && !show_creditnotes);
        bool show_inv = show_invoices || preset != Preset::CUSTOM;
        bool show_cred = show_creditnotes || preset != Preset::CUSTOM;
    
        if (show_inv || filter_empty)
        {
            GncInvoiceType inv_type;

            switch (owner_type)
            {
            case GNC_OWNER_CUSTOMER:
                inv_type = GNC_INVOICE_CUST_INVOICE;
                break;
            case GNC_OWNER_VENDOR:
                inv_type = GNC_INVOICE_VEND_INVOICE;
                break;
            case GNC_OWNER_EMPLOYEE:
                inv_type = GNC_INVOICE_EMPL_INVOICE;
                break;
            default:
                g_assert_not_reached ();
            }

            QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_TYPE, nullptr);
            QofQueryPredData *type_pred = qof_query_int32_predicate (QOF_COMPARE_EQUAL, inv_type);
            qof_query_add_term (query, type_path, type_pred, QOF_QUERY_AND);
        }

        if (show_cred || filter_empty)
        {
            GncInvoiceType credit_type;

            switch (owner_type)
            {
            case GNC_OWNER_CUSTOMER:
                credit_type = GNC_INVOICE_CUST_CREDIT_NOTE;
                break;
            case GNC_OWNER_VENDOR:
                credit_type = GNC_INVOICE_VEND_CREDIT_NOTE;
                break;
            case GNC_OWNER_EMPLOYEE:
                credit_type = GNC_INVOICE_EMPL_CREDIT_NOTE;
                break;
            default:
                g_assert_not_reached ();
            }

            QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_TYPE, nullptr);
            QofQueryPredData *type_pred = qof_query_int32_predicate (QOF_COMPARE_EQUAL, credit_type);
            qof_query_add_term (query, type_path, type_pred,
                filter_empty || !show_inv ? QOF_QUERY_AND : QOF_QUERY_OR);
        }
    }

    { // Filter only active invoices.
        QofQuery *query_active = qof_query_create_for (GNC_ID_INVOICE);
        qof_query_set_book (query_active, book);

        qof_query_add_boolean_match(
            query_active,
            qof_query_build_param_list(QOF_PARAM_ACTIVE, nullptr),
            true,
            QOF_QUERY_AND);

        qof_query_merge_in_place (query, query_active, QOF_QUERY_AND); 
        qof_query_destroy (query_active);
    }

    if (preset == Preset::OVERDUE)
    {
        QofQuery *query_overdue = qof_query_create_for (GNC_ID_INVOICE);
        qof_query_set_book (query_overdue, book);

        {
            QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_PAID, nullptr);

            qof_query_add_boolean_match(query_overdue,
                                        type_path,
                                        false,
                                        QOF_QUERY_AND);
        }

        {
            QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

            qof_query_add_boolean_match(query_overdue,
                                        type_path,
                                        true,
                                        QOF_QUERY_AND);
        }

        {
            GDate date;
            gnc_gdate_set_today(&date);

            if (overdue_days)
                g_date_subtract_days(&date, overdue_days);

            QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_DUE, nullptr);
            QofQueryPredData *pred_end = qof_query_date_predicate(
                QOF_COMPARE_LT, QOF_DATE_MATCH_NORMAL,
                gnc_time64_get_day_end_gdate(&date)
            );

            qof_query_add_term(query_overdue, type_path, pred_end, QOF_QUERY_AND);
        }

        qof_query_merge_in_place (query, query_overdue, QOF_QUERY_AND); 
        qof_query_destroy (query_overdue);
    }
    else if (preset == Preset::CUSTOM) {
        if (show_paid != show_unpaid || !show_paid)
        {
            QofQuery *query_paid = qof_query_create_for (GNC_ID_INVOICE);
            qof_query_set_book (query_paid, book);

            {
                QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_PAID, nullptr);

                qof_query_add_boolean_match(query_paid,
                                            type_path,
                                            show_paid,
                                            QOF_QUERY_AND);
            }

            if (!show_paid && !show_unpaid) { // If both options are false, add a TRUE match so no results are shown.
                QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_PAID, nullptr);

                qof_query_add_boolean_match(query_paid,
                                            type_path,
                                            true,
                                            QOF_QUERY_AND);
            }

            qof_query_merge_in_place (query, query_paid, QOF_QUERY_AND); 
            qof_query_destroy (query_paid);
        }

        if (show_posted != show_unposted || !show_posted)
        {
            QofQuery *query_posted = qof_query_create_for (GNC_ID_INVOICE);
            qof_query_set_book (query_posted, book);

            {
                QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                qof_query_add_boolean_match(query_posted,
                                            type_path,
                                            show_posted,
                                            QOF_QUERY_AND);
            }

            if (!show_posted && !show_unposted) { // If both options are false, add a TRUE match so no results are shown.
                QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                qof_query_add_boolean_match(query_posted,
                                            type_path,
                                            true,
                                            QOF_QUERY_AND);
            }

            qof_query_merge_in_place (query, query_posted, QOF_QUERY_AND); 
            qof_query_destroy (query_posted);
        }

        if (use_start_date && start_date != 0)
        {
            QofQuery *query_start = qof_query_create_for (GNC_ID_INVOICE);
            qof_query_set_book (query_start, book);

            {
                QofQuery *query_posted = qof_query_create_for (GNC_ID_INVOICE);
                qof_query_set_book (query_posted, book);

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                    qof_query_add_boolean_match(query_posted,
                                                type_path,
                                                true,
                                                QOF_QUERY_AND);
                }

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_POSTED, nullptr);
                    QofQueryPredData *pred_start = qof_query_date_predicate(QOF_COMPARE_GTE, QOF_DATE_MATCH_NORMAL, start_date);
                    qof_query_add_term(query_posted, type_path, pred_start, QOF_QUERY_AND);
                }

                qof_query_merge_in_place (query_start, query_posted, QOF_QUERY_AND); 
                qof_query_destroy (query_posted);
            }

            {
                QofQuery *query_unposted = qof_query_create_for (GNC_ID_INVOICE);
                qof_query_set_book (query_unposted, book);

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                    qof_query_add_boolean_match(query_unposted,
                                                type_path,
                                                false,
                                                QOF_QUERY_AND);
                }

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_OPENED, nullptr);
                    QofQueryPredData *pred_start = qof_query_date_predicate(QOF_COMPARE_GTE, QOF_DATE_MATCH_NORMAL, start_date);
                    qof_query_add_term(query_unposted, type_path, pred_start, QOF_QUERY_AND);
                }

                qof_query_merge_in_place (query_start, query_unposted, QOF_QUERY_OR); 
                qof_query_destroy (query_unposted);
            }

            qof_query_merge_in_place (query, query_start, QOF_QUERY_AND); 
            qof_query_destroy (query_start);
        }

        if (use_end_date && end_date != 0)
        {
            QofQuery *query_end = qof_query_create_for (GNC_ID_INVOICE);
            qof_query_set_book (query_end, book);

            {
                QofQuery *query_posted = qof_query_create_for (GNC_ID_INVOICE);
                qof_query_set_book (query_posted, book);

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                    qof_query_add_boolean_match(query_posted,
                                                type_path,
                                                true,
                                                QOF_QUERY_AND);
                }

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_POSTED, nullptr);
                    QofQueryPredData *pred_end = qof_query_date_predicate(QOF_COMPARE_LTE, QOF_DATE_MATCH_NORMAL, end_date);
                    qof_query_add_term(query_posted, type_path, pred_end, QOF_QUERY_AND);
                }

                qof_query_merge_in_place (query_end, query_posted, QOF_QUERY_AND); 
                qof_query_destroy (query_posted);
            }

            {
                QofQuery *query_unposted = qof_query_create_for (GNC_ID_INVOICE);
                qof_query_set_book (query_unposted, book);

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                    qof_query_add_boolean_match(query_unposted,
                                                type_path,
                                                false,
                                                QOF_QUERY_AND);
                }

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_OPENED, nullptr);
                    QofQueryPredData *pred_end = qof_query_date_predicate(QOF_COMPARE_LTE, QOF_DATE_MATCH_NORMAL, end_date);
                    qof_query_add_term(query_unposted, type_path, pred_end, QOF_QUERY_AND);
                }

                qof_query_merge_in_place (query_end, query_unposted, QOF_QUERY_OR); 
                qof_query_destroy (query_unposted);
            }

            qof_query_merge_in_place (query, query_end, QOF_QUERY_AND); 
            qof_query_destroy (query_end);
        }
    }

    if (search_term.length ())
    {
        QofQuery *query_term = qof_query_create_for (GNC_ID_INVOICE);
        qof_query_set_book (query_term, book);

        QofQueryParamList *params[4] = {
            nullptr,
            nullptr,
            nullptr,
            nullptr,
        };

        // Customer name
        params[0] = qof_query_build_param_list (INVOICE_OWNER, OWNER_PARENT,
                                                OWNER_NAME, nullptr);

        // Job name
        params[1] = qof_query_build_param_list (INVOICE_OWNER, OWNER_NAME,
                                                nullptr);

        // Invoice id
        params[2] = qof_query_build_param_list (INVOICE_ID, nullptr);

        // Invoice billing id 
        params[3] = qof_query_build_param_list (INVOICE_BILLINGID, nullptr);

        for (size_t i = 0; i < std::size(params); i++) {
            auto *path = params[i];

            QofQueryPredData *owner_name_pred =
                qof_query_string_predicate(
                    QOF_COMPARE_CONTAINS,
                    search_term.c_str(),
                    QOF_STRING_MATCH_CASEINSENSITIVE,
                    false);

            qof_query_add_term(
                query_term,
                path,
                owner_name_pred,
                i == 0 ? QOF_QUERY_AND : QOF_QUERY_OR);
        }

        qof_query_merge_in_place (query, query_term, QOF_QUERY_AND); 
        qof_query_destroy (query_term);
    }

    return query;
}

void
GncFilterInvoicesDialogInternal::dialog_open ()
{
    g_assert (plugin_page);
    g_assert (apply_filter);

    ENTER ("(fd %p, page %p)", dialog, plugin_page);

    if (dialog)
    {
        gtk_window_present (GTK_WINDOW (dialog));
        LEAVE ("existing dialog");
        return;
    }

    auto *builder = gtk_builder_new ();
    bool add_from_file = gnc_builder_add_from_file (builder, UI_FILE, "invoices-filters");

    if (!add_from_file)
    {
        g_object_unref (G_OBJECT(builder));
        g_return_if_fail (false);
    }

    dialog = GTK_WIDGET (gtk_builder_get_object (builder, "invoices-filters"));

    if (!dialog) {
        g_object_unref (G_OBJECT (builder));
        g_return_if_fail (false);
    }

    {
        auto window = GTK_WINDOW (gnc_plugin_page_get_window (plugin_page));

        g_assert (window);

        gtk_window_set_transient_for(GTK_WINDOW (dialog), window);
    }

    /* Translators: The %s is the name of the plugin page */
    gchar *title = g_strdup_printf(
        _("Filter %s by…"),
        gnc_plugin_page_get_page_name (plugin_page)
    );

    gtk_window_set_title(GTK_WINDOW(dialog), title);

    g_free(title);

    gtk_widget_show_all (dialog);

    g_signal_connect(dialog, "destroy", G_CALLBACK (dialog_destroy), this);

    {
        custom_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "preset-custom"));
        overdue_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "preset-overdue"));
        custom_controls = GTK_WIDGET (gtk_builder_get_object(builder, "custom-controls"));
        overdue_controls = GTK_WIDGET (gtk_builder_get_object(builder, "overdue-controls"));

        if (!custom_toggle || !overdue_toggle || !custom_controls || !overdue_controls)
        {
            g_warn_if_fail(false);
        }
        else
        {
            {
                if (owner_type == GNC_OWNER_VENDOR)
                {
                    gtk_button_set_label(
                        GTK_BUTTON (overdue_toggle),
                        _("Overdue Bills")
                    );
                }
                else if (owner_type == GNC_OWNER_EMPLOYEE)
                {
                    gtk_button_set_label(
                        GTK_BUTTON (overdue_toggle),
                        _("Overdue Expense Vouchers")
                    );
                }
            }

            g_signal_connect(custom_toggle, "toggled",
                             G_CALLBACK(action_custom_toggle), this);

            g_signal_connect(overdue_toggle, "toggled",
                             G_CALLBACK(action_overdue_toggle), this);
        }

        set_preset (preset);
    }

    {
        overdue_days_zero = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-0"));
        overdue_days_seven = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-7"));
        overdue_days_fourteen = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-14"));
        overdue_days_thirty = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-30"));
        overdue_days_sixty = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-60"));
        overdue_days_ninety = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-90"));

        if (!overdue_days_zero || !overdue_days_seven || !overdue_days_fourteen ||
            !overdue_days_thirty || !overdue_days_sixty || !overdue_days_ninety)
        {
            g_warn_if_fail(false);
        }
        else
        {
            GtkToggleButton *toggles[] = {
                overdue_days_zero, overdue_days_seven, overdue_days_fourteen,
                overdue_days_thirty, overdue_days_sixty, overdue_days_ninety
            };

            switch (overdue_days)
            {
            case 0:
                set_overdue_days (overdue_days_zero);
                break;
            case 7:
                set_overdue_days (overdue_days_seven);
                break;
            case 14:
                set_overdue_days (overdue_days_fourteen);
                break;
            case 30:
                set_overdue_days (overdue_days_thirty);
                break;
            case 60:
                set_overdue_days (overdue_days_sixty);
                break;
            case 90:
                set_overdue_days (overdue_days_ninety);
                break;
            default:
                break;
            }

            for (size_t i = 0; i < std::size (toggles); i++)
            {
                g_signal_connect(toggles[i], "toggled",
                                 G_CALLBACK(action_overdue_days_toggle), this);
            }
        }
    }

    {
        paid_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-paid"));

        if (!paid_toggle)
        {
            g_warn_if_fail(false);
        }
        else
        {
            set_paid (show_paid);

            g_signal_connect(paid_toggle, "toggled",
                             G_CALLBACK(action_paid_toggle), this);
        }
    }

    {
        unpaid_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-unpaid"));


        if (!unpaid_toggle)
        {
            g_warn_if_fail(false);
        }
        else
        {
            set_unpaid (show_unpaid);

            g_signal_connect(unpaid_toggle, "toggled",
                             G_CALLBACK(action_unpaid_toggle), this);
        }
    }

    {
        posted_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-posted"));

        if (!posted_toggle)
        {
            g_warn_if_fail(false);
        }
        else
        {
            set_posted (show_posted);

            g_signal_connect(posted_toggle, "toggled",
                             G_CALLBACK(action_posted_toggle), this);
        }
    }

    {
        unposted_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-unposted"));

        if (!unposted_toggle)
        {
            g_warn_if_fail(false);
        }
        else
        {
            set_unposted (show_unposted);

            g_signal_connect(unposted_toggle, "toggled",
                             G_CALLBACK(action_unposted_toggle), this);
        }
    }

    {
        invoices_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-invoices"));
        
        if (!invoices_toggle)
        {
            g_warn_if_fail(false);
        }
        else
        {
            if (owner_type == GNC_OWNER_VENDOR)
            {
                gtk_button_set_label(
                    GTK_BUTTON (invoices_toggle),
                    _("Show Bills")
                );
            }
            else if (owner_type == GNC_OWNER_EMPLOYEE)
            {
                gtk_button_set_label(
                    GTK_BUTTON (invoices_toggle),
                    _("Show Expense Vouchers")
                );
            }

            set_invoices (show_invoices);

            g_signal_connect(invoices_toggle, "toggled",
                             G_CALLBACK(action_invoices_toggle), this);
        }
    }

    {
        creditnotes_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-creditnotes"));

        if (!creditnotes_toggle)
        {
            g_warn_if_fail(false);
        }
        else
        {
            gtk_toggle_button_set_active(creditnotes_toggle, show_creditnotes);
            set_creditnotes (show_creditnotes);

            g_signal_connect(creditnotes_toggle, "toggled",
                             G_CALLBACK(action_creditnotes_toggle), this);
        }
    }

    {
        GtkWidget* start_box = GTK_WIDGET (gtk_builder_get_object (builder, "start_date_box"));

        if (!start_box) {
            g_warn_if_fail (false);
        }
        else {
            start_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "filter-by-start"));
            start_picker = GNC_DATE_EDIT (gnc_date_edit_new (time (NULL), FALSE, TRUE));

            if (start_picker)
            {
                gtk_box_pack_start (GTK_BOX (start_box), GTK_WIDGET (start_picker), TRUE, TRUE, 0);

                g_signal_connect(start_picker->date_entry, "activate",
                                 G_CALLBACK(action_start_picker_activate), this);
            }
            else g_warn_if_fail (false);

            if (start_toggle)
            {
                g_signal_connect(start_toggle, "toggled",
                                 G_CALLBACK(action_start_toggle), this);
            }
            else
            {
                g_warn_if_fail (false);
            }

            set_use_start_date (use_start_date);
            set_dates(true);

            gtk_widget_show_all (start_box);
        }
    }

    {
        GtkWidget* end_box = GTK_WIDGET (gtk_builder_get_object (builder, "end_date_box"));

        if (!end_box) {
            g_warn_if_fail (false);
        }
        else {
            end_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "filter-by-end"));
            end_picker = GNC_DATE_EDIT (gnc_date_edit_new (time (NULL), FALSE, TRUE));

            if (end_picker)
            {
                gtk_box_pack_start (GTK_BOX (end_box), GTK_WIDGET (end_picker), TRUE, TRUE, 0);

                g_signal_connect(end_picker->date_entry, "activate",
                                 G_CALLBACK(action_end_picker_activate), this);
            }
            else
            {
                g_warn_if_fail (false);
            }

            if (end_toggle)
            {
                g_signal_connect(end_toggle, "toggled",
                                 G_CALLBACK(action_end_toggle), this);
            }
            else
            {
                g_warn_if_fail (false);
            }

            set_use_end_date (use_end_date);
            set_dates(true);

            gtk_widget_show_all(end_box);
        }
    }

    {
        auto *apply_button = GTK_BUTTON (gtk_builder_get_object(builder, "apply-dates"));

        if (!apply_button)
        {
            g_warn_if_fail(false);
        }
        else
        {
            g_signal_connect(apply_button, "clicked",
                             G_CALLBACK(action_apply_dates), this);
        }
    }

    {
        search_entry = GTK_ENTRY (gtk_builder_get_object(builder, "search"));

        if (!search_entry)
        {
            g_warn_if_fail(false);
        }
        else
        {
            if (owner_type == GNC_OWNER_VENDOR)
            {
                gtk_entry_set_placeholder_text(
                    search_entry,
                    "Type to search for Bills..."
                );
            }
            else if (owner_type == GNC_OWNER_EMPLOYEE)
            {
                gtk_entry_set_placeholder_text(
                    search_entry,
                    "Type to search for Expense Vouchers..."
                );
            }

            if (search_term.length())
                set_search_term (search_term.c_str());

            g_signal_connect(search_entry, "activate",
                             G_CALLBACK(action_search_entry_activate), this);
        }

        auto *apply_button = GTK_BUTTON (gtk_builder_get_object(builder, "apply-search"));

        if (!apply_button)
        {
            g_warn_if_fail(false);
        }
        else {
            g_signal_connect(apply_button, "clicked",
                             G_CALLBACK(action_apply_search_entry), this);
        }
    }

    {
        auto *reset_button = GTK_BUTTON (gtk_builder_get_object(builder, "reset"));

        if (!reset_button)
        {
            g_warn_if_fail(false);
        }
        else
        {
            g_signal_connect (reset_button, "clicked",
                              G_CALLBACK (action_reset), this);
        }
    }

    {
        auto *close_button = GTK_BUTTON (gtk_builder_get_object(builder, "close"));

        if (!close_button)
        {
            g_warn_if_fail(false);
        }
        else
        {
            g_signal_connect(close_button, "clicked",
                             G_CALLBACK(action_close), dialog);
        }
    }

    g_object_unref(G_OBJECT(builder));

    LEAVE("");
}

GncFilterInvoicesDialog::GncFilterInvoicesDialog (
    GncPluginPage &plugin_page, GncOwnerType &owner_type,
    std::function<void()> apply_cb
)
{
    g_return_if_fail(owner_type == GNC_OWNER_CUSTOMER ||
                     owner_type == GNC_OWNER_VENDOR ||
                     owner_type == GNC_OWNER_EMPLOYEE);

    filter = std::make_unique<GncFilterInvoicesDialogInternal> (
        owner_type, plugin_page, apply_cb
    ); 
}

GncFilterInvoicesDialog::~GncFilterInvoicesDialog() = default;

void
GncFilterInvoicesDialog::create_dialog ()
{
    g_return_if_fail (filter != nullptr);

    filter->dialog_open ();
}

QofQuery *
GncFilterInvoicesDialog::create_qof_query ()
{
    g_return_val_if_fail (filter != nullptr, nullptr);

    return filter->create_qof_query ();
}
