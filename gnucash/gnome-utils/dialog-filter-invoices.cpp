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
    GncOwnerType m_owner_type;
    GncPluginPage *m_plugin_page = nullptr;
    GtkWidget *m_dialog = nullptr;

    // State
    Preset m_preset = Preset::CUSTOM;
    int m_overdue_days = 0;
    bool m_show_paid = true;
    bool m_show_unpaid = true;
    bool m_show_posted = true;
    bool m_show_unposted = true;
    bool m_show_invoices = true;
    bool m_show_creditnotes = true;
    bool m_use_start_date = false;
    bool m_use_end_date = false;
    time64 m_start_date = 0;
    time64 m_end_date = 0;
    std::string m_search_term;
    // State end

    GtkToggleButton *m_custom_toggle = nullptr;
    GtkToggleButton *m_overdue_toggle = nullptr;
    GtkWidget *m_custom_controls = nullptr;
    GtkWidget *m_overdue_controls = nullptr;

    GtkToggleButton *m_overdue_days_zero = nullptr;
    GtkToggleButton *m_overdue_days_seven = nullptr;
    GtkToggleButton *m_overdue_days_fourteen = nullptr;
    GtkToggleButton *m_overdue_days_thirty = nullptr;
    GtkToggleButton *m_overdue_days_sixty = nullptr;
    GtkToggleButton *m_overdue_days_ninety = nullptr;

    GtkToggleButton *m_paid_toggle = nullptr;
    GtkToggleButton *m_unpaid_toggle = nullptr;
    GtkToggleButton *m_posted_toggle = nullptr;
    GtkToggleButton *m_unposted_toggle = nullptr;
    GtkToggleButton *m_invoices_toggle = nullptr;
    GtkToggleButton *m_creditnotes_toggle = nullptr;
    GtkToggleButton *m_start_toggle = nullptr;
    GtkToggleButton *m_end_toggle = nullptr;
    GNCDateEdit *m_start_picker = nullptr;
    GNCDateEdit *m_end_picker = nullptr;
    GtkEntry *m_search_entry = nullptr;

public:
    std::function<void()> m_apply_filter = [](){
        g_warn_if_fail (false);
    };

    GncFilterInvoicesDialogInternal (GncOwnerType &owner, GncPluginPage &page,
                                     std::function<void()> apply_cb) :
                                    m_owner_type(owner), m_plugin_page(&page)
    {
        if (apply_cb)
            m_apply_filter = apply_cb;
        else
            g_warn_if_fail (false);
    }
 
    ~GncFilterInvoicesDialogInternal ()
    {
        if (m_dialog)
            gtk_widget_destroy (GTK_WIDGET (m_dialog));
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
        if (m_use_start_date || m_use_end_date)
            m_apply_filter ();
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
    filter->m_apply_filter ();

    return false;
}

static gboolean
action_overdue_toggle (GtkToggleButton *button, gpointer user_data)
{
    if (!gtk_toggle_button_get_active(button))
        return false;

    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_preset (Preset::OVERDUE);
    filter->m_apply_filter ();

    return false;
}

static gboolean
action_overdue_days_toggle (GtkToggleButton *button, gpointer user_data)
{
    if (!gtk_toggle_button_get_active(button))
        return false;

    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_overdue_days (button);
    filter->m_apply_filter ();

    return false;
}

static gboolean
action_paid_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_paid (state);
    filter->m_apply_filter ();

    return false;
}

static gboolean
action_unpaid_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_unpaid (state);
    filter->m_apply_filter ();

    return false;
}

static gboolean
action_posted_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_posted (state);
    filter->m_apply_filter ();

    return false;
}

static gboolean
action_unposted_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_unposted (state);
    filter->m_apply_filter ();

    return false;
}

static gboolean
action_invoices_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_invoices (state);
    filter->m_apply_filter ();

    return false;
}

static gboolean
action_creditnotes_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_creditnotes (state);
    filter->m_apply_filter ();

    return false;
}

static void
action_start_picker_activate (GtkWidget *date, gpointer user_data)
{
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_dates (false);
    filter->m_apply_filter ();
}

static gboolean
action_start_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_use_start_date (state);
    filter->m_apply_filter ();

    return false;
}

static void
action_end_picker_activate (GtkWidget *date, gpointer user_data)
{
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_dates (false);
    filter->m_apply_filter ();
}

static gboolean
action_end_toggle (GtkToggleButton *button, gpointer user_data)
{
    auto state = gtk_toggle_button_get_active(button);
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_use_end_date (state);
    filter->m_apply_filter ();

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
    filter->m_apply_filter ();
}

static void
action_apply_search_entry (GtkButton *button, gpointer user_data)
{
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);

    filter->set_search_term ();
    filter->m_apply_filter ();
}

static void
action_reset (GtkButton *button, gpointer user_data)
{
    auto *filter = static_cast<GncFilterInvoicesDialogInternal *> (user_data);
    if (filter->reset ())
        filter->m_apply_filter ();
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
    m_dialog = nullptr;

    m_custom_toggle = nullptr;
    m_overdue_toggle = nullptr;
    m_custom_controls = nullptr;
    m_overdue_controls = nullptr;

    m_overdue_days_zero = nullptr;
    m_overdue_days_seven = nullptr;
    m_overdue_days_fourteen = nullptr;
    m_overdue_days_thirty = nullptr;
    m_overdue_days_sixty = nullptr;
    m_overdue_days_ninety = nullptr;

    m_paid_toggle = nullptr;
    m_unpaid_toggle = nullptr;
    m_posted_toggle = nullptr;
    m_unposted_toggle = nullptr;
    m_invoices_toggle = nullptr;
    m_creditnotes_toggle = nullptr;
    m_start_toggle = nullptr;
    m_end_toggle = nullptr;
    m_start_picker = nullptr;
    m_end_picker = nullptr;
    m_search_entry = nullptr;
}

bool 
GncFilterInvoicesDialogInternal::reset ()
{
    if (m_preset == Preset::CUSTOM
        && m_show_paid && m_show_unpaid
        && m_show_posted && m_show_unposted
        && m_show_invoices && m_show_creditnotes
        && m_search_term == ""
        && !m_use_start_date && !m_use_end_date)
    {
        return false;
    }
    else if (m_preset == Preset::OVERDUE
             && m_overdue_days == 0
             && m_search_term == "")
    {
        return false;
    }

    if (m_preset == Preset::CUSTOM)
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
    else if (m_preset == Preset::OVERDUE)
    {
        set_overdue_days (m_overdue_days_zero);
    }

    set_search_term ("");

    g_warn_if_fail (m_dialog);

    return true;
}

void
GncFilterInvoicesDialogInternal::resize_window ()
{
    g_return_if_fail (m_dialog);

    GtkRequisition requisition;
    gtk_widget_get_preferred_size (GTK_WIDGET(m_dialog),
                                   nullptr,
                                   &requisition);

    gtk_window_resize (GTK_WINDOW (m_dialog),
                       requisition.width,
                       requisition.height);
}

void
GncFilterInvoicesDialogInternal::set_preset (Preset p)
{
    m_preset = p;
    GtkToggleButton *toggle;

    switch (p)
    {
    case Preset::CUSTOM:
      toggle = m_custom_toggle;
      gtk_widget_hide (m_overdue_controls);
      gtk_widget_show (m_custom_controls);
      break;
    case Preset::OVERDUE:
        toggle = m_overdue_toggle;
        gtk_widget_hide (m_custom_controls);
        gtk_widget_show (m_overdue_controls);
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

    if (button == m_overdue_days_seven)
        days = 7;
    else if (button == m_overdue_days_fourteen)
        days = 14;
    else if (button == m_overdue_days_thirty)
        days = 30;
    else if (button == m_overdue_days_sixty)
        days = 60;
    else if (button == m_overdue_days_ninety)
        days = 90;

    m_overdue_days = days;
}

void
GncFilterInvoicesDialogInternal::set_paid (bool status)
{
    m_show_paid = status;

    if (m_paid_toggle)
        gtk_toggle_button_set_active(m_paid_toggle, m_show_paid);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_unpaid (bool status)
{
    m_show_unpaid = status;

    if (m_unpaid_toggle)
        gtk_toggle_button_set_active(m_unpaid_toggle, m_show_unpaid);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_posted (bool status)
{
    m_show_posted = status;

    if (m_posted_toggle)
        gtk_toggle_button_set_active(m_posted_toggle, m_show_posted);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_unposted (bool status)
{
    m_show_unposted = status;

    if (m_unposted_toggle)
        gtk_toggle_button_set_active(m_unposted_toggle, m_show_unposted);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_invoices (bool status)
{
    m_show_invoices = status;

    if (m_invoices_toggle)
        gtk_toggle_button_set_active(m_invoices_toggle, m_show_invoices);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_creditnotes (bool status)
{
    m_show_creditnotes = status;

    if (m_creditnotes_toggle)
        gtk_toggle_button_set_active(m_creditnotes_toggle, m_show_creditnotes);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_search_term (const char *term)
{
    m_search_term = term;

    if (m_search_entry)
        gtk_entry_set_text(m_search_entry, m_search_term.c_str());
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_search_term ()
{
    const gchar *text = gtk_entry_get_text(m_search_entry);

    set_search_term (text);
}

void
GncFilterInvoicesDialogInternal::set_use_start_date (bool status)
{
    m_use_start_date = status;

    if (m_start_toggle)
        gtk_toggle_button_set_active(m_start_toggle, m_use_start_date);
    else
        g_warn_if_fail(false);

    if (m_start_picker)
        gtk_widget_set_sensitive (GTK_WIDGET (m_start_picker), m_use_start_date);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_use_end_date (bool status)
{
    m_use_end_date = status;

    if (m_end_toggle)
        gtk_toggle_button_set_active(m_end_toggle, m_use_end_date);
    else
        g_warn_if_fail(false);

    if (m_end_picker)
        gtk_widget_set_sensitive (GTK_WIDGET (m_end_picker), m_use_end_date);
    else
        g_warn_if_fail(false);
}

void
GncFilterInvoicesDialogInternal::set_dates (bool init)
{
    if (m_start_picker)
    {
        if (init && m_start_date != 0)
        {
            gnc_date_edit_set_time(m_start_picker, m_start_date);
        }
        else
        {
            m_start_date = gnc_date_edit_get_date(m_start_picker);
        }
    }
    else g_warn_if_fail(false);

    if (m_end_picker)
    {
        if (init && m_end_date != 0)
        {
            gnc_date_edit_set_time(m_end_picker, m_end_date);
        }
        else
        {
            m_end_date = gnc_date_edit_get_date_end(m_end_picker);
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
    if (m_start_picker)
        gnc_date_edit_set_time(m_start_picker, time(NULL));
    else
        g_warn_if_fail(false);

    if (m_end_picker)
        gnc_date_edit_set_time(m_end_picker, time(NULL));
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
        bool filter_empty = m_preset == Preset::CUSTOM && (!m_show_invoices && !m_show_creditnotes);
        bool show_inv = m_show_invoices || m_preset != Preset::CUSTOM;
        bool show_cred = m_show_creditnotes || m_preset != Preset::CUSTOM;
    
        if (show_inv || filter_empty)
        {
            GncInvoiceType inv_type;

            switch (m_owner_type)
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

            switch (m_owner_type)
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

    if (m_preset == Preset::OVERDUE)
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

            if (m_overdue_days)
                g_date_subtract_days(&date, m_overdue_days);

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
    else if (m_preset == Preset::CUSTOM) {
        if (m_show_paid != m_show_unpaid || !m_show_paid)
        {
            QofQuery *query_paid = qof_query_create_for (GNC_ID_INVOICE);
            qof_query_set_book (query_paid, book);

            {
                QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_PAID, nullptr);

                qof_query_add_boolean_match(query_paid,
                                            type_path,
                                            m_show_paid,
                                            QOF_QUERY_AND);
            }

            if (!m_show_paid && !m_show_unpaid) { // If both options are false, add a TRUE match so no results are shown.
                QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_PAID, nullptr);

                qof_query_add_boolean_match(query_paid,
                                            type_path,
                                            true,
                                            QOF_QUERY_AND);
            }

            qof_query_merge_in_place (query, query_paid, QOF_QUERY_AND); 
            qof_query_destroy (query_paid);
        }

        if (m_show_posted != m_show_unposted || !m_show_posted)
        {
            QofQuery *query_posted = qof_query_create_for (GNC_ID_INVOICE);
            qof_query_set_book (query_posted, book);

            {
                QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                qof_query_add_boolean_match(query_posted,
                                            type_path,
                                            m_show_posted,
                                            QOF_QUERY_AND);
            }

            if (!m_show_posted && !m_show_unposted) { // If both options are false, add a TRUE match so no results are shown.
                QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                qof_query_add_boolean_match(query_posted,
                                            type_path,
                                            true,
                                            QOF_QUERY_AND);
            }

            qof_query_merge_in_place (query, query_posted, QOF_QUERY_AND); 
            qof_query_destroy (query_posted);
        }

        if (m_use_start_date && m_start_date != 0)
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
                    QofQueryPredData *pred_start = qof_query_date_predicate(QOF_COMPARE_GTE, QOF_DATE_MATCH_NORMAL, m_start_date);
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
                    QofQueryPredData *pred_start = qof_query_date_predicate(QOF_COMPARE_GTE, QOF_DATE_MATCH_NORMAL, m_start_date);
                    qof_query_add_term(query_unposted, type_path, pred_start, QOF_QUERY_AND);
                }

                qof_query_merge_in_place (query_start, query_unposted, QOF_QUERY_OR); 
                qof_query_destroy (query_unposted);
            }

            qof_query_merge_in_place (query, query_start, QOF_QUERY_AND); 
            qof_query_destroy (query_start);
        }

        if (m_use_end_date && m_end_date != 0)
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
                    QofQueryPredData *pred_end = qof_query_date_predicate(QOF_COMPARE_LTE, QOF_DATE_MATCH_NORMAL, m_end_date);
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
                    QofQueryPredData *pred_end = qof_query_date_predicate(QOF_COMPARE_LTE, QOF_DATE_MATCH_NORMAL, m_end_date);
                    qof_query_add_term(query_unposted, type_path, pred_end, QOF_QUERY_AND);
                }

                qof_query_merge_in_place (query_end, query_unposted, QOF_QUERY_OR); 
                qof_query_destroy (query_unposted);
            }

            qof_query_merge_in_place (query, query_end, QOF_QUERY_AND); 
            qof_query_destroy (query_end);
        }
    }

    if (m_search_term.length ())
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
                    m_search_term.c_str(),
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
    g_assert (m_plugin_page);
    g_assert (m_apply_filter);

    ENTER ("(fd %p, page %p)", m_dialog, m_plugin_page);

    if (m_dialog)
    {
        gtk_window_present (GTK_WINDOW (m_dialog));
        LEAVE ("existing m_dialog");
        return;
    }

    auto *builder = gtk_builder_new ();
    bool add_from_file = gnc_builder_add_from_file (builder, UI_FILE, "invoices-filters");

    if (!add_from_file)
    {
        g_object_unref (G_OBJECT(builder));
        g_return_if_fail (false);
    }

    m_dialog = GTK_WIDGET (gtk_builder_get_object (builder, "invoices-filters"));

    if (!m_dialog) {
        g_object_unref (G_OBJECT (builder));
        g_return_if_fail (false);
    }

    {
        auto window = GTK_WINDOW (gnc_plugin_page_get_window (m_plugin_page));

        g_assert (window);

        gtk_window_set_transient_for(GTK_WINDOW (m_dialog), window);
    }

    /* Translators: The %s is the name of the plugin page */
    gchar *title = g_strdup_printf(
        _("Filter %s by…"),
        gnc_plugin_page_get_page_name (m_plugin_page)
    );

    gtk_window_set_title(GTK_WINDOW(m_dialog), title);

    g_free(title);

    gtk_widget_show_all (m_dialog);

    g_signal_connect(m_dialog, "destroy", G_CALLBACK (dialog_destroy), this);

    {
        m_custom_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "preset-custom"));
        m_overdue_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "preset-overdue"));
        m_custom_controls = GTK_WIDGET (gtk_builder_get_object(builder, "custom-controls"));
        m_overdue_controls = GTK_WIDGET (gtk_builder_get_object(builder, "overdue-controls"));

        if (!m_custom_toggle || !m_overdue_toggle || !m_custom_controls || !m_overdue_controls)
        {
            g_warn_if_fail(false);
        }
        else
        {
            {
                if (m_owner_type == GNC_OWNER_VENDOR)
                {
                    gtk_button_set_label(
                        GTK_BUTTON (m_overdue_toggle),
                        _("Overdue Bills")
                    );
                }
                else if (m_owner_type == GNC_OWNER_EMPLOYEE)
                {
                    gtk_button_set_label(
                        GTK_BUTTON (m_overdue_toggle),
                        _("Overdue Expense Vouchers")
                    );
                }
            }

            g_signal_connect(m_custom_toggle, "toggled",
                             G_CALLBACK(action_custom_toggle), this);

            g_signal_connect(m_overdue_toggle, "toggled",
                             G_CALLBACK(action_overdue_toggle), this);
        }

        set_preset (m_preset);
    }

    {
        m_overdue_days_zero = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-0"));
        m_overdue_days_seven = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-7"));
        m_overdue_days_fourteen = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-14"));
        m_overdue_days_thirty = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-30"));
        m_overdue_days_sixty = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-60"));
        m_overdue_days_ninety = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-90"));

        if (!m_overdue_days_zero || !m_overdue_days_seven || !m_overdue_days_fourteen ||
            !m_overdue_days_thirty || !m_overdue_days_sixty || !m_overdue_days_ninety)
        {
            g_warn_if_fail(false);
        }
        else
        {
            GtkToggleButton *toggles[] = {
                m_overdue_days_zero, m_overdue_days_seven, m_overdue_days_fourteen,
                m_overdue_days_thirty, m_overdue_days_sixty, m_overdue_days_ninety
            };

            switch (m_overdue_days)
            {
            case 0:
                set_overdue_days (m_overdue_days_zero);
                break;
            case 7:
                set_overdue_days (m_overdue_days_seven);
                break;
            case 14:
                set_overdue_days (m_overdue_days_fourteen);
                break;
            case 30:
                set_overdue_days (m_overdue_days_thirty);
                break;
            case 60:
                set_overdue_days (m_overdue_days_sixty);
                break;
            case 90:
                set_overdue_days (m_overdue_days_ninety);
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
        m_paid_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-paid"));

        if (!m_paid_toggle)
        {
            g_warn_if_fail(false);
        }
        else
        {
            set_paid (m_show_paid);

            g_signal_connect(m_paid_toggle, "toggled",
                             G_CALLBACK(action_paid_toggle), this);
        }
    }

    {
        m_unpaid_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-unpaid"));


        if (!m_unpaid_toggle)
        {
            g_warn_if_fail(false);
        }
        else
        {
            set_unpaid (m_show_unpaid);

            g_signal_connect(m_unpaid_toggle, "toggled",
                             G_CALLBACK(action_unpaid_toggle), this);
        }
    }

    {
        m_posted_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-posted"));

        if (!m_posted_toggle)
        {
            g_warn_if_fail(false);
        }
        else
        {
            set_posted (m_show_posted);

            g_signal_connect(m_posted_toggle, "toggled",
                             G_CALLBACK(action_posted_toggle), this);
        }
    }

    {
        m_unposted_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-unposted"));

        if (!m_unposted_toggle)
        {
            g_warn_if_fail(false);
        }
        else
        {
            set_unposted (m_show_unposted);

            g_signal_connect(m_unposted_toggle, "toggled",
                             G_CALLBACK(action_unposted_toggle), this);
        }
    }

    {
        m_invoices_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-invoices"));
        
        if (!m_invoices_toggle)
        {
            g_warn_if_fail(false);
        }
        else
        {
            if (m_owner_type == GNC_OWNER_VENDOR)
            {
                gtk_button_set_label(
                    GTK_BUTTON (m_invoices_toggle),
                    _("Show Bills")
                );
            }
            else if (m_owner_type == GNC_OWNER_EMPLOYEE)
            {
                gtk_button_set_label(
                    GTK_BUTTON (m_invoices_toggle),
                    _("Show Expense Vouchers")
                );
            }

            set_invoices (m_show_invoices);

            g_signal_connect(m_invoices_toggle, "toggled",
                             G_CALLBACK(action_invoices_toggle), this);
        }
    }

    {
        m_creditnotes_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-creditnotes"));

        if (!m_creditnotes_toggle)
        {
            g_warn_if_fail(false);
        }
        else
        {
            gtk_toggle_button_set_active(m_creditnotes_toggle, m_show_creditnotes);
            set_creditnotes (m_show_creditnotes);

            g_signal_connect(m_creditnotes_toggle, "toggled",
                             G_CALLBACK(action_creditnotes_toggle), this);
        }
    }

    {
        GtkWidget* start_box = GTK_WIDGET (gtk_builder_get_object (builder, "start_date_box"));

        if (!start_box) {
            g_warn_if_fail (false);
        }
        else {
            m_start_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "filter-by-start"));
            m_start_picker = GNC_DATE_EDIT (gnc_date_edit_new (time (NULL), FALSE, TRUE));

            if (m_start_picker)
            {
                gtk_box_pack_start (GTK_BOX (start_box), GTK_WIDGET (m_start_picker), TRUE, TRUE, 0);

                g_signal_connect(m_start_picker->date_entry, "activate",
                                 G_CALLBACK(action_start_picker_activate), this);
            }
            else g_warn_if_fail (false);

            if (m_start_toggle)
            {
                g_signal_connect(m_start_toggle, "toggled",
                                 G_CALLBACK(action_start_toggle), this);
            }
            else
            {
                g_warn_if_fail (false);
            }

            set_use_start_date (m_use_start_date);
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
            m_end_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "filter-by-end"));
            m_end_picker = GNC_DATE_EDIT (gnc_date_edit_new (time (NULL), FALSE, TRUE));

            if (m_end_picker)
            {
                gtk_box_pack_start (GTK_BOX (end_box), GTK_WIDGET (m_end_picker), TRUE, TRUE, 0);

                g_signal_connect(m_end_picker->date_entry, "activate",
                                 G_CALLBACK(action_end_picker_activate), this);
            }
            else
            {
                g_warn_if_fail (false);
            }

            if (m_end_toggle)
            {
                g_signal_connect(m_end_toggle, "toggled",
                                 G_CALLBACK(action_end_toggle), this);
            }
            else
            {
                g_warn_if_fail (false);
            }

            set_use_end_date (m_use_end_date);
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
        m_search_entry = GTK_ENTRY (gtk_builder_get_object(builder, "search"));

        if (!m_search_entry)
        {
            g_warn_if_fail(false);
        }
        else
        {
            if (m_owner_type == GNC_OWNER_VENDOR)
            {
                gtk_entry_set_placeholder_text(
                    m_search_entry,
                    "Type to search for Bills..."
                );
            }
            else if (m_owner_type == GNC_OWNER_EMPLOYEE)
            {
                gtk_entry_set_placeholder_text(
                    m_search_entry,
                    "Type to search for Expense Vouchers..."
                );
            }

            if (m_search_term.length())
                set_search_term (m_search_term.c_str());

            g_signal_connect(m_search_entry, "activate",
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
                             G_CALLBACK(action_close), m_dialog);
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

    m_filter = std::make_unique<GncFilterInvoicesDialogInternal> (
        owner_type, plugin_page, apply_cb
    ); 
}

GncFilterInvoicesDialog::~GncFilterInvoicesDialog() = default;

void
GncFilterInvoicesDialog::create_dialog ()
{
    g_return_if_fail (m_filter != nullptr);

    m_filter->dialog_open ();
}

QofQuery *
GncFilterInvoicesDialog::create_qof_query ()
{
    g_return_val_if_fail (m_filter != nullptr, nullptr);

    return m_filter->create_qof_query ();
}
