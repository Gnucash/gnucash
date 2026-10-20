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
    GncFilterInvoicesDialog::OwnerType m_owner_type;
    GncPluginPage *m_plugin_page = nullptr;
    GtkWidget *m_dialog = nullptr;

    // State
    bool m_dialog_initialized = false;
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
        PWARN("Filter Function not set");
    };

    GncFilterInvoicesDialogInternal (
        GncFilterInvoicesDialog::OwnerType owner,
        GncPluginPage &page,
        std::function<void()> apply_cb) : m_owner_type(owner),
                                          m_plugin_page(&page)
    {
        if (apply_cb)
            m_apply_filter = apply_cb;
        else
            g_warn_if_fail (false);

        dialog_create ();
    }
 
    ~GncFilterInvoicesDialogInternal ()
    {
        if (m_dialog)
            gtk_widget_destroy (GTK_WIDGET (m_dialog));
    }

    QofQuery *create_qof_query (bool empty = false);

    void dialog_open();

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

    void dialog_create ();

    bool setup_ui_elements (GtkBuilder *builder);

    bool setup_ui_presets (GtkBuilder *builder);

    bool setup_ui_overdue (GtkBuilder *builder);

    bool setup_ui_custom_toggles (GtkBuilder *builder);

    bool setup_ui_custom_start_end_date (GtkBuilder *builder);

    bool setup_ui_search (GtkBuilder *builder);

    bool setup_ui_buttons (GtkBuilder *builder);

    bool query_filter_base (QofQuery *query, bool empty);

    void query_filter_active (QofQuery *query);

    void query_filter_overdue (QofQuery *query);

    void query_filter_custom_paid_unpaid (QofQuery *query);

    void query_filter_custom_posted_unposted (QofQuery *query);

    QofQuery *query_filter_date_compare (bool is_posted, bool is_less,
                                         time64 compare_date);

    void query_filter_custom_start (QofQuery *query);

    void query_filter_custom_end (QofQuery *query);

    void query_filter_search_term (QofQuery *query);
};


/************************************************************/
/*                     Command callbacks                    */
/************************************************************/

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
    auto *entry = GTK_ENTRY(editable);
    g_return_if_fail (entry);

    const gchar *text = gtk_entry_get_text(entry);
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


/************************************************************/
/*                     Implementation                       */
/************************************************************/

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
        PWARN("toggle is not set"); 

    resize_window ();
}

void
GncFilterInvoicesDialogInternal::set_overdue_days (GtkToggleButton *button)
{
    g_return_if_fail (button);

    gtk_toggle_button_set_active (button, true);

    if (button == m_overdue_days_zero)
        m_overdue_days = 0;
    else if (button == m_overdue_days_seven)
        m_overdue_days = 7;
    else if (button == m_overdue_days_fourteen)
        m_overdue_days = 14;
    else if (button == m_overdue_days_thirty)
        m_overdue_days = 30;
    else if (button == m_overdue_days_sixty)
        m_overdue_days = 60;
    else if (button == m_overdue_days_ninety)
        m_overdue_days = 90;
}

void
GncFilterInvoicesDialogInternal::set_paid (bool status)
{
    m_show_paid = status;

    g_return_if_fail (m_paid_toggle);

    gtk_toggle_button_set_active(m_paid_toggle, m_show_paid);
}

void
GncFilterInvoicesDialogInternal::set_unpaid (bool status)
{
    m_show_unpaid = status;

    g_return_if_fail (m_unpaid_toggle);

    gtk_toggle_button_set_active(m_unpaid_toggle, m_show_unpaid);
}

void
GncFilterInvoicesDialogInternal::set_posted (bool status)
{
    m_show_posted = status;

    g_return_if_fail (m_posted_toggle);

    gtk_toggle_button_set_active(m_posted_toggle, m_show_posted);
}

void
GncFilterInvoicesDialogInternal::set_unposted (bool status)
{
    m_show_unposted = status;

    g_return_if_fail (m_unposted_toggle);

    gtk_toggle_button_set_active(m_unposted_toggle, m_show_unposted);
}

void
GncFilterInvoicesDialogInternal::set_invoices (bool status)
{
    m_show_invoices = status;

    g_return_if_fail (m_invoices_toggle);

    gtk_toggle_button_set_active(m_invoices_toggle, m_show_invoices);
}

void
GncFilterInvoicesDialogInternal::set_creditnotes (bool status)
{
    m_show_creditnotes = status;

    g_return_if_fail (m_creditnotes_toggle);

    gtk_toggle_button_set_active(m_creditnotes_toggle, m_show_creditnotes);
}

void
GncFilterInvoicesDialogInternal::set_search_term (const char *term)
{
    m_search_term = term;

    g_return_if_fail (m_search_entry);

    gtk_entry_set_text(m_search_entry, m_search_term.c_str());
}

void
GncFilterInvoicesDialogInternal::set_search_term ()
{
    g_return_if_fail (m_search_entry);

    const gchar *text = gtk_entry_get_text(m_search_entry);

    set_search_term (text);
}

void
GncFilterInvoicesDialogInternal::set_use_start_date (bool status)
{
    m_use_start_date = status;

    g_return_if_fail (m_start_toggle && m_start_picker);

    gtk_toggle_button_set_active(m_start_toggle, m_use_start_date);

    gtk_widget_set_sensitive (GTK_WIDGET (m_start_picker),
                              m_use_start_date);
}

void
GncFilterInvoicesDialogInternal::set_use_end_date (bool status)
{
    m_use_end_date = status;

    g_return_if_fail (m_end_toggle && m_end_picker);

    gtk_toggle_button_set_active(m_end_toggle, m_use_end_date);

    gtk_widget_set_sensitive (GTK_WIDGET (m_end_picker), m_use_end_date);
}

void
GncFilterInvoicesDialogInternal::set_dates (bool init)
{
    g_return_if_fail (m_start_picker && m_end_picker);

    if (init && m_start_date != 0)
        gnc_date_edit_set_time(m_start_picker, m_start_date);
    else
        m_start_date = gnc_date_edit_get_date(m_start_picker);

    if (init && m_end_date != 0)
        gnc_date_edit_set_time(m_end_picker, m_end_date);
    else
        m_end_date = gnc_date_edit_get_date_end(m_end_picker);
}

void
GncFilterInvoicesDialogInternal::reset_dates ()
{
    g_return_if_fail (m_start_picker && m_end_picker);

    gnc_date_edit_set_time(m_start_picker, time (NULL));
    gnc_date_edit_set_time(m_end_picker, time (NULL));

    set_dates(false);
}

QofQuery *
GncFilterInvoicesDialogInternal::create_qof_query (bool empty)
{
    auto *book = gnc_get_current_book ();

    auto *query = qof_query_create_for (GNC_ID_INVOICE);
    qof_query_set_book (query, book);

    // If false, we dont apply any more filters, because base filters are
    // configured in a way that returns no results.
    if (!query_filter_base (query, empty))
        return query;

    query_filter_active (query);

    if (m_preset == Preset::OVERDUE)
    {
        query_filter_overdue (query);
    }
    else if (m_preset == Preset::CUSTOM)
    {
        if (m_show_paid != m_show_unpaid || !m_show_paid)
            query_filter_custom_paid_unpaid (query);

        if (m_show_posted != m_show_unposted || !m_show_posted)
            query_filter_custom_posted_unposted (query);

        if (m_use_start_date && m_start_date != 0)
            query_filter_custom_start (query);

        if (m_use_end_date && m_end_date != 0)
            query_filter_custom_end (query);
    }

    // Add search term to filter if not empty.
    if (m_search_term.length ())
        query_filter_search_term (query);

    return query;
}

bool
GncFilterInvoicesDialogInternal::setup_ui_elements (GtkBuilder *builder)
{
    g_return_val_if_fail (setup_ui_presets (builder) &&
                          setup_ui_overdue (builder) &&
                          setup_ui_custom_toggles (builder) &&
                          setup_ui_custom_start_end_date (builder) &&
                          setup_ui_search (builder) &&
                          setup_ui_buttons (builder), false);

    return true;
}

bool
GncFilterInvoicesDialogInternal::setup_ui_presets (GtkBuilder *builder)
{
    m_custom_toggle =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "preset-custom"));
    m_overdue_toggle =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "preset-overdue"));
    m_custom_controls =
        GTK_WIDGET (gtk_builder_get_object(builder, "custom-controls"));
    m_overdue_controls =
        GTK_WIDGET (gtk_builder_get_object(builder, "overdue-controls"));

    g_return_val_if_fail (m_custom_toggle && m_overdue_toggle &&
                          m_custom_controls && m_overdue_controls, false);

    if (m_owner_type == GncFilterInvoicesDialog::TYPE_VENDOR)
        gtk_button_set_label(GTK_BUTTON (m_overdue_toggle), _("Overdue Bills"));
    else if (m_owner_type == GncFilterInvoicesDialog::TYPE_EMPLOYEE)
        gtk_button_set_label(GTK_BUTTON (m_overdue_toggle),
                             _("Overdue Expense Vouchers"));

    g_signal_connect(m_custom_toggle, "toggled",
                     G_CALLBACK(action_custom_toggle), this);

    g_signal_connect(m_overdue_toggle, "toggled",
                     G_CALLBACK(action_overdue_toggle), this);

    set_preset (m_preset);

    return true;
}

bool
GncFilterInvoicesDialogInternal::setup_ui_overdue (GtkBuilder *builder)
{
    m_overdue_days_zero =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-0"));
    m_overdue_days_seven =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-7"));
    m_overdue_days_fourteen =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-14"));
    m_overdue_days_thirty =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-30"));
    m_overdue_days_sixty =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-60"));
    m_overdue_days_ninety =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-90"));

    g_return_val_if_fail (m_overdue_days_zero && m_overdue_days_seven && 
                          m_overdue_days_fourteen && m_overdue_days_thirty && 
                          m_overdue_days_sixty && m_overdue_days_ninety, false);

    GtkToggleButton *toggles[] = {m_overdue_days_zero, m_overdue_days_seven,
                                  m_overdue_days_fourteen,
                                  m_overdue_days_thirty, m_overdue_days_sixty,
                                  m_overdue_days_ninety};

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
        PWARN ("Invalid m_overdue_days value");
        break;
    }

    for (auto &toggle : toggles)
    {
        g_signal_connect(toggle, "toggled",
                         G_CALLBACK(action_overdue_days_toggle), this);
    }

    return true;
}

bool
GncFilterInvoicesDialogInternal::setup_ui_custom_toggles (GtkBuilder *builder)
{
    m_paid_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object (builder,
                                                               "show-paid"));
    m_unpaid_toggle =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object (builder, "show-unpaid"));
    m_posted_toggle =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object (builder, "show-posted"));
    m_unposted_toggle =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object (builder, "show-unposted"));
    m_invoices_toggle =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object (builder, "show-invoices"));
    m_creditnotes_toggle =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object (builder,
                                                   "show-creditnotes"));
    auto *apply_button =
        GTK_BUTTON (gtk_builder_get_object(builder, "apply-dates"));

    g_return_val_if_fail (m_paid_toggle && m_unpaid_toggle &&
                          m_posted_toggle && m_unposted_toggle &&
                          m_invoices_toggle && m_creditnotes_toggle &&
                          apply_button, false); 

    set_paid (m_show_paid);
    set_unpaid (m_show_unpaid);
    set_posted (m_show_posted);
    set_unposted (m_show_unposted);
    set_invoices (m_show_invoices);
    set_creditnotes (m_show_creditnotes);

    g_signal_connect (m_paid_toggle, "toggled",
                      G_CALLBACK (action_paid_toggle), this);
    g_signal_connect (m_unpaid_toggle, "toggled",
                      G_CALLBACK (action_unpaid_toggle), this);
    g_signal_connect (m_posted_toggle, "toggled",
                      G_CALLBACK (action_posted_toggle), this);
    g_signal_connect (m_unposted_toggle, "toggled",
                      G_CALLBACK (action_unposted_toggle), this);
    g_signal_connect (m_invoices_toggle, "toggled",
                      G_CALLBACK (action_invoices_toggle), this);
    g_signal_connect (m_creditnotes_toggle, "toggled",
                      G_CALLBACK (action_creditnotes_toggle), this);
    g_signal_connect (apply_button, "clicked",
                      G_CALLBACK (action_apply_dates), this);

    if (m_owner_type == GncFilterInvoicesDialog::TYPE_VENDOR)
        gtk_button_set_label (GTK_BUTTON (m_invoices_toggle), _("Show Bills"));
    else if (m_owner_type == GncFilterInvoicesDialog::TYPE_EMPLOYEE)
        gtk_button_set_label (GTK_BUTTON (m_invoices_toggle),
                              _("Show Expense Vouchers"));

    return true;
}

bool
GncFilterInvoicesDialogInternal::setup_ui_custom_start_end_date (
    GtkBuilder *builder)
{
    GtkWidget* start_box =
        GTK_WIDGET (gtk_builder_get_object (builder, "start_date_box"));
    GtkWidget* end_box =
        GTK_WIDGET (gtk_builder_get_object (builder, "end_date_box"));

    m_start_toggle =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "filter-by-start"));
    m_end_toggle =
        GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "filter-by-end"));

    m_start_picker =
        GNC_DATE_EDIT (gnc_date_edit_new (time (NULL), FALSE, TRUE));
    m_end_picker = GNC_DATE_EDIT (gnc_date_edit_new (time (NULL), FALSE, TRUE));

    g_return_val_if_fail (start_box && m_start_toggle && m_start_picker &&
                          end_box && m_end_toggle && m_end_picker, false);

    gtk_box_pack_start (GTK_BOX (start_box),
                        GTK_WIDGET (m_start_picker), TRUE, TRUE, 0);
    gtk_box_pack_start (GTK_BOX (end_box),
                        GTK_WIDGET (m_end_picker), TRUE, TRUE, 0);

    g_signal_connect (m_start_picker->date_entry, "activate",
                      G_CALLBACK (action_start_picker_activate), this);
    g_signal_connect (m_start_toggle, "toggled",
                      G_CALLBACK (action_start_toggle), this);
    g_signal_connect (m_end_picker->date_entry, "activate",
                      G_CALLBACK (action_end_picker_activate), this);
    g_signal_connect (m_end_toggle, "toggled",
                      G_CALLBACK (action_end_toggle), this);

    set_use_start_date (m_use_start_date);
    set_use_end_date (m_use_end_date);
    set_dates (true);

    gtk_widget_show_all (start_box);
    gtk_widget_show_all (end_box);

    return true;
}

bool
GncFilterInvoicesDialogInternal::setup_ui_search (GtkBuilder *builder)
{
    m_search_entry = GTK_ENTRY (gtk_builder_get_object (builder, "search"));
    auto *apply_button = GTK_BUTTON (gtk_builder_get_object (builder,
                                                             "apply-search"));

    g_return_val_if_fail (m_search_entry && apply_button, false);

    if (m_owner_type == GncFilterInvoicesDialog::TYPE_VENDOR)
        gtk_entry_set_placeholder_text (m_search_entry,
                                        "Type to search for Bills...");
    else if (m_owner_type == GncFilterInvoicesDialog::TYPE_EMPLOYEE)
        gtk_entry_set_placeholder_text (
            m_search_entry,
            "Type to search for Expense Vouchers...");

    if (m_search_term.length())
        set_search_term (m_search_term.c_str());

    g_signal_connect (m_search_entry, "activate",
                      G_CALLBACK(action_search_entry_activate), this);

    g_signal_connect (apply_button, "clicked",
                      G_CALLBACK(action_apply_search_entry), this);

    return true;
}

bool
GncFilterInvoicesDialogInternal::setup_ui_buttons (GtkBuilder *builder)
{
    auto *reset_button = GTK_BUTTON (gtk_builder_get_object (builder, "reset"));
    auto *close_button = GTK_BUTTON (gtk_builder_get_object (builder, "close"));

    g_return_val_if_fail (reset_button && close_button, false);

    g_signal_connect (reset_button, "clicked",
                      G_CALLBACK (action_reset), this);

    g_signal_connect_swapped (close_button, "clicked",
                             G_CALLBACK (gtk_widget_hide), m_dialog);

    return true;
}

void
GncFilterInvoicesDialogInternal::dialog_open ()
{
    g_return_if_fail (m_dialog && m_dialog_initialized);

    gtk_widget_show (m_dialog);
}

void
GncFilterInvoicesDialogInternal::dialog_close ()
{
    if (m_dialog)
        gtk_widget_hide (m_dialog);
}

void
GncFilterInvoicesDialogInternal::dialog_create ()
{
    g_return_if_fail (m_plugin_page);
    g_return_if_fail (m_apply_filter);

    ENTER ("(fd %p, page %p)", m_dialog, m_plugin_page);

    if (m_dialog)
    {
        gtk_window_present (GTK_WINDOW (m_dialog));
        LEAVE ("existing m_dialog");
        return;
    }

    auto *builder = gtk_builder_new ();
    bool add_from_file =
        gnc_builder_add_from_file (builder, UI_FILE, "invoices-filters");

    if (!add_from_file)
    {
        g_object_unref (G_OBJECT(builder));
        PERR("Failed to build filter dialog: Could not open file '%s'",
             UI_FILE);

        return;
    }

    m_dialog =
        GTK_WIDGET (gtk_builder_get_object (builder, "invoices-filters"));

    if (!m_dialog) {
        g_object_unref (G_OBJECT (builder));
        PERR("Failed to build filter dialog: Could not get object 'invoices-filters'");

        return;
    }

    if (!setup_ui_elements (builder)) {
        g_object_unref (G_OBJECT (builder));
        PERR("Failed to build filter dialog: Could not set up ui-elements");

        return;
    }

    gtk_window_set_transient_for(
        GTK_WINDOW (m_dialog),
        GTK_WINDOW (gnc_plugin_page_get_window (m_plugin_page)));

    /* Translators: The %s is the name of the plugin page */
    gchar *title = g_strdup_printf(
        _("Filter %s by…"),
        gnc_plugin_page_get_page_name (m_plugin_page));

    gtk_window_set_title(GTK_WINDOW(m_dialog), title);

    g_free(title);

    gtk_widget_hide (m_dialog);

    g_signal_connect(m_dialog, "delete-event",
                     G_CALLBACK(gtk_widget_hide_on_delete), this);

    g_object_unref(G_OBJECT(builder));

    m_dialog_initialized = true;

    LEAVE("");
}

bool
GncFilterInvoicesDialogInternal::query_filter_base (QofQuery *query, bool empty)
{
    // Base-filter to show invoices/creditnotes for
    // either customers, vendors or employees.

    bool filter_empty = empty ||
                        (m_preset == Preset::CUSTOM &&
                         (!m_show_invoices && !m_show_creditnotes));
    bool show_inv = m_show_invoices || m_preset != Preset::CUSTOM;
    bool show_cred = m_show_creditnotes || m_preset != Preset::CUSTOM;

    if (show_inv || filter_empty)
    {
        GncInvoiceType inv_type;

        switch (m_owner_type)
        {
          case GncFilterInvoicesDialog::TYPE_CUSTOMER:
            inv_type = GNC_INVOICE_CUST_INVOICE;
            break;
          case GncFilterInvoicesDialog::TYPE_VENDOR:
            inv_type = GNC_INVOICE_VEND_INVOICE;
            break;
          case GncFilterInvoicesDialog::TYPE_EMPLOYEE:
            inv_type = GNC_INVOICE_EMPL_INVOICE;
            break;
        }

        qof_query_add_term (query,
                            qof_query_build_param_list (INVOICE_TYPE, nullptr),
                            qof_query_int32_predicate (QOF_COMPARE_EQUAL,
                                                       inv_type),
                            QOF_QUERY_AND);
    }

    if (show_cred || filter_empty)
    {
        GncInvoiceType credit_type;

        switch (m_owner_type)
        {
        case GncFilterInvoicesDialog::TYPE_CUSTOMER:
            credit_type = GNC_INVOICE_CUST_CREDIT_NOTE;
            break;
        case GncFilterInvoicesDialog::TYPE_VENDOR:
            credit_type = GNC_INVOICE_VEND_CREDIT_NOTE;
            break;
        case GncFilterInvoicesDialog::TYPE_EMPLOYEE:
            credit_type = GNC_INVOICE_EMPL_CREDIT_NOTE;
            break;
        }

        qof_query_add_term (query,
                            qof_query_build_param_list (INVOICE_TYPE, nullptr),
                            qof_query_int32_predicate (QOF_COMPARE_EQUAL,
                                                       credit_type),
                            filter_empty || !show_inv ? QOF_QUERY_AND :
                                                        QOF_QUERY_OR);
    }

    // Return false to indicate that filters will purposedly return zero
    // results.
    if (filter_empty)
        return false;

    return true;
}

void
GncFilterInvoicesDialogInternal::query_filter_active (QofQuery *query)
{
    // Filter only active invoices.

    auto *book = gnc_get_current_book ();
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

void
GncFilterInvoicesDialogInternal::query_filter_overdue (QofQuery *query)
{
    auto *book = gnc_get_current_book ();
    QofQuery *query_overdue = qof_query_create_for (GNC_ID_INVOICE);
    qof_query_set_book (query_overdue, book);

    qof_query_add_boolean_match(query_overdue,
                                qof_query_build_param_list (INVOICE_IS_PAID,
                                                            nullptr),
                                false, QOF_QUERY_AND);

    qof_query_add_boolean_match(query_overdue,
                                qof_query_build_param_list (INVOICE_IS_POSTED,
                                                            nullptr),
                                true, QOF_QUERY_AND);

    GDate date;
    gnc_gdate_set_today (&date);

    if (m_overdue_days)
        g_date_subtract_days (&date, m_overdue_days);

    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_DUE,
                                                               nullptr);
    QofQueryPredData *pred_end =
        qof_query_date_predicate(QOF_COMPARE_LT, QOF_DATE_MATCH_NORMAL,
                                 gnc_time64_get_day_end_gdate(&date));

    qof_query_add_term(query_overdue, type_path, pred_end, QOF_QUERY_AND);

    qof_query_merge_in_place (query, query_overdue, QOF_QUERY_AND); 
    qof_query_destroy (query_overdue);
}

void
GncFilterInvoicesDialogInternal::query_filter_custom_paid_unpaid (
    QofQuery *query)
{
    auto *book = gnc_get_current_book ();
    QofQuery *query_paid = qof_query_create_for (GNC_ID_INVOICE);
    qof_query_set_book (query_paid, book);

    qof_query_add_boolean_match(query_paid,
                                qof_query_build_param_list (INVOICE_IS_PAID,
                                                            nullptr),
                                m_show_paid, QOF_QUERY_AND);

    // If both options are false, add a TRUE match so no results are shown.
    if (!m_show_paid && !m_show_unpaid)
        qof_query_add_boolean_match(query_paid,
                                    qof_query_build_param_list (INVOICE_IS_PAID,
                                                                nullptr),
                                    true, QOF_QUERY_AND);

    qof_query_merge_in_place (query, query_paid, QOF_QUERY_AND); 
    qof_query_destroy (query_paid);
}

void
GncFilterInvoicesDialogInternal::query_filter_custom_posted_unposted (
    QofQuery *query)
{
    auto *book = gnc_get_current_book ();
    QofQuery *query_posted = qof_query_create_for (GNC_ID_INVOICE);
    qof_query_set_book (query_posted, book);

    qof_query_add_boolean_match(query_posted,
                                qof_query_build_param_list (INVOICE_IS_POSTED,
                                                            nullptr),
                                m_show_posted, QOF_QUERY_AND);

    // If both options are false, add a TRUE match so no results are shown.
    if (!m_show_posted && !m_show_unposted)
        qof_query_add_boolean_match(query_posted,
                                    qof_query_build_param_list (
                                        INVOICE_IS_POSTED, nullptr),
                                    true,
                                    QOF_QUERY_AND);

    qof_query_merge_in_place (query, query_posted, QOF_QUERY_AND); 
    qof_query_destroy (query_posted);
}

QofQuery *
GncFilterInvoicesDialogInternal::query_filter_date_compare (bool is_posted,
                                                            bool is_less,
                                                            time64 compare_date)
{
    auto *book = gnc_get_current_book ();
    QofQuery *query_compare = qof_query_create_for (GNC_ID_INVOICE);
    qof_query_set_book (query_compare, book);

    qof_query_add_boolean_match(
        query_compare,
        qof_query_build_param_list (INVOICE_IS_POSTED, nullptr),
        is_posted, QOF_QUERY_AND);

    qof_query_add_term (query_compare,
                        qof_query_build_param_list (
                            is_posted ? INVOICE_POSTED : INVOICE_OPENED,
                            nullptr),
                        qof_query_date_predicate (
                            is_less ? QOF_COMPARE_LTE : QOF_COMPARE_GTE,
                            QOF_DATE_MATCH_NORMAL,
                            compare_date),
                        QOF_QUERY_AND);

    return query_compare;
}

void
GncFilterInvoicesDialogInternal::query_filter_custom_start (QofQuery *query)
{
    auto *book = gnc_get_current_book ();
    QofQuery *query_start = qof_query_create_for (GNC_ID_INVOICE);
    qof_query_set_book (query_start, book);

    // If invoice IS posted, and posted_date is greater than or equal to the
    // chosen start date.
    auto *query_posted = query_filter_date_compare (
        true, // INVOICE_IS_POSTED = true 
        false, // INVOICE_POSTED greater than or equal to: 
        m_start_date);

    qof_query_merge_in_place (query_start, query_posted, QOF_QUERY_AND); 
    qof_query_destroy (query_posted);

    // If invoice IS NOT posted, and  opened_at is greater than or equal to
    // the chosen start date.
    auto *query_unposted = query_filter_date_compare (
        false, // INVOICE_IS_POSTED = false 
        false, // INVOICE_POSTED greater than or equal to: 
        m_start_date);

    qof_query_merge_in_place (query_start, query_unposted, QOF_QUERY_OR); 
    qof_query_destroy (query_unposted);

    qof_query_merge_in_place (query, query_start, QOF_QUERY_AND); 
    qof_query_destroy (query_start);
}

void
GncFilterInvoicesDialogInternal::query_filter_custom_end (QofQuery *query)
{
    auto *book = gnc_get_current_book ();
    QofQuery *query_end = qof_query_create_for (GNC_ID_INVOICE);
    qof_query_set_book (query_end, book);

    // If invoice IS posted, and posted_date is less than or equal to the
    // chosen end date.
    auto *query_posted = query_filter_date_compare (
        true, // invoice_is_posted = true 
        true, // invoice_posted less than or equal to: 
        m_end_date);

    qof_query_merge_in_place (query_end, query_posted, QOF_QUERY_AND); 
    qof_query_destroy (query_posted);

    // If invoice IS NOT posted, and opened_at is less than or equal to
    // the chosen end date.
    auto *query_unposted = query_filter_date_compare (
        false, // invoice_is_posted = false
        true, // invoice_posted less than or equal to: 
        m_end_date);

    qof_query_merge_in_place (query_end, query_unposted, QOF_QUERY_OR); 
    qof_query_destroy (query_unposted);

    qof_query_merge_in_place (query, query_end, QOF_QUERY_AND); 
    qof_query_destroy (query_end);
}

void
GncFilterInvoicesDialogInternal::query_filter_search_term (QofQuery *query)
{
    auto *book = gnc_get_current_book ();
    QofQuery *query_term = qof_query_create_for (GNC_ID_INVOICE);
    qof_query_set_book (query_term, book);

    QofQueryParamList *params[4] = {nullptr, nullptr, nullptr, nullptr};

    // Customer name
    params[0] = qof_query_build_param_list (INVOICE_OWNER, OWNER_PARENT,
                                            OWNER_NAME, nullptr);

    // Job name
    params[1] = qof_query_build_param_list (INVOICE_OWNER, OWNER_NAME, nullptr);

    // Invoice id
    params[2] = qof_query_build_param_list (INVOICE_ID, nullptr);

    // Invoice billing id 
    params[3] = qof_query_build_param_list (INVOICE_BILLINGID, nullptr);

    for (auto &param : params) {
        QofQueryPredData *owner_name_pred =
            qof_query_string_predicate (QOF_COMPARE_CONTAINS,
                                        m_search_term.c_str(),
                                        QOF_STRING_MATCH_CASEINSENSITIVE,
                                        false);

        qof_query_add_term (query_term, param, owner_name_pred,
                            param == params[0] ? QOF_QUERY_AND : QOF_QUERY_OR);
    }

    qof_query_merge_in_place (query, query_term, QOF_QUERY_AND); 
    qof_query_destroy (query_term);
}

GncFilterInvoicesDialog::GncFilterInvoicesDialog (
    GncPluginPage &plugin_page, GncFilterInvoicesDialog::OwnerType owner_type,
    std::function<void()> apply_cb
)
{
    if (!apply_cb)
    {
        PERR ("Failed to initialize filter dialog: Invalid apply_cb parameter");
        return;
    }

    m_filter = std::make_unique<GncFilterInvoicesDialogInternal> (
        owner_type, plugin_page, apply_cb); 
}

GncFilterInvoicesDialog::~GncFilterInvoicesDialog() = default;

void
GncFilterInvoicesDialog::dialog_open ()
{
    g_return_if_fail (m_filter != nullptr);

    m_filter->dialog_open ();
}

QofQuery *
GncFilterInvoicesDialog::create_qof_query (bool empty)
{
    g_return_val_if_fail (m_filter != nullptr, nullptr);

    return m_filter->create_qof_query (empty);
}
