/*
 * dialog-sie4-export.c -- SIE4 export dialog
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "dialog-sie4-export.h"

#include "gnc-accounting-period.h"
#include "gnc-commodity.h"
#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "qofbook.h"

#include "sie4-export.h"

#define GNC_PREFS_GROUP_SIE4 "dialogs.export.sie4"

#define BOOK_OPTION_COMPANY_NAME "options/Business/Company Name"
#define BOOK_OPTION_COMPANY_ADDRESS "options/Business/Company Address"
#define BOOK_OPTION_COMPANY_CONTACT "options/Business/Company Contact Person"
#define BOOK_OPTION_COMPANY_PHONE "options/Business/Company Phone Number"
#define BOOK_OPTION_COMPANY_ID "options/Business/Company ID"

typedef struct
{
    GtkWidget *company_name;
    GtkWidget *file_id;
    GtkWidget *organization_number;
    GtkWidget *contact;
    GtkWidget *street_address;
    GtkWidget *postal_address;
    GtkWidget *phone;
    GtkWidget *account_plan;
    GtkWidget *currency_code;
    GtkWidget *voucher_series;
    GtkWidget *current_start;
    GtkWidget *current_end;
    GtkWidget *previous_start;
    GtkWidget *previous_end;
    GtkWidget *dimensions;
    GtkWidget *zero_balances;
    GtkWidget *transaction_numbers;
} Sie4DialogWidgets;

typedef struct
{
    gboolean initialized;
    gchar *file_name;
    gchar *company_name;
    gchar *file_id;
    gchar *organization_number;
    gchar *contact;
    gchar *street_address;
    gchar *postal_address;
    gchar *phone;
    gchar *account_plan;
    gchar *currency_code;
    gchar *voucher_series;
    time64 current_start;
    time64 current_end;
    time64 previous_start;
    time64 previous_end;
    gboolean include_business_dimensions;
    gboolean include_zero_balances;
    gboolean use_transaction_numbers;
} Sie4DialogState;

enum
{
    RESPONSE_RESET_DEFAULTS = 1
};

static Sie4DialogState previous_dialog_state = { 0 };

static const gchar *
safe_text (const gchar *text)
{
    return text ? text : "";
}

static const gchar *
entry_text (GtkWidget *entry)
{
    return safe_text (gtk_entry_get_text (GTK_ENTRY (entry)));
}

static const gchar *
default_company_name (QofBook *book)
{
    const gchar *tax_name = gnc_get_current_book_tax_name ();

    if (tax_name && *tax_name)
        return tax_name;

    return qof_book_get_string_option (book, BOOK_OPTION_COMPANY_NAME);
}

static GtkWidget *
add_label (GtkGrid *grid, gint row, const gchar *text)
{
    GtkWidget *label = gtk_label_new_with_mnemonic (text);

    gtk_widget_set_halign (label, GTK_ALIGN_START);
    gtk_grid_attach (grid, label, 0, row, 1, 1);

    return label;
}

static GtkWidget *
add_entry (GtkGrid *grid, gint *row, const gchar *label_text, const gchar *value)
{
    GtkWidget *entry = gtk_entry_new ();
    GtkWidget *label = add_label (grid, *row, label_text);

    gtk_label_set_mnemonic_widget (GTK_LABEL (label), entry);
    gtk_entry_set_text (GTK_ENTRY (entry), safe_text (value));
    gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);
    gtk_widget_set_hexpand (entry, TRUE);
    gtk_grid_attach (grid, entry, 1, *row, 1, 1);
    (*row)++;

    return entry;
}

static GtkWidget *
add_date_edit (GtkGrid *grid, gint *row, const gchar *label_text, time64 value)
{
    GtkWidget *date = gnc_date_edit_new (value, FALSE, FALSE);
    GtkWidget *label = add_label (grid, *row, label_text);

    gtk_label_set_mnemonic_widget (GTK_LABEL (label), date);
    gtk_widget_set_hexpand (date, TRUE);
    gtk_grid_attach (grid, date, 1, *row, 1, 1);
    (*row)++;

    return date;
}

static gchar **
split_address (const gchar *address)
{
    return g_strsplit (safe_text (address), "\n", 4);
}

static gboolean
valid_account_plan (const gchar *account_plan)
{
    if (!account_plan || !*account_plan)
        return TRUE;

    return g_ascii_strcasecmp (account_plan, "BAS95") == 0 ||
           g_ascii_strcasecmp (account_plan, "BAS96") == 0 ||
           g_ascii_strcasecmp (account_plan, "EUBAS97") == 0 ||
           g_ascii_strcasecmp (account_plan, "NE2007") == 0 ||
           g_ascii_strncasecmp (account_plan, "BAS2", 4) == 0;
}

static gboolean
valid_currency_code (const gchar *currency_code)
{
    if (!currency_code || !*currency_code)
        return TRUE;

    return strlen (currency_code) == 3 &&
           g_ascii_isalpha (currency_code[0]) &&
           g_ascii_isalpha (currency_code[1]) &&
           g_ascii_isalpha (currency_code[2]);
}

static gboolean
valid_voucher_series (const gchar *voucher_series)
{
    if (!voucher_series || !*voucher_series)
        return FALSE;

    for (const gchar *p = voucher_series; *p; p++)
    {
        if (g_ascii_isspace (*p) || *p == '"')
            return FALSE;
    }

    return TRUE;
}

static const gchar *
default_currency_code (void)
{
    gnc_commodity *currency = gnc_default_currency ();

    return currency ? gnc_commodity_get_mnemonic (currency) : "SEK";
}

static time64
default_current_start (void)
{
    time64 start = gnc_accounting_period_fiscal_start ();
    time64 end = gnc_accounting_period_fiscal_end ();

    /* Prefer the configured accounting period; fall back to the current
     * calendar year if preferences are unset or inconsistent. */
    if (start <= 0 || end <= 0 || start > end)
    {
        GDate today;
        gnc_gdate_set_today (&today);
        return gnc_dmy2time64 (1, 1, g_date_get_year (&today));
    }

    return gnc_time64_get_day_start (start);
}

static time64
default_current_end (void)
{
    time64 start = gnc_accounting_period_fiscal_start ();
    time64 end = gnc_accounting_period_fiscal_end ();

    if (start <= 0 || end <= 0 || start > end)
    {
        GDate today;
        gnc_gdate_set_today (&today);
        return gnc_dmy2time64_end (31, 12, g_date_get_year (&today));
    }

    return gnc_time64_get_day_end (end);
}

static time64
shift_year_start (time64 value, guint years)
{
    GDate date = time64_to_gdate (value);
    g_date_subtract_years (&date, years);
    return gnc_time64_get_day_start (gdate_to_time64 (date));
}

static time64
shift_year_end (time64 value, guint years)
{
    GDate date = time64_to_gdate (value);
    g_date_subtract_years (&date, years);
    return gnc_time64_get_day_end (gdate_to_time64 (date));
}

static gchar *
default_filename (const gchar *company_name, time64 current_end)
{
    GDate date = time64_to_gdate (current_end);
    gchar *base = g_strdup (company_name && *company_name ? company_name : "gnucash");

    g_strstrip (base);
    for (gchar *p = base; *p; p++)
    {
        if (g_ascii_isspace (*p))
            *p = '_';
        else if (*p == G_DIR_SEPARATOR || *p == '/' || *p == '\\')
            *p = '-';
    }

    gchar *filename = g_strdup_printf ("%s_%04u.SE", base, g_date_get_year (&date));
    g_free (base);
    return filename;
}

static GtkWidget *
create_extra_widget (Sie4DialogWidgets *widgets)
{
    QofBook *book = gnc_get_current_book ();
    const gchar *company_name = default_company_name (book);
    const gchar *company_id = qof_book_get_string_option (book, BOOK_OPTION_COMPANY_ID);
    const gchar *contact = qof_book_get_string_option (book, BOOK_OPTION_COMPANY_CONTACT);
    const gchar *phone = qof_book_get_string_option (book, BOOK_OPTION_COMPANY_PHONE);
    gchar **address_lines = split_address (qof_book_get_string_option (book, BOOK_OPTION_COMPANY_ADDRESS));
    gchar *postal_address = g_strjoinv (" ", address_lines + 1);
    time64 current_start = default_current_start ();
    time64 current_end = default_current_end ();
    gint row = 0;

    GtkWidget *box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 6);
    GtkWidget *grid = gtk_grid_new ();
    GtkWidget *options_label = gtk_label_new (NULL);

    gtk_label_set_markup (GTK_LABEL (options_label),
                          _("<b>SIE4 export options</b>"));
    gtk_widget_set_halign (options_label, GTK_ALIGN_START);
    gtk_box_pack_start (GTK_BOX (box), options_label, FALSE, FALSE, 0);

    gtk_grid_set_row_spacing (GTK_GRID (grid), 6);
    gtk_grid_set_column_spacing (GTK_GRID (grid), 12);
    gtk_box_pack_start (GTK_BOX (box), grid, FALSE, FALSE, 0);

    widgets->company_name = add_entry (GTK_GRID (grid), &row, _("_Company name:"), company_name);
    widgets->file_id = add_entry (GTK_GRID (grid), &row, _("File/company _number:"), "");
    widgets->organization_number = add_entry (GTK_GRID (grid), &row, _("_Organization number:"), company_id);
    widgets->contact = add_entry (GTK_GRID (grid), &row, _("_Contact:"), contact);
    widgets->street_address = add_entry (GTK_GRID (grid), &row, _("_Street address:"), address_lines[0]);
    widgets->postal_address = add_entry (GTK_GRID (grid), &row, _("_Postal address:"), postal_address);
    widgets->phone = add_entry (GTK_GRID (grid), &row, _("_Phone:"), phone);
    widgets->account_plan = add_entry (GTK_GRID (grid), &row, _("Account _plan:"), "EUBAS97");
    widgets->currency_code = add_entry (GTK_GRID (grid), &row, _("Accounting _currency:"), default_currency_code ());
    widgets->voucher_series = add_entry (GTK_GRID (grid), &row, _("_Voucher series:"), "A");

    widgets->current_start = add_date_edit (GTK_GRID (grid), &row, _("Current year _start:"), current_start);
    widgets->current_end = add_date_edit (GTK_GRID (grid), &row, _("Current year _end:"), current_end);
    widgets->previous_start = add_date_edit (GTK_GRID (grid), &row, _("Previous year s_tart:"), shift_year_start (current_start, 1));
    widgets->previous_end = add_date_edit (GTK_GRID (grid), &row, _("Previous year e_nd:"), shift_year_end (current_end, 1));

    widgets->dimensions = gtk_check_button_new_with_mnemonic (_("Include customer, vendor and invoice _dimensions"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widgets->dimensions), TRUE);
    gtk_grid_attach (GTK_GRID (grid), widgets->dimensions, 0, row, 2, 1);
    row++;

    widgets->zero_balances = gtk_check_button_new_with_mnemonic (_("Include accounts with _zero values"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widgets->zero_balances), FALSE);
    gtk_grid_attach (GTK_GRID (grid), widgets->zero_balances, 0, row, 2, 1);
    row++;

    widgets->transaction_numbers =
        gtk_check_button_new_with_mnemonic (_("Use GnuCash transaction _numbers as voucher numbers"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widgets->transaction_numbers), TRUE);
    gtk_grid_attach (GTK_GRID (grid), widgets->transaction_numbers, 0, row, 2, 1);

    g_free (postal_address);
    g_strfreev (address_lines);
    return box;
}

static void
clear_dialog_state (Sie4DialogState *state)
{
    g_free (state->file_name);
    g_free (state->company_name);
    g_free (state->file_id);
    g_free (state->organization_number);
    g_free (state->contact);
    g_free (state->street_address);
    g_free (state->postal_address);
    g_free (state->phone);
    g_free (state->account_plan);
    g_free (state->currency_code);
    g_free (state->voucher_series);
    memset (state, 0, sizeof (*state));
}

static void
state_set_string (gchar **target, const gchar *value)
{
    g_free (*target);
    *target = g_strdup (safe_text (value));
}

static void
populate_default_dialog_state (Sie4DialogState *state)
{
    QofBook *book = gnc_get_current_book ();
    const gchar *company_name = default_company_name (book);
    const gchar *company_id = qof_book_get_string_option (book, BOOK_OPTION_COMPANY_ID);
    const gchar *contact = qof_book_get_string_option (book, BOOK_OPTION_COMPANY_CONTACT);
    const gchar *phone = qof_book_get_string_option (book, BOOK_OPTION_COMPANY_PHONE);
    gchar **address_lines = split_address (qof_book_get_string_option (book, BOOK_OPTION_COMPANY_ADDRESS));
    gchar *postal_address = g_strjoinv (" ", address_lines + 1);

    clear_dialog_state (state);
    state->initialized = TRUE;
    state_set_string (&state->company_name, company_name);
    state_set_string (&state->file_id, "");
    state_set_string (&state->organization_number, company_id);
    state_set_string (&state->contact, contact);
    state_set_string (&state->street_address, address_lines[0]);
    state_set_string (&state->postal_address, postal_address);
    state_set_string (&state->phone, phone);
    state_set_string (&state->account_plan, "EUBAS97");
    state_set_string (&state->currency_code, default_currency_code ());
    state_set_string (&state->voucher_series, "A");
    state->current_start = default_current_start ();
    state->current_end = default_current_end ();
    state->previous_start = shift_year_start (state->current_start, 1);
    state->previous_end = shift_year_end (state->current_end, 1);
    state->include_business_dimensions = TRUE;
    state->include_zero_balances = FALSE;
    state->use_transaction_numbers = TRUE;
    state->file_name = default_filename (state->company_name, state->current_end);

    g_free (postal_address);
    g_strfreev (address_lines);
}

static void
apply_dialog_state (GtkFileChooser *chooser,
                    Sie4DialogWidgets *widgets,
                    const Sie4DialogState *state)
{
    gtk_entry_set_text (GTK_ENTRY (widgets->company_name), safe_text (state->company_name));
    gtk_entry_set_text (GTK_ENTRY (widgets->file_id), safe_text (state->file_id));
    gtk_entry_set_text (GTK_ENTRY (widgets->organization_number), safe_text (state->organization_number));
    gtk_entry_set_text (GTK_ENTRY (widgets->contact), safe_text (state->contact));
    gtk_entry_set_text (GTK_ENTRY (widgets->street_address), safe_text (state->street_address));
    gtk_entry_set_text (GTK_ENTRY (widgets->postal_address), safe_text (state->postal_address));
    gtk_entry_set_text (GTK_ENTRY (widgets->phone), safe_text (state->phone));
    gtk_entry_set_text (GTK_ENTRY (widgets->account_plan), safe_text (state->account_plan));
    gtk_entry_set_text (GTK_ENTRY (widgets->currency_code), safe_text (state->currency_code));
    gtk_entry_set_text (GTK_ENTRY (widgets->voucher_series), safe_text (state->voucher_series));
    gnc_date_edit_set_time (GNC_DATE_EDIT (widgets->current_start), state->current_start);
    gnc_date_edit_set_time (GNC_DATE_EDIT (widgets->current_end), state->current_end);
    gnc_date_edit_set_time (GNC_DATE_EDIT (widgets->previous_start), state->previous_start);
    gnc_date_edit_set_time (GNC_DATE_EDIT (widgets->previous_end), state->previous_end);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widgets->dimensions),
                                  state->include_business_dimensions);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widgets->zero_balances),
                                  state->include_zero_balances);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widgets->transaction_numbers),
                                  state->use_transaction_numbers);

    if (state->file_name && *state->file_name)
    {
        if (g_path_is_absolute (state->file_name))
            gtk_file_chooser_set_filename (chooser, state->file_name);
        else
            gtk_file_chooser_set_current_name (chooser, state->file_name);
    }
}

static void
restore_default_dialog_state (GtkFileChooser *chooser,
                              Sie4DialogWidgets *widgets)
{
    Sie4DialogState defaults = { 0 };
    gchar *starting_dir;

    populate_default_dialog_state (&defaults);

    starting_dir = gnc_get_default_directory (GNC_PREFS_GROUP_SIE4);
    if (starting_dir)
        gtk_file_chooser_set_current_folder (chooser, starting_dir);
    g_free (starting_dir);

    apply_dialog_state (chooser, widgets, &defaults);
    clear_dialog_state (&defaults);
}

static void
capture_dialog_state (GtkFileChooser *chooser,
                      Sie4DialogWidgets *widgets,
                      Sie4DialogState *state)
{
    clear_dialog_state (state);
    state->initialized = TRUE;
    state->file_name = gtk_file_chooser_get_filename (chooser);
    state_set_string (&state->company_name, entry_text (widgets->company_name));
    state_set_string (&state->file_id, entry_text (widgets->file_id));
    state_set_string (&state->organization_number, entry_text (widgets->organization_number));
    state_set_string (&state->contact, entry_text (widgets->contact));
    state_set_string (&state->street_address, entry_text (widgets->street_address));
    state_set_string (&state->postal_address, entry_text (widgets->postal_address));
    state_set_string (&state->phone, entry_text (widgets->phone));
    state_set_string (&state->account_plan, entry_text (widgets->account_plan));
    state_set_string (&state->currency_code, entry_text (widgets->currency_code));
    state_set_string (&state->voucher_series, entry_text (widgets->voucher_series));
    state->current_start = gnc_time64_get_day_start (
        gnc_date_edit_get_date (GNC_DATE_EDIT (widgets->current_start)));
    state->current_end = gnc_time64_get_day_end (
        gnc_date_edit_get_date (GNC_DATE_EDIT (widgets->current_end)));
    state->previous_start = gnc_time64_get_day_start (
        gnc_date_edit_get_date (GNC_DATE_EDIT (widgets->previous_start)));
    state->previous_end = gnc_time64_get_day_end (
        gnc_date_edit_get_date (GNC_DATE_EDIT (widgets->previous_end)));
    state->include_business_dimensions =
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widgets->dimensions));
    state->include_zero_balances =
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widgets->zero_balances));
    state->use_transaction_numbers =
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widgets->transaction_numbers));
}

static gboolean
collect_settings (GtkFileChooser *chooser,
                  Sie4DialogWidgets *widgets,
                  GncSie4ExportSettings *settings,
                  GtkWindow *parent)
{
    settings->file_name = gtk_file_chooser_get_filename (chooser);
    settings->company_name = entry_text (widgets->company_name);
    settings->file_id = entry_text (widgets->file_id);
    settings->organization_number = entry_text (widgets->organization_number);
    settings->contact = entry_text (widgets->contact);
    settings->street_address = entry_text (widgets->street_address);
    settings->postal_address = entry_text (widgets->postal_address);
    settings->phone = entry_text (widgets->phone);
    settings->account_plan = entry_text (widgets->account_plan);
    settings->currency_code = entry_text (widgets->currency_code);
    settings->voucher_series = entry_text (widgets->voucher_series);
    settings->current_start = gnc_time64_get_day_start (
        gnc_date_edit_get_date (GNC_DATE_EDIT (widgets->current_start)));
    settings->current_end = gnc_time64_get_day_end (
        gnc_date_edit_get_date (GNC_DATE_EDIT (widgets->current_end)));
    settings->previous_start = gnc_time64_get_day_start (
        gnc_date_edit_get_date (GNC_DATE_EDIT (widgets->previous_start)));
    settings->previous_end = gnc_time64_get_day_end (
        gnc_date_edit_get_date (GNC_DATE_EDIT (widgets->previous_end)));
    settings->include_business_dimensions =
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widgets->dimensions));
    settings->include_zero_balances =
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widgets->zero_balances));
    settings->use_transaction_numbers =
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widgets->transaction_numbers));

    if (!settings->file_name || !*settings->file_name)
    {
        gnc_error_dialog (parent, "%s", _("Please choose a file for the SIE4 export."));
        return FALSE;
    }

    if (!settings->company_name || !*settings->company_name)
    {
        gnc_error_dialog (parent, "%s", _("Company name is required for SIE4 export."));
        return FALSE;
    }

    if (settings->current_start > settings->current_end)
    {
        gnc_error_dialog (parent, "%s", _("The current financial year start date must be before the end date."));
        return FALSE;
    }

    gchar *voucher_series = g_strdup (safe_text (settings->voucher_series));
    g_strstrip (voucher_series);
    if (!voucher_series || !*voucher_series)
    {
        gnc_error_dialog (parent, "%s", _("Voucher series is required for SIE4 export."));
        g_free (voucher_series);
        return FALSE;
    }
    if (!valid_voucher_series (voucher_series))
    {
        gnc_error_dialog (parent, "%s",
                          _("Voucher series must not contain spaces or quote marks for SIE4 export."));
        g_free (voucher_series);
        return FALSE;
    }
    g_free (voucher_series);

    if (settings->previous_start > settings->previous_end)
    {
        gnc_error_dialog (parent, "%s", _("The previous financial year start date must be before the end date."));
        return FALSE;
    }

    if (!valid_account_plan (settings->account_plan))
    {
        gnc_error_dialog (parent, "%s",
                          _("The SIE account plan must be BAS95, BAS96, EUBAS97, NE2007, or BAS2xxx."));
        return FALSE;
    }

    if (!valid_currency_code (settings->currency_code))
    {
        gnc_error_dialog (parent, "%s",
                          _("The SIE accounting currency must be a three-letter ISO 4217 code."));
        return FALSE;
    }

    return TRUE;
}
static void
gnc_file_sie4_export_internal (GtkWindow *parent)
{
    Sie4DialogWidgets widgets = { 0 };
    GtkWidget *dialog;
    GtkFileFilter *filter;
    GtkWidget *extra_widget;
    gchar *starting_dir;
    gboolean done = FALSE;

    dialog = gtk_file_chooser_dialog_new (_("Export SIE4"),
                                          parent,
                                          GTK_FILE_CHOOSER_ACTION_SAVE,
                                          _("_Cancel"), GTK_RESPONSE_CANCEL,
                                          _("_Export"), GTK_RESPONSE_ACCEPT,
                                          NULL);
    gtk_dialog_add_button (GTK_DIALOG (dialog), _("Reset defaults"),
                           RESPONSE_RESET_DEFAULTS);
    gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
    gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);

    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, _("SIE4 files (*.SE, *.se)"));
    gtk_file_filter_add_pattern (filter, "*.SE");
    gtk_file_filter_add_pattern (filter, "*.se");
    gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (dialog), filter);

    extra_widget = create_extra_widget (&widgets);
    gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER (dialog), extra_widget);

    starting_dir = gnc_get_default_directory (GNC_PREFS_GROUP_SIE4);
    if (starting_dir)
        gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), starting_dir);
    g_free (starting_dir);

    if (previous_dialog_state.initialized)
        apply_dialog_state (GTK_FILE_CHOOSER (dialog), &widgets, &previous_dialog_state);
    else
        restore_default_dialog_state (GTK_FILE_CHOOSER (dialog), &widgets);

    gtk_widget_show_all (extra_widget);

    while (!done)
    {
        gint response = gtk_dialog_run (GTK_DIALOG (dialog));
        GncSie4ExportSettings settings = { 0 };
        GncSie4ExportResult result = { 0 };
        GError *error = NULL;

        if (response == RESPONSE_RESET_DEFAULTS)
        {
            restore_default_dialog_state (GTK_FILE_CHOOSER (dialog), &widgets);
            continue;
        }

        if (response != GTK_RESPONSE_ACCEPT)
            break;

        if (!collect_settings (GTK_FILE_CHOOSER (dialog), &widgets, &settings, parent))
        {
            g_free ((gchar *)settings.file_name);
            continue;
        }

        if (gnc_sie4_export (&settings, &result, &error))
        {
            gchar *dir = g_path_get_dirname (settings.file_name);
            gnc_set_default_directory (GNC_PREFS_GROUP_SIE4, dir);
            g_free (dir);
            if (result.generated_voucher_numbers > 0)
            {
                gchar *message = g_strdup_printf (
                    ngettext ("The SIE4 file has been exported to '%s'.\n\n"
                              "%u transaction had an empty GnuCash transaction number, so a voucher number was generated for it.",
                              "The SIE4 file has been exported to '%s'.\n\n"
                              "%u transactions had empty GnuCash transaction numbers, so voucher numbers were generated for them.",
                              result.generated_voucher_numbers),
                    settings.file_name, result.generated_voucher_numbers);
                gnc_info_dialog (parent, "%s", message);
                g_free (message);
            }
            else
            {
                gnc_info_dialog (parent, _("The SIE4 file has been exported to '%s'."), settings.file_name);
            }
            done = TRUE;
        }
        else
        {
            gnc_error_dialog (parent, _("SIE4 export failed: %s"),
                              error ? error->message : _("Unknown error"));
            g_clear_error (&error);
        }

        g_free ((gchar *)settings.file_name);
    }

    capture_dialog_state (GTK_FILE_CHOOSER (dialog), &widgets, &previous_dialog_state);
    gtk_widget_destroy (dialog);
}

void
gnc_file_sie4_export (GtkWindow *parent)
{
    gnc_file_sie4_export_internal (parent);
}
