/********************************************************************\
 * dialog-fincalc.c : dialog for a financial calculator             *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <locale.h>
#include <time.h>

#include "dialog-fincalc.h"
#include "dialog-utils.h"
#include "finproto.h"
#include "finvar.h"
#include "gnc-amount-edit.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-engine.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-locale-utils.h"


#define DIALOG_FINCALC_CM_CLASS "dialog-fincalc"
#define GNC_PREFS_GROUP "dialogs.fincalc"

typedef enum
{
    PAYMENT_PERIODS = 0,
    INTEREST_RATE,
    PRESENT_VALUE,
    PERIODIC_PAYMENT,
    FUTURE_VALUE,
    NUM_FIN_CALC_VALUES
} FinCalcValue;


/** Datatypes ***********************************************************/
struct _FinCalcDialog
{
    GtkWindow *window;

    GtkWidget *amounts[NUM_FIN_CALC_VALUES];

    GtkWidget *calc_button;
    GtkWidget *help_button;
    GtkWidget *close_button;

    GtkDropDown *compounding_combo;
    GtkDropDown *payment_combo;

    GtkWidget *end_of_period_radio;
    GtkWidget *precision;
    GtkWidget *discrete_compounding_radio;

    GtkWidget *payment_total_label;
    gboolean closing;
    gboolean window_destroyed;
    gboolean window_size_saved;

    financial_info financial_info;
};

static unsigned int periods[] =
{
    1, /* annual */
    2, /* semi-annual */
    3, /* tri-annual */
    4, /* quarterly */
    6, /* bi-monthly */
    12, /* monthly */
    24, /* semi-monthly */
    26, /* bi-weekly */
    52, /* weekly */
    360, /* daily (360) */
    365, /* daily (365) */
};

/* This static indicates the debugging module that this .o belongs to.  */
__attribute__((unused)) static QofLogModule log_module = GNC_MOD_GUI;


/** Prototypes **********************************************************/
void fincalc_update_calc_button_cb (GtkWidget *unused, gpointer user_data);
void fincalc_calc_clicked_cb (GtkButton *button, gpointer user_data);
void fincalc_compounding_radio_toggled (GtkCheckButton *checkbutton, gpointer user_data);
void fincalc_amount_clear_clicked_cb (GtkButton *button, gpointer user_data);
void fincalc_precision_spin_value_changed_cb (GtkSpinButton *button, gpointer user_data);
void fincalc_response_button_cb (GtkWidget *widget, gpointer user_data);

static void fincalc_close (FinCalcDialog *fcd);

/** Implementations *****************************************************/

/* Ensure the given argument is one of the values in the periods array
 * above and return the index of the value. */
static int
normalize_period (unsigned int *period)
{
    int i;

    g_return_val_if_fail (period, 0);

    for (i = (sizeof (periods) / sizeof (unsigned int)) - 1; i >= 0; i--)
        if (*period >= periods[i])
        {
            *period = periods[i];
            return i;
        }

    *period = periods[0];

    return 0;
}

/* Copy the values in the financial_info structure to the GUI */
static void
fi_to_gui (FinCalcDialog *fcd)
{
    int precision;
    static char string[64];
    gnc_numeric total;
    gnc_numeric npp;
    gnc_numeric pmt;
    int i;

    if (fcd == NULL)
        return;

    npp = gnc_numeric_create (fcd->financial_info.npp, 1);

    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(fcd->amounts[PAYMENT_PERIODS]),
                                npp);
    gnc_amount_edit_set_damount (GNC_AMOUNT_EDIT(fcd->amounts[INTEREST_RATE]),
                                 fcd->financial_info.ir);
    gnc_amount_edit_set_damount (GNC_AMOUNT_EDIT(fcd->amounts[PRESENT_VALUE]),
                                 fcd->financial_info.pv);
    gnc_amount_edit_set_damount (GNC_AMOUNT_EDIT(fcd->amounts[PERIODIC_PAYMENT]),
                                 fcd->financial_info.pmt);
    gnc_amount_edit_set_damount (GNC_AMOUNT_EDIT(fcd->amounts[FUTURE_VALUE]),
                                 -fcd->financial_info.fv);

    pmt = double_to_gnc_numeric (fcd->financial_info.pmt, 100000, GNC_HOW_RND_ROUND_HALF_UP);

    precision = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(fcd->precision));

    total = gnc_numeric_mul (npp, pmt, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);

    xaccSPrintAmount (string, total, gnc_share_print_info_places (precision));
    gtk_label_set_text (GTK_LABEL(fcd->payment_total_label), string);

    i = normalize_period (&fcd->financial_info.CF);
    gtk_drop_down_set_selected (fcd->compounding_combo, i);

    i = normalize_period (&fcd->financial_info.PF);
    gtk_drop_down_set_selected (fcd->payment_combo, i);

    gtk_check_button_set_active (GTK_CHECK_BUTTON(fcd->end_of_period_radio),
                                 !fcd->financial_info.bep);

    gtk_check_button_set_active (GTK_CHECK_BUTTON(fcd->discrete_compounding_radio),
                                 fcd->financial_info.disc);
}

static unsigned int
selected_period (GtkDropDown *drop_down)
{
    const guint selected = gtk_drop_down_get_selected (drop_down);
    return selected < G_N_ELEMENTS (periods) ? periods[selected] : periods[0];
}

/* Copy the values in the GUI to the financial_info structure */
static void
gui_to_fi (FinCalcDialog *fcd)
{
    GtkCheckButton *check_button;
    GtkWidget *entry;
    gnc_numeric npp;
    const gchar *text;

    if (fcd == NULL)
        return;

    /* treat PAYMENT_PERIODS as a plain GtkEntry */
    entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT(fcd->amounts[PAYMENT_PERIODS]));
    text = gnc_entry_get_text (GTK_ENTRY(entry));
    if (text && *text)
    {
        gnc_numeric out = gnc_numeric_from_string (text);
        if (!gnc_numeric_check (out))
            npp = gnc_numeric_convert (out, 1, GNC_HOW_RND_TRUNC);
        else
            npp = gnc_numeric_zero ();
    }
    else
        npp = gnc_numeric_zero ();
    fcd->financial_info.npp = npp.num;

    fcd->financial_info.ir =
        gnc_amount_edit_get_damount (GNC_AMOUNT_EDIT(fcd->amounts[INTEREST_RATE]));

    fcd->financial_info.pv =
        gnc_amount_edit_get_damount (GNC_AMOUNT_EDIT(fcd->amounts[PRESENT_VALUE]));

    fcd->financial_info.pmt =
        gnc_amount_edit_get_damount (GNC_AMOUNT_EDIT(fcd->amounts[PERIODIC_PAYMENT]));

    fcd->financial_info.fv =
        gnc_amount_edit_get_damount (GNC_AMOUNT_EDIT(fcd->amounts[FUTURE_VALUE]));
    fcd->financial_info.fv = -fcd->financial_info.fv;

    fcd->financial_info.CF = selected_period (fcd->compounding_combo);
    fcd->financial_info.PF = selected_period (fcd->payment_combo);

    check_button = GTK_CHECK_BUTTON(fcd->end_of_period_radio);
    fcd->financial_info.bep = !gtk_check_button_get_active (check_button);

    check_button = GTK_CHECK_BUTTON(fcd->discrete_compounding_radio);
    fcd->financial_info.disc = gtk_check_button_get_active (check_button);

    fcd->financial_info.prec = gnc_locale_decimal_places ();
}

/* Set the sensitivity of the calculation buttons based on the argument. */
static void
fincalc_update_calc_button_internal_cb (GtkWidget *widget, gpointer user_data)
{
    FinCalcDialog *fcd = user_data;
    const gchar *text;
    gint i;

    if (fcd == NULL)
        return;

    for (i = 0; i < NUM_FIN_CALC_VALUES; i++)
    {
        GtkWidget *entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT(fcd->amounts[i]));
        text = gnc_entry_get_text (GTK_ENTRY(entry));
        if ((text == NULL) || (*text == '\0'))
        {
            gtk_widget_set_sensitive (GTK_WIDGET(fcd->calc_button), TRUE);
            return;
        }
    }
    gtk_widget_set_sensitive (GTK_WIDGET(fcd->calc_button), FALSE);
}

void
fincalc_update_calc_button_cb (GtkWidget *widget, gpointer user_data)
{
    FinCalcDialog *fcd = user_data;
    fincalc_update_calc_button_internal_cb (widget, fcd);
}

static void
fincalc_dropdown_selected_cb (GObject *drop_down, G_GNUC_UNUSED GParamSpec *pspec,
                              gpointer user_data)
{
    fincalc_update_calc_button_internal_cb (GTK_WIDGET (drop_down), user_data);
}

static void
fincalc_save_window_size (FinCalcDialog *fcd)
{
    if (fcd && fcd->window && !fcd->window_size_saved)
    {
        gnc_save_window_size (GNC_PREFS_GROUP, fcd->window);
        fcd->window_size_saved = TRUE;
    }
}

/* Release the component and clear the borrowed window after destruction. */
static void
fincalc_dialog_destroy (G_GNUC_UNUSED GtkWidget *window, gpointer user_data)
{
    FinCalcDialog *fcd = user_data;

    if (!fcd || fcd->window_destroyed)
        return;

    fincalc_save_window_size (fcd);
    fcd->window_destroyed = TRUE;
    gnc_unregister_gui_component_by_data (DIALOG_FINCALC_CM_CLASS, fcd);
    fcd->window = NULL;
    g_free (fcd);
}

void
fincalc_compounding_radio_toggled (GtkCheckButton *checkbutton, gpointer user_data)
{
    FinCalcDialog *fcd = user_data;
    gboolean sensitive;

    if (fcd == NULL)
        return;

    fincalc_update_calc_button_cb (GTK_WIDGET(checkbutton), fcd);
    sensitive = gtk_check_button_get_active (checkbutton);
    gtk_widget_set_sensitive (GTK_WIDGET (fcd->compounding_combo), sensitive);
}

void
fincalc_amount_clear_clicked_cb (GtkButton *button, G_GNUC_UNUSED gpointer user_data)
{
    GNCAmountEdit *edit = GNC_AMOUNT_EDIT(g_object_get_data (G_OBJECT(button), "edit"));
    GtkWidget *entry = gnc_amount_edit_gtk_entry (edit);
    gnc_numeric value;

    if (entry && GTK_IS_ENTRY(entry))
        gnc_entry_set_text (GTK_ENTRY(entry), "");

    gnc_amount_edit_expr_is_valid (edit, &value, TRUE, NULL);
}

void
fincalc_precision_spin_value_changed_cb (G_GNUC_UNUSED GtkSpinButton *button,
                                         gpointer user_data)
{
    FinCalcDialog *fcd = user_data;
    if (fcd)
        gtk_widget_set_sensitive (GTK_WIDGET(fcd->calc_button), TRUE);
}

static void
init_fi (FinCalcDialog *fcd)
{
    struct lconv *lc;

    if (fcd == NULL)
        return;

    lc = gnc_localeconv ();

    fcd->financial_info.npp = 12;
    fcd->financial_info.ir = 8.5;
    fcd->financial_info.pv = 15000.0;
    fcd->financial_info.pmt = -400.0;
    fcd->financial_info.CF = 12;
    fcd->financial_info.PF = 12;
    fcd->financial_info.bep = FALSE;
    fcd->financial_info.disc = TRUE;
    fcd->financial_info.prec = lc->frac_digits;

    fi_calc_future_value (&fcd->financial_info);
}

/* Determine whether the value can be calculated. If it can, return
 * NULL. Otherwise, return a string describing the reason and the offending
 * entry in error_item. */
static const char *
can_calc_value (FinCalcDialog *fcd, FinCalcValue value, int *error_item)
{
    const char *missing = _("This program can only calculate one value at a time. "
                            "You must enter values for all but one quantity.");
    const char *bad_exp = _("GnuCash cannot determine the value in one of the fields. "
                            "You must enter a valid expression.");
    const char *string;
    gnc_numeric nvalue;
    unsigned int i;

    if (fcd == NULL)
        return NULL;

    /* Check for missing values */
    for (i = 0; i < NUM_FIN_CALC_VALUES; i++)
        if (i != value)
        {
            GtkWidget *entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT(fcd->amounts[i]));
            string = gnc_entry_get_text (GTK_ENTRY(entry));
            if ((string == NULL) || (*string == '\0'))
            {
                *error_item = i;
                return missing;
            }

            /* treat PAYMENT_PERIODS as a plain GtkEntry */
            if (i != PAYMENT_PERIODS)
            {
                if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT(fcd->amounts[i]), NULL))
                {
                    *error_item = i;
                    return bad_exp;
                }
            }
        }

    /* Check for zero interest */
    switch (value)
    {
    case PAYMENT_PERIODS:
    case PRESENT_VALUE:
    case PERIODIC_PAYMENT:
    case FUTURE_VALUE:
        nvalue = gnc_amount_edit_get_amount
                 (GNC_AMOUNT_EDIT(fcd->amounts[INTEREST_RATE]));
        if (gnc_numeric_zero_p (nvalue))
        {
            *error_item = INTEREST_RATE;
            return _("The interest rate cannot be zero.");
        }
        break;
    default:
        break;
    }

    /* Check for zero payment periods */
    switch (value)
    {
    case INTEREST_RATE:
    case PRESENT_VALUE:
    case PERIODIC_PAYMENT:
    case FUTURE_VALUE:
        {
            /* treat PAYMENT_PERIODS as a plain GtkEntry */
            GNCAmountEdit *edit = GNC_AMOUNT_EDIT(fcd->amounts[PAYMENT_PERIODS]);
            gint result = gnc_amount_edit_expr_is_valid (edit, &nvalue, TRUE, NULL);

            if (result == 1)
            {
                *error_item = PAYMENT_PERIODS;
                return bad_exp;
            }
            if (gnc_numeric_zero_p (nvalue))
            {
                *error_item = PAYMENT_PERIODS;
                return _("The number of payments cannot be zero.");
            }
            if (gnc_numeric_negative_p (nvalue))
            {
                *error_item = PAYMENT_PERIODS;
                return _("The number of payments cannot be negative.");
            }
        }
        break;
    default:
        break;
    }

    return NULL;
}

static void
calc_value (FinCalcDialog *fcd, FinCalcValue value)
{
    const char *string;
    int error_item = 0;

    if (fcd == NULL)
        return;

    string = can_calc_value (fcd, value, &error_item);
    if (string != NULL)
    {
        GtkWidget *entry;

        gnc_error_dialog (GTK_WINDOW (fcd->window), "%s", string);
        if (error_item == 0)
            entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT(fcd->amounts[0]));
        else
            entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT(fcd->amounts[error_item]));
        gtk_widget_grab_focus (entry);
        return;
    }

    gui_to_fi (fcd);

    switch (value)
    {
    case PAYMENT_PERIODS:
        fi_calc_num_payments (&fcd->financial_info);
        break;
    case INTEREST_RATE:
        fi_calc_interest (&fcd->financial_info);
        break;
    case PRESENT_VALUE:
        fi_calc_present_value (&fcd->financial_info);
        break;
    case PERIODIC_PAYMENT:
        fi_calc_payment (&fcd->financial_info);
        break;
    case FUTURE_VALUE:
        fi_calc_future_value (&fcd->financial_info);
        break;
    default:
        break;
    }

    fi_to_gui (fcd);

    gtk_widget_set_sensitive (GTK_WIDGET(fcd->calc_button), FALSE);
}

void
fincalc_calc_clicked_cb (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    FinCalcDialog *fcd = user_data;
    if (!fcd || fcd->closing)
        return;
    const gchar *text;
    gint i;

    for (i = 0; i < NUM_FIN_CALC_VALUES; i++)
    {
        GtkWidget *entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT(fcd->amounts[i]));
        text = gnc_entry_get_text (GTK_ENTRY(entry));
        if ((text != NULL) && (*text != '\0'))
            continue;
        calc_value (fcd, i);
        return;
    }
    calc_value (fcd, NUM_FIN_CALC_VALUES);
}

void
fincalc_response_button_cb (GtkWidget *widget, gpointer user_data)
{
    FinCalcDialog *fcd = user_data;

    if (!fcd || fcd->closing)
        return;
    if (widget == fcd->help_button)
    {
        gnc_gnome_help (fcd->window, DF_MANUAL, DL_FIN_CALC);
        return;
    }
    fincalc_close (fcd);
}


static void
close_handler (gpointer user_data)
{
    FinCalcDialog *fcd = user_data;

    if (!fcd || fcd->window_destroyed || !fcd->window)
        return;

    fcd->closing = TRUE;
    fincalc_save_window_size (fcd);
    gtk_window_destroy (fcd->window);
}

static void
fincalc_close (FinCalcDialog *fcd)
{
    if (!fcd || fcd->closing || fcd->window_destroyed)
        return;

    fcd->closing = TRUE;
    gnc_close_gui_component_by_data (DIALOG_FINCALC_CM_CLASS, fcd);
}

static gboolean
fincalc_close_request_cb (G_GNUC_UNUSED GtkWindow *window, gpointer user_data)
{
    fincalc_close (user_data);
    return TRUE;
}

static gboolean
show_handler (G_GNUC_UNUSED const char *klass, G_GNUC_UNUSED gint component_id,
              gpointer user_data, G_GNUC_UNUSED gpointer iter_data)
{
    FinCalcDialog *fcd = user_data;

    if (!fcd || fcd->closing || fcd->window_destroyed)
        return FALSE;
    gtk_window_present (fcd->window);
    return TRUE;
}


/** Initialize an edit field that will display a general number.
 *
 *  @param edit A pointer to the edit widget.
 *
 *  @param min_places The minimum number of places after the decimal
 *  point.
 *
 *  @param max_places The maximum number of places after the decimal
 *  point.
 *
 *  @param fraction The fraction used to maintain numbers internally.
 */
static void
fincalc_init_gae (GNCAmountEdit *edit,
                  gint min_places,
                  gint max_places,
                  gint fraction)
{
    GNCPrintAmountInfo print_info;
    GtkWidget *entry;

    print_info = gnc_integral_print_info ();
    print_info.min_decimal_places = min_places;
    print_info.max_decimal_places = max_places;

    gnc_amount_edit_set_print_info (edit, print_info);
    gnc_amount_edit_set_fraction (edit, fraction);
    gnc_amount_edit_set_evaluate_on_enter (edit, TRUE);
    entry = gnc_amount_edit_gtk_entry (edit);
    gtk_entry_set_alignment (GTK_ENTRY(entry), 1.0);
}

/** Initialize an edit field that will display a number in the users
 *  local currency.
 *
 *  @param edit A pointer to the edit widget.
 */
static void
fincalc_init_commodity_gae (GNCAmountEdit *edit)
{
    GNCPrintAmountInfo print_info;
    gnc_commodity *commodity;
    gint fraction;
    GtkWidget *entry;

    commodity = gnc_default_currency ();
    fraction = gnc_commodity_get_fraction (commodity);
    print_info = gnc_commodity_print_info (commodity, FALSE);

    gnc_amount_edit_set_print_info (edit, print_info);
    gnc_amount_edit_set_fraction (edit, fraction);
    gnc_amount_edit_set_evaluate_on_enter (edit, TRUE);
    entry = gnc_amount_edit_gtk_entry (edit);
    gtk_entry_set_alignment (GTK_ENTRY(entry), 1.0);
}

static gboolean
fincalc_dialog_key_press_cb (G_GNUC_UNUSED GtkEventControllerKey *key, guint keyval,
                             G_GNUC_UNUSED guint keycode,
                             G_GNUC_UNUSED GdkModifierType state,
                             gpointer user_data)
{
    if (keyval != GDK_KEY_Escape)
        return FALSE;

    fincalc_close (user_data);
    return TRUE;
}

void
gnc_ui_fincalc_dialog_create (GtkWindow *parent)
{
    FinCalcDialog *fcd;
    GtkWidget *button;
    GtkDropDown *drop_down;
    GtkWidget *edit;
    GtkWidget *spin;
    GtkWidget *hbox;
    GtkBuilder *builder;
    GtkAdjustment *adjustment;

    if (gnc_forall_gui_components (DIALOG_FINCALC_CM_CLASS, show_handler, NULL))
        return;

    fcd = g_new0 (FinCalcDialog, 1);

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-fincalc.ui", "periods_model");
    gnc_builder_add_from_file (builder, "dialog-fincalc.ui", "financial_calculator_window");

    fcd->window = GTK_WINDOW (gtk_builder_get_object
        (builder, "financial_calculator_window"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET (fcd->window), "gnc-id-financial-calc");

    if (parent != NULL)
        gtk_window_set_transient_for (fcd->window, parent);

    gnc_register_gui_component (DIALOG_FINCALC_CM_CLASS,
                                NULL, close_handler, fcd);

    g_signal_connect (fcd->window, "destroy",
                      G_CALLBACK (fincalc_dialog_destroy), fcd);
    g_signal_connect (fcd->window, "close-request",
                      G_CALLBACK (fincalc_close_request_cb), fcd);

    GtkEventController *event_controller_window = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(fcd->window), event_controller_window);
    g_signal_connect (G_OBJECT(event_controller_window),
                      "key-pressed",
                      G_CALLBACK(fincalc_dialog_key_press_cb), fcd);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "payment_periods_hbox"));
    edit = gnc_amount_edit_new ();
    fincalc_init_gae (GNC_AMOUNT_EDIT(edit), 0, 0, 1);
    fcd->amounts[PAYMENT_PERIODS] = edit;
    gtk_box_prepend (GTK_BOX(hbox), GTK_WIDGET(edit));
    g_signal_connect (G_OBJECT(edit), "changed",
                      G_CALLBACK(fincalc_update_calc_button_internal_cb), fcd);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "payment_periods_clear_button"));
    g_object_set_data (G_OBJECT(button), "edit", edit);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "interest_rate_hbox"));
    edit = gnc_amount_edit_new ();
    fincalc_init_gae (GNC_AMOUNT_EDIT(edit), 2, 5, 100000);
    fcd->amounts[INTEREST_RATE] = edit;
    gtk_box_prepend (GTK_BOX(hbox), GTK_WIDGET(edit));
    g_signal_connect (G_OBJECT(edit), "changed",
                      G_CALLBACK(fincalc_update_calc_button_internal_cb), fcd);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "interest_rate_clear_button"));
    g_object_set_data (G_OBJECT(button), "edit", edit);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "present_value_hbox"));
    edit = gnc_amount_edit_new ();
    fincalc_init_commodity_gae (GNC_AMOUNT_EDIT(edit));
    fcd->amounts[PRESENT_VALUE] = edit;
    gtk_box_prepend (GTK_BOX(hbox), GTK_WIDGET(edit));
    g_signal_connect (G_OBJECT(edit), "changed",
                      G_CALLBACK(fincalc_update_calc_button_internal_cb), fcd);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "present_value_clear_button"));
    g_object_set_data (G_OBJECT(button), "edit", edit);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "periodic_payment_hbox"));
    edit = gnc_amount_edit_new ();
    fincalc_init_commodity_gae (GNC_AMOUNT_EDIT(edit));
    fcd->amounts[PERIODIC_PAYMENT] = edit;
    gtk_box_prepend (GTK_BOX(hbox), GTK_WIDGET(edit));
    g_signal_connect (G_OBJECT(edit), "changed",
                      G_CALLBACK(fincalc_update_calc_button_internal_cb), fcd);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "periodic_payment_clear_button"));
    g_object_set_data (G_OBJECT(button), "edit", edit);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "future_value_hbox"));
    edit = gnc_amount_edit_new ();
    fincalc_init_commodity_gae (GNC_AMOUNT_EDIT(edit));
    fcd->amounts[FUTURE_VALUE] = edit;
    gtk_box_prepend (GTK_BOX(hbox), GTK_WIDGET(edit));
    g_signal_connect (G_OBJECT(edit), "changed",
                      G_CALLBACK(fincalc_update_calc_button_internal_cb), fcd);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "future_value_clear_button"));
    g_object_set_data (G_OBJECT(button), "edit", edit);

    fcd->calc_button = GTK_WIDGET(gtk_builder_get_object (builder, "calc_button"));
    fcd->help_button = GTK_WIDGET(gtk_builder_get_object (builder, "help_button"));
    fcd->close_button = GTK_WIDGET(gtk_builder_get_object (builder, "close_button"));

    drop_down = GTK_DROP_DOWN (gtk_builder_get_object (builder, "compounding_combo"));
    fcd->compounding_combo = drop_down;
    g_signal_connect (fcd->compounding_combo, "notify::selected",
                      G_CALLBACK (fincalc_dropdown_selected_cb), fcd);

    drop_down = GTK_DROP_DOWN (gtk_builder_get_object (builder, "payment_combo"));
    fcd->payment_combo = drop_down;
    g_signal_connect (fcd->payment_combo, "notify::selected",
                      G_CALLBACK (fincalc_dropdown_selected_cb), fcd);

    spin = GTK_WIDGET(gtk_builder_get_object (builder, "precision_spin"));
    adjustment = gtk_adjustment_new (2, 0, 10, 1, 1, 1);
    gtk_spin_button_set_adjustment (GTK_SPIN_BUTTON(spin), adjustment);
    fcd->precision = spin;

    button = GTK_WIDGET(gtk_builder_get_object (builder, "period_payment_radio"));
    fcd->end_of_period_radio = button;

    button = GTK_WIDGET(gtk_builder_get_object (builder, "discrete_compounding_radio"));
    fcd->discrete_compounding_radio = button;

    fcd->payment_total_label = GTK_WIDGET(gtk_builder_get_object (builder, "payment_total_label"));

    button = GTK_WIDGET(gtk_builder_get_object (builder, "schedule_button"));
    gtk_widget_set_visible (GTK_WIDGET(button), FALSE);

    gnc_builder_connect_signals (builder, fcd);
    init_fi (fcd);
    fi_to_gui (fcd);
    gtk_widget_grab_focus (fcd->amounts[PAYMENT_PERIODS]);

    g_object_unref (builder);

    gnc_restore_window_size (GNC_PREFS_GROUP, fcd->window, parent);
    gtk_window_present (fcd->window);
}

void
gnc_ui_fincalc_dialog_destroy (FinCalcDialog *fcd)
{
    if (fcd == NULL)
        return;

    fincalc_close (fcd);
}
