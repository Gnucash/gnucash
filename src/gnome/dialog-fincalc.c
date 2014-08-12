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

#include "config.h"

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
    GtkWidget *dialog;

    GtkWidget *amounts[NUM_FIN_CALC_VALUES];

    GtkWidget *calc_button;

    GtkWidget *compounding_combo;
    GtkWidget *payment_combo;

    GtkWidget *end_of_period_radio;
    GtkWidget *discrete_compounding_radio;

    GtkWidget *payment_total_label;

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
static QofLogModule log_module = GNC_MOD_GUI;


/** Prototypes **********************************************************/
void fincalc_update_calc_button_cb(GtkWidget *unused, FinCalcDialog *fcd);
void fincalc_calc_clicked_cb(GtkButton *button, FinCalcDialog *fcd);
void fincalc_compounding_radio_toggled(GtkToggleButton *togglebutton, gpointer data);
void fincalc_amount_clear_clicked_cb(GtkButton *button, FinCalcDialog *fcd);
void fincalc_response_cb (GtkDialog *dialog, gint response, FinCalcDialog *fcd);

/** Implementations *****************************************************/

/* Ensure the given argument is one of the values in the periods array
 * above and return the index of the value. */
static int
normalize_period(unsigned int *period)
{
    int i;

    g_return_val_if_fail (period, 0);

    for (i = (sizeof(periods) / sizeof(unsigned int)) - 1; i >= 0; i--)
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
fi_to_gui(FinCalcDialog *fcd)
{
    const gnc_commodity *commodity;
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

    commodity = gnc_default_currency ();

    total = gnc_numeric_mul (npp, pmt, gnc_commodity_get_fraction (commodity),
                             GNC_HOW_RND_ROUND_HALF_UP);

    xaccSPrintAmount (string, total, gnc_default_print_info (FALSE));
    gtk_label_set_text (GTK_LABEL(fcd->payment_total_label), string);

    i = normalize_period(&fcd->financial_info.CF);
    gtk_combo_box_set_active(GTK_COMBO_BOX(fcd->compounding_combo), i);

    i = normalize_period(&fcd->financial_info.PF);
    gtk_combo_box_set_active(GTK_COMBO_BOX(fcd->payment_combo), i);

    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(fcd->end_of_period_radio),
                                 !fcd->financial_info.bep);

    gtk_toggle_button_set_active
    (GTK_TOGGLE_BUTTON(fcd->discrete_compounding_radio),
     fcd->financial_info.disc);
}

/* Copy the values in the GUI to the financial_info structure */
static void
gui_to_fi(FinCalcDialog *fcd)
{
    GtkToggleButton *toggle;
    gnc_numeric npp;
    int i;

    if (fcd == NULL)
        return;

    npp =
        gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(fcd->amounts[PAYMENT_PERIODS]));
    fcd->financial_info.npp = npp.num;

    fcd->financial_info.ir =
        gnc_amount_edit_get_damount(GNC_AMOUNT_EDIT(fcd->amounts[INTEREST_RATE]));

    fcd->financial_info.pv =
        gnc_amount_edit_get_damount(GNC_AMOUNT_EDIT(fcd->amounts[PRESENT_VALUE]));

    fcd->financial_info.pmt =
        gnc_amount_edit_get_damount(GNC_AMOUNT_EDIT(fcd->amounts[PERIODIC_PAYMENT]));

    fcd->financial_info.fv =
        gnc_amount_edit_get_damount(GNC_AMOUNT_EDIT(fcd->amounts[FUTURE_VALUE]));
    fcd->financial_info.fv = -fcd->financial_info.fv;

    i = gtk_combo_box_get_active(GTK_COMBO_BOX(fcd->compounding_combo));
    fcd->financial_info.CF = periods[i];

    i = gtk_combo_box_get_active(GTK_COMBO_BOX(fcd->payment_combo));
    fcd->financial_info.PF = periods[i];

    toggle = GTK_TOGGLE_BUTTON(fcd->end_of_period_radio);
    fcd->financial_info.bep = !gtk_toggle_button_get_active(toggle);

    toggle = GTK_TOGGLE_BUTTON(fcd->discrete_compounding_radio);
    fcd->financial_info.disc = gtk_toggle_button_get_active(toggle);

    fcd->financial_info.prec = gnc_locale_decimal_places ();
}

/* Set the sensitivity of the calculation buttons based on the argument. */
void
fincalc_update_calc_button_cb(GtkWidget *unused, FinCalcDialog *fcd)
{
    const gchar *text;
    gint i;

    if (fcd == NULL)
        return;

    for (i = 0; i < NUM_FIN_CALC_VALUES; i++)
    {
        text = gtk_entry_get_text(GTK_ENTRY(fcd->amounts[i]));
        if ((text == NULL) || (*text == '\0'))
        {
            gtk_widget_set_sensitive(GTK_WIDGET(fcd->calc_button), TRUE);
            return;
        }
    }

    gtk_widget_set_sensitive(GTK_WIDGET(fcd->calc_button), FALSE);
}

/* Free the calc button list and free the FinCalcDialog structure. */
static void
fincalc_dialog_destroy(GtkObject *object, gpointer data)
{
    FinCalcDialog *fcd = data;

    if (fcd == NULL)
        return;

    gnc_unregister_gui_component_by_data (DIALOG_FINCALC_CM_CLASS, fcd);

    g_free(fcd);
}

void
fincalc_compounding_radio_toggled(GtkToggleButton *togglebutton, gpointer data)
{
    FinCalcDialog *fcd = data;
    gboolean sensitive;

    if (fcd == NULL)
        return;

    fincalc_update_calc_button_cb(GTK_WIDGET(togglebutton), fcd);

    sensitive = gtk_toggle_button_get_active (togglebutton);

    gtk_widget_set_sensitive (fcd->compounding_combo, sensitive);
}

void
fincalc_amount_clear_clicked_cb(GtkButton *button, FinCalcDialog *fcd)
{
    GtkEntry * edit = GTK_ENTRY(g_object_get_data(G_OBJECT(button), "edit"));

    if (edit && GTK_IS_ENTRY(edit))
        gtk_entry_set_text(edit, "");
}

static void
init_fi(FinCalcDialog *fcd)
{
    struct lconv *lc;

    if (fcd == NULL)
        return;

    lc = gnc_localeconv();

    fcd->financial_info.npp = 12;
    fcd->financial_info.ir = 8.5;
    fcd->financial_info.pv = 15000.0;
    fcd->financial_info.pmt = -400.0;
    fcd->financial_info.CF = 12;
    fcd->financial_info.PF = 12;
    fcd->financial_info.bep = FALSE;
    fcd->financial_info.disc = TRUE;
    fcd->financial_info.prec = lc->frac_digits;

    fi_calc_future_value(&fcd->financial_info);
}

/* Determine whether the value can be calculated. If it can, return
 * NULL. Otherwise, return a string describing the reason and the offending
 * entry in error_item. */
static const char *
can_calc_value(FinCalcDialog *fcd, FinCalcValue value, int *error_item)
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
            string = gtk_entry_get_text(GTK_ENTRY(fcd->amounts[i]));
            if ((string == NULL) || (*string == '\0'))
            {
                *error_item = i;
                return missing;
            }

            if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (fcd->amounts[i])))
            {
                *error_item = i;
                return bad_exp;
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
                 (GNC_AMOUNT_EDIT (fcd->amounts[INTEREST_RATE]));
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
        nvalue = gnc_amount_edit_get_amount
                 (GNC_AMOUNT_EDIT(fcd->amounts[PAYMENT_PERIODS]));
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
        break;
    default:
        break;
    }

    return NULL;
}

static void
calc_value(FinCalcDialog *fcd, FinCalcValue value)
{
    const char *string;
    int error_item = 0;

    if (fcd == NULL)
        return;

    string = can_calc_value(fcd, value, &error_item);
    if (string != NULL)
    {
        GtkWidget *entry;

        gnc_error_dialog(fcd->dialog, "%s", string);
        if (error_item == 0)
            entry = fcd->amounts[0];
        else
            entry = fcd->amounts[error_item];
        gtk_widget_grab_focus (entry);
        return;
    }

    gui_to_fi(fcd);

    switch (value)
    {
    case PAYMENT_PERIODS:
        fi_calc_num_payments(&fcd->financial_info);
        break;
    case INTEREST_RATE:
        fi_calc_interest(&fcd->financial_info);
        break;
    case PRESENT_VALUE:
        fi_calc_present_value(&fcd->financial_info);
        break;
    case PERIODIC_PAYMENT:
        fi_calc_payment(&fcd->financial_info);
        break;
    case FUTURE_VALUE:
        fi_calc_future_value(&fcd->financial_info);
        break;
    default:
        PERR("Unknown financial variable");
        break;
    }

    fi_to_gui(fcd);

    gtk_widget_set_sensitive(GTK_WIDGET(fcd->calc_button), FALSE);
}

void
fincalc_calc_clicked_cb(GtkButton *button, FinCalcDialog *fcd)
{
    const gchar *text;
    gint i;

    for (i = 0; i < NUM_FIN_CALC_VALUES; i++)
    {
        text = gtk_entry_get_text(GTK_ENTRY(fcd->amounts[i]));
        if ((text != NULL) && (*text != '\0'))
            continue;
        calc_value(fcd, i);
        return;
    }
}

void fincalc_response_cb (GtkDialog *dialog,
                          gint response,
                          FinCalcDialog *fcd)
{
    switch (response)
    {
    case GTK_RESPONSE_OK:
        /* Do something here whenever the hidden schedule button is clicked. */
        /* Fall through */

    case GTK_RESPONSE_CLOSE:
        gnc_save_window_size(GNC_PREFS_GROUP, GTK_WINDOW(dialog));
        break;

    default:
        /* Cancel, destroy, etc.  Do nothing. */
        break;
    }

    gnc_close_gui_component_by_data (DIALOG_FINCALC_CM_CLASS, fcd);
}


static void
close_handler (gpointer user_data)
{
    FinCalcDialog *fcd = user_data;

    gtk_widget_destroy (fcd->dialog);
}

static gboolean
show_handler (const char *klass, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    FinCalcDialog *fcd = user_data;

    if (!fcd)
        return(FALSE);
    gtk_window_present (GTK_WINDOW(fcd->dialog));
    return(TRUE);
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

    print_info = gnc_integral_print_info ();
    print_info.min_decimal_places = min_places;
    print_info.max_decimal_places = max_places;

    gnc_amount_edit_set_print_info (edit, print_info);
    gnc_amount_edit_set_fraction (edit, fraction);
    gnc_amount_edit_set_evaluate_on_enter (edit, TRUE);
    gtk_entry_set_alignment (GTK_ENTRY(edit), 1.0);
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

    commodity = gnc_default_currency();
    fraction = gnc_commodity_get_fraction(commodity);
    print_info = gnc_commodity_print_info (commodity, FALSE);

    gnc_amount_edit_set_print_info (edit, print_info);
    gnc_amount_edit_set_fraction (edit, fraction);
    gnc_amount_edit_set_evaluate_on_enter (edit, TRUE);
    gtk_entry_set_alignment (GTK_ENTRY(edit), 1.0);
}

void
gnc_ui_fincalc_dialog_create(void)
{
    FinCalcDialog *fcd;
    GtkWidget *button;
    GtkWidget *combo;
    GtkWidget *edit;
    GtkWidget *hbox;
    GtkBuilder *builder;

    if (gnc_forall_gui_components (DIALOG_FINCALC_CM_CLASS,
                                   show_handler, NULL))
        return;


    fcd = g_new0(FinCalcDialog, 1);

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-fincalc.glade", "liststore1");
    gnc_builder_add_from_file (builder, "dialog-fincalc.glade", "liststore2");
    gnc_builder_add_from_file (builder, "dialog-fincalc.glade", "Financial Calculator Dialog");

    fcd->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "Financial Calculator Dialog"));

    gnc_register_gui_component (DIALOG_FINCALC_CM_CLASS,
                                NULL, close_handler, fcd);

    g_signal_connect (G_OBJECT (fcd->dialog), "destroy",
                      G_CALLBACK (fincalc_dialog_destroy), fcd);


    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "payment_periods_hbox"));
    edit = gnc_amount_edit_new();
    fincalc_init_gae (GNC_AMOUNT_EDIT (edit), 0, 0, 1);
    fcd->amounts[PAYMENT_PERIODS] = edit;
    gtk_box_pack_end(GTK_BOX(hbox), edit, FALSE, FALSE, 0);
    g_signal_connect (G_OBJECT(edit), "changed",
                      G_CALLBACK (fincalc_update_calc_button_cb), fcd);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "payment_periods_clear_button"));
    g_object_set_data(G_OBJECT(button), "edit", edit);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "interest_rate_hbox"));
    edit = gnc_amount_edit_new();
    fincalc_init_gae (GNC_AMOUNT_EDIT (edit), 2, 5, 100000);
    fcd->amounts[INTEREST_RATE] = edit;
    gtk_box_pack_end(GTK_BOX(hbox), edit, FALSE, FALSE, 0);
    g_signal_connect (G_OBJECT(edit), "changed",
                      G_CALLBACK (fincalc_update_calc_button_cb), fcd);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "interest_rate_clear_button"));
    g_object_set_data(G_OBJECT(button), "edit", edit);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "present_value_hbox"));
    edit = gnc_amount_edit_new();
    fincalc_init_commodity_gae (GNC_AMOUNT_EDIT (edit));
    fcd->amounts[PRESENT_VALUE] = edit;
    gtk_box_pack_end(GTK_BOX(hbox), edit, FALSE, FALSE, 0);
    g_signal_connect (G_OBJECT(edit), "changed",
                      G_CALLBACK (fincalc_update_calc_button_cb), fcd);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "present_value_clear_button"));
    g_object_set_data(G_OBJECT(button), "edit", edit);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "periodic_payment_hbox"));
    edit = gnc_amount_edit_new();
    fincalc_init_commodity_gae (GNC_AMOUNT_EDIT (edit));
    fcd->amounts[PERIODIC_PAYMENT] = edit;
    gtk_box_pack_end(GTK_BOX(hbox), edit, FALSE, FALSE, 0);
    g_signal_connect (G_OBJECT(edit), "changed",
                      G_CALLBACK (fincalc_update_calc_button_cb), fcd);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "periodic_payment_clear_button"));
    g_object_set_data(G_OBJECT(button), "edit", edit);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "future_value_hbox"));
    edit = gnc_amount_edit_new();
    fincalc_init_commodity_gae (GNC_AMOUNT_EDIT (edit));
    fcd->amounts[FUTURE_VALUE] = edit;
    gtk_box_pack_end(GTK_BOX(hbox), edit, FALSE, FALSE, 0);
    g_signal_connect (G_OBJECT(edit), "changed",
                      G_CALLBACK (fincalc_update_calc_button_cb), fcd);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "future_value_clear_button"));
    g_object_set_data(G_OBJECT(button), "edit", edit);


    fcd->calc_button = GTK_WIDGET(gtk_builder_get_object (builder, "calc_button"));


    combo = GTK_WIDGET(gtk_builder_get_object (builder, "compounding_combo"));
    fcd->compounding_combo = combo;
    g_signal_connect(fcd->compounding_combo, "changed",
                     G_CALLBACK (fincalc_update_calc_button_cb), fcd);

    combo = GTK_WIDGET(gtk_builder_get_object (builder, "payment_combo"));
    fcd->payment_combo = combo;
    g_signal_connect(fcd->compounding_combo, "changed",
                     G_CALLBACK (fincalc_update_calc_button_cb), fcd);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "period_payment_radio"));
    fcd->end_of_period_radio = button;

    button = GTK_WIDGET(gtk_builder_get_object (builder, "discrete_compounding_radio"));
    fcd->discrete_compounding_radio = button;

    fcd->payment_total_label = GTK_WIDGET(gtk_builder_get_object (builder, "payment_total_label"));

    button = GTK_WIDGET(gtk_builder_get_object (builder, "schedule_button"));
    gtk_widget_hide (button);

    init_fi(fcd);

    fi_to_gui(fcd);

    gtk_widget_grab_focus(fcd->amounts[PAYMENT_PERIODS]);

    /* Connect all signals specified in glade. */
    gtk_builder_connect_signals(builder, fcd);
    g_object_unref(G_OBJECT(builder));

    gnc_restore_window_size(GNC_PREFS_GROUP, GTK_WINDOW(fcd->dialog));
    gtk_widget_show(fcd->dialog);
}

void
gnc_ui_fincalc_dialog_destroy(FinCalcDialog *fcd)
{
    if (fcd == NULL)
        return;

    gnc_close_gui_component_by_data (DIALOG_FINCALC_CM_CLASS, fcd);
}
