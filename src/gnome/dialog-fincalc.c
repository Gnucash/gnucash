/********************************************************************\
 * dialog-fincalc.c : dialog for a financial calculator             *
 * Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>      *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gnome.h>
#include <locale.h>
#include <time.h>

#include "dialog-fincalc.h"
#include "dialog-utils.h"
#include "finproto.h"
#include "finvar.h"
#include "glade-gnc-dialogs.h"
#include "glade-support.h"
#include "gnc-amount-edit.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-dateedit.h"
#include "gnc-engine-util.h"
#include "messages.h"
#include "query-user.h"


#define DIALOG_FINCALC_CM_CLASS "dialog-fincalc"

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

  GtkWidget *compounding_menu;
  GtkWidget *payment_menu;

  GtkWidget *end_of_period_radio;
  GtkWidget *discrete_compounding_radio;

  GtkWidget *payment_total_label;

  financial_info financial_info;

  GList *calc_buttons;
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
static short module = MOD_GUI;


/** Prototypes **********************************************************/


/** Implementations *****************************************************/

/* Ensure the given argument is one of the values in the periods array
 * above and return the index of the value. */
static int
normalize_period(unsigned int *period)
{
  int i;

  assert(period != NULL);

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

  pmt = double_to_gnc_numeric (fcd->financial_info.pmt, 100000, GNC_RND_ROUND);

  commodity = gnc_locale_default_currency ();

  total = gnc_numeric_mul (npp, pmt, gnc_commodity_get_fraction (commodity),
                           GNC_RND_ROUND);

  xaccSPrintAmount (string, total, gnc_default_print_info (FALSE));
  gtk_label_set_text (GTK_LABEL(fcd->payment_total_label), string);

  i = normalize_period(&fcd->financial_info.CF);
  gtk_option_menu_set_history(GTK_OPTION_MENU(fcd->compounding_menu), i);

  i = normalize_period(&fcd->financial_info.PF);
  gtk_option_menu_set_history(GTK_OPTION_MENU(fcd->payment_menu), i);

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

  i = gnc_option_menu_get_active(fcd->compounding_menu);
  fcd->financial_info.CF = periods[i];

  i = gnc_option_menu_get_active(fcd->payment_menu);
  fcd->financial_info.PF = periods[i];

  toggle = GTK_TOGGLE_BUTTON(fcd->end_of_period_radio);
  fcd->financial_info.bep = !gtk_toggle_button_get_active(toggle);

  toggle = GTK_TOGGLE_BUTTON(fcd->discrete_compounding_radio);
  fcd->financial_info.disc = gtk_toggle_button_get_active(toggle);

  fcd->financial_info.prec = gnc_locale_decimal_places ();
}

/* Set the sensitivity of the calculation buttons based on the argument. */
static void
fincalc_allow_calc(FinCalcDialog *fcd, gboolean allow_calc)
{
  GList *node;

  if (fcd == NULL)
    return;

  for (node = fcd->calc_buttons; node != NULL; node = node->next)
    gtk_widget_set_sensitive(GTK_WIDGET(node->data), allow_calc);
}

/* Free the calc button list and free the FinCalcDialog structure. */
static void
fincalc_dialog_destroy(GtkObject *object, gpointer data)
{
  FinCalcDialog *fcd = data;

  if (fcd == NULL)
    return;

  gnc_unregister_gui_component_by_data (DIALOG_FINCALC_CM_CLASS, fcd);

  g_list_free(fcd->calc_buttons);
  fcd->calc_buttons = NULL;

  g_free(fcd);
}

static void
fincalc_entry_changed(GtkEditable *editable, gpointer data)
{
  FinCalcDialog *fcd = data;

  if (fcd == NULL)
    return;

  fincalc_allow_calc(fcd, TRUE);
}

static void
fincalc_menu_changed(GtkButton *button, FinCalcDialog *fcd)
{
  if (fcd == NULL)
    return;

  fincalc_allow_calc(fcd, TRUE);
}

static void
connect_fincalc_menu_item(GtkWidget *item, gpointer data)
{
  gtk_signal_connect(GTK_OBJECT(item), "activate",
                     GTK_SIGNAL_FUNC(fincalc_menu_changed), data);
}

static void
fincalc_radio_toggled(GtkToggleButton *togglebutton, gpointer data)
{
  FinCalcDialog *fcd = data;

  if (fcd == NULL)
    return;

  fincalc_allow_calc(fcd, TRUE);
}

static void
close_button_clicked(GtkButton *button, FinCalcDialog *fcd)
{
  gnc_close_gui_component_by_data (DIALOG_FINCALC_CM_CLASS, fcd);
}

static void
fincalc_entry_clear_clicked(GtkButton *button, GtkEntry *entry)
{
  gtk_entry_set_text(GTK_ENTRY(entry), "");
}

static void
fincalc_amount_clear_clicked(GtkButton *button, GNCAmountEdit *edit)
{
  gtk_entry_set_text(GTK_ENTRY (edit->amount_entry), "");
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
  const char *missing = _("You must enter values for the other quantities.");
  const char *bad_exp = _("You must enter a valid expression.");
  const char *string;
  gnc_numeric nvalue;
  int i;

  if (fcd == NULL)
    return NULL;

  /* Check for missing values */
  for (i = 0; i < NUM_FIN_CALC_VALUES; i++)
    if (i != value)
    {
      GtkWidget *entry;

      entry = GNC_AMOUNT_EDIT (fcd->amounts[i])->amount_entry;
      string = gtk_entry_get_text(GTK_ENTRY(entry));
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

    gnc_error_dialog_parented(GTK_WINDOW(fcd->dialog), string);
    if (error_item == 0)
      entry = fcd->amounts[0];
    else
      entry = GNC_AMOUNT_EDIT (fcd->amounts[error_item])->amount_entry;
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

  fincalc_allow_calc(fcd, FALSE);
}

static void
calc_payment_periods(GtkButton *button, FinCalcDialog *fcd)
{
  calc_value(fcd, PAYMENT_PERIODS);
}

static void
calc_interest_rate(GtkButton *button, FinCalcDialog *fcd)
{
  calc_value(fcd, INTEREST_RATE);
}

static void
calc_present_value(GtkButton *button, FinCalcDialog *fcd)
{
  calc_value(fcd, PRESENT_VALUE);
}

static void
calc_periodic_payment(GtkButton *button, FinCalcDialog *fcd)
{
  calc_value(fcd, PERIODIC_PAYMENT);
}

static void
calc_future_value(GtkButton *button, FinCalcDialog *fcd)
{
  calc_value(fcd, FUTURE_VALUE);
}

static void
close_handler (gpointer user_data)
{
  FinCalcDialog *fcd = user_data;

  gnome_dialog_close (GNOME_DIALOG (fcd->dialog));
}

FinCalcDialog *
gnc_ui_fincalc_dialog_create(void)
{
  gnc_commodity *commodity;
  GNCPrintAmountInfo print_info;
  FinCalcDialog *fcd;
  GtkObject *fcdo;
  GtkWidget *button;
  GtkWidget *entry;
  GtkWidget *menu;
  GtkWidget *hbox;
  GtkWidget *edit;

  commodity = gnc_locale_default_currency ();

  fcd = g_new0(FinCalcDialog, 1);

  fcd->dialog = create_Financial_Calculator_Dialog();
  fcdo = GTK_OBJECT(fcd->dialog);

  gnc_register_gui_component (DIALOG_FINCALC_CM_CLASS,
                              NULL, close_handler, fcd);

  gtk_signal_connect(fcdo, "destroy",
                     GTK_SIGNAL_FUNC(fincalc_dialog_destroy), fcd);


  edit = gnc_amount_edit_new();
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
  print_info = gnc_integral_print_info ();
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit), 1);
  fcd->amounts[PAYMENT_PERIODS] = edit;
  gtk_widget_show (edit);

  hbox = lookup_widget (fcd->dialog, "payment_periods_hbox");
  gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

  entry = GNC_AMOUNT_EDIT (edit)->amount_entry;
  gtk_signal_connect(GTK_OBJECT(entry), "changed",
                     GTK_SIGNAL_FUNC(fincalc_entry_changed), fcd);


  edit = gnc_amount_edit_new();
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
  print_info.max_decimal_places = 5;
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit), 100000);
  fcd->amounts[INTEREST_RATE] = edit;
  gtk_widget_show (edit);

  hbox = lookup_widget (fcd->dialog, "interest_rate_hbox");
  gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

  entry = GNC_AMOUNT_EDIT (edit)->amount_entry;
  gtk_signal_connect(GTK_OBJECT(entry), "changed",
                     GTK_SIGNAL_FUNC(fincalc_entry_changed), fcd);


  print_info = gnc_commodity_print_info (commodity, FALSE);

  edit = gnc_amount_edit_new();
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit),
                                gnc_commodity_get_fraction (commodity));
  fcd->amounts[PRESENT_VALUE] = edit;
  gtk_widget_show (edit);

  hbox = lookup_widget (fcd->dialog, "present_value_hbox");
  gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

  entry = GNC_AMOUNT_EDIT (edit)->amount_entry;
  gtk_signal_connect(GTK_OBJECT(entry), "changed",
                     GTK_SIGNAL_FUNC(fincalc_entry_changed), fcd);


  edit = gnc_amount_edit_new();
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit),
                                gnc_commodity_get_fraction (commodity));
  fcd->amounts[PERIODIC_PAYMENT] = edit;
  gtk_widget_show (edit);

  hbox = lookup_widget (fcd->dialog, "periodic_payment_hbox");
  gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

  entry = GNC_AMOUNT_EDIT (edit)->amount_entry;
  gtk_signal_connect(GTK_OBJECT(entry), "changed",
                     GTK_SIGNAL_FUNC(fincalc_entry_changed), fcd);


  edit = gnc_amount_edit_new();
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit),
                                gnc_commodity_get_fraction (commodity));
  fcd->amounts[FUTURE_VALUE] = edit;
  gtk_widget_show (edit);

  hbox = lookup_widget (fcd->dialog, "future_value_hbox");
  gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

  entry = GNC_AMOUNT_EDIT (edit)->amount_entry;
  gtk_signal_connect(GTK_OBJECT(entry), "changed",
                     GTK_SIGNAL_FUNC(fincalc_entry_changed), fcd);


  button = lookup_widget (fcd->dialog, "payment_periods_calc_button");
  fcd->calc_buttons = g_list_prepend(fcd->calc_buttons, button);
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(calc_payment_periods), fcd);

  button = lookup_widget (fcd->dialog, "interest_rate_calc_button");
  fcd->calc_buttons = g_list_prepend(fcd->calc_buttons, button);
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(calc_interest_rate), fcd);

  button = lookup_widget (fcd->dialog, "present_value_calc_button");
  fcd->calc_buttons = g_list_prepend(fcd->calc_buttons, button);
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(calc_present_value), fcd);

  button = lookup_widget (fcd->dialog, "periodic_payment_calc_button");
  fcd->calc_buttons = g_list_prepend(fcd->calc_buttons, button);
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(calc_periodic_payment), fcd);

  button = lookup_widget (fcd->dialog, "future_value_calc_button");
  fcd->calc_buttons = g_list_prepend(fcd->calc_buttons, button);
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(calc_future_value), fcd);

  button = lookup_widget (fcd->dialog, "payment_periods_clear_button");
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(fincalc_entry_clear_clicked),
                     fcd->amounts[PAYMENT_PERIODS]);

  button = lookup_widget (fcd->dialog, "interest_rate_clear_button");
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(fincalc_amount_clear_clicked),
                     fcd->amounts[INTEREST_RATE]);

  button = lookup_widget (fcd->dialog, "present_value_clear_button");
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(fincalc_amount_clear_clicked),
                     fcd->amounts[PRESENT_VALUE]);

  button = lookup_widget (fcd->dialog, "periodic_payment_clear_button");
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(fincalc_amount_clear_clicked),
                     fcd->amounts[PERIODIC_PAYMENT]);

  button = lookup_widget (fcd->dialog, "future_value_clear_button");
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(fincalc_amount_clear_clicked),
                     fcd->amounts[FUTURE_VALUE]);

  menu = lookup_widget (fcd->dialog, "compounding_menu");
  fcd->compounding_menu = menu;
  gnc_option_menu_init(menu);
  menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(menu));
  gtk_container_forall(GTK_CONTAINER(menu), connect_fincalc_menu_item, fcd);

  menu = lookup_widget (fcd->dialog, "payment_menu");
  fcd->payment_menu = menu;
  gnc_option_menu_init(menu);
  menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(menu));
  gtk_container_forall(GTK_CONTAINER(menu), connect_fincalc_menu_item, fcd);

  button = lookup_widget (fcd->dialog, "period_payment_radio");
  fcd->end_of_period_radio = button;
  gtk_signal_connect(GTK_OBJECT(button), "toggled",
                     GTK_SIGNAL_FUNC(fincalc_radio_toggled), fcd);

  button = lookup_widget (fcd->dialog, "discrete_compounding_radio");
  fcd->discrete_compounding_radio = button;
  gtk_signal_connect(GTK_OBJECT(button), "toggled",
                     GTK_SIGNAL_FUNC(fincalc_radio_toggled), fcd);

  button = lookup_widget (fcd->dialog, "close_button");
  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(close_button_clicked), fcd);
  fcd->payment_total_label = lookup_widget (fcd->dialog,
                                            "payment_total_label");

  button = lookup_widget (fcd->dialog, "schedule_button");
  gtk_widget_hide (button);

  init_fi(fcd);

  fi_to_gui(fcd);

  fincalc_allow_calc(fcd, FALSE);

  gtk_widget_grab_focus(fcd->amounts[PAYMENT_PERIODS]);

  gtk_widget_show(fcd->dialog);

  return fcd;
}

void
gnc_ui_fincalc_dialog_destroy(FinCalcDialog *fcd)
{
  if (fcd == NULL)
    return;

  gnc_close_gui_component_by_data (DIALOG_FINCALC_CM_CLASS, fcd);
}
