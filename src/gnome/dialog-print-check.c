/********************************************************************\
 * dialog-print-check.c : dialog to control check printing.         *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (C) 2006 David Hampton <hampton@employees.org>         *
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
#include <stdio.h>
#include <libguile.h>

#include "gnc-date.h"
#include "gnc-gconf-utils.h"
#include "gnc-numeric.h"
#include "gnc-plugin-page-register.h"
#include "dialog-print-check.h"
#include "dialog-utils.h"
#include "print-session.h"
#include "gnc-ui.h"
#include "gnc-date-format.h"

#define CHECK_PRINT_NUM_FORMATS 4
#define CHECK_PRINT_NUM_POSITIONS 4
#define CHECK_PRINT_NUM_UNITS 4

#define GCONF_SECTION 	       "dialogs/print_checks"
#define KEY_CHECK_FORMAT       "check_format"
#define KEY_CHECK_POSITION     "check_position"
#define KEY_DATE_FORMAT_USER   "date_format_custom"
#define KEY_CUSTOM_PAYEE       "custom_payee"
#define KEY_CUSTOM_DATE        "custom_date"
#define KEY_CUSTOM_WORDS       "custom_amount_words"
#define KEY_CUSTOM_NUMBER      "custom_amount_number"
#define KEY_CUSTOM_MEMO        "custom_memo"
#define KEY_CUSTOM_TRANSLATION "custom_translation"
#define KEY_CUSTOM_ROTATION    "custom_rotation"
#define KEY_CUSTOM_UNITS       "custom_units"

/* Used by glade_xml_signal_autoconnect_full */
void gnc_ui_print_check_response_cb(GtkDialog * dialog, gint response, PrintCheckDialog * pcd);
void gnc_print_check_combobox_changed(GtkComboBox *widget, PrintCheckDialog * pcd);
static void gnc_ui_print_save_dialog(PrintCheckDialog * pcd);
static void gnc_ui_print_restore_dialog(PrintCheckDialog * pcd);
void gnc_ui_print_restore_dialog(PrintCheckDialog * pcd);

struct _print_check_dialog {
  GladeXML * xml;
  GtkWidget * dialog;

  GncPluginPageRegister *plugin_page;
  const char    *payee;
  gnc_numeric    amount;
  time_t         date;
  const char    *memo;

  GtkWidget * format_combobox;
  GtkWidget * position_combobox;
  GtkWidget * custom_table;
  GtkSpinButton * payee_x,  * payee_y;
  GtkSpinButton * date_x,   * date_y;
  GtkSpinButton * words_x,  * words_y;
  GtkSpinButton * number_x, * number_y;
  GtkSpinButton * memo_x,   * memo_y;
  GtkSpinButton * translation_x, * translation_y;
  GtkSpinButton * check_rotation;
  GtkWidget * translation_label;

  GtkWidget * units_combobox;

  GtkWidget * date_format;

  gchar *format_string;

};


static void
save_float_pair (const char *section, const char *key, double a, double b)
{
  GSList *coord_list = NULL;

  coord_list = g_slist_append(coord_list, &a);
  coord_list = g_slist_append(coord_list, &b);
  gnc_gconf_set_list(section, key, GCONF_VALUE_FLOAT, coord_list, NULL);
  g_slist_free(coord_list);
}

static void
get_float_pair (const char *section, const char *key, double *a, double *b)
{
  GSList *coord_list;
  
  coord_list = gnc_gconf_get_list (section, key, GCONF_VALUE_FLOAT, NULL);
  if (NULL == coord_list) {
    *a = 0;
    *b = 0;
    return;
  }

  *a = *(gdouble*)g_slist_nth_data(coord_list, 0);
  *b = *(gdouble*)g_slist_nth_data(coord_list, 1);
  g_slist_free(coord_list);
}

static void
gnc_ui_print_save_dialog(PrintCheckDialog * pcd)
{
  const gchar *format;
  gint active;

  /* Options page */
  active = gtk_combo_box_get_active(GTK_COMBO_BOX(pcd->format_combobox));
  gnc_gconf_set_int(GCONF_SECTION, KEY_CHECK_FORMAT, active, NULL);
  active = gtk_combo_box_get_active(GTK_COMBO_BOX(pcd->position_combobox));
  gnc_gconf_set_int(GCONF_SECTION, KEY_CHECK_POSITION, active, NULL);
  active = gnc_date_format_get_format (GNC_DATE_FORMAT(pcd->date_format));
  gnc_gconf_set_int(GCONF_SECTION, KEY_DATE_FORMAT, active, NULL);
  if (active == QOF_DATE_FORMAT_CUSTOM) {
    format = gnc_date_format_get_custom (GNC_DATE_FORMAT(pcd->date_format));
    gnc_gconf_set_string(GCONF_SECTION, KEY_DATE_FORMAT_USER, format, NULL);
  } else {
    gnc_gconf_unset (GCONF_SECTION, KEY_DATE_FORMAT_USER, NULL);
  }

  /* Custom format page */
  save_float_pair(GCONF_SECTION, KEY_CUSTOM_PAYEE,
		  gtk_spin_button_get_value(pcd->payee_x),
		  gtk_spin_button_get_value(pcd->payee_y));
  save_float_pair(GCONF_SECTION, KEY_CUSTOM_DATE,
		  gtk_spin_button_get_value(pcd->date_x),
		  gtk_spin_button_get_value(pcd->date_y));
  save_float_pair(GCONF_SECTION, KEY_CUSTOM_WORDS,
		  gtk_spin_button_get_value(pcd->words_x),
		  gtk_spin_button_get_value(pcd->words_y));
  save_float_pair(GCONF_SECTION, KEY_CUSTOM_NUMBER,
		  gtk_spin_button_get_value(pcd->number_x),
		  gtk_spin_button_get_value(pcd->number_y));
  save_float_pair(GCONF_SECTION, KEY_CUSTOM_MEMO,
		  gtk_spin_button_get_value(pcd->memo_x),
		  gtk_spin_button_get_value(pcd->memo_y));
  save_float_pair(GCONF_SECTION, KEY_CUSTOM_TRANSLATION,
		  gtk_spin_button_get_value(pcd->translation_x),
		  gtk_spin_button_get_value(pcd->translation_y));
  gnc_gconf_set_float(GCONF_SECTION, KEY_CUSTOM_ROTATION,
		      gtk_spin_button_get_value(pcd->check_rotation),
		      NULL);
  active = gtk_combo_box_get_active(GTK_COMBO_BOX(pcd->units_combobox));
  gnc_gconf_set_int(GCONF_SECTION, KEY_CUSTOM_UNITS, active, NULL);
}

void
gnc_ui_print_restore_dialog(PrintCheckDialog * pcd)
{
  gchar *format;
  gdouble x, y;
  gint active;

  /* Options page */
  active = gnc_gconf_get_int(GCONF_SECTION, KEY_CHECK_FORMAT, NULL);
  gtk_combo_box_set_active(GTK_COMBO_BOX(pcd->format_combobox), active);
  active = gnc_gconf_get_int(GCONF_SECTION, KEY_CHECK_POSITION, NULL);
  gtk_combo_box_set_active(GTK_COMBO_BOX(pcd->position_combobox), active);
  active = gnc_gconf_get_int(GCONF_SECTION, KEY_DATE_FORMAT, NULL);
  gnc_date_format_set_format(GNC_DATE_FORMAT(pcd->date_format), active);
  if (active == QOF_DATE_FORMAT_CUSTOM) {
    format = gnc_gconf_get_string(GCONF_SECTION, KEY_DATE_FORMAT_USER, NULL);
    if (format) {
      gnc_date_format_set_custom(GNC_DATE_FORMAT(pcd->date_format), format);
      g_free(format);
    }
  }

  /* Custom format page */
  get_float_pair(GCONF_SECTION, KEY_CUSTOM_PAYEE, &x, &y);
  gtk_spin_button_set_value(pcd->payee_x, x);
  gtk_spin_button_set_value(pcd->payee_y, y);

  get_float_pair(GCONF_SECTION, KEY_CUSTOM_DATE, &x, &y);
  gtk_spin_button_set_value(pcd->date_x, x);
  gtk_spin_button_set_value(pcd->date_y, y);
  get_float_pair(GCONF_SECTION, KEY_CUSTOM_WORDS, &x, &y);
  gtk_spin_button_set_value(pcd->words_x, x);
  gtk_spin_button_set_value(pcd->words_y, y);
  get_float_pair(GCONF_SECTION, KEY_CUSTOM_NUMBER, &x, &y);
  gtk_spin_button_set_value(pcd->number_x, x);
  gtk_spin_button_set_value(pcd->number_y, y);
  get_float_pair(GCONF_SECTION, KEY_CUSTOM_MEMO, &x, &y);
  gtk_spin_button_set_value(pcd->memo_x, x);
  gtk_spin_button_set_value(pcd->memo_y, y);
  get_float_pair(GCONF_SECTION, KEY_CUSTOM_TRANSLATION, &x, &y);
  gtk_spin_button_set_value(pcd->translation_x, x);
  gtk_spin_button_set_value(pcd->translation_y, y);
  x = gnc_gconf_get_float(GCONF_SECTION, KEY_CUSTOM_ROTATION, NULL);
  gtk_spin_button_set_value(pcd->check_rotation, x);
  active = gnc_gconf_get_int(GCONF_SECTION, KEY_CUSTOM_UNITS, NULL);
  gtk_combo_box_set_active(GTK_COMBO_BOX(pcd->units_combobox), active);
}


/********************************************************************\
 * gnc_ui_print_check_dialog_create
 * make a new print check dialog and wait for it.
\********************************************************************/

void
gnc_ui_print_check_dialog_create(GncPluginPageRegister *plugin_page,
				 const char    *payee,
				 gnc_numeric    amount,
				 time_t         date,
				 const char    *memo)
{
  PrintCheckDialog * pcd;
  GladeXML *xml;
  GtkWidget *table;
  GtkWindow *window;

  pcd = g_new0(PrintCheckDialog, 1);
  pcd->plugin_page = plugin_page;
  pcd->payee = payee;
  pcd->amount = amount;
  pcd->date = date;
  pcd->memo = memo;

  xml = gnc_glade_xml_new ("print.glade", "Print Check Dialog");
  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, pcd);

  pcd->xml = xml;
  pcd->dialog = glade_xml_get_widget (xml, "Print Check Dialog");

  /* now pick out the relevant child widgets */
  pcd->format_combobox = glade_xml_get_widget (xml, "check_format_combobox");
  pcd->position_combobox = glade_xml_get_widget (xml, "check_position_combobox");

  pcd->custom_table = glade_xml_get_widget (xml, "custom_table");
  pcd->payee_x = GTK_SPIN_BUTTON(glade_xml_get_widget (xml, "payee_x_entry"));
  pcd->payee_y = GTK_SPIN_BUTTON(glade_xml_get_widget (xml, "payee_y_entry"));
  pcd->date_x = GTK_SPIN_BUTTON(glade_xml_get_widget (xml, "date_x_entry"));
  pcd->date_y = GTK_SPIN_BUTTON(glade_xml_get_widget (xml, "date_y_entry"));
  pcd->words_x =
    GTK_SPIN_BUTTON(glade_xml_get_widget (xml, "amount_words_x_entry"));
  pcd->words_y =
    GTK_SPIN_BUTTON(glade_xml_get_widget (xml, "amount_words_y_entry"));
  pcd->number_x =
    GTK_SPIN_BUTTON(glade_xml_get_widget (xml, "amount_numbers_x_entry"));
  pcd->number_y =
    GTK_SPIN_BUTTON(glade_xml_get_widget (xml, "amount_numbers_y_entry"));
  pcd->memo_x = GTK_SPIN_BUTTON(glade_xml_get_widget (xml, "memo_x_entry"));
  pcd->memo_y = GTK_SPIN_BUTTON(glade_xml_get_widget (xml, "memo_y_entry"));
  pcd->translation_x = GTK_SPIN_BUTTON(glade_xml_get_widget (xml, "translation_x_entry"));
  pcd->translation_y = GTK_SPIN_BUTTON(glade_xml_get_widget (xml, "translation_y_entry"));
  pcd->translation_label = glade_xml_get_widget (xml, "translation_label");
  pcd->check_rotation =
    GTK_SPIN_BUTTON(glade_xml_get_widget (xml, "check_rotation_entry"));
  pcd->units_combobox = glade_xml_get_widget (xml, "units_combobox");

  window = GTK_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window);
  gtk_window_set_transient_for(GTK_WINDOW(pcd->dialog), window);

  /* Create and attach the date-format chooser */
  table = glade_xml_get_widget (xml, "options_table");
  pcd->date_format = gnc_date_format_new_without_label();
  gtk_table_attach_defaults(GTK_TABLE(table), pcd->date_format, 1, 3, 2, 7);

  gnc_ui_print_restore_dialog(pcd);
  gnc_restore_window_size(GCONF_SECTION, GTK_WINDOW(pcd->dialog));
  gtk_widget_show_all(pcd->dialog);
}

/********************************************************************\
 * gnc_ui_print_check_dialog_ok_cb
\********************************************************************/

static void
gnc_ui_print_check_dialog_ok_cb(PrintCheckDialog * pcd)
{
  SCM        make_check_format = scm_c_eval_string("make-print-check-format");
  SCM        print_check = scm_c_eval_string("gnc:print-check");
  SCM        format_data;
  SCM        fmt, posn, cust_format, date_format;
  int        sel_option;
  double     multip = 72.0;

  char       * formats[]   = { "quicken", "deluxe", "wallet", "custom" };
  char       * positions[] = { "top", "middle", "bottom", "custom" };

  sel_option = gtk_combo_box_get_active(GTK_COMBO_BOX(pcd->format_combobox));
  if (-1 == sel_option)
    return;
  fmt = scm_str2symbol(formats[sel_option]);

  sel_option = gtk_combo_box_get_active(GTK_COMBO_BOX(pcd->position_combobox));
  if (-1 == sel_option)
    return;
  posn = scm_str2symbol(positions[sel_option]);

  sel_option = gtk_combo_box_get_active(GTK_COMBO_BOX(pcd->units_combobox));
  switch(sel_option) {
  case -1: return;
  case 0:  multip = 72.0; break;   /* inches */
  case 1:  multip = 28.346; break; /* cm */
  case 2:  multip = 2.8346; break; /* mm */
  case 3:  multip = 1.0; break;    /* points */
  }
    
  date_format = scm_makfrom0str
    (gnc_date_format_get_custom(GNC_DATE_FORMAT(pcd->date_format)));
  
  cust_format = 
    SCM_LIST7
    (scm_cons(scm_str2symbol("payee"),
	      SCM_LIST2(scm_make_real(multip*gtk_spin_button_get_value(pcd->payee_x)),
			scm_make_real(multip*gtk_spin_button_get_value(pcd->payee_y)))),
     scm_cons(scm_str2symbol("date"),
	      SCM_LIST2(scm_make_real(multip*gtk_spin_button_get_value(pcd->date_x)),
			scm_make_real(multip*gtk_spin_button_get_value(pcd->date_y)))),
     scm_cons(scm_str2symbol("amount-words"),
	      SCM_LIST2(scm_make_real(multip*gtk_spin_button_get_value(pcd->words_x)),
			scm_make_real(multip*gtk_spin_button_get_value(pcd->words_y)))),
     scm_cons(scm_str2symbol("amount-number"),
	      SCM_LIST2(scm_make_real(multip*gtk_spin_button_get_value(pcd->number_x)),
			scm_make_real(multip*gtk_spin_button_get_value(pcd->number_y)))),
     scm_cons(scm_str2symbol("memo"),
	      SCM_LIST2(scm_make_real(multip*gtk_spin_button_get_value(pcd->memo_x)),
			scm_make_real(multip*gtk_spin_button_get_value(pcd->memo_y)))),
     scm_cons(scm_str2symbol("translate"),
	      SCM_LIST2(scm_make_real(multip*gtk_spin_button_get_value(pcd->translation_x)),
			scm_make_real(multip*gtk_spin_button_get_value(pcd->translation_y)))),
     scm_cons(scm_str2symbol("rotate"),
	      scm_make_real(gtk_spin_button_get_value(pcd->check_rotation))));

  /* hide the window */
  gtk_widget_hide(pcd->dialog);

  /* now call the callback passed in from the scheme side with 
     the format as an arg */
  format_data = scm_apply(make_check_format,
			  SCM_LIST4(fmt, posn, date_format, cust_format),
			  SCM_EOL);

  scm_apply(print_check,
	    SCM_LIST5(format_data,
		      scm_makfrom0str(pcd->payee),
		      scm_make_real(gnc_numeric_to_double (pcd->amount)),
		      scm_ulong2num(pcd->date),
		      scm_makfrom0str(pcd->memo)),
	    SCM_EOL);
  
}


static void
gnc_print_check_set_sensitive (GtkWidget *widget, gpointer data)
{
  gboolean sensitive = GPOINTER_TO_INT(data);
  gtk_widget_set_sensitive(widget, sensitive);
}


void
gnc_print_check_combobox_changed (GtkComboBox *widget,
				  PrintCheckDialog * pcd)
{
  gboolean sensitive;
  gint value;

  value = gtk_combo_box_get_active(GTK_COMBO_BOX(pcd->format_combobox));
  if (-1 == value)
    return;
  sensitive = (value == (CHECK_PRINT_NUM_FORMATS - 1));
  gtk_container_foreach(GTK_CONTAINER(pcd->custom_table),
			gnc_print_check_set_sensitive,
			GINT_TO_POINTER(sensitive));
  if (sensitive == TRUE)
    return;
  
  value = gtk_combo_box_get_active(GTK_COMBO_BOX(pcd->position_combobox));
  if (-1 == value)
    return;
  sensitive = (value == (CHECK_PRINT_NUM_POSITIONS - 1));
  gtk_widget_set_sensitive(GTK_WIDGET(pcd->translation_label), sensitive);
  gtk_widget_set_sensitive(GTK_WIDGET(pcd->translation_x), sensitive);
  gtk_widget_set_sensitive(GTK_WIDGET(pcd->translation_y), sensitive);
}

void
gnc_ui_print_check_response_cb(GtkDialog * dialog,
			       gint response,
			       PrintCheckDialog *pcd)
{
  switch (response) {
    case GTK_RESPONSE_HELP:
      gnc_gnome_help(HF_HELP, HL_PRINTCHECK);
      return;

    case GTK_RESPONSE_OK:
      gnc_ui_print_check_dialog_ok_cb(pcd);
      gnc_ui_print_save_dialog(pcd);
      /* fall through */

    case GTK_RESPONSE_CANCEL:
      gnc_save_window_size(GCONF_SECTION, GTK_WINDOW(dialog));
      break;
  }

  gtk_widget_destroy(pcd->dialog);
  g_object_unref(pcd->xml);
  g_free(pcd);
}
