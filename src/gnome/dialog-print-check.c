/********************************************************************\
 * dialog-print-check.c : dialog to control check printing.         *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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

#define _GNU_SOURCE

#include "config.h"

#include <stdio.h>
#include <gnome.h>
#include <libguile.h>

#include "gnc-date.h"
#include "messages.h"
#include "gnc-numeric.h"
#include "gnc-plugin-page-register.h"
#include "dialog-print-check.h"
#include "dialog-utils.h"
#include "print-session.h"
#include "gnc-ui.h"
#include "gnc-date-format.h"

#define CHECK_PRINT_NUM_FORMATS 3
#define CHECK_PRINT_NUM_POSITIONS 4
#define CHECK_PRINT_NUM_UNITS 4

#define PRINT_CHECK_DATA "print-check-data"

/* Used by glade_xml_signal_autoconnect_full */
void gnc_ui_print_check_dialog_ok_cb(GtkButton * button, gpointer user_data);
void gnc_ui_print_check_dialog_cancel_cb(GtkButton * button, gpointer user_data);
void gnc_ui_print_check_dialog_help_cb(GtkButton * button, gpointer user_data);


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

  pcd = g_object_get_data(G_OBJECT(plugin_page), PRINT_CHECK_DATA);
  if (pcd) {
    pcd->payee = payee;
    pcd->amount = amount;
    pcd->date = date;
    pcd->memo = memo;
    gnc_date_format_refresh(GNC_DATE_FORMAT(pcd->date_format));
    gtk_window_present (GTK_WINDOW(pcd->dialog));
    return;
  }

  pcd = g_new0(PrintCheckDialog, 1);
  g_object_set_data(G_OBJECT(plugin_page), PRINT_CHECK_DATA, pcd);
  pcd->plugin_page = plugin_page;
  pcd->payee = payee;
  pcd->amount = amount;
  pcd->date = date;
  pcd->memo = memo;

  xml = gnc_glade_xml_new ("print.glade", "Print Check Dialog");
  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, pcd);

  pcd->dialog = glade_xml_get_widget (xml, "Print Check Dialog");

  /* now pick out the relevant child widgets */
  pcd->format_picker = glade_xml_get_widget (xml, "check_format_picker");
  pcd->position_picker = glade_xml_get_widget (xml, "check_position_picker");

  pcd->payee_x = glade_xml_get_widget (xml, "payee_x_entry");
  pcd->payee_y = glade_xml_get_widget (xml, "payee_y_entry");
  pcd->date_x = glade_xml_get_widget (xml, "date_x_entry");
  pcd->date_y = glade_xml_get_widget (xml, "date_y_entry");
  pcd->words_x = glade_xml_get_widget (xml, "amount_words_x_entry");
  pcd->words_y = glade_xml_get_widget (xml, "amount_words_y_entry");
  pcd->number_x = glade_xml_get_widget (xml, "amount_numbers_x_entry");
  pcd->number_y = glade_xml_get_widget (xml, "amount_numbers_y_entry");
  pcd->memo_x = glade_xml_get_widget (xml, "memo_x_entry");
  pcd->memo_y = glade_xml_get_widget (xml, "memo_y_entry");
  pcd->check_position = glade_xml_get_widget (xml, "check_position_entry");
  pcd->format_entry = glade_xml_get_widget (xml, "date_format_entry");
  pcd->units_picker = glade_xml_get_widget (xml, "units_picker");

  /* fix the option menus so we can diagnose which option is 
     selected */
  gnc_option_menu_init(pcd->format_picker);
  gnc_option_menu_init(pcd->position_picker);
  gnc_option_menu_init(pcd->units_picker);

  window = GTK_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window);
  gtk_window_set_transient_for(GTK_WINDOW(pcd->dialog), window);

  /* Create and attach the date-format chooser */
  table = glade_xml_get_widget (xml, "options_table");
  pcd->date_format = gnc_date_format_new_without_label();
  gtk_table_attach_defaults(GTK_TABLE(table), pcd->date_format, 1, 4, 2, 7);

  gtk_widget_show_all(pcd->dialog);
}


/********************************************************************\
 * gnc_ui_print_check_dialog_destroy
 *
 * Don't destroy the dialog until the program exits. This will
 * maintain *all* user settings from invocation to invocation.
 *
\********************************************************************/

void
gnc_ui_print_check_dialog_destroy(PrintCheckDialog * pcd)
{
  gtk_widget_destroy(pcd->dialog);
  pcd->dialog = NULL;

  g_object_set_data(G_OBJECT(pcd->plugin_page), PRINT_CHECK_DATA, NULL);
  g_free(pcd);
}

static void
gnc_ui_print_check_dialog_hide(PrintCheckDialog * pcd)
{
  gtk_widget_hide(pcd->dialog);
}

static double 
entry_to_double(GtkWidget * entry)
{
  const char  * text = gtk_entry_get_text(GTK_ENTRY(entry));
  double retval = 0.0;
  
  sscanf(text, "%le", &retval);
  
  return retval;
}
  
/********************************************************************\
 * gnc_ui_print_check_dialog_ok_cb
\********************************************************************/

void
gnc_ui_print_check_dialog_ok_cb(GtkButton * button, 
                                gpointer user_data)
{
  PrintCheckDialog * pcd = user_data;

  SCM        make_check_format = scm_c_eval_string("make-print-check-format");
  SCM        print_check = scm_c_eval_string("gnc:print-check");
  SCM        format_data;
  SCM        fmt, posn, cust_format, date_format;
  int        sel_option;
  double     multip = 72.0;

  char       * formats[]   = { "quicken", "deluxe", "wallet", "custom" };
  char       * positions[] = { "top", "middle", "bottom", "custom" };

  sel_option = gnc_option_menu_get_active(pcd->format_picker);
  fmt        = scm_str2symbol(formats[sel_option]);

  sel_option = gnc_option_menu_get_active(pcd->position_picker);
  posn       = scm_str2symbol(positions[sel_option]);

  sel_option = gnc_option_menu_get_active(pcd->units_picker);
  switch(sel_option) {
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
	      SCM_LIST2(scm_make_real(multip*entry_to_double(pcd->payee_x)),
			scm_make_real(multip*entry_to_double(pcd->payee_y)))),
     scm_cons(scm_str2symbol("date"),
	      SCM_LIST2(scm_make_real(multip*entry_to_double(pcd->date_x)),
			scm_make_real(multip*entry_to_double(pcd->date_y)))),
     scm_cons(scm_str2symbol("amount-words"),
	      SCM_LIST2(scm_make_real(multip*entry_to_double(pcd->words_x)),
			scm_make_real(multip*entry_to_double(pcd->words_y)))),
     scm_cons(scm_str2symbol("amount-number"),
	      SCM_LIST2(scm_make_real(multip*entry_to_double(pcd->number_x)),
			scm_make_real(multip*entry_to_double(pcd->number_y)))),
     scm_cons(scm_str2symbol("memo"),
	      SCM_LIST2(scm_make_real(multip*entry_to_double(pcd->memo_x)),
			scm_make_real(multip*entry_to_double(pcd->memo_y)))),
     scm_cons(scm_str2symbol("position"),
	      scm_make_real(multip*entry_to_double(pcd->check_position))),
     scm_cons(scm_str2symbol("date-format"),
	      scm_makfrom0str(gtk_entry_get_text(GTK_ENTRY(pcd->format_entry)))));

  /* hide the window */
  gnc_ui_print_check_dialog_hide(pcd);

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


/********************************************************************\
 * gnc_ui_print_check_dialog_cancel_cb
\********************************************************************/

void
gnc_ui_print_check_dialog_cancel_cb(GtkButton * button, 
                                    gpointer user_data)
{
  PrintCheckDialog * pcd = user_data;

  gnc_ui_print_check_dialog_hide(pcd);
}

/********************************************************************\
 * gnc_ui_print_check_dialog_help_cb
\********************************************************************/

void
gnc_ui_print_check_dialog_help_cb(GtkButton * button, 
                                  gpointer user_data)
{
  gnc_gnome_help(HF_USAGE, HL_PRINTCHECK);
}
