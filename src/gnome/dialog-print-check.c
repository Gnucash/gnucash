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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#define _GNU_SOURCE

#include "config.h"

#include <stdio.h>
#include <gnome.h>
#include <guile/gh.h>

#include "date.h"
#include "messages.h"
#include "dialog-print-check.h"
#include "dialog-utils.h"
#include "window-help.h"
#include "print-session.h"
#include "gnc-ui.h"

#define CHECK_PRINT_NUM_FORMATS 2
#define CHECK_PRINT_NUM_POSITIONS 4
#define CHECK_PRINT_NUM_DATEFORMATS 9
#define CHECK_PRINT_NUM_UNITS 4

#define MAX_DATE_LEN 80

/* Used by glade_xml_signal_autoconnect_full */
void gnc_ui_print_check_dialog_ok_cb(GtkButton * button, 
				     gpointer user_data);
void gnc_ui_print_check_dialog_cancel_cb(GtkButton * button, 
					 gpointer user_data);
void gnc_ui_print_check_dialog_help_cb(GtkButton * button, 
				       gpointer user_data);
void gnc_ui_print_check_format_changed_cb(GtkWidget *unused, 
					  gpointer user_data);

static gboolean saved_include_century = TRUE;
static gboolean saved_month_name = FALSE;
static gboolean saved_month_name_long = FALSE;

static int gnc_ui_print_get_option_menu_item (GtkWidget *widget)
{
  GtkWidget  * menu, * menuitem;

  menu     = gtk_option_menu_get_menu(GTK_OPTION_MENU(widget));
  menuitem = gtk_menu_get_active(GTK_MENU(menu));
  return GPOINTER_TO_INT(gtk_object_get_data(GTK_OBJECT(menuitem),
					     "option_index"));
}

static void
gnc_ui_print_enable_month (PrintCheckDialog *pcd, gboolean sensitive)
{
  gtk_widget_set_sensitive(pcd->month_label, sensitive);
  gtk_widget_set_sensitive(pcd->month_name, sensitive);
}

static void
gnc_ui_print_enable_year (PrintCheckDialog *pcd, gboolean sensitive)
{
  gtk_widget_set_sensitive(pcd->year_label, sensitive);
  gtk_widget_set_sensitive(pcd->include_century, sensitive);
}

static void
gnc_ui_print_enable_format (PrintCheckDialog *pcd, gboolean sensitive)
{
  gtk_widget_set_sensitive(pcd->custom_label, sensitive);
  gtk_widget_set_sensitive(pcd->custom_format, sensitive);
}

static void
gnc_ui_print_compute_new_format (PrintCheckDialog *pcd)
{
  int sel_option = gnc_ui_print_get_option_menu_item(pcd->dformat_picker);
  static gchar *format, *c;
  gchar date_string[MAX_DATE_LEN];
  time_t secs_now;
  struct tm today;

  if (pcd->format_string) {
    g_free(pcd->format_string);
    pcd->format_string = NULL;
  }

  if (sel_option >= DATE_FORMAT_LOCALE) {
    format = g_strdup(gtk_entry_get_text(GTK_ENTRY(pcd->custom_format)));
    gtk_widget_set_sensitive(pcd->month_name_long, FALSE);
    gnc_ui_print_enable_month(pcd, FALSE);
    gnc_ui_print_enable_year(pcd, FALSE);
    gnc_ui_print_enable_format(pcd, TRUE);
    goto finish;
  }

  gnc_ui_print_enable_year(pcd, TRUE);
  gnc_ui_print_enable_format(pcd, FALSE);
  if (sel_option == DATE_FORMAT_ISO) {
    gnc_ui_print_enable_month(pcd, FALSE);
    gtk_widget_set_sensitive(pcd->month_name_long, FALSE);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pcd->month_name),
				 FALSE);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pcd->month_name_long),
				 FALSE);
  } else {
    gnc_ui_print_enable_month(pcd, TRUE);
  }

  if (!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pcd->month_name))) {
    format = g_strdup(getDateFormatString(sel_option));
    gtk_widget_set_sensitive(pcd->month_name_long, FALSE);
  } else {
    format = g_strdup(getDateTextFormatString(sel_option));
    gtk_widget_set_sensitive(pcd->month_name_long, TRUE);
    if (gtk_toggle_button_get_active
	(GTK_TOGGLE_BUTTON(pcd->month_name_long))) {
      c = strchr(format, 'b');
      if (c)
	*c = 'B';
    }
  }
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pcd->include_century))) {
    c = strchr(format, 'y');
    if (c)
      *c = 'Y';
  }
  
finish:
  pcd->format_string = format;
  secs_now = time(NULL);
  localtime_r(&secs_now, &today);
  strftime(date_string, MAX_DATE_LEN, format, &today);
  gtk_label_set_text(GTK_LABEL(pcd->sample_date), date_string);
}

void
gnc_ui_print_check_format_changed_cb(GtkWidget *unused, 
				     gpointer user_data)
{
  PrintCheckDialog * pcd = user_data;

  gnc_ui_print_compute_new_format(pcd);
}



/********************************************************************\
 * gnc_ui_print_check_dialog_create
 * make a new print check dialog and wait for it.
\********************************************************************/

PrintCheckDialog * 
gnc_ui_print_check_dialog_create(SCM callback)
{
  PrintCheckDialog * pcd = g_new0(PrintCheckDialog, 1);
  GladeXML *xml;

  xml = gnc_glade_xml_new ("print.glade", "Print Check Dialog");

  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, pcd);

  pcd->dialog = glade_xml_get_widget (xml, "Print Check Dialog");
  pcd->callback = callback;

  /* now pick out the relevant child widgets */
  pcd->format_picker = glade_xml_get_widget (xml, "check_format_picker");
  pcd->position_picker = glade_xml_get_widget (xml, "check_position_picker");
  pcd->dformat_picker = glade_xml_get_widget (xml, "date_format_picker");

  pcd->month_label = glade_xml_get_widget (xml, "month_label");
  pcd->month_name = glade_xml_get_widget (xml, "month_name");
  pcd->month_name_long = glade_xml_get_widget (xml, "month_name_long");
  pcd->year_label = glade_xml_get_widget (xml, "year_label");
  pcd->include_century = glade_xml_get_widget (xml, "include_century");
  pcd->sample_date = glade_xml_get_widget (xml, "sample_date");
  pcd->custom_label = glade_xml_get_widget (xml, "custom_label");
  pcd->custom_format = glade_xml_get_widget (xml, "custom_format");

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

  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pcd->include_century),
			       saved_include_century);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pcd->month_name),
			       saved_month_name);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pcd->month_name_long),
			       saved_month_name_long);

  /* fix the option menus so we can diagnose which option is 
     selected */
  gnc_option_menu_init(pcd->format_picker);
  gnc_option_menu_init(pcd->position_picker);
  gnc_option_menu_init_w_signal(pcd->dformat_picker,
				gnc_ui_print_check_format_changed_cb, pcd);
  gnc_option_menu_init(pcd->units_picker);

  scm_protect_object(pcd->callback);

  /* Set initial format to gnucash default */
  gtk_option_menu_set_history(GTK_OPTION_MENU(pcd->dformat_picker),
			      getDateFormat());
  gnc_ui_print_compute_new_format(pcd);

  gtk_widget_show_all(pcd->dialog);

  return pcd;
}


/********************************************************************\
 * gnc_ui_print_check_dialog_destroy
\********************************************************************/

void
gnc_ui_print_check_dialog_destroy(PrintCheckDialog * pcd)
{
  gnome_dialog_close(GNOME_DIALOG(pcd->dialog));
  
  scm_unprotect_object(pcd->callback);

  if (pcd->format_string)
    g_free(pcd->format_string);
  pcd->dialog = NULL;

  g_free(pcd);
}

static double 
entry_to_double(GtkWidget * entry)
{
  char  * text = gtk_entry_get_text(GTK_ENTRY(entry));
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

  SCM        make_check_format = gh_eval_str("make-print-check-format");
  SCM        callback;
  SCM        fmt, posn, cust_format, date_format;
  int        sel_option;
  double     multip = 72.0;

  char       * formats[]   = { "quicken", "custom" };
  char       * positions[] = { "top", "middle", "bottom", "custom" };

  saved_include_century =
    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pcd->include_century));
  saved_month_name =
    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pcd->month_name));
  saved_month_name_long =
    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pcd->month_name_long));

  sel_option = gnc_ui_print_get_option_menu_item(pcd->format_picker);
  fmt        = gh_symbol2scm(formats[sel_option]);

  sel_option = gnc_ui_print_get_option_menu_item(pcd->position_picker);
  posn       = gh_symbol2scm(positions[sel_option]);

  sel_option = gnc_ui_print_get_option_menu_item(pcd->units_picker);
  switch(sel_option) {
  case 0:  multip = 72.0; break;   /* inches */
  case 1:  multip = 28.346; break; /* cm */
  case 2:  multip = 2.8346; break; /* mm */
  case 3:  multip = 1.0; break;    /* points */
  }
    
  date_format = gh_str02scm(pcd->format_string);
  
  cust_format = 
    SCM_LIST7
    (gh_cons(gh_symbol2scm("payee"),
             SCM_LIST2(gh_double2scm(multip*entry_to_double(pcd->payee_x)),
                       gh_double2scm(multip*entry_to_double(pcd->payee_y)))),
     gh_cons(gh_symbol2scm("date"),
             SCM_LIST2(gh_double2scm(multip*entry_to_double(pcd->date_x)),
                       gh_double2scm(multip*entry_to_double(pcd->date_y)))),
     gh_cons(gh_symbol2scm("amount-words"),
             SCM_LIST2(gh_double2scm(multip*entry_to_double(pcd->words_x)),
                       gh_double2scm(multip*entry_to_double(pcd->words_y)))),
     gh_cons(gh_symbol2scm("amount-number"),
             SCM_LIST2(gh_double2scm(multip*entry_to_double(pcd->number_x)),
                       gh_double2scm(multip*entry_to_double(pcd->number_y)))),
     gh_cons(gh_symbol2scm("memo"),
             SCM_LIST2(gh_double2scm(multip*entry_to_double(pcd->memo_x)),
                       gh_double2scm(multip*entry_to_double(pcd->memo_y)))),
     gh_cons(gh_symbol2scm("position"),
             gh_double2scm(multip*entry_to_double(pcd->check_position))),
     gh_cons(gh_symbol2scm("date-format"),
             gh_str02scm(gtk_entry_get_text(GTK_ENTRY(pcd->format_entry)))));

  callback = pcd->callback;
  
  /* destroy the window */
  gnc_ui_print_check_dialog_destroy(pcd);

  /* now call the callback passed in from the scheme side with 
     the format as an arg */
  gh_call1(callback,
           gh_apply(make_check_format,
                    SCM_LIST4(fmt, posn, date_format, cust_format)));
  
}


/********************************************************************\
 * gnc_ui_print_check_dialog_cancel_cb
\********************************************************************/

void
gnc_ui_print_check_dialog_cancel_cb(GtkButton * button, 
                                    gpointer user_data)
{
  PrintCheckDialog * pcd = user_data;

  gnc_ui_print_check_dialog_destroy(pcd);
}

/********************************************************************\
 * gnc_ui_print_check_dialog_help_cb
\********************************************************************/

void
gnc_ui_print_check_dialog_help_cb(GtkButton * button, 
                                  gpointer user_data)
{
  helpWindow(NULL, NULL, HH_PRINTCHECK);
}
