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
#include <libguile.h>
#include "guile-mappings.h"

#include "date.h"
#include "messages.h"
#include "gnc-numeric.h"
#include "window-register.h"
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
  gtk_widget_set_sensitive(pcd->month_num, sensitive);
  gtk_widget_set_sensitive(pcd->month_abbrev, sensitive);
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
  gboolean enable_year, enable_month, enable_custom, check_modifiers;
  static gchar *format, *c;
  gchar date_string[MAX_DATE_LEN];
  time_t secs_now;
  struct tm today;

  switch (sel_option) {
   case DATE_FORMAT_CUSTOM:
    format = g_strdup(gtk_entry_get_text(GTK_ENTRY(pcd->custom_format)));
    enable_year = enable_month = check_modifiers = FALSE;
    enable_custom = TRUE;
    break;

   case DATE_FORMAT_LOCALE:
    format = g_strdup(getDateFormatString(DATE_FORMAT_LOCALE));
    enable_year = enable_month = check_modifiers = enable_custom = FALSE;
    break;

   case DATE_FORMAT_ISO:
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pcd->month_num), TRUE);
    enable_year = check_modifiers = TRUE;
    enable_month = enable_custom = FALSE;
    break;

   default:
    enable_year = enable_month = check_modifiers = TRUE;
    enable_custom = FALSE;
    break;
  }

  /* Tweak widget sensitivities, as appropriate. */
  gnc_ui_print_enable_year(pcd, enable_year);
  gnc_ui_print_enable_month(pcd, enable_month);
  gnc_ui_print_enable_format(pcd, enable_custom);

  /* Update the format string based upon the user's preferences */
  if (check_modifiers) {
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pcd->month_num))) {
      format = g_strdup(getDateFormatString(sel_option));
    } else {
      format = g_strdup(getDateTextFormatString(sel_option));
      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pcd->month_name))) {
	c = strchr(format, 'b');
	if (c)
	  *c = 'B';
      }
    }
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pcd->include_century))){
      c = strchr(format, 'y');
      if (c)
	*c = 'Y';
    }
  }

  /*
   * Give feedback on the format string so users can see how it works
   * without having to read the strftime man page. Prevent recursive
   * signals.
   */
  gtk_signal_handler_block_by_data(GTK_OBJECT(pcd->custom_format), pcd);
  gtk_entry_set_text(GTK_ENTRY(pcd->custom_format), format);
  gtk_signal_handler_unblock_by_data(GTK_OBJECT(pcd->custom_format), pcd);
  
  /* Save the format string for when OK is clicked */
  if (pcd->format_string)
    g_free(pcd->format_string);
  pcd->format_string = format;

  /* Visual feedback on what the date will look like. */
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

void
gnc_ui_print_check_dialog_create(RegWindow     *reg_data,
				 const char    *payee,
				 gnc_numeric    amount,
				 time_t         date,
				 const char    *memo)
{
  PrintCheckDialog * pcd;
  GladeXML *xml;

  pcd = (PrintCheckDialog *)gnc_RegWindow_get_pcd(reg_data);
  if (pcd) {
    pcd->payee = payee;
    pcd->amount = amount;
    pcd->date = date;
    pcd->memo = memo;
    gnc_ui_print_compute_new_format(pcd);
    gtk_window_present (GTK_WINDOW(pcd->dialog));
    return;
  }

  pcd = g_new0(PrintCheckDialog, 1);
  gnc_RegWindow_set_pcd(reg_data, pcd);
  pcd->reg_data = reg_data;
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
  pcd->dformat_picker = glade_xml_get_widget (xml, "date_format_picker");

  pcd->month_label = glade_xml_get_widget (xml, "month_label");
  pcd->month_num = glade_xml_get_widget (xml, "month_num");
  pcd->month_abbrev = glade_xml_get_widget (xml, "month_abbrev");
  pcd->month_name = glade_xml_get_widget (xml, "month_name");
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

  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pcd->include_century), TRUE);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pcd->month_num), TRUE);

  /* fix the option menus so we can diagnose which option is 
     selected */
  gnc_option_menu_init(pcd->format_picker);
  gnc_option_menu_init(pcd->position_picker);
  gnc_option_menu_init_w_signal(pcd->dformat_picker,
				gnc_ui_print_check_format_changed_cb, pcd);
  gnc_option_menu_init(pcd->units_picker);

  /* Set initial format to gnucash default */
  gtk_option_menu_set_history(GTK_OPTION_MENU(pcd->dformat_picker),
			      getDateFormat());
  gnc_ui_print_compute_new_format(pcd);

  gnome_dialog_set_parent(GNOME_DIALOG(pcd->dialog),
			  GTK_WINDOW(gnc_RegWindow_window(reg_data)));
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
  if (pcd->format_string)
    g_free(pcd->format_string);

  gnome_dialog_close(GNOME_DIALOG(pcd->dialog));
  gtk_widget_destroy(pcd->dialog);
  pcd->dialog = NULL;

  gnc_RegWindow_set_pcd(pcd->reg_data, NULL);
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

  SCM        make_check_format = scm_c_eval_string("make-print-check-format");
  SCM        print_check = scm_c_eval_string("gnc:print-check");
  SCM        format_data;
  SCM        fmt, posn, cust_format, date_format;
  int        sel_option;
  double     multip = 72.0;

  char       * formats[]   = { "quicken", "custom" };
  char       * positions[] = { "top", "middle", "bottom", "custom" };

  sel_option = gnc_ui_print_get_option_menu_item(pcd->format_picker);
  fmt        = scm_str2symbol(formats[sel_option]);

  sel_option = gnc_ui_print_get_option_menu_item(pcd->position_picker);
  posn       = scm_str2symbol(positions[sel_option]);

  sel_option = gnc_ui_print_get_option_menu_item(pcd->units_picker);
  switch(sel_option) {
  case 0:  multip = 72.0; break;   /* inches */
  case 1:  multip = 28.346; break; /* cm */
  case 2:  multip = 2.8346; break; /* mm */
  case 3:  multip = 1.0; break;    /* points */
  }
    
  date_format = scm_makfrom0str(pcd->format_string);
  
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
  helpWindow(NULL, NULL, HH_PRINTCHECK);
}
