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

#include "messages.h"
#include "glade-gnc-dialogs.h"
#include "dialog-print-check.h"
#include "dialog-utils.h"
#include "window-help.h"
#include "print-session.h"
#include "gnc-ui.h"

#define CHECK_PRINT_NUM_FORMATS 2
#define CHECK_PRINT_NUM_POSITIONS 4
#define CHECK_PRINT_NUM_DATEFORMATS 9
#define CHECK_PRINT_NUM_UNITS 4


/********************************************************************\
 * gnc_ui_print_check_dialog_create
 * make a new print check dialog and wait for it.
\********************************************************************/

PrintCheckDialog * 
gnc_ui_print_check_dialog_create(SCM callback) {
  PrintCheckDialog * pcd = g_new0(PrintCheckDialog, 1);  

  /* call the glade-defined creator */
  pcd->dialog = create_Print_Check_Dialog();
  pcd->callback = callback;

  /* now pick out the relevant child widgets */
  pcd->format_picker = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                           "check_format_picker");
  pcd->position_picker = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                             "check_position_picker");
  pcd->dformat_picker = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                            "date_format_picker");

  pcd->payee_x = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                     "payee_x_entry");
  pcd->payee_y = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                     "payee_y_entry");
  pcd->date_x = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                     "date_x_entry");
  pcd->date_y = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                     "date_y_entry");
  pcd->words_x = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                     "amount_words_x_entry");
  pcd->words_y = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                     "amount_words_y_entry");
  pcd->number_x = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                     "amount_numbers_x_entry");
  pcd->number_y = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                     "amount_numbers_y_entry");
  pcd->memo_x = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                     "memo_x_entry");
  pcd->memo_y = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                     "memo_y_entry");
  pcd->check_position = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                            "check_position_entry");
  pcd->format_entry = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                            "date_format_entry");

  pcd->units_picker = gtk_object_get_data(GTK_OBJECT(pcd->dialog),
                                          "units_picker");

  /* fix the option menus so we can diagnose which option is 
     selected */
  gnc_option_menu_init(pcd->format_picker);
  gnc_option_menu_init(pcd->position_picker);
  gnc_option_menu_init(pcd->dformat_picker);
  gnc_option_menu_init(pcd->units_picker);

  /* set data so we can find the struct in callbacks */
  gtk_object_set_data(GTK_OBJECT(pcd->dialog), "print_check_structure",
                      pcd);

  scm_protect_object(pcd->callback);

  gtk_widget_show_all(pcd->dialog);

  return pcd;
}


/********************************************************************\
 * gnc_ui_print_check_dialog_destroy
\********************************************************************/

void
gnc_ui_print_check_dialog_destroy(PrintCheckDialog * pcd) {
  gnome_dialog_close(GNOME_DIALOG(pcd->dialog));
  
  scm_unprotect_object(pcd->callback);

  pcd->dialog = NULL;

  g_free(pcd);
}

static double 
entry_to_double(GtkWidget * entry) {
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
                                gpointer user_data) {
  GtkWidget        * dialog = user_data;
  PrintCheckDialog * pcd = 
    gtk_object_get_data(GTK_OBJECT(dialog), "print_check_structure");

  SCM        make_check_format = gh_eval_str("make-print-check-format");
  SCM        callback;
  SCM        fmt, posn, cust_format, date_format;
  GtkWidget  * menu, * menuitem;
  int        sel_option;
  double     multip = 72.0;

  char       * formats[]   = { "quicken", "custom" };
  char       * positions[] = { "top", "middle", "bottom", "custom" };
  char       * dateformats[] = { "%B %e, %Y",
                                 "%e %B, %Y",
                                 "%b %e, %Y",
                                 "%e %b, %Y",
                                 "%m/%d/%Y",
                                 "%m/%d/%y",
                                 "%d/%m/%Y",
                                 "%d/%m/%y",
                                 "custom" };
                                 
  menu       = gtk_option_menu_get_menu(GTK_OPTION_MENU(pcd->format_picker));
  menuitem   = gtk_menu_get_active(GTK_MENU(menu));
  sel_option = GPOINTER_TO_INT(gtk_object_get_data(GTK_OBJECT(menuitem),
                                                   "option_index"));
  fmt        = gh_symbol2scm(formats[sel_option]);

  menu       = gtk_option_menu_get_menu(GTK_OPTION_MENU(pcd->position_picker));
  menuitem   = gtk_menu_get_active(GTK_MENU(menu));
  sel_option = GPOINTER_TO_INT(gtk_object_get_data(GTK_OBJECT(menuitem),
                                                   "option_index"));
  posn       = gh_symbol2scm(positions[sel_option]);

  menu       = gtk_option_menu_get_menu(GTK_OPTION_MENU(pcd->units_picker));
  menuitem   = gtk_menu_get_active(GTK_MENU(menu));
  sel_option = GPOINTER_TO_INT(gtk_object_get_data(GTK_OBJECT(menuitem),
                                                   "option_index"));
  switch(sel_option) {
  case 0:  multip = 72.0; break;   /* inches */
  case 1:  multip = 28.346; break; /* cm */
  case 2:  multip = 2.8346; break; /* mm */
  case 3:  multip = 1.0; break;    /* points */
  }
    
  menu       = gtk_option_menu_get_menu(GTK_OPTION_MENU(pcd->dformat_picker));
  menuitem   = gtk_menu_get_active(GTK_MENU(menu));
  sel_option = GPOINTER_TO_INT(gtk_object_get_data(GTK_OBJECT(menuitem),
                                                   "option_index"));
  date_format = gh_str02scm(dateformats[sel_option]);
  
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
                                    gpointer user_data) {
  PrintCheckDialog * pcd =
    gtk_object_get_data(GTK_OBJECT(user_data), "print_check_structure");

  gnc_ui_print_check_dialog_destroy(pcd);
}

/********************************************************************\
 * gnc_ui_print_check_dialog_help_cb
\********************************************************************/

void
gnc_ui_print_check_dialog_help_cb(GtkButton * button, 
                                  gpointer user_data) {
  helpWindow(NULL, NULL, HH_PRINTCHECK);
}

