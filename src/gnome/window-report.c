/********************************************************************\
 * gnc-report-window.c
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
 * Copyright (C) 1999 Jeremy Collins ( gtk-xmhtml port )            *
 * Copyright (C) 2000 Dave Peticolas                                *
 * Copyright (C) 2000 Bill Gribble                                  *
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
 *                                                                  *
\********************************************************************/

#include "config.h"
#include <gnome.h>

#include <guile/gh.h>

#include "glade-gnc-dialogs.h"
#include "window-report.h"
#include "gnc-html.h"
#include "gnc-html-history.h"
#include "dialog-options.h"
#include "query-user.h"

struct _gnc_report_window {
  GtkWidget   * toplevel;  
  GtkWidget   * paned;

  GtkWidget   * popup;
  GtkWidget   * back_widg;
  GtkWidget   * fwd_widg;

  GNCOptionWin * option_dialog;
  GNCOptionDB  * odb;
  SCM          scm_options;

  gnc_html   * html;
};

/* all open report windows... makes cleanup easier */
static GList * open_report_windows = NULL;


/********************************************************************
 * gnc_report_window_check_urltype
 * is it OK to show a certain URLType in this window?
 ********************************************************************/

static int
gnc_report_window_check_urltype(URLType t) {
  switch (t) {
  case URL_TYPE_REPORT:
    return TRUE;
    break;
  default:
    return FALSE;
    break;
  }
}


/********************************************************************
 * toolbar callbacks
 ********************************************************************/

static int 
gnc_report_window_fwd_cb(GtkWidget * w, gpointer data) {
  gnc_report_window     * report = data;
  gnc_html_history_node * node = NULL;

  gnc_html_history_forward(gnc_html_get_history(report->html));
  node = gnc_html_history_get_current(gnc_html_get_history(report->html));
  if(node) {
    gnc_html_show_url(report->html, node->type, node->location, 
                      node->label, 0);
  }
  return TRUE;
}

static int 
gnc_report_window_back_cb(GtkWidget * w, gpointer data) {
  gnc_report_window     * report = data;
  gnc_html_history_node * node;
  
  gnc_html_history_back(gnc_html_get_history(report->html));
  node = gnc_html_history_get_current(gnc_html_get_history(report->html));
  if(node) {
    gnc_html_show_url(report->html, node->type, node->location, 
                      node->label, 0);
  }
  return TRUE;
}


static int
gnc_report_window_stop_button_cb(GtkWidget * w, gpointer data) {
  gnc_report_window       * report = data;
  gnc_html_cancel(report->html);
  return TRUE;
}

static int
gnc_report_window_reload_button_cb(GtkWidget * w, gpointer data) {
  gnc_report_window       * report = data;
  gnc_html_reload(report->html);
  return TRUE;
}


static void
gnc_report_window_set_back_button(gnc_report_window * win, int enabled) {
  gtk_widget_set_sensitive(win->back_widg, enabled);
}


static void
gnc_report_window_set_fwd_button(gnc_report_window * win, int enabled) {
  gtk_widget_set_sensitive(win->fwd_widg, enabled);
}


static void
gnc_options_dialog_apply_cb(GNCOptionWin * propertybox,
			    gpointer user_data) {
  gnc_report_window * win = user_data;
  gnc_option_db_commit(win->odb);
  gnc_html_reload(win->html);
}

static void
gnc_options_dialog_help_cb(GNCOptionWin * propertybox,
			   gpointer user_data) {
  gnome_ok_dialog("Set the report options you want using this dialog.");
}

static void
gnc_options_dialog_close_cb(GNCOptionWin * propertybox,
                            gpointer user_data) {
  gnc_report_window * win = user_data;
  gtk_widget_hide(gnc_options_dialog_widget(win->option_dialog));
  gtk_paned_set_position(GTK_PANED(win->paned), 0);
}


/********************************************************************
 * gnc_report_window_load_cb
 * called after a report is loaded into the gnc_html widget 
 ********************************************************************/

static void 
gnc_report_window_load_cb(gnc_html * html, URLType type, 
                          const gchar * location, const gchar * label, 
                          gpointer data) {
  gnc_report_window * win = data;
  int  report_id;
  SCM  find_report = gh_eval_str("gnc:find-report");
  SCM  get_options = gh_eval_str("gnc:report-options");
  SCM  inst_report;
  SCM  inst_options;
  
  if(!strncmp("id=", location, 3)) {
    sscanf(location+3, "%d", &report_id);
  }
  else {
    return;
  }
  
  /* get the inst-report from the Scheme-side hash, and get its
   * options */
  inst_report   = gh_call1(find_report, gh_int2scm(report_id));
  inst_options  = gh_call1(get_options, inst_report); 

  /* get rid of the options dialog, unless it's just for a reload */
  if(!gh_eq_p(inst_options, win->scm_options)) {
    if(win->odb) 
      gnc_option_db_destroy(win->odb);

    if(win->option_dialog) 
      gnc_options_dialog_destroy(win->option_dialog);

    win->odb           = NULL;
    win->option_dialog = NULL;
  }

  if(inst_options != SCM_BOOL_F) {  
    if(!win->odb) {
      win->odb           = gnc_option_db_new(inst_options);
      gnc_option_db_clean(win->odb);
    }
    if(!win->option_dialog) {
      win->option_dialog = gnc_options_dialog_new(FALSE);
      gtk_paned_add1(GTK_PANED(win->paned), 
                     gnc_options_dialog_widget(win->option_dialog));
      gnc_build_options_dialog_contents(win->option_dialog,
                                        win->odb);
      gtk_widget_show_all(win->paned);
      gtk_widget_hide(gnc_options_dialog_widget(win->option_dialog));
    }
    
    gnc_options_dialog_set_apply_cb(win->option_dialog,
                                    gnc_options_dialog_apply_cb,
                                    win);
    
    gnc_options_dialog_set_help_cb(win->option_dialog,
                                   gnc_options_dialog_help_cb,
                                   win);

    gnc_options_dialog_set_close_cb(win->option_dialog,
                                    gnc_options_dialog_close_cb,
                                    win);
  }
  
  scm_unprotect_object(win->scm_options);
  win->scm_options = inst_options;
  scm_protect_object(win->scm_options);

  if(gnc_html_history_forward_p(gnc_html_get_history(win->html))) {
    gnc_report_window_set_fwd_button(win, TRUE); 
  }
  else {
    gnc_report_window_set_fwd_button(win, FALSE); 
  }

  if(gnc_html_history_back_p(gnc_html_get_history(win->html))) {
    gnc_report_window_set_back_button(win, TRUE); 
  }
  else {
    gnc_report_window_set_back_button(win, FALSE); 
  }
}



/********************************************************************
 * gnc_report_window_destroy_cb
 ********************************************************************/

static void
gnc_report_window_destroy_cb(GtkWidget * w, gpointer data) {
  gnc_report_window * rw = data;
  gnc_report_window_destroy(rw);
}


/********************************************************************
 * gnc_report_window_button_cb
 * mouse button clicks
 ********************************************************************/

static int
gnc_report_window_button_cb(gnc_html * html, GdkEventButton * event,
                            gpointer user_data) {
  gnc_report_window * win = (gnc_report_window *)user_data;
  
  if(event->type == GDK_BUTTON_PRESS) {
    if(event->button == 3) {      
      if(win->popup) {
        gnome_popup_menu_do_popup(win->popup, NULL, NULL, 
                                  event, (gpointer)win);
        return TRUE;
      }
    }
  }
  return FALSE;
}

static int
gnc_report_window_params_cb(GtkWidget * w, gpointer data) {
  gnc_report_window * win = data;
  if (!win->option_dialog)
    return TRUE;
  gtk_widget_show(gnc_options_dialog_widget(win->option_dialog));
  gtk_paned_set_position(GTK_PANED(win->paned), 400);
  return TRUE;
}

static void
gnc_report_window_print_cb(GtkWidget * w, gpointer data) {
  gnc_report_window * win = data;
  gnc_html_print(win->html);
}


/********************************************************************
 * gnc_report_window_new 
 * allocates and opens up a report window
 ********************************************************************/

gnc_report_window *
gnc_report_window_new(GtkWidget * container) {
  
  gnc_report_window * report = g_new0(gnc_report_window, 1);
  GtkObject         * tlo; 
  GtkWidget         * toolbar = NULL;

  GnomeUIInfo       toolbar_data[] = 
  {
    { GNOME_APP_UI_ITEM,
      _("Back"),
      _("Move back one step in the history"),
      gnc_report_window_back_cb, report,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_BACK,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      _("Forward"),
      _("Move forward one step in the history"),
      gnc_report_window_fwd_cb, report,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_FORWARD,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      N_("Reload"),
      N_("Reload the current report"),
      gnc_report_window_reload_button_cb, report,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_REFRESH,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      N_("Stop"),
      N_("Cancel outstanding HTML requests"),
      gnc_report_window_stop_button_cb, report,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_STOP,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM,
      _("Export"),
      _("Export HTML-formatted report to file"),
      NULL, report,
      NULL,
      GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_CONVERT,
      0, 0, NULL
    },    
    { GNOME_APP_UI_ITEM,
      _("Parameters"),
      _("Edit report options"),
      gnc_report_window_params_cb, report,
      NULL,
      GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_PROPERTIES,
      0, 0, NULL
    },    
    { GNOME_APP_UI_ITEM,
      _("Print"),
      _("Print report window"),
      gnc_report_window_print_cb, report,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_PRINT,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM,
      _("Close"),
      _("Close this report window"),
      gnc_report_window_destroy_cb, report,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_CLOSE,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };
  
  report->html        = gnc_html_new();
  report->scm_options = SCM_BOOL_F;
  scm_protect_object(report->scm_options);

  if(container) {
    report->toplevel = container;    
    report->paned    = gtk_hpaned_new();
    gtk_paned_add2(GTK_PANED(report->paned), 
                   gnc_html_get_widget(report->html));
    gtk_container_add(GTK_CONTAINER(container), report->paned);
    report->popup = gnome_popup_menu_new(toolbar_data);
  }
  else {
    report->toplevel    = create_Report_Window();
  
    tlo = GTK_OBJECT(report->toplevel);
    toolbar         = gtk_object_get_data(tlo, "report_toolbar");
    report->paned   = gtk_object_get_data(tlo, "report_paned");
    
    gnome_app_fill_toolbar(GTK_TOOLBAR(toolbar), toolbar_data, NULL);
    gtk_paned_add2(GTK_PANED(report->paned), 
                   gnc_html_get_widget(report->html));
  }
  
  report->back_widg = toolbar_data[0].widget;
  report->fwd_widg  = toolbar_data[1].widget;
  
  gtk_paned_set_gutter_size(GTK_PANED(report->paned), 20);
  gtk_paned_set_position(GTK_PANED(report->paned), 0);

  gnc_html_set_urltype_cb(report->html, gnc_report_window_check_urltype);
  gnc_html_set_load_cb(report->html, gnc_report_window_load_cb, report);
  gnc_html_set_button_cb(report->html, gnc_report_window_button_cb, report);

  gtk_signal_connect(GTK_OBJECT(report->toplevel), "destroy",
                     GTK_SIGNAL_FUNC(gnc_report_window_destroy_cb), 
                     (gpointer)report);

  report->option_dialog = NULL;
  report->odb = NULL;
  gtk_widget_show_all(report->toplevel);

  open_report_windows = g_list_append(open_report_windows, report);
  
  return report;
}


/********************************************************************
 * gnc_report_window_destroy 
 * free and destroy a window 
 ********************************************************************/

void
gnc_report_window_destroy(gnc_report_window * win) {

  /* delete the window from the open list */
  open_report_windows = g_list_remove(open_report_windows, win);

  gtk_signal_disconnect_by_func(GTK_OBJECT(win->toplevel), 
                                GTK_SIGNAL_FUNC(gnc_report_window_destroy_cb), 
                                (gpointer)win);
  
  /* handle the gnc-html carefully */
  gtk_widget_ref(gnc_html_get_widget(win->html));
  gnc_html_destroy(win->html);

  if(win->option_dialog) {
    gtk_widget_ref(gnc_options_dialog_widget(win->option_dialog));
    gnc_options_dialog_destroy(win->option_dialog);
  }

  if(win->odb) gnc_option_db_destroy(win->odb);
  if(win->toplevel) gtk_widget_destroy(GTK_WIDGET(win->toplevel)); 
  
  
  win->toplevel      = NULL;
  win->option_dialog = NULL;
  win->odb           = NULL;
  win->html          = NULL;

  scm_unprotect_object(win->scm_options);

  g_free(win);
}

gnc_html *
gnc_report_window_get_html(gnc_report_window * report) {
  return report->html;
}

void
gnc_report_window_show_report(gnc_report_window * report, int report_id) {
  char * location = g_strdup_printf("id=%d", report_id);  
  gnc_html_show_url(report->html, URL_TYPE_REPORT, location, NULL, 0);
  g_free(location);
}


void
reportWindow(int report_id) {
  gnc_report_window * win = gnc_report_window_new(NULL);
  gnc_report_window_show_report(win, report_id);
}


void
gnc_ui_destroy_report_windows(void) {
  while(open_report_windows != NULL) {
    gnc_report_window_destroy((gnc_report_window *)open_report_windows->data);
  }
}

