/********************************************************************\
 * gnc-report-window.c                                              *
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

#include <g-wrap-runtime-guile.h>

#include "dialog-options.h"
#include "dialog-utils.h"
#include "glade-gnc-dialogs.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "gnc-html-history.h"
#include "gnc-html.h"
#include "gnc-ui.h"
#include "query-user.h"
#include "window-report.h"

#define WINDOW_REPORT_CM_CLASS "window-report"

struct _gnc_report_window {
  GtkWidget    * container;   
  gboolean     top_level;

  GtkWidget    * popup;     
  GtkWidget    * back_widg;
  GtkWidget    * fwd_widg;

  SCM          scm_report;
  SCM          scm_options;
  SCM          scm_options_edit;

  gnc_html     * html;
};


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_HTML;

static gint last_width = 0;
static gint last_height = 0;


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
gnc_report_window_export_button_cb(GtkWidget * w, gpointer data) {
  gnc_report_window       * report = data;
  ENTER (" ");
  gnc_html_export(report->html);
  return TRUE;
}


static int
gnc_report_window_params_cb(GtkWidget * w, gpointer data) {
  gnc_report_window * report = data;
  SCM scm_wintype = gh_eval_str("<gnc:report-window*>");

  if(gh_procedure_p(report->scm_options_edit)) {
    gh_call2(report->scm_options_edit, 
             report->scm_options,
             report->scm_report);
  }
  else {
    gnc_report_window_default_params_editor(report->scm_options, 
                                            report->scm_report);
  }
  
  return TRUE;
}

static int
gnc_report_window_newwin_cb(GtkWidget * w, gpointer data) {
  gnc_report_window       * report = data;
  gnc_report_window       * newwin = gnc_report_window_new(NULL);
  SCM   get_id = gh_eval_str("gnc:report-id");
  SCM   id = gh_call1(get_id, report->scm_report);

  if(gh_number_p(id)) {
    gnc_report_window_show_report(newwin, gh_scm2int(id));
  }
  return TRUE;
}


static int
gnc_report_window_reload_button_cb(GtkWidget * w, gpointer data) {
  gnc_report_window * report = data;
  SCM               dirty_report = gh_eval_str("gnc:report-set-dirty?!");

  gh_call2(dirty_report, report->scm_report, SCM_BOOL_T);
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

void
gnc_report_window_reload(gnc_report_window * win) {
  gnc_html_reload(win->html);
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
  SCM  get_editor  = gh_eval_str("gnc:report-options-editor");
  SCM  show_report = gh_eval_str("gnc:report-register-display");
  SCM  unshow_report = gh_eval_str("gnc:report-unregister-display");
  SCM  scm_wintype = gh_eval_str("<gnc:report-window*>");
  SCM  inst_report;
  SCM  inst_options;
  SCM  inst_options_ed;
  
  if(!strncmp("id=", location, 3)) {
    sscanf(location+3, "%d", &report_id);
  }
  else {
    return;
  }
  
  /* get the inst-report from the Scheme-side hash, and get its
   * options and editor thunk */
  if((inst_report = gh_call1(find_report, gh_int2scm(report_id))) ==
     SCM_BOOL_F) {
    return;
  }

  /* unregister ourselves as a "displayer" of the current report */
  if(win->scm_report != SCM_BOOL_F) {
    gh_call2(unshow_report, win->scm_report, 
             gw_wcp_assimilate_ptr(win, scm_wintype));
  }

  inst_options    = gh_call1(get_options, inst_report); 
  inst_options_ed = gh_call1(get_editor, inst_report);
  
  scm_unprotect_object(win->scm_options);
  win->scm_options = inst_options;
  scm_protect_object(win->scm_options);

  scm_unprotect_object(win->scm_options_edit);
  win->scm_options_edit = inst_options_ed;
  scm_protect_object(win->scm_options_edit);

  scm_unprotect_object(win->scm_report);
  win->scm_report = inst_report;
  scm_protect_object(win->scm_report);

  if(win->scm_report != SCM_BOOL_F) {
    gh_call2(show_report, win->scm_report, 
             gw_wcp_assimilate_ptr(win, scm_wintype));
  }

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
  gnc_report_window * win = data;
  SCM  scm_wintype = gh_eval_str("<gnc:report-window*>");
  SCM  unshow_report = gh_eval_str("gnc:report-unregister-display");

  if(win->scm_report != SCM_BOOL_F) {
    gh_call2(unshow_report, win->scm_report, 
             gw_wcp_assimilate_ptr(win, scm_wintype));
  }

  /* make sure we don't get a double dose -o- destruction */ 
  gtk_signal_disconnect_by_data(GTK_OBJECT(win->container), 
                                data);

  /* delete the window from the open list */
  if(win->top_level) {
    gnc_unregister_gui_component_by_data (WINDOW_REPORT_CM_CLASS, win);
  }
  gnc_html_destroy(win->html);

  if(win->popup) {
    gtk_widget_destroy(win->popup);
  }
  win->popup         = NULL;
  win->container     = NULL;
  win->html          = NULL;

  scm_unprotect_object(win->scm_options);
  scm_unprotect_object(win->scm_options_edit);
  scm_unprotect_object(win->scm_report);

  g_free(win);
}


/********************************************************************
 * gnc_report_window_close_cb
 ********************************************************************/

static void
gnc_report_window_close_cb(GtkWidget * w, gpointer data) {
  gnc_report_window * rw = data;

  if(rw->top_level) {
    gnc_close_gui_component_by_data (WINDOW_REPORT_CM_CLASS, rw);
  }
  else {
    gtk_widget_destroy(rw->container);
  }
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

static void
gnc_report_window_print_cb(GtkWidget * w, gpointer data) {
  gnc_report_window * win = data;
  gnc_html_print(win->html);
}

static void 
gnc_report_window_history_destroy_cb(gnc_html_history_node * node, 
                                     gpointer user_data) {
  static SCM         remover = SCM_BOOL_F;
  int                report_id;
  
  if(remover == SCM_BOOL_F) {
    remover = gh_eval_str("gnc:report-remove-by-id");
  }
  
  if(node && 
     (node->type == URL_TYPE_REPORT) && 
     !strncmp("id=", node->location, 3)) {
    sscanf(node->location+3, "%d", &report_id);
    /*    printf("unreffing report %d and children\n", report_id);
          gh_call1(remover, gh_int2scm(report_id)); */
  }
  else {
    return;
  }
}

static void
close_handler (gpointer user_data)
{
  gnc_report_window *win = user_data;
  
  if (win->top_level)
  {
    gdk_window_get_geometry (GTK_WIDGET(win->container)->window, NULL, NULL,
                             &last_width, &last_height, NULL);
    
    gnc_save_window_size ("report_win", last_width, last_height);
  }
  
  gnc_report_window_destroy (win);
}

/********************************************************************
 * gnc_report_window_new 
 * allocates and opens up a report window
 ********************************************************************/

gnc_report_window *
gnc_report_window_new(GtkWidget * container) {

  gnc_report_window * report = g_new0(gnc_report_window, 1);
  GtkObject         * tlo; 
  GtkWidget         * cframe = NULL;
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
      gnc_report_window_export_button_cb, report,
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
      _("New window"),
      _("Open this report in a new window"),
      gnc_report_window_newwin_cb, report,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_NEW,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      _("Close"),
      _("Close this report window"),
      gnc_report_window_close_cb, report,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_CLOSE,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };
  
  report->html             = gnc_html_new();
  report->scm_options      = SCM_BOOL_F;
  report->scm_options_edit = SCM_BOOL_F;
  report->scm_report       = SCM_BOOL_F;

  gnc_html_history_set_node_destroy_cb(gnc_html_get_history(report->html),
                                       gnc_report_window_history_destroy_cb,
                                       (gpointer)report);
  
  scm_protect_object(report->scm_options);
  scm_protect_object(report->scm_options_edit);
  scm_protect_object(report->scm_report);

  if(container) {
    report->container = container;
    report->top_level = FALSE;
    report->popup = gnome_popup_menu_new(toolbar_data);
    gtk_container_add(GTK_CONTAINER(container), 
                      gnc_html_get_widget(report->html));
  }
  else {
    report->container = create_Report_Window();
    report->top_level = TRUE;
    report->popup = NULL;

    tlo = GTK_OBJECT(report->container);
    toolbar         = gtk_object_get_data(tlo, "report_toolbar");
    cframe          = gtk_object_get_data(tlo, "report_frame");
    gnome_app_fill_toolbar(GTK_TOOLBAR(toolbar), toolbar_data, NULL);
    gtk_container_add(GTK_CONTAINER(cframe), 
                      gnc_html_get_widget(report->html));
    gnc_register_gui_component (WINDOW_REPORT_CM_CLASS, NULL,
                                close_handler, report);
  }
  
  report->back_widg = toolbar_data[0].widget;
  report->fwd_widg  = toolbar_data[1].widget;

  gnc_html_set_urltype_cb(report->html, gnc_report_window_check_urltype);
  gnc_html_set_load_cb(report->html, gnc_report_window_load_cb, report);
  gnc_html_set_button_cb(report->html, gnc_report_window_button_cb, report);

  gtk_signal_connect(GTK_OBJECT(report->container), "destroy",
                     GTK_SIGNAL_FUNC(gnc_report_window_destroy_cb),
                     report);

  if (report->top_level) {
    if (last_width == 0)
      gnc_get_window_size("report_win", &last_width, &last_height);
    
    gtk_window_set_default_size(GTK_WINDOW(report->container),
                                last_width, last_height);
    
    gnc_window_adjust_for_screen (GTK_WINDOW(report->container));
  }
  
  gtk_widget_show_all(report->container);
  
  return report;
}


/********************************************************************
 * gnc_report_window_destroy 
 * free and destroy a window 
 ********************************************************************/

void
gnc_report_window_destroy(gnc_report_window * win) {

  if (!win) return;

  gtk_widget_destroy(GTK_WIDGET(win->container));
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
  gnc_report_window * win;

  gnc_set_busy_cursor (NULL);
  win = gnc_report_window_new(NULL);
  gnc_report_window_show_report(win, report_id);
  gnc_unset_busy_cursor (NULL);
}

void
gnc_print_report (int report_id)
{
  gnc_html *html;
  char * location;

  html = gnc_html_new ();

  gnc_set_busy_cursor (NULL);
  location = g_strdup_printf("id=%d", report_id);  
  gnc_html_show_url(html, URL_TYPE_REPORT, location, NULL, FALSE);
  g_free(location);
  gnc_unset_busy_cursor (NULL);

  gnc_html_print (html);

  gnc_html_destroy (html);
}


struct report_default_params_data {
  GNCOptionWin * win;
  GNCOptionDB  * db;
  SCM          scm_options;
  SCM          scm_report;
};


static void
gnc_options_dialog_apply_cb(GNCOptionWin * propertybox,
                            gpointer user_data) {
  SCM  dirty_report = gh_eval_str("gnc:report-set-dirty?!");
  struct report_default_params_data * win = user_data;
  
  if(!win) return;
  gnc_option_db_commit(win->db);
  gh_call2(dirty_report, win->scm_report, SCM_BOOL_T);
}

static void
gnc_options_dialog_help_cb(GNCOptionWin * propertybox,
                           gpointer user_data) {
  gnome_ok_dialog("Set the report options you want using this dialog.");
}

static void
gnc_options_dialog_close_cb(GNCOptionWin * propertybox,
                            gpointer user_data) {
  struct report_default_params_data * win = user_data;
  gnc_option_db_destroy(win->db);
  scm_unprotect_object(win->scm_options);
  gnc_options_dialog_destroy(win->win);
  g_free(win);
}


void
gnc_report_window_default_params_editor(SCM options, SCM report) {
  struct report_default_params_data * prm = 
    g_new0(struct report_default_params_data, 1);

  prm->scm_options = options;
  prm->scm_report  = report;
  prm->db          = gnc_option_db_new(prm->scm_options);
  prm->win         = gnc_options_dialog_new(TRUE);
  
  scm_protect_object(prm->scm_options);
  scm_protect_object(prm->scm_report);

  gnc_build_options_dialog_contents(prm->win, prm->db);
  gnc_options_dialog_set_apply_cb(prm->win, 
                                  gnc_options_dialog_apply_cb,
                                  (gpointer)prm);
  gnc_options_dialog_set_help_cb(prm->win, 
                                  gnc_options_dialog_help_cb,
                                  (gpointer)prm);
  gnc_options_dialog_set_close_cb(prm->win, 
                                  gnc_options_dialog_close_cb,
                                  (gpointer)prm);

}
