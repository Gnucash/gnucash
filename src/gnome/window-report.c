/********************************************************************\
 * window-report.c -- a report window for hypertext help.           *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
 * Copyright (C) 1999 Jeremy Collins ( gtk-xmhtml port )            *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#include <gnome.h>

#include "config.h"

#include "window-report.h"
#include "window-html.h"
#include "option-util.h"
#include "guile-util.h"
#include "dialog-options.h"
#include "util.h"

static short module = MOD_HTML; 

static HTMLWindow *reportwindow = NULL;


typedef struct _ReportData ReportData;
struct _ReportData
{
  gchar *text;

  GNCOptionDB *odb;

  GtkWidget *option_dialog;

  SCM rendering_thunk;
  SCM guile_options;
};


static ReportData *
report_data_new()
{
  ReportData *rd;
  
  rd = g_new0(ReportData, 1);

  rd->guile_options = SCM_UNDEFINED;
  rd->rendering_thunk = SCM_UNDEFINED;

  return rd;
}

static void
report_data_destroy(HTMLHistoryData history_data)
{
  ReportData *rd = history_data;

  g_free(rd->text);
  rd->text = NULL;

  gnc_option_db_destroy(rd->odb);
  rd->odb = NULL;

  if (rd->option_dialog != NULL)
    gtk_widget_destroy(rd->option_dialog);
  rd->option_dialog = NULL;

  if (rd->guile_options != SCM_UNDEFINED)
    gnc_unregister_c_side_scheme_ptr(rd->guile_options);
  rd->guile_options = SCM_UNDEFINED;

  if (rd->rendering_thunk != SCM_UNDEFINED)
    gnc_unregister_c_side_scheme_ptr(rd->rendering_thunk);
  rd->rendering_thunk = SCM_UNDEFINED;

  g_free(rd);
}

static void
report_data_set_text(ReportData *rd, const gchar *text)
{
  g_free(rd->text);
  rd->text = g_strdup(text);
}

static void
report_data_set_rendering_thunk(ReportData *rd, const SCM rendering_thunk)
{
  if (rd->rendering_thunk != SCM_UNDEFINED)
    gnc_unregister_c_side_scheme_ptr(rd->rendering_thunk);

  rd->rendering_thunk = rendering_thunk;
}

static void
gnc_options_dialog_apply_cb(GnomePropertyBox *propertybox,
			    gint arg1, gpointer user_data)
{
  ReportData *rd = user_data;

  if (arg1 == -1)
    gnc_option_db_commit(rd->odb);
}

static void
gnc_options_dialog_help_cb(GnomePropertyBox *propertybox,
			   gint arg1, gpointer user_data)
{
  gnome_ok_dialog("Set the report options you want using this dialog.");
}


static void
report_data_set_guile_options(ReportData *rd, const SCM guile_options)
{
  GnomePropertyBox *prop_box;

  if (rd->guile_options != SCM_UNDEFINED)
  {
    gnc_unregister_c_side_scheme_ptr(rd->guile_options);
    gnc_option_db_destroy(rd->odb);
  }

  if (rd->option_dialog != NULL)
    gtk_widget_destroy(rd->option_dialog);

  if (gh_scm2bool(gh_not(guile_options)))
  {
    rd->guile_options = SCM_UNDEFINED;
    rd->option_dialog = NULL;
    return;
  }

  rd->guile_options = guile_options;
  gnc_register_c_side_scheme_ptr(guile_options);

  rd->odb = gnc_option_db_new();

  gnc_option_db_init(rd->odb, guile_options);

  rd->option_dialog = gnome_property_box_new();
  gnome_dialog_close_hides(GNOME_DIALOG(rd->option_dialog), TRUE);

  prop_box = GNOME_PROPERTY_BOX(rd->option_dialog);
  gnc_build_options_dialog_contents(prop_box, rd->odb);

  gnc_option_db_clean(rd->odb);

  gtk_signal_connect(GTK_OBJECT(rd->option_dialog), "apply",
                     GTK_SIGNAL_FUNC(gnc_options_dialog_apply_cb), rd);

  gtk_signal_connect(GTK_OBJECT(rd->option_dialog), "help",
                     GTK_SIGNAL_FUNC(gnc_options_dialog_help_cb), rd);
}


static HTMLHistoryData
reportAnchorCB(XmHTMLAnchorCallbackStruct *acbs,
               HTMLHistoryData history_data)
{
  switch(acbs->url_type)
  {
    /* a local file with a possible jump to label */
    case ANCHOR_FILE_LOCAL:
      PERR(" this report window doesn't support ftp: %s\n", acbs->href);
      break;
    /* other types are unsupported, but it would be fun if they were ... */
    case ANCHOR_FTP:
      PERR(" this report window doesn't support ftp: %s\n", acbs->href);
      break;
    case ANCHOR_HTTP:
      PERR (" this report window doesn't support http: %s\n", acbs->href);
      break;
    case ANCHOR_MAILTO:
      PERR(" this report window doesn't support email: %s\n", acbs->href);
      break;
    case ANCHOR_UNKNOWN:
    default:
      PERR(" don't know this type of url: %s\n", acbs->href);
      break;
  }

  return NULL;
}

static void
reportJumpCB(HTMLHistoryData history_data, char **set_text, char **set_label)
{
  ReportData *rd = (ReportData *) history_data;
  char *text;
  SCM text_scm;

  *set_text = NULL;
  *set_label = NULL;

  if (rd->text != NULL)
  {
    *set_text = rd->text;
    return;
  }

  if (!gh_procedure_p(rd->rendering_thunk))
    return;

  text_scm = gh_call0(rd->rendering_thunk);

  if (!gh_string_p(text_scm))
    return;

  text = gh_scm2newstr(text_scm, NULL);
  if (text == NULL)
    return;

  report_data_set_text(rd, text);
  free(text);

  *set_text = rd->text;
}


static void
gnc_report_options_changed_cb(gpointer data)
{
  HTMLWindow *hw = data;
  HTMLHistoryData hd;
  ReportData *rd;

  hd = gnc_html_window_history_data(hw);
  if (hd == NULL)
    return;

  rd = (ReportData *) hd;
  report_data_set_text(rd, NULL);

  gnc_html_load(hw);
}


static void
gnc_report_properties_cb(GtkWidget *widget, gpointer data)
{
  ReportData *rd = data;

  if (rd->option_dialog == NULL)
    return;

  gtk_widget_show_all(rd->option_dialog);
  gdk_window_raise(GTK_WIDGET(rd->option_dialog)->window);
}


/********************************************************************\
 * reportWindow                                                     * 
 *   opens up a report window, and displays html                    * 
 *                                                                  * 
 * Args:   title - the title of the window                          * 
 *         text  - the html text to display                         * 
 * Return: none                                                     * 
\********************************************************************/
void
reportWindow(const char *title, SCM rendering_thunk, SCM guile_options)
{
  ReportData *rd;

  if (reportwindow == NULL)
    reportwindow = gnc_html_window_new(report_data_destroy, reportAnchorCB,
                                       reportJumpCB);

  rd = report_data_new();
  report_data_set_rendering_thunk(rd, rendering_thunk);
  report_data_set_guile_options(rd, guile_options);

  if (rd->odb != NULL)
    gnc_option_db_register_change_callback(rd->odb,
                                           gnc_report_options_changed_cb,
                                           reportwindow);

  if (rd->option_dialog != NULL)
  {
    gchar *prop_title;

    prop_title = g_strconcat(title, " (Parameters)", NULL);
    gtk_window_set_title(GTK_WINDOW(rd->option_dialog), prop_title);
    g_free(prop_title);
  }

  {
    GnomeUIInfo user_buttons[] =
    {
      { GNOME_APP_UI_ITEM,
        "Properties", 
        "Set the properties for this report.",
        gnc_report_properties_cb, rd,
        NULL,
        GNOME_APP_PIXMAP_STOCK, 
        GNOME_STOCK_PIXMAP_PROPERTIES,
        0, 0, NULL
      }
    };

    gint num_buttons = sizeof(user_buttons) / sizeof(GnomeUIInfo);

    htmlWindow(NULL, &reportwindow, title, rd, user_buttons, num_buttons);
  }
}


/********************************************************************\
 * gnc_ui_destroy_report_windows                                    * 
 *   destroys any open report windows                               * 
 *                                                                  * 
 * Args:   none                                                     * 
 * Return: none                                                     * 
\********************************************************************/
void
gnc_ui_destroy_report_windows()
{
  gnc_html_window_destroy(reportwindow);
  reportwindow = NULL;

  DEBUG("report windows destroyed.\n");
}

/* ----------------------- END OF FILE ---------------------  */
