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

#include "top-level.h"

#include <gnome.h>

#include "window-report.h"
#include "window-html.h"
#include "option-util.h"
#include "guile-util.h"
#include "dialog-options.h"
#include "messages.h"
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
  SCM rendering_thunk_id;

  SCM guile_options;
  SCM guile_options_id;
};


static ReportData *
report_data_new()
{
  ReportData *report_data;
  
  report_data = g_new0(ReportData, 1);

  report_data->guile_options = SCM_UNDEFINED;
  report_data->guile_options_id = SCM_UNDEFINED;

  report_data->rendering_thunk = SCM_UNDEFINED;
  report_data->rendering_thunk_id = SCM_UNDEFINED;

  return report_data;
}

static void
report_data_destroy(HTMLUserData user_data)
{
  ReportData *report_data = user_data;

  g_free(report_data->text);
  report_data->text = NULL;

  gnc_option_db_destroy(report_data->odb);
  report_data->odb = NULL;

  if (report_data->option_dialog != NULL)
    gtk_widget_destroy(report_data->option_dialog);
  report_data->option_dialog = NULL;

  if (report_data->guile_options_id != SCM_UNDEFINED)
    gnc_unregister_c_side_scheme_ptr_id(report_data->guile_options_id);
  report_data->guile_options = SCM_UNDEFINED;
  report_data->guile_options_id = SCM_UNDEFINED;

  if (report_data->rendering_thunk_id != SCM_UNDEFINED)
    gnc_unregister_c_side_scheme_ptr_id(report_data->rendering_thunk_id);
  report_data->rendering_thunk = SCM_UNDEFINED;
  report_data->rendering_thunk_id = SCM_UNDEFINED;

  g_free(report_data);
}

static void
report_data_set_text(ReportData *report_data, const gchar *text)
{
  g_free(report_data->text);
  report_data->text = g_strdup(text);
}

static void
report_data_set_rendering_thunk(ReportData *report_data,
                                const SCM rendering_thunk)
{
  if (report_data->rendering_thunk_id != SCM_UNDEFINED)
    gnc_unregister_c_side_scheme_ptr_id(report_data->rendering_thunk_id);

  report_data->rendering_thunk = rendering_thunk;
  report_data->rendering_thunk_id = gnc_register_c_side_scheme_ptr(rendering_thunk);
}

static void
gnc_options_dialog_apply_cb(GnomePropertyBox *propertybox,
			    gint arg1, gpointer user_data)
{
  ReportData *report_data = user_data;

  if (arg1 == -1)
    gnc_option_db_commit(report_data->odb);
}

static void
gnc_options_dialog_help_cb(GnomePropertyBox *propertybox,
			   gint arg1, gpointer user_data)
{
  gnome_ok_dialog("Set the report options you want using this dialog.");
}


static void
report_data_set_guile_options(ReportData *report_data, const SCM guile_options)
{
  GnomePropertyBox *prop_box;

  if (report_data->guile_options_id != SCM_UNDEFINED)
  {
    gnc_unregister_c_side_scheme_ptr_id(report_data->guile_options_id);
    gnc_option_db_destroy(report_data->odb);
  }

  if (report_data->option_dialog != NULL)
    gtk_widget_destroy(report_data->option_dialog);

  if (gh_scm2bool(gh_not(guile_options)))
  {
    report_data->guile_options = SCM_UNDEFINED;
    report_data->option_dialog = NULL;
    return;
  }

  report_data->guile_options = guile_options;
  report_data->guile_options_id =
    gnc_register_c_side_scheme_ptr(guile_options);

  report_data->odb = gnc_option_db_new();

  gnc_option_db_init(report_data->odb, guile_options);

  report_data->option_dialog = gnome_property_box_new();
  gnome_dialog_close_hides(GNOME_DIALOG(report_data->option_dialog), TRUE);

  prop_box = GNOME_PROPERTY_BOX(report_data->option_dialog);
  gnc_build_options_dialog_contents(prop_box, report_data->odb);

  gnc_option_db_clean(report_data->odb);

  gtk_signal_connect(GTK_OBJECT(report_data->option_dialog), "apply",
                     GTK_SIGNAL_FUNC(gnc_options_dialog_apply_cb),
                     report_data);

  gtk_signal_connect(GTK_OBJECT(report_data->option_dialog), "help",
                     GTK_SIGNAL_FUNC(gnc_options_dialog_help_cb),
                     report_data);
}


static HTMLData *
reportAnchorCB(XmHTMLAnchorCallbackStruct *acbs,
               HTMLUserData user_data)
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
reportJumpCB(HTMLUserData user_data, char **set_text, char **set_label)
{
  ReportData *report_data = user_data;
  char *text;
  SCM text_scm;

  *set_text = NULL;
  *set_label = NULL;

  if (report_data->text != NULL)
  {
    *set_text = report_data->text;
    return;
  }

  if (!gh_procedure_p(report_data->rendering_thunk))
    return;

  text_scm = gh_call0(report_data->rendering_thunk);

  if (!gh_string_p(text_scm))
    return;

  text = gh_scm2newstr(text_scm, NULL);
  if (text == NULL)
    return;

  report_data_set_text(report_data, text);
  free(text);

  *set_text = report_data->text;
}


static void
gnc_report_options_changed_cb(gpointer data)
{
  ReportData *report_data = data;
  ReportData *real_data;

  if (report_data == NULL)
    return;

  report_data_set_text(report_data, NULL);

  real_data = gnc_html_window_user_data(reportwindow);
  if (report_data != real_data)
    return;

  gnc_html_load(reportwindow);
}


static void
gnc_report_properties_cb(GtkWidget *widget, gpointer data)
{
  ReportData *report_data = data;

  if (report_data->option_dialog == NULL)
    return;

  gtk_widget_show_all(report_data->option_dialog);
  gdk_window_raise(GTK_WIDGET(report_data->option_dialog)->window);
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
  ReportData *report_data;
  HTMLData *html_data;

  if (reportwindow == NULL)
    reportwindow = gnc_html_window_new(reportAnchorCB, reportJumpCB);

  report_data = report_data_new();
  report_data_set_rendering_thunk(report_data, rendering_thunk);
  report_data_set_guile_options(report_data, guile_options);

  if (report_data->odb != NULL)
    gnc_option_db_register_change_callback(report_data->odb,
                                           gnc_report_options_changed_cb,
                                           report_data);

  if (report_data->option_dialog != NULL)
  {
    gchar *prop_title;

    prop_title = g_strconcat(title, " (", PARAMETERS_STR, ")", NULL);
    gtk_window_set_title(GTK_WINDOW(report_data->option_dialog), prop_title);
    g_free(prop_title);
  }

  if (report_data->option_dialog != NULL)
  {
    GnomeUIInfo user_buttons[] =
    {
      { GNOME_APP_UI_ITEM,
        PARAMETERS_STR,
        TOOLTIP_REPORT_PARM,
        gnc_report_properties_cb, report_data,
        NULL,
        GNOME_APP_PIXMAP_STOCK, 
        GNOME_STOCK_PIXMAP_PROPERTIES,
        0, 0, NULL
      }
    };

    gint num_buttons = sizeof(user_buttons) / sizeof(GnomeUIInfo);

    html_data = gnc_html_data_new(title, report_data,
                                  report_data_destroy,
                                  user_buttons, num_buttons);
  }
  else
    html_data = gnc_html_data_new(title, report_data,
                                  report_data_destroy, NULL, 0);

  htmlWindow(NULL, &reportwindow, html_data);
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
