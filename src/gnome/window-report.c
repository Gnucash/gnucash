/********************************************************************\
 * window-report.c -- a report window for hypertext help.           *
 * Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>            *
 * Copyright (C) 1998 Linas Vepstas  <linas@linas.org>              *
 * Copyright (C) 1999 Jeremy Collins ( gtk-xmhtml port )            *
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

#include "top-level.h"

#include <gnome.h>
#include <sys/stat.h>
#include <stdio.h>

#include "window-report.h"
#include "window-html.h"
#include "option-util.h"
#include "guile-util.h"
#include "dialog-options.h"
#include "ui-callbacks.h"
#include "query-user.h"
#include "messages.h"
#include "util.h"
#include "FileBox.h"
#include "gfec.h"

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
  SCM change_callback_id;
};


static ReportData *
report_data_new()
{
  ReportData *report_data;
  
  report_data = g_new0(ReportData, 1);

  report_data->rendering_thunk = SCM_UNDEFINED;
  report_data->rendering_thunk_id = SCM_UNDEFINED;
  report_data->change_callback_id = SCM_UNDEFINED;

  return report_data;
}

static void
report_data_destroy(HTMLUserData user_data)
{
  ReportData *report_data = user_data;

  g_free(report_data->text);
  report_data->text = NULL;

  if (report_data->change_callback_id != SCM_UNDEFINED)
    gnc_option_db_unregister_change_callback_id
      (report_data->odb, report_data->change_callback_id);
  report_data->change_callback_id = SCM_UNDEFINED;

  gnc_option_db_destroy(report_data->odb);
  report_data->odb = NULL;

  if (report_data->option_dialog != NULL)
    gtk_widget_destroy(report_data->option_dialog);
  report_data->option_dialog = NULL;

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

  if (report_data->odb != NULL)
  {
    gnc_option_db_destroy(report_data->odb);
    report_data->odb = NULL;
  }

  if (report_data->option_dialog != NULL)
  {
    gtk_widget_destroy(report_data->option_dialog);
    report_data->option_dialog = NULL;
  }

  if (guile_options == SCM_BOOL_F)
    return;

  report_data->odb = gnc_option_db_new(guile_options);

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
    case ANCHOR_FILE_LOCAL:
    case ANCHOR_FTP:
    case ANCHOR_HTTP:
    case ANCHOR_MAILTO:
    case ANCHOR_UNKNOWN:
    default:
      gnome_url_show(acbs->href);
      break;
  }

  return NULL;
}

static void
gnc_report_error_dialog(const char *message)
{
  GtkWindow *parent;
  gchar *text;

  parent = GTK_WINDOW(gnc_html_window_get_window(reportwindow));

  if (message == NULL)
    text = REPORT_ERR_MSG;
  else
    text = g_strconcat(REPORT_ERR_MSG, "\n\n", message, NULL);

  PERR("gnc_report_error_dialog: error running report.\n%s\n", message);

  gnc_error_dialog_parented(parent, text);

  if (message != NULL)
    g_free(text);
}

static char *
gnc_run_report(ReportData *report_data)
{
  SCM result;

  if (!gh_procedure_p(report_data->rendering_thunk))
    return NULL;

  gnc_set_busy_cursor(NULL);

  result = gfec_apply(report_data->rendering_thunk, SCM_EOL,
                      gnc_report_error_dialog);

  gnc_unset_busy_cursor(NULL);

  if (!gh_string_p(result))
    return NULL;

  return gh_scm2newstr(result, NULL);
}

static void
reportJumpCB(HTMLUserData user_data, char **set_text, char **set_label)
{
  ReportData *report_data = user_data;
  char *text;

  *set_text = NULL;
  *set_label = NULL;

  if (report_data->text != NULL)
  {
    *set_text = report_data->text;
    return;
  }

  text = gnc_run_report(report_data);
  if (text == NULL)
    return;

  report_data_set_text(report_data, text);
  free(text);

  *set_text = report_data->text;
}


static void
gnc_report_options_changed_cb(void *data)
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
  {
    GtkWidget *window = gnc_html_window_get_window(reportwindow);
    gnc_info_dialog_parented(GTK_WINDOW(window), REPORT_NOPARM_MSG);
    return;
  }

  gtk_widget_show_all(report_data->option_dialog);
  gdk_window_raise(GTK_WIDGET(report_data->option_dialog)->window);
}


static void
gnc_report_export(ReportData *report_data)
{
  GtkWindow *parent;
  char *export_filename;
  struct stat file_status;
  FILE *export_dest;
  char *message;
  char *text;

  if (report_data->text == NULL)
    text = "";
  else
    text = report_data->text;

  /* Get the filename */
  export_filename = fileBox(EXPORT_TO_STR, NULL);
  if (export_filename == NULL)
    return;

  parent = GTK_WINDOW(gnc_html_window_get_window(reportwindow));

  /* See if the file exists */
  if ((stat(export_filename, &file_status) == 0))
  {
    gncBoolean result;

    message = g_strdup_printf(FMB_EEXIST_MSG, export_filename);
    result = gnc_verify_dialog_parented(parent, message, GNC_F);
    g_free(message);

    if (!result)
      return;
  }

  /* Open the file */
  export_dest = fopen(export_filename, "w");
  if (export_dest == NULL)
  {
    message = g_strdup_printf(FILE_EOPEN_MSG, export_filename);
    gnc_error_dialog_parented(parent, message);
    g_free(message);

    return;
  }

  /* Write the data */
  if (fputs(text, export_dest) == EOF)
  {
    message = g_strdup_printf(FILE_EWRITE_MSG, export_filename);
    gnc_error_dialog_parented(parent, message);
    g_free(message);

    return;
  }

  /* Close the file */
  if (fclose(export_dest) == EOF)
  {
    message = g_strdup_printf(FILE_ECLOSE_MSG, export_filename);
    gnc_error_dialog_parented(parent, message);
    g_free(message);

    return;
  }
}

static void 
gnc_report_export_cb(GtkWidget *widget, gpointer data)
{
  ReportData *report_data = data;

  gnc_report_export(report_data);
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
    report_data->change_callback_id =
      gnc_option_db_register_change_callback(report_data->odb,
                                             gnc_report_options_changed_cb,
                                             report_data, NULL, NULL);

  if (report_data->option_dialog != NULL)
  {
    gchar *prop_title;

    prop_title = g_strconcat(title, " (", PARAMETERS_STR, ")", NULL);
    gtk_window_set_title(GTK_WINDOW(report_data->option_dialog), prop_title);
    g_free(prop_title);
  }

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
      },
      { GNOME_APP_UI_ITEM,
        EXPORT_STR,
        TOOLTIP_EXPORT_REPORT,
        gnc_report_export_cb, report_data,
        NULL,
        GNOME_APP_PIXMAP_STOCK,
        GNOME_STOCK_PIXMAP_CONVERT,
        0, 0, NULL
      }
    };

    gint num_buttons = sizeof(user_buttons) / sizeof(GnomeUIInfo);

    html_data = gnc_html_data_new(title, report_data, report_data_destroy,
                                  user_buttons, num_buttons);
  }

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
