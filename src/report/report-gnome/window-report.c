/********************************************************************
 * window-report.c                                                  *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************/

#include "config.h"

#include <gnome.h>
#include <glib/gi18n.h>
#include <errno.h>
#include <libguile.h>
#include <sys/stat.h>

#include "swig-runtime.h"
#include "dialog-options.h"
#include "file-utils.h"
#include "gnc-gkeyfile-utils.h"
#include "gnc-report.h"
#include "gnc-ui.h"
#include "option-util.h"
#include "gnc-html.h"
#include "window-report.h"
#include "guile-mappings.h"

#include "gnc-plugin-page-report.h"
#include "gnc-report.h"

#define WINDOW_REPORT_CM_CLASS "window-report"
#define MDI_CHILD_CONFIG "mdi_child_config"

/********************************************************************
 *
 ********************************************************************/

void
reportWindow(int report_id)
{
  gnc_set_busy_cursor (NULL, TRUE);
  gnc_main_window_open_report(report_id, NULL);
  gnc_unset_busy_cursor (NULL);
}

/********************************************************************
 * default parameters editor handling 
 ********************************************************************/

struct report_default_params_data {
  GNCOptionWin * win;
  GNCOptionDB  * db;
  SCM          scm_options;
  SCM          cur_report;
};


static void
gnc_options_dialog_apply_cb(GNCOptionWin * propertybox,
                            gpointer user_data)
{
  SCM  dirty_report = scm_c_eval_string("gnc:report-set-dirty?!");
  struct report_default_params_data * win = user_data;
  
  if(!win) return;
  gnc_option_db_commit(win->db);
  scm_call_2(dirty_report, win->cur_report, SCM_BOOL_T);
}

static void
gnc_options_dialog_help_cb(GNCOptionWin * propertybox,
                           gpointer user_data)
{
  GtkWidget *dialog, *parent;
  struct report_default_params_data * prm = user_data;

  parent = gnc_options_dialog_widget(prm->win);
  dialog = gtk_message_dialog_new(GTK_WINDOW(parent),
				  GTK_DIALOG_DESTROY_WITH_PARENT,
				  GTK_MESSAGE_INFO,
				  GTK_BUTTONS_OK,
				  "%s",
				  _("Set the report options you want using this dialog."));
  g_signal_connect(G_OBJECT(dialog), "response",
		   (GCallback)gtk_widget_destroy, NULL);
  gtk_widget_show(dialog);
}

static void
gnc_options_dialog_close_cb(GNCOptionWin * propertybox,
                            gpointer user_data)
{
  struct report_default_params_data * win = user_data;
  SCM    set_editor = scm_c_eval_string("gnc:report-set-editor-widget!");
  
  scm_call_2(set_editor, win->cur_report, SCM_BOOL_F);
  gnc_options_dialog_destroy(win->win);
  gnc_option_db_destroy(win->db);
  scm_gc_unprotect_object(win->scm_options);
  g_free(win);
}


GtkWidget * 
gnc_report_window_default_params_editor(SCM options, SCM report)
{
  SCM get_editor        = scm_c_eval_string("gnc:report-editor-widget");
  SCM get_report_type   = scm_c_eval_string("gnc:report-type");
  SCM get_template      = scm_c_eval_string("gnc:find-report-template");
  SCM get_template_name = scm_c_eval_string("gnc:report-template-name");
  SCM ptr;
  
  const gchar *title = NULL;

  ptr = scm_call_1(get_editor, report);
  if(ptr != SCM_BOOL_F) {
    #define FUNC_NAME "gtk_window_present"
    GtkWindow * w = SWIG_MustGetPtr(ptr, SWIG_TypeQuery("_p_GtkWidget"), 1, 0);
    gtk_window_present(w);
    #undef FUNC_NAME
    return NULL;
  }
  else {
    struct report_default_params_data * prm = 
      g_new0(struct report_default_params_data, 1);
    
    prm->scm_options = options;
    prm->cur_report  = report;
    prm->db          = gnc_option_db_new(prm->scm_options);

    /* Get the title of the report's template. */
    ptr = scm_call_1(get_report_type, report);
    if (ptr != SCM_BOOL_F) {
      ptr = scm_call_1(get_template, ptr);
      if (ptr != SCM_BOOL_F) {
        ptr = scm_call_1(get_template_name, ptr);
        if (SCM_STRINGP(ptr))
          title = SCM_STRING_CHARS(ptr);
      }
    }

    /* Don't forget to translate the window title */
    prm->win  = gnc_options_dialog_new((gchar*) (title && *title ? _(title) : ""));
    
    scm_gc_protect_object(prm->scm_options);
    scm_gc_protect_object(prm->cur_report);
    
    gnc_options_dialog_build_contents(prm->win, prm->db);
    gnc_option_db_clean(prm->db);

    gnc_options_dialog_set_apply_cb(prm->win, 
                                    gnc_options_dialog_apply_cb,
                                    (gpointer)prm);
    gnc_options_dialog_set_help_cb(prm->win, 
                                   gnc_options_dialog_help_cb,
                                   (gpointer)prm);
    gnc_options_dialog_set_close_cb(prm->win, 
                                    gnc_options_dialog_close_cb,
                                    (gpointer)prm);
    return gnc_options_dialog_widget(prm->win);
  }
}

void
gnc_report_raise_editor(SCM report)
{
    SCM get_editor = scm_c_eval_string("gnc:report-editor-widget");
    SCM editor = scm_call_1(get_editor, report);
    #define FUNC_NAME "gtk_window_present"
    GtkWidget *w = SWIG_MustGetPtr(editor,
                                   SWIG_TypeQuery("_p_GtkWidget"), 1, 0);
    #undef FUNC_NAME
    gtk_window_present(GTK_WINDOW(w));
}

static gboolean
gnc_html_file_stream_cb (const char *location, char ** data, int *len)
{
  *len = gncReadFile (location, data);
  return (*len > 0);
}

static gboolean
gnc_html_report_stream_cb (const char *location, char ** data, int *len)
{
  gboolean ok;

  ok = gnc_run_report_id_string (location, data);

  if (!ok) {
    *data = g_strdup_printf ("<html><body><h3>%s</h3>"
			     "<p>%s</p></body></html>", 
			     _("Report error"),
			     _("An error occurred while running the report."));
    
    /* Make sure the progress bar is finished, which will also
       make the GUI sensitive again. Easier to do this via guile
       because otherwise we would need to link against gnome-utils
       and a lot more. */
    scm_c_eval_string("(gnc:report-finished)");
  }

  *len = strlen(*data);
  return ok;
}

/* TODO: unroll start_editor */
static gboolean
gnc_html_options_url_cb (const char *location, const char *label,
                         gboolean new_window, GNCURLResult *result)
{
  SCM start_editor = scm_c_eval_string ("gnc:report-edit-options");
  SCM report;
  int report_id;

  g_return_val_if_fail (location != NULL, FALSE);
  g_return_val_if_fail (result != NULL, FALSE);

  result->load_to_stream = FALSE;

  /* href="gnc-options:report-id=2676" */
  if (strncmp ("report-id=", location, 10) == 0)
  {
    if (sscanf (location + 10, "%d", &report_id) != 1)
    {
      result->error_message =
        g_strdup_printf (_("Badly formed options URL: %s"), location);

      return FALSE;
    }

    report = gnc_report_find(report_id);
    if (report == SCM_UNDEFINED ||
        report == SCM_BOOL_F)
    {
      result->error_message =
        g_strdup_printf (_("Badly-formed report id: %s"), location);

      return FALSE;
    }

    scm_call_1 (start_editor, report);

    return TRUE;
  }
  else
  {
    result->error_message =
      g_strdup_printf (_("Badly formed options URL: %s"), location);

    return FALSE;
  }
}

static gboolean
gnc_html_report_url_cb (const char *location, const char *label,
                        gboolean new_window, GNCURLResult *result)
{
  g_return_val_if_fail (location != NULL, FALSE);
  g_return_val_if_fail (result != NULL, FALSE);

  /* make a new window if necessary */ 
  if (new_window)
  {
    char *url;

    url = gnc_build_url (URL_TYPE_REPORT, location, label);
    gnc_main_window_open_report_url (url, NULL);
    g_free (url);

    result->load_to_stream = FALSE;
  }
  else
  {
    result->load_to_stream = TRUE;
  }

  return TRUE;
}

static gboolean
gnc_html_help_url_cb (const char *location, const char *label,
                      gboolean new_window, GNCURLResult *result)
{
  g_return_val_if_fail (location != NULL, FALSE);

  gnc_gnome_help (location, label);
  return TRUE;
}

void
gnc_report_init (void)
{
  /* Reference the report page plugin to ensure it exists in the gtk
   * type system. */
  GNC_TYPE_PLUGIN_PAGE_REPORT;

  gnc_html_register_stream_handler (URL_TYPE_HELP, gnc_html_file_stream_cb);
  gnc_html_register_stream_handler (URL_TYPE_FILE, gnc_html_file_stream_cb);
  gnc_html_register_stream_handler (URL_TYPE_REPORT, gnc_html_report_stream_cb);

  gnc_html_register_url_handler (URL_TYPE_OPTIONS, gnc_html_options_url_cb);
  gnc_html_register_url_handler (URL_TYPE_REPORT, gnc_html_report_url_cb);
  gnc_html_register_url_handler (URL_TYPE_HELP, gnc_html_help_url_cb);
}
