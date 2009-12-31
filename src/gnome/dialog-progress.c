/********************************************************************\
 * dialog-progress.c -- GnuCash progress dialog                     *
 * Copyright (C) 2000 Dave Peticolas                                *
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
\********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <libguile.h>
#include "guile-mappings.h"

#include "dialog-progress.h"
#include "dialog-utils.h"


struct _GNCProgressDialog
{
  GtkWidget *dialog;

  GtkWidget *primary_label;
  GtkWidget *secondary_label;
  GtkWidget *progress_bar;
  GtkWidget *sub_label;
  GtkWidget *log;

  GtkWidget *ok_button;
  GtkWidget *cancel_button;

  /* The stack of virtual progress bars. */
  GList     *bars;
  /* The fraction of the current bar that is filled. */
  gdouble    bar_value;
  /* The value of the real (top-level) bar before the last push. */
  gdouble    total_offset;
  /* The product of all weights in the stack. */
  gdouble    total_weight;

  GNCProgressCancelFunc cancel_func;
  gpointer user_data;

  SCM cancel_scm_func;

  gboolean use_ok_button;
  gboolean closed;
  gboolean finished;
  gboolean destroyed;
  gboolean title_set;
};

typedef struct
{
  gdouble offset;
  gdouble weight;
} VirtualBar;

static void
gnc_progress_maybe_destroy(GNCProgressDialog *progress)
{
  g_return_if_fail(progress);

  if (!(progress->closed && progress->destroyed))
    return;

  if (progress->dialog != NULL)
    gtk_widget_destroy(progress->dialog);
}


static void
ok_cb(GtkWidget * widget, gpointer data)
{
  GNCProgressDialog *progress = data;

  g_return_if_fail(progress);

  if (progress->dialog != NULL)
    gtk_widget_hide(progress->dialog);
  progress->closed = TRUE;
  gnc_progress_maybe_destroy(progress);
}


static void
cancel_cb(GtkWidget * widget, gpointer data)
{
  GNCProgressDialog *progress = data;

  g_return_if_fail(progress);

  if (progress->cancel_func && !progress->cancel_func(progress->user_data))
    return;

  if (progress->cancel_scm_func != SCM_UNDEFINED)
  {
    SCM result;

    result = scm_call_0(progress->cancel_scm_func);

    if (!scm_is_true(result))
      return;
  }

  if (progress->dialog != NULL)
    gtk_widget_hide(progress->dialog);
  progress->closed = TRUE;
  gnc_progress_maybe_destroy(progress);
}


static gboolean
delete_cb(GtkWidget *widget, GdkEvent  *event, gpointer data)
{
  GNCProgressDialog *progress = data;

  g_return_val_if_fail(progress, TRUE);

  if (progress->finished)
  {
    if (progress->dialog != NULL)
      gtk_widget_hide(progress->dialog);
    progress->closed = TRUE;
    gnc_progress_maybe_destroy(progress);
    return TRUE;
  }

  if (progress->cancel_func)
  {
    if (progress->cancel_func(progress->user_data))
    {
      if (progress->dialog != NULL)
        gtk_widget_hide(progress->dialog);
      progress->closed = TRUE;
      gnc_progress_maybe_destroy(progress);
      return TRUE;
    }
  }

  if (progress->cancel_scm_func != SCM_UNDEFINED)
  {
    SCM result;

    result = scm_call_0(progress->cancel_scm_func);

    if (scm_is_true(result))
    {
      if (progress->dialog != NULL)
        gtk_widget_hide(progress->dialog);
      progress->closed = TRUE;
      gnc_progress_maybe_destroy(progress);
      return TRUE;
    }
  }

  /* Don't delete the window, wait for gnc_progress_dialog_destroy. */
  return TRUE;
}


static void
destroy_cb(GtkObject *object, gpointer data)
{
  GNCProgressDialog *progress = data;

  g_return_if_fail(progress);

  /* Make sure the callbacks aren't invoked */
  progress->cancel_func = NULL;
  if (progress->cancel_scm_func != SCM_UNDEFINED)
    scm_gc_unprotect_object(progress->cancel_scm_func);
  progress->cancel_scm_func = SCM_UNDEFINED;

  g_free(progress);
}


static void
gnc_progress_dialog_create(GtkWidget * parent, GNCProgressDialog *progress)
{
  GtkWidget *dialog;
  GtkObject *tdo;
  GladeXML  *xml;

  g_return_if_fail(progress);

  xml = gnc_glade_xml_new("progress.glade", "Progress Dialog");

  dialog = glade_xml_get_widget(xml, "Progress Dialog");
  progress->dialog = dialog;
  tdo = GTK_OBJECT(dialog);

  /* parent */
  if (parent != NULL)
    gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(parent));

  g_signal_connect(tdo, "delete_event", G_CALLBACK(delete_cb), progress);

  g_signal_connect(tdo, "destroy", G_CALLBACK(destroy_cb), progress);

  progress->primary_label = glade_xml_get_widget(xml, "primary_label");
  gtk_widget_hide(progress->primary_label);

  progress->secondary_label = glade_xml_get_widget(xml, "secondary_label");
  gtk_widget_hide(progress->secondary_label);

  progress->progress_bar = glade_xml_get_widget(xml, "progress_bar");
  progress->total_offset = 0;
  progress->total_weight = 1;
  progress->bar_value = 0;

  progress->sub_label = glade_xml_get_widget(xml, "sub_label");
  gtk_widget_hide(progress->sub_label);

  progress->log = glade_xml_get_widget(xml, "progress_log");
  gtk_widget_hide(glade_xml_get_widget(xml, "progress_log_window"));

  progress->ok_button = glade_xml_get_widget(xml, "ok_button");

  g_signal_connect(progress->ok_button, "clicked",
                   G_CALLBACK(ok_cb), progress);

  if (!progress->use_ok_button)
    gtk_widget_hide(progress->ok_button);

  progress->cancel_button = glade_xml_get_widget(xml, "cancel_button");

  g_signal_connect(progress->cancel_button, "clicked",
                   G_CALLBACK(cancel_cb), progress);

  progress->cancel_func = NULL;
  progress->user_data = NULL;

  progress->cancel_scm_func = SCM_UNDEFINED;

  progress->closed = FALSE;
  progress->finished = FALSE;
  progress->destroyed = FALSE;
  progress->title_set = FALSE;
}


GNCProgressDialog *
gnc_progress_dialog_new(GtkWidget * parent, gboolean use_ok_button)
{
  GNCProgressDialog *progress;

  progress = g_new0(GNCProgressDialog, 1);

  progress->use_ok_button = use_ok_button;

  gnc_progress_dialog_create(parent, progress);

  gtk_widget_show(progress->dialog);

  gnc_progress_dialog_update(progress);

  return progress;
}


GNCProgressDialog *
gnc_progress_dialog_custom(GtkLabel       *primary,
                           GtkLabel       *secondary,
                           GtkProgressBar *bar,
                           GtkLabel       *suboperation,
                           GtkTextView    *log)
{
  GNCProgressDialog *progress;

  progress = g_new0(GNCProgressDialog, 1);

  /* Set up widgets. */
  progress->dialog = NULL;
  progress->primary_label = GTK_WIDGET(primary);
  progress->secondary_label = GTK_WIDGET(secondary);
  progress->progress_bar = GTK_WIDGET(bar);
  progress->sub_label = GTK_WIDGET(suboperation);
  progress->log = GTK_WIDGET(log);
  progress->ok_button = NULL;
  progress->cancel_button = NULL;

  /* Initialize all other items. */
  progress->total_offset = 0;
  progress->total_weight = 1;
  progress->bar_value = 0;
  progress->cancel_func = NULL;
  progress->user_data = NULL;
  progress->cancel_scm_func = SCM_UNDEFINED;
  progress->use_ok_button = FALSE;
  progress->closed = FALSE;
  progress->finished = FALSE;
  progress->destroyed = FALSE;
  progress->title_set = FALSE;

  return progress;
}


void
gnc_progress_dialog_set_title(GNCProgressDialog *progress, const char *title)
{
  g_return_if_fail(progress);

  if (!progress->dialog)
    return;

  if (title == NULL)
    title = "";

  gtk_window_set_title(GTK_WINDOW(progress->dialog), title);

  progress->title_set = TRUE;

  gnc_progress_dialog_update(progress);
}


void
gnc_progress_dialog_set_primary(GNCProgressDialog *progress,
                                const gchar *str)
{
  g_return_if_fail(progress);

  if (progress->primary_label == NULL)
    return;

  if (str == NULL || *str == '\0')
    gtk_widget_hide(progress->primary_label);
  else
  {
    /* Display the primary text with the HIG-recommended style. */
    char *markup = g_markup_printf_escaped("<span weight=\"bold\" size=\"larger\">%s</span>", str);

    gtk_label_set_markup(GTK_LABEL(progress->primary_label), markup);
    g_free(markup);
    gtk_widget_show(progress->primary_label);
  }

  gnc_progress_dialog_update(progress);
}


void
gnc_progress_dialog_set_heading(GNCProgressDialog *progress,
                                 const char *heading)
{
  g_return_if_fail(progress);

  if (progress->primary_label == NULL)
    return;

  if (heading == NULL || *heading == '\0')
    gtk_widget_hide(progress->primary_label);
  else
  {
    gtk_label_set_text(GTK_LABEL(progress->primary_label), heading);
    gtk_widget_show(progress->primary_label);
  }

  gnc_progress_dialog_update(progress);
}


void
gnc_progress_dialog_set_secondary(GNCProgressDialog *progress,
                                  const gchar *str)
{
  g_return_if_fail(progress);

  if (progress->secondary_label == NULL)
    return;

  if (str == NULL || *str == '\0')
    gtk_widget_hide(progress->secondary_label);
  else
  {
    gtk_label_set_text(GTK_LABEL(progress->secondary_label), str);
    gtk_widget_show(progress->secondary_label);
  }

  gnc_progress_dialog_update(progress);
}


void
gnc_progress_dialog_set_sub(GNCProgressDialog *progress,
                            const gchar *str)
{
  g_return_if_fail(progress);

  if (progress->sub_label == NULL)
    return;

  if (str == NULL || *str == '\0')
    gtk_widget_hide(progress->sub_label);
  else
  {
    /* Display the suboperation text with the HIG-recommended style. */
    char *markup = g_markup_printf_escaped("<span style=\"italic\">%s</span>", str);

    gtk_label_set_markup(GTK_LABEL(progress->sub_label), markup);
    g_free(markup);
    gtk_widget_show(progress->sub_label);
  }

  gnc_progress_dialog_update(progress);
}


void
gnc_progress_dialog_reset_log(GNCProgressDialog *progress)
{
  GtkTextBuffer *buf;

  g_return_if_fail(progress);

  if (progress->log == NULL)
    return;

  /* Reset the text buffer. */
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(progress->log));
  gtk_text_buffer_set_text(buf, "", -1);
  gtk_text_buffer_set_modified(buf, FALSE);

  /* Show the log and its parent (in case it is in a scrolled window). */
  gtk_widget_show(progress->log);
  gtk_widget_show(gtk_widget_get_parent(progress->log));

  gnc_progress_dialog_update(progress);
}


void
gnc_progress_dialog_append_log(GNCProgressDialog *progress, const gchar *str)
{
  GtkTextBuffer *buf;
  GtkTextIter    iter;

  g_return_if_fail(progress);

  if (progress->log == NULL || !str || !*str)
    return;

  /* Append to the text buffer. */
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(progress->log));
  gtk_text_buffer_get_end_iter(buf, &iter);
  gtk_text_buffer_insert(buf, &iter, str, -1);

  gnc_progress_dialog_update(progress);
}


void
gnc_progress_dialog_pause(GNCProgressDialog *progress)
{
  gchar *suffix;

  g_return_if_fail(progress);

  suffix = g_strconcat(" ", _("(paused)"), NULL);

  if (progress->sub_label && GTK_WIDGET_VISIBLE(progress->sub_label))
  {
    const gchar *txt = gtk_label_get_text(GTK_LABEL(progress->sub_label));

    if (txt && !g_str_has_suffix(txt, suffix))
    {
      gchar *newtxt = g_strconcat(txt, suffix, NULL);
      gnc_progress_dialog_set_sub(progress, newtxt);
      g_free(newtxt);
    }
  }
  else if (progress->dialog)
  {
    const gchar *txt = gtk_window_get_title(GTK_WINDOW(progress->dialog));

    if (txt && !g_str_has_suffix(txt, suffix))
    {
      gchar *newtxt = g_strconcat(txt, suffix, NULL);
      gtk_window_set_title(GTK_WINDOW(progress->dialog), newtxt);
      g_free(newtxt);
    }
  }
  else if (progress->primary_label &&
           GTK_WIDGET_VISIBLE(progress->primary_label))
  {
    const gchar *txt = gtk_label_get_text(GTK_LABEL(progress->primary_label));

    if (txt && !g_str_has_suffix(txt, suffix))
    {
      gchar *newtxt = g_strconcat(txt, suffix, NULL);
      gnc_progress_dialog_set_primary(progress, newtxt);
      g_free(newtxt);
    }
  }

  g_free(suffix);

  gnc_progress_dialog_update(progress);
}

void
gnc_progress_dialog_resume(GNCProgressDialog *progress)
{
  gchar *suffix;

  g_return_if_fail(progress);

  suffix = g_strconcat(" ", _("(paused)"), NULL);

  /* Remove any pause indication from the suboperation label. */
  if (progress->sub_label)
  {
    const gchar *txt = gtk_label_get_text(GTK_LABEL(progress->sub_label));

    if (txt && g_str_has_suffix(txt, suffix))
    {
      gchar *newtxt = g_strndup(txt, strlen(txt) - strlen(suffix));
      gnc_progress_dialog_set_sub(progress, newtxt);
      g_free(newtxt);
    }
  }

  /* Remove any pause indication from the window title. */
  if (progress->dialog)
  {
    const gchar *txt = gtk_window_get_title(GTK_WINDOW(progress->dialog));

    if (txt && g_str_has_suffix(txt, suffix))
    {
      gchar *newtxt = g_strndup(txt, strlen(txt) - strlen(suffix));
      gtk_window_set_title(GTK_WINDOW(progress->dialog), newtxt);
      g_free(newtxt);
    }
  }

  /* Remove any pause indication from the primary text. */
  if (progress->primary_label)
  {
    const gchar *txt = gtk_label_get_text(GTK_LABEL(progress->primary_label));

    if (txt && g_str_has_suffix(txt, suffix))
    {
      gchar *newtxt = g_strndup(txt, strlen(txt) - strlen(suffix));
      gnc_progress_dialog_set_primary(progress, newtxt);
      g_free(newtxt);
    }
  }

  g_free(suffix);

  gnc_progress_dialog_update(progress);
}


void
gnc_progress_dialog_set_cancel_func(GNCProgressDialog *progress,
                                    GNCProgressCancelFunc cancel_func,
                                    gpointer user_data)
{
  g_return_if_fail(progress);

  if (progress->cancel_button == NULL)
    return;

  progress->cancel_func = cancel_func;
  progress->user_data = user_data;

  if (cancel_func)
    gtk_widget_show(progress->cancel_button);
}


void
gnc_progress_dialog_set_cancel_scm_func(GNCProgressDialog *progress,
                                        SCM cancel_scm_func)
{
  g_return_if_fail(progress);

  if (progress->cancel_button == NULL)
    return;

  if (progress->cancel_scm_func != SCM_UNDEFINED)
    scm_gc_unprotect_object(progress->cancel_scm_func);

  if (scm_is_procedure(cancel_scm_func))
  {
    progress->cancel_scm_func = cancel_scm_func;
    scm_gc_protect_object(cancel_scm_func);
    gtk_widget_show(progress->cancel_button);
  }
  else
    progress->cancel_scm_func = SCM_UNDEFINED;
}


void
gnc_progress_dialog_set_value(GNCProgressDialog *progress, gdouble value)
{
  GtkProgressBar *bar;

  g_return_if_fail(progress);

  /* Get the progress bar widget. */
  bar = GTK_PROGRESS_BAR(progress->progress_bar);
  if (bar == NULL)
    return;

  /* Update the progress bar. If value is over 1,
   * the bar will pulse instead of fill. */
  if (value > 1)
      gtk_progress_bar_pulse(bar);
  else
  {
    progress->bar_value = value > 0 ? value : 0;
    gtk_progress_bar_set_fraction(bar,
      progress->total_offset + progress->bar_value * progress->total_weight);
  }

  gnc_progress_dialog_update(progress);
}


guint
gnc_progress_dialog_push(GNCProgressDialog *progress, gdouble weight)
{
  GtkProgressBar *bar;
  VirtualBar     *newbar;

  g_return_val_if_fail(progress, 0);
  g_return_val_if_fail(weight > 0, 0);

  /* Get the progress bar widget. */
  bar = GTK_PROGRESS_BAR(progress->progress_bar);
  if (bar == NULL)
    return 0;

  /* Create the new virtual progress bar. */
  newbar = g_new0(VirtualBar, 1);
  newbar->offset = progress->bar_value;
  if (newbar->offset + weight > 1)
    /* The requested weight is more than the unfilled portion of the bar. */
    newbar->weight = 1 - newbar->offset;
  else
    newbar->weight = weight;
  progress->bars = g_list_prepend(progress->bars, newbar);

  /* Set the total effective offset and weight */
  progress->total_offset = gtk_progress_bar_get_fraction(bar);
  progress->total_weight *= newbar->weight;

  /* Set the new bar as unfilled. */
  progress->bar_value = 0;

  return g_list_length(progress->bars);
}


guint
gnc_progress_dialog_pop(GNCProgressDialog *progress)
{
  VirtualBar     *bar;

  g_return_val_if_fail(progress, 0);

  /* Get the progress bar widget. */
  if (progress->progress_bar == NULL || progress->bars == NULL)
    return 0;

  /* Pop the bar off the bar stack. */
  bar = progress->bars->data;
  progress->bars = g_list_delete_link(progress->bars, progress->bars);

  /* Determine the value of the current bar. */
  progress->bar_value = bar->offset + bar->weight * progress->bar_value;

  /* Set the total effective offset and weight. */
  if (progress->bars == NULL)
  {
    progress->total_offset = 0;
    progress->total_weight = 1;
  }
  else
  {
    progress->total_offset -= bar->offset *
                              ((VirtualBar *) progress->bars->data)->weight;
    progress->total_weight /= bar->weight;
  }
  g_free(bar);

  if (progress->bars == NULL)
    return 0;
  return g_list_length(progress->bars);
}


guint
gnc_progress_dialog_pop_full(GNCProgressDialog *progress)
{
  gnc_progress_dialog_set_value(progress, 1);
  return gnc_progress_dialog_pop(progress);
}


void
gnc_progress_dialog_reset_value(GNCProgressDialog *progress)
{
  g_return_if_fail(progress);

  /* Return to the top level. */
  while (gnc_progress_dialog_pop(progress));

  /* Reset the bar to empty. */
  gnc_progress_dialog_set_value(progress, 0);
}


void
gnc_progress_dialog_update(GNCProgressDialog *progress)
{
  while (gtk_events_pending())
    gtk_main_iteration();
}


void
gnc_progress_dialog_finish(GNCProgressDialog *progress)
{
  g_return_if_fail(progress);

  if (!progress->use_ok_button)
  {
    if (progress->dialog != NULL)
      gtk_widget_hide(progress->dialog);
    progress->closed = TRUE;
  }

  gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progress->progress_bar), 1.0);

  gtk_widget_set_sensitive(progress->ok_button, TRUE);
  gtk_widget_set_sensitive(progress->cancel_button, FALSE);

  if (GTK_WIDGET_VISIBLE(progress->primary_label))
    gnc_progress_dialog_set_heading(progress, _("Complete"));

  if (!progress->title_set)
    gtk_window_set_title(GTK_WINDOW(progress->dialog), _("Complete"));

  gtk_window_set_modal(GTK_WINDOW(progress->dialog), FALSE);

  progress->finished = TRUE;

  gnc_progress_dialog_update(progress);
}


void
gnc_progress_dialog_destroy(GNCProgressDialog *progress)
{
  g_return_if_fail(progress);

  /* Make sure the callbacks aren't invoked */
  progress->cancel_func = NULL;
  if (progress->cancel_scm_func != SCM_UNDEFINED)
    scm_gc_unprotect_object(progress->cancel_scm_func);
  progress->cancel_scm_func = SCM_UNDEFINED;

  if (!progress->finished)
  {
    if (progress->dialog != NULL)
      gtk_widget_hide(progress->dialog);
    progress->closed = TRUE;
  }

  progress->destroyed = TRUE;

  gnc_progress_maybe_destroy(progress);
}
