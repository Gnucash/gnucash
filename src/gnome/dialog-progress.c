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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <gnome.h>
#include <guile/gh.h>

#include "dialog-progress.h"
#include "dialog-utils.h"
#include "messages.h"


struct _GNCProgressDialog
{
  GtkWidget *dialog;

  GtkWidget *heading_label;
  GtkWidget *progress_bar;

  GtkWidget *ok_button;
  GtkWidget *cancel_button;

  GNCProgressCancelFunc cancel_func;
  gpointer user_data;

  SCM cancel_scm_func;

  gboolean use_ok_button;
  gboolean closed;
  gboolean finished;
  gboolean destroyed;
  gboolean title_set;
};


static void
gnc_progress_maybe_destroy (GNCProgressDialog *progress)
{
  if (!(progress->closed && progress->destroyed))
    return;

  gtk_widget_destroy(progress->dialog);
}

static void
ok_cb(GtkWidget * widget, gpointer data)
{
  GNCProgressDialog *progress = data; 

  gtk_widget_hide(progress->dialog);
  progress->closed = TRUE;
  gnc_progress_maybe_destroy (progress);
}

static void
cancel_cb(GtkWidget * widget, gpointer data)
{
  GNCProgressDialog *progress = data; 

  if (progress->cancel_func && !progress->cancel_func (progress->user_data))
    return;

  if (progress->cancel_scm_func != SCM_UNDEFINED)
  {
    SCM result;

    result = gh_call0(progress->cancel_scm_func);

    if (!gh_scm2bool (result))
      return;
  }

  gtk_widget_hide(progress->dialog);
  progress->closed = TRUE;
  gnc_progress_maybe_destroy (progress);
}

static gboolean
delete_cb(GtkWidget *widget, GdkEvent  *event, gpointer data)
{
  GNCProgressDialog *progress = data; 

  if (progress->finished)
  {
    gtk_widget_hide(progress->dialog);
    progress->closed = TRUE;
    gnc_progress_maybe_destroy (progress);
    return TRUE;
  }

  if (progress->cancel_func)
  {
    if (progress->cancel_func (progress->user_data))
    {
      gtk_widget_hide(progress->dialog);
      progress->closed = TRUE;
      gnc_progress_maybe_destroy (progress);
      return TRUE;
    }
  }

  if (progress->cancel_scm_func != SCM_UNDEFINED)
  {
    SCM result;

    result = gh_call0(progress->cancel_scm_func);

    if (gh_scm2bool (result))
    {
      gtk_widget_hide(progress->dialog);
      progress->closed = TRUE;
      gnc_progress_maybe_destroy (progress);
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

  /* Make sure the callbacks aren't invoked */
  progress->cancel_func = NULL;
  if (progress->cancel_scm_func != SCM_UNDEFINED)
    scm_unprotect_object (progress->cancel_scm_func);
  progress->cancel_scm_func = SCM_UNDEFINED;

  g_free(progress);
}

static void
gnc_progress_dialog_create(GtkWidget * parent, GNCProgressDialog *progress)
{
  GtkWidget *dialog;
  GtkObject *tdo;
  GladeXML  *xml;

  xml = gnc_glade_xml_new ("progress.glade", "Progress Dialog");

  dialog = glade_xml_get_widget (xml, "Progress Dialog");
  progress->dialog = dialog;
  tdo = GTK_OBJECT (dialog);

  /* parent */
  if (parent != NULL)
    gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(parent));

  gtk_signal_connect (tdo, "delete_event",
                      GTK_SIGNAL_FUNC (delete_cb), progress);

  gtk_signal_connect (tdo, "destroy", GTK_SIGNAL_FUNC (destroy_cb), progress);

  progress->heading_label = glade_xml_get_widget (xml, "heading_label");
  gtk_widget_hide(progress->heading_label);

  progress->progress_bar = glade_xml_get_widget (xml, "progress_bar");
  gtk_progress_set_show_text (GTK_PROGRESS(progress->progress_bar), TRUE);
  gtk_progress_configure (GTK_PROGRESS(progress->progress_bar),
                          0.0, 0.0, 100.0);

  progress->ok_button = glade_xml_get_widget (xml, "ok_button");

  gtk_signal_connect(GTK_OBJECT(progress->ok_button), "clicked",
                     GTK_SIGNAL_FUNC(ok_cb), progress);

  if (!progress->use_ok_button)
    gtk_widget_hide (progress->ok_button);

  progress->cancel_button = glade_xml_get_widget (xml, "cancel_button");

  gtk_signal_connect(GTK_OBJECT(progress->cancel_button), "clicked",
                     GTK_SIGNAL_FUNC(cancel_cb), progress);

  progress->cancel_func = NULL;
  progress->user_data = NULL;

  progress->cancel_scm_func = SCM_UNDEFINED;

  progress->closed = FALSE;
  progress->finished = FALSE;
  progress->destroyed = FALSE;
  progress->title_set = FALSE;
}

GNCProgressDialog *
gnc_progress_dialog_new (GtkWidget * parent, gboolean use_ok_button)
{
  GNCProgressDialog *progress;

  progress = g_new0(GNCProgressDialog, 1);

  progress->use_ok_button = use_ok_button;

  gnc_progress_dialog_create(parent, progress);

  gtk_widget_show(progress->dialog);

  gnc_progress_dialog_update (progress);

  return progress;
}

void
gnc_progress_dialog_set_title (GNCProgressDialog *progress, const char *title)
{
  if (progress == NULL)
    return;

  if (title == NULL)
    title = "";

  gtk_window_set_title (GTK_WINDOW (progress->dialog), title);

  progress->title_set = TRUE;

  gnc_progress_dialog_update (progress);
}

void
gnc_progress_dialog_set_heading (GNCProgressDialog *progress,
                                 const char *heading)
{
  if (progress == NULL)
    return;

  if (heading == NULL || *heading == '\0')
    gtk_widget_hide (progress->heading_label);
  else
  {
    gtk_label_set_text (GTK_LABEL (progress->heading_label), heading);
    gtk_widget_show (progress->heading_label);
  }

  gnc_progress_dialog_update (progress);
}

void
gnc_progress_dialog_set_limits (GNCProgressDialog *progress,
                                gfloat min, gfloat max)
{
  if (progress == NULL)
    return;

  gtk_progress_configure (GTK_PROGRESS (progress->progress_bar),
                          min, min, max);

  gnc_progress_dialog_update (progress);
}

void
gnc_progress_dialog_set_activity_mode (GNCProgressDialog *progress,
                                       gboolean activity_mode)
{
  if (progress == NULL)
    return;

  gtk_progress_set_activity_mode (GTK_PROGRESS (progress->progress_bar),
                                  activity_mode);

  gnc_progress_dialog_update (progress);
}

void
gnc_progress_dialog_set_cancel_func (GNCProgressDialog *progress,
                                     GNCProgressCancelFunc cancel_func,
                                     gpointer user_data)
{
  if (progress == NULL)
    return;

  progress->cancel_func = cancel_func;
  progress->user_data = user_data;

  if (cancel_func)
    gtk_widget_show (progress->cancel_button);
}

void
gnc_progress_dialog_set_cancel_scm_func (GNCProgressDialog *progress,
                                         SCM cancel_scm_func)
{
  if (progress == NULL)
    return;

  if (progress->cancel_scm_func != SCM_UNDEFINED)
    scm_unprotect_object (progress->cancel_scm_func);

  if (gh_procedure_p(cancel_scm_func))
  {
    progress->cancel_scm_func = cancel_scm_func;
    scm_protect_object (cancel_scm_func);
    gtk_widget_show (progress->cancel_button);
  }
  else
    progress->cancel_scm_func = SCM_UNDEFINED;
}

void
gnc_progress_dialog_set_value (GNCProgressDialog *progress, gfloat value)
{
  if (progress == NULL)
    return;

  gtk_progress_set_value (GTK_PROGRESS (progress->progress_bar), value);

  gnc_progress_dialog_update (progress);
}

void
gnc_progress_dialog_update (GNCProgressDialog *progress)
{
  while (gtk_events_pending())
    gtk_main_iteration();
}

void
gnc_progress_dialog_finish (GNCProgressDialog *progress)
{
  if (progress == NULL)
    return;

  if (!progress->use_ok_button)
  {
    gtk_widget_hide (progress->dialog);
    progress->closed = TRUE;
  }

  gtk_progress_set_percentage (GTK_PROGRESS (progress->progress_bar), 1.0);

  gtk_widget_set_sensitive (progress->ok_button, TRUE);
  gtk_widget_set_sensitive (progress->cancel_button, FALSE);

  if (GTK_WIDGET_VISIBLE(progress->heading_label))
    gnc_progress_dialog_set_heading (progress, _("Complete"));

  if (!progress->title_set)
    gtk_window_set_title (GTK_WINDOW (progress->dialog), _("Complete"));

  gtk_window_set_modal (GTK_WINDOW (progress->dialog), FALSE);

  progress->finished = TRUE;

  gnc_progress_dialog_update (progress);
}

void
gnc_progress_dialog_destroy (GNCProgressDialog *progress)
{
  if (progress == NULL)
    return;

  /* Make sure the callbacks aren't invoked */
  progress->cancel_func = NULL;
  if (progress->cancel_scm_func != SCM_UNDEFINED)
    scm_unprotect_object (progress->cancel_scm_func);
  progress->cancel_scm_func = SCM_UNDEFINED;

  if (!progress->finished)
  {
    gtk_widget_hide (progress->dialog);
    progress->closed = TRUE;
  }

  progress->destroyed = TRUE;

  gnc_progress_maybe_destroy (progress);
}
