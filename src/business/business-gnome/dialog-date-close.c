/*
 * dialog-date-close.c -- Dialog to ask a question and request a date
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"

#include "dialog-date-close.h"

typedef struct _dialog_date_close_window {
  GtkWidget *dialog;
  GtkWidget *date;
  Timespec *ts;
  gboolean retval;
} DialogDateClose;

static void
gnc_dialog_date_close_ok_cb (GtkWidget *widget, gpointer user_data)
{
  DialogDateClose *ddc = user_data;
  time_t tt;

  tt = gnome_date_edit_get_date (GNOME_DATE_EDIT (ddc->date));
  timespecFromTime_t (ddc->ts, tt);
  ddc->retval = TRUE;
  gnome_dialog_close (GNOME_DIALOG (ddc->dialog));
}

static void
gnc_dialog_date_close_cancel_cb (GtkWidget *widget, gpointer user_data)
{
  DialogDateClose *ddc = user_data;
  ddc->retval = FALSE;
  gnome_dialog_close (GNOME_DIALOG (ddc->dialog));
}

static gint
gnc_dialog_date_close_cb (GnomeDialog *dialog, gpointer data)
{
  gtk_main_quit ();
  return FALSE;
}

static void
build_date_close_window (GtkWidget *hbox, const char *message)
{
  GtkWidget *pixmap = NULL;
  GtkWidget *label;
  GtkWidget *alignment;
  char *s;

  /* Make noises, basically */
  gnome_triggers_vdo(message, GNOME_MESSAGE_BOX_QUESTION, NULL);

  s = gnome_unconditional_pixmap_file("gnome-question.png");
  if (s) {
    pixmap = gnome_pixmap_new_from_file(s);
    g_free(s);
  }

  if (pixmap) {
    gtk_box_pack_start (GTK_BOX(hbox), pixmap, FALSE, TRUE, 0);
    gtk_widget_show (pixmap);
  }

  label = gtk_label_new (message);
  gtk_label_set_justify (GTK_LABEL (label), GTK_JUSTIFY_LEFT);
  gtk_misc_set_padding (GTK_MISC (label), GNOME_PAD, 0);
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);
  gtk_widget_show (label);

  /* Add some extra space on the right to balance the pixmap */
  if (pixmap) {
    alignment = gtk_alignment_new (0., 0., 0., 0.);
    gtk_widget_set_usize (alignment, GNOME_PAD, -1);
    gtk_widget_show (alignment);
    
    gtk_box_pack_start (GTK_BOX (hbox), alignment, FALSE, FALSE, 0);
  }
}

gboolean
gnc_dialog_date_close_parented (GtkWidget *parent, const char *message,
				const char *label_message,
				gboolean ok_is_default, Timespec *ts)
{
  DialogDateClose *ddc;
  GtkWidget *hbox;
  GtkWidget *label;
  GladeXML *xml;
  gboolean retval;

  if (!message || !label_message || !ts)
    return FALSE;

  ddc = g_new0 (DialogDateClose, 1);
  ddc->ts = ts;

  xml = gnc_glade_xml_new ("date-close.glade", "Date Close Dialog");
  ddc->dialog = glade_xml_get_widget (xml, "Date Close Dialog");
  ddc->date = glade_xml_get_widget (xml, "date");
  hbox = glade_xml_get_widget (xml, "the_hbox");
  label = glade_xml_get_widget (xml, "label");

  if (parent)
    gnome_dialog_set_parent (GNOME_DIALOG(ddc->dialog), GTK_WINDOW(parent));

  build_date_close_window (hbox, message);

  gnome_date_edit_set_time (GNOME_DATE_EDIT (ddc->date), ts->tv_sec);
  gtk_label_set_text (GTK_LABEL (label), label_message);

  gnome_dialog_button_connect
    (GNOME_DIALOG(ddc->dialog), 0,
     GTK_SIGNAL_FUNC(gnc_dialog_date_close_ok_cb), ddc);
  gnome_dialog_button_connect
    (GNOME_DIALOG(ddc->dialog), 1,
     GTK_SIGNAL_FUNC(gnc_dialog_date_close_cancel_cb), ddc);

  gtk_signal_connect (GTK_OBJECT(ddc->dialog), "close",
                      GTK_SIGNAL_FUNC(gnc_dialog_date_close_cb), ddc);

  gtk_window_set_modal (GTK_WINDOW (ddc->dialog), TRUE);
  gtk_widget_show (ddc->dialog);
  gtk_main ();

  retval = ddc->retval;
  g_free (ddc);

  return retval;
}
