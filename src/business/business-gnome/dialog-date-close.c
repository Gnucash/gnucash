/*
 * dialog-date-close.c -- Dialog to ask a question and request a date
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"
#include "gnc-engine-util.h"
#include "gnc-gui-query.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-date-edit.h"
#include "gnc-account-sel.h"

#include "business-gnome-utils.h"
#include "dialog-date-close.h"

typedef struct _dialog_date_close_window {
  GtkWidget *dialog;
  GtkWidget *date;
  GtkWidget *post_date;
  GtkWidget *acct_combo;
  GtkWidget *memo_entry;
  GtkWidget *question_check;
  GncBillTerm *terms;
  Timespec *ts, *ts2;
  GList * acct_types;
  GNCBook *book;
  Account *acct;
  char **memo;
  gboolean retval;
  gboolean answer;
} DialogDateClose;

static void
gnc_dialog_date_close_ok_cb (GtkWidget *widget, gpointer user_data)
{
  DialogDateClose *ddc = user_data;

  if (ddc->acct_combo) {
    Account *acc;

    acc = gnc_account_sel_get_account( GNC_ACCOUNT_SEL(ddc->acct_combo) );

    if (!acc) {
      gnc_error_dialog (ddc->dialog,
			_("No Account selected.  Please try again."));
      return;
    }

    ddc->acct = acc;
  }

  if (ddc->post_date)
    *(ddc->ts2) = gnc_date_edit_get_date_ts (GNC_DATE_EDIT (ddc->post_date));

  if (ddc->date) {
    if (ddc->terms)
      *(ddc->ts) = gncBillTermComputeDueDate (ddc->terms, *(ddc->ts2));
    else
      *(ddc->ts) = gnc_date_edit_get_date_ts (GNC_DATE_EDIT (ddc->date));
  }

  if (ddc->memo_entry && ddc->memo)
    *(ddc->memo) = gtk_editable_get_chars (GTK_EDITABLE (ddc->memo_entry),
					   0, -1);
  if (ddc->question_check)
	  ddc->answer = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ddc->question_check));
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
fill_in_acct_info (DialogDateClose *ddc)
{
  GNCAccountSel *gas = GNC_ACCOUNT_SEL (ddc->acct_combo);

  /* How do I set the book? */
  gnc_account_sel_set_acct_filters( gas, ddc->acct_types );
  gnc_account_sel_set_new_account_ability( gas, TRUE );
  gnc_account_sel_set_new_account_modal( gas, TRUE );

  /* XXX: Some way to remember the last selection? */
  gnc_account_sel_set_account( gas, NULL );
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
				gboolean ok_is_default,
				/* Returned data ... */
				Timespec *ts)
{
  DialogDateClose *ddc;
  GtkWidget *date_box;
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
  hbox = glade_xml_get_widget (xml, "the_hbox");
  label = glade_xml_get_widget (xml, "label");

  date_box = glade_xml_get_widget (xml, "date_box");
  ddc->date = gnc_date_edit_new (time(NULL), FALSE, FALSE);
  gtk_box_pack_start (GTK_BOX(date_box), ddc->date, TRUE, TRUE, 0);

  if (parent)
    gnome_dialog_set_parent (GNOME_DIALOG(ddc->dialog), GTK_WINDOW(parent));

  build_date_close_window (hbox, message);

  gnc_date_edit_set_time_ts (GNC_DATE_EDIT (ddc->date), *ts);
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
  gtk_widget_show_all (ddc->dialog);
  gtk_main ();

  retval = ddc->retval;
  g_list_free (ddc->acct_types);
  g_free (ddc);

  return retval;
}

static void
post_date_changed_cb (GNCDateEdit *gde, gpointer d)
{
  DialogDateClose *ddc = d;
  Timespec post_date;
  Timespec due_date;

  post_date = gnc_date_edit_get_date_ts (gde);
  due_date = gncBillTermComputeDueDate (ddc->terms, post_date);
  gnc_date_edit_set_time_ts (GNC_DATE_EDIT (ddc->date), due_date);
}

gboolean
gnc_dialog_dates_acct_question_parented (GtkWidget *parent, const char *message,
				const char *ddue_label_message,
				const char *post_label_message,
				const char *acct_label_message,
				const char *question_check_message,
				gboolean ok_is_default,
				GList * acct_types, GNCBook *book,
				GncBillTerm *terms,
				/* Returned Data... */
				Timespec *ddue, Timespec *post,
				char **memo, Account **acct, gboolean *answer)
{
  DialogDateClose *ddc;
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *date_box;
  GtkWidget *acct_box;
  GladeXML *xml;
  gboolean retval;

  if (!message || !ddue_label_message || !post_label_message ||
      !acct_label_message || !acct_types || !book || !ddue || !post || !acct)
    return FALSE;
  if (question_check_message && !answer)
	  return FALSE;

  ddc = g_new0 (DialogDateClose, 1);
  ddc->ts = ddue;
  ddc->ts2 = post;
  ddc->book = book;
  ddc->acct_types = acct_types;
  ddc->memo = memo;
  ddc->terms = terms;

  xml = gnc_glade_xml_new ("date-close.glade", "Date Account Dialog");
  ddc->dialog = glade_xml_get_widget (xml, "Date Account Dialog");
  ddc->memo_entry = glade_xml_get_widget (xml, "memo_entry");
  hbox = glade_xml_get_widget (xml, "the_hbox");

  acct_box = glade_xml_get_widget (xml, "acct_hbox");
  ddc->acct_combo = gnc_account_sel_new();
  gtk_box_pack_start (GTK_BOX(acct_box), ddc->acct_combo, TRUE, TRUE, 0);

  date_box = glade_xml_get_widget (xml, "date_box");
  ddc->date = gnc_date_edit_new (time(NULL), FALSE, FALSE);
  gtk_box_pack_start (GTK_BOX(date_box), ddc->date, TRUE, TRUE, 0);

  date_box = glade_xml_get_widget (xml, "post_date_box");
  ddc->post_date = gnc_date_edit_new (time(NULL), FALSE, FALSE);
  gtk_box_pack_start (GTK_BOX(date_box), ddc->post_date, TRUE, TRUE, 0);

  ddc->question_check = glade_xml_get_widget(xml, "question_check");
  
  if (parent)
    gnome_dialog_set_parent (GNOME_DIALOG(ddc->dialog), GTK_WINDOW(parent));

  build_date_close_window (hbox, message);

  /* Set the labels */
  label = glade_xml_get_widget (xml, "date_label");
  gtk_label_set_text (GTK_LABEL (label), ddue_label_message);
  label = glade_xml_get_widget (xml, "postdate_label");
  gtk_label_set_text (GTK_LABEL (label), post_label_message);
  label = glade_xml_get_widget (xml, "acct_label");
  gtk_label_set_text (GTK_LABEL (label), acct_label_message);

  if (question_check_message)
  {
	  gtk_label_set_text(GTK_LABEL(GTK_BIN(ddc->question_check)->child), question_check_message);
	  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ddc->question_check), *answer);
  } else {
	  gtk_widget_hide(ddc->question_check);
	  gtk_widget_hide(glade_xml_get_widget(xml, "hide1"));
  }


  /* Set the post date widget */
  gnc_date_edit_set_time_ts (GNC_DATE_EDIT (ddc->post_date), *post);

  /* Deal with the terms handling of the due date */
  if (terms) {
    gtk_signal_connect (GTK_OBJECT (ddc->post_date), "date_changed",
			post_date_changed_cb, ddc);
    gtk_widget_set_sensitive (ddc->date, FALSE);
    post_date_changed_cb (GNC_DATE_EDIT (ddc->post_date), ddc);
  } else
    gnc_date_edit_set_time_ts (GNC_DATE_EDIT (ddc->date), *ddue);

  /* Setup the account widget */
  fill_in_acct_info (ddc);

  /* Connect the buttons */
  gnome_dialog_button_connect
    (GNOME_DIALOG(ddc->dialog), 0,
     GTK_SIGNAL_FUNC(gnc_dialog_date_close_ok_cb), ddc);
  gnome_dialog_button_connect
    (GNOME_DIALOG(ddc->dialog), 1,
     GTK_SIGNAL_FUNC(gnc_dialog_date_close_cancel_cb), ddc);

  gtk_signal_connect (GTK_OBJECT(ddc->dialog), "close",
                      GTK_SIGNAL_FUNC(gnc_dialog_date_close_cb), ddc);

  gtk_window_set_modal (GTK_WINDOW (ddc->dialog), TRUE);
  gtk_widget_show_all (ddc->dialog);
  gtk_main ();

  retval = ddc->retval;
  *acct = ddc->acct;
  if (question_check_message)
	  *answer = ddc->answer;
  g_free (ddc);

  return retval;
}

gboolean
gnc_dialog_date_acct_parented (GtkWidget *parent, const char *message,
			       const char *date_label_message,
			       const char *acct_label_message,
			       gboolean ok_is_default,
			       GList * acct_types, GNCBook *book,
			       /* Returned Data... */
			       Timespec *date, Account **acct)
{
  DialogDateClose *ddc;
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *date_box;
  GtkWidget *acct_box;
  GladeXML *xml;
  gboolean retval;

  if (!message || !date_label_message || !acct_label_message ||
      !acct_types || !book || !date || !acct)
    return FALSE;

  ddc = g_new0 (DialogDateClose, 1);
  ddc->ts = date;
  ddc->book = book;
  ddc->acct_types = acct_types;

  xml = gnc_glade_xml_new ("date-close.glade", "Date Account Dialog");
  ddc->dialog = glade_xml_get_widget (xml, "Date Account Dialog");
  hbox = glade_xml_get_widget (xml, "the_hbox");

  acct_box = glade_xml_get_widget (xml, "acct_hbox");
  ddc->acct_combo = gnc_account_sel_new();
  gtk_box_pack_start (GTK_BOX(acct_box), ddc->acct_combo, TRUE, TRUE, 0);

  date_box = glade_xml_get_widget (xml, "date_box");
  ddc->date = gnc_date_edit_new (time(NULL), FALSE, FALSE);
  gtk_box_pack_start (GTK_BOX(date_box), ddc->date, TRUE, TRUE, 0);

  if (parent)
    gnome_dialog_set_parent (GNOME_DIALOG(ddc->dialog), GTK_WINDOW(parent));

  build_date_close_window (hbox, message);

  /* Set the labels */
  label = glade_xml_get_widget (xml, "date_label");
  gtk_label_set_text (GTK_LABEL (label), date_label_message);
  label = glade_xml_get_widget (xml, "acct_label");
  gtk_label_set_text (GTK_LABEL (label), acct_label_message);

  /* Set the date widget */
  gnc_date_edit_set_time_ts (GNC_DATE_EDIT (ddc->date), *date);

  /* Setup the account widget */
  fill_in_acct_info (ddc);

  /* Connect the buttons */
  gnome_dialog_button_connect
    (GNOME_DIALOG(ddc->dialog), 0,
     GTK_SIGNAL_FUNC(gnc_dialog_date_close_ok_cb), ddc);
  gnome_dialog_button_connect
    (GNOME_DIALOG(ddc->dialog), 1,
     GTK_SIGNAL_FUNC(gnc_dialog_date_close_cancel_cb), ddc);

  gtk_signal_connect (GTK_OBJECT(ddc->dialog), "close",
                      GTK_SIGNAL_FUNC(gnc_dialog_date_close_cb), ddc);

  gtk_window_set_modal (GTK_WINDOW (ddc->dialog), TRUE);
  gtk_widget_show_all (ddc->dialog);

  gtk_widget_hide_all (glade_xml_get_widget (xml, "postdate_label"));
  gtk_widget_hide_all (glade_xml_get_widget (xml, "post_date"));
  gtk_widget_hide_all (glade_xml_get_widget (xml, "memo_entry"));
  gtk_widget_hide_all (glade_xml_get_widget (xml, "memo_label"));

  gtk_main ();

  retval = ddc->retval;
  *acct = ddc->acct;
  g_free (ddc);

  return retval;
}
