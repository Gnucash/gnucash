/*
 * dialog-date-close.c -- Dialog to ask a question and request a date
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"
#include "gnc-engine-util.h"
#include "gnc-gui-query.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "AccWindow.h"
#include "gnc-date-edit.h"

#include "business-utils.h"
#include "dialog-date-close.h"

typedef struct _dialog_date_close_window {
  GtkWidget *dialog;
  GtkWidget *date;
  GtkWidget *post_date;
  GtkWidget *acct_combo;
  GtkWidget *memo_entry;
  GncBillTerm *terms;
  Timespec *ts, *ts2;
  GList * acct_types;
  GNCBook *book;
  Account *acct;
  char **memo;
  gboolean retval;
} DialogDateClose;

static Account *
ask_make_acct (DialogDateClose *ddc, const char *name, gboolean new_acc)
{
  GList *types = NULL;
  Account *acc;

  if (new_acc) {
    const gchar *format  = _("The account \"%s\" does not exist.\n"
			     "Would you like to create it?");
    if (!gnc_verify_dialog_parented (ddc->dialog, TRUE, format, name))
      return NULL;
  }

  acc = gnc_ui_new_accounts_from_name_window_with_types (name,
							 ddc->acct_types);
  g_list_free (types);
  return acc;
}

static void
gnc_dialog_date_close_ok_cb (GtkWidget *widget, gpointer user_data)
{
  DialogDateClose *ddc = user_data;

  if (ddc->acct_combo) {
    Account *acc;
    gboolean new_acc = TRUE;
    char *name =
      g_strdup (gtk_entry_get_text
		(GTK_ENTRY ((GTK_COMBO (ddc->acct_combo))->entry)));

    if (!name || safe_strcmp (name, "") == 0) {
      g_free (name);
      name = _("You must enter an account name.");
      gnc_error_dialog_parented (GTK_WINDOW (ddc->dialog), name);
      return;
    }

    acc = xaccGetAccountFromFullName (gnc_book_get_group (ddc->book),
				      name, gnc_get_account_separator ());
    while (!acc) {
      acc = ask_make_acct (ddc, name, new_acc);
      if (!acc) {
	g_free (name);
	return;
      }

      if (g_list_index (ddc->acct_types, (gpointer)xaccAccountGetType (acc))
	  == -1) {
	const char *format = _("Invalid Account Type, %s.\n"
			       "Please try again...");

	gnc_error_dialog_parented (GTK_WINDOW (ddc->dialog),
				   format,
				   xaccAccountGetTypeStr (xaccAccountGetType (acc)));
	g_free (name);
	name = xaccAccountGetFullName (acc, gnc_get_account_separator ());
	acc = NULL;
      }

      new_acc = FALSE;
    }

    g_free (name);
    ddc->acct = acc;
  }

  if (ddc->date)
    *(ddc->ts) = gnc_date_edit_get_date_ts (GNC_DATE_EDIT (ddc->date));

  if (ddc->post_date)
    *(ddc->ts2) = gnc_date_edit_get_date_ts (GNC_DATE_EDIT (ddc->post_date));

  if (ddc->memo_entry && ddc->memo)
    *(ddc->memo) = gtk_editable_get_chars (GTK_EDITABLE (ddc->memo_entry),
					   0, -1);

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
  gnc_fill_account_select_combo (ddc->acct_combo, ddc->book, ddc->acct_types);
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
gnc_dialog_dates_acct_parented (GtkWidget *parent, const char *message,
				const char *ddue_label_message,
				const char *post_label_message,
				const char *acct_label_message,
				gboolean ok_is_default,
				GList * acct_types, GNCBook *book,
				GncBillTerm *terms,
				/* Returned Data... */
				Timespec *ddue, Timespec *post,
				char **memo, Account **acct)
{
  DialogDateClose *ddc;
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *date_box;
  GladeXML *xml;
  gboolean retval;

  if (!message || !ddue_label_message || !post_label_message ||
      !acct_label_message || !acct_types || !book || !ddue || !post || !acct)
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
  ddc->acct_combo = glade_xml_get_widget (xml, "acct_combo");
  hbox = glade_xml_get_widget (xml, "the_hbox");
  ddc->memo_entry = glade_xml_get_widget (xml, "memo_entry");

  date_box = glade_xml_get_widget (xml, "date_box");
  ddc->date = gnc_date_edit_new (time(NULL), FALSE, FALSE);
  gtk_box_pack_start (GTK_BOX(date_box), ddc->date, TRUE, TRUE, 0);

  date_box = glade_xml_get_widget (xml, "post_date_box");
  ddc->post_date = gnc_date_edit_new (time(NULL), FALSE, FALSE);
  gtk_box_pack_start (GTK_BOX(date_box), ddc->post_date, TRUE, TRUE, 0);

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

  /* Set the post date widget */
  gnc_date_edit_set_time_ts (GNC_DATE_EDIT (ddc->post_date), *post);

  /* Deal with the terms handling of the due date */
  if (terms) {
    gtk_signal_connect (GTK_OBJECT (ddc->post_date), "date_changed",
			post_date_changed_cb, ddc);
    gtk_widget_set_sensitive (ddc->date, FALSE);
    post_date_changed_cb (ddc->post_date, ddc);
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
  ddc->acct_combo = glade_xml_get_widget (xml, "acct_combo");
  hbox = glade_xml_get_widget (xml, "the_hbox");

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
