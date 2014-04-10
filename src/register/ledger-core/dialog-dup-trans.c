/********************************************************************\
 * dialog-dup-trans.c -- duplicate transaction dialog               *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
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

#include "config.h"

#include <gnome.h>
#include <time.h>

#include "dialog-utils.h"
#include "gnc-date-edit.h"
#include "gnc-engine-util.h"
#include "gnc-ui.h"
#include "messages.h"


/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

typedef struct
{
  GtkWidget * dialog;

  gboolean focus_out;

  GtkWidget * date_edit;
  GtkWidget * num_edit;
} DupTransDialog;


/* Parses the string value and returns true if it is a
 * number. In that case, *num is set to the value parsed.
 * Copied from numcell.c */
static gboolean
parse_num (const char *string, long int *num)
{
  long int number;

  if (string == NULL)
    return FALSE;

  if (!gnc_strisnum(string))
    return FALSE;

  number = strtol(string, NULL, 10);

  if ((number == LONG_MIN) || (number == LONG_MAX))
    return FALSE;

  if (num != NULL)
    *num = number;

  return TRUE;
}

/*
 * This code works around an annoying bug in the SpinButton -- as soon
 * as you focus into the spin button it wants to force a digit to
 * appear and there is nothing the user can do.  Once you focus in,
 * the user cannot make the spin button entry be empty.
 *
 * To make matters worse, the spin button draws this number AFTER a
 * focus-out event, so we can't just use that event to clear it out.
 *
 * To work around this problem we hook into two signals, focus-out and
 * draw.  The focus-out event lets us know when we leave the
 * spinbutton entry, and we set a flag to remember this fact, and also
 * queue a redraw (just to be sure).  The draw event happens more
 * frequently, but also happens after the spin button digitizes
 * itself.  So when we hit a draw event we can check the flag and if
 * it's set we can potentially empty out the entry.
 *
 * This also means you cannot have a check numbered "0", but that is
 * probably a reasonable limitation.
 */
static void
gnc_dup_trans_focus_out_cb (GtkSpinButton *button, GdkEventFocus *event,
			    gpointer user_data)
{
  DupTransDialog *dt_dialog = user_data;

  g_return_if_fail(GTK_IS_SPIN_BUTTON(button));
  if (!dt_dialog) return;

  dt_dialog->focus_out = TRUE;
  gtk_widget_queue_draw(GTK_WIDGET(button));
}

static void
gnc_dup_trans_draw_cb (GtkSpinButton *button, GdkRectangle *unused, gpointer data)
{
  DupTransDialog *dt_dialog = data;

  g_return_if_fail(GTK_IS_SPIN_BUTTON(button));
  if (!dt_dialog) return;

  if (!dt_dialog->focus_out)
    return;

  dt_dialog->focus_out = FALSE;

  if (!gtk_spin_button_get_value_as_int(button))
    gtk_entry_set_text(GTK_ENTRY(button), "");
}

static void
gnc_dup_trans_dialog_create (GtkWidget * parent, DupTransDialog *dt_dialog,
                             time_t date, const char *num_str)
{
  GtkWidget *dialog;
  GladeXML  *xml;

  xml = gnc_glade_xml_new ("register.glade", "Duplicate Transaction Dialog");

  dialog = glade_xml_get_widget (xml, "Duplicate Transaction Dialog");
  dt_dialog->dialog = dialog;

  /* parent */
  if (parent != NULL)
    gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (parent));

  /* default to ok */
  gnome_dialog_set_default (GNOME_DIALOG(dialog), 0);

  /* date widget */
  {
    GtkWidget *date_edit;
    GtkWidget *hbox;

    date_edit = gnc_date_edit_new (date, FALSE, FALSE);
    hbox = glade_xml_get_widget (xml, "date_hbox");
    gtk_widget_show (date_edit);

    gtk_box_pack_end (GTK_BOX (hbox), date_edit, TRUE, TRUE, 0);
    dt_dialog->date_edit = date_edit;
  }

  {
    GtkWidget *num_spin;
    long int num;

    num_spin = glade_xml_get_widget (xml, "num_spin");
    dt_dialog->num_edit = num_spin;

    gnome_dialog_editable_enters (GNOME_DIALOG (dialog),
                                  GTK_EDITABLE (num_spin));

    gtk_signal_connect (GTK_OBJECT(num_spin), "focus-out-event",
			GTK_SIGNAL_FUNC(gnc_dup_trans_focus_out_cb), dt_dialog);
    gtk_signal_connect (GTK_OBJECT(num_spin), "draw",
			GTK_SIGNAL_FUNC(gnc_dup_trans_draw_cb), dt_dialog);

    if (num_str && parse_num (num_str, &num))
      gtk_spin_button_set_value (GTK_SPIN_BUTTON (num_spin), num + 1);
    else
      gtk_entry_set_text (GTK_ENTRY (num_spin), "");
  }
}

/********************************************************************\
 * gnc_dup_trans_dialog                                             *
 *   opens up a window to do an automatic transfer between accounts *
 *                                                                  * 
 * Args:   parent  - the parent of the window to be created         *
 *         date    - the initial date to use, and the output        *
 *                   parameter for the new date                     *
 *         num     - input num field                                *
 *         out_num - output num field, g_newed string               *
 * Return: TRUE if user closes dialog with 'OK'                     *
\********************************************************************/
gboolean
gnc_dup_trans_dialog (GtkWidget * parent, time_t *date_p,
                      const char *num, char **out_num)
{
  DupTransDialog *dt_dialog;
  GNCDateEdit *gde;
  GtkWidget *entry;
  gboolean ok;
  gint result;

  if (!date_p || !out_num)
    return FALSE;

  dt_dialog = g_new0 (DupTransDialog, 1);

  gnc_dup_trans_dialog_create (parent, dt_dialog, *date_p, num);

  gde = GNC_DATE_EDIT (dt_dialog->date_edit);
  entry = gde->date_entry;

  gtk_widget_grab_focus (entry);

  result = gnome_dialog_run_and_close (GNOME_DIALOG (dt_dialog->dialog));

  if (result == 0)
  {
    *date_p = gnc_date_edit_get_date (GNC_DATE_EDIT (dt_dialog->date_edit));
    *out_num = g_strdup (gtk_entry_get_text (GTK_ENTRY (dt_dialog->num_edit)));
    ok = TRUE;
  }
  else
    ok = FALSE;

  g_free (dt_dialog);

  return ok;
}
