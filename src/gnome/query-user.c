/********************************************************************\
 * query-user.c -- functions for creating dialogs for GnuCash       * 
 * Copyright (C) 1998, 1999, 2000 Linas Vepstas                     *
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
 
#include "gnc-ui.h"
#include "messages.h"
#include "query-user.h"
#include "gnc-engine-util.h"


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;


/********************************************************************\
 * gnc_ok_cancel_dialog_parented                                    *
 *   display a message, and asks the user to press "Ok" or "Cancel" *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   parent  - the parent window                              *
 *         message - the message to display                         *
 *         default - the button that will be the default            *
 * Return: the result the user selected                             *
\********************************************************************/
GNCVerifyResult
gnc_ok_cancel_dialog_parented(gncUIWidget parent, const char *message,
                              GNCVerifyResult default_result)
{
  GtkWidget *dialog = NULL;
  gint default_button;
  gint result;

  dialog = gnome_message_box_new(message,
                                 GNOME_MESSAGE_BOX_QUESTION,
                                 GNOME_STOCK_BUTTON_OK,
                                 GNOME_STOCK_BUTTON_CANCEL,
                                 NULL);

  switch (default_result)
  {
    case GNC_VERIFY_OK:
      default_button = 0;
      break;
    case GNC_VERIFY_CANCEL:
      default_button = 1;
      break;
    default:
      PWARN("bad default button\n");
      default_button = 0;
      break;
  }

  gnome_dialog_set_default(GNOME_DIALOG(dialog), default_button);
  if (parent != NULL)
    gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(parent));

  result = gnome_dialog_run_and_close(GNOME_DIALOG(dialog));

  switch (result)
  {
    case 0:
      return GNC_VERIFY_OK;
    case 1:
    default:
      return GNC_VERIFY_CANCEL;
  }
}

/********************************************************************\
 * gnc_verify_cancel_dialog                                         *
 *   display a message, and asks the user to press "Yes", "No", or  *
 *   "Cancel"                                                       *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   message - the message to display                         *
 *         default - the button that will be the default            *
 * Return: the result the user selected                             *
\********************************************************************/

GNCVerifyResult
gnc_verify_cancel_dialog(const char * message, GNCVerifyResult default_res) 
{
  return gnc_verify_cancel_dialog_parented(gnc_ui_get_toplevel(), message, 
                                           default_res);
}


GNCVerifyResult
gnc_verify_cancel_dialog_parented(GtkWidget *parent, const char *message,
                                  GNCVerifyResult default_result)
{
  GtkWidget *verify_box = NULL;
  gint default_button;
  gint result;
  
  verify_box = gnome_message_box_new(message,
				     GNOME_MESSAGE_BOX_QUESTION,
				     GNOME_STOCK_BUTTON_YES,
				     GNOME_STOCK_BUTTON_NO,
                                     GNOME_STOCK_BUTTON_CANCEL,
				     NULL);

  switch (default_result)
  {
    case GNC_VERIFY_YES:
      default_button = 0;
      break;
    case GNC_VERIFY_NO:
      default_button = 1;
      break;
    case GNC_VERIFY_CANCEL:
      default_button = 2;
      break;
    default:
      PWARN("bad default button\n");
      default_button = 0;
      break;
  }

  gnome_dialog_set_default(GNOME_DIALOG(verify_box), default_button);
  if (parent != NULL)
    gnome_dialog_set_parent(GNOME_DIALOG(verify_box), GTK_WINDOW(parent));

  result = gnome_dialog_run_and_close(GNOME_DIALOG(verify_box));

  switch (result)
  {
    case 0:
      return GNC_VERIFY_YES;
    case 1:
      return GNC_VERIFY_NO;
    case 2:
    default:
      return GNC_VERIFY_CANCEL;
  }
}

/********************************************************************\
 * gnc_verify_dialog                                                *
 *   display a message, and asks the user to press "Yes" or "No"    *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   message - the message to display                         *
 *         yes_is_default - If true, "Yes" is default,              *
 *                          "No" is the default button.             *
 * Return: true for "Yes", false for "No"                           *
\********************************************************************/
gboolean
gnc_verify_dialog(const char *message, gboolean yes_is_default)
{
  return gnc_verify_dialog_parented(gnc_ui_get_toplevel(),
                                    message, yes_is_default);
}

/********************************************************************\
 * gnc_verify_dialog_parented                                       *
 *   display a message, and asks the user to press "Yes" or "No"    *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   parent  - the parent window                              *
 *         message - the message to display                         *
 *         yes_is_default - If true, "Yes" is default,              *
 *                          "No" is the default button.             *
 * Return: true for "Yes", false for "No"                           *
\********************************************************************/
gboolean
gnc_verify_dialog_parented(gncUIWidget parent, const char *message,
                           gboolean yes_is_default)
{
  GtkWidget *verify_box = NULL;

  verify_box = gnome_message_box_new(message,
				     GNOME_MESSAGE_BOX_QUESTION,
				     GNOME_STOCK_BUTTON_YES,
				     GNOME_STOCK_BUTTON_NO,
				     NULL);

  if (parent == NULL)
    parent = gnc_ui_get_toplevel ();

  if (parent != NULL)
    gnome_dialog_set_parent(GNOME_DIALOG(verify_box), GTK_WINDOW(parent));
  
  gnome_dialog_set_default(GNOME_DIALOG(verify_box), yes_is_default ? 0 : 1);

  return (gnome_dialog_run_and_close(GNOME_DIALOG(verify_box)) == 0);
}

/********************************************************************\
 * gnc_info_dialog                                                  * 
 *   displays an information dialog box                             * 
 *                                                                  * 
 * Args:   message - the information message to display             * 
 * Return: none                                                     * 
\********************************************************************/
void 
gnc_info_dialog(const char *message)
{
  GtkWidget *top = gnc_ui_get_toplevel ();

  if (top)
    gnc_info_dialog_parented (GTK_WINDOW(top), message);
  else
    gnc_info_dialog_parented (NULL, message);
}

/********************************************************************\
 * gnc_info_dialog_parented                                         * 
 *   displays an information dialog box                             * 
 *                                                                  * 
 * Args:   parent  - the parent window                              *  
 *         message - the information message to display             * 
 * Return: none                                                     * 
\********************************************************************/
void 
gnc_info_dialog_parented(GtkWindow *parent, const char *message)
{
  GtkWidget *info_box = NULL;

  info_box = gnome_ok_dialog_parented(message, parent);

  gnome_dialog_run_and_close(GNOME_DIALOG(info_box));
}

/********************************************************************\
 * gnc_warning_dialog                                               * 
 *   displays a warning dialog box                                  * 
 *                                                                  * 
 * Args:   message - the warning message to display                 * 
 * Return: none                                                     * 
\********************************************************************/
void 
gnc_warning_dialog(const char *message)
{
  gnc_warning_dialog_parented(gnc_ui_get_toplevel(), message);
}

/********************************************************************\
 * gnc_warning_dialog_parented                                      * 
 *   displays a warning dialog box                                  * 
 *                                                                  * 
 * Args:   parent  - the parent window                              *  
 *         message - the warning message to display                 * 
 * Return: none                                                     * 
\********************************************************************/
void 
gnc_warning_dialog_parented(GtkWidget *parent, const char *message)
{
  GtkWidget *warning_box = NULL;

  warning_box = gnome_warning_dialog_parented(message, GTK_WINDOW(parent));

  gnome_dialog_run_and_close(GNOME_DIALOG(warning_box));
}

/********************************************************************\
 * gnc_error_dialog                                                 * 
 *   displays an error dialog box                                   * 
 *                                                                  * 
 * Args:   message - the error message to display                   * 
 * Return: none                                                     * 
\********************************************************************/
void 
gnc_error_dialog(const char *message)
{
  GtkWidget *top = gnc_ui_get_toplevel ();

  if (top)
    gnc_error_dialog_parented(GTK_WINDOW(top), message);
  else
    gnc_error_dialog_parented(NULL, message);
}

/********************************************************************\
 * gnc_error_dialog_parented                                        * 
 *   displays an error dialog box                                   * 
 *                                                                  * 
 * Args:   parent  - the parent window                              *
 *         message - the error message to display                   * 
 * Return: none                                                     * 
\********************************************************************/
void 
gnc_error_dialog_parented(GtkWindow *parent, const char *message)
{
  GtkWidget *error_box = NULL;
  
  error_box = gnome_error_dialog_parented(message, parent);

  gnome_dialog_run_and_close(GNOME_DIALOG(error_box));
}

void 
gnc_error_dialog_parented2(gncUIWidget parent, const char *message)
{
    gnc_error_dialog_parented(GTK_WINDOW(parent), message);
}


static void
gnc_choose_radio_button_cb(GtkWidget *w, gpointer data)
{
  int *result = data;

  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w)))
    *result = GPOINTER_TO_INT(gtk_object_get_user_data(GTK_OBJECT(w)));
}

/********************************************************************
 gnc_choose_radio_option_dialog_parented

 display a group of radio_buttons and return the index of
 the selected one

*/
int
gnc_choose_radio_option_dialog_parented(gncUIWidget parent,
                                        const char *title, 
                                        const char *msg,
                                        int default_value,
                                        GList *radio_list)
{
  int radio_result = 0; /* initial selected value is first one */
  GtkWidget *vbox;
  GtkWidget *main_vbox;
  GtkWidget *label;
  GtkWidget *frame;
  GtkWidget *radio_button;
  GtkWidget *dialog;
  GtkWidget *dvbox;
  GSList *group = NULL;
  GList *node;
  int i;

  main_vbox = gtk_vbox_new(FALSE, 3);
  gtk_container_border_width(GTK_CONTAINER(main_vbox), 5);
  gtk_widget_show(main_vbox);

  label = gtk_label_new(msg);
  gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(main_vbox), label, FALSE, FALSE, 0);
  gtk_widget_show(label);

  frame = gtk_frame_new(NULL);
  gtk_container_border_width(GTK_CONTAINER(frame), 5);
  gtk_box_pack_start(GTK_BOX(main_vbox), frame, FALSE, FALSE, 0);
  gtk_widget_show(frame);

  vbox = gtk_vbox_new(TRUE, 3);
  gtk_container_border_width(GTK_CONTAINER(vbox), 5);
  gtk_container_add(GTK_CONTAINER(frame), vbox);
  gtk_widget_show(vbox);

  for (node = radio_list, i = 0; node; node = node->next, i++)
  {
    radio_button = gtk_radio_button_new_with_label(group, node->data);
    group = gtk_radio_button_group(GTK_RADIO_BUTTON(radio_button));

    if (i == default_value) /* default is first radio button */
    {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio_button), TRUE);
      radio_result = default_value;
    }

    gtk_widget_show(radio_button);
    gtk_box_pack_start(GTK_BOX(vbox), radio_button, FALSE, FALSE, 0);
    gtk_object_set_user_data(GTK_OBJECT(radio_button), GINT_TO_POINTER(i));
    gtk_signal_connect(GTK_OBJECT(radio_button), "clicked",
		       GTK_SIGNAL_FUNC(gnc_choose_radio_button_cb),
		       &radio_result);
  }

  dialog = gnome_dialog_new(title,
			    GNOME_STOCK_BUTTON_OK,
			    NULL);

  if (parent)
    gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (parent));

  /* default to ok */
  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);

  /* destroy, don't hide */
  gnome_dialog_close_hides(GNOME_DIALOG(dialog), FALSE);

  dvbox = GNOME_DIALOG(dialog)->vbox;

  gtk_box_pack_start(GTK_BOX(dvbox), main_vbox, TRUE, TRUE, 0);

  if (gnome_dialog_run_and_close(GNOME_DIALOG(dialog)) != 0)
    radio_result = -1;

  return radio_result;
}
