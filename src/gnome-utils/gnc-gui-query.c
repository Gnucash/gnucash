/********************************************************************\
 * gnc-gui-query.c -- functions for creating dialogs for GnuCash    * 
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
 
#include "gnc-engine-util.h"
#include "gnc-gui-query.h"
#include "gnc-ui.h"
#include "messages.h"


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;


/********************************************************************\
 * gnc_ok_cancel_dialog_common                                      *
 *   display a message, and asks the user to press "Ok" or "Cancel" *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   parent  - the parent window                              *
 *         default - the button that will be the default            *
 *         message - the message to display                         *
 *         format - the format string for the message to display    *
 *                   This is a standard 'printf' style string.      *
 *         args - a pointer to the first argument for the format    *
 *                string.                                           *
 * Return: the result the user selected                             *
\********************************************************************/
static GNCVerifyResult
gnc_ok_cancel_dialog_common(gncUIWidget parent,
			    GNCVerifyResult default_result,
			    const gchar *format, va_list args)
{
  GtkWidget *dialog = NULL;
  gint default_button;
  gint result;
  gchar *buffer;

  buffer = g_strdup_vprintf(format, args);
  dialog = gnome_message_box_new(format,
                                 GNOME_MESSAGE_BOX_QUESTION,
                                 GNOME_STOCK_BUTTON_OK,
                                 GNOME_STOCK_BUTTON_CANCEL,
                                 NULL);
  g_free(buffer);

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

GNCVerifyResult
gnc_ok_cancel_dialog(GNCVerifyResult default_result, const gchar *format, ...)
{
  gboolean result;
  va_list args;

  va_start(args, format);
  result = gnc_ok_cancel_dialog_common(gnc_ui_get_toplevel(),
				       default_result, format, args);
  va_end(args);
  return(result);
}

GNCVerifyResult
gnc_ok_cancel_dialog_parented(gncUIWidget parent,
			      GNCVerifyResult default_result,
			      const gchar *format, ...)
{
  gboolean result;
  va_list args;

  va_start(args, format);
  result = gnc_ok_cancel_dialog_common(parent ? parent : gnc_ui_get_toplevel(),
				       default_result, format, args);
  va_end(args);
  return(result);
}



/********************************************************************\
 * gnc_verify_cancel_dialog_common                                  *
 *   display a message, and asks the user to press "Yes", "No", or  *
 *   "Cancel"                                                       *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   parent  - the parent window                              *
 *         default - the button that will be the default            *
 *         format - the format string for the message to display    *
 *                   This is a standard 'printf' style string.      *
 *         args - a pointer to the first argument for the format    *
 *                string.                                           *
 * Return: the result the user selected                             *
\********************************************************************/

static GNCVerifyResult
gnc_verify_cancel_dialog_common(GtkWidget *parent,
				GNCVerifyResult default_result,
				const gchar *format, va_list args)
{
  GtkWidget *verify_box = NULL;
  gint default_button;
  gint result;
  gchar *buffer;

  buffer = g_strdup_vprintf(format, args);
  verify_box = gnome_message_box_new(format,
				     GNOME_MESSAGE_BOX_QUESTION,
				     GNOME_STOCK_BUTTON_YES,
				     GNOME_STOCK_BUTTON_NO,
                                     GNOME_STOCK_BUTTON_CANCEL,
				     NULL);
  g_free(buffer);

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

GNCVerifyResult
gnc_verify_cancel_dialog(GNCVerifyResult default_result, const gchar * format, ...)
{
  gboolean result;
  va_list args;

  va_start(args, format);
  result = gnc_verify_cancel_dialog_common(gnc_ui_get_toplevel(),
					   default_result, format, args);
  va_end(args);
  return(result);
}

GNCVerifyResult
gnc_verify_cancel_dialog_parented(GtkWidget *parent,
				  GNCVerifyResult default_result,
				  const gchar *format, ...)
{
  gboolean result;
  va_list args;

  va_start(args, format);
  result = gnc_verify_cancel_dialog_common(parent ? parent : gnc_ui_get_toplevel(),
					   default_result, format, args);
  va_end(args);
  return(result);
}



/********************************************************************\
 * gnc_verify_dialog_common                                         *
 *   display a message, and asks the user to press "Yes" or "No"    *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   parent  - the parent window                              *
 *         yes_is_default - If true, "Yes" is default,              *
 *                          "No" is the default button.             *
 *         format - the format string for the message to display    *
 *                   This is a standard 'printf' style string.      *
 *         args - a pointer to the first argument for the format    *
 *                string.                                           *
\********************************************************************/
static gboolean
gnc_verify_dialog_common(gncUIWidget parent, gboolean yes_is_default,
			 const gchar *format, va_list args)
{
  GtkWidget *verify_box = NULL;
  gchar *buffer;

  buffer = g_strdup_vprintf(format, args);
  verify_box = gnome_message_box_new(buffer,
				     GNOME_MESSAGE_BOX_QUESTION,
				     GNOME_STOCK_BUTTON_YES,
				     GNOME_STOCK_BUTTON_NO,
				     NULL);
  g_free(buffer);

  if (parent != NULL)
    gnome_dialog_set_parent(GNOME_DIALOG(verify_box), GTK_WINDOW(parent));
  
  gnome_dialog_set_default(GNOME_DIALOG(verify_box), (yes_is_default ? 0 : 1));

  return (gnome_dialog_run_and_close(GNOME_DIALOG(verify_box)) == 0);
}

gboolean
gnc_verify_dialog(gboolean yes_is_default, const gchar *format, ...)
{
  gboolean result;
  va_list args;

  va_start(args, format);
  result = gnc_verify_dialog_common(gnc_ui_get_toplevel(),
				    yes_is_default, format, args);
  va_end(args);
  return(result);
}

gboolean
gnc_verify_dialog_parented(gncUIWidget parent, gboolean yes_is_default,
			   const gchar *format, ...)
{
  gboolean result;
  va_list args;

  va_start(args, format);
  result = gnc_verify_dialog_common(parent ? parent : gnc_ui_get_toplevel(),
				    yes_is_default, format, args);
  va_end(args);

  return(result);
}



/********************************************************************\
 * gnc_info_dialog_common                                           * 
 *   displays an information dialog box                             * 
 *                                                                  * 
 * Args:   parent  - the parent window                              *  
 *         format - the format string for the message to display    *
 *                   This is a standard 'printf' style string.      *
 *         args - a pointer to the first argument for the format    *
 *                string.                                           *
 * Return: none                                                     * 
\********************************************************************/
static void 
gnc_info_dialog_common(GtkWindow *parent, const gchar *format, va_list args)
{
  GtkWidget *info_box = NULL;
  gchar *buffer;

  buffer = g_strdup_vprintf(format, args);
  info_box = gnome_ok_dialog_parented(buffer, parent);
  g_free(buffer);

  gnome_dialog_run_and_close(GNOME_DIALOG(info_box));
}

void 
gnc_info_dialog(const gchar *format, ...)
{
  va_list args;

  va_start(args, format);
  gnc_info_dialog_common(GTK_WINDOW(gnc_ui_get_toplevel()), format, args);
  va_end(args);
}

void 
gnc_info_dialog_parented(GtkWindow *parent, const gchar *format, ...)
{
  va_list args;

  va_start(args, format);
  gnc_info_dialog_common(parent ? parent : GTK_WINDOW(gnc_ui_get_toplevel()),
			 format, args);
  va_end(args);
}




/********************************************************************\
 * gnc_warning_dialog_common                                        * 
 *   displays a warning dialog box                                  * 
 *                                                                  * 
 * Args:   parent  - the parent window                              *  
 *         format - the format string for the message to display    *
 *                   This is a standard 'printf' style string.      *
 *         args - a pointer to the first argument for the format    *
 *                string.                                           *
 * Return: none                                                     * 
\********************************************************************/
static void 
gnc_warning_dialog_common(GtkWidget *parent, const gchar *format, va_list args)
{
  GtkWidget *warning_box = NULL;
  gchar *buffer;

  buffer = g_strdup_vprintf(format, args);
  warning_box = gnome_warning_dialog_parented(buffer, GTK_WINDOW(parent));
  g_free(buffer);

  gnome_dialog_run_and_close(GNOME_DIALOG(warning_box));
}

void 
gnc_warning_dialog(const gchar *format, ...)
{
  va_list args;

  va_start(args, format);
  gnc_warning_dialog_common(gnc_ui_get_toplevel(), format, args);
  va_end(args);
}

void 
gnc_warning_dialog_parented(GtkWidget *parent, const gchar *format, ...)
{
  va_list args;

  va_start(args, format);
  gnc_warning_dialog_common(parent ? parent : gnc_ui_get_toplevel(),
			    format, args);
  va_end(args);
}



/********************************************************************\
 * gnc_error_dialog_common                                          * 
 *   displays an error dialog box                                   * 
 *                                                                  * 
 * Args:   parent  - the parent window                              *
 *         format - the format string for the message to display    *
 *                   This is a standard 'printf' style string.      *
 *         args - a pointer to the first argument for the format    *
 *                string.                                           *
 * Return: none                                                     * 
\********************************************************************/
static void 
gnc_error_dialog_common(GtkWindow *parent, const gchar *format, va_list args)
{
  GtkWidget *error_box = NULL;
  gchar *buffer;

  buffer = g_strdup_vprintf(format, args);
  error_box = gnome_error_dialog_parented(buffer, parent);
  g_free(buffer);

  gnome_dialog_run_and_close(GNOME_DIALOG(error_box));
}

void 
gnc_error_dialog(const gchar *format, ...)
{
  GtkWidget *parent;
  va_list args;

  parent = gnc_ui_get_toplevel();

  va_start(args, format);
  gnc_error_dialog_common(parent ? GTK_WINDOW(parent) : NULL, format, args);
  va_end(args);
}

void 
gnc_error_dialog_parented(GtkWindow *parent, const gchar *format, ...)
{
  va_list args;

  va_start(args, format);
  gnc_error_dialog_common(parent ? parent : GTK_WINDOW(gnc_ui_get_toplevel()),
			  format, args);
  va_end(args);
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

  dialog = gnome_dialog_new (title,
                             GNOME_STOCK_BUTTON_OK,
                             GNOME_STOCK_BUTTON_CANCEL,
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
