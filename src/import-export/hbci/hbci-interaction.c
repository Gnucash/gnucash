/* hbci-interaction.c
   Copyright 2002 by Christian Stimming <stimming@tuhh.de> */

/***************************************************************************
 *                                                                         *
 *   This library is free software; you can redistribute it and/or         *
 *   modify it under the terms of the GNU Lesser General Public            *
 *   License as published by the Free Software Foundation; either          *
 *   version 2.1 of the License, or (at your option) any later version.    *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston,                 *
 *   MA  02111-1307  USA                                                   *
 *                                                                         *
 ***************************************************************************/

#include "config.h"

#include <stdio.h>
#include <string.h>
#include <locale.h>
#include "hbci-interaction.h"
#include "hbci-interactionP.h"

#include <aqbanking/banking.h>
#include "dialog-utils.h"
#include "druid-utils.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "global-options.h"

#include "dialog-pass.h"
#include "gnc-hbci-utils.h"

#define PREF_TAB_ONLINE_BANKING N_("Online Banking & Importing")



/** Adds the interactor and progressmonitor classes to the api. */
GNCInteractor *gnc_AB_BANKING_interactors (AB_BANKING *api, GtkWidget *parent)
{
  GNCInteractor *data;
  
  data = g_new0 (GNCInteractor, 1);
  data->parent = parent;
  data->keepAlive = TRUE;
  data->cache_valid = FALSE;
  data->cache_pin = 
    gnc_lookup_boolean_option(PREF_TAB_ONLINE_BANKING,
			      "HBCI Remember PIN in memory",
                              FALSE);
  data->showbox_id = 1;
  data->showbox_hash = g_hash_table_new(NULL, NULL); 

  /* set HBCI_Interactor */
  gnc_hbci_add_callbacks(api, data);
  return data;
}



/* ************************************************************
 */


GtkWidget *GNCInteractor_parent(GNCInteractor *i)
{
  g_assert(i);
  return i->parent;
}

static void GNCInteractor_setRunning (GNCInteractor *data)
{
  g_assert(data);
  data->state = RUNNING;
  gtk_widget_set_sensitive (GTK_WIDGET (data->abort_button), TRUE);
  gtk_widget_set_sensitive (GTK_WIDGET (data->close_button), FALSE);
}
static void GNCInteractor_setFinished (GNCInteractor *data)
{
  g_assert(data);
  data->state = FINISHED;
  gtk_widget_set_sensitive (GTK_WIDGET (data->abort_button), FALSE);
  gtk_widget_set_sensitive (GTK_WIDGET (data->close_button), TRUE);
  if (gtk_toggle_button_get_active
      (GTK_TOGGLE_BUTTON (data->close_checkbutton)))
    GNCInteractor_hide (data);
}
static void GNCInteractor_setAborted (GNCInteractor *data)
{
  g_assert(data);
  data->state = ABORTED;
  gtk_widget_set_sensitive (GTK_WIDGET (data->abort_button), FALSE);
  gtk_widget_set_sensitive (GTK_WIDGET (data->close_button), TRUE);
  data->keepAlive = FALSE;
}


gboolean GNCInteractor_aborted(const GNCInteractor *i)
{
  g_assert(i);
  return !(i->keepAlive);
}

void GNCInteractor_show_nodelete(GNCInteractor *i)
{
  gboolean cache_pin = 
    gnc_lookup_boolean_option(PREF_TAB_ONLINE_BANKING,
			      "HBCI Remember PIN in memory",
			      FALSE);
  g_assert(i);
  /* Show widgets */
  gtk_widget_show_all (i->dialog);

  /* Make sure the cache_pin option is up to date. */
  if (cache_pin != i->cache_pin) {
    i->cache_pin = cache_pin;
    if (cache_pin == FALSE)
      GNCInteractor_erasePIN (i);
  }
}
void GNCInteractor_show(GNCInteractor *i)
{
  g_assert(i);
  GNCInteractor_show_nodelete(i);
  /* Clear log window. */
  gtk_editable_delete_text (GTK_EDITABLE (i->log_text), 0, -1);
}


void GNCInteractor_hide(GNCInteractor *i)
{
  g_assert(i);
  if (gtk_toggle_button_get_active 
      (GTK_TOGGLE_BUTTON (i->close_checkbutton)))
    gtk_widget_hide_all (i->dialog);
  gnc_set_boolean_option ("__gui", "hbci_close_on_finish",
			  gtk_toggle_button_get_active 
			  (GTK_TOGGLE_BUTTON (i->close_checkbutton)));
}

void GNCInteractor_delete(GNCInteractor *data)
{
  if (data == NULL)
    return;
  if (data->dialog != NULL) {
    gnc_set_boolean_option ("__gui", "hbci_close_on_finish",
			    gtk_toggle_button_get_active 
			    (GTK_TOGGLE_BUTTON (data->close_checkbutton)));
    gtk_object_unref (GTK_OBJECT (data->dialog));
    gtk_widget_destroy (data->dialog);
  }
  
  data->dialog = NULL;

  g_hash_table_destroy(data->showbox_hash);
}

void GNCInteractor_set_cache_valid(GNCInteractor *i, gboolean value)
{
  g_assert(i);
  i->cache_valid = value;
}

void GNCInteractor_erasePIN(GNCInteractor *i)
{
  g_assert(i);
  if (i->pw != NULL)
    g_free (memset (i->pw, 0, strlen (i->pw)));
  i->pw = NULL;
  i->cache_valid = FALSE;
  /* i->user = NULL; */
}
void GNCInteractor_reparent (GNCInteractor *i, GtkWidget *new_parent)
{
  g_assert (i);
  if (new_parent != i->parent)
    {
      i->parent = new_parent;
      /*if (GTK_WIDGET (i->dialog) -> parent != NULL)
	gtk_widget_reparent (GTK_WIDGET (i->dialog), new_parent);
	else
	gtk_widget_set_parent (GTK_WIDGET (i->dialog), new_parent);*/
      gnome_dialog_set_parent (GNOME_DIALOG (i->dialog), 
			       GTK_WINDOW (new_parent));
    }
}

/********************************************************
 * Now all the callback functions 
 */

static int inputBoxCB(AB_BANKING *ab,
		      GWEN_TYPE_UINT32 flags,
		      const char *title,
		      const char *text,
		      char *resultbuffer,
		      int minsize,
		      int maxLen)
{
  GNCInteractor *data;
  char *msgstr = NULL, *passwd = NULL;
  int retval = 0;
  int newPin;
  int hideInput;
  GWEN_BUFFER *buffer1, *buffer2;
  int bufsize = 10+strlen(text);
  const char *latin1text;
  const char *latin1title;

  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);
  g_assert(maxLen > minsize);

  buffer1 = GWEN_Buffer_new(0, bufsize, 0, 0);
  buffer2 = GWEN_Buffer_new(0, bufsize, 0, 0);
  AB_ImExporter_Utf8ToDta (title, bufsize, buffer1);
  AB_ImExporter_Utf8ToDta (text, bufsize, buffer2);
  latin1title = GWEN_Buffer_GetStart (buffer1);
  latin1text = GWEN_Buffer_GetStart (buffer2);

  newPin = (flags | AB_BANKING_INPUT_FLAGS_CONFIRM) == 0;
  /*   printf("inputBoxCB: Requesting newPind: %s\n", newPin ? "true" : "false"); */
  hideInput = (flags | AB_BANKING_INPUT_FLAGS_SHOW) != 0;
  if (!hideInput)
    printf("inputBoxCB: Oops, hideInput is false, i.e. the input is supposed to be readable -- not implemented.\n");

  while (TRUE) {

    if (newPin) {
      msgstr = g_strdup_printf("%s\n\n%s", latin1title, latin1text);
      retval = gnc_hbci_get_initial_password (data->parent,
					      msgstr,
					      &passwd);
      g_free (msgstr);
    }
    else {
      if (data->cache_valid && text && (strcmp(text, data->cache_text)==0)) {
	/* Cached user matches, so use cached PIN. */
	/*printf("Got the cached PIN for user %s.\n", HBCI_User_userId (user));*/
	strcpy(resultbuffer, data->pw);
	return 0;
      }
      else {
	msgstr = g_strdup_printf("%s\n\n%s", latin1title, latin1text);
	
	retval = gnc_hbci_get_password (data->parent,
					msgstr,
					NULL,
					&passwd);
	g_free (msgstr);
      } /* user == data->user */
    } /* newPin */
    
    if (!retval)
      break;
    
    if (strlen(passwd) < (unsigned int)minsize) {
      gboolean retval;
      char *msg = 
	g_strdup_printf (  _("The PIN needs to be at least %d characters \n"
			     "long. Do you want to try again?"),
			   minsize);
      retval = gnc_verify_dialog (GTK_WIDGET (data->parent), 
					   TRUE,
					   msg);
      g_free (msg);
      if (!retval)
	break;
    }
    else {
      g_assert (maxLen > strlen(resultbuffer));
      strcpy(resultbuffer, passwd);
      if (text && data->cache_pin) {
	/*printf("Cached the PIN for user %s.\n", HBCI_User_userId (user));*/
	data->cache_text = g_strdup(text);
	if (data->pw)
	  g_free (memset (data->pw, 0, strlen (data->pw)));
	data->pw = passwd;
      }
      else 
	g_free (memset (passwd, 0, strlen (passwd)));
      GWEN_Buffer_free (buffer1);
      GWEN_Buffer_free (buffer2);
      return 0;
    }
  }
  
  /* User wanted to abort. */
  GWEN_Buffer_free (buffer1);
  GWEN_Buffer_free (buffer2);
  return 1;
}

static int keepAlive(void *user_data)
{
  GNCInteractor *data = user_data;
  g_assert(data);
  /*fprintf(stdout, "my-keepAlive: returning 1\n");*/

  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));

  return data->keepAlive;
}


static void destr(void *bp, void *user_data)
{
  GNCInteractor *data = user_data;
  if (data == NULL)
    return;

  if (data->pw) {
    memset (data->pw, 0, strlen(data->pw));
    g_free (data->pw);
  }
}

/* ************************************************************ 
 */

static void 
hideBoxCB(AB_BANKING *ab, GWEN_TYPE_UINT32 id)
{
  GNCInteractor *data;
  GtkWidget *dialog;
  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);

  if (id > 0) {
    dialog = g_hash_table_lookup(data->showbox_hash, (gpointer)id);
  } else {
    dialog = data->showbox_last;
  }
  if (dialog) {
    gnome_dialog_close (GNOME_DIALOG (dialog));
    gtk_widget_destroy (dialog);
    g_hash_table_remove(data->showbox_hash, (gpointer)id);
  }
}

static GWEN_TYPE_UINT32
showBoxCB(AB_BANKING *ab, GWEN_TYPE_UINT32 flags,
	  const char *title, const char *text)
{
  char *msgstr;
  GtkWidget *dialog;
  GNCInteractor *data;
  GWEN_TYPE_UINT32 result;
  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);
  
  /* Create message string */
  msgstr = g_strdup_printf ("%s\n%s", title, text);

  /* Create new dialog */
  dialog = gnome_ok_dialog_parented (msgstr, GTK_WINDOW (data->parent));
  gnome_dialog_close_hides (GNOME_DIALOG(dialog), TRUE);
  gtk_widget_show_all (dialog);

  result = data->showbox_id;
  g_hash_table_insert(data->showbox_hash, (gpointer)result, dialog);
  data->showbox_id++;
  data->showbox_last = dialog;

  g_free (msgstr);
  return result;
}

/* ************************************************************ 
 */

static int messageBoxCB(AB_BANKING *ab, GWEN_TYPE_UINT32 flags, 
			const char *title, const char *text, 
			const char *b1, const char *b2, const char *b3)
{
  GNCInteractor *data;
  GtkWidget *dialog, *label;
  int result;

  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);

  dialog = gnome_dialog_new (title, b1, b2, b3, NULL);
  gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (data->parent));
  gnome_dialog_set_close (GNOME_DIALOG (dialog), TRUE);
  label = gtk_label_new (text);
  gtk_box_pack_start (GTK_BOX (GNOME_DIALOG (dialog)->vbox), label, TRUE, TRUE, 0);

  result = gnome_dialog_run_and_close (GNOME_DIALOG (dialog));
  if (result<0 || result>2) {
    printf("messageBoxCB: Bad result %d", result);
    return 0;
  }
  return result+1;
}

/* ************************************************************ 
 */

#define progress_id 4711

static GWEN_TYPE_UINT32 progressStartCB(AB_BANKING *ab, const char *title, 
					const char *text, GWEN_TYPE_UINT32 total)
{
  GNCInteractor *data;
  //GtkWidget *dialog;
  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);

  gtk_entry_set_text (GTK_ENTRY (data->job_entry), title);
  gtk_entry_set_text (GTK_ENTRY (data->action_entry), text);

/*   printf("progressLogCB: Logging msg: %s\n", text); */
/*   GNCInteractor_add_log_text (data, text); */

  gtk_progress_set_percentage (GTK_PROGRESS (data->action_progress), 
			       0.0);
  data->action_max = total;
  GNCInteractor_setRunning(data);

  GNCInteractor_show(data);
  return progress_id;
}

static int progressAdvanceCB(AB_BANKING *ab, GWEN_TYPE_UINT32 id, 
			     GWEN_TYPE_UINT32 progress)
{
  GNCInteractor *data;
  //GtkWidget *dialog;
  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);

  if ((id != 0) && (id != progress_id)) {
    printf("progressLogCB: Oops, wrong progress id %d -- ignored.\n", id);
  }

  if (progress != AB_BANKING_PROGRESS_NONE) {
    printf("progressLogCB: Setting progress to %d out of %f.\n", progress, data->action_max);
    gtk_progress_set_percentage (GTK_PROGRESS (data->action_progress), 
				 progress/data->action_max);
  }

  keepAlive(data);
  return 0;
}


static int progressLogCB(AB_BANKING *ab, GWEN_TYPE_UINT32 id, 
			 AB_BANKING_LOGLEVEL level, const char *text)
{
  GNCInteractor *data;
  //GtkWidget *dialog;
  GWEN_BUFFER *buffer;
  int bufsize = 10+strlen(text);
  const char *latin1text;

  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);

  buffer = GWEN_Buffer_new(0, bufsize, 0, 0);
  AB_ImExporter_Utf8ToDta (text, bufsize, buffer);
  latin1text = GWEN_Buffer_GetStart (buffer);

  if ((id != 0) && (id != progress_id)) {
    printf("progressLogCB: Oops, wrong progress id %d -- ignored.\n", id);
  }

  printf("progressLogCB: Logging msg: %s\n", latin1text);
  GNCInteractor_add_log_text (data, latin1text);

  GWEN_Buffer_free (buffer);
  keepAlive(data);
  return 0;
}

static int progressEndCB(AB_BANKING *ab, GWEN_TYPE_UINT32 id)
{
  GNCInteractor *data;
  //GtkWidget *dialog;
  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);

  if ((id != 0) && (id != progress_id)) {
    printf("progressLogCB: Oops, wrong progress id %d -- ignored.\n", id);
  }

  GNCInteractor_setFinished(data);

  keepAlive(data);
  return 0;
}



/* ************************************************************ 
 */

int debug_pmonitor = FALSE;
void GNCInteractor_add_log_text (GNCInteractor *data, const char *msg)
{
  int pos;
  g_assert(data);
  
  pos = gtk_text_get_length (GTK_TEXT (data->log_text));
  gtk_editable_insert_text (GTK_EDITABLE (data->log_text),
			    msg, strlen (msg),
			    &pos);
  gtk_editable_insert_text (GTK_EDITABLE (data->log_text),
			    "\n", 1,
			    &pos);
}

static void
on_button_clicked (GtkButton *button,
		   gpointer user_data)
{
  GNCInteractor *data = user_data;
  char *name;
  g_assert(data);
  
  name = gtk_widget_get_name (GTK_WIDGET (button));
  if (strcmp (name, "abort_button") == 0) {
    GNCInteractor_setAborted(data);
  } else if (strcmp (name, "close_button") == 0) {
    if (data->state != RUNNING) {
      gtk_widget_hide_all (data->dialog); 
      /*data->dont_hide = FALSE;*/
      /*GNCInteractor_hide (data);*/
    }
  } else {
    printf("on_button_clicked: Oops, unknown button: %s\n",
	   name);
  }
  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));
}

GWEN_INHERIT(AB_BANKING, GNCInteractor)

/********************************************************
 * Constructor 
 */
void
gnc_hbci_add_callbacks(AB_BANKING *ab, GNCInteractor *data)
{
  GtkWidget *dialog;
  GladeXML *xml;

  /* Create the progress dialog window */
  xml = gnc_glade_xml_new ("hbci.glade", "HBCI_connection_dialog");

  g_assert (dialog = glade_xml_get_widget (xml, "HBCI_connection_dialog"));
  data->dialog = dialog;
  g_assert (data->job_entry = glade_xml_get_widget (xml, "job_entry"));
  g_assert (data->action_entry = glade_xml_get_widget (xml, "action_entry"));
  g_assert (data->action_progress = 
	    glade_xml_get_widget (xml, "action_progress"));
  g_assert (data->log_text = glade_xml_get_widget (xml, "log_text"));
  g_assert (data->abort_button = glade_xml_get_widget (xml, "abort_button"));
  gtk_widget_set_sensitive (GTK_WIDGET (data->abort_button), FALSE);
  g_assert (data->close_button = glade_xml_get_widget (xml, "close_button"));
  g_assert (data->close_checkbutton = 
	    glade_xml_get_widget (xml, "close_checkbutton"));

  /* grey out the progress bar -- its unused at the moment */
  gtk_widget_set_sensitive (data->action_progress, FALSE);
  gtk_toggle_button_set_active 
    (GTK_TOGGLE_BUTTON (data->close_checkbutton), 
     gnc_lookup_boolean_option("__gui", "hbci_close_on_finish", TRUE));

  gtk_signal_connect (GTK_OBJECT (data->abort_button), "clicked", 
		      GTK_SIGNAL_FUNC (on_button_clicked), data);
  gtk_signal_connect (GTK_OBJECT (data->close_button), "clicked", 
		      GTK_SIGNAL_FUNC (on_button_clicked), data);

  if (data->parent)
    gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (data->parent));
  /*gtk_widget_set_parent (GTK_WIDGET (dialog), data->parent);*/

  gtk_object_ref (GTK_OBJECT (dialog));
  gtk_widget_hide_all (dialog);

  GWEN_INHERIT_SETDATA(AB_BANKING, GNCInteractor,
                       ab, data,
                       &destr);

  AB_Banking_SetMessageBoxFn(ab, messageBoxCB);
  AB_Banking_SetInputBoxFn(ab, inputBoxCB);
  AB_Banking_SetShowBoxFn(ab, showBoxCB);
  AB_Banking_SetHideBoxFn(ab, hideBoxCB);
  AB_Banking_SetProgressStartFn(ab, progressStartCB);
  AB_Banking_SetProgressAdvanceFn(ab, progressAdvanceCB);
  AB_Banking_SetProgressLogFn(ab, progressLogCB);
  AB_Banking_SetProgressEndFn(ab, progressEndCB);

  AB_Banking_SetUserData(ab, data);

  /*inter = HBCI_InteractorCB_new4(&destr,
				 &msgInputPin,
				 &msgInsertMediumOrAbort,
				 &msgInsertCorrectMediumOrAbort,
				 &keepAlive,
				 &msgStartInputPinViaKeypadCB,
				 &msgEndInputPinViaKeypadCB,
				 NULL,
				 &closeConnection,
				 &actStarted,
				 &logMsg,
				 data);*/

}
