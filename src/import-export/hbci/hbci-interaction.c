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

#include <gwenhywfar/bio_buffer.h>
#include <gwenhywfar/xml.h>

#include <iconv.h>

#define PREF_TAB_ONLINE_BANKING N_("Online Banking & Importing")

gchar *gnc__extractText(const char *text);

/** Adds the interactor and progressmonitor classes to the api. */
GNCInteractor *gnc_AB_BANKING_interactors (AB_BANKING *api, GtkWidget *parent)
{
  GNCInteractor *data;
  
  data = g_new0 (GNCInteractor, 1);
  data->parent = parent;
  data->gnc_iconv_handler = iconv_open("ISO8859-1", "UTF-8");
  g_assert(data->gnc_iconv_handler != (iconv_t)(-1));
  data->keepAlive = TRUE;
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
  iconv_close(data->gnc_iconv_handler);

  g_free (data);
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
    /* AB_Banking_SetEnablePinCaching (ab, cache_pin); */
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

gboolean GNCInteractor_get_cache_valid(const GNCInteractor *i)
{
  g_assert(i);
  return i->cache_pin;
}
void GNCInteractor_set_cache_valid(GNCInteractor *i, gboolean value)
{
  g_assert(i);
  /* Nothing to be done right now. */
}

void GNCInteractor_erasePIN(GNCInteractor *i)
{
  g_assert(i);
  /* Nothing to be done right now. */
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

gboolean GNCInteractor_hadErrors (const GNCInteractor *i)
{
  g_assert (i);
  return i->msgBoxError != 0;
}

/* ************************************************************ 
 */

/* This function extracts the normal text part out of the
   combi-strings that are passed from aqbanking. */
gchar *gnc__extractText(const char *text) 
{
  gchar *res;
  GWEN_BUFFEREDIO *bio;
  GWEN_XMLNODE *xmlNode;
  GWEN_BUFFER *buf;
  int rv;

  if (!text)
    text = "";

  buf=GWEN_Buffer_new(0, 256, 0, 1);
  GWEN_Buffer_AppendString(buf, text);
  GWEN_Buffer_Rewind(buf);

  /* check whether there is a html tag */
  bio=GWEN_BufferedIO_Buffer2_new(buf, 1);
  GWEN_BufferedIO_SetReadBuffer(bio, 0, 256);
  xmlNode=GWEN_XMLNode_new(GWEN_XMLNodeTypeTag, "html");
  rv=GWEN_XML_Parse(xmlNode, bio,
		    GWEN_XML_FLAGS_DEFAULT |
		    GWEN_XML_FLAGS_HANDLE_OPEN_HTMLTAGS |
		    GWEN_XML_FLAGS_NO_CONDENSE |
		    GWEN_XML_FLAGS_KEEP_CNTRL);
  GWEN_BufferedIO_Close(bio);
  GWEN_BufferedIO_free(bio);

  if (rv) {
    res = g_strdup(text);
  }
  else {
    GWEN_XMLNODE *nn;

    nn=GWEN_XMLNode_GetFirstData(xmlNode);
    if (nn) {
      res = g_strdup(GWEN_XMLNode_GetData(nn));
    }
    else {
      res = g_strdup(text);
    }
  }
  GWEN_XMLNode_free(xmlNode);
  return res;
}


char *gnc_hbci_utf8ToLatin1(GNCInteractor *data, const char *utf)
{
  int inbytes, outbytes;
  char *utf8extracted, *latin1;
  char *inbuffer, *outbuffer;

  g_assert(data);
  if (!utf) return g_strdup("");

  /* Get rid of the aaaarg html-combi-text part */
  utf8extracted = gnc__extractText(utf);
/*   printf("Extracted \"%s\" into \"%s\"\n", utf, utf8extracted); */

  inbuffer = utf8extracted;
  inbytes = strlen(inbuffer);
  outbytes = inbytes + 2;
  latin1 = g_strndup(inbuffer, outbytes);
  outbuffer = latin1;

  iconv(data->gnc_iconv_handler, &inbuffer, &inbytes,
	&outbuffer, &outbytes);
  if (outbytes > 0)
    *outbuffer = '\0';

/*   printf("Converted \"%s\" into \"%s\"\n", utf8extracted, latin1); */
  g_free(utf8extracted);
  return latin1;
}

/********************************************************
 * Now all the callback functions 
 */

static int inputBoxCB(AB_BANKING *ab,
		      GWEN_TYPE_UINT32 flags,
		      const char *utf8title,
		      const char *utf8text,
		      char *resultbuffer,
		      int minsize,
		      int maxLen)
{
  GNCInteractor *data;
  char *passwd = NULL;
  int retval = 0;
  int newPin;
  int hideInput;
  gchar *title, *text;

  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);
  g_assert(maxLen > minsize);

  text = gnc_hbci_utf8ToLatin1(data, utf8text);
  title = gnc_hbci_utf8ToLatin1(data, utf8title);

  newPin = (flags | AB_BANKING_INPUT_FLAGS_CONFIRM) == 0;
  hideInput = (flags | AB_BANKING_INPUT_FLAGS_SHOW) != 0;

  while (TRUE) {

    if (newPin) {
      if (!hideInput)
	printf("inputBoxCB: Oops, hideInput==false and newPin==true, i.e. the input is supposed to be readable -- not implemented (since I thought this does not make sense when entering a new PIN).\n");
      retval = gnc_hbci_get_initial_password (data->parent,
					      title,
					      text,
					      &passwd);
    }
    else {
      retval = gnc_hbci_get_password (data->parent,
				      title,
				      text,
				      NULL,
				      &passwd,
				      hideInput);
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
      g_free (memset (passwd, 0, strlen (passwd)));
      g_free(title);
      g_free(text);
      return 0;
    }
  }
  
  /* User wanted to abort. */
  g_free(title);
  g_free(text);
  return 1;
}

/* **************************************** 
 */


static int getTanCB(AB_BANKING *ab,
		    const char *token,
		    const char *utf8title,
		    const char *utf8text,
		    char *resultbuffer,
		    int minsize,
		    int maxLen)
{
  GNCInteractor *data;
  char *passwd = NULL;
  int retval = 0;
  gchar *title, *text;

  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);
  g_assert(maxLen > minsize);

  text = gnc_hbci_utf8ToLatin1(data, utf8text);
  title = gnc_hbci_utf8ToLatin1(data, utf8title);

  while (TRUE) {

    retval = gnc_hbci_get_password (data->parent,
				    title,
				    text,
				    NULL,
				    &passwd,
				    FALSE);

    if (!retval)
      break;
    
    if (strlen(passwd) < (unsigned int)minsize) {
      gboolean retval;
      char *msg = 
	g_strdup_printf (  _("This TAN needs to be at least %d characters \n"
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

      g_free (memset (passwd, 0, strlen (passwd)));
      g_free(title);
      g_free(text);
      return 0;
    }
  }
  
  /* User wanted to abort. */
  g_free(title);
  g_free(text);
  return 1;
}


/* ************************************************************ 
 */

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

  GNCInteractor_delete (data);
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
	  const char *utf8title, const char *utf8text)
{
  GtkWidget *dialog;
  GNCInteractor *data;
  GWEN_TYPE_UINT32 result;
  gchar *title, *text;

  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);
  
  text = gnc_hbci_utf8ToLatin1(data, utf8text);
  title = gnc_hbci_utf8ToLatin1(data, utf8title);

  /* Create new dialog */
  dialog = gnome_ok_dialog_parented (text, GTK_WINDOW (data->parent));
  if (title && (strlen(title) > 0))
    gtk_window_set_title (GTK_WINDOW (dialog), title);
  gnome_dialog_close_hides (GNOME_DIALOG(dialog), TRUE);
  gtk_widget_show_all (dialog);

  result = data->showbox_id;
  g_hash_table_insert(data->showbox_hash, (gpointer)result, dialog);
  data->showbox_id++;
  data->showbox_last = dialog;

  g_free(title);
  g_free(text);
  return result;
}

/* ************************************************************ 
 */

static int messageBoxCB(AB_BANKING *ab, GWEN_TYPE_UINT32 flags, 
			const char *utf8title, const char *utf8text, 
			const char *b1, const char *b2, const char *b3)
{
  GNCInteractor *data;
  GtkWidget *dialog, *label;
  int result;
  gchar *text, *title, *b1text, *b2text, *b3text;

  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);
  data->msgBoxError = flags & AB_BANKING_MSG_FLAGS_TYPE_ERROR;

  text = gnc_hbci_utf8ToLatin1(data, utf8text);
  title = gnc_hbci_utf8ToLatin1(data, utf8title);
  b1text = gnc_hbci_utf8ToLatin1(data, b1);
  b2text = gnc_hbci_utf8ToLatin1(data, b2);
  b3text = gnc_hbci_utf8ToLatin1(data, b3);

  dialog = gnome_dialog_new (title, 
			     b1 ? b1text : NULL,
			     b2 ? b2text : NULL,
			     b3 ? b3text : NULL,
			     NULL);
  gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (data->parent));
  gnome_dialog_set_close (GNOME_DIALOG (dialog), TRUE);
  label = gtk_label_new (text);
  gtk_label_set_justify (GTK_LABEL (label), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start (GTK_BOX (GNOME_DIALOG (dialog)->vbox), label, TRUE, TRUE, 0);
  gtk_widget_show (label);

  result = gnome_dialog_run_and_close (GNOME_DIALOG (dialog));
  if (result<0 || result>2) {
    printf("messageBoxCB: Bad result %d", result);
    g_free(title);
    g_free(text);
    g_free(b1text);
    g_free(b2text);
    g_free(b3text);
    return 0;
  }
  g_free(title);
  g_free(text);
  g_free(b1text);
  g_free(b2text);
  g_free(b3text);
  return result+1;
}


/* ************************************************************ 
 */

#define progress_id 4711

static GWEN_TYPE_UINT32 progressStartCB(AB_BANKING *ab, const char *utf8title, 
					const char *utf8text, GWEN_TYPE_UINT32 total)
{
  GNCInteractor *data;
  gchar *title, *text;

  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);
  
  text = gnc_hbci_utf8ToLatin1(data, utf8text);
  title = gnc_hbci_utf8ToLatin1(data, utf8title);

  /* Now set the text etc */
  gtk_entry_set_text (GTK_ENTRY (data->job_entry), title);
  gtk_entry_set_text (GTK_ENTRY (data->action_entry), text);

  /*   printf("progressLogCB: Logging msg: %s\n", text); */
  /*   GNCInteractor_add_log_text (data, text); */

  /* Set progress bar */
  gtk_progress_set_percentage (GTK_PROGRESS (data->action_progress), 
			       0.0);
  data->action_max = total;
  GNCInteractor_setRunning(data);
  /* printf("progressStartCB: Action \"%s\" started, total %d.\n",
     text, total); */

  /* Show the dialog */
  GNCInteractor_show(data);

  g_free(title);
  g_free(text);
  return progress_id;
}

static int progressAdvanceCB(AB_BANKING *ab, GWEN_TYPE_UINT32 id, 
			     GWEN_TYPE_UINT32 progress)
{
  GNCInteractor *data;

  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);

  if ((id != 0) && (id != progress_id)) {
/*     printf("progressLogCB: Oops, wrong progress id %d -- ignored.\n", id); */
  }

  if (progress != AB_BANKING_PROGRESS_NONE) {
    /* printf("progressLogCB: Progress set to %d out of %f.\n", 
       progress, data->action_max); */
    if (progress <= data->action_max) 
      gtk_progress_set_percentage (GTK_PROGRESS (data->action_progress), 
				   progress/data->action_max);
  }

  keepAlive(data);
  return 0;
}


static int progressLogCB(AB_BANKING *ab, GWEN_TYPE_UINT32 id, 
			 AB_BANKING_LOGLEVEL level, const char *utf8text)
{
  GNCInteractor *data;
  gchar *text;

  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);

  text = gnc_hbci_utf8ToLatin1(data, utf8text);

  if ((id != 0) && (id != progress_id)) {
/*     printf("progressLogCB: Oops, wrong progress id %d -- ignored.\n", id); */
  }

  /* printf("progressLogCB: Logging msg: %s\n", text); */
  GNCInteractor_add_log_text (data, text);

  g_free(text);
  keepAlive(data);
  return 0;
}

static int progressEndCB(AB_BANKING *ab, GWEN_TYPE_UINT32 id)
{
  GNCInteractor *data;
  g_assert(ab);
  data = AB_Banking_GetUserData(ab);
  g_assert(data);

  if ((id != 0) && (id != progress_id)) {
/*     printf("progressLogCB: Oops, wrong progress id %d -- ignored.\n", id); */
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

  /* AB_Banking_SetGetPinFn(ab,); */
  AB_Banking_SetGetTanFn(ab, getTanCB);

  AB_Banking_SetUserData(ab, data);

}
