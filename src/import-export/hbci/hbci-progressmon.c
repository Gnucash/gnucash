/* hbci-progressmon.c
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

#include <stdio.h>
#include <string.h>
#include "hbci-interaction.h"
#include "hbci-interactionP.h"

#include <openhbci/interactorcb.h>
#include <openhbci/progressmonitorcb.h>
#include "dialog-utils.h"
#include "druid-utils.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"

int debug_pmonitor = FALSE;


static void close_dialog (GNCInteractor *data)
{
  if (data == NULL)
    return;
  if (data->dialog != NULL) 
    gtk_widget_destroy (data->dialog);
  data->dialog = NULL;
}

void delete_GNCInteractor (GNCInteractor *data) 
{
  if (data == NULL)
    return;

  close_dialog(data);

  g_free (data);
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
}
static void GNCInteractor_setAborted (GNCInteractor *data)
{
  g_assert(data);
  data->state = ABORTED;
  gtk_widget_set_sensitive (GTK_WIDGET (data->abort_button), FALSE);
  gtk_widget_set_sensitive (GTK_WIDGET (data->close_button), TRUE);
  data->keepAlive = FALSE;
}


/*******************************************************************
 * now the callbacks
 */
static void transStarted (TransProgressType type,
			  int jobs, void *user_data)
{
  GNCInteractor *data = user_data;
  g_assert(data);
  
  GNCInteractor_setRunning (data);

  //printf("Executing %d jobs.\n",jobs);
  data->jobs = jobs;
  data->current_job = 0;

  gtk_entry_set_text (GTK_ENTRY (data->job_entry), "");
  gtk_entry_set_text (GTK_ENTRY (data->action_entry), "");
  gtk_progress_set_percentage (GTK_PROGRESS (data->action_progress), 0.0);
  if (debug_pmonitor)
    printf("transStarted-cb: current_job %d, jobs %d, current_act %d, actions %d.\n", 
	   data->current_job, data->jobs, data->current_act, data->actions);

  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));
}
static void transFinished (void *user_data)
{
  GNCInteractor *data = user_data;
  g_assert(data);
  GNCInteractor_setFinished (data);
  gtk_entry_set_text (GTK_ENTRY (data->job_entry), _("Finished"));
  gtk_entry_set_text (GTK_ENTRY (data->action_entry), _("Finished"));
  gtk_progress_set_percentage (GTK_PROGRESS (data->action_progress), 1.0);
  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));
  if (debug_pmonitor)
    printf("transFinished-cb: current_job %d, jobs %d, current_act %d, actions %d.\n", 
	   data->current_job, data->jobs, data->current_act, data->actions);
}
static void jobStarted(JobProgressType type, int actions, void *user_data)
{
  GNCInteractor *data = user_data;
  const char *msg = NULL;
  g_assert(data);
    
  switch(type){
  case JOB_OPENINGDIALOG:
    msg = "Eröffne Dialog";
    break;
  case JOB_CLOSINGDIALOG:
    msg = "Schließe Dialog";
    break;
    /** Opening network connection. */
  case    JOB_OPENINGNETWORK:
    msg = "Beginne Netzwerkverbindung";
    break;
    /** Closing network connection. */
  case    JOB_CLOSINGNETWORK:
    msg = "Schließe Netzwerkverbindung";
    break;
    /** Get balance */
  case    JOB_GET_BALANCE:
    msg = "Job: Saldo abholen";
    break;
    /** Get transaction statement */
  case    JOB_GET_TRANS:
    msg = "Job: Umsätze abholen";
    break;
    /** Transfer money */
  case    JOB_NEW_TRANSFER:
    msg = "Job: Neue Überweisung";
    break;
    /** Debit note */
  case    JOB_DEBIT_NOTE:
    msg = "Job: Debit note";
    break;
    /** Get standing orders */
  case    JOB_GET_STO:
    msg = "";
    break;
    /** Create a new standing order */
  case    JOB_NEW_STO:
    msg = "";
    break;
    /** Delete a standing order */
  case    JOB_DELETE_STO:
    msg = "";
    break;
    /** Get account list */
  case    JOB_GET_ACCOUNTS:
    msg = "Job: Kontenliste abholen";
    break;
    /** Get SystemId */
  case    JOB_GET_SYSTEMID:
    msg = "Job: System-Kennung abgleichen";
    break;
    /** Get keys */
  case    JOB_GET_KEYS:
    msg = "Job: Schlüssel holen";
    break;
    /** Send keys */
  case    JOB_SEND_KEYS:
    msg = "Job: Schlüssel senden";
    break;
  }
  g_assert(msg);
    
  //printf("Jobstart (w/ %d actions): %s\n",actions, msg);
  data->actions = actions;
  data->current_act = 0;
  gtk_entry_set_text (GTK_ENTRY (data->job_entry), msg);
  gtk_entry_set_text (GTK_ENTRY (data->action_entry), "");
  gtk_progress_set_percentage (GTK_PROGRESS (data->action_progress), 0.0);
  if (debug_pmonitor)
    printf("jobStarted-cb: current_job %d, jobs %d, current_act %d, actions %d, msg %s.\n", 
	   data->current_job, data->jobs, data->current_act, data->actions, msg);
  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));
}
static void jobFinished (void *user_data)
{
  GNCInteractor *data = user_data;
  g_assert(data);
  data->current_job++;
  gtk_entry_set_text (GTK_ENTRY (data->job_entry), _("Done"));
  //gtk_entry_set_text (GTK_ENTRY (data->action_entry), _("Done"));
  //GNCInteractor_setFinished (data);
  //gtk_progress_set_percentage (GTK_PROGRESS (data->action_progress), 1.0);
  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));
  if (debug_pmonitor)
    printf("jobFinished-cb: current_job %d, jobs %d, current_act %d, actions %d.\n", 
	   data->current_job, data->jobs, data->current_act, data->actions);
}

static void actStarted (ActionProgressType type, void *user_data)
{
  GNCInteractor *data = user_data;
  const char *msg = NULL;
  g_assert(data);
  switch (type) {
    /** Sending message. */
  case ACT_SENDINGMESSAGE:
    msg = "Sending message";
    break;
    /** Waiting for response. */
  case ACT_WAITRESPONSE:
    msg = "Waiting for response";
    break;
    /** Creating HBCI job. Number of Job will follow in string argument. */
  case ACT_CREATEHBCIJOB:
    msg = "Creatin HBCI Job";
    break;
    /** Contacting server. Server IP address will follow in string argument. */
  case ACT_CONTACTINGSERVER:
    msg = "Contacting server";
    break;
    /** Checking Job result. */
  case ACT_CHKRESULT:
    msg = "Checking Job result";
    break;
    /** Updating local system. */
  case ACT_UPDATESYSTEM:
    msg = "Updating local system";
    break;
    /** Closing connection. */
  case ACT_CLOSECONNECTION:
    msg = "Closing connection";
    break;
  }
  
  g_assert(msg);
  gtk_entry_set_text (GTK_ENTRY (data->action_entry), msg);
  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));
  if (debug_pmonitor)
    printf("actStarted-cb: current_job %d, jobs %d, current_act %d, actions %d, msg %s.\n", 
	   data->current_job, data->jobs, data->current_act, data->actions, msg);
}
static void actFinished (void *user_data)
{
  GNCInteractor *data = user_data;
  g_assert(data);
  data->current_act++;
  gtk_entry_set_text (GTK_ENTRY (data->action_entry), _("Done"));
  gtk_progress_set_percentage (GTK_PROGRESS (data->action_progress), 
			       (data->current_act < data->actions) ?
			       ((float) data->current_act / 
				(float) data->actions) : 
			       1.0);
  if (debug_pmonitor)
    printf("actFinished-cb: current_job %d, jobs %d, current_act %d, actions %d.\n", 
	   data->current_job, data->jobs, data->current_act, data->actions);
  if (data->current_act > data->actions) {
    printf("actFinished-cb: oops, current_act==%d is > than actions==%d.\n",
	   data->current_act, data->actions);
  }
  
  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));
}
static void logMsg (const char *msg, void *user_data)
{
  /* Note: this isn't used anyway. */
  GNCInteractor *data = user_data;
  g_assert(data);
  
  printf("logMsg: Logging msg: %s\n", msg);
  add_log_text (data, msg);
			    
  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));
}

void add_log_text (GNCInteractor *data, const char *msg)
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

static void destr(void *user_data) 
{
  GNCInteractor *data = user_data;

  delete_GNCInteractor (data);
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
    if (data->state != RUNNING)
      close_dialog (data);
  } else {
    printf("on_button_clicked: Oops, unknown button: %s\n",
	   name);
  }
  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));
}

HBCI_ProgressMonitor *
gnc_hbci_new_pmonitor(GNCInteractor *data)
{
  HBCI_ProgressMonitorCB *pmon;
  GtkWidget *dialog;
  GladeXML *xml;

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

  gtk_signal_connect (GTK_OBJECT (data->abort_button), "clicked", 
		      GTK_SIGNAL_FUNC (on_button_clicked), data);
  gtk_signal_connect (GTK_OBJECT (data->close_button), "clicked", 
		      GTK_SIGNAL_FUNC (on_button_clicked), data);

  if (data->parent)
    gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (data->parent));
  //gtk_widget_set_parent (GTK_WIDGET (dialog), data->parent);

  gtk_widget_hide_all (dialog);

  pmon = HBCI_ProgressMonitorCB_new(&destr,
				    &transStarted, &transFinished,
				    &jobStarted, &jobFinished, 
				    &actStarted, &actFinished, 
				    &logMsg,
				    data);

  return HBCI_ProgressMonitorCB_ProgressMonitor(pmon);
}
