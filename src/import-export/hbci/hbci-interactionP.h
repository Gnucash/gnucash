/* hbci-interactorP.h -- private header for the functionality offerd by the 
   method in hbci-interaction.h.
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

#ifndef HBCI_INTERACTIONP_H
#define HBCI_INTERACTIONP_H

#include <openhbci/interactor.h>
#include <openhbci/progressmonitor.h>
#include <gnome.h>


typedef enum {
  INIT,
  RUNNING,
  FINISHED,
  ABORTED,
  CLOSING
} PMon_state;

typedef struct _data 
{
  GtkWidget *parent;

  GtkWidget *dialog;
  GtkWidget *job_entry;
  GtkWidget *action_entry;
  GtkWidget *action_progress;
  GtkWidget *log_text;

  gboolean keepAlive;
  PMon_state state;

  int jobs;
  int current_job;
  int actions;
  int current_act;

  const HBCI_User *user;
  char *pw;
} Inter_data;

void delete_Inter_data (Inter_data *data);

HBCI_ProgressMonitor *
gnc_hbci_new_pmonitor(Inter_data *data);

HBCI_Interactor *
gnc_hbci_new_interactor(Inter_data *data);

void add_log_text (Inter_data *data, const char *msg);



#endif
