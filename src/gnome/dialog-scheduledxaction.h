/********************************************************************\
 * dialog-scheduledxaction.h : dialogs for scheduled transactions   *
 * Copyright (C) 2001 Joshua Sled <jsled@asynchronous.org>          *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef DIALOG_SCHEDULEDXACTION_H
#define DIALOG_SCHEDULEDXACTION_H

#include "SchedXaction.h"

#define DIALOG_SCHEDXACTION_CM_CLASS "dialog-scheduledtransactions"
#define DIALOG_SCHEDXACTION_EDITOR_CM_CLASS "dialog-scheduledtransaction-editor"

#define SXED_GCONF_SECTION "dialogs/scheduled_trans/transaction_editor"
#define KEY_CREATE_AUTO	"create_auto"
#define KEY_NOTIFY	"notify"
#define KEY_CREATE_DAYS	"create_days"
#define KEY_REMIND_DAYS	"remind_days"

struct _SchedXactionDialog;
struct _SchedXactionEditorDialog;

typedef struct _SchedXactionDialog SchedXactionDialog;
typedef struct _SchedXactionEditorDialog SchedXactionEditorDialog;

SchedXactionDialog * gnc_ui_scheduled_xaction_dialog_create(void);
void gnc_ui_scheduled_xaction_dialog_destroy(SchedXactionDialog *sxd);
void row_select_handler( GtkCList *clist, gint row, gint col,
                         GdkEventButton *event, gpointer d );

void gnc_sxd_list_refresh( SchedXactionDialog *sxd );

SchedXactionEditorDialog *
gnc_ui_scheduled_xaction_editor_dialog_create( SchedXactionDialog *sxd,
					       SchedXaction *sx,
                                               gboolean newSX );

void gnc_ui_scheduled_xaction_editor_dialog_destroy( SchedXactionEditorDialog *sxd );

/**
 * Sets up a book opened hook.  The function called may open a "since
 * last run" dialog based upon the user's preferences.
 **/
void gnc_ui_sx_initialize (void);

#endif
