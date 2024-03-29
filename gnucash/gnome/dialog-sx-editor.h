/********************************************************************\
 * dialog-sx-editor.h : dialog for scheduled transaction editing    *
 * Copyright (C) 2001,2006 Joshua Sled <jsled@asynchronous.org>     *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of version 2 and/or version 3 of the   *
 * GNU General Public License as published by the Free Software     *
 * Foundation.                                                      *
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

#ifndef DIALOG_SX_EDITOR_H
#define DIALOG_SX_EDITOR_H

#include "SchedXaction.h"
#include <gtk/gtk.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DIALOG_SCHEDXACTION_CM_CLASS "dialog-scheduledtransactions"
#define DIALOG_SCHEDXACTION_EDITOR_CM_CLASS "dialog-scheduledtransaction-editor"

#define GNC_PREFS_GROUP_SXED "dialogs.sxs.transaction-editor"
#define GNC_PREF_CREATE_DAYS "create-days"
#define GNC_PREF_REMIND_DAYS "remind-days"
#define GNC_PREF_CREATE_AUTO "create-auto"
#define GNC_PREF_NOTIFY      "notify"

typedef struct _GncSxEditorDialog GncSxEditorDialog;

GncSxEditorDialog* gnc_ui_scheduled_xaction_editor_dialog_create(GtkWindow *parent,
    SchedXaction *sx, gboolean newSX);

void gnc_ui_scheduled_xaction_editor_dialog_destroy(GncSxEditorDialog *sxd);

/**
 * Sets up a book opened hook.  The function called may open a "since
 * last run" dialog based upon the user's preferences.
 **/
void gnc_ui_sx_initialize (void);

#ifdef __cplusplus
}
#endif

#endif
