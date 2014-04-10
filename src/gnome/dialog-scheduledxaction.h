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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef DIALOG_SCHEDULEDXACTION_H
#define DIALOG_SCHEDULEDXACTION_H

#include "config.h"
#include <gnome.h>
#include "SchedXaction.h"

struct _SchedXactionDialog;
struct _SchedXactionEditorDialog;

typedef struct _SchedXactionDialog SchedXactionDialog;
typedef struct _SchedXactionEditorDialog SchedXactionEditorDialog;

SchedXactionDialog * gnc_ui_scheduled_xaction_dialog_create(void);
void gnc_ui_scheduled_xaction_dialog_destroy(SchedXactionDialog *sxd);
void row_select_handler( GtkCList *clist, gint row, gint col, GdkEventButton *event, gpointer d );

SchedXactionEditorDialog *
gnc_ui_scheduled_xaction_editor_dialog_create( SchedXactionDialog *sxd,
					       SchedXaction *sx,
                                               int newP );
void gnc_ui_scheduled_xaction_editor_dialog_destroy(SchedXactionEditorDialog *sxd);

#endif

/**
 * TODO:
 * . date-entries should back-stop each other?
 * . modify gtkentry to prohibit fracation num-occurance values.
 **/
