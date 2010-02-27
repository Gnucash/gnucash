/********************************************************************\
 * dialog-sx-since-last-run.h : dialog for scheduled transaction    *
 * since-last-run processing.                                       *
 * Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org>          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of version 2 of the GNU General Public *
 * License as published by the Free Software Foundation.            *
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

#ifndef DIALOG_SX_SINCE_LAST_RUN_H
#define DIALOG_SX_SINCE_LAST_RUN_H

#include "config.h"

#include <gtk/gtk.h>

#include "gnc-sx-instance-model.h"
#include "gnc-plugin-page-sx-list.h"

typedef struct _GncSxSlrTreeModelAdapter GncSxSlrTreeModelAdapter;
typedef struct _GncSxSinceLastRunDialog GncSxSinceLastRunDialog;

/**
 * This encapsulates the "run when file opened" application logic.  As such,
 * it should probably move to a non-ui file.
 **/
void gnc_sx_sxsincelast_book_opened(void);

/**
 * Create the since-last-run dialog.
 **/
GncSxSinceLastRunDialog*  gnc_ui_sx_since_last_run_dialog(GncSxInstanceModel *sx_instances,
        GList *auto_created_txn_guids);

#endif
