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

#include "SchedXaction.h"
#include "gnc-plugin-page-sx-list.h"

typedef struct _GncSxSlrTreeModelAdapter GncSxSlrTreeModelAdapter;
typedef struct _GncSxSinceLastRunDialog GncSxSinceLastRunDialog;
typedef struct _GncSxSlrSummary
{
     gboolean need_dialog; /**< If the dialog needs to be displayed. **/

     gint num_instances; /**< The number of total instances (all states). **/
     gint num_to_create_instances; /**< The number of (not-auto-create) to-create instances. **/
     gint num_auto_create_instances;  /**< The total number of auto-create instances. **/
     gint num_auto_create_no_notify_instances; /**< The number of automatically-created instances that do no request notification. **/
} GncSxSlrSummary;

GncSxSlrTreeModelAdapter* gnc_sx_get_slr_model(void);

/**
 * @param summary Caller-provided, populated with a summarization of the
 * state of the model.  Specifically, used to determine if there are SLR SXes
 * that need either auto-creation or user-interaction.
 **/
void gnc_sx_slr_model_summarize(GncSxSlrTreeModelAdapter *model, GncSxSlrSummary *summary);

/**
 * This encapsulates the "run when file opened" application logic.  As such,
 * it should probably move to a non-ui file.
 **/
void gnc_sx_sxsincelast_book_opened(void);

/**
 * Create the since-last-run dialog.
 **/
GncSxSinceLastRunDialog*  gnc_ui_sx_since_last_run_dialog(GncSxSlrTreeModelAdapter *model);

// eliminate...
void gnc_ui_sxsincelast_dialog_create(void);

void gnc_sx_slr_model_effect_change(GncSxSlrTreeModelAdapter *model, gboolean auto_create_only, GList **created_transaction_guids, GList **creation_errors);

#endif
