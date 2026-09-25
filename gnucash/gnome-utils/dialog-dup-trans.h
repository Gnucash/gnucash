/********************************************************************\
 * dialog-dup-trans.h -- duplicate transaction dialog               *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
 * Copyright (C) 2011, Christian Stimming                           *
 * Author: Christian Stimming
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


#ifndef DIALOGDUPTRANS_H
#define DIALOGDUPTRANS_H

#include <gtk/gtk.h>
#include <gnc-date.h>

#ifdef __cplusplus
extern "C"
{
#endif

/** Result of an accepted duplicate transaction request. The completion
 * callback owns the result and must release it with gnc_dup_trans_result_free().
 * A NULL result denotes cancellation or parent destruction. */
typedef struct
{
    time64 date;
    GDate gdate;
    gchar *num;
    gchar *tnum;
    gchar *doclink;
} GncDupTransResult;

typedef void (*GncDupTransDialogCallback) (GncDupTransResult *result,
                                           gpointer user_data);

void gnc_dup_trans_result_free (GncDupTransResult *result);

/** Presents the duplicate transaction request without entering a nested main
 * loop. The callback is invoked exactly once and takes ownership of result. */
void gnc_dup_trans_dialog_async (GtkWindow *parent,
                                 const gchar *window_title,
                                 const gchar *title,
                                 gboolean show_date,
                                 time64 initial_date,
                                 const gchar *num,
                                 const gchar *tnum,
                                 const gchar *doclink,
                                 GncDupTransDialogCallback completed,
                                 gpointer user_data);

void gnc_dup_date_dialog_async (GtkWindow *parent,
                                const gchar *title,
                                const GDate *initial_date,
                                GncDupTransDialogCallback completed,
                                gpointer user_data);

void gnc_dup_time64_dialog_async (GtkWindow *parent,
                                  const gchar *window_title,
                                  const gchar *title,
                                  time64 initial_date,
                                  GncDupTransDialogCallback completed,
                                  gpointer user_data);

#ifdef __cplusplus
}
#endif

#endif /* DIALOGDUPTRANS_H */
