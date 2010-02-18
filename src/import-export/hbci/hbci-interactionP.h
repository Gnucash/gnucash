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
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *
 *   MA  02110-1301  USA                                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef HBCI_INTERACTIONP_H
#define HBCI_INTERACTIONP_H

#include <aqbanking/banking.h>
#include <iconv.h>
#include "gnc-hbci-utils.h"


typedef enum
{
    INIT,
    RUNNING,
    FINISHED,
    ABORTED,
    CLOSING
} PMon_state;

struct _inter_data
{
    GtkWidget *parent;
    GtkWidget *dialog;

    /* Progress bars */
    GtkWidget *job_entry;
    GtkWidget *action_entry;
    GtkWidget *action_progress;
    /* Counters for progress bar */
    double action_max;

    /* Log window */
    GtkWidget *log_text;

    /* Buttons */
    GtkWidget *abort_button;
    GtkWidget *close_button;
    GtkWidget *close_checkbutton;

    const char *format_pin_user_bank;
    const char *format_pin_min_char;

    /* The iconv handler for utf8 -> latin1 conversion */
    GIConv gnc_iconv_handler;

    /* Flags to keep track on whether an HBCI action is running or
       not. */
    gboolean keepAlive;
    PMon_state state;

    /* Flag on Whether the PIN should be cached. */
    gboolean cache_pin;

    /* Dialogs */
    int showbox_id;
    GHashTable *showbox_hash;
    GtkWidget *showbox_last;

    /* Flag whether the last dialog showed any error */
    gboolean msgBoxError;
    /* Cache the lowest loglevel, corresponding to the most serious
       warning. */
    AB_BANKING_LOGLEVEL min_loglevel;
};

void delete_GNCInteractor (GNCInteractor *data);

void
gnc_hbci_add_callbacks(AB_BANKING *ba, GNCInteractor *data);

/* Performs the full conversion from the (aaarg) utf8-combi-texts
   passed from aqbanking into a "latin1-normal-text" format for
   us. The returned string is owned by the caller. */
gchar *gnc_hbci_utf8ToLatin1(GNCInteractor *data, const char *utf);

#endif
