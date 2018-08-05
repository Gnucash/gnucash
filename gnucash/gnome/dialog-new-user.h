/********************************************************************\
 * dialog-new-user.h -- new user dialog for GnuCash                 *
 * Copyright (C) 2001 Dave Peticolas <dave@krondo.com>              *
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

#ifndef DIALOG_NEW_USER_H
#define DIALOG_NEW_USER_H

#define GNC_PREFS_GROUP_NEW_USER "dialogs.new-user"
#define GNC_PREF_FIRST_STARTUP "first-startup"

typedef struct _GNCNewUserDialog GNCNewUserDialog;

void gnc_ui_new_user_dialog (void);
void gnc_set_first_startup (gboolean first_startup);

/* Register the qif-import assistant */
void gnc_new_user_dialog_register_qif_assistant (void (*cb_fcn)(void));

/* private */
void gncp_new_user_finish (void);

#endif
