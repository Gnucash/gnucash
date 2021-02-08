/********************************************************************\
 * dialog-help.h -- GnuCash help window                             *
 * Copyright (C) 2021 Robert Fewell                                 *
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

#ifndef DIALOG_HELP_H
#define DIALOG_HELP_H

/**
 * Create a Help Dialog based on a pointer to a help_dialog_args
 *  structure.
 *
 * @param user_data A help_dialog_args structure
 */
void gnc_help_dialog_with_struct (gpointer user_data);

/**
 * Create a Help Dialog based arguments
 *
 * @param parent The parent widget window
 * @param dir_name The directory to load the html pages from
 * @param anchor The page to load, maybe of the form print-check.html#print-check
 */
void gnc_help_dialog_with_args (GtkWindow *parent, const char *dir_name,
                                const char *anchor);

/**
 * Set the function to call when a help link is used
 *
 * @param cd The help dialog function
 * @param user_data A help_dialog_args structure
 */
void gnc_help_dialog_set_help_func (GFunc cb, gpointer user_data);

#endif
