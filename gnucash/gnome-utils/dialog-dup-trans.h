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


/********************************************************************\
 * gnc_dup_trans_dialog                                             *
 *   opens up a window to do an automatic transfer between accounts *
 *                                                                  *
 * Args:   parent    - the parent of the window to be created       *
 *         title     - the text of the title label, otherwise       *
 *                     defaults to "New Transaction Information"    *
 *         show_date - TRUE to display date label and edit widgets  *
 *         date      - the initial date to use, and the output      *
 *                     parameter for the new date                   *
 *         num       - input num field                              *
 *         out_num   - output num field, g_newed string             *
 *         tnum      - input tnum field, if used, else NULL         *
 *         out_tnum  - output tnum field, g_newed string            *
 *         tassoc    - input association field, if used, else NULL  *
 *         out_tnum  - output association field, g_newed string     *
 * Return: TRUE if user closes dialog with 'OK'                     *
\********************************************************************/
gboolean
gnc_dup_trans_dialog (GtkWidget * parent, const char* title, gboolean show_date,
                      time64 *date_p, const char *num, char **out_num,
                      const char *tnum, char **out_tnum,
                      const char *tassoc, char **out_tassoc);

gboolean
gnc_dup_trans_dialog_gdate (GtkWidget * parent, GDate *gdate_p,
                            const char *num, char **out_num);


/**
 * Opens up a window to ask for a date for the duplicated element
 *
 * \param parent The parent of the window to be created
 * \param title The text of the title label
 * \param date  The initial date to use, and the output
 *                   parameter for the new date. Must not be NULL.
 *
 * \return TRUE if user closes dialog with 'OK', otherwise FALSE
 */
gboolean
gnc_dup_date_dialog (GtkWidget * parent, const char* title, GDate *date);

#endif // DIALOGDUPTRANS_H
