/********************************************************************\
 * gnc-err-popup.h -- GnuCash GUI Error Popup                       *
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
 *                                                                  *
 *   Author: Linas Vepstas (linas@linas.org)                        *
\********************************************************************/

/** @file gnc-err-popup.h @brief GnuCash GUI error loging facility */

#ifndef GNC_ERR_POPUP_H
#define GNC_ERR_POPUP_H

#include <glib.h>
#include <stdarg.h>
#include <stdio.h>

/* -------------------------------------------------------- */
/* Infrastructure to send messages go to GUI popups, not to stderr! 
 * Incompletely implemented, needs work.
 * XXX This probably duplicates some popup code elswwhere in the 
 * code and should be trashed at earliest convenience.
 */
typedef void (*GNCGuiMessage) (const char *format, va_list args);
void gnc_set_warning_message (GNCGuiMessage func);
void gnc_set_error_message (GNCGuiMessage func);

gboolean gnc_send_gui_warning (const char *format, ...) G_GNUC_PRINTF(1,2);
gboolean gnc_send_gui_error (const char *format, ...) G_GNUC_PRINTF(1,2);

#define PWARN_GUI(format, args...) {               \
   gnc_send_gui_error(format, ## args);            \
}

#endif /* GNC_ERR_POPUP_H */
