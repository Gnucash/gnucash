/********************************************************************\
 * gnucash.h                                                        *
 *   (C) 1998 Rob Browning <rlb@cs.utexas.edu>                      *
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
\********************************************************************/

#ifndef GNUCASH_H
#define GNUCASH_H

/* There must be no non-(gnucash or guile) args in argc or argv by the
   time you call this.  This means, for example that you need to have
   called gnome_init before calling this function. */

int  gnc_main (int argc, char *argv[]);

void gnc_gw_init(void);

int  gnucash_ui_init(void);
int  gnc_ui_start_event_loop(void);

int  gnucash_ui_is_running(void);
int  gnucash_ui_is_terminating(void);

void gnc_ui_shutdown(void);
void gnc_ui_destroy(void);

void gnc_shutdown(int exit_status);

int     gnc_get_global_argc(void);
char ** gnc_get_global_argv(void);

#endif
