/********************************************************************\
 * gnc-file-history.h -- functions to maintain file-history menu    *
 * Copyright (C) 2000 Robby Stephenson         	                    *
 * Copyright (C) 2005 David Hampton            	                    *
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

#ifndef GNC_FILE_HISTORY_H
#define GNC_FILE_HISTORY_H

#define MAX_HISTORY_FILES 10	/* May be any number up to 10 */
#define HISTORY_STRING_SECTION  "history"
#define HISTORY_STRING_MAXFILES "maxfiles"
#define HISTORY_STRING_FILE_N   "file%d"

void gnc_history_add_file 		(const char *filename);
char * gnc_history_get_last		(void);
gchar *gnc_history_gconf_index_to_key   (guint index);
gint   gnc_history_gconf_key_to_index   (const gchar *fullkey);

#endif
