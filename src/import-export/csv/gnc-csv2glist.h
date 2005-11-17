/*
 * gnc-csv2glist.h -- Parse Comma Separated Variable files
 *
 * Created by:  Derek Atkins <derek@ihtfp.com>
 * Copyright (c) 2004 Derek Atkins <derek@ihtfp.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef GNC_IMPORT_CSV_2GLIST_H
#define GNC_IMPORT_CSV_2GLIST_H

/**
 * gnc_csv_parse -- parse a Comma Separated Variable FILE into
 *                  a list of lists of values.
 *
 * Args:  file : the input file to read
 * Returns     : a list of lists of strings.  Both lists and all strings
 *               must be g_free()'d by the caller.
 */

GList * gnc_csv_parse (FILE *file);

#endif
