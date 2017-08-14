/*
 * import-parse.h -- a generic "parser" API for importers..  Allows importers
 * 	to parse dates and numbers, and provides a UI to ask for users to
 * 	resolve ambiguities.
 *
 * Created by:	Derek Atkins <derek@ihtfp.com>
 * Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
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

#ifndef IMPORT_PARSE_H
#define IMPORT_PARSE_H

#include "qof.h"

typedef enum
{
    GNCIF_NONE		= 0,

    /* number formats */
    GNCIF_NUM_PERIOD	= (1 << 1),
    GNCIF_NUM_COMMA	= (1 << 2),

    /* date formats */
    GNCIF_DATE_MDY	= (1 << 8),
    GNCIF_DATE_DMY	= (1 << 9),
    GNCIF_DATE_YMD	= (1 << 10),
    GNCIF_DATE_YDM	= (1 << 11)
} GncImportFormat;


GncImportFormat gnc_import_test_numeric(const char* str, GncImportFormat fmts);
GncImportFormat gnc_import_test_date(const char* str, GncImportFormat fmts);


GncImportFormat gnc_import_choose_fmt(const char* msg, GncImportFormat fmts,
                                      gpointer user_data);

gboolean gnc_import_parse_numeric(const char* str, GncImportFormat fmt,
                                  gnc_numeric *val);
gboolean gnc_import_parse_date(const char *date, GncImportFormat fmt,
                               Timespec *val);

/* Set and clear flags in bit-flags */
#define import_set_flag(i,f) (i |= f)
#define import_clear_flag(i,f) (i &= ~f)


#endif /* IMPORT_PARSE_H */
