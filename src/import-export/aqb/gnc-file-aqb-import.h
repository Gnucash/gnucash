/*
 * gnc-file-aqb-import.h --
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

/**
 * @addtogroup Import_Export
 * @{
 * @addtogroup AqBanking
 * @{
 * @file aqbanking/gnc-file-aqb-import.h
 * @brief DTAUS import module interface
 * @author Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#ifndef GNC_FILE_AQB_IMPORT_H
#define GNC_FILE_AQB_IMPORT_H

#include <glib.h>

G_BEGIN_DECLS

/**
 * This routine will pop up a standard file selection dialog asking the user to
 * pick a file to import.  This file will be opened and read.  Its contents will
 * be imported into the current book, using the import matcher from
 * import-main-matcher.h.
 *
 * @param aqbanking_importername The aqbanking importer module that should be
 * used.  Possible values: "dtaus", "csv", "swift", or more.
 *
 * @param aqbanking_formatname In aqbanking, each importer has one or more data
 * formats available which define the actual data fields that should be used.
 * In aqbanking, such a different format is called a "profile".  Possible values
 * for swift: "swift-mt940" or "swift-mt942", but for all others: "default", or
 * more precisely: Look into $datadir/aqbanking/imexporters and look into the
 * "name" field of the foo.conf files.
 *
 * @param exec_as_aqbanking_jobs If TRUE, additionally queue the imported
 * transactions as online jobs over aqbanking/HBCI.  If FALSE, just import the
 * transactions and that's it.
 */
void gnc_file_aqbanking_import (const gchar *aqbanking_importername,
                                const gchar *aqbanking_formatname,
                                gboolean exec_as_aqbanking_jobs);

G_END_DECLS

/** @} */
/** @} */

#endif /* GNC_FILE_AQB_IMPORT_H */
