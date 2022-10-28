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
 * @brief File import module interface
 * @author Copyright (C) 2022 John Ralls <jralls@ceridwen.us>
 */

#ifndef GNC_FILE_AQB_IMPORT_H
#define GNC_FILE_AQB_IMPORT_H

#include <glib.h>

G_BEGIN_DECLS

/**
 * Import files via AQBanking's Import Dialog. This permits importing
 * any file format that Aqbanking supports.
 * @param parent A GtkWindow for setting the import dialog transient for.
 */
void gnc_file_aqbanking_import_dialog (GtkWindow *parent);


G_END_DECLS

/** @} */
/** @} */

#endif /* GNC_FILE_AQB_IMPORT_H */
