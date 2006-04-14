/*
 * druid-gnc-xml-import.h --
 * Copyright (C) 2006 Andreas Koehler <andi5.py@gmx.net>
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

#ifndef DRUID_GNC_XML_IMPORT_H
#define DRUID_GNC_XML_IMPORT_H

#include "qof.h"

gboolean gnc_xml_convert_single_file (const gchar *filename);

/* this is NOT fully implemented */
void gnc_xml_merge_files (void);

#endif /* DRUID_GNC_XML_IMPORT_H */
