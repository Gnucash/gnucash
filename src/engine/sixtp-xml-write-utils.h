/********************************************************************
 * sixtp-xml-write-utils.h                                          *
 * Copyright 2001 Gnumatic, Inc.                                    *
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
 ********************************************************************/

#ifndef _SIXTP_XML_WRITE_UTILS_H_
#define _SIXTP_XML_WRITE_UTILS_H_

#include <config.h>

#include <glib.h>

#include "gnc-xml-helper.h"
#include "gnc-numeric.h"
#include "GNCId.h"
#include "date.h"
#include "kvp_frame.h"

gboolean xml_add_str(xmlNodePtr p, const char *tag, const char *str,
                     gboolean include_if_empty);

gboolean xml_add_character(xmlNodePtr p, const char *tag, const char c);

gboolean xml_add_gint64(xmlNodePtr p, const char *tag, const gint64 value);

gboolean xml_add_gint32(xmlNodePtr p, const char *tag, const gint32 value);

gboolean xml_add_double(xmlNodePtr p, const char *tag, const double value);

gboolean xml_add_gnc_numeric(xmlNodePtr p, const char *tag,
                             const gnc_numeric n);

gboolean xml_add_guid(xmlNodePtr p, const char *tag, const GUID *guid);

gboolean xml_add_editable_timespec(xmlNodePtr p, const char *tag,
                                   const Timespec *ts,
                                   gboolean include_if_zero);

gboolean xml_add_kvp_frame(xmlNodePtr p, const char *tag,
                           const kvp_frame *kvpf, gboolean add_if_empty);



#endif /* _SIXTP_XML_WRITE_UTILS_H_ */
