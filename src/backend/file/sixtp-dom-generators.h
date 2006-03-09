/********************************************************************
 * sixtp-dom-generators.h                                           *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************/

#ifndef SIXTP_DOM_GENERATORS_H
#define SIXTP_DOM_GENERATORS_H

#include <glib.h>

#include "gnc-xml-helper.h"

#include "sixtp-dom-generators.h"
#include "gnc-commodity.h"
#include "qof.h"
#include "Recurrence.h"

xmlNodePtr text_to_dom_tree(const char *tag, const char *str);
xmlNodePtr int_to_dom_tree(const char *tag, gint64 val);
xmlNodePtr guid_to_dom_tree(const char *tag, const GUID* gid);
xmlNodePtr commodity_ref_to_dom_tree(const char *tag, const gnc_commodity *c);
xmlNodePtr timespec_to_dom_tree(const char *tag, const Timespec *spec);
gchar * timespec_nsec_to_string(const Timespec *ts);
gchar * timespec_sec_to_string(const Timespec *ts);
xmlNodePtr gdate_to_dom_tree(const char *tag, const GDate *spec);
xmlNodePtr gnc_numeric_to_dom_tree(const char *tag, const gnc_numeric *num);
xmlNodePtr kvp_frame_to_dom_tree(const char *tag, const kvp_frame *frame);
xmlNodePtr guint_to_dom_tree(const char *tag, guint an_int);
xmlNodePtr recurrence_to_dom_tree(const gchar *tag, const Recurrence *r);

gchar* double_to_string(double value);

#endif /* _SIXTP_DOM_GENERATORS_H_ */
