/********************************************************************\
 * qofquery-serialize.h -- Convert QofQuery to XML                  *
 * Copyright (C) 2004 Linas Vepstas <linas@linas.org>               *
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

/** @file qofquery-serialize.h
    @breif Convert QofQuery to XML
    @author Copyright (C) 2001,2002,2004 Linas Vepstas <linas@linas.org>
 */

#ifndef QOF_QUERY_SERIALIZE_H
#define QOF_QUERY_SERIALIZE_H

#include <qof/qofquery.h>
#include <libxml/tree.h>

/** Take the query passed as input, and serialize it into XML.
 *  The DTD used will be a very qofquery specific DTD
 *  This is NOT the XQuery XML.
 */
xmlNodePtr qof_query_to_xml (QofQuery *q);

#endif /* QOF_QUERY_SERIALIZE_H */
