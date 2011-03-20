/********************************************************************\
 * qofquery-deserial.h -- Convert Qof-Query XML to QofQuery         *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/
/*
 qofquery-deserial.h
 Convert Qof-Query XML to QofQuery
author Copyright (C) 2004 Linas Vepstas <linas@linas.org>
*/

#ifndef QOF_QUERY_DESERIAL_H
#define QOF_QUERY_DESERIAL_H

#include "qofquery.h"
#include <libxml/tree.h>

/*
    Qof Queries can be converted to and from XML so that they
    can be sent from here to there. This file implements the
    routine needed to convert the XML back into a C struct.

    Unfinished. XXX Why is this easier than reading a text/sql
    file?

NOTE: Development of this idea has ceased and this file is
no longer included in the QOF library. It remains in CVS for now.

 */
/* Given an XML tree, reconstruct and return the equivalent query. */
QofQuery *qof_query_from_xml (xmlNodePtr);

#endif /* QOF_QUERY_DESERIAL_H */
