/********************************************************************\
 * qof.h -- Master QOF public include file                          *
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

#ifndef QOF_H_
#define QOF_H_
/** @defgroup QOF Query Object Framework */
/** @{ */

/**
    @addtogroup Date Date:  Date and Time Printing, Parsing and Manipulation
    @ingroup QOF
*/
/**
    @addtogroup Entity Entity: Types, Identity and Instance Framework
    @ingroup QOF

*/
/**
    @addtogroup KVP KVP: Key-Value Pairs
    @ingroup QOF
*/
/**
    @addtogroup Numeric Numeric: Rational Number Handling w/ Rounding Error Control
    @ingroup QOF
*/
/**
    @addtogroup Object Object: Dynamic Object Class Framework
    @ingroup QOF
*/
/**
    @addtogroup Query Query: Querying for Objects
    @ingroup QOF
*/
/**
    @addtogroup Trace Trace: Error Reporting and Debugging
    @ingroup QOF
*/
/**
    @addtogroup Utilities Misc Utilities
    @ingroup QOF
*/
/** @} */

#include "qof/gnc-date.h"
#include "qof/gnc-engine-util.h"
#include "qof/gnc-numeric.h"
#include "qof/gnc-event.h"
#include "qof/gnc-trace.h"
#include "qof/guid.h"
#include "qof/kvp_frame.h"
#include "qof/qofbackend.h"
#include "qof/qofid.h"
#include "qof/qofbook.h"
#include "qof/qofclass.h"
#include "qof/qofobject.h"
#include "qof/qofquery.h"
#include "qof/qofquerycore.h"
#include "qof/qofsession.h"
#include "qof/qofsql.h"

#endif /* QOF_H_ */
