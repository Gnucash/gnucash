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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef QOF_H_
#define QOF_H_
/** @defgroup QOF Query Object Framework
 @{
*/

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
    @addtogroup Math128 Math128: 128-bit Integer Math Library
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
/** @addtogroup Event Event: QOF event handlers.
	@ingroup QOF
*/
/** @addtogroup Choice Choice and collect : One to many links.
	@ingroup QOF
*/
/** @addtogroup BookMerge Merging QofBook structures
	@ingroup QOF
*/
/** \addtogroup Reference Referring to entities outside a partial book.
	\ingroup QOF
*/
/**
    @addtogroup Utilities Misc Utilities
    @ingroup QOF
*/
/** @} */

#include <glib.h>
#include "qofid.h"
#include "qoflog.h"
#include "gnc-date.h"
#include "gnc-numeric.h"
#include "qofutil.h"
#include "guid.h"
#include "kvp_frame.h"
#include "kvp-util.h"
#include "kvp-util-p.h"
#include "qofbackend.h"
#include "qofid-p.h"
#include "qofinstance-p.h"
#include "qofbook.h"
#include "qofclass.h"
#include "qofevent.h"
#include "qofobject.h"
#include "qofquery.h"
#include "qofquerycore.h"
#include "qofsession.h"
#include "qofsql.h"
#include "qofchoice.h"
#include "qofbookmerge.h"
#include "qofreference.h"

/** allow easy logging of QSF debug messages */
#define QOF_MOD_QSF "qof.backend.qsf"
/** allow easy loading of the QSF backend */
#define QSF_BACKEND_LIB "gncqof-backend-qsf"

#endif /* QOF_H_ */
