/********************************************************************\
 * gncEmployee.h -- the Core Employee Interface                     *
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
/** @addtogroup Business
    @{ */
/** @addtogroup Employee
    @{ */
/** @file gncEmployee.h
    @brief Employee Interface
    @author Copyright (C) 2001 Derek Atkins <warlord@MIT.EDU>
*/

#ifndef GNC_EMPLOYEE_H_
#define GNC_EMPLOYEE_H_

typedef struct _gncEmployee GncEmployee;
typedef struct _gncEmployeeClass GncEmployeeClass;

#include "gncAddress.h"
#include "Account.h"

#define GNC_ID_EMPLOYEE "gncEmployee"

/* --- type macros --- */
#define GNC_TYPE_EMPLOYEE            (gnc_employee_get_type ())
#define GNC_EMPLOYEE(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_EMPLOYEE, GncEmployee))
#define GNC_EMPLOYEE_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_EMPLOYEE, GncEmployeeClass))
#define GNC_IS_EMPLOYEE(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_EMPLOYEE))
#define GNC_IS_EMPLOYEE_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_EMPLOYEE))
#define GNC_EMPLOYEE_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_EMPLOYEE, GncEmployeeClass))
GType gnc_employee_get_type(void);

/** @name Create/Destroy Functions
 @{ */
GncEmployee *gncEmployeeCreate (QofBook *book);
void gncEmployeeDestroy (GncEmployee *employee);
void gncEmployeeBeginEdit (GncEmployee *employee);
void gncEmployeeCommitEdit (GncEmployee *employee);
int gncEmployeeCompare (const GncEmployee *a, const GncEmployee *b);
/** @} */

/** @name Set Functions
 @{ */
void gncEmployeeSetID (GncEmployee *employee, const char *id);
void gncEmployeeSetUsername (GncEmployee *employee, const char *username);
void gncEmployeeSetLanguage (GncEmployee *employee, const char *language);
void gncEmployeeSetAcl (GncEmployee *employee, const char *acl);
void gncEmployeeSetWorkday (GncEmployee *employee, gnc_numeric workday);
void gncEmployeeSetRate (GncEmployee *employee, gnc_numeric rate);
void gncEmployeeSetCurrency (GncEmployee *employee, gnc_commodity * currency);
void gncEmployeeSetActive (GncEmployee *employee, gboolean active);
void gncEmployeeSetCCard (GncEmployee *employee, Account* ccard_acc);
void qofEmployeeSetAddr (GncEmployee *employee, QofInstance *addr_ent);

/** @} */

/** @name Get Functions
 @{ */
QofBook * gncEmployeeGetBook (GncEmployee *employee);
const char * gncEmployeeGetID (const GncEmployee *employee);
const char * gncEmployeeGetUsername (const GncEmployee *employee);
GncAddress * gncEmployeeGetAddr (const GncEmployee *employee);
const char * gncEmployeeGetLanguage (const GncEmployee *employee);
const char * gncEmployeeGetAcl (const GncEmployee *employee);
gnc_numeric gncEmployeeGetWorkday (const GncEmployee *employee);
gnc_numeric gncEmployeeGetRate (const GncEmployee *employee);
gnc_commodity * gncEmployeeGetCurrency (const GncEmployee *employee);
gboolean gncEmployeeGetActive (const GncEmployee *employee);
Account * gncEmployeeGetCCard (const GncEmployee *employee);
/** @} */


/** Return a pointer to the instance gncEmployee that is identified
 *  by the guid, and is residing in the book. Returns NULL if the
 *  instance can't be found.
 *  Equivalent function prototype is
 *  GncEmployee * gncEmployeeLookup (QofBook *book, const GUID *guid);
 */
static inline GncEmployee * gncEmployeeLookup (const QofBook *book, const GUID *guid)
{
    QOF_BOOK_RETURN_ENTITY(book, guid, GNC_ID_EMPLOYEE, GncEmployee);
}

gboolean gncEmployeeIsDirty (const GncEmployee *employee);

#define EMPLOYEE_ID			"id"
#define EMPLOYEE_USERNAME	"username"
#define EMPLOYEE_ADDR		"addr"
#define EMPLOYEE_LANGUAGE 	"native language"
#define EMPLOYEE_ACL		"acl"
#define EMPLOYEE_WORKDAY	"workday"
#define EMPLOYEE_RATE		"rate"
#define EMPLOYEE_CC    "credit_card_account"

/** deprecated routines */
#define gncEmployeeGetGUID(E) qof_entity_get_guid(QOF_INSTANCE(E))
#define gncEmployeeGetBook(E) qof_instance_get_book(QOF_INSTANCE(E))
#define gncEmployeeRetGUID(E) (E ? *(qof_entity_get_guid(QOF_INSTANCE(E))) : *(guid_null()))
#define gncEmployeeLookupDirect(G,B) gncEmployeeLookup((B),&(G))

#endif /* GNC_EMPLOYEE_H_ */
/** @} */
/** @} */
