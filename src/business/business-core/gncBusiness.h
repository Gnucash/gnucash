/* gncBusiness.h -- Business Helper Functions
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
/** @addtogroup Engine
    @{ */
/** @addtogroup Business
    The Business Engine provides a set of structures for
    that provide small-business accounting features.
    @{ */

/** @file gncBusiness.h -- Business Helper Functions
 *  @author Copyright (C) 2002 Derek Atkins
 *  @author Derek Atkins <warlord@MIT.EDU>
 */

/** @} */
/** @} */
#ifndef GNC_BUSINESS_H_
#define GNC_BUSINESS_H_


/* @deprecated backwards-compat definitions */
#define GNC_BILLTERM_MODULE_NAME GNC_ID_BILLTERM
#define GNC_CUSTOMER_MODULE_NAME GNC_ID_CUSTOMER
#define GNC_EMPLOYEE_MODULE_NAME GNC_ID_EMPLOYEE
#define GNC_ENTRY_MODULE_NAME    GNC_ID_ENTRY
#define GNC_INVOICE_MODULE_NAME  GNC_ID_INVOICE
#define GNC_JOB_MODULE_NAME      GNC_ID_JOB
#define GNC_ORDER_MODULE_NAME    GNC_ID_ORDER
#define GNC_OWNER_MODULE_NAME    GNC_ID_OWNER
#define GNC_TAXTABLE_MODULE_NAME GNC_ID_TAXTABLE
#define GNC_VENDOR_MODULE_NAME   GNC_ID_VENDOR

#endif /* GNC_BUSINESS_H_ */
