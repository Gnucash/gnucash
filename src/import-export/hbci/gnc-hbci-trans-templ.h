/********************************************************************\
 * gnc-hbci-trans-templ.h -- Templates for HBCI transactions        *
 * Copyright (C) 2003 Christian Stimming                            *
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
\********************************************************************/

/** @file gnc-hbci-trans-templ.h Templates for HBCI transactions */

#include "qof.h"

/** A template for a HBCI transaction */
typedef struct _trans_data GNCTransTempl;

/** @name Constructor */
/*@{*/
GNCTransTempl *gnc_trans_templ_new(void);
GNCTransTempl *gnc_trans_templ_new_full(const char *name,
                                        const char *recp_name,
                                        const char *recp_account,
                                        const char *recp_bankcode,
                                        gnc_numeric amount,
                                        const char *purpose,
                                        const char *purpose_cont);

void gnc_trans_templ_delete(GNCTransTempl *t);
void gnc_trans_templ_delete_glist(GList *l);
/*@}*/

/** @name Serialization -- to kvp_frame and back */
/*@{*/
/** Constructor from a kvp_frame (the kvp_frame is left unchanged) */
GNCTransTempl *gnc_trans_templ_from_kvp(kvp_frame *k);
/** Creates a kvp_frame from this TransTempl */
kvp_frame *gnc_trans_templ_to_kvp(const GNCTransTempl *t);

/** Creates a GList of GNCTransTempl from a GList of kvp_values which
    in turn contain a kvp_frame. */
GList *gnc_trans_templ_glist_from_kvp_glist(GList *v);
/** Creates a GList of kvp_value (which in turn contain a kvp_frame)
    from a GList of GNCTransTempl. */
GList *gnc_trans_templ_kvp_glist_from_glist(GList *k);
/*@}*/

/** @name GNCTransTempl value access */
/*@{*/
const char *gnc_trans_templ_get_name(const GNCTransTempl *t);
const char *gnc_trans_templ_get_name_key(const GNCTransTempl *t);
const char *gnc_trans_templ_get_recp_name(const GNCTransTempl *t);
const char *gnc_trans_templ_get_recp_account(const GNCTransTempl *t);
const char *gnc_trans_templ_get_recp_bankcode(const GNCTransTempl *t);

/** Amount */
gnc_numeric gnc_trans_templ_get_amount(const GNCTransTempl *t);

/** Purpose, description */
const char *gnc_trans_templ_get_purpose(const GNCTransTempl *t);
const char *gnc_trans_templ_get_purpose_cont(const GNCTransTempl *t);
/*@}*/

/** @name GNCTransTempl value storing */
/*@{*/
void gnc_trans_templ_set_name(GNCTransTempl *t, const char *);
void gnc_trans_templ_set_recp_name(GNCTransTempl *t, const char *);
void gnc_trans_templ_set_recp_account(GNCTransTempl *t, const char *);
void gnc_trans_templ_set_recp_bankcode(GNCTransTempl *t, const char *);

/** Amount */
void gnc_trans_templ_set_amount(GNCTransTempl *t, gnc_numeric );

/** Purpose, description */
void gnc_trans_templ_set_purpose(GNCTransTempl *t, const char *);
void gnc_trans_templ_set_purpose_cont(GNCTransTempl *t, const char *);
/*@}*/

