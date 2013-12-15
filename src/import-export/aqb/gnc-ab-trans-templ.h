/*
 * gnc-ab-trans-templ.h --
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

/**
 * @addtogroup Import_Export
 * @{
 * @addtogroup AqBanking
 * @{
 * @file gnc-ab-trans-templ.h
 * @brief Templates for AqBanking transactions
 * @author Copyright (C) 2003 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#ifndef GNC_AB_TRANS_TEMPL_H
#define GNC_AB_TRANS_TEMPL_H

#include <glib.h>

#include "qof.h"

G_BEGIN_DECLS

/** A template for an AqBanking transaction */
typedef struct _GncABTransTempl GncABTransTempl;

/**
 * Create a template with unset contents.
 *
 * @return A newly allocated GncABTransTempl
 */
GncABTransTempl *gnc_ab_trans_templ_new(void);

/**
 * Create a template with given contents.
 *
 * @param name Name of the template
 * @param recp_name Name of the recipient
 * @param recp_account Account Number of the recipient
 * @param recp_bankcode Bank Code of the recipient
 * @param amount Amount
 * @param purpose First purpose line
 * @param purpose_cont Second purpose line
 * @return A newly allocated GncABTransTempl
 */
GncABTransTempl *gnc_ab_trans_templ_new_full(
    const gchar *name, const gchar *recp_name, const gchar *recp_account,
    const gchar *recp_bankcode, gnc_numeric amount, const gchar *purpose,
    const gchar *purpose_cont);

/**
 * Create a template, taking the values from a kvp_frame.
 *
 * @param k kvp_frame
 * @return A newly allocated GncABTransTempl
 */
GncABTransTempl *gnc_ab_trans_templ_new_from_kvp(const kvp_frame *k);

/**
 * Create a list of templates from a list of kvp_values which in turn
 * contain a kvp_frame.
 *
 * @param v GList of kvp_values
 * @return A GList of newly allocated GncABTransTempls
 */
GList *gnc_ab_trans_templ_list_new_from_kvp_list(GList *v);

/**
 * Free the memory used by a template.
 *
 * @param t GncABTransTempl to be freed
 */
void gnc_ab_trans_templ_free(GncABTransTempl *t);

/**
 * Free the memory used by a list of templates, including the list itself.
 *
 * @param l GList of GncABTransTempl
 */
void gnc_ab_trans_templ_list_free(GList *l);

/**
 * Create a kvp_frame a given template.
 *
 * @param t Template
 * @return A newly allocated kvp_frame
 */
kvp_frame *gnc_ab_trans_templ_to_kvp(const GncABTransTempl *t);

/**
 * Create a list of kvp_values, which in turn contain a kvp_frame, from a list
 * of templates.
 *
 * @param k GList of GncABTransTempls
 * @return GList of newly allocated kvp_values
 */
GList *gnc_ab_trans_templ_list_to_kvp_list(GList *k);

/**
 * @param t Template
 * @return Name of the template, an internal string
 */
const gchar *gnc_ab_trans_templ_get_name(const GncABTransTempl *t);

/**
 * @param t Template
 * @return Name of the recipient, an internal string
 */
const gchar *gnc_ab_trans_templ_get_recp_name(const GncABTransTempl *t);

/**
 * @param t Template
 * @return Account Number of the recipient, an internal string
 */
const gchar *gnc_ab_trans_templ_get_recp_account(const GncABTransTempl *t);

/**
 * @param t Template
 * @return Bank Code of the recipient, an internal string
 */
const gchar *gnc_ab_trans_templ_get_recp_bankcode(const GncABTransTempl *t);

/**
 * @param t Template
 * @return Amount
 */
gnc_numeric gnc_ab_trans_templ_get_amount(const GncABTransTempl *t);

/**
 * @param t Template
 * @return First purpose line, an internal string
 */
const gchar *gnc_ab_trans_templ_get_purpose(const GncABTransTempl *t);

/**
 * @param t Template
 * @return Second purpose line, an internal string
 */
const gchar *gnc_ab_trans_templ_get_purpose_cont(const GncABTransTempl *t);

/**
 * Set the name of a template.
 *
 * @param t Template
 * @param name Name
 */
void gnc_ab_trans_templ_set_name(GncABTransTempl *t, const gchar *name);

/**
 * Replace the Account Number of the recipient stored in a template.
 *
 * @param t Template
 * @param recp_name Account Number of the recipient
 */
void gnc_ab_trans_templ_set_recp_name(GncABTransTempl *t,
                                      const gchar *recp_name);

/**
 * Replace the Account Number of the recipient stored in a template.
 *
 * @param t Template
 * @param recp_account Account Number of the recipient
 */
void gnc_ab_trans_templ_set_recp_account(GncABTransTempl *t,
        const gchar *recp_account);

/**
 * Replace the Bank Code of the recipient stored in a template.
 *
 * @param t Template
 * @param recp_bankcode Bank Code of the recipient
 */
void gnc_ab_trans_templ_set_recp_bankcode(GncABTransTempl *t,
        const gchar *recp_bankcode);

/**
 * Replace the amount stored in a template.
 *
 * @param t Template
 * @param amount Amount
 */
void gnc_ab_trans_templ_set_amount(GncABTransTempl *t, gnc_numeric amount);

/**
 * Replace the first purpose line stored in a template.
 *
 * @param t Template
 * @param purpose First purpose line
 */
void gnc_ab_trans_templ_set_purpose(GncABTransTempl *t, const gchar *purpose);

/**
 * Replace the second purpose line stored in a template.
 *
 * @param t Template
 * @param purpose_cont Second purpose line
 */
void gnc_ab_trans_templ_set_purpose_cont(GncABTransTempl *t,
        const gchar *purpose_cont);

G_END_DECLS

#endif /* GNC_AB_TRANS_TEMPL_H */
/** @} */
/** @} */
