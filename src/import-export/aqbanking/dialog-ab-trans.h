/*
 * dialog-ab-trans.h --
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
 * @file dialog-ab-trans.h
 * @brief Dialog for AqBanking transaction data
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2004 Bernd Wagner
 * @author Copyright (C) 2006 David Hampton <hampton@employees.org>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#ifndef DIALOG_AB_TRANS_H
#define DIALOG_AB_TRANS_H

#include <gtk/gtk.h>
#include <aqbanking/banking.h>

#include "Account.h"

G_BEGIN_DECLS

#define GNC_RESPONSE_NOW GTK_RESPONSE_YES
#define GNC_RESPONSE_LATER GTK_RESPONSE_NO

typedef struct _GncABTransDialog GncABTransDialog;

typedef enum _GncABTransType GncABTransType;
enum _GncABTransType
{
    SINGLE_TRANSFER = 0,
    SINGLE_DEBITNOTE,
    SINGLE_INTERNAL_TRANSFER
    , SEPA_TRANSFER
    , SEPA_DEBITNOTE
};
/**
  * Returns true if the given GncABTransType is an European (SEPA) transaction
  * (transfer or debit note), otherwise false.
  */
gboolean gnc_ab_trans_isSEPA(GncABTransType t);

/**
 * Create a new AqBanking transfer dialog
 *
 * @param parent Widget to use as parent, may be NULL
 * @param ab_acc Aqbanking account
 * @param commodity_scu commodity used for the amount
 * @param trans_type Type of transaction
 * @param templates A GList of template transactions which will become fully
 * managed by the dialog, so do not free it and retrieve snapshots via
 * gnc_ab_trans_dialog_get_templ()
 * @return A new GncABTransDialog, free with gnc_ab_trans_dialog_free()
 */
GncABTransDialog *gnc_ab_trans_dialog_new(GtkWidget *parent, AB_ACCOUNT *ab_acc,
        gint commodity_scu,
        GncABTransType trans_type,
        GList *templates);

/**
 * Run the Aqbanking transfer dialog until correct values where entered or
 * the user cancelled the dialog.
 *
 * @param td Transaction dialog
 * @param ab_acc AqBanking account
 * @return GTK_RESPONSE_CANCEL or GTK_RESPONSE_DESTROY_EVENT if the user cancelled the dialog
 * and GNC_RESPONSE_NOW otherwise.
 */
gint gnc_ab_trans_dialog_run_until_ok(GncABTransDialog *td);

/**
 * Free a Aqbanking transfer dialog
 *
 * @param td Transaction dialog
 */
void gnc_ab_trans_dialog_free(GncABTransDialog *td);

/**
 * Retrieve the current list of transaction templates from the dialog @a
 * td, unless @a changed is a specified location and the templates have
 * not been touched by the user.
 *
 * @param td Transaction dialog
 * @param changed Location to store whether the templates have been
 * changed, may be NULL
 * @return The a newly allocated list of the internal transaction
 * templates. Free this one via g_list_free().
 */
GList *gnc_ab_trans_dialog_get_templ(const GncABTransDialog *td,
                                     gboolean *changed);

/**
 * Retrieve the widget used as parent.
 *
 * @param td Transaction dialog
 * @return The parent
 */
GtkWidget *gnc_ab_trans_dialog_get_parent(const GncABTransDialog *td);

/**
 * Receive the Aqbanking Transaction filled by the dialog
 *
 * @param td Transaction dialog
 * @return An Aqbanking transaction
 */
const AB_TRANSACTION *gnc_ab_trans_dialog_get_ab_trans(
    const GncABTransDialog *td);

/**
 * Receive the Aqbanking job associated with the transaction
 *
 * @param td Transaction dialog
 * @return An Aqbanking job
 */
AB_JOB *gnc_ab_trans_dialog_get_job(const GncABTransDialog *td);

/**
 * FIXME
 *
 * @param td Transaction dialog
 * @return FIXME
 */
AB_JOB *gnc_ab_get_trans_job(AB_ACCOUNT *ab_acc, const AB_TRANSACTION *ab_trans,
                             GncABTransType trans_type);

G_END_DECLS

/** @} */
/** @} */

#endif /* DIALOG_AB_TRANS_H */
