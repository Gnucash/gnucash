/********************************************************************\
 * gnc-gen-transaction.h -- window/dialog for generic transaction   *
 *                                                       importer   *
 * Copyright (C) 2002 Christian Stimming                            *
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
\********************************************************************/
/**@file
 * \brief Transaction matcher main window
 */
#ifndef GNC_GEN_TRANSACTION_H
#define GNC_GEN_TRANSACTION_H

#include <gnome.h>
#include "Transaction.h"

typedef struct _generic_transaction_info GNCGenTransaction;

/** Create a new generic transaction dialog window and return it. 
 *
 * @param parent The parent GtkWidget. May be NULL.
 * @param heading The heading label in the Importer window. May be NULL. */
GNCGenTransaction *gnc_gen_trans_new (GtkWidget *parent, 
				      const gchar* heading);

/** Deletes the given object. */
void gnc_gen_trans_delete (GNCGenTransaction *info);

/* Add a newly imported Transaction to the Transaction Importer.
 *
 * @param gui The Transaction Importer to use.
 * @param trans The Transaction to add. 
 */
void gnc_gen_trans_add_trans(GNCGenTransaction *gui, Transaction *trans);

/** Run this dialog and return only after the user pressed Ok, Cancel,
 * or closed the window. This means that all actual importing will
 * have been finished upon returning.
 */
gboolean gnc_gen_trans_run (GNCGenTransaction *info);

/** Freeze the underlying GtkCList. Do this before you add a lot of
 * transactions. */
void gnc_gen_trans_freeze (GNCGenTransaction *gui);

/** Thaw the underlying GtkCList. Do this after you added a lot of
 * transactions. */
void gnc_gen_trans_thaw (GNCGenTransaction *gui);

/** Set the allowed amount range for fuzzy amount matching. */
void gnc_gen_trans_set_fuzzy_amount (GNCGenTransaction *info, 
				     double fuzzy_amount);
/** Return the allowed amount range for fuzzy amount matching. */
double 
gnc_gen_trans_get_fuzzy_amount (const GNCGenTransaction *info);



#endif
