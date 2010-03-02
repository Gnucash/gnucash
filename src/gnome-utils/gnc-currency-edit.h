/*
 * gnc-currency-edit.h -- Currency editor widget
 *
 * Copyright (C) 2000 Free Software Foundation
 * All rights reserved.
 *
 * Dave Peticolas <dave@krondo.com>
 *
 * GnuCash is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 */

/** @addtogroup GUI
    @{ */
/** @addtogroup GncCurrencyEdit
 * @{ */
/** @file gnc-currency-edit.h
 *  @brief Currency selection widget.
 *  @author Dave Peticolas <dave@krondo.com>
 *  @author David Hampton <hampton@employees.org>
 *
 *  This widget is a GtkComboBox that is wrapped with support
 *  functions for building/selecting from a list of ISO4217 currency
 *  names.  All data is maintained within the widget itself, which
 *  makes the name/item lookup functions somewhat complicated.  The
 *  alternative coding would be to keep an auxiliary list of strings
 *  attacked to the widget for lookup purposes, but that would be 100%
 *  redundant information.
 *
 *  When the GtkComboCellEntry widget supports completion, this Gnucash
 *  widget should be modified so that it is based upon that widget.
 *  That would give users the capability to select a currency by typing
 *  its ISO 4217 code (e.g. USD, GBP, etc).  Moving to that widget
 *  today, however, would cause more problems that its worth.  There is
 *  currently no way to get access to the embedded GtkEntry widget, and
 *  therefore no way to implement completion in gnucash or prevent the
 *  user from typing in random data.
 */

#ifndef GNC_CURRENCY_EDIT_H
#define GNC_CURRENCY_EDIT_H

#include "gnc-commodity.h"

/** @name Basic Object Implementation */
/** @{ */

#define GNC_TYPE_CURRENCY_EDIT	    (gnc_currency_edit_get_type())
#define GNC_CURRENCY_EDIT(o)	    (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_CURRENCY_EDIT, GNCCurrencyEdit))
#define GNC_CURRENCY_EDIT_CLASS(k)  (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_CURRENCY_EDIT, GNCCurrencyEditClass))
#define GNC_IS_CURRENCY_EDIT(o)	    (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_CURRENCY_EDIT))

typedef struct
{
    GtkComboBoxEntry combobox;
} GNCCurrencyEdit;

typedef struct
{
    GtkComboBoxEntryClass combobox;
} GNCCurrencyEditClass;

/** Return the GType for the GNCCurrencyEdit currency selection widget.
 *
 *  @return A GType value.
 */
GType gnc_currency_edit_get_type (void);


/** Create a new GNCCurrencyEdit widget which can be used to provide
 *  an easy way to enter ISO currency codes.
 *
 *  @return A GNCCurrencyEdit widget.
 */
GtkWidget *gnc_currency_edit_new (void);
/** @} */


/** @name Get/Set Functions */
/** @{ */

/** Set the widget to display a certain currency name.
 *
 *  @param gce The currency editor widget to set.
 *
 *  @param currency The currency to set as the displayed/selected
 *  value of the widget.
 */
void gnc_currency_edit_set_currency (GNCCurrencyEdit *gce,
                                     const gnc_commodity *currency);


/** Retrieve the displayed currency of the widget.
 *
 *  @param gce The currency editor widget whose values should be retrieved.
 *
 *  @return A pointer to the selected currency (a gnc_commodity
 *  structure).
 */
gnc_commodity *gnc_currency_edit_get_currency (GNCCurrencyEdit *gce);

/** @} */

#endif

/** @} */
/** @} */

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
