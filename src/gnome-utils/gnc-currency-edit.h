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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

#ifndef GNC_CURRENCY_EDIT_H
#define GNC_CURRENCY_EDIT_H

#include <gnome.h>

#include "gnc-commodity.h"

#define GNC_TYPE_CURRENCY_EDIT	    (gnc_currency_edit_get_type())
#define GNC_CURRENCY_EDIT(o)	    (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_CURRENCY_EDIT, GNCCurrencyEdit))
#define GNC_CURRENCY_EDIT_CLASS(k)  (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_CURRENCY_EDIT, GNCCurrencyEditClass))
#define GNC_IS_CURRENCY_EDIT(o)	    (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_CURRENCY_EDIT))

typedef struct {
        GtkCombo combo;
} GNCCurrencyEdit;

typedef struct {
        GtkComboClass parent_class;
} GNCCurrencyEditClass;

GType		gnc_currency_edit_get_type	(void);

GtkWidget      *gnc_currency_edit_new		(void);

void		gnc_currency_edit_set_currency	(GNCCurrencyEdit *gce,
						 const gnc_commodity *currency);

gnc_commodity  *gnc_currency_edit_get_currency	(GNCCurrencyEdit *gce);

#endif

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
