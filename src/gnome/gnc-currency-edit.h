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

#ifndef __GNC_CURRENCY_EDIT_H_
#define __GNC_CURRENCY_EDIT_H_ 1

#include <gnome.h>

#include "gnc-commodity.h"

BEGIN_GNOME_DECLS


#define GNC_CURRENCY_EDIT(obj)          GTK_CHECK_CAST (obj, gnc_currency_edit_get_type(), GNCCurrencyEdit)
#define GNC_CURRENCY_EDIT_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, gnc_currency_edit_get_type(), GNCCurrencyEditClass)
#define GNC_IS_CURRENCY_EDIT(obj)       GTK_CHECK_TYPE (obj, gnc_currency_edit_get_type ())

typedef struct {
        GtkCombo combo;
} GNCCurrencyEdit;

typedef struct {
        GtkComboClass parent_class;
} GNCCurrencyEditClass;

guint      gnc_currency_edit_get_type       (void);

GtkWidget *gnc_currency_edit_new            (void);

void       gnc_currency_edit_set_currency   (GNCCurrencyEdit *gce,
                                             const gnc_commodity *currency);

const gnc_commodity * gnc_currency_edit_get_currency (GNCCurrencyEdit *gce);

END_GNOME_DECLS

#endif

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
