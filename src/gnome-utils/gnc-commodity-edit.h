/*
 * gnc-commodity-edit.h -- Commodity editing widget
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

#ifndef __GNC_COMMODITY_EDIT_H_
#define __GNC_COMMODITY_EDIT_H_ 1

#include <gnome.h>

#include "gnc-commodity.h"

BEGIN_GNOME_DECLS


#define GNC_COMMODITY_EDIT(obj)          GTK_CHECK_CAST (obj, gnc_commodity_edit_get_type(), GNCCommodityEdit)
#define GNC_COMMODITY_EDIT_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, gnc_commodity_edit_get_type(), GNCCommodityEditClass)
#define GNC_IS_COMMODITY_EDIT(obj)       GTK_CHECK_TYPE (obj, gnc_commodity_edit_get_type ())

typedef struct {
        GtkHBox hbox;

        GtkWidget *entry;  /* display of commodity name */
        GtkWidget *button; /* button for popping up commodity window */

        gnc_commodity *selected_commodity;
} GNCCommodityEdit;

typedef struct {
        GtkHBoxClass parent_class;

        void (*changed) (GNCCommodityEdit *edit);
} GNCCommodityEditClass;

guint      gnc_commodity_edit_get_type       (void);

GtkWidget *gnc_commodity_edit_new            (void);

void       gnc_commodity_edit_set_commodity  (GNCCommodityEdit *gce,
                                              gnc_commodity *commodity);

gnc_commodity * gnc_commodity_edit_get_commodity (GNCCommodityEdit *gce);

END_GNOME_DECLS

#endif

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
