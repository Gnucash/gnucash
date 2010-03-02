/*
 * gnc-amount-edit.h -- amount editor widget
 *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>
 * All rights reserved.
 *
 * GnuCash is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
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
/*
  @NOTATION@
 */

#ifndef GNC_AMOUNT_EDIT_H
#define GNC_AMOUNT_EDIT_H

#include "qof.h"
#include "gnc-ui-util.h"

#define GNC_TYPE_AMOUNT_EDIT          (gnc_amount_edit_get_type())
#define GNC_AMOUNT_EDIT(obj)          G_TYPE_CHECK_INSTANCE_CAST (obj, GNC_TYPE_AMOUNT_EDIT, GNCAmountEdit)
#define GNC_AMOUNT_EDIT_CLASS(klass)  G_TYPE_CHECK_CLASS_CAST (klass, GNC_TYPE_AMOUNT_EDIT, GNCAmountEditClass)
#define GNC_IS_AMOUNT_EDIT(obj)       G_TYPE_CHECK_INSTANCE_TYPE (obj, GNC_TYPE_AMOUNT_EDIT)

typedef struct
{
    GtkEntry entry;

    gboolean need_to_parse;

    GNCPrintAmountInfo print_info;

    gnc_numeric amount;

    int fraction;

    gboolean evaluate_on_enter;

} GNCAmountEdit;

typedef struct
{
    GtkEntryClass parent_class;

    /* Signals for notification/filtering of changes */
    void (*amount_changed) (GNCAmountEdit *gae);
} GNCAmountEditClass;

GType     gnc_amount_edit_get_type        (void);

GtkWidget *gnc_amount_edit_new            (void);

GtkWidget *gnc_amount_edit_gtk_entry      (GNCAmountEdit *gae);

void      gnc_amount_edit_set_amount      (GNCAmountEdit *gae,
        gnc_numeric amount);
void      gnc_amount_edit_set_damount     (GNCAmountEdit *gae,
        double amount);

gnc_numeric gnc_amount_edit_get_amount    (GNCAmountEdit *gae);
double      gnc_amount_edit_get_damount   (GNCAmountEdit *gae);

gboolean  gnc_amount_edit_evaluate        (GNCAmountEdit *gae);

void      gnc_amount_edit_set_print_info  (GNCAmountEdit *gae,
        GNCPrintAmountInfo print_info);

void      gnc_amount_edit_set_fraction    (GNCAmountEdit *gae, int fraction);

void      gnc_amount_edit_set_evaluate_on_enter (GNCAmountEdit *gae,
        gboolean evaluate_on_enter);
#endif
