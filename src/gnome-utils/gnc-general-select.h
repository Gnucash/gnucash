/*
 * gnc-general-select.h -- General Selection Widget
 *
 * Copyright (C) 2001 Free Software Foundation
 * All rights reserved.
 *
 * Derek Atkins <warlord@MIT.EDU>
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
/*
  @NOTATION@
 */

#ifndef GNC_GENERAL_SELECT_H
#define GNC_GENERAL_SELECT_H

#define GNC_TYPE_GENERAL_SELECT          (gnc_general_select_get_type ())
#define GNC_GENERAL_SELECT(obj)          G_TYPE_CHECK_INSTANCE_CAST (obj, gnc_general_select_get_type(), GNCGeneralSelect)
#define GNC_GENERAL_SELECT_CLASS(klass)  G_TYPE_CHECK_CLASS_CAST (klass, gnc_general_select_get_type(), \ GNCGeneralSelectClass)
#define GNC_IS_GENERAL_SELECT(obj)       G_TYPE_CHECK_INSTANCE_TYPE (obj, gnc_general_select_get_type ())

typedef const char *	(*GNCGeneralSelectGetStringCB) (gpointer);
typedef gpointer 	(*GNCGeneralSelectNewSelectCB) (gpointer cbarg, gpointer default_selection, GtkWidget *parent);

typedef enum
{
    GNC_GENERAL_SELECT_TYPE_SELECT = 1,
    GNC_GENERAL_SELECT_TYPE_EDIT = 2,
    GNC_GENERAL_SELECT_TYPE_VIEW = 3
} GNCGeneralSelectType;

typedef struct
{
    GtkHBox hbox;

    GtkWidget *entry;  /* display of selection name */
    GtkWidget *button; /* button for popping up selection window */

    gpointer selected_item;

    GNCGeneralSelectGetStringCB	get_string;
    GNCGeneralSelectNewSelectCB	new_select;
    gpointer			cb_arg;

    int disposed; /* private */
} GNCGeneralSelect;

typedef struct
{
    GtkHBoxClass parent_class;

    void 		(*changed) (GNCGeneralSelect *edit);
} GNCGeneralSelectClass;


GtkWidget *gnc_general_select_new            (GNCGeneralSelectType type,
        GNCGeneralSelectGetStringCB get_string,
        GNCGeneralSelectNewSelectCB new_select,
        gpointer cb_arg);
void       gnc_general_select_set_selected   (GNCGeneralSelect *gsl,
        gpointer selected);
gpointer   gnc_general_select_get_selected   (GNCGeneralSelect *gsl);
const char *gnc_general_select_get_printname (GNCGeneralSelect *gsl,
        gpointer selection);
GType      gnc_general_select_get_type       (void);

void       gnc_general_select_make_mnemonic_target (GNCGeneralSelect *gsl, GtkWidget *label);

#endif

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
