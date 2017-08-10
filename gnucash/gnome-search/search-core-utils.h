/*
 * search-core-utils.h -- common functions for search code
 * Copyright (C) 2006 David Hampton <hampton@employees.org>
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
 *
 */

/* Functions for creating the common two column GtkComboBox used by
 * all the search widgets. */

enum gnc_combo_search_cols
{
    GNC_COMBO_SEARCH_COL_TEXT = 0,
    GNC_COMBO_SEARCH_COL_VALUE,
    NUM_GNC_COMBO_SEARCH_COLS,
};

GtkWidget *gnc_combo_box_new_search (void);
void gnc_combo_box_search_add (GtkComboBox *combo, const gchar *text, guint value);
guint gnc_combo_box_search_get_active (GtkComboBox *combo);
void gnc_combo_box_search_set_active (GtkComboBox *combo, guint value);
void gnc_combo_box_search_changed(GtkComboBox *combo, guint *value);

