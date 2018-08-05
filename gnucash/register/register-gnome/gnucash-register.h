/********************************************************************\
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNUCASH_REGISTER_H
#define GNUCASH_REGISTER_H

#include <gtk/gtk.h>
#include "split-register-model.h"
#include "table-allgui.h"
#include "gnucash-sheet.h"

/** @ingroup Register
 * @addtogroup Gnome
 * @{
 */
/** @file gnucash-sheet.h
 * @brief Public declarations of GnucashRegister class.
 */


#define GNUCASH_TYPE_REGISTER     (gnucash_register_get_type ())
#define GNUCASH_REGISTER(obj)     (G_TYPE_CHECK_INSTANCE_CAST((obj), GNUCASH_TYPE_REGISTER, GnucashRegister))
#define GNUCASH_REGISTER_CLASS(k) (G_TYPE_CHECK_CLASS_CAST ((k), GNUCASH_TYPE_REGISTER))
#define GNUCASH_IS_REGISTER(o)    (G_TYPE_CHECK_INSTANCE_TYPE((o), GNUCASH_TYPE_REGISTER))


typedef struct _GnucashRegister GnucashRegister;
typedef struct _GnucashRegisterClass GnucashRegisterClass;


GType gnucash_register_get_type (void);

/** this already has scrollbars attached */
GtkWidget *gnucash_register_new (Table *table, gchar *state_section);

void gnucash_register_goto_virt_cell (GnucashRegister *reg,
                                      VirtualCellLocation vcell_loc);

void gnucash_register_goto_virt_loc (GnucashRegister *reg,
                                     VirtualLocation virt_loc);

void gnucash_register_goto_next_virt_row (GnucashRegister *reg);

typedef gboolean (*VirtualLocationMatchFunc) (VirtualLocation virt_loc,
        gpointer user_data);

void gnucash_register_goto_next_matching_row (GnucashRegister *reg,
        VirtualLocationMatchFunc match,
        gpointer user_data);

void gnucash_register_attach_popup(GnucashRegister *reg, GtkWidget *popup,
                                   gpointer data);

gboolean gnucash_register_has_selection (GnucashRegister *reg);
void gnucash_register_cut_clipboard (GnucashRegister *reg);
void gnucash_register_copy_clipboard (GnucashRegister *reg);
void gnucash_register_paste_clipboard (GnucashRegister *reg);
void gnucash_register_refresh_from_prefs (GnucashRegister *reg);
void gnucash_register_set_moved_cb (GnucashRegister *reg,
                                    GFunc cb, gpointer cb_data);

GnucashSheet *gnucash_register_get_sheet (GnucashRegister *reg);
/** @} */
#endif
