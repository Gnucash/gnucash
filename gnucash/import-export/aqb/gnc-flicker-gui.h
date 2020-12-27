/*
 * gnc-flicker-gui.h
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
 * @file gnc-flicker-gui.h
 * @brief GUI callbacks for Flicker and ChipTAN(optisch)
 * @author Copyright (C) 2020 Christian Wehling <christian.wehling@web.de>
 */

#ifndef GNC_FLICKER_GUI_H
#define GNC_FLICKER_GUI_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

// structured data for the GUI
typedef struct
{
    GtkWidget     *dialog;
    GtkWidget     *input_entry;
    GtkWidget     *flicker_challenge;
    GtkWidget     *flicker_marker;
    GtkWidget     *flicker_hbox;
    GtkAdjustment *adj_barwidth;
    GtkAdjustment *adj_delay;
    GtkSpinButton *spin_barwidth;
    GtkSpinButton *spin_delay;
} GncFlickerGui;

/**
 * Initialize the dialog and drawing area
 *
 * @param pChallenge: The answer from the bank which is shown as a flickering picture
 * @param gui: The structure of the Dialog-Widgets
 */
void ini_flicker_gui (const char *pChallenge, GncFlickerGui *gui);

G_END_DECLS

#endif /* GNC_FLICKER_GUI_H */
