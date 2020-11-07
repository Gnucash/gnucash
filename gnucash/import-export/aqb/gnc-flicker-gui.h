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
typedef struct _GncFlickerGui GncFlickerGui;

struct _GncFlickerGui
{
	GtkWidget *dialog;
	GtkWidget *input_entry;
	GtkWidget *flicker_challenge;
	GtkWidget *flicker_marker;
	GtkWidget *flicker_hbox;
	GtkAdjustment *adj_Barwidth;
	GtkAdjustment *adj_Delay;
	GtkSpinButton *spin_Barwidth;
	GtkSpinButton *spin_Delay;
};

/**
 * convert the bank challenge into the 5 bits for the flicker data
 *
 * @param challenge: the challenge from the bank
 * @return char: the Bank Challenge including the leading start sequence
 */
char *FlickerDaten (char const *challenge);

/**
 * A timer for redrawing the flickering painting,
 * which can be set by the value of the "Delay" spinbutton
 *
 * @param widget: the widget to redraw
 */
gboolean time_handler (GtkWidget *widget);

/**
 *  Show the colored triangle as a pointer for the position of the TAN generator
 *
 *  @param cr: Pointer to the Cairo
 */
void doMarkerDrawing (cairo_t *cr);

/**
 *  emitted when drawing area "flicker marker" is visible
 *  and initalize the drawing area
 *
 *  @param widget: the widget for the drawing
 */
void on_flicker_marker_map (GtkWidget *widget);

/**
 * Initialize the drawing area "flicker marker" in black and draw the marker for
 * the position of the TAN-Generator
 *
 * @param widget: The Widget which send the Callback
 * @param cr: Pointer to the Cairo
 * @param data: user_data
 * @return gboolean
 */
gboolean on_flicker_marker_draw (GtkWidget *widget, cairo_t *cr, gpointer user_data);

/**
 *  draws the 5 flickering bars of the bank data
 *
 *  @param cr: Pointer to the Cairo
 */
void doFlickerDrawing (cairo_t *cr);

/**
 * emitted when the drawing area "flicker challenge" is visible
 *
 * @param widget: The drawing area for the challenge
 */
void on_flicker_challenge_map (GtkWidget *widget);

/**
 * Initialize the drawingarea to black and paint the flickerchallenge
 * Call the function flickerStep for the flickering
 *
 * @param widget: The Widget which send the Callback
 * @param cr: Pointer to the Cairo
 * @param data: user_data
 * @return gboolean
 */
gboolean on_flicker_challenge_draw (GtkWidget *widget, cairo_t *cr, gpointer user_data);

/**
 * called when the drawing area is destroyed
 *
 * @param widget: The drawing area
 * @param user_data: The User data
 */
void on_flicker_challenge_destroy (GtkWidget *widget, gpointer user_data);

/**
 * The Adjustvalue for the Spinbutton "Barwidth"
 *
 * @param spin: The Spinbutton for the bar width
 * @param widget: The widget to redraw the marker for the TAN-Generator
 */
void on_spin_Barwidth_value_changed (GtkSpinButton *spin, GtkWidget *widget);

gboolean on_spin_Barwidth_button_press_event (GtkWidget *widget, GdkEventButton *event, gpointer user_data);

/**
 * The Adjustvalue for the Spinbutton "Delay"
 *
 * @param spin: The adjustment for the delay
 * @param widget: The widget to redraw the flicker_challenge
 */
void on_spin_Delay_value_changed (GtkSpinButton *spin, GtkWidget *widget);

gboolean on_spin_Delay_button_press_event (GtkWidget *widget, GdkEventButton *event, gpointer user_data);

/**
 * Initialize the dialog and drawing area
 *
 * @param pChallenge: The answer from the bank which is shown as a flickering picture
 * @param gui: The structure of the Dialog-Widgets
 */
void ini_flicker_gui (const char *pChallenge, GncFlickerGui *gui);

G_END_DECLS

#endif /* GNC_FLICKER_GUI_H */
