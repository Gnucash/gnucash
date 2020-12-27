/*
 * gnc-flicker-gui.c --
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
 * @internal
 * @file gnc-flicker-gui.c
 * @brief GUI callbacks for Flicker and ChipTAN(optisch)
 * @author Copyright (C) 2020 Christian Wehling <christian.wehling@web.de>
 */

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-flicker-gui.h"
#include "gnc-state.h"
#include "gnc-ui.h"

#define GNC_PREFS_GROUP     "dialogs.flicker"
#define GNC_STATE_SECTION   "Flicker"
#define STATE_KEY_BAR_WIDTH "barwidth"
#define STATE_KEY_DELAY     "delay"

#define BAR_WIDTH  44    /* Width of the flicker bars */
#define BAR_HEIGHT 200   /* Height of the flicker bars */
#define MARGIN     12    /* Distance between the flicker bars */
#define DELAY      50    /* Pause between the flickering painting */

static guint flicker_data (char const *challenge);
static gboolean time_handler (GtkWidget *widget);
static void do_marker_drawing (cairo_t *cr);
static void draw_bit (cairo_t *cr, _Bool bit, int i);
static void do_flicker_drawing (GtkWidget *widget, cairo_t *cr);
static void do_flicker_load_state (GtkWidget *dialog);
static void do_flicker_store_state (GtkWidget *dialog);
static gboolean on_flicker_challenge_draw (GtkWidget *widget, cairo_t *cr,
                                           gpointer user_data);
static void on_flicker_challenge_map (GtkWidget *widget);
static void on_flicker_challenge_destroy (GtkWidget *widget, gpointer user_data);
static gboolean on_flicker_marker_draw (GtkWidget *widget, cairo_t *cr,
                                        gpointer user_data);
static void on_flicker_marker_map (GtkWidget *widget);
static void on_spin_barwidth_value_changed (GtkSpinButton *spin, GtkWidget *widget);
static void on_spin_delay_value_changed (GtkSpinButton *spin, GtkWidget *widget);
static void on_dialog_destroy (GtkWidget *dialog, gpointer user_data);

/* structured data for the flicker variables */
struct Flickerdraw {
    const char *challenge;
    guint challenge_length;
    guint margin;           /* Distance between bars */
    guint barwidth;         /* Bar width */
    guint barheight;        /* Bar height */
    guint x_barpos;         /* x-value for the position of the bar */
    guint y_barpos;         /* y-value for the position of the bar */
    guint x_drawpos;        /* x-value of the first painting position */
    guint y_drawpos;        /* y-value of the first painting position */
    guint height;           /* Height of the drawing area */
    guint width;            /* Width of the drawing area */
    guint delay;            /* Waiting time between frames in milliseconds */
    guint halfbyteid;
    guint clock;
    guint interval;
    gboolean change_interval;
};
static struct Flickerdraw flickerdraw;

static GncFlickerGui *flickergui = NULL;
static _Bool bitarray[255][5];

/* this function will return number corresponding 0,1,2..,9,A,B,C,D,E,F */
static guint
get_num (const char ch)
{
    int num =0;
    if (ch >= '0' && ch <= '9')
        num = ch - 0x30;
    else
    {
        switch (ch)
        {
            case 'A': case 'a': num = 10; break;
            case 'B': case 'b': num = 11; break;
            case 'C': case 'c': num = 12; break;
            case 'D': case 'd': num = 13; break;
            case 'E': case 'e': num = 14; break;
            case 'F': case 'f': num = 15; break;
            default: num = 0;
        }
    }
    /* The bank challenge has been verified by Aqbanking,
     * so I assume that no error can occur. */
    return num;
}

/* convert the bank challenge into the 5 bits for the flicker data */
static guint
flicker_data (const char *challenge)
{
    /* bitfield is a clock bit and a 4-bit code with the bits reversed
       (bit 1 is the least significant and bit 4 the most
       so 0x1 is 1000 and 0x8 is 0001) */
    static const _Bool bits[16][5] =
    {
        {0, 0, 0, 0, 0}, {0, 1, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 1, 1, 0, 0},
        {0, 0, 0, 1, 0}, {0, 1, 0, 1, 0}, {0, 0, 1, 1, 0}, {0, 1, 1, 1, 0},
        {0, 0, 0, 0, 1}, {0, 1, 0, 0, 1}, {0, 0, 1, 0, 1}, {0, 1, 1, 0, 1},
        {0, 0, 0, 1, 1}, {0, 1, 0, 1, 1}, {0, 0, 1, 1, 1}, {0, 1, 1, 1, 1}
    };

    /* prepend synchronization identifier */
    char* code = g_strdup_printf ("0FFF%s", challenge);
    guint challenge_length = strlen (code);

    /* Swap the position of the bits in pairs throughout the bank challenge
       (low-order nibble first). */
    for (guint i = 0; i < challenge_length; i += 2)
    {
        guint val1 = get_num (code[i]);
        guint val2 = get_num (code[i+1]);

        memcpy (&bitarray[i], bits[val2], sizeof(bits[val2]));
        memcpy (&bitarray[i+1], bits[val1], sizeof(bits[val1]));
    }
    g_free (code);

    return challenge_length;
}

/* A timer for redrawing the flickering painting, is started here and
 * called up again when the "Delay" value is changed */
static gboolean
time_handler (GtkWidget *widget)
{
    /* Change of waiting time */
    if (flickerdraw.change_interval)
    {
        g_source_remove (flickerdraw.interval);
        flickerdraw.interval = g_timeout_add (flickerdraw.delay,
                                              (GSourceFunc) time_handler,
                                              (gpointer) widget);
        flickerdraw.change_interval = FALSE;
        return FALSE;
    }
    gtk_widget_queue_draw (widget);

    return TRUE;
}

/* Show the colored triangle as a pointer for the position of the TAN generator */
static void
do_marker_drawing (cairo_t *cr)
{
    guint pos1;
    guint pos2;

    /* Initialize the drawing area to black */
    cairo_set_source_rgb (cr, 0, 0, 0);
    cairo_paint (cr);

    cairo_set_source_rgb (cr, 0.9, 0.1, 0.1);
    /* draw the left triangle */
    pos1 = flickerdraw.x_drawpos + flickerdraw.barwidth / 2;
    cairo_move_to (cr, pos1, 20);
    cairo_line_to (cr, pos1 + 10, 2);
    cairo_line_to (cr, pos1 - 10, 2);
    cairo_close_path (cr);
    cairo_stroke_preserve (cr);
    cairo_fill (cr);

    /* draw the right triangle */
    pos2 = flickerdraw.x_drawpos + 4 * flickerdraw.margin + 4 * flickerdraw.barwidth +
           flickerdraw.barwidth / 2;
    cairo_move_to (cr, pos2, 20);
    cairo_line_to (cr, pos2 + 10, 2);
    cairo_line_to (cr, pos2 - 10, 2);
    cairo_close_path (cr);
    cairo_stroke_preserve (cr);
    cairo_fill (cr);
}

/* draws the 5 flickering bars of the bank data into the drawing area */
static void
draw_bit (cairo_t *cr, _Bool bit, int i)
{
    if (bit & 1)
        cairo_set_source_rgb (cr, 1, 1, 1);
    else
        cairo_set_source_rgb (cr, 0, 0, 0);

    flickerdraw.x_barpos = flickerdraw.x_drawpos + i * flickerdraw.margin +
                           i * flickerdraw.barwidth;
    cairo_rectangle (cr, flickerdraw.x_barpos, flickerdraw.y_barpos,
                     flickerdraw.barwidth, flickerdraw.barheight);
    cairo_fill (cr);
}

/* Prepares the drawing area for the flicker graphic. */
static void
do_flicker_drawing (GtkWidget *widget, cairo_t *cr)
{
    /* Always align the flicker display in the middle of the drawing area */
    flickerdraw.width = gtk_widget_get_allocated_width (widget);

    /* Start position of the first bar */
    flickerdraw.x_drawpos = (flickerdraw.width - 4 * flickerdraw.margin -
                             5 * flickerdraw.barwidth) / 2;

    /* Initialize the drawing area to black */
    cairo_set_source_rgb (cr, 0, 0, 0);
    cairo_paint (cr);

    /* paint the flicker graphic */
    bitarray[flickerdraw.halfbyteid][0] = flickerdraw.clock;
    draw_bit (cr, flickerdraw.clock, 0);
    for (int i = 1; i <= 4; i++)
        draw_bit (cr, bitarray[flickerdraw.halfbyteid][i], i);

    /* Each flicker point is drawn twice. Once with clock = 1 and once with clock = 0 */
    if (!flickerdraw.clock)
    {
        flickerdraw.clock = 1;
        flickerdraw.halfbyteid++;
        if (flickerdraw.halfbyteid >= flickerdraw.challenge_length)
            flickerdraw.halfbyteid = 0;
    }
    else if (flickerdraw.clock)
        flickerdraw.clock = 0;
}

/* Load the state of the GUI (Size of the Dialog, Value of the Spinbutton)  */
static void
do_flicker_load_state (GtkWidget *dialog)
{
    /* Load the values in the spin button */
    GKeyFile *state_file = gnc_state_get_current ();

    if (g_key_file_has_key (state_file, GNC_STATE_SECTION, STATE_KEY_BAR_WIDTH, NULL))
        flickerdraw.barwidth = g_key_file_get_integer (state_file,
                                                       GNC_STATE_SECTION,
                                                       STATE_KEY_BAR_WIDTH, NULL);
    else
        flickerdraw.barwidth = BAR_WIDTH;

    if (g_key_file_has_key (state_file, GNC_STATE_SECTION, STATE_KEY_DELAY, NULL))
        flickerdraw.delay = g_key_file_get_integer (state_file,
                                                    GNC_STATE_SECTION,
                                                    STATE_KEY_DELAY, NULL);
    else
        flickerdraw.delay = DELAY;

    /* Load window size and position */
    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW (dialog),
                             gnc_ui_get_main_window (NULL));
}

/* Stores the state of the GUI (Size of the Dialog, Value of the Spinbutton) */
static void
do_flicker_store_state (GtkWidget *dialog)
{
    /* Save the values in the spin button */
    GKeyFile *state_file = gnc_state_get_current ();

    if (flickerdraw.barwidth != BAR_WIDTH)
        g_key_file_set_integer (state_file, GNC_STATE_SECTION,
                                STATE_KEY_BAR_WIDTH, flickerdraw.barwidth);
    else if (g_key_file_has_key (state_file, GNC_STATE_SECTION,
                                 STATE_KEY_BAR_WIDTH, NULL))
        g_key_file_remove_key (state_file, GNC_STATE_SECTION,
                               STATE_KEY_BAR_WIDTH, NULL);

    if (flickerdraw.delay != DELAY)
        g_key_file_set_integer (state_file, GNC_STATE_SECTION,
                                STATE_KEY_DELAY, flickerdraw.delay);
    else if (g_key_file_has_key (state_file, GNC_STATE_SECTION,
                                 STATE_KEY_DELAY, NULL))
        g_key_file_remove_key (state_file, GNC_STATE_SECTION,
                               STATE_KEY_DELAY, NULL);

    /* Save window size and position */
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW (dialog));
}

/* This signal is emitted when the drawing area "flicker challenge" is visible */
static void
on_flicker_challenge_map (GtkWidget *widget)
{
    flickerdraw.challenge_length = flicker_data (flickerdraw.challenge);

    /* Set the height of the drawing area */
    flickerdraw.height = flickerdraw.barheight + 2 * flickerdraw.y_barpos;
    gtk_widget_set_size_request (widget, -1, flickerdraw.height);

    /* Call up the time function and start the flicker display */
    flickerdraw.interval = g_timeout_add (flickerdraw.delay,
                                          (GSourceFunc) time_handler,
                                          (gpointer) widget);
}

/* Initialize the drawingarea to black and paint the flickerchallenge */
static gboolean
on_flicker_challenge_draw (GtkWidget *widget, cairo_t *cr,
                           __attribute__((unused)) gpointer user_data)
{
    do_flicker_drawing (widget, cr);

    return FALSE;
}

/* called when the drawing area is destroyed */
static void
on_flicker_challenge_destroy (GtkWidget *widget,
                              __attribute__((unused)) gpointer user_data)
{
    /* remove the timeout function */
    g_source_remove (flickerdraw.interval);
}

/* Initialize the drawing area "flicker marker" in black and draw the marker for
 * the position of the TAN-Generator */
static gboolean
on_flicker_marker_draw (__attribute__((unused)) GtkWidget *widget, cairo_t *cr,
                        __attribute__((unused)) gpointer data)
{
    do_marker_drawing (cr);

    return FALSE;
}

/* This signal is emitted when the drawing area "flicker marker" is visible */
static void
on_flicker_marker_map (GtkWidget *widget)
{
    /* Set the height of the drawing area */
    gtk_widget_set_size_request (widget, -1, flickerdraw.y_barpos);
}

/* The value for "Field width" has been changed on the spin button and the
 * flicker display is updated */
static void
on_spin_barwidth_value_changed (GtkSpinButton *spin, GtkWidget *widget)
{
    flickerdraw.barwidth = gtk_spin_button_get_value_as_int (spin);
    flickerdraw.x_drawpos = (flickerdraw.width - 4 * flickerdraw.margin -
                             5 * flickerdraw.barwidth) / 2;

    /* Moving the position triangles */
    gtk_widget_queue_draw (widget);
}

/* The value for "waiting time" was changed on the spin button and
 * the speed of the flickering display is updated */
static void
on_spin_delay_value_changed (GtkSpinButton *spin, GtkWidget *widget)
{
    flickerdraw.delay = gtk_spin_button_get_value_as_int (spin);

    flickerdraw.change_interval = TRUE;
    time_handler (widget);
}

static void
on_dialog_destroy (GtkWidget *dialog, __attribute__((unused)) gpointer user_data)
{
    /* Store window size and initial setting values */
    do_flicker_store_state (dialog);
}

/* The widgets for the GUI are prepared and the first parameters are set  */
void
ini_flicker_gui (const char *pChallenge, GncFlickerGui *gui)
{
    /* Establish reference to the dialog widgets created in gnc_gwen_gui */
    flickergui = gui;

    /* Load window size and initial setting values */
    do_flicker_load_state (GTK_WIDGET (flickergui->dialog));

    /* Initialize application */
    flickerdraw.barheight = BAR_HEIGHT;
    flickerdraw.margin = MARGIN;
    flickerdraw.y_barpos = 20;        /* First painting position */
    flickerdraw.halfbyteid = 0;
    flickerdraw.clock = 1;

    flickerdraw.challenge = pChallenge;

    g_signal_connect (GTK_WINDOW (flickergui->dialog), "destroy",
                      G_CALLBACK (on_dialog_destroy), NULL);

    g_signal_connect (GTK_WIDGET (flickergui->flicker_challenge), "map",
                      G_CALLBACK (on_flicker_challenge_map), NULL);
    g_signal_connect (GTK_WIDGET (flickergui->flicker_challenge), "draw",
                      G_CALLBACK (on_flicker_challenge_draw), NULL);
    g_signal_connect (GTK_WIDGET (flickergui->flicker_challenge), "destroy",
                      G_CALLBACK (on_flicker_challenge_destroy), NULL);

    g_signal_connect (GTK_WIDGET (flickergui->flicker_marker), "map",
                      G_CALLBACK (on_flicker_marker_map), NULL);
    g_signal_connect (GTK_WIDGET (flickergui->flicker_marker), "draw",
                      G_CALLBACK (on_flicker_marker_draw), NULL);

    flickergui->adj_barwidth = gtk_adjustment_new (0.0, 10.0, 80.0, 1.0, 10.0, 0.0);
    gtk_spin_button_set_adjustment (flickergui->spin_barwidth, flickergui->adj_barwidth);
    gtk_spin_button_set_value (GTK_SPIN_BUTTON (flickergui->spin_barwidth),
                               flickerdraw.barwidth);
    g_signal_connect (GTK_WIDGET (flickergui->spin_barwidth), "value-changed",
                      G_CALLBACK (on_spin_barwidth_value_changed),
                                 flickergui->flicker_marker);
    gtk_widget_set_focus_on_click (GTK_WIDGET (flickergui->spin_barwidth), FALSE);

    flickergui->adj_delay = gtk_adjustment_new (0.0, 10.0, 100.0, 10.0, 10.0, 0.0);
    gtk_spin_button_set_adjustment (flickergui->spin_delay, flickergui->adj_delay);
    gtk_spin_button_set_value (GTK_SPIN_BUTTON (flickergui->spin_delay),
                               flickerdraw.delay);
    g_signal_connect (GTK_WIDGET (flickergui->spin_delay), "value-changed",
                      G_CALLBACK (on_spin_delay_value_changed),
                      flickergui->flicker_challenge);
    gtk_widget_set_focus_on_click (GTK_WIDGET (flickergui->spin_delay), FALSE);

    gtk_widget_grab_focus (GTK_WIDGET (flickergui->input_entry));
}
