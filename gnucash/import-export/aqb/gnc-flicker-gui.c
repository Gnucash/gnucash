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

#include <cairo.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-flicker-gui.h"

/* structured data for the flicker variables */
struct _flickerdraw {
	const char *challenge;
	guint maxWert;
    guint margin;           /* Abstand zwischen Balken */
    guint barwidth;         /* Balkenbreite */
    guint barheight;        /* Balkenhöhe */
    guint x_barpos;			/* x-Wert für die Position des Balken */
    guint y_barpos;			/* y-Wert für die Position des Balken */
    guint x_drawpos;		/* Erste Zeichenposition */
    guint y_drawpos;		/* Erste Zeichenposition */
    guint height;			/* Höhe der Zeichenfläche */
    guint width;			/* Breite der Zeichenfläche */
    guint delay;            /* Wartezeit zwischen Frames in Millisekunden */
    guint halfbyteid;
    guint clock;
    guint interval;
    gboolean change_interval;
} flickerdraw;

static GncFlickerGui *flickergui = NULL;
_Bool bitarray[100][5];

/* convert the bank challenge into the 5 bits for the flicker data */
char
*FlickerDaten (char const *challenge)
{

    /* bitfield: clock, bits 2^0, 2^1, 2^2, 2^3 */
    _Bool bits[16][5] = { {FALSE, FALSE, FALSE, FALSE, FALSE},  /* '0' */
                      {FALSE, TRUE, FALSE, FALSE, FALSE},       /* '1' */
                      {FALSE, FALSE, TRUE, FALSE, FALSE},       /* '2' */
                      {FALSE, TRUE, TRUE, FALSE, FALSE},        /* '3' */
                      {FALSE, FALSE, FALSE, TRUE, FALSE},       /* '4' */
                      {FALSE, TRUE, FALSE, TRUE, FALSE},        /* '5' */
                      {FALSE, FALSE, TRUE, TRUE, FALSE},        /* '6' */
                      {FALSE, TRUE, TRUE, TRUE, FALSE},         /* '7' */
                      {FALSE, FALSE, FALSE, FALSE, TRUE},       /* '8' */
                      {FALSE, TRUE, FALSE, FALSE, TRUE},        /* '9' */
                      {FALSE, FALSE, TRUE, FALSE, TRUE},        /* 'A' */
                      {FALSE, TRUE, TRUE, FALSE, TRUE},         /* 'B' */
                      {FALSE, FALSE, FALSE, TRUE, TRUE},        /* 'C' */
                      {FALSE, TRUE, FALSE, TRUE, TRUE},         /* 'D' */
                      {FALSE, FALSE, TRUE, TRUE, TRUE},         /* 'E' */
                      {FALSE, TRUE, TRUE, TRUE, TRUE}           /* 'F' */
    };

    /* prepend synchronization identifier */
    char *code = g_malloc (strlen (challenge) + 5);

    int i; int j;
    char *arr1 = g_malloc (2);
    char *arr2 = g_malloc (2);

    strcpy (code, "0FFF");
    strcat (code, challenge);
    code[strlen (code)+1] = '\0';

    for (i = 0; i < strlen (code); i += 2) {
        sprintf (arr1, "%c", code[i+1]);
		sprintf (arr2, "%c", code[i]);
        for (j = 0; j < 16; j++) {
            if (strtol(arr1, NULL, 16) == j) {
                bitarray[i][0] = bits[j][0];
                bitarray[i][1] = bits[j][1];
                bitarray[i][2] = bits[j][2];
                bitarray[i][3] = bits[j][3];
                bitarray[i][4] = bits[j][4];
			}
			if (strtol(arr2, NULL, 16) == j) {
                bitarray[i+1][0] = bits[j][0];
                bitarray[i+1][1] = bits[j][1];
                bitarray[i+1][2] = bits[j][2];
                bitarray[i+1][3] = bits[j][3];
                bitarray[i+1][4] = bits[j][4];
			}
		}
    }
    g_free (arr1);
    g_free (arr2);

    return code;
}

/* A clock is started here and called up again when the "Delay" value is changed */
gboolean
time_handler (GtkWidget *widget)
{
//	g_print("[Aufruf]: %s \n", __FUNCTION__ );

    /* Änderung der Wartezeit */
    if (flickerdraw.change_interval)
    {
        g_source_remove (flickerdraw.interval);
        flickerdraw.interval = g_timeout_add (flickerdraw.delay, (GSourceFunc) time_handler, (gpointer) widget);
        flickerdraw.change_interval = FALSE;
        return FALSE;
    }
    gtk_widget_queue_draw (widget);

    return TRUE;
}

/* Show the colored triangle as a pointer for the position of the TAN generator */
 void
 doMarkerDrawing (cairo_t *cr)
 {

	    guint pos1;
	    guint pos2;

	    /* Initialize the drawing area to black */
	    cairo_set_source_rgb (cr, 0, 0, 0);
	    cairo_paint (cr);

	    cairo_set_source_rgb (cr, 0.9, 0.1, 0.1);
	    /* Dreieck links */
	    pos1 = flickerdraw.x_drawpos+flickerdraw.barwidth/2;
	    cairo_move_to(cr, pos1, 20);
	    cairo_line_to(cr, pos1+10, 2);
	    cairo_line_to(cr, pos1-10, 2);
	    cairo_close_path(cr);
	    cairo_stroke_preserve(cr);
	    cairo_fill(cr);

	    /* Dreieck rechts */
	    pos2 = flickerdraw.x_drawpos+4*flickerdraw.margin+4*flickerdraw.barwidth+flickerdraw.barwidth/2;
	    cairo_move_to(cr, pos2, 20);
	    cairo_line_to(cr, pos2+10, 2);
	    cairo_line_to(cr, pos2-10, 2);
	    cairo_close_path(cr);
	    cairo_stroke_preserve(cr);
	    cairo_fill(cr);

 }

/* display the flicker graphic in the drawing area */
 void
 doFlickerDrawing (cairo_t *cr)
 {

	guint i;

    /* Initialize the drawing area to black */
    cairo_set_source_rgb (cr, 0, 0, 0);
    cairo_paint (cr);

    bitarray[flickerdraw.halfbyteid][0] = flickerdraw.clock;

    for ( i = 0; i <= 4; i++ ) {
    	if ( bitarray[flickerdraw.halfbyteid][i] == TRUE ) {
    		cairo_set_source_rgb(cr, 1, 1, 1); // white
    	}
    	else {
    		cairo_set_source_rgb(cr, 0, 0, 0); // black
    	}
    	flickerdraw.x_barpos = flickerdraw.x_drawpos + i*flickerdraw.margin + i*flickerdraw.barwidth;
    	cairo_rectangle (cr, flickerdraw.x_barpos, flickerdraw.y_barpos, flickerdraw.barwidth, flickerdraw.barheight);
    	cairo_fill (cr);
    }

/* Each flicker point is drawn twice. Once with clock = 1 and once with clock = 0 */
    if ( flickerdraw.clock == 0 ) {
        flickerdraw.clock = 1;
        flickerdraw.halfbyteid++;
        if ( flickerdraw.halfbyteid >= flickerdraw.maxWert ) {
            flickerdraw.halfbyteid = 0;
        }
    }
    else if ( flickerdraw.clock == 1 ) {
        flickerdraw.clock = 0;
    }

}

/* *************************************
*
* from here the signals of the GUI are processed
* sequence: ini_flicker_gui / on_flicker_map / time_handler / on_flicker_draw / time_handler / on_flicker_draw etc.
*
****************************************/

/* This signal is emitted when the drawing area "flicker marker" is visible */
void
on_flicker_marker_map (GtkWidget *widget)
{
    gtk_widget_set_size_request (widget, flickerdraw.width, flickerdraw.y_barpos);

}

/* This signal is emitted when the drawing area "flicker marker" is supposed to render itself */
gboolean
on_flicker_marker_draw (__attribute__((unused)) GtkWidget *widget, cairo_t *cr, __attribute__((unused)) gpointer data)
{
    doMarkerDrawing (cr);

    return FALSE;
}

/* This signal is emitted when the drawing area "flicker challenge" is visible */
void
on_flicker_challenge_map (GtkWidget *widget)
{

    gchar *code = g_malloc (strlen (flickerdraw.challenge)+4);
    code = FlickerDaten (flickerdraw.challenge);
    flickerdraw.maxWert = strlen(code);

    /* Todo:
     * Read the dialog size from a file and set the starting values ​​for the spin buttons.
     */

	flickerdraw.width = gtk_widget_get_allocated_width (widget);  /* Flexibel: Breite der ganzen Zeichenfläche */
	flickerdraw.height = flickerdraw.barheight + 2*flickerdraw.y_barpos;          		/* Fix: Höhe der Zeichenfläche */
    flickerdraw.x_drawpos = (flickerdraw.width - 4*flickerdraw.margin - 5*flickerdraw.barwidth)/2;		/* Flexibel: Erste Zeichenposition */

    gtk_widget_set_size_request (widget, flickerdraw.width, flickerdraw.height);

    /* Zeitfunktion aufrufen und Flickeranzeige starten */
    flickerdraw.interval = g_timeout_add (flickerdraw.delay, (GSourceFunc) time_handler, (gpointer) widget);

}

/* This signal is emitted when the drawing area "flicker_challenge" is supposed to render itself */
gboolean
on_flicker_challenge_draw (__attribute__((unused)) GtkWidget *widget, cairo_t *cr, __attribute__((unused)) gpointer user_data)
{
//	g_print("[Aufruf]: %s \n", __FUNCTION__ );
    doFlickerDrawing (cr);

    return FALSE;
}

/* called when drawing area is destroyed */
void
on_flicker_challenge_destroy (__attribute__((unused)) GtkWidget *widget, __attribute__((unused)) gpointer user_data)
{
    /* Todo:
     * Write the dialog size and the values ​​for the spin buttons to a file.
     */

    g_source_remove (flickerdraw.interval);

}

/* The value for "Field width" has been changed on the spin button and the flicker display is updated */
void
on_spin_Barwidth_value_changed (GtkSpinButton *spin, GtkWidget *widget)
{

	flickerdraw.barwidth = gtk_spin_button_get_value_as_int (spin);
	flickerdraw.x_drawpos = (flickerdraw.width - 4*flickerdraw.margin - 5*flickerdraw.barwidth)/2;		/* Flexibel: Erste Zeichenposition */

	/* Verschieben der Positionsdreiecke */
	gtk_widget_queue_draw (widget);

	/* Todo:
	 * after value changed set the focus on TAN input_entry
	 */
	//	gtk_widget_grab_focus (GTK_WIDGET (flickergui->input_entry));

}

/* Maus rechtsklick deaktivieren */
gboolean
on_spin_Barwidth_button_press_event (__attribute__((unused)) GtkWidget *widget, GdkEventButton *event, __attribute__((unused)) gpointer user_data)
{

    if (event->button == 3) {
        return TRUE;
    }
    return FALSE;

}

/* The value for "waiting time" was changed on the spin button and the flicker display is updated */
void
on_spin_Delay_value_changed (GtkSpinButton *spin, GtkWidget *widget)
{

    flickerdraw.delay = gtk_spin_button_get_value_as_int (spin);

    flickerdraw.change_interval = TRUE;
    time_handler (widget);

    /* Todo:
     * after value changed set the focus on TAN input_entry
     */
    //	gtk_widget_grab_focus (GTK_WIDGET (flickergui->input_entry));

}

/* Maus rechtsklick deaktivieren */
gboolean
on_spin_Delay_button_press_event (__attribute__((unused)) GtkWidget *widget, GdkEventButton *event, __attribute__((unused)) gpointer user_data)
{

    if (event->button == 3) {
        return TRUE;
    }
    return FALSE;

}

/* The widgets for the GUI are prepared and the first parameters are set  */
void
ini_flicker_gui (const char *pChallenge, GncFlickerGui *gui)
{
	gchar *tooltip_text;

    /* Bezug auf die im Hauptprogramm erstellten Dialog-Widgets */
    flickergui = gui;

	/* Anwendung initialisieren und Felder mit Werten vorbelegen */
	/* konstante Werte */
	flickerdraw.barheight = 200;    /* Konstant: Höhe der Balken */
	flickerdraw.y_barpos = 20;	    /* Kontant: Erste Zeichenposition */
	flickerdraw.margin = 12;        /* Konstant: horizontaler Abstand zwischen den Balken */
	flickerdraw.halfbyteid = 0;
	flickerdraw.clock = 1;

	/* Flexible Werte, einstellbar per GUI */
	flickerdraw.barwidth = 60;      /* Variabel, Eingabe per GUI: Breite der einzelnen Balken */
	flickerdraw.delay = 50;         /* Variabel, Eingabe per GUI: Verzögerungszeit zwischen dem Blinken */

    /* Globale Variablen mit Werten vorbelegen */
    flickerdraw.challenge = pChallenge;

    /* Todo:
	 * Make the dialog resizable.
	 */
    // gtk_window_set_resizable (GTK_WINDOW (flickergui->dialog), FALSE);

    gtk_widget_set_visible (GTK_WIDGET (flickergui->flicker_challenge), TRUE);
    g_signal_connect (GTK_WIDGET (flickergui->flicker_challenge), "map", G_CALLBACK (on_flicker_challenge_map), NULL);
    g_signal_connect (GTK_WIDGET (flickergui->flicker_challenge), "draw", G_CALLBACK (on_flicker_challenge_draw), NULL);
    g_signal_connect (GTK_WIDGET (flickergui->flicker_challenge), "destroy", G_CALLBACK (on_flicker_challenge_destroy), NULL);

    gtk_widget_set_visible (GTK_WIDGET (flickergui->flicker_marker), TRUE);
    g_signal_connect (GTK_WIDGET (flickergui->flicker_marker), "map", G_CALLBACK (on_flicker_marker_map), NULL);
    g_signal_connect (GTK_WIDGET (flickergui->flicker_marker), "draw", G_CALLBACK (on_flicker_marker_draw), NULL);

    gtk_widget_set_visible (GTK_WIDGET (flickergui->flicker_hbox), TRUE);

    flickergui->adj_Barwidth = gtk_adjustment_new (0.0, 10.0, 80.0, 1.0, 10.0, 0.0);
    gtk_spin_button_set_adjustment (flickergui->spin_Barwidth, flickergui->adj_Barwidth);
    gtk_spin_button_set_value (GTK_SPIN_BUTTON (flickergui->spin_Barwidth), flickerdraw.barwidth);
    g_signal_connect (GTK_WIDGET (flickergui->spin_Barwidth), "value-changed", G_CALLBACK (on_spin_Barwidth_value_changed), flickergui->flicker_marker);
    g_signal_connect (GTK_WIDGET (flickergui->spin_Barwidth), "button-press-event", G_CALLBACK (on_spin_Barwidth_button_press_event), NULL);
    gtk_widget_set_visible (GTK_WIDGET (flickergui->spin_Barwidth), TRUE);
    gtk_widget_set_focus_on_click (GTK_WIDGET (flickergui->spin_Barwidth), FALSE);
    tooltip_text = g_strdup (_("Setting the bar width, /n adapting to the size of the TAN generator"));
    gtk_widget_set_tooltip_text (GTK_WIDGET (flickergui->spin_Barwidth), tooltip_text);
    g_free(tooltip_text);

    flickergui->adj_Delay = gtk_adjustment_new (0.0, 10.0, 1000.0, 10.0, 10.0, 0.0);
    gtk_spin_button_set_adjustment (flickergui->spin_Delay, flickergui->adj_Delay);
    gtk_spin_button_set_value (GTK_SPIN_BUTTON (flickergui->spin_Delay), flickerdraw.delay);
    g_signal_connect (GTK_WIDGET (flickergui->spin_Delay), "value-changed", G_CALLBACK (on_spin_Delay_value_changed), flickergui->flicker_challenge);
    g_signal_connect (GTK_WIDGET (flickergui->spin_Delay), "button-press-event", G_CALLBACK (on_spin_Delay_button_press_event), NULL);
    gtk_widget_set_visible (GTK_WIDGET (flickergui->spin_Delay), TRUE);
    gtk_widget_set_focus_on_click (GTK_WIDGET (flickergui->spin_Delay), FALSE);
    tooltip_text = g_strdup (_("Setting the delay time, /n with small values the flicker graphic is repeated faster"));
    gtk_widget_set_tooltip_text (GTK_WIDGET (flickergui->spin_Delay), tooltip_text);
    g_free(tooltip_text);

    gtk_widget_grab_focus (GTK_WIDGET (flickergui->input_entry));

}
