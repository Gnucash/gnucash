/********************************************************************\
 * dialog-totd.c -- Dialog to display a "tip of the day"            *
 * Copyright (C) 2000 Robert Merkel <rgmerk@mira.net>               *
 * Large fractions borrowed from gnome-hint (Copyright (C) 2000)    *
 * Free Software Foundation                                         *
 *                                                                  *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gnome.h>

#include "dialog-totd.h"
#include "global-options.h"
#include "gnc-ui.h"
#include "messages.h"
#include "query-user.h"
#include "tip-of-the-day.h"


/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

static GtkWidget *win = NULL;
static GtkWidget *disable_cb = NULL;
static GtkWidget *canvas = NULL;
static GtkWidget *scrollwin = NULL;

static GnomeCanvasItem *hint_item;
static GnomeCanvasItem *blue_background;
static GnomeCanvasItem *white_background;
static gboolean old_enabled;

static int width = 400, height = 200;


/** Prototypes *********************************************************/
static void draw_on_canvas(GtkWidget *canvas, char *hint);
static void grow_text_if_necessary(void);
static GtkWidget *gnc_ui_totd_dialog_create(void);
static void totd_previous_cb(GtkWidget *widget, gpointer data);
static void totd_next_cb(GtkWidget * widget, gpointer data);
static void totd_close_cb(GtkWidget *widget, gpointer data);

/** Implementations ***************************************************/

/************************************************************************\
 * gnc_ui_totd_dialog_crfeate_and_run                                   *
 *   display and run the "Tip of the Day" dialog                        *
 *                                                                      *
 * Returns: nothing                                                     *
\************************************************************************/

void gnc_ui_totd_dialog_create_and_run(void)
{
  if(win == NULL)
  {
    gnc_ui_totd_dialog_create();
    gtk_widget_show_all(win);
  }
  else
  {
    gdk_window_raise(win->window);
  }
  return;
}

static GtkWidget *
gnc_ui_totd_dialog_create(void)
{
  char *new_hint;

  win = gnome_dialog_new(_("Tip of the Day"), 
			 GNOME_STOCK_BUTTON_PREV, 
			 GNOME_STOCK_BUTTON_NEXT, 
			 GNOME_STOCK_BUTTON_CLOSE, 
			 NULL);	

  gnome_dialog_set_parent(GNOME_DIALOG(win), GTK_WINDOW(gnc_get_ui_data()));

  gnome_dialog_set_default(GNOME_DIALOG(win), 2);
  gnome_dialog_close_hides(GNOME_DIALOG(win), FALSE);
			   
  scrollwin = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrollwin),
				 GTK_POLICY_NEVER,
				 GTK_POLICY_NEVER);
  gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(win)->vbox),
                     scrollwin, TRUE, TRUE, 0);
  canvas = gnome_canvas_new();
  gnome_canvas_set_scroll_region(GNOME_CANVAS(canvas),
  				 0.0,0.0,width,height);
  gtk_widget_set_usize(canvas,width,height);
  gtk_widget_ensure_style(canvas);
  gtk_container_add(GTK_CONTAINER(scrollwin), canvas);
  new_hint = gnc_get_current_tip();
  draw_on_canvas(canvas, new_hint);
  free(new_hint);
  gtk_widget_show_all(scrollwin);

  old_enabled = gnc_lookup_boolean_option("General",
					  "Display \"Tip of the Day\"",
					  TRUE);
  {
    const char *message = _("Display this dialog next time");
    disable_cb = gtk_check_button_new_with_label(message);
  }
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (disable_cb), old_enabled);

  gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(win)->vbox),
                     disable_cb, TRUE, TRUE, 0);
  gtk_widget_show(disable_cb);

  gnome_dialog_button_connect(GNOME_DIALOG(win), 0,
			      GTK_SIGNAL_FUNC(totd_previous_cb), NULL);
  gnome_dialog_button_connect(GNOME_DIALOG(win), 1,
			      GTK_SIGNAL_FUNC(totd_next_cb), NULL);
  gnome_dialog_button_connect(GNOME_DIALOG(win), 2,
			      GTK_SIGNAL_FUNC(totd_close_cb), NULL);
  gtk_signal_connect(GTK_OBJECT(win), "close",
		     GTK_SIGNAL_FUNC(totd_close_cb), NULL);

  return win;
}

static void
totd_previous_cb(GtkWidget *widget, gpointer data)
{
  char *new_hint;
  gnc_decrement_tip();
  new_hint = gnc_get_current_tip();
  gnome_canvas_item_set(hint_item,
			"text",new_hint,
			NULL);
  grow_text_if_necessary();
  free(new_hint);
  return;
}

static void
totd_next_cb(GtkWidget * widget, gpointer data)
{
  char *new_hint;
  gnc_increment_tip();
  new_hint = gnc_get_current_tip();
  gnome_canvas_item_set(hint_item,
			"text", new_hint, NULL);
  grow_text_if_necessary();
  free(new_hint);
  return;
}

static void
totd_close_cb(GtkWidget *widget, gpointer data)
{
  gboolean new_enabled =
    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(disable_cb));

  gnc_increment_tip();

  gtk_widget_destroy(GTK_WIDGET(win));
  win = NULL;
  if (new_enabled != old_enabled)
  {
    gnc_set_boolean_option("General", 
			   "Display \"Tip of the Day\"",
			   new_enabled);
    gnc_option_refresh_ui_by_name("General", "Display \"Tip of the Day\"");
  }
  return;
}


/* increases the size of the canvas and enables scrolling if the text
   gets big enough */

static void
grow_text_if_necessary(void)
{
  double w,h;
  int ww,hh;
  int changed = FALSE;

  gtk_object_get(GTK_OBJECT(hint_item),
		 "text_width",&w,
		 "text_height",&h,
		 NULL);
  /*add border, and 10 pixels around*/
  w+=75+10;
  h+=50+10;
  /*some sanity limits*/
  /*if(w>800) w = 800;
    if(h>600) h = 600;*/

  if(w>width) {
    width = w;
    changed = TRUE;
  }
  if(h>height) {
    height = h;
    changed = TRUE;
  }

  if(!changed)
    return;

  /*limits on size*/
  ww = width; hh = height;
  if(ww>720) ww = 720;
  if(hh>450) hh = 450;

  if(ww != width || hh != height)
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrollwin),
				   GTK_POLICY_AUTOMATIC,
				   GTK_POLICY_AUTOMATIC);
  else
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrollwin),
				   GTK_POLICY_NEVER,
				   GTK_POLICY_NEVER);

  /*here we grow the canvas*/
  gtk_widget_set_usize(canvas,ww,hh);
  gnome_canvas_set_scroll_region(GNOME_CANVAS(canvas),
				 0.0,0.0,width,height);

  gnome_canvas_item_set(blue_background,
			"x2",(double)width,
			"y2",(double)height,
			NULL);
  gnome_canvas_item_set(white_background,
			"x2",(double)width,
			"y2",(double)height,
			NULL);
  gnome_canvas_item_set(hint_item,
			"x",(double)(((width-75)/2)+75),
			"y",(double)(((height-50)/2)+50),
			"clip_width",(double)(width-75),
			"clip_height",(double)(height-50),
			NULL);
}

/* places items on the canvas to make up the tip of the day */

static void
draw_on_canvas(GtkWidget *canvas, char *hint)
{
  GnomeCanvasItem *item;

  blue_background = gnome_canvas_item_new(
    gnome_canvas_root(GNOME_CANVAS(canvas)),
    gnome_canvas_rect_get_type(),
    "x1",(double)0.0,
    "y1",(double)0.0,
    "x2",(double)400.0,
    "y2",(double)200.0,
    "fill_color","sea green",
    NULL);

  white_background = gnome_canvas_item_new(
    gnome_canvas_root(GNOME_CANVAS(canvas)),
    gnome_canvas_rect_get_type(),
    "x1",(double)75.0,
    "y1",(double)50.0,
    "x2",(double)400.0,
    "y2",(double)200.0,
    "fill_color","white",
    NULL);

  hint_item = gnome_canvas_item_new(
    gnome_canvas_root(GNOME_CANVAS(canvas)),
    gnome_canvas_text_get_type(),
    "x",(double)237.5,
    "y",(double)125.0,
    "fill_color","black",
    "font_gdk",canvas->style->font,
    "clip_width",(double)325.0,
    "clip_height",(double)150.0,
    "clip",TRUE,
    "text",hint,
    NULL);

  item = gnome_canvas_item_new(
    gnome_canvas_root(GNOME_CANVAS(canvas)),
    gnome_canvas_text_get_type(),
    "x",(double)200.0,
    "y",(double)25.0,
    "fill_color","white",
    "font",_("-*-helvetica-bold-r-normal-*-*-180-*-*-p-*-*-*"),
    "text",_("Tip of the Day:"),
    NULL);

  grow_text_if_necessary();
}
/********************** END OF FILE *********************************\
\********************************************************************/
