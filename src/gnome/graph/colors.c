#include <colors.h>

/* Is this approach OK for multi-headed displays, etc.? i.e. where
   different windows in the app might have different visuals?
   Probably not. */

static GdkColorContext *color_context = NULL;

static int
colors_init(static GtkWidget *main_window) {

  if(!color_context) {
    gint n;
    
    color_context =
      gdk_color_context_new(gtk_widget_get_visual(main_window),
                            gtk_widget_get_colormap(main_window));
    assert(color_context);
    
    color_split_row.pixel = 0; /* this is important! */
    color_split_row.red = 44730;
    color_split_row.green = 53052;
    color_split_row.blue = 65535;

    n = 0; /* this is important! */
    
    gdk_color_context_get_pixels (color_context,
                                  &color_split_row.red,
                                  &color_split_row.green,
                                  &color_split_row.blue,
                                  1, &color_split_row.pixel, &n);
    
    assert(n == 1);


    assert(gdk_color_parse("black", &color_black) != 0);
    color_black.pixel = 0; /* this is important! */
    n = 0; /* this is important! */
    
    gdk_color_context_get_pixels (color_context,
                                  &color_black.red,
                                  &color_black.green,
                                  &color_black.blue,
                                  1, &color_black.pixel, &n);
    
    assert(n == 1);


    assert(gdk_color_parse("red", &color_red) != 0);
    color_red.pixel = 0; /* this is important! */
    n = 0; /* this is important! */
    
    gdk_color_context_get_pixels (color_context,
                                  &color_red.red,
                                  &color_red.green,
                                  &color_red.blue,
                                  1, &color_red.pixel, &n);
    
    assert(n == 1);


    /*assert(gdk_color_parse("#", &color_split_row) != 0);*/
    
    /*main_row_color.red = 65000;
      main_row_color.green = 65000;
      main_row_color.blue = 65000;*/
    
    /*main_row_color.pixel = gdk_color_context_get_pixel(color_context,
      main_row_color.red,
      main_row_color.green,
      main_row_color.blue,
      &failure);
      assert(!failure); */
    
    /*

    color_context =
      gdk_color_context_new(gtk_widget_get_visual(main_window),
                            gtk_widget_get_colormap(main_window));


      color_split_row.red = 44730 >> 8;
      color_split_row.green = 53052 >> 8;
      color_split_row.blue = 65535 >> 8;
      
      color_split_row.red = 44730;
      color_split_row.green = 53052;
      color_split_row.blue = 65535;
      
      color_split_row.pixel = gdk_color_context_get_pixel(color_context,
      color_split_row.red,
      color_split_row.green,
      color_split_row.blue,
      &failure);
    */
  }
  return(0);
}
