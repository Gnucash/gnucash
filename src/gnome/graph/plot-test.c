/* File: plot-test.c */

/* Eventually the graphs should draw to a pixmap and refresh from
   there.  This is *easy* to do in GTK.  */

#include <stdio.h>
#include <assert.h>
#include <getopt.h>
#include <gtk/gtk.h>
#include <gdk/gdkprivate.h>
#include <plot.h>

#include <Account.h>
#include <LedgerUtils.h>
#include <FileIO.h>

#include <plot.h>

static char *filename = NULL;

void
destroy() {
  gtk_main_quit ();
}

typedef struct {
  char *label;
  double value;
} PieChartItem;

static const PieChartItem item1 = {"horses", 121.32};
static const PieChartItem item2 = {"throbbing expanse", 350.19};
static const PieChartItem item3 = {"tangibles", 23.32};
static const PieChartItem item4 = {"intangibles", 45.44};
static const PieChartItem item5 = {"giant fungi", 241.87};

static const PieChartItem *pie_data[] = {
  &item1,
  &item2,
  &item3,
  &item4,
  &item5,
  0
};

static double
pie_chart_slice(GtkWidget *w,
                GdkColor *color,
                const int width,
                const int height,
                const int center_x,
                const int center_y,
                const double total,
                const double start_angle,
                const PieChartItem *item) {

  GdkGC *gc = NULL;
  const int pie_width = (2 * width) / 3;
  const int pie_height = (2 * width) / 3;
  const double full_circle = 360 * 64;
  const double arc_sweep = (item->value / total) * full_circle;

  gc = gdk_gc_new(GTK_WIDGET(w)->window);
  gdk_gc_copy(gc, GTK_WIDGET(w)->style->black_gc);
  gdk_gc_set_foreground(gc, color);

  gdk_draw_arc(w->window, gc, TRUE,
               center_x - pie_width / 2,
               center_y - pie_width / 2,
               pie_width, pie_height,
               rint(start_angle), rint(arc_sweep));

  gdk_gc_unref(gc);
  return arc_sweep;
}

static void
pie_chart(GtkWidget *w, const PieChartItem *pie_chart_items[]) {
  if(pie_chart_items) {
    const PieChartItem **item = pie_chart_items;
    double total = 0;
    double current_angle = 0;
    int current_color = 0;

    /* this is going to be handled through guile eventually */
    const char *pie_color_strings[] = {
      "DarkGreen",
      "FireBrick",
      "DarkBlue",
      "DarkOliveGreen",
      "DarkOrange",
      "MediumSeaGreen",
      "peru",
      "DarkOrchid",
      "LimeGreen"
    };

    GdkColorContext *color_context =
      gdk_color_context_new(gtk_widget_get_visual(w),
                            gtk_widget_get_colormap(w));
    assert(color_context);

    /* get total */
    while(*item) {
      fprintf(stderr, "%s\n", (*item)->label);
      total += (*item)->value;
      item++;
    }

    item = pie_chart_items;
    while(*item) {
      /* This is a *really* inefficient way to handle the colors, but
         it's guaranteed safe.  We can do something smarter later, but
         it may not be worth worrying about. */
      GdkColor color;
      int n;
      const int width = 400;
      const int height = 400;
      const int center_x = 200;
      const int center_y = 200;

      assert(gdk_color_parse(pie_color_strings[current_color], &color) != 0);
      color.pixel = 0; /* this is important! */
      n = 0;           /* this is important! */
      gdk_color_context_get_pixels (color_context,
                                    &color.red,
                                    &color.green,
                                    &color.blue,
                                    1, &color.pixel, &n);
      assert(n == 1);
      
      current_angle += 
        pie_chart_slice(w, &color, width, height, center_x, center_y,
                        total, current_angle, *item);
      current_color++;
      if(current_color == sizeof(pie_color_strings) / sizeof(char *))
        current_color = 0;
      item++;
    }
    gdk_color_context_free(color_context);
  }
}

static double
pie_chart_slice_plotutils(const char color[],
                          const int width,
                          const int height,
                          const int center_x,
                          const int center_y,
                          const double total,
                          const double start_angle,
                          const PieChartItem *item) {

  const int pie_radius = width / 3;
  const double full_circle = 2 * M_PI;
  const double arc_sweep = (item->value / total) * full_circle;

  savestate();
  filltype(1);
  colorname(color);
  move(0,0);

  savestate();
  frotate(start_angle * 180 / M_PI);
  cont(pie_radius, 0); 
  arc(0, 0,
      pie_radius, 0,
      pie_radius * cos(arc_sweep),
      pie_radius * sin(arc_sweep)); 
  cont(0,0);
  endpath();
  
  restorestate();
  
  move(0.90 * width / 2 * cos(start_angle + arc_sweep / 2),
       0.90 * width / 2 * sin(start_angle + arc_sweep / 2)); 
  
  alabel('c', 'c', item->label);

  restorestate();

  fprintf(stderr, "start: %f  sweep: %f\n", start_angle, arc_sweep);
  return arc_sweep;
}

static void
pie_plotutils(const PieChartItem *pie_chart_items[]) {
  if(pie_chart_items) {
    const PieChartItem **item = pie_chart_items;
    double total = 0;
    double current_angle = 0;
    int current_color = 0;

    /* this is going to be handled through guile eventually */
    const char *pie_color_strings[] = {
      "DarkGreen",
      "FireBrick",
      "DarkBlue",
      "DarkOliveGreen",
      "DarkOrange",
      "MediumSeaGreen",
      "peru",
      "DarkOrchid",
      "LimeGreen",
      0
    };

    /* get total */
    while(*item) {
      fprintf(stderr, "%s\n", (*item)->label);
      total += (*item)->value;
      item++;
    }

    item = pie_chart_items;
    while(*item) {
      /* This is a *really* inefficient way to handle the colors, but
         it's guaranteed safe.  We can do something smarter later, but
         it may not be worth worrying about. */
      const int width = 400;
      const int height = 400;
      const int center_x = 200;
      const int center_y = 200;

      current_angle += 
        pie_chart_slice_plotutils(pie_color_strings[current_color],
                                  width, height, center_x, center_y,
                                  total, current_angle, *item);
      current_color++;
      if(!pie_color_strings[current_color]) current_color = 0;
      item++;
    }
  }  
}

static void
pie_window_expose(GtkDrawingArea *da, GdkEventExpose *event, gpointer data) {
  //fprintf(stderr, "%p\n", data);
  //pie_chart(GTK_WIDGET(da), data);

  {
    int handle;
    GdkWindowPrivate *priv = (GdkWindowPrivate *)(GTK_WIDGET(da)->window);

    assert(parampl("XDRAWABLE_DISPLAY", priv->xdisplay) == 0);
    assert(parampl("XDRAWABLE_DRAWABLE1", &(priv->xwindow)) == 0);
    //assert(parampl("XDRAWABLE_DRAWABLE2", &(priv->xwindow)) == 0);
    assert(parampl("XDRAWABLE_DRAWABLE2", NULL) == 0);
    handle = newpl("Xdrawable", stdin, stdout, stderr);
    assert(handle);
    selectpl(handle);
    assert(openpl() == 0);
    space(-200, -200, 200, 200);
    colorname("grey83");
    box(-200, -200, 200, 200);
    pie_plotutils(pie_data);
    assert(closepl() == 0);
    selectpl(0);
    deletepl(handle);
  }
}

static GtkWidget *
pie_window() {
  GtkWidget *window;
  GtkWidget *vbox;
  GtkWidget *quit_button;
  GtkWidget *da;

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  gtk_signal_connect (GTK_OBJECT (window), "destroy",
		      GTK_SIGNAL_FUNC (destroy), NULL);
  gtk_container_border_width (GTK_CONTAINER (window), 3);

  quit_button = gtk_button_new_with_label ("Quit");
  gtk_signal_connect_object (GTK_OBJECT (quit_button), "clicked",
			     GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (window));

  vbox = gtk_vbox_new(FALSE, 0);

  da = gtk_drawing_area_new();
  gtk_drawing_area_size(GTK_DRAWING_AREA(da), 400,400);
  gtk_box_pack_start(GTK_BOX(vbox), da, FALSE, FALSE, 0);
  gtk_widget_show(da);

  gtk_signal_connect(GTK_OBJECT (da),
                     "expose_event",
                     GTK_SIGNAL_FUNC (pie_window_expose),
                     pie_data);
  gtk_widget_set_events (da, GDK_EXPOSURE_MASK);

  gtk_container_add (GTK_CONTAINER(window), vbox);
  gtk_box_pack_end(GTK_BOX(vbox), quit_button, FALSE, FALSE, 0);

  gtk_widget_show (quit_button);
  gtk_widget_show(vbox);
  gtk_widget_show (window);

  return(window);
}

static void
usage() {
  static char usage_string[] = 
    "usage: plot-test [ --pie ] filename\n"
    "--pie   generate pie chart.\n";

  fputs(usage_string, stderr);
}

int
main(int argc, char *argv[]) {
  int opt_pie = 0;
  struct option cmd_line_opts[] = {
    {"pie", 0, &opt_pie, 1},
    {0, 0, 0, 0},
  };
  int result = 0;
  int handle;
  GtkWidget *main_win;
  GtkWidget *display_area;
  GtkWidget *list = NULL;
  
  char c;
  while((c = getopt_long(argc, argv, "", cmd_line_opts, NULL)) != EOF) {
    if(c == ':') {
      usage();
      exit(1);
    }
    if(c == '?') {
      usage();
      exit(1);
    }
  }

  if(optind != (argc - 1)) {
    usage();
    exit(1);
  }
  
  filename = argv[optind];
  
  gtk_init(&argc, &argv);
  
  //main_win = make_main_window(&list);
  //add_accounts_to_list(list, filename);

  if(opt_pie) pie_window();
  
  gtk_main();

  return result;
}
