/********************************************************************
 * gnc-html-guppi.c -- embed guppi objects in the html stream.      *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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
 ********************************************************************/

#include "config.h"

#ifdef USE_GUPPI

#include <gnome.h>
#include <glib.h>
#include <guile/gh.h>
#include <gtkhtml/gtkhtml.h>
#include <gtkhtml/gtkhtml-embedded.h>

#include <libguppi/guppi-version.h>
#include <libguppitank/guppi-tank.h>

#include "gnc-engine-util.h"
#include "gnc-html.h"
#include "gnc-html-guppi.h"
#include "gnc-ui-util.h"

static short module = MOD_GUI;

static int handle_piechart(gnc_html * html, GtkHTMLEmbedded * eb, gpointer d);
static int handle_barchart(gnc_html * html, GtkHTMLEmbedded * eb, gpointer d);
static int handle_scatter(gnc_html * html, GtkHTMLEmbedded * eb, gpointer d);


/********************************************************************
 * gnc_html_guppi_init
 * add object handlers for guppi objects 
 ********************************************************************/

void
gnc_html_guppi_init(void) {
  guppi_tank_init();
  gnc_html_register_object_handler("gnc-guppi-pie", handle_piechart);
  gnc_html_register_object_handler("gnc-guppi-bar", handle_barchart);
  gnc_html_register_object_handler("gnc-guppi-scatter", handle_scatter);
}

void
gnc_html_guppi_shutdown(void) {
  guppi_tank_shutdown();
}

static void 
gnc_html_guppi_print_cb(GtkHTMLEmbedded * eb, GnomePrintContext * pc,
                        gpointer data) {
  GtkWidget   * w = data;
  GuppiObject * o = gtk_object_get_user_data(GTK_OBJECT(w));

  guppi_object_print(o, pc);
}

/* the handlers for pie. bar, and scatter charts */ 
static int
handle_piechart(gnc_html * html, GtkHTMLEmbedded * eb, gpointer data) {
  GtkWidget * widg = NULL;
  int       retval;
  widg = gnc_html_embedded_piechart(html, eb->width, eb->height, 
                                    eb->params); 

  if(widg) {
    if(gtk_signal_lookup("draw_print", gtk_html_embedded_get_type())) {
      gtk_signal_connect(GTK_OBJECT(eb), "draw_print", gnc_html_guppi_print_cb,
                         widg);    
    }

    gtk_widget_show_all(widg);
    gtk_container_add(GTK_CONTAINER(eb), widg);
    gtk_widget_set_usize(GTK_WIDGET(eb), eb->width, eb->height);
    retval = TRUE;
  }
  else {
    retval = FALSE;
  }
  return retval;
}

static int 
handle_barchart(gnc_html * html, GtkHTMLEmbedded * eb, gpointer data) {
  GtkWidget * widg = NULL;
  int       retval;
  widg = gnc_html_embedded_barchart(html, eb->width, eb->height, 
                                    eb->params); 
  if(widg) {
    if(gtk_signal_lookup("draw_print",  gtk_html_embedded_get_type())) {
      gtk_signal_connect(GTK_OBJECT(eb), "draw_print", gnc_html_guppi_print_cb,
                         widg);    
    }

    gtk_widget_show_all(widg);
    gtk_container_add(GTK_CONTAINER(eb), widg);
    gtk_widget_set_usize(GTK_WIDGET(eb), eb->width, eb->height);
    retval = TRUE;
  }
  else {
    retval = FALSE;
  }
  return retval;
}  

static int 
handle_scatter(gnc_html * html, GtkHTMLEmbedded * eb, gpointer data) {
  GtkWidget * widg = NULL;
  int       retval;
  widg = gnc_html_embedded_scatter(html, eb->width, eb->height, 
                                   eb->params); 
  if(widg) {
    if(gtk_signal_lookup("draw_print",  gtk_html_embedded_get_type())) {
      gtk_signal_connect(GTK_OBJECT(eb), "draw_print", gnc_html_guppi_print_cb,
                         widg);    
    }

    gtk_widget_show_all(widg);
    gtk_container_add(GTK_CONTAINER(eb), widg);
    gtk_widget_set_usize(GTK_WIDGET(eb), eb->width, eb->height);
    retval = TRUE;
  }
  else {
    retval = FALSE;
  }
  return retval;
}
  

/********************************************************************
 * now we start the actual embedded object creation routines.  well,
 * after some utilities.
 ********************************************************************/

static double * 
read_doubles(const char * string, int nvalues) {
  int    n;
  int    choffset=0;
  int    accum = 0;
  double * retval = g_new0(double, nvalues);

  gnc_push_locale ("C");

  for(n=0; n<nvalues; n++) {
    sscanf(string+accum, "%le%n", &retval[n], &choffset);
    accum += choffset;
  }

  gnc_pop_locale ();

  return retval;
}

static char ** 
read_strings(const char * string, int nvalues) {
  int    n;
  int    choffset=0;
  int    accum = 0;
  char   ** retval = g_new0(char *, nvalues);
  char   thischar;
  const char * inptr = string;
  int    escaped = FALSE;

  for(n=0; n < nvalues; n++) {
    retval[n] = g_new0(char, strlen(string+accum)+1);
    retval[n][0] = 0;
    choffset = 0;
    while((thischar = *inptr) != 0) {
      if(thischar == '\\') {
        escaped = TRUE;
        inptr++;
      }
      else if((thischar != ' ') || escaped) {
        retval[n][choffset] = thischar;
        retval[n][choffset+1] = 0;    
        choffset++;
        escaped = FALSE;
        inptr++;
      }
      else {
        /* an unescaped space */
        escaped = FALSE;
        inptr++;
        break;
      }
    }
    accum += choffset;
    /* printf("retval[%d] = '%s'\n", n, retval[n]); */
  }
  
  return retval;  
}

static void
free_strings(char ** strings, int nstrings) {
  int  count;

  if(!strings) return;

  for(count=0; count < nstrings; count++) {
    g_free(strings[count]);
    strings[count] = NULL;
  }
  g_free(strings);
}


/* 
 * if at least one is non-zero, return TRUE
 * ie TRUE==good
 */

static gboolean
check_doubles(double *numbers, int size)
{
  int count;

  for(count = 0;count < size; count++)
  {
    /* FIXME: floating point equalities are strictly evil but 
     * it shouldn't catch us here
     */
    if(numbers[count] != 0.0)
    {
      return TRUE;
    }
  }

  return FALSE;
}


struct guppi_chart_data {
  GtkWidget    * widget;
  GuppiObject  * guppiobject;
  gnc_html     * parent;

  GPtrArray    * data_1_callbacks;
  GPtrArray    * data_2_callbacks;
  GPtrArray    * data_3_callbacks;

  GPtrArray    * legend_1_callbacks;
  GPtrArray    * legend_2_callbacks;
  GPtrArray    * legend_3_callbacks;  
};

static struct guppi_chart_data *
gnc_guppi_chart_data_new(void) {
  struct guppi_chart_data * rv = g_new0(struct guppi_chart_data, 1);
  rv->widget      = NULL;
  rv->guppiobject = NULL;
  return rv;
}

static void
gnc_guppi_chart_data_destroy(struct guppi_chart_data * d) {
  g_free(d);
}


/* callbacks for button double-click on a pie slice, barchart bar, or
 * legend element.  generic_callback is used by all of them. */

static void
guppi_generic_callback(gnc_html * html, GPtrArray * array, gint index) {
  URLType   type;
  char      * location = NULL;
  char      * label = NULL;
  char      * url = g_ptr_array_index(array, index);

  if(!url) return;
  if(url[0] == '\0') return;

  type = gnc_html_parse_url(html, url, &location, &label);
  gnc_html_show_url(html, type, location, label, 0);

  g_free(location);
  g_free(label);
}

static void
guppi_slice_1_callback(gint slice, gpointer user_data) {
  struct guppi_chart_data * chart = user_data;
  guppi_generic_callback(chart->parent, 
                         chart->data_1_callbacks,
                         slice);
}

static void
guppi_slice_2_callback(gint slice, gpointer user_data) {
  struct guppi_chart_data * chart = user_data;
  guppi_generic_callback(chart->parent, 
                         chart->data_2_callbacks,
                         slice);
}

static void
guppi_slice_3_callback(gint slice, gpointer user_data) {
  struct guppi_chart_data * chart = user_data;
  guppi_generic_callback(chart->parent, 
                         chart->data_3_callbacks,
                         slice);
}

static void
guppi_legend_1_callback(gint item, gpointer user_data) {
  struct guppi_chart_data * chart = user_data;
  guppi_generic_callback(chart->parent, 
                         chart->legend_1_callbacks,
                         item);
}

static void
guppi_legend_2_callback(gint item, gpointer user_data) {
  struct guppi_chart_data * chart = user_data;
  guppi_generic_callback(chart->parent, 
                         chart->legend_2_callbacks,
                         item);
}

static void
guppi_legend_3_callback(gint item, gpointer user_data) {
  struct guppi_chart_data * chart = user_data;
  guppi_generic_callback(chart->parent, 
                         chart->legend_3_callbacks,
                         item);
}

static void
guppi_bar_1_callback(gint row, gint col, gpointer user_data) {
  struct guppi_chart_data * chart = user_data;
  guppi_generic_callback(chart->parent, 
                         chart->data_1_callbacks,
                         /* (chart->cols * row)+ FIXME: would require
                            to store col number.*/
			 col);  
}

static void
guppi_bar_2_callback(gint row, gint col, gpointer user_data) {
  struct guppi_chart_data * chart = user_data;
  guppi_generic_callback(chart->parent, 
                         chart->data_1_callbacks,
                         /* (chart->cols * row)+ FIXME: see above*/
                         col);  
}

static void
guppi_bar_3_callback(gint row, gint col, gpointer user_data) {
  struct guppi_chart_data * chart = user_data;
  guppi_generic_callback(chart->parent, 
                         chart->data_1_callbacks,
                         /* (chart->cols * row)+ FIXME: see above*/
                         col);  
}

static GPtrArray * 
convert_string_array(char ** strings, int nstrings) {
  GPtrArray  * retval = g_ptr_array_new();
  int        i;

  /*  g_ptr_array_set_size(retval, nstrings); */
  for(i=0; i < nstrings; i++) {
    g_ptr_array_add(retval, strings[i]);    
  }
  return retval;
}

static gboolean
gnc_has_guppi_version(int major, int minor, int micro)
{
  if (guppi_version_major () > major)
    return TRUE;
  if (guppi_version_major () < major)
    return FALSE;

  if (guppi_version_minor () > minor)
    return TRUE;
  if (guppi_version_minor () < minor)
    return FALSE;

  return (guppi_version_micro () >= micro);
}

/********************************************************************
 * gnc_html_embedded_piechart
 * create a Guppi piechart from an HTML <object> block
 ********************************************************************/

GtkWidget * 
gnc_html_embedded_piechart(gnc_html * parent, int w, int h, 
                           GHashTable * params) {
  struct guppi_chart_data * chart = gnc_guppi_chart_data_new();
  GuppiObject * piechart = NULL;
  GuppiObject * title = NULL;
  GtkArg      arglist[17];
  int         argind=0;
  char        * param;
  int         datasize;
  double      * data=NULL;
  char        ** labels=NULL;
  char        ** colors=NULL;
  char        ** callbacks=NULL;
  char        * gtitle;

  chart->parent = parent;

  if((param = g_hash_table_lookup(params, "datasize")) != NULL) {
    sscanf(param, "%d", &datasize);
    arglist[argind].name   = "data_size";
    arglist[argind].type   = GTK_TYPE_INT;
    GTK_VALUE_INT(arglist[argind]) = datasize;
    argind++;
  }
  if((param = g_hash_table_lookup(params, "data")) != NULL) {
    data = read_doubles(param, datasize);
    arglist[argind].name   = "data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = data;
    argind++;
  }
  if((param = g_hash_table_lookup(params, "labels")) != NULL) {
    labels = read_strings(param, datasize);
    arglist[argind].name   = "labels";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = labels;
    argind++;
  }
  if((param = g_hash_table_lookup(params, "colors")) != NULL) {
    colors = read_strings(param, datasize);
    arglist[argind].name   = "colors";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = colors;
    argind++;
  }
  if((param = g_hash_table_lookup(params, "slice_urls_1")) != NULL) {
    arglist[argind].name   = "slice_callback1";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = &guppi_slice_1_callback;
    argind++;
    arglist[argind].name   = "slice_callback1_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = chart;
    argind++;
    
    callbacks = read_strings(param, datasize);
    chart->data_1_callbacks = convert_string_array(callbacks, datasize);
    g_free(callbacks);
  }
  if((param = g_hash_table_lookup(params, "slice_urls_2")) != NULL) {
    arglist[argind].name   = "slice_callback2";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = &guppi_slice_2_callback;
    argind++;
    arglist[argind].name   = "slice_callback2_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = chart;
    argind++;

    callbacks = read_strings(param, datasize);
    chart->data_2_callbacks = convert_string_array(callbacks, datasize);
    g_free(callbacks);
  }
  if((param = g_hash_table_lookup(params, "slice_urls_3")) != NULL) {
    arglist[argind].name   = "slice_callback3";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = &guppi_slice_3_callback;
    argind++;
    arglist[argind].name   = "slice_callback3_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = chart;
    argind++;

    callbacks = read_strings(param, datasize);
    chart->data_3_callbacks = convert_string_array(callbacks, datasize);
    g_free(callbacks);
  }
  if((param = g_hash_table_lookup(params, "legend_urls_1")) != NULL) {
    arglist[argind].name   = "legend_callback1";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = &guppi_legend_1_callback;
    argind++;
    arglist[argind].name   = "legend_callback1_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = chart;
    argind++;
    
    callbacks = read_strings(param, datasize);
    chart->legend_1_callbacks = convert_string_array(callbacks, datasize);
    g_free(callbacks);
  }
  if((param = g_hash_table_lookup(params, "legend_urls_2")) != NULL) {
    arglist[argind].name   = "legend_callback2";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = &guppi_legend_2_callback;
    argind++;
    arglist[argind].name   = "legend_callback2_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = chart;
    argind++;
    
    callbacks = read_strings(param, datasize);
    chart->legend_2_callbacks = convert_string_array(callbacks, datasize);
    g_free(callbacks);
  }
  if((param = g_hash_table_lookup(params, "legend_urls_3")) != NULL) {
    arglist[argind].name   = "legend_callback3";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = &guppi_legend_3_callback;
    argind++;
    arglist[argind].name   = "legend_callback3_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = chart;
    argind++;
    
    callbacks = read_strings(param, datasize);
    chart->legend_3_callbacks = convert_string_array(callbacks, datasize);
    g_free(callbacks);
  }

  piechart = guppi_object_newv ("pie", w, h, argind, arglist);

  if(piechart) {
    if((gtitle = g_hash_table_lookup(params, "title")) != NULL) {
      title = guppi_object_new("title", w, h,
                               "title", gtitle,
                               "subtitle", g_hash_table_lookup(params, 
                                                               "subtitle"),
                               "subobject", piechart,
                               "on_top", TRUE, NULL);
      chart->widget = guppi_object_build_widget(title);  
      chart->guppiobject = title;
    }
    else {
      chart->widget = guppi_object_build_widget(piechart);  
      chart->guppiobject = piechart; 
    }      
  }
  else {
    gnc_guppi_chart_data_destroy(chart);
    chart = NULL;
  }

  g_free(data);
  free_strings(labels, datasize);
  free_strings(colors, datasize);
  
  if(chart) {
    gtk_object_set_user_data(GTK_OBJECT(chart->widget), chart->guppiobject);
    return chart->widget;
  }
  else 
    return NULL;
}


/********************************************************************
 * gnc_html_embedded_barchart
 * create a Guppi barchart from an HTML <object> block
 ********************************************************************/

GtkWidget * 
gnc_html_embedded_barchart(gnc_html * parent, 
                           int w, int h, GHashTable * params) {
  struct guppi_chart_data * chart = gnc_guppi_chart_data_new();
  GuppiObject * barchart = NULL;
  GuppiObject * title = NULL;
  GtkArg      arglist[21];
  int         argind=0;
  char        * param;
  int         datarows=0;
  int         datacols=0;
  int         legend_reversed=0;
  int         rotate=0;
  int         stacked=0;  
  int         normalize_stacks=0;  
  double      * data=NULL;
  char        ** col_labels=NULL;
  char        ** row_labels=NULL;
  char        ** col_colors=NULL;
  char        ** callbacks=NULL;
  char        * gtitle = NULL;

  chart->parent = parent;

  if((param = g_hash_table_lookup(params, "data_rows")) != NULL) {
    sscanf(param, "%d", &datarows);
    arglist[argind].name   = "data_rows";
    arglist[argind].type   = GTK_TYPE_INT;
    GTK_VALUE_INT(arglist[argind]) = datarows;
    argind++;    
  }
  if((param = g_hash_table_lookup(params, "data_cols")) != NULL) {
    sscanf(param, "%d", &datacols);
    arglist[argind].name   = "data_columns";
    arglist[argind].type   = GTK_TYPE_INT;
    GTK_VALUE_INT(arglist[argind]) = datacols;
    argind++;    
  }
  if((param = g_hash_table_lookup(params, "data")) != NULL) {
    data = read_doubles(param, datarows*datacols);
    arglist[argind].name   = "data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = data;
    argind++;    
  }

#if 0
  /* These still give segfault, regardless of what you do. Shit. */
  if(gnc_has_guppi_version(0,35,6) &&
     (param = g_hash_table_lookup(params, "x_axis_label")) != NULL) {
    arglist[argind].name   = "x_axis_label";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = param;
    argind++;    
  }
  if(gnc_has_guppi_version(0,35,6) &&
     (param = g_hash_table_lookup(params, "y_axis_label")) != NULL) {
    arglist[argind].name   = "y_axis_label";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = param;
    argind++;    
  }
#endif

  if((param = g_hash_table_lookup(params, "col_labels")) != NULL) {
    col_labels = read_strings(param, datacols);
    arglist[argind].name   = "column_labels";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = col_labels;
    argind++;    
  }
  if((param = g_hash_table_lookup(params, "row_labels")) != NULL) {
    row_labels = read_strings(param, datarows);
    arglist[argind].name   = "row_labels";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = row_labels;
    argind++;    
  }
  if((param = g_hash_table_lookup(params, "col_colors")) != NULL) {
    col_colors = read_strings(param, datacols);
    arglist[argind].name   = "column_colors";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = col_colors;
    argind++;    
  }
  if((param = g_hash_table_lookup(params, "rotate_row_labels")) != NULL) {
    sscanf(param, "%d", &rotate);
    arglist[argind].name   = "rotate_x_axis_labels";
    arglist[argind].type   = GTK_TYPE_BOOL;
    GTK_VALUE_BOOL(arglist[argind]) = rotate;
    argind++;    
  }
  if((param = g_hash_table_lookup(params, "bar_urls_1")) != NULL) {
    arglist[argind].name   = "bar_callback1";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = &guppi_bar_1_callback;
    argind++;
    arglist[argind].name   = "bar_callback1_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = chart;
    argind++;
    
    callbacks = read_strings(param, datarows*datacols);
    chart->data_1_callbacks = convert_string_array(callbacks, 
                                                   datarows*datacols);
    g_free(callbacks);
  }
  if((param = g_hash_table_lookup(params, "bar_urls_2")) != NULL) {
    arglist[argind].name   = "bar_callback2";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = &guppi_bar_2_callback;
    argind++;
    arglist[argind].name   = "bar_callback2_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = chart;
    argind++;
    
    callbacks = read_strings(param, datarows*datacols);
    chart->data_2_callbacks = convert_string_array(callbacks, 
                                                   datarows*datacols);
    g_free(callbacks);
  }
  if((param = g_hash_table_lookup(params, "bar_urls_3")) != NULL) {
    arglist[argind].name   = "bar_callback3";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = &guppi_bar_3_callback;
    argind++;
    arglist[argind].name   = "bar_callback3_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = chart;
    argind++;
    
    callbacks = read_strings(param, datarows*datacols);
    chart->data_3_callbacks = convert_string_array(callbacks, 
                                                   datarows*datacols);
    g_free(callbacks);
  }
  if((param = g_hash_table_lookup(params, "legend_urls_1")) != NULL) {
    arglist[argind].name   = "legend_callback1";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = &guppi_legend_1_callback;
    argind++;
    arglist[argind].name   = "legend_callback1_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = chart;
    argind++;
    
    callbacks = read_strings(param, datarows*datacols);
    chart->legend_1_callbacks = convert_string_array(callbacks, 
                                                     datarows*datacols);
    g_free(callbacks);
  }
  if((param = g_hash_table_lookup(params, "legend_urls_2")) != NULL) {
    arglist[argind].name   = "legend_callback2";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = &guppi_legend_2_callback;
    argind++;
    arglist[argind].name   = "legend_callback2_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = chart;
    argind++;
    
    callbacks = read_strings(param, datarows*datacols);
    chart->legend_2_callbacks = convert_string_array(callbacks, 
                                                     datarows*datacols);
    g_free(callbacks);
  }
  if((param = g_hash_table_lookup(params, "legend_urls_3")) != NULL) {
    arglist[argind].name   = "legend_callback3";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = &guppi_legend_3_callback;
    argind++;
    arglist[argind].name   = "legend_callback3_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = chart;
    argind++;
    
    callbacks = read_strings(param, datarows*datacols);
    chart->legend_3_callbacks = convert_string_array(callbacks, 
                                                     datarows*datacols);
    g_free(callbacks);
  }
  if(gnc_has_guppi_version(0,35,4) &&
     (param = g_hash_table_lookup(params, "legend_reversed")) != NULL) {
    sscanf(param, "%d", &rotate);
    arglist[argind].name   = "legend_reversed";
    arglist[argind].type   = GTK_TYPE_BOOL;
    GTK_VALUE_BOOL(arglist[argind]) = rotate;
    argind++;
  }
  if(gnc_has_guppi_version(0,35,4) &&
     (param = g_hash_table_lookup(params, "stacked")) != NULL) {
    sscanf(param, "%d", &stacked);
    arglist[argind].name   = "stacked";
    arglist[argind].type   = GTK_TYPE_BOOL;
    GTK_VALUE_BOOL(arglist[argind]) = stacked;
    argind++;
  }
  if(gnc_has_guppi_version(0,35,4) &&
     (param = g_hash_table_lookup(params, "normalize_stacks")) != NULL) {
    sscanf(param, "%d", &normalize_stacks);
    arglist[argind].name   = "normalize_stacks";
    arglist[argind].type   = GTK_TYPE_BOOL;
    GTK_VALUE_BOOL(arglist[argind]) = normalize_stacks;
    argind++;
  }

  barchart = guppi_object_newv("barchart", w, h,
                               argind, arglist);

  if(barchart) {
    if((gtitle = g_hash_table_lookup(params, "title")) != NULL) {      
      title = guppi_object_new("title", w, h,
                               "title", gtitle,
                               "subtitle", g_hash_table_lookup(params, 
                                                               "subtitle"),
                               "subobject", barchart,
                               "on_top", TRUE, NULL);
      
      chart->widget = guppi_object_build_widget(title);  
      chart->guppiobject = title;
    }
    else {
      chart->widget = guppi_object_build_widget(barchart);  
      chart->guppiobject = barchart;
    }
  }
  else {
    gnc_guppi_chart_data_destroy(chart);
    chart = NULL;
  }

  g_free(data);
  free_strings(col_labels, datacols);
  free_strings(row_labels, datarows);
  free_strings(col_colors, datacols);

  if(chart) {
    gtk_object_set_user_data(GTK_OBJECT(chart->widget), chart->guppiobject);
    return chart->widget;
  }
  else
    return NULL;
}


/********************************************************************
 * gnc_html_embedded_scatter
 * create a Guppi scatter plot from an HTML <object> block
 ********************************************************************/

GtkWidget * 
gnc_html_embedded_scatter(gnc_html * parent, 
                          int w, int h, GHashTable * params) {
  struct guppi_chart_data * chart = gnc_guppi_chart_data_new();
  GuppiObject  * scatter = NULL;
  GuppiObject  * title = NULL;
  GtkArg       arglist[8];
  int          argind=0;
  char         * param;
  int          datasize;
  unsigned int color;
  double       * x_data=NULL;
  double       * y_data=NULL;
  char         * gtitle = NULL;

  chart->parent = parent;

  if((param = g_hash_table_lookup(params, "datasize")) != NULL) {
    sscanf(param, "%d", &datasize);
    arglist[argind].name   = "data_size";
    arglist[argind].type   = GTK_TYPE_INT;
    GTK_VALUE_INT(arglist[argind]) = datasize;
    argind++;    
  }
  if((param = g_hash_table_lookup(params, "color")) != NULL) {
    sscanf(param, "%x", &color);
    arglist[argind].name   = "color_rgba";
    arglist[argind].type   = GTK_TYPE_UINT;
    GTK_VALUE_UINT(arglist[argind]) = color;
    argind++;    
  }
  if((param = g_hash_table_lookup(params, "x_data")) != NULL) {
    x_data = read_doubles(param, datasize);
    arglist[argind].name   = "x_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = x_data;
    argind++;    
  }
  if((param = g_hash_table_lookup(params, "y_data")) != NULL) {
    y_data = read_doubles(param, datasize);
    arglist[argind].name   = "y_data";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = y_data;
    argind++;    
  }
  if((param = g_hash_table_lookup(params, "x_axis_label")) != NULL) {
    arglist[argind].name   = "x_axis_label";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = param;
    argind++;    
  }
  if((param = g_hash_table_lookup(params, "y_axis_label")) != NULL) {
    arglist[argind].name   = "y_axis_label";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = param;
    argind++;    
  }
  if((param = g_hash_table_lookup(params, "marker")) != NULL) {
    arglist[argind].name   = "marker";
    arglist[argind].type   = GTK_TYPE_POINTER;
    GTK_VALUE_POINTER(arglist[argind]) = param;
    argind++;    
  }
  
  scatter = guppi_object_newv("scatter", w, h,
                             argind, arglist);

  if(scatter) {
    if((gtitle = g_hash_table_lookup(params, "title")) != NULL) {
      title = guppi_object_new("title", w, h,
                               "title", gtitle,
                               "subtitle", g_hash_table_lookup(params, 
                                                               "subtitle"),
                               "subobject", scatter,
                               "on_top", TRUE, NULL);
      chart->widget = guppi_object_build_widget(title);  
      chart->guppiobject = title;
    }
    else {
      chart->widget = guppi_object_build_widget(scatter);  
      chart->guppiobject = scatter;
    }
  }
  else {
    gnc_guppi_chart_data_destroy(chart);
    chart = NULL;
  }
  g_free(x_data);
  g_free(y_data);
  
  if(chart) {
    gtk_object_set_user_data(GTK_OBJECT(chart->widget), chart->guppiobject);
    return chart->widget;
  }
  else 
    return NULL;
}


/********************************************************************
 * gnc_html_embedded_account_tree
 * create a gnucash account tree display from an <object> form
 ********************************************************************/

static void 
set_bools(char * indices, gboolean * array, int num) {
  int index, n, args;
  int accum = 0;
  int choffset = 0;
  
  for(n=0; n<num; n++) {
    args = sscanf(indices+accum, "%d%n", &index, &choffset);
    accum += choffset;
    if(args == 0) {
      break;
    }
    if(index < num) {
      array[index] = TRUE;
    }
  }
}


#endif
