/********************************************************************
 * gnc-html-embedded.c -- embed objects in the html stream.         *
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

#include <gnome.h>
#include <glib.h>
#include <libguppitank/guppi-tank.h>

#include "gnc-html-embedded.h"
#include "mainwindow-account-tree.h"

static double * 
read_doubles(const char * string, int nvalues) {
  int    n;
  int    choffset=0;
  int    accum = 0;
  double * retval = g_new0(double, nvalues);

  for(n=0; n<nvalues; n++) {
    sscanf(string+accum, "%le%n", &retval[n], &choffset);
    accum += choffset;
  }
  
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


/********************************************************************
 * gnc_html_embedded_piechart
 * create a Guppi piechart from an HTML <object> block
 ********************************************************************/

GtkWidget * 
gnc_html_embedded_piechart(int w, int h, GHashTable * params) {
  GuppiObject * piechart = NULL;
  GuppiObject * title = NULL;
  GtkWidget   * rv;
  GtkArg      arglist[5];
  int         argind=0;
  char        * param;
  int         datasize;
  double      * data=NULL;
  char        ** labels=NULL;
  char        ** colors=NULL;
  char        * gtitle;
  
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
  
  piechart = guppi_object_newv("pie", w, h,
                               argind, arglist);

  if(piechart) {
    if((gtitle = g_hash_table_lookup(params, "title")) != NULL) {
      title = guppi_object_new("title", w, h,
                               "title", gtitle,
                               "subtitle", g_hash_table_lookup(params, 
                                                               "subtitle"),
                               "subobject", piechart,
                               "on_top", TRUE, NULL);
      rv = guppi_object_build_widget(title);  
      gtk_object_set_user_data(GTK_OBJECT(rv), (gpointer)title);
    }
    else {
      rv = guppi_object_build_widget(piechart);  
      gtk_object_set_user_data(GTK_OBJECT(rv), (gpointer)piechart);
    }      
  }
  else {
    rv = NULL;
  }

  g_free(data);
  free_strings(labels, datasize);
  free_strings(colors, datasize);
  
  return rv;
}


/********************************************************************
 * gnc_html_embedded_barchart
 * create a Guppi barchart from an HTML <object> block
 ********************************************************************/

GtkWidget * 
gnc_html_embedded_barchart(int w, int h, GHashTable * params) {

  GuppiObject * barchart = NULL;
  GuppiObject * title = NULL;
  GtkArg      arglist[9];
  int         argind=0;
  GtkWidget   * rv;
  char        * param;
  int         datarows=0;
  int         datacols=0;
  int         rotate=0;
  double      * data=NULL;
  char        ** col_labels=NULL;
  char        ** row_labels=NULL;
  char        ** col_colors=NULL;
  char        * gtitle = NULL;

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
      
      rv = guppi_object_build_widget(title);  
      gtk_object_set_user_data(GTK_OBJECT(rv), (gpointer)title);
    }
    else {
      rv = guppi_object_build_widget(barchart);  
      gtk_object_set_user_data(GTK_OBJECT(rv), (gpointer)barchart);
    }
  }
  else {
    rv = NULL;
  }

  g_free(data);
  free_strings(col_labels, datacols);
  free_strings(row_labels, datarows);
  free_strings(col_colors, datacols);

  return rv;
}


/********************************************************************
 * gnc_html_embedded_scatter
 * create a Guppi scatter plot from an HTML <object> block
 ********************************************************************/

GtkWidget * 
gnc_html_embedded_scatter(int w, int h, GHashTable * params) {

  GuppiObject * scatter = NULL;
  GuppiObject * title = NULL;
  GtkWidget   * rv;
  GtkArg      arglist[8];
  int         argind=0;
  char        * param;
  int         datasize;
  int         color;
  double      * x_data=NULL;
  double      * y_data=NULL;
  char        * gtitle = NULL;

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
      rv = guppi_object_build_widget(title);  
      gtk_object_set_user_data(GTK_OBJECT(rv), (gpointer)title);
    }
    else {
      rv = guppi_object_build_widget(scatter);  
      gtk_object_set_user_data(GTK_OBJECT(rv), (gpointer)scatter);
    }
  }
  else {
    rv = NULL;
  }
  g_free(x_data);
  g_free(y_data);
  
  return rv;
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

GtkWidget * 
gnc_html_embedded_account_tree(int w, int h, GHashTable * params) {
  AccountViewInfo info;
  GtkWidget       * tree = gnc_mainwin_account_tree_new();
  char            * param;
  int             * fields;

  memset(&info, 0, sizeof(AccountViewInfo));

  if((param = g_hash_table_lookup(params, "fields"))) {
    set_bools(param, &(info.show_field[0]), NUM_ACCOUNT_FIELDS);
  }
  if((param = g_hash_table_lookup(params, "types"))) {
    set_bools(param, &(info.include_type[0]), NUM_ACCOUNT_TYPES);
  }
  gnc_mainwin_account_tree_set_view_info(GNC_MAINWIN_ACCOUNT_TREE(tree),
                                         info);
  
  gnc_account_tree_refresh(GNC_ACCOUNT_TREE
                           (GNC_MAINWIN_ACCOUNT_TREE(tree)->acc_tree));
  return tree;
}
