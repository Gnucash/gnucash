/********************************************************************
 * gnc-html.c -- display HTML with some special gnucash tags.       *
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

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <gtkhtml/gtkhtml.h>
#include <gtkhtml/gtkhtml-embedded.h>
#include <libguppitank/guppi-tank.h>
#include <gnome.h>
#include <regex.h>
#include <glib.h>
#include <ghttp.h>
#include <guile/gh.h>

#include "Account.h"
#include "Group.h"
#include "RegWindow.h"
#include "File.h"
#include "FileDialog.h"
#include "dialog-utils.h"
#include "window-register.h"
#include "print-session.h"
#include "gnc-html.h"
#include "gnc-html-history.h"
#include "gnc-html-embedded.h"
#include "window-help.h"
#include "window-report.h"

struct _gnc_html {
  GtkWidget * container;
  GtkWidget * html;

  gchar     * current_link; 

  URLType   base_type;
  gchar     * base_location;

  /* callbacks */
  GncHTMLUrltypeCB  urltype_cb;
  GncHTMLLoadCB     load_cb;
  GncHTMLFlyoverCB  flyover_cb;
  GncHTMLButtonCB   button_cb;

  ghttp_request     * request; 
  gpointer          flyover_cb_data;
  gpointer          load_cb_data;
  gpointer          button_cb_data;
  
  struct _gnc_html_history * history; 
};

static char error_404[] = 
"<html><body><h3>Not found</h3><p>The specified URL could not be loaded.</body></html>";

/********************************************************************
 * gnc_html_parse_url
 * this takes a URL and determines the protocol type, location, and
 * possible anchor name from the URL.
 ********************************************************************/

URLType
gnc_html_parse_url(gnc_html * html, const gchar * url, 
                   char ** url_location, char ** url_label) {
  char        uri_rexp[] = "^(([^:]*):)?([^#]+)?(#(.*))?$";
  regex_t     compiled;
  regmatch_t  match[6];
  char        * protocol=NULL, * path=NULL, * label=NULL;
  int         found_protocol=0, found_path=0, found_label=0; 
  URLType     retval;   

  regcomp(&compiled, uri_rexp, REG_EXTENDED);

  if(!regexec(&compiled, url, 6, match, 0)) {
    if(match[2].rm_so != -1) {
      protocol = g_new0(char, match[2].rm_eo - match[2].rm_so + 1);
      strncpy(protocol, url + match[2].rm_so, 
              match[2].rm_eo - match[2].rm_so);
      protocol[match[2].rm_eo - match[2].rm_so] = 0;
      found_protocol = 1;      
    }
    if(match[3].rm_so != -1) {
      path = g_new0(char, match[3].rm_eo - match[3].rm_so + 1);
      strncpy(path, url+match[3].rm_so, 
              match[3].rm_eo - match[3].rm_so);
      path[match[3].rm_eo - match[3].rm_so] = 0;
      found_path = 1;
    }
    if(match[5].rm_so != -1) {
      label = g_new0(char, match[5].rm_eo - match[5].rm_so + 1);
      strncpy(label, url+match[5].rm_so, 
              match[5].rm_eo - match[5].rm_so);
      label[match[5].rm_eo - match[5].rm_so] = 0;
      found_label = 1;
    }
  }

  regfree(&compiled);

  if(found_protocol) {
    if(!strcmp(protocol, "file")) {
      retval = URL_TYPE_FILE;
    }
    else if(!strcmp(protocol, "http")) {
      retval = URL_TYPE_HTTP;
    }
    else if(!strcmp(protocol, "ftp")) {
      retval = URL_TYPE_FTP;
    }
    else if(!strcmp(protocol, "https")) {
      retval = URL_TYPE_SECURE;
    }
    else if(!strcmp(protocol, "gnc-register")) {
      retval = URL_TYPE_REGISTER;
    }
    else if(!strcmp(protocol, "gnc-report")) {
      retval = URL_TYPE_REPORT;
    }
    else if(!strcmp(protocol, "gnc-scm")) {
      retval = URL_TYPE_SCHEME;
    }
    else if(!strcmp(protocol, "gnc-help")) {
      retval = URL_TYPE_HELP;
    }
    else {
      printf(" ** gnc-html WARNING : unhandled URL type for '%s'\n",
             url);
      retval = URL_TYPE_OTHER;
    }
  }
  else if(found_label && !found_path) {
    retval = URL_TYPE_JUMP;
  }
  else {
    retval = html->base_type;
  }
  
  g_free(protocol);
 
  switch(retval) {
  case URL_TYPE_FILE:    
    if(!found_protocol && path && html->base_location) {
      *url_location = g_new0(char, 
                             strlen(path) + strlen(html->base_location) + 1);
      *url_location[0] = 0;

      strcpy(*url_location, html->base_location);      
      strcat(*url_location, "/");
      strcat(*url_location, path);
      
      g_free(path);
    }
    else {
      *url_location = path;
    }
    break;

  case URL_TYPE_JUMP:
    *url_location = NULL;
    g_free(path);
    break;

  case URL_TYPE_OTHER:
  default:
    if(!found_protocol && path && html->base_location) {
      *url_location = g_new0(char, 
                             strlen(path) + strlen(html->base_location) + 1);
      *url_location[0] = 0;
      strcpy(*url_location, html->base_location);
      strcat(*url_location, path);
      g_free(path);
    }
    else {
      *url_location = path;
    }
    break;
  }
  
  *url_label = label;
  return retval;
}


static char * 
extract_base_name(URLType type, const gchar * path) {
  char       http_rexp[] = "^(//[^/]*)(/.*)?$";
  char       other_rexp[] = "^(.*)(/([^/]*))?$";
  regex_t    compiled_h, compiled_o;
  regmatch_t match[4];
  char       * machine=NULL, * location = NULL, * base=NULL;
  int        free_location = 0;

  regcomp(&compiled_h, http_rexp, REG_EXTENDED);
  regcomp(&compiled_o, other_rexp, REG_EXTENDED);

  if(!path) return NULL;

  switch(type) {
  case URL_TYPE_HTTP:
  case URL_TYPE_SECURE:
  case URL_TYPE_FTP:
    if(!regexec(&compiled_h, path, 4, match, 0)) {
      if(match[1].rm_so != -1) {
        machine = g_new0(char, strlen(path) + 2);
        strncpy(machine, path+match[1].rm_so, 
                match[1].rm_eo - match[1].rm_so);
      } 
      if(match[2].rm_so != -1) {
        location = g_new0(char, match[2].rm_eo - match[2].rm_so + 1);
        strncpy(location, path+match[2].rm_so, 
                match[2].rm_eo - match[2].rm_so);
        free_location = 1;
      }
    }  
    break;
  default:
    location = g_strdup(path);
  }
   
  if(location) {
    if(!regexec(&compiled_o, location, 4, match, 0)) {
      if(match[2].rm_so != -1) {
        base = g_new0(char,  match[1].rm_eo - match[1].rm_so + 1);
        strncpy(base, path+match[1].rm_so, 
                match[1].rm_eo - match[1].rm_so);
      }
    }
  }
  
  regfree(&compiled_h);
  regfree(&compiled_o);

  if(free_location) {
    g_free(location);
  }

  if(machine) {
    strcat(machine, "/");
    if(base) { 
      strcat(machine, base);
    }
    g_free(base);
    return machine;
  }
  else {
    g_free(machine);
    return base;
  }
}

static char * url_type_names[] = {
  "file:", "", "http:", "ftp:", "https:", 
  "gnc-register:", "gnc-report:", "gnc-scm:",
  "gnc-help:", ""
};


static gchar  *
rebuild_url(URLType type, const gchar * location, const gchar * label) {
  if(label) {
    return g_strdup_printf("%s%s#%s", url_type_names[type], 
                           (location ? location : ""), label);
  }
  else {
    return g_strdup_printf("%s%s", url_type_names[type], 
                           (location ? location : ""));
  }
}
    
/********************************************************************
 * gnc_html_load_to_stream : actually do the work of loading the HTML
 * or binary data referenced by a URL and feeding it into the GtkHTML
 * widget.
 ********************************************************************/

static void
gnc_html_load_to_stream(gnc_html * html, GtkHTMLStream * handle,
                        URLType type, const gchar * location, 
                        const gchar * label) {
  int           fsize;
  char          * fdata = NULL;
  char          * fullurl;
  int           id;
  SCM           run_report;
  SCM           scmtext;

  if(!html) {
    return;
  }
  
  switch(type) {
  case URL_TYPE_HELP:
  case URL_TYPE_FILE:
    fsize = gncReadFile(location, &fdata);
    if(fsize > 0) {
      gtk_html_write(GTK_HTML(html->html), handle, fdata, fsize);
      gtk_html_end(GTK_HTML(html->html), handle, GTK_HTML_STREAM_OK);      
    }
    else {
      gtk_html_write(GTK_HTML(html->html), handle, error_404, 
                     strlen(error_404));
      gtk_html_end(GTK_HTML(html->html), handle, GTK_HTML_STREAM_ERROR);
    }
    g_free(fdata);
    
    break;
    
  case URL_TYPE_HTTP:
  case URL_TYPE_FTP:
  case URL_TYPE_SECURE:
    fullurl = rebuild_url(type, location, label);
    
    ghttp_set_uri(html->request, fullurl);
    ghttp_set_header(html->request, http_hdr_Connection, "close");
    ghttp_clean(html->request);
    ghttp_prepare(html->request);
    ghttp_process(html->request);

    if(ghttp_get_body_len(html->request) > 0) {
      gtk_html_write(GTK_HTML(html->html), handle, 
                     ghttp_get_body(html->request), 
                     ghttp_get_body_len(html->request));
      gtk_html_end(GTK_HTML(html->html), handle, GTK_HTML_STREAM_OK);
    }
    else {
      gtk_html_write(GTK_HTML(html->html), handle, error_404, 
                     strlen(error_404));
      gtk_html_end(GTK_HTML(html->html), handle, GTK_HTML_STREAM_ERROR);
    }

    break;
    
  case URL_TYPE_REPORT:
    run_report = gh_eval_str("gnc:inst-report-run");

    if(!strncmp("id=", location, 3)) {
      /* get the report ID */
      sscanf(location+3, "%d", &id);
      
      /* get the HTML text */ 
      scmtext = gh_call1(run_report, gh_int2scm(id));
      fdata = gh_scm2newstr(scmtext, &fsize);
      
      if(fdata) {
        gtk_html_write(GTK_HTML(html->html), handle, fdata, fsize);
        free(fdata);
        fdata = NULL;
        fsize = 0;
      }
      else {
        gtk_html_write(GTK_HTML(html->html), handle, error_404, 
                       strlen(error_404));
        printf(" ** gnc_html WARNING : report HTML generator failed.\n");
      }
    }
    gtk_html_end(GTK_HTML(html->html), handle, GTK_HTML_STREAM_OK);
    break;
    
  case URL_TYPE_REGISTER:
  case URL_TYPE_SCHEME:
  default:
    printf(" ** gnc-html WARNING : load_to_stream for inappropriate type\n");
    printf("    url = '%s#%s'\n", location, label);
    gtk_html_write(GTK_HTML(html->html), handle, error_404, 
                   strlen(error_404));
    gtk_html_end(GTK_HTML(html->html), handle, GTK_HTML_STREAM_ERROR);
    break;
    
  }
}


/********************************************************************
 * gnc_html_link_clicked_cb - called when user left-clicks on html
 * anchor. 
 ********************************************************************/

static void 
gnc_html_link_clicked_cb(GtkHTML * html, const gchar * url, gpointer data) {
  URLType   type;
  char      * location = NULL;
  char      * label = NULL;
  gnc_html  * gnchtml = (gnc_html *)data;

  type = gnc_html_parse_url(gnchtml, url, &location, &label);
  gnc_html_show_url(gnchtml, type, location, label, 0);
  g_free(location);
  g_free(label);
}


/********************************************************************
 * gnc_html_url_requested_cb - called when a URL needs to be 
 * loaded within the loading of a page (embedded image).
 ********************************************************************/

static void 
gnc_html_url_requested_cb(GtkHTML * html, char * url,
                          GtkHTMLStream * handle, gpointer data) {
  URLType       type;
  char          * location=NULL;
  char          * label=NULL;
  gnc_html      * gnchtml = (gnc_html *)data;

  type = gnc_html_parse_url(gnchtml, url, &location, &label);
  gnc_html_load_to_stream(gnchtml, handle, type, location, label);
  g_free(location);
  g_free(label);
}


static void 
gnc_html_guppi_print_cb(GtkHTMLEmbedded * eb, GnomePrintContext * pc,
                        gpointer data) {
  GtkWidget   * w = data;
  GuppiObject * o = gtk_object_get_user_data(GTK_OBJECT(w));

  /* this is a magical scaling factor (gtkhtml and guppi assume different 
   * screen resolutions) */
  gnome_print_scale(pc, 0.6944, 0.6944);
  guppi_object_print(o, pc);
}

static void 
gnc_html_guppi_redraw_cb(GtkHTMLEmbedded * eb,
                         GdkPixmap * pix, GdkGC * gc, gint x, gint y, 
                         gpointer data) {
  /* nothing special to do */
}

/********************************************************************
 * gnc_html_object_requested_cb - called when an applet needs to be
 * loaded.  
 ********************************************************************/

static int
gnc_html_object_requested_cb(GtkHTML * html, GtkHTMLEmbedded * eb,
                             gpointer data) {
  GtkWidget * widg = NULL;
  int retval = FALSE;

  if(!strcmp(eb->classid, "gnc-guppi-pie")) {
    widg = gnc_html_embedded_piechart(eb->width, eb->height, 
                                      eb->params); 
    if(widg) {
      gtk_widget_show_all(widg);
      gtk_container_add(GTK_CONTAINER(eb), widg);
      gtk_widget_set_usize(GTK_WIDGET(eb), eb->width, eb->height);
    }
    retval = TRUE;
  }
  else if(!strcmp(eb->classid, "gnc-guppi-bar")) {
    widg = gnc_html_embedded_barchart(eb->width, eb->height, 
                                      eb->params); 
    if(widg) {
      gtk_widget_show_all(widg);
      gtk_container_add(GTK_CONTAINER(eb), widg);
      gtk_widget_set_usize(GTK_WIDGET(eb), eb->width, eb->height);
    }
    retval = TRUE;
  }
  else if(!strcmp(eb->classid, "gnc-guppi-scatter")) {
    widg = gnc_html_embedded_scatter(eb->width, eb->height, 
                                     eb->params); 
    if(widg) {
      gtk_widget_show_all(widg);
      gtk_container_add(GTK_CONTAINER(eb), widg);
      gtk_widget_set_usize(GTK_WIDGET(eb), eb->width, eb->height);
    }
    retval = TRUE;
  }

  if(widg) {
    gtk_signal_connect(GTK_OBJECT(eb), "draw_gdk",
                       GTK_SIGNAL_FUNC(gnc_html_guppi_redraw_cb),
                       widg);
    gtk_signal_connect(GTK_OBJECT(eb), "draw_print",
                       GTK_SIGNAL_FUNC(gnc_html_guppi_print_cb),
                       widg);
  }
  
  return retval;
}


/********************************************************************
 * gnc_html_on_url_cb - called when user rolls over html anchor
 ********************************************************************/

static void 
gnc_html_on_url_cb(GtkHTML * html, const gchar * url, gpointer data) {
  gnc_html * gnchtml = (gnc_html *) data;

  g_free(gnchtml->current_link);
  gnchtml->current_link = g_strdup(url);
  if(gnchtml->flyover_cb) {
    (gnchtml->flyover_cb)(gnchtml, url, gnchtml->flyover_cb_data);
  }
}


/********************************************************************
 * gnc_html_set_base_cb 
 ********************************************************************/

static void 
gnc_html_set_base_cb(GtkHTML * gtkhtml, const gchar * base, 
                     gpointer data) {
  gnc_html * html = (gnc_html *)data;
  URLType  type;
  char     * location = NULL;
  char     * label = NULL;

  type = gnc_html_parse_url(html, base, &location, &label);

  g_free(html->base_location);
  g_free(label);

  html->base_type     = type;
  html->base_location = location;
  
}


/********************************************************************
 * gnc_html_key_cb
 ********************************************************************/

static gboolean
gnc_html_key_cb(GtkWidget *widget, GdkEventKey *event, gpointer data) {
  gnc_html * hw = (gnc_html *) data;
  
  GtkAdjustment * vadj = 
    gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(hw->container));
  GtkAdjustment * hadj = 
    gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(hw->container));
  
  gfloat        v_value = vadj->value;
  gfloat        h_value = hadj->value;

  switch (event->keyval)
  {
    case GDK_KP_Left:
    case GDK_Left:
      h_value -= hadj->step_increment;
      break;
    case GDK_KP_Right:
    case GDK_Right:
      h_value += hadj->step_increment;
      break;
    case GDK_KP_Up:
    case GDK_Up:
      v_value -= vadj->step_increment;
      break;
    case GDK_KP_Down:
    case GDK_Down:
      v_value += vadj->step_increment;
      break;
    case GDK_KP_Page_Up:
    case GDK_Page_Up:
      v_value -= vadj->page_increment;
      break;
    case GDK_KP_Page_Down:
    case GDK_Page_Down:
    case GDK_space:
      v_value += vadj->page_increment;
      break;
    case GDK_KP_Home:
    case GDK_Home:
      v_value = vadj->lower;
      break;
    case GDK_KP_End:
    case GDK_End:
      v_value = vadj->upper;
      break;
    default:
      return FALSE;
  }

  v_value = CLAMP(v_value, vadj->lower, vadj->upper - vadj->page_size);
  h_value = CLAMP(h_value, hadj->lower, hadj->upper - hadj->page_size);

  gtk_adjustment_set_value(vadj, v_value);
  gtk_adjustment_set_value(hadj, h_value);

  return TRUE;
}

static int
gnc_html_button_press_cb(GtkWidget * widg, GdkEventButton * event,
                         gpointer user_data) {
  gnc_html * html = user_data;

  if(html->button_cb) {
    (html->button_cb)(html, event, html->button_cb_data);
    return TRUE;
  }
  else {
    return FALSE;
  }
}


/********************************************************************
 * gnc_html_open_register
 * open a register window 
 ********************************************************************/

static void
gnc_html_open_register(gnc_html * html, const gchar * location) {
  Account   * acct;
  RegWindow * reg;

  /* href="gnc-register:account=My Bank Account" */
  if(!strncmp("account=", location, 8)) {
    acct = xaccGetAccountFromFullName(gncGetCurrentGroup(),
                                      location+8, 
                                      gnc_get_account_separator());
    reg = regWindowSimple(acct);
    gnc_register_raise(reg);
  }
}


/********************************************************************
 * gnc_html_open_report
 * open a report window 
 ********************************************************************/

static void
gnc_html_open_report(gnc_html * html, const gchar * location,
                     const gchar * label, int newwin) {
  gnc_report_window * rwin;
  GtkHTMLStream * stream;

  /* make a new window if necessary */ 
  if(newwin) {
    rwin = gnc_report_window_new(NULL);
    html = gnc_report_window_get_html(rwin);
  }

  gnc_html_history_append(html->history,
                          gnc_html_history_node_new(URL_TYPE_REPORT, 
                                                    location, label));
  g_free(html->base_location);
  html->base_type     = URL_TYPE_FILE;
  html->base_location = NULL;

  stream = gtk_html_begin(GTK_HTML(html->html));
  gnc_html_load_to_stream(html, stream, URL_TYPE_REPORT, location, label);
}


/********************************************************************
 * gnc_html_open_help
 * open a help window 
 ********************************************************************/

static void
gnc_html_open_help(gnc_html * html, const gchar * location,
                   const gchar * label, int newwin) {
  gnc_help_window * help = NULL;
  
  if(newwin) {
    help = gnc_help_window_new();
    gnc_help_window_show_help(help, location, label);
  }
  else {
    gnc_html_show_url(html, URL_TYPE_FILE, location, label, 0);
  }      
}


/********************************************************************
 * gnc_html_open_scm
 * insert some scheme-generated HTML
 ********************************************************************/

static void
gnc_html_open_scm(gnc_html * html, const gchar * location,
                  const gchar * label, int newwin) {
  printf("gnc_html_open_scm : location='%s'\n", location);
}


/********************************************************************
 * gnc_html_show_url 
 * 
 * open a URL.  This is called when the user clicks a link or 
 * for the creator of the gnc_html window to explicitly request 
 * a URL. 
 ********************************************************************/

void 
gnc_html_show_url(gnc_html * html, URLType type, 
                  const gchar * location, const gchar * label,
                  int newwin_hint) {
  
  GtkHTMLStream * handle;
  int           newwin;

  /* make sure it's OK to show this URL type in this window */
  if(newwin_hint == 0) {
    newwin = !((html->urltype_cb)(type));
  }
  else {
    newwin = 1;
  }

  switch(type) {
  case URL_TYPE_REGISTER:
    gnc_html_open_register(html, location);
    break;

  case URL_TYPE_REPORT:
    gnc_html_open_report(html, location, label, newwin);
    break;
    
  case URL_TYPE_HELP:
    gnc_html_open_help(html, location, label, newwin);
    break;
    
  case URL_TYPE_SCHEME:
    gnc_html_open_scm(html, location, label, newwin);
    break;
    
  case URL_TYPE_JUMP:
    gtk_html_jump_to_anchor(GTK_HTML(html->html), label);
    break;
    
  case URL_TYPE_HTTP:
  case URL_TYPE_FTP:
  case URL_TYPE_SECURE:
  case URL_TYPE_FILE:
    html->base_type     = type;

    if(html->base_location) g_free(html->base_location);
    html->base_location = extract_base_name(type, location);
    
    /* FIXME : handle newwin = 1 */
    gnc_html_history_append(html->history,
                            gnc_html_history_node_new(type, 
                                                      location, label));
    handle = gtk_html_begin(GTK_HTML(html->html));
    gnc_html_load_to_stream(html, handle, type, location, label);
    break;
    
  default:
    break;
  }

  if(html->load_cb) {
    (html->load_cb)(html, type, location, label, html->load_cb_data);
  }
}


/********************************************************************
 * gnc_html_reload
 * reload the current page 
 ********************************************************************/

void
gnc_html_reload(gnc_html * html) {
  gnc_html_history_node * n = gnc_html_history_get_current(html->history);
  if(n) {
    gnc_html_show_url(html, n->type, n->location, n->label, 0);
  }
}


/********************************************************************\
 * gnc_html_new
 * create and set up a new gtkhtml widget.
\********************************************************************/

gnc_html * 
gnc_html_new() {
  gnc_html * retval = g_new0(gnc_html, 1);
  
  retval->container = gtk_scrolled_window_new(NULL, NULL);
  retval->html   = gtk_html_new();
  
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(retval->container),
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);

  gtk_container_add(GTK_CONTAINER(retval->container), 
                    GTK_WIDGET(retval->html));
  
  retval->history   = gnc_html_history_new();
  retval->request   = ghttp_request_new();

  /* signals */
  gtk_signal_connect(GTK_OBJECT(retval->html), "url_requested",
                     GTK_SIGNAL_FUNC(gnc_html_url_requested_cb),
                     (gpointer)retval);
  
  gtk_signal_connect(GTK_OBJECT(retval->html), "on_url",
                     GTK_SIGNAL_FUNC(gnc_html_on_url_cb),
                     (gpointer)retval);
  
  gtk_signal_connect(GTK_OBJECT(retval->html), "set_base",
                     GTK_SIGNAL_FUNC(gnc_html_set_base_cb),
                     (gpointer)retval);
  
  gtk_signal_connect(GTK_OBJECT(retval->html), "link_clicked",
                     GTK_SIGNAL_FUNC(gnc_html_link_clicked_cb),
                     (gpointer)retval);
  
  gtk_signal_connect (GTK_OBJECT (retval->html), "object_requested",
                      GTK_SIGNAL_FUNC (gnc_html_object_requested_cb), 
                      (gpointer)retval);

  gtk_signal_connect (GTK_OBJECT (retval->html), "button_press_event",
                      GTK_SIGNAL_FUNC (gnc_html_button_press_cb), 
                      (gpointer)retval);
  
  gtk_signal_connect (GTK_OBJECT(retval->html), "key_press_event", 
                      GTK_SIGNAL_FUNC(gnc_html_key_cb), (gpointer)retval);
  
  gtk_widget_show_all(GTK_WIDGET(retval->html));
  
  gtk_html_load_empty(GTK_HTML(retval->html));
  
  return retval;
}


/********************************************************************\
 * gnc_html_destroy
 * destroy the struct
\********************************************************************/

void
gnc_html_destroy(gnc_html * html) {
  gtk_widget_unref(html->container);
  gnc_html_history_destroy(html->history);
  if(html->request) {
    ghttp_request_destroy(html->request);
  }

  g_free(html->current_link);
  g_free(html->base_location);
  g_free(html);
}

void
gnc_html_set_urltype_cb(gnc_html * html, GncHTMLUrltypeCB urltype_cb) {
  html->urltype_cb = urltype_cb;
}

void
gnc_html_set_load_cb(gnc_html * html, GncHTMLLoadCB load_cb,
                     gpointer data) {
  html->load_cb = load_cb;
  html->load_cb_data = data;
}

void
gnc_html_set_flyover_cb(gnc_html * html, GncHTMLFlyoverCB flyover_cb,
                        gpointer data) {
  html->flyover_cb       = flyover_cb;
  html->flyover_cb_data  = data;
}

void
gnc_html_set_button_cb(gnc_html * html, GncHTMLButtonCB button_cb,
                        gpointer data) {
  html->button_cb       = button_cb;
  html->button_cb_data  = data;
}

void
gnc_html_print(gnc_html * html) {
  PrintSession * ps = gnc_print_session_create();
  
  gtk_html_print(GTK_HTML(html->html),
                 GNOME_PRINT_CONTEXT(ps->meta));
  gnc_print_session_done(ps);
  gnc_print_session_print(ps);
}

gnc_html_history * 
gnc_html_get_history(gnc_html * html) {
  return html->history;
}


GtkWidget * 
gnc_html_get_widget(gnc_html * html) {
  return html->container;
}


#ifdef _TEST_GNC_HTML_
int
main(int argc, char ** argv) {
  
  GtkWidget * wind;
  gnc_html  * html;
 
  gnome_init("test", "1.0", argc, argv);
  gdk_rgb_init();
  gtk_widget_set_default_colormap (gdk_rgb_get_cmap ());
  gtk_widget_set_default_visual (gdk_rgb_get_visual ());
  
  wind = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  html = gnc_html_new();

  gtk_container_add(GTK_CONTAINER(wind), GTK_WIDGET(html->container));
  gtk_widget_show_all(wind);

  gnc_html_load_file(html, "test.html");

  gtk_main();

}
#endif

