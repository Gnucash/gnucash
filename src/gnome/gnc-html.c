/********************************************************************
 * gnc-html.c -- display HTML with some special gnucash tags.       *
 *                                                                  *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
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
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

#include <gtkhtml/gtkhtml.h>
#include <gtkhtml/gtkhtml-embedded.h>
#ifdef USE_GUPPI
#include <libguppitank/guppi-tank.h>
#endif
#include <gnome.h>
#include <regex.h>
#include <glib.h>
#include <guile/gh.h>

#include "Account.h"
#include "Group.h"
#include "RegWindow.h"
#include "File.h"
#include "FileBox.h"
#include "FileDialog.h"
#include "dialog-utils.h"
#include "window-register.h"
#include "print-session.h"
#include "global-options.h"
#include "gnc-engine-util.h"
#include "gnc-gpg.h"
#include "gnc-html.h"
#include "gnc-http.h"
#include "gnc-html-history.h"
#include "gnc-network.h"
#include "query-user.h"
#include "window-help.h"
#include "window-main.h"
#include "window-report.h"
#include "messages.h"


struct _gnc_html {
  GtkWidget   * container;         /* parent of the gtkhtml widget */
  GtkWidget   * html;              /* gtkhtml widget itself */
  gchar       * current_link;      /* link under mouse pointer */

  URLType     base_type;           /* base of URL (path - filename) */
  gchar       * base_location;

  gnc_http    * http;              /* handles HTTP requests */ 
  GHashTable  * request_info;      /* hash uri to GList of GtkHTMLStream * */

  /* callbacks */
  GncHTMLUrltypeCB  urltype_cb;     /* is this type OK for this instance? */
  GncHTMLLoadCB     load_cb;
  GncHTMLFlyoverCB  flyover_cb;
  GncHTMLButtonCB   button_cb;
  
  gpointer          flyover_cb_data;
  gpointer          load_cb_data;
  gpointer          button_cb_data;
  
  struct _gnc_html_history * history; 
};


/* indicates the debugging module that this .o belongs to.  */
static short module = MOD_HTML;

/* hashes an HTML <object classid="ID"> classid to a handler function */
static GHashTable * gnc_html_object_handlers = NULL;

/* hashes an action name from a FORM definition to a handler function.
 * <form method=METHOD action=gnc-action:ACTION-NAME?ACTION-ARGS> 
 * action-args is what gets passed to the handler. */
static GHashTable * gnc_html_action_handlers = NULL;

static char error_404[] = 
"<html><body><h3>Not found</h3><p>The specified URL could not be loaded.</body></html>";

static char error_start[] = "<html><body><h3>Error</h3><p>There was an error loading the specified URL. <p>Error message: <p> ";
static char error_end[] = "</body></html>";

static char error_report[] = 
"<html><body><h3>Report error</h3><p>An error occurred while running the report.</body></html>";


static char * 
extract_machine_name(const gchar * path) {
  char       machine_rexp[] = "^(//[^/]*)/*(.*)?$";
  regex_t    compiled_m;
  regmatch_t match[4];
  char       * machine=NULL;

  if(!path) return NULL;
  
  regcomp(&compiled_m, machine_rexp, REG_EXTENDED);
  
  /* step 1: split the machine name away from the path
   * components */
  if(!regexec(&compiled_m, path, 4, match, 0)) {
    /* $1 is the machine name */ 
    if(match[1].rm_so != -1) {
      machine = g_strndup(path+match[1].rm_so, 
                          match[1].rm_eo - match[1].rm_so);
    } 
  }
  return machine;
}


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
    else if(!strcmp(protocol, "gnc-action")) {
      retval = URL_TYPE_ACTION;
    }
    else if(!strcmp(protocol, "gnc-register")) {
      retval = URL_TYPE_REGISTER;
    } 
    else if(!strcmp(protocol, "gnc-acct-tree")) {
      retval = URL_TYPE_ACCTTREE;
    }
    else if(!strcmp(protocol, "gnc-report")) {
      retval = URL_TYPE_REPORT;
    }
    else if(!strcmp(protocol, "gnc-options")) {
      retval = URL_TYPE_OPTIONS;
    }
    else if(!strcmp(protocol, "gnc-scm")) {
      retval = URL_TYPE_SCHEME;
    }
    else if(!strcmp(protocol, "gnc-help")) {
      retval = URL_TYPE_HELP;
    }
    else {
      PWARN("unhandled URL type for '%s'", url ? url : "(null)");
      retval = URL_TYPE_OTHER;
    }
  }
  else if(found_label && !found_path) {
    retval = URL_TYPE_JUMP;
  }
  else {
    if(html) {
      retval = html->base_type;
    }
    else {
      retval = URL_TYPE_FILE;
    }
  }
  
  g_free(protocol);
 
  switch(retval) {
  case URL_TYPE_FILE:    
    if(!found_protocol && path && html && html->base_location) {
      if(path[0] == '/') {
        *url_location = g_strdup(path);
      }
      else {
        *url_location = g_strconcat(html->base_location, path, NULL);
      }
      g_free(path);
    }
    else {
      *url_location = g_strdup(path);
      g_free(path);
    }
    break;
    
  case URL_TYPE_JUMP:
    *url_location = NULL;
    g_free(path);
    break;

  case URL_TYPE_OTHER:
  default:
    if(!found_protocol && path && html && html->base_location) {
      if(path[0] == '/') {
        *url_location = 
          g_strconcat(extract_machine_name(html->base_location),
                      "/", path+1, NULL);
      }
      else {
        *url_location = g_strconcat(html->base_location, path, NULL);
      }
      g_free(path);
    }
    else {
      *url_location = g_strdup(path);
      g_free(path);
    }
    break;
  }
  
  *url_label = label;
  return retval;
}


static char * 
extract_base_name(URLType type, const gchar * path) {
  char       machine_rexp[] = "^(//[^/]*)/*(/.*)?$";
  char       path_rexp[] = "^/*(.*)/+([^/]*)$";
  regex_t    compiled_m, compiled_p;
  regmatch_t match[4];
  char       * machine=NULL, * location = NULL, * base=NULL;
  char       * basename=NULL;

  if(!path) return NULL;
  
  regcomp(&compiled_m, machine_rexp, REG_EXTENDED);
  regcomp(&compiled_p, path_rexp, REG_EXTENDED);

  switch(type) {
  case URL_TYPE_HTTP:
  case URL_TYPE_SECURE:
  case URL_TYPE_FTP:
    /* step 1: split the machine name away from the path
     * components */
    if(!regexec(&compiled_m, path, 4, match, 0)) {
      /* $1 is the machine name */ 
      if(match[1].rm_so != -1) {
        machine = g_strndup(path+match[1].rm_so, 
                            match[1].rm_eo - match[1].rm_so);
      } 
      /* $2 is the path */
      if(match[2].rm_so != -1) {
        location = g_strndup(path+match[2].rm_so, 
                             match[2].rm_eo - match[2].rm_so);
      }
    }  
    break;
  default:
    location = g_strdup(path);
  }
  /* step 2: split up the path into prefix and file components */ 
  if(location) {
    if(!regexec(&compiled_p, location, 4, match, 0)) {
      if(match[1].rm_so != -1) {
        base = g_strndup(location+match[1].rm_so, 
                         match[1].rm_eo - match[1].rm_so);
      }
      else {
        base = NULL;
      }
    }
  }
  
  regfree(&compiled_m);
  regfree(&compiled_p);
  
  if(machine) {
    if(base && (strlen(base) > 0)) {
      basename = g_strconcat(machine, "/", base, "/", NULL);
    }
    else {
      basename = g_strconcat(machine, "/", NULL);
    }
  }
  else {
    if(base && (strlen(base) > 0)) {
      basename = g_strdup(base);
    }
    else {
      basename = NULL;
    }
  }

  g_free(machine);
  g_free(base);
  g_free(location);
  return basename;
}

static char * url_type_names[] = {
  "file:", "", "http:", "ftp:", "https:", 
  "gnc-register:", "gnc-acct-tree:", "gnc-report:", "gnc-options:", "gnc-scm:",
  "gnc-help:", "gnc-xml:", "gnc-action:", ""
};


static gchar  *
rebuild_url(URLType type, const gchar * location, const gchar * label) {
  if(label) {
    return g_strdup_printf("%s%s#%s", url_type_names[type], 
                           (location ? location : ""),
                           label ? label : "");
  }
  else {
    return g_strdup_printf("%s%s", url_type_names[type], 
                           (location ? location : ""));
  }
}

static gboolean
http_allowed() {
  return gnc_lookup_boolean_option("Network", "Allow http network access", 
                                   TRUE);
}

static gboolean
https_allowed() {
  return gnc_lookup_boolean_option("Network", "Allow https network access", 
                                   TRUE);
}

static gboolean
gnc_network_allowed() {
  return gnc_lookup_boolean_option("Network", "Enable GnuCash Network", 
                                   TRUE);
}



/************************************************************
 * gnc_html_http_request_cb: fires when an HTTP request is completed.
 * this is when it's time to load the data into the GtkHTML widget. 
 ************************************************************/

static void
gnc_html_http_request_cb(const gchar * uri, int completed_ok, 
                         const gchar * body, gint body_len, 
                         gpointer user_data) {
  gnc_html * html = user_data; 
  URLType  type;
  char     * location = NULL;
  char     * label    = NULL;
  GList    * handles  = NULL;
  GList    * current;
  
  g_hash_table_lookup_extended(html->request_info, uri, 
                               (gpointer *)&location, 
                               (gpointer *)&handles);
  
  /* handles will be NULL for an HTTP POST transaction, where we are
   * displaying the reply data. */
  if(!handles) {    
    GtkHTMLStream * handle = gtk_html_begin(GTK_HTML(html->html));
    if(completed_ok) {
      gtk_html_write(GTK_HTML(html->html), handle, body, body_len);
    }
    else {
      gtk_html_write(GTK_HTML(html->html), handle,
                     error_start, strlen(error_start));
      gtk_html_write(GTK_HTML(html->html), handle, 
                     body, body_len);
      gtk_html_write(GTK_HTML(html->html), handle,
                     error_end, strlen(error_end));
      gtk_html_end(GTK_HTML(html->html), handle, GTK_HTML_STREAM_OK);
    }
  }
  /* otherwise, it's a normal SUBMIT transaction */ 
  else {
    /* before writing to the handles, make sure any new traffic won't
     * see them while we're working */
    g_hash_table_remove(html->request_info, uri);
    g_free(location);
    location = NULL;

    for(current = handles; current; current = current->next) {
      /* request completed OK... write the HTML to the handles that
       * asked for that URI. */
      if(completed_ok) {
        gtk_html_write(GTK_HTML(html->html), (GtkHTMLStream *)(current->data),
                       body, body_len);
        gtk_html_end(GTK_HTML(html->html), (GtkHTMLStream *)(current->data), 
                     GTK_HTML_STREAM_OK);
        type = gnc_html_parse_url(html, uri, &location, &label);
        if(label) {
          gtk_html_jump_to_anchor(GTK_HTML(html->html), label);
        }
        g_free(location);
        g_free(label);
        location = label = NULL;
      }
      /* request failed... body is the ghttp error text. */
      else {
        gtk_html_write(GTK_HTML(html->html), (GtkHTMLStream *)(current->data), 
                       error_start, strlen(error_start));
        gtk_html_write(GTK_HTML(html->html), (GtkHTMLStream *)(current->data), 
                       body, body_len);
        gtk_html_write(GTK_HTML(html->html), (GtkHTMLStream *)(current->data), 
                       error_end, strlen(error_end));
        gtk_html_end(GTK_HTML(html->html), (GtkHTMLStream *)(current->data), 
                     GTK_HTML_STREAM_ERROR);
      }
    }
    g_list_free(handles);    
  }
  
  gnc_unset_busy_cursor (html->html);
}


/************************************************************
 * gnc_html_start_request: starts the gnc-http object working on an
 * http/https request.
 ************************************************************/

static void 
gnc_html_start_request(gnc_html * html, gchar * uri, GtkHTMLStream * handle) {
  GList * handles = NULL;
  gint  need_request = FALSE;

  /* we want to make a list of handles to fill with this URI.
   * multiple handles with the same URI will all get filled when the
   * request comes in. */
  handles = g_hash_table_lookup(html->request_info, uri);
  if(!handles) {
    need_request = TRUE;
  }

  handles = g_list_append(handles, handle);
  g_hash_table_insert(html->request_info, uri, handles);
  
  if(need_request) {
    gnc_set_busy_cursor (html->html, FALSE);
    gnc_http_start_request(html->http, uri, gnc_html_http_request_cb, 
                           (gpointer)html);
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
    if(label) {
      gtk_html_jump_to_anchor(GTK_HTML(html->html), label);
    }
    break;
    
  case URL_TYPE_SECURE:
    if(!https_allowed()) {
      gnc_error_dialog("Secure HTTP access is disabled.\n"
                       "You can enable it in the Network section of\n"
                       "the Preferences dialog.");
      break;
    }
    
  case URL_TYPE_HTTP:
    if(!http_allowed()) {
      gnc_error_dialog("Network HTTP access is disabled.\n"
                       "You can enable it in the Network section of\n"
                       "the Preferences dialog.");
    }
    else {
      fullurl = rebuild_url(type, location, label);
      gnc_html_start_request(html, fullurl, handle);
    }
    break;
    
  case URL_TYPE_REPORT:
    run_report = gh_eval_str("gnc:report-run");

    if(!strncmp("id=", location, 3)) {
      /* get the report ID */
      sscanf(location+3, "%d", &id);
      
      /* get the HTML text */ 
      scmtext = gh_call1(run_report, gh_int2scm(id));
      if(scmtext == SCM_BOOL_F) {
        gtk_html_write(GTK_HTML(html->html), handle, 
                       error_report, strlen(error_report));        
      }
      else {
        fdata = gh_scm2newstr(scmtext, &fsize);
        if(fdata) {
          gtk_html_write(GTK_HTML(html->html), handle, fdata, fsize);
          TRACE ("%s", fdata);
          free(fdata);
          fdata = NULL;
          fsize = 0;
          if(label) {
            gtk_html_jump_to_anchor(GTK_HTML(html->html), label);
          }
        }
        else {
          gtk_html_write(GTK_HTML(html->html), handle, error_404, 
                         strlen(error_404));
          PWARN("report HTML generator failed.");
        }
      }
    }

    gtk_html_end(GTK_HTML(html->html), handle, GTK_HTML_STREAM_OK);
    break;
    
  case URL_TYPE_REGISTER:
  case URL_TYPE_ACCTTREE:
  case URL_TYPE_OPTIONS:
  case URL_TYPE_SCHEME:
  case URL_TYPE_FTP:
  default:
    PWARN("load_to_stream for inappropriate type\n"
          "\turl = '%s#%s'\n",
          location ? location : "(null)",
          label ? label : "(null)");
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


#ifdef USE_GUPPI
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
#endif /* USE_GUPPI */


/********************************************************************
 * gnc_html_object_requested_cb - called when an applet needs to be
 * loaded.  
 ********************************************************************/

static int
gnc_html_object_requested_cb(GtkHTML * html, GtkHTMLEmbedded * eb,
                             gpointer data) {
  GtkWidget * widg = NULL;
  gnc_html  * gnchtml = data; 
  int retval = FALSE;
  GncHTMLObjectCB h;

  if(!eb || !(eb->classid) || !gnc_html_object_handlers) return FALSE;
  
  h = g_hash_table_lookup(gnc_html_object_handlers, eb->classid);
  if(h) {
    return h(gnchtml, eb, data);
  }
  else {
    return FALSE;
  }
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


/********************************************************************
 * gnc_html_button_press_cb
 * mouse button callback (if any)
 ********************************************************************/

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
 * gnc_html_pack/unpack_form_data
 * convert an encoded arg string to/from a name-value hash table
 ********************************************************************/

GHashTable *
gnc_html_unpack_form_data(const char * encoding) {
  GHashTable * rv = g_hash_table_new(g_str_hash, g_str_equal);
  gnc_html_merge_form_data(rv, encoding);
  return rv;
}

void
gnc_html_merge_form_data(GHashTable * rv, const char * encoding) {
  char * next_pair = NULL; 
  char * name  = NULL;
  char * value = NULL;
  char * extr_name  = NULL;
  char * extr_value = NULL;
  
  if(!encoding) {
    return;
  }
  next_pair = g_strdup(encoding);

  while(next_pair) {
    name = next_pair;
    if((value = strchr(name, '=')) != NULL) {
      extr_name = g_strndup(name, value-name);
      next_pair = strchr(value, '&');
      if(next_pair) {
        extr_value = g_strndup(value+1, next_pair-value-1);
        next_pair++;
      }
      else {
        extr_value = g_strdup(value+1);
      }
      
      g_hash_table_insert(rv, 
                          gnc_html_decode_string(extr_name),
                          gnc_html_decode_string(extr_value));
      g_free(extr_name);
      g_free(extr_value);
    }
    else {
      next_pair = NULL;
    }
  }
}

static gboolean
free_form_data_helper(gpointer k, gpointer v, gpointer user) {
  g_free(k);
  g_free(v);
  return TRUE;
}

void 
gnc_html_free_form_data(GHashTable * d) {
  g_hash_table_foreach_remove(d, free_form_data_helper, NULL);
  g_hash_table_destroy(d);
}

static void
pack_form_data_helper(gpointer key, gpointer val, 
                      gpointer user_data) {
  char * old_str = *(char **)user_data;
  char * enc_key = gnc_html_encode_string((char *)key);
  char * enc_val = gnc_html_encode_string((char *)val);
  char * new_str = NULL;

  if(old_str) {
    new_str = g_strconcat(old_str, "&", enc_key, "=", enc_val, NULL);
  }
  else {
    new_str = g_strconcat(enc_key, "=", enc_val, NULL);
  }
  *(char **)user_data = new_str;
  g_free(old_str);
}

char *
gnc_html_pack_form_data(GHashTable * form_data) {
  char * encoded = NULL;
  g_hash_table_foreach(form_data, pack_form_data_helper, &encoded);
  return encoded;
}


/********************************************************************
 * gnc_html_button_submit_cb
 * form submission callback
 ********************************************************************/

static int
gnc_html_submit_cb(GtkHTML * html, const gchar * method, 
                   const gchar * action, const gchar * encoded_form_data,
                   gpointer user_data) {
  gnc_html * gnchtml = user_data;
  char     * location = NULL;
  char     * new_loc = NULL;
  char     * label = NULL;
  char     * submit_encoding = NULL;
  char     ** action_parts;
  GHashTable * form_data = gnc_html_unpack_form_data(encoded_form_data);
  URLType  type;
  GncHTMLActionCB cb;

  type = gnc_html_parse_url(gnchtml, action, &location, &label);
  
  if(type == URL_TYPE_ACTION) {
    if(gnc_network_allowed()) {
      if(gnc_html_action_handlers) {
        action_parts = g_strsplit(location, "?", 2);
        if(action_parts && action_parts[0]) {
          gnc_html_merge_form_data(form_data, action_parts[1]);
          cb = g_hash_table_lookup(gnc_html_action_handlers, action_parts[0]);
          if(cb) {
            cb(gnchtml, method, action_parts[0], form_data);
          }
          else {
            PWARN ("no handler for gnc-network action '%s'\n",
                   action ? action : "(null)");
          }
        }
        else {
          PWARN ("tried to split on ? but failed...\n");
        }
      }
    }
    else {
      gnc_error_dialog(_("GnuCash Network is disabled and the link "
                         "you have clicked requires it.\n"
                         "You can enable it in the Network section\n"
                         "of the Preferences dialog."));
    }
  }
  else {
    if(!strcasecmp(method, "get")) {
      gnc_html_generic_get_submit(gnchtml, action, form_data);
    }
    else if(!strcasecmp(method, "post")) {
      gnc_html_generic_post_submit(gnchtml, action, form_data);
    }
  }
  
  g_free(location);
  g_free(label);
  g_free(new_loc);
  gnc_html_free_form_data(form_data);
  return TRUE;
}


/********************************************************************
 * gnc_html_open_register
 * open a register window 
 ********************************************************************/

static void
gnc_html_open_register(gnc_html * html, const gchar * location) {
  RegWindow   * reg = NULL;
  Split       * split = NULL;
  Account     * acct;
  Transaction * trans;
  GList       * node;

  /* href="gnc-register:account=My Bank Account" */
  if(!strncmp("account=", location, 8)) {
    acct = xaccGetAccountFromFullName(gncGetCurrentGroup(),
                                      location+8, 
                                      gnc_get_account_separator());
    reg = regWindowSimple(acct);
    gnc_register_raise(reg);
  }
  /* href="gnc-register:guid=12345678901234567890123456789012" */
  else if(!strncmp("guid=", location, 5)) {
    GUID guid;

    if (!string_to_guid(location + 5, &guid))
    {
      PWARN ("Bad guid: %s", location + 5);
      return;
    }

    switch (xaccGUIDType (&guid))
    {
      case GNC_ID_NONE:
      case GNC_ID_NULL:
        PWARN ("No such entity: %s", location + 5);
        return;

      case GNC_ID_ACCOUNT:
        acct = xaccAccountLookup (&guid);
        reg = regWindowSimple(acct);
        break;

      case GNC_ID_TRANS:
        trans = xaccTransLookup (&guid);
        split = NULL;

        for (node = xaccTransGetSplitList (trans); node; node = node->next)
        {
          split = node->data;
          if (xaccSplitGetAccount (split))
            break;
        }

        if (!split)
          return;

        reg = regWindowSimple (xaccSplitGetAccount (split));
        break;

      case GNC_ID_SPLIT:
        split = xaccSplitLookup (&guid);
        if (!split)
          return;

        reg = regWindowSimple (xaccSplitGetAccount (split));
        break;

      default:
        return;
    }

    gnc_register_raise(reg);
    if (split)
      gnc_register_jump_to_split (reg, split);
  }
  else {
    gnc_warning_dialog(_("Badly formed gnc-register: URL."));
  }
}

/********************************************************************
 * gnc_html_open_options
 * open an editor for report parameters 
 ********************************************************************/

static void
gnc_html_open_options(gnc_html * html, const gchar * location) {
  int report_id;
  SCM find_report = gh_eval_str("gnc:find-report");
  SCM get_options = gh_eval_str("gnc:report-options");
  SCM get_editor = gh_eval_str("gnc:report-options-editor");
  
  SCM report;
  SCM options;
  SCM editor;

  /* href="gnc-options:report-id=2676" */
  if(!strncmp("report-id=", location, 10)) {
    sscanf(location+10, "%d", &report_id);
    report = gh_call1(find_report, gh_int2scm(report_id));
    options = gh_call1(get_options, report);
    editor = gh_call1(get_editor, report);
    gh_call2(editor, options, report);
  }
  else {
    gnc_warning_dialog(_("Badly formed gnc-options: URL."));
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
  GtkHTMLStream * handle;
  char * rebuilt_url;

  /* make a new window if necessary */ 
  if(newwin) {
    rebuilt_url = rebuild_url(URL_TYPE_REPORT, location, label);
    gnc_main_window_open_report_url(rebuilt_url, FALSE);
    g_free(rebuilt_url);
  }
  else {
    gnc_html_history_append(html->history,
                            gnc_html_history_node_new(URL_TYPE_REPORT, 
                                                      location, label));
    
    g_free(html->base_location);
    html->base_type     = URL_TYPE_FILE;
    html->base_location = NULL;
    
    handle = gtk_html_begin(GTK_HTML(html->html));
    gnc_html_load_to_stream(html, handle, URL_TYPE_REPORT, location, label);
  }
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
  PINFO("location='%s'", location ? location : "(null)");
}


/********************************************************************
 * gnc_html_show_data
 * display some HTML that the creator of the gnc-html got from 
 * somewhere. 
 ********************************************************************/

void
gnc_html_show_data(gnc_html * html, const char * data, 
                   int datalen) {
  GtkHTMLStream * handle = gtk_html_begin(GTK_HTML(html->html));
  gtk_html_write(GTK_HTML(html->html), handle, data, datalen);
  gtk_html_end(GTK_HTML(html->html), handle, GTK_HTML_STREAM_OK);  
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

  if (!html) return;
  if (!location) return;

  /* make sure it's OK to show this URL type in this window */
  if(newwin_hint == 0) {
    if (html->urltype_cb)
      newwin = !((html->urltype_cb)(type));
    else
      newwin = 0;
  }
  else {
    newwin = 1;
  }

  if(!newwin) {
    gnc_html_cancel(html);
  }

  switch(type) {
  case URL_TYPE_REGISTER:
    gnc_html_open_register(html, location);
    break;

  case URL_TYPE_REPORT:
    gnc_html_open_report(html, location, label, newwin);
    break;

  case URL_TYPE_OPTIONS:
    gnc_html_open_options(html, location);
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
    
  case URL_TYPE_SECURE:
    if(!https_allowed()) {
      gnc_error_dialog(_("Secure HTTP access is disabled.\n"
                         "You can enable it in the Network section of\n"
                         "the Preferences dialog."));
      break;
    }
  case URL_TYPE_HTTP:
    if(!http_allowed()) {
      gnc_error_dialog(_("Network HTTP access is disabled.\n"
                         "You can enable it in the Network section of\n"
                         "the Preferences dialog."));
      break;
    }
  case URL_TYPE_FILE:
    html->base_type     = type;
    
    if(html->base_location) g_free(html->base_location);
    html->base_location = extract_base_name(type, location);

    /* FIXME : handle newwin = 1 */
    gnc_html_history_append(html->history,
                            gnc_html_history_node_new(type, location, label));
    handle = gtk_html_begin(GTK_HTML(html->html));
    gnc_html_load_to_stream(html, handle, type, location, label);
    break;
    
  case URL_TYPE_ACTION:
    gnc_html_history_append(html->history,
                            gnc_html_history_node_new(type, 
                                                      location, label));
    gnc_html_submit_cb(GTK_HTML(html->html), "get", 
                       rebuild_url(type, location, label), NULL,
                       (gpointer)html);
    break;
    
  URL_TYPE_ACCTTREE:
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


/********************************************************************
 * gnc_html_new
 * create and set up a new gtkhtml widget.
 ********************************************************************/

gnc_html * 
gnc_html_new(void) {
  gnc_html * retval = g_new0(gnc_html, 1);
  
  retval->container = gtk_scrolled_window_new(NULL, NULL);
  retval->html      = gtk_html_new();

  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(retval->container),
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);

  gtk_container_add(GTK_CONTAINER(retval->container), 
                    GTK_WIDGET(retval->html));

  retval->request_info = g_hash_table_new(g_str_hash, g_str_equal);
  retval->http         = gnc_http_new();
  retval->history      = gnc_html_history_new();

  gtk_widget_ref (retval->container);
  gtk_object_sink (GTK_OBJECT (retval->container));

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
  
  gtk_signal_connect (GTK_OBJECT(retval->html), "submit", 
                      GTK_SIGNAL_FUNC(gnc_html_submit_cb), (gpointer)retval);
  
  gtk_widget_show_all(GTK_WIDGET(retval->html));
  
  gtk_html_load_empty(GTK_HTML(retval->html));
  
  return retval;
}


/********************************************************************
 * gnc_html_cancel
 * cancel any outstanding HTML fetch requests. 
 ********************************************************************/

static gboolean
html_cancel_helper(gpointer key, gpointer value, gpointer user_data) {
  g_free(key);
  g_list_free((GList *)value);
  return TRUE;
}

void
gnc_html_cancel(gnc_html * html) {
  /* remove our own references to requests */ 
  gnc_http_cancel_requests(html->http);
  
  g_hash_table_foreach_remove(html->request_info, html_cancel_helper, NULL);
}


/********************************************************************
 * gnc_html_destroy
 * destroy the struct
 ********************************************************************/

void
gnc_html_destroy(gnc_html * html) {

  if(!html) return;

  /* cancel any outstanding HTTP requests */
  gnc_html_cancel(html);
  
  gnc_html_history_destroy(html->history);

  gtk_widget_destroy(html->container);
  gtk_widget_unref(html->container);

  g_free(html->current_link);
  g_free(html->base_location);

  html->container     = NULL;
  html->html          = NULL;
  html->history       = NULL;
  html->current_link  = NULL;
  html->base_location = NULL;

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

/**************************************************************
 * gnc_html_export : wrapper around the builtin function in gtkhtml
 **************************************************************/

static gboolean 
raw_html_receiver (gpointer     engine,
                   const gchar *data,
                   guint        len,
                   gpointer     user_data) {
  FILE *fh = (FILE *) user_data;
  fwrite (data, len, 1, fh);
  return TRUE;
}

void
gnc_html_export(gnc_html * html) {
  const char *filepath;
  FILE *fh;
  
  filepath = fileBox (_("Save HTML To File"), NULL, NULL);
  if (!filepath)
    return;

  PINFO (" user selected file=%s\n", filepath ? filepath : "(null)");

  fh = fopen (filepath, "w");
  if (NULL == fh) {
    const char *fmt = _("Could not open the file\n"
                        "     %s\n%s");
    char *buf = g_strdup_printf (fmt, filepath ? filepath : "(null)",
                                 strerror (errno) ? strerror (errno) : "");
    gnc_error_dialog (buf);
    if (buf) g_free (buf);
    return;
  }
  
  gtk_html_save (GTK_HTML(html->html), raw_html_receiver, fh);
  fclose (fh);
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
  if (!html) return NULL;
  return html->history;
}


GtkWidget * 
gnc_html_get_widget(gnc_html * html) {
  if (!html) return NULL;
  return html->container;
}

void
gnc_html_register_object_handler(const char * classid, 
                                 GncHTMLObjectCB hand) {
  if(!gnc_html_object_handlers) {
    gnc_html_object_handlers = g_hash_table_new(g_str_hash, g_str_equal);
  }
  g_hash_table_insert(gnc_html_object_handlers, g_strdup(classid), hand);
}

void
gnc_html_unregister_object_handler(const char * classid) {
  gchar * keyptr=NULL;
  gchar * valptr=NULL;

  g_hash_table_lookup_extended(gnc_html_object_handlers,
                               classid, 
                               (gpointer *)&keyptr, 
                               (gpointer *)&valptr);
  if(keyptr) {
    g_free(keyptr);
    g_hash_table_remove(gnc_html_object_handlers, classid);
  }
}

void
gnc_html_register_action_handler(const char * actionid, 
                                 GncHTMLActionCB hand) {
  if(!gnc_html_action_handlers) {
    gnc_html_action_handlers = g_hash_table_new(g_str_hash, g_str_equal);
  }
  g_hash_table_insert(gnc_html_action_handlers, g_strdup(actionid), hand);
}

void
gnc_html_unregister_action_handler(const char * actionid) {
  gchar * keyptr=NULL;
  gchar * valptr=NULL;

  g_hash_table_lookup_extended(gnc_html_action_handlers,
                               actionid, 
                               (gpointer *)&keyptr, 
                               (gpointer *)&valptr);
  if(keyptr) {
    g_free(keyptr);
    g_hash_table_remove(gnc_html_action_handlers, actionid);
  }
}


/********************************************************************
 * gnc_html_encode_string
 * RFC 1738 encoding of string for submission with an HTML form.
 * GPL code lifted from gtkhtml.  copyright notice: 
 * 
 * Copyright (C) 1997 Martin Jones (mjones@kde.org)
 * Copyright (C) 1997 Torben Weis (weis@kde.org)
 * Copyright (C) 1999 Helix Code, Inc.
 ********************************************************************/

char *
gnc_html_encode_string(const char * str) {
  static gchar *safe = "$-._!*(),"; /* RFC 1738 */
  unsigned pos      = 0;
  GString *encoded  = g_string_new ("");
  gchar buffer[5], *ptr;
  guchar c;
  
  if(!str) return NULL;

  while(pos < strlen(str)) {
    c = (unsigned char) str[pos];
    
    if ((( c >= 'A') && ( c <= 'Z')) ||
        (( c >= 'a') && ( c <= 'z')) ||
        (( c >= '0') && ( c <= '9')) ||
        (strchr(safe, c))) {
      encoded = g_string_append_c (encoded, c);
    }
    else if ( c == ' ' ) {
      encoded = g_string_append_c (encoded, '+');
    }
    else if ( c == '\n' ) {
      encoded = g_string_append (encoded, "%0D%0A");
    }
    else if ( c != '\r' ) {
      sprintf( buffer, "%%%02X", (int)c );
      encoded = g_string_append (encoded, buffer);
    }
    pos++;
  }
  
  ptr = encoded->str;
  
  g_string_free (encoded, FALSE);
  
  return (char *)ptr;  
}


char *
gnc_html_decode_string(const char * str) {
  static gchar * safe = "$-._!*(),"; /* RFC 1738 */
  GString * decoded  = g_string_new ("");
  const gchar   * ptr;
  guchar  c;
  guint   hexval;
  ptr = str;
  
  if(!str) return NULL;

  while(*ptr) {
    c = (unsigned char) *ptr;
    if ((( c >= 'A') && ( c <= 'Z')) ||
        (( c >= 'a') && ( c <= 'z')) ||
        (( c >= '0') && ( c <= '9')) ||
        (strchr(safe, c))) {
      decoded = g_string_append_c (decoded, c);
    }
    else if ( c == '+' ) {
      decoded = g_string_append_c (decoded, ' ');
    }
    else if (!strncmp(ptr, "%0D0A", 5)) {
      decoded = g_string_append (decoded, "\n");
      ptr += 4;
    }
    else if(c == '%') {
      ptr++;
      sscanf(ptr, "%02X", &hexval);
      ptr++;
      decoded = g_string_append_c(decoded, (char)hexval);
    }
    ptr++;
  }
  ptr = decoded->str;
  g_string_free (decoded, FALSE);  

  return (char *)ptr;  
}

/********************************************************************
 * escape/unescape_newlines : very simple string encoding for GPG
 * ASCII-armored text.
 ********************************************************************/

char * 
gnc_html_unescape_newlines(const gchar * in) {
  const char * ip = in;
  char    * cstr = NULL;
  GString * rv = g_string_new("");

  for(ip=in; *ip; ip++) {
    if((*ip == '\\') && (*(ip+1)=='n')) {
      g_string_append(rv, "\n");
      ip++; 
    }
    else {
      g_string_append_c(rv, *ip);
    }
  }

  g_string_append_c(rv, 0);
  cstr = rv->str;
  g_string_free(rv, FALSE);
  return cstr;
}

char * 
gnc_html_escape_newlines(const gchar * in) {
  char *out;
  const char * ip   = in;
  GString * escaped = g_string_new("");

  for(ip=in; *ip; ip++) {
    if(*ip == '\012') {
      g_string_append(escaped, "\\n");
    }
    else {
      g_string_append_c(escaped, *ip);      
    }
  }
  g_string_append_c(escaped, 0);
  out = escaped->str;
  g_string_free(escaped, FALSE);
  return out;
}


/********************************************************************
 * gnc_html_generic_get_submit() : normal 'get' submit method. 
 ********************************************************************/

void
gnc_html_generic_get_submit(gnc_html * html, const char * action, 
                            GHashTable * form_data) {
  URLType type;
  char    * location = NULL;
  char    * label = NULL;
  char    * fullurl = NULL;
  char    * encoded = gnc_html_pack_form_data(form_data);

  type    = gnc_html_parse_url(html, action, &location, &label);
  fullurl = g_strconcat(location, "?", encoded, NULL);
  gnc_html_show_url(html, type, fullurl, label, 0);

  g_free(encoded);
  g_free(location);
  g_free(label);
  g_free(fullurl);
}


/********************************************************************
 * gnc_html_generic_post_submit() : normal 'post' submit method. 
 ********************************************************************/

void
gnc_html_generic_post_submit(gnc_html * html, const char * action, 
                             GHashTable * form_data) {
  char * encoded = gnc_html_pack_form_data(form_data);
  char * copy = strdup(encoded);
  gnc_http_start_post(html->http, action, 
                      "application/x-www-form-urlencoded",
                      copy, strlen(copy), 
                      gnc_html_http_request_cb, html);
  g_free(encoded);
}


/********************************************************************
 * gnc_html_multipart_post_submit() : this is really sort of useless
 * but I'll make it better later.  It's useless because FTMP CGI/php
 * don't properly decode the urlencoded values.
 ********************************************************************/

static void
multipart_post_helper(gpointer key, gpointer val, 
                      gpointer user_data) {
  char * old_str = *(char **)user_data;
  char * new_str = 
    g_strconcat(old_str,
                "--XXXgncXXX\r\n",
                "Content-Disposition: form-data; name=\"",
                (char *)key, "\"\r\n\r\n",
                (char *)val, "\r\n",
                NULL);
  *(char **)user_data = new_str;
  g_free(old_str);
}


void
gnc_html_multipart_post_submit(gnc_html * html, const char * action, 
                               GHashTable * form_data) {

  char * htmlstr = g_strdup("");
  char * next_htmlstr;

  /* encode the arguments from the hash table */
  g_hash_table_foreach(form_data, multipart_post_helper, &htmlstr);
  
  /* add the closing boundary marker */
  next_htmlstr = g_strconcat(htmlstr, "--XXXgncXXX--\r\n", NULL);
  g_free(htmlstr);
  htmlstr = next_htmlstr;
  next_htmlstr = NULL;
  gnc_http_start_post(html->http, action, 
                      "multipart/form-data; boundary=XXXgncXXX",
                      htmlstr, strlen(htmlstr), 
                      gnc_html_http_request_cb, html);

  g_free(htmlstr);
}
