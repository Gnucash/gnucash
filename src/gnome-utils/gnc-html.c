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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/

// libgtkhtml docs:
// http://www.fifi.org/doc/libgtkhtml-dev/html/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <regex.h>
#include <libguile.h>

#include <gtkhtml/gtkhtml.h>
#include <gtkhtml/gtkhtml-embedded.h>

#include "Account.h"
#include "print-session.h"
#include "gnc-engine.h"
#include "gnc-gui-query.h"
#include "gnc-html.h"
#include "gnc-html-history.h"
#include "gnc-html-graph-gog.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"

/* Do not treat -Wstrict-aliasing warnings as errors because of problems of the
 * G_LOCK* macros as declared by glib.  See
 * http://bugzilla.gnome.org/show_bug.cgi?id=316221 for additional information.
 */
#if (__GNUC__ >= 4 && __GNUC_MINOR__ >= 2)
#    pragma GCC diagnostic warning "-Wstrict-aliasing"
#endif


struct gnc_html_struct {
  GtkWidget   * window;            /* window this html goes into */
  GtkWidget   * container;         /* parent of the gtkhtml widget */
  GtkWidget   * html;              /* gtkhtml widget itself */
  gchar       * current_link;      /* link under mouse pointer */

  URLType     base_type;           /* base of URL (path - filename) */
  gchar       * base_location;

    //gnc_http    * http;              /* handles HTTP requests */ 
  GHashTable  * request_info;      /* hash uri to GList of GtkHTMLStream * */

  /* callbacks */
  GncHTMLUrltypeCB  urltype_cb;     /* is this type OK for this instance? */
  GncHTMLLoadCB     load_cb;
  GncHTMLFlyoverCB  flyover_cb;
  GncHTMLButtonCB   button_cb;
  
  gpointer          flyover_cb_data;
  gpointer          load_cb_data;
  gpointer          button_cb_data;
  
  gnc_html_history * history; 
};


/* indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_HTML;

/* hashes for URLType -> protocol and protocol -> URLType */
static GHashTable * gnc_html_type_to_proto_hash = NULL;
static GHashTable * gnc_html_proto_to_type_hash = NULL;

/* hashes an HTML <object classid="ID"> classid to a handler function */
static GHashTable * gnc_html_object_handlers = NULL;

/* hashes an action name from a FORM definition to a handler function.
 * <form method=METHOD action=gnc-action:ACTION-NAME?ACTION-ARGS> 
 * action-args is what gets passed to the handler. */
static GHashTable * gnc_html_action_handlers = NULL;

/* hashes handlers for loading different URLType data */
static GHashTable * gnc_html_stream_handlers = NULL;

/* hashes handlers for handling different URLType data */
static GHashTable * gnc_html_url_handlers = NULL;

static char error_404_format[] =
"<html><body><h3>%s</h3><p>%s</body></html>";
static char error_404_title[] = N_("Not found");
static char error_404_body[] = 
N_("The specified URL could not be loaded.");

#ifdef GTKHTML_USES_GTKPRINT
static GtkPrintSettings *print_settings = NULL;
G_LOCK_DEFINE_STATIC(print_settings);
#endif


static char * 
extract_machine_name(const gchar * path)
{
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


/* Register the URLType if it doesn't already exist.
 * Returns TRUE if successful, FALSE if the type already exists.
 */
gboolean
gnc_html_register_urltype (URLType type, const char *protocol)
{
  if (!gnc_html_type_to_proto_hash) {
    gnc_html_type_to_proto_hash = g_hash_table_new (g_str_hash, g_str_equal);
    gnc_html_proto_to_type_hash = g_hash_table_new (g_str_hash, g_str_equal);
  }
  if (!protocol) return FALSE;
  if (g_hash_table_lookup (gnc_html_type_to_proto_hash, type))
    return FALSE;

  g_hash_table_insert (gnc_html_type_to_proto_hash, type, (gpointer)protocol);
  if (*protocol)
    g_hash_table_insert (gnc_html_proto_to_type_hash, (gpointer)protocol, type);

  return TRUE;
}

/********************************************************************
 * gnc_html_parse_url
 * this takes a URL and determines the protocol type, location, and
 * possible anchor name from the URL.
 ********************************************************************/

URLType
gnc_html_parse_url(gnc_html * html, const gchar * url, 
                   char ** url_location, char ** url_label)
{
  char        uri_rexp[] = "^(([^:][^:]+):)?([^#]+)?(#(.*))?$";
  regex_t     compiled;
  regmatch_t  match[6];
  char        * protocol=NULL, * path=NULL, * label=NULL;
  int         found_protocol=0, found_path=0, found_label=0; 
  URLType     retval;   

  DEBUG("parsing %s, base_location %s",
        url ? url : "(null)",
        html ? (html->base_location ? html->base_location
                : "(null base_location)")
        : "(null html)");
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
    retval = g_hash_table_lookup (gnc_html_proto_to_type_hash, protocol);
    if (!retval) {
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
 
  if (!safe_strcmp (retval, URL_TYPE_FILE)) {
    if(!found_protocol && path && html && html->base_location) {
      if (g_path_is_absolute(path)) {
        *url_location = g_strdup(path);
      }
      else {
        *url_location =
          g_build_filename(html->base_location, path, (gchar*)NULL);
      }
      g_free(path);
    }
    else {
      *url_location = g_strdup(path);
      g_free(path);
    }

  } else if (!safe_strcmp (retval, URL_TYPE_JUMP)) {
    *url_location = NULL;
    g_free(path);

  } else {
    /* case URL_TYPE_OTHER: */

    if(!found_protocol && path && html && html->base_location) {
      if (g_path_is_absolute(path)) {
        *url_location =
          g_build_filename(extract_machine_name(html->base_location),
                           path, (gchar*)NULL);
      }
      else {
        *url_location =
          g_build_filename(html->base_location, path, (gchar*)NULL);
      }
      g_free(path);
    }
    else {
      *url_location = g_strdup(path);
      g_free(path);
    }
  }
  
  *url_label = label;
  return retval;
}


static char * 
extract_base_name(URLType type, const gchar * path)
{
  char       machine_rexp[] = "^(//[^/]*)/*(/.*)?$";
  char       path_rexp[] = "^/*(.*)/+([^/]*)$";
  regex_t    compiled_m, compiled_p;
  regmatch_t match[4];
  char       * machine=NULL, * location = NULL, * base=NULL;
  char       * basename=NULL;

  DEBUG(" ");
  if(!path) return NULL;
  
  regcomp(&compiled_m, machine_rexp, REG_EXTENDED);
  regcomp(&compiled_p, path_rexp, REG_EXTENDED);

  if (!safe_strcmp (type, URL_TYPE_HTTP) ||
      !safe_strcmp (type, URL_TYPE_SECURE) ||
      !safe_strcmp (type, URL_TYPE_FTP)) {

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

  } else {
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

void
gnc_html_initialize (void)
{
  int i;
  static struct {
    URLType	type;
    char *	protocol;
  } types[] = {
    { URL_TYPE_FILE, "file" },
    { URL_TYPE_JUMP, "" },
    { URL_TYPE_HTTP, "http" },
    { URL_TYPE_FTP, "ftp" },
    { URL_TYPE_SECURE, "https" },
    { URL_TYPE_REGISTER, "gnc-register" },
    { URL_TYPE_ACCTTREE, "gnc-acct-tree" },
    { URL_TYPE_REPORT, "gnc-report" },
    { URL_TYPE_OPTIONS, "gnc-options" },
    { URL_TYPE_SCHEME, "gnc-scm" },
    { URL_TYPE_HELP, "gnc-help" },
    { URL_TYPE_XMLDATA, "gnc-xml" },
    { URL_TYPE_PRICE, "gnc-price" },
    { URL_TYPE_BUDGET, "gnc-budget" },
    { URL_TYPE_OTHER, "" },
    { NULL, NULL }};

  for (i = 0; types[i].type; i++)
    gnc_html_register_urltype (types[i].type, types[i].protocol);

  // initialize graphing support
  gnc_html_graph_gog_init();
}


char  *
gnc_build_url (URLType type, const gchar * location, const gchar * label)
{
  char * type_name;

  DEBUG(" ");
  type_name = g_hash_table_lookup (gnc_html_type_to_proto_hash, type);
  if (!type_name)
    type_name = "";

  if(label) {
    return g_strdup_printf("%s%s%s#%s", type_name, (*type_name ? ":" : ""),
                           (location ? location : ""),
                           label ? label : "");
  }
  else {
    return g_strdup_printf("%s%s%s", type_name, (*type_name ? ":" : ""),
                           (location ? location : ""));
  }
}

static gboolean
http_allowed()
{
  return TRUE;
}

static gboolean
https_allowed()
{
  return TRUE;
}

/************************************************************
 * gnc_html_start_request: starts the gnc-http object working on an
 * http/https request.
 ************************************************************/
static void 
gnc_html_start_request(gnc_html * html, gchar * uri, GtkHTMLStream * handle)
{
  GList * handles = NULL;
  gint  need_request = FALSE;

  /* we want to make a list of handles to fill with this URI.
   * multiple handles with the same URI will all get filled when the
   * request comes in. */
  DEBUG("requesting %s", uri);
  handles = g_hash_table_lookup(html->request_info, uri);
  if(!handles) {
    need_request = TRUE;
  }

  handles = g_list_append(handles, handle);
  g_hash_table_insert(html->request_info, uri, handles);
  
  if(need_request) {
      g_critical("we've not supported network requests for years");
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
                        const gchar * label)
{
  char * fdata = NULL;
  int fdata_len = 0;

  DEBUG("type %s, location %s, label %s", type ? type : "(null)",
	location ? location : "(null)", label ? label : "(null)");
  if(!html) {
    return;
  }

  if (gnc_html_stream_handlers) {
    GncHTMLStreamCB stream_handler;

    stream_handler = g_hash_table_lookup (gnc_html_stream_handlers, type);
    if (stream_handler) {
      gboolean ok = stream_handler (location, &fdata, &fdata_len);

      if(ok) {
        fdata = fdata ? fdata : g_strdup ("");
        gtk_html_write(GTK_HTML(html->html), handle, fdata, fdata_len);
        gtk_html_end(GTK_HTML(html->html), handle, GTK_HTML_STREAM_OK);      
      }
      else {
        fdata = fdata ? fdata : 
	  g_strdup_printf (error_404_format, 
			   _(error_404_title), _(error_404_body));
        gtk_html_write(GTK_HTML(html->html), handle, fdata, strlen (fdata));
        gtk_html_end(GTK_HTML(html->html), handle, GTK_HTML_STREAM_ERROR);
      }

      g_free(fdata);

      if(label) {
	while (gtk_events_pending ())
	  gtk_main_iteration ();
        gtk_html_jump_to_anchor(GTK_HTML(html->html), label);
      }

      return;
    }
  }

  do {
    if (!safe_strcmp (type, URL_TYPE_SECURE) ||
	!safe_strcmp (type, URL_TYPE_HTTP)) {

      if (!safe_strcmp (type, URL_TYPE_SECURE)) {
	if(!https_allowed()) {
	  gnc_error_dialog( html->window,
                            _("Secure HTTP access is disabled. "
                              "You can enable it in the Network section of "
                              "the Preferences dialog."));
	  break;
	}
      }

      if(!http_allowed()) {
	gnc_error_dialog( html->window,
                          _("Network HTTP access is disabled. "
                            "You can enable it in the Network section of "
                            "the Preferences dialog."));
      } else {
	char *fullurl;
      
	fullurl = gnc_build_url(type, location, label);
	gnc_html_start_request(html, fullurl, handle);
      }

    } else {
      PWARN("load_to_stream for inappropriate type\n"
	    "\turl = '%s#%s'\n",
	    location ? location : "(null)",
	    label ? label : "(null)");
      fdata = g_strdup_printf (error_404_format, 
			       _(error_404_title), _(error_404_body));
      gtk_html_write(GTK_HTML(html->html), handle, fdata, strlen (fdata));
      gtk_html_end(GTK_HTML(html->html), handle, GTK_HTML_STREAM_ERROR);
      g_free (fdata);
    }

  } while (FALSE);

}


/********************************************************************
 * gnc_html_link_clicked_cb - called when user left-clicks on html
 * anchor. 
 ********************************************************************/

static void 
gnc_html_link_clicked_cb(GtkHTML * html, const gchar * url, gpointer data)
{
  URLType   type;
  char      * location = NULL;
  char      * label = NULL;
  gnc_html  * gnchtml = (gnc_html *)data;

  DEBUG("Clicked %s", url);
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
                          GtkHTMLStream * handle, gpointer data)
{
  URLType       type;
  char          * location=NULL;
  char          * label=NULL;
  gnc_html      * gnchtml = (gnc_html *)data;

  DEBUG("requesting %s", url);
  type = gnc_html_parse_url(gnchtml, url, &location, &label);
  gnc_html_load_to_stream(gnchtml, handle, type, location, label);
  g_free(location);
  g_free(label);
}


/********************************************************************
 * gnc_html_object_requested_cb - called when an applet needs to be
 * loaded.  
 ********************************************************************/

static gboolean
gnc_html_object_requested_cb(GtkHTML * html, GtkHTMLEmbedded * eb,
                             gpointer data)
{
  gnc_html  * gnchtml = data; 
  GncHTMLObjectCB h;

  DEBUG(" ");
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
gnc_html_on_url_cb(GtkHTML * html, const gchar * url, gpointer data)
{
  gnc_html * gnchtml = (gnc_html *) data;

  DEBUG("Rollover %s", url ? url : "(null)");
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
                     gpointer data)
{
  gnc_html * html = (gnc_html *)data;
  URLType  type;
  char     * location = NULL;
  char     * label = NULL;

  DEBUG("Setting base location to %s", base);
  type = gnc_html_parse_url(html, base, &location, &label);

  g_free(html->base_location);
  g_free(label);

  html->base_type     = type;
  html->base_location = location;
}


/********************************************************************
 * gnc_html_button_press_cb
 * mouse button callback (if any)
 ********************************************************************/

static int
gnc_html_button_press_cb(GtkWidget * widg, GdkEventButton * event,
                         gpointer user_data)
{
  gnc_html * html = user_data;

  DEBUG("Button Press");
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
gnc_html_unpack_form_data(const char * encoding)
{
  GHashTable * rv;

  DEBUG(" ");
  rv = g_hash_table_new(g_str_hash, g_str_equal);
  gnc_html_merge_form_data(rv, encoding);
  return rv;
}

void
gnc_html_merge_form_data(GHashTable * rv, const char * encoding)
{
  char * next_pair = NULL; 
  char * name  = NULL;
  char * value = NULL;
  char * extr_name  = NULL;
  char * extr_value = NULL;
  
  DEBUG(" ");
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
free_form_data_helper(gpointer k, gpointer v, gpointer user)
{
  DEBUG(" ");
  g_free(k);
  g_free(v);
  return TRUE;
}

void 
gnc_html_free_form_data(GHashTable * d)
{
  DEBUG(" ");
  g_hash_table_foreach_remove(d, free_form_data_helper, NULL);
  g_hash_table_destroy(d);
}

static void
pack_form_data_helper(gpointer key, gpointer val, 
                      gpointer user_data)
{
  char * old_str = *(char **)user_data;
  char * enc_key = gnc_html_encode_string((char *)key);
  char * enc_val = gnc_html_encode_string((char *)val);
  char * new_str = NULL;

  DEBUG(" ");
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
gnc_html_pack_form_data(GHashTable * form_data)
{
  char * encoded = NULL;
  DEBUG(" ");
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
                   gpointer user_data)
{
  gnc_html * gnchtml = user_data;
  char     * location = NULL;
  char     * new_loc = NULL;
  char     * label = NULL;
  GHashTable * form_data;
  URLType  type;

  DEBUG(" ");
  form_data = gnc_html_unpack_form_data(encoded_form_data);
  type = gnc_html_parse_url(gnchtml, action, &location, &label);

  g_critical("form submission hasn't been supported in years.");
  
  g_free(location);
  g_free(label);
  g_free(new_loc);
  gnc_html_free_form_data(form_data);
  return TRUE;
}


/********************************************************************
 * gnc_html_open_scm
 * insert some scheme-generated HTML
 ********************************************************************/

static void
gnc_html_open_scm(gnc_html * html, const gchar * location,
                  const gchar * label, int newwin)
{
  PINFO("location='%s'", location ? location : "(null)");
}


/********************************************************************
 * gnc_html_show_data
 * display some HTML that the creator of the gnc-html got from 
 * somewhere. 
 ********************************************************************/

void
gnc_html_show_data(gnc_html * html, const char * data, 
                   int datalen)
{
  GtkHTMLStream * handle;

  DEBUG("datalen %d, data %20.20s", datalen, data);
  handle = gtk_html_begin(GTK_HTML(html->html));
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
                  gboolean new_window_hint)
{
  GncHTMLUrlCB url_handler;
  GtkHTMLStream * handle;
  gboolean new_window;

  DEBUG(" ");
  if (!html) return;
  if (!location) return;

  /* make sure it's OK to show this URL type in this window */
  if(new_window_hint == 0) {
    if (html->urltype_cb)
      new_window = !((html->urltype_cb)(type));
    else
      new_window = FALSE;
  }
  else {
    new_window = TRUE;
  }

  if(!new_window) {
    gnc_html_cancel(html);
  }

  if (gnc_html_url_handlers)
    url_handler = g_hash_table_lookup (gnc_html_url_handlers, type);
  else
    url_handler = NULL;

  if (url_handler)
  {
    GNCURLResult result;
    gboolean ok;

    result.load_to_stream = FALSE;
    result.url_type = type;
    result.location = NULL;
    result.label = NULL;
    result.base_type = URL_TYPE_FILE;
    result.base_location = NULL;
    result.error_message = NULL;

    ok = url_handler (location, label, new_window, &result);
    if (!ok)
    {
      if (result.error_message)
        gnc_error_dialog( html->window, "%s", result.error_message);
      else
	/* %s is a URL (some location somewhere). */
        gnc_error_dialog( html->window, _("There was an error accessing %s."), location);

      if (html->load_cb)
        html->load_cb (html, result.url_type,
                       location, label,
                       html->load_cb_data);
    }
    else if (result.load_to_stream)
    {
      gnc_html_history_node *hnode;
      const char *new_location;
      const char *new_label;
      GtkHTMLStream * stream;

      new_location = result.location ? result.location : location;
      new_label = result.label ? result.label : label;
      hnode = gnc_html_history_node_new (result.url_type,
                                         new_location, new_label);

      gnc_html_history_append (html->history, hnode);

      g_free (html->base_location);
      html->base_type = result.base_type;
      html->base_location =
	    g_strdup (extract_base_name(result.base_type, new_location));
      DEBUG("resetting base location to %s",
	    html->base_location ? html->base_location : "(null)");

      stream = gtk_html_begin (GTK_HTML(html->html));
      gnc_html_load_to_stream (html, stream, result.url_type,
                               new_location, new_label);

      if (html->load_cb)
        html->load_cb (html, result.url_type,
                       new_location, new_label,
                       html->load_cb_data);
    }

    g_free (result.location);
    g_free (result.label);
    g_free (result.base_location);
    g_free (result.error_message);

    return;
  }

  if (!safe_strcmp (type, URL_TYPE_SCHEME)) {
    gnc_html_open_scm(html, location, label, new_window);

  } else if (!safe_strcmp (type, URL_TYPE_JUMP)) {
    gtk_html_jump_to_anchor(GTK_HTML(html->html), label);

  } else if (!safe_strcmp (type, URL_TYPE_SECURE) ||
	     !safe_strcmp (type, URL_TYPE_HTTP) ||
	     !safe_strcmp (type, URL_TYPE_FILE)) {

    do {
      if (!safe_strcmp (type, URL_TYPE_SECURE)) {
	if(!https_allowed()) {
	  gnc_error_dialog( html->window,
                            _("Secure HTTP access is disabled. "
                              "You can enable it in the Network section of "
                              "the Preferences dialog."));
	  break;
	}
      }

      if (safe_strcmp (type, URL_TYPE_FILE)) {
	if(!http_allowed()) {
	  gnc_error_dialog( html->window,
                            _("Network HTTP access is disabled. "
                              "You can enable it in the Network section of "
                              "the Preferences dialog."));
	  break;
	}
      }

      html->base_type     = type;
      
      if(html->base_location) g_free(html->base_location);
      html->base_location = extract_base_name(type, location);

      /* FIXME : handle new_window = 1 */
      gnc_html_history_append(html->history,
			      gnc_html_history_node_new(type, location, label));
      handle = gtk_html_begin(GTK_HTML(html->html));
      gnc_html_load_to_stream(html, handle, type, location, label);

    } while (FALSE);

  } else {
    PERR ("URLType %s not supported.", type);
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
gnc_html_reload(gnc_html * html)
{
  gnc_html_history_node * n;

  DEBUG(" ");
  n = gnc_html_history_get_current(html->history);
  if(n) {
    gnc_html_show_url(html, n->type, n->location, n->label, 0);
  }
}


/********************************************************************
 * gnc_html_new
 * create and set up a new gtkhtml widget.
 ********************************************************************/

gnc_html * 
gnc_html_new( GtkWindow *parent )
{
  gnc_html * retval = g_new0(gnc_html, 1);

  ENTER("parent %p", parent);
  
  retval->window    = GTK_WIDGET(parent);
  retval->container = gtk_scrolled_window_new(NULL, NULL);
  retval->html      = gtk_html_new();

  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(retval->container),
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);

  gtk_container_add(GTK_CONTAINER(retval->container), 
                    GTK_WIDGET(retval->html));

  retval->request_info = g_hash_table_new(g_str_hash, g_str_equal);
  //retval->http         = gnc_http_new();
  retval->history      = gnc_html_history_new();

#ifdef HAVE_GTK_2_10
  g_object_ref_sink(retval->container);
#else
  g_object_ref (retval->container);
  gtk_object_sink (GTK_OBJECT (retval->container));
#endif

  /* signals */
  g_signal_connect(retval->html, "url_requested",
		   G_CALLBACK(gnc_html_url_requested_cb),
		   retval);
  
  g_signal_connect(retval->html, "on_url",
		   G_CALLBACK(gnc_html_on_url_cb),
		   retval);
  
  g_signal_connect(retval->html, "set_base",
		   G_CALLBACK(gnc_html_set_base_cb),
		   retval);
  
  g_signal_connect(retval->html, "link_clicked",
		   G_CALLBACK(gnc_html_link_clicked_cb),
		   retval);
  
  g_signal_connect (retval->html, "object_requested",
		    G_CALLBACK (gnc_html_object_requested_cb), 
		    retval);

  g_signal_connect (retval->html, "button_press_event",
		    G_CALLBACK (gnc_html_button_press_cb), 
		    retval);

  g_signal_connect (retval->html, "submit", 
		    G_CALLBACK(gnc_html_submit_cb),
		    retval);
  
  gtk_html_load_empty(GTK_HTML(retval->html));

  LEAVE("retval %p", retval);
  
  return retval;
}


/********************************************************************
 * gnc_html_cancel
 * cancel any outstanding HTML fetch requests. 
 ********************************************************************/

static gboolean
html_cancel_helper(gpointer key, gpointer value, gpointer user_data)
{
  g_free(key);
  g_list_free((GList *)value);
  return TRUE;
}

void
gnc_html_cancel(gnc_html * html)
{
  /* remove our own references to requests */ 
    //gnc_http_cancel_requests(html->http);
  
  g_hash_table_foreach_remove(html->request_info, html_cancel_helper, NULL);
}


/********************************************************************
 * gnc_html_destroy
 * destroy the struct
 ********************************************************************/

void
gnc_html_destroy(gnc_html * html)
{

  if(!html) return;

  /* cancel any outstanding HTTP requests */
  gnc_html_cancel(html);
  
  gnc_html_history_destroy(html->history);

  gtk_widget_destroy(html->container);
  g_object_unref(html->container);

  g_free(html->current_link);
  g_free(html->base_location);

  html->window        = NULL;
  html->container     = NULL;
  html->html          = NULL;
  html->history       = NULL;
  html->current_link  = NULL;
  html->base_location = NULL;

  g_free(html);
}

void
gnc_html_set_urltype_cb(gnc_html * html, GncHTMLUrltypeCB urltype_cb)
{
  html->urltype_cb = urltype_cb;
}

void
gnc_html_set_load_cb(gnc_html * html, GncHTMLLoadCB load_cb,
                     gpointer data)
{
  html->load_cb = load_cb;
  html->load_cb_data = data;
}

void
gnc_html_set_flyover_cb(gnc_html * html, GncHTMLFlyoverCB flyover_cb,
                        gpointer data)
{
  html->flyover_cb       = flyover_cb;
  html->flyover_cb_data  = data;
}

void
gnc_html_set_button_cb(gnc_html * html, GncHTMLButtonCB button_cb,
                        gpointer data)
{
  html->button_cb       = button_cb;
  html->button_cb_data  = data;
}

void
gnc_html_copy(gnc_html *html)
{
  g_return_if_fail(html);

  gtk_html_copy(GTK_HTML(html->html));
}

/**************************************************************
 * gnc_html_export : wrapper around the builtin function in gtkhtml
 **************************************************************/

static gboolean 
raw_html_receiver (gpointer     engine,
                   const gchar *data,
                   size_t        len,
                   gpointer     user_data)
{
  FILE *fh = (FILE *) user_data;
  size_t written;

  do {
    written = fwrite (data, 1, len, fh);
    len -= written;
  } while (len > 0);
  return TRUE;
}

gboolean
gnc_html_export(gnc_html * html, const char *filepath)
{
  FILE *fh;

  g_return_val_if_fail (html != NULL, FALSE);
  g_return_val_if_fail (filepath != NULL, FALSE);

  fh = g_fopen (filepath, "w");
  if (!fh)
    return FALSE;

  gtk_html_save (GTK_HTML(html->html), GINT_TO_POINTER(raw_html_receiver), fh);

  fclose (fh);

  return TRUE;
}

#ifdef GTKHTML_USES_GTKPRINT
static void
draw_page_cb(GtkPrintOperation *operation, GtkPrintContext *context,
             gint page_nr, gpointer user_data)
{
    gnc_html *html = user_data;

    gtk_html_print_page((GtkHTML*) html->html, context);
}

void
gnc_html_print(gnc_html *html)
{
    GtkPrintOperation *print;
    GtkPrintOperationResult res;

    print = gtk_print_operation_new();

    G_LOCK(print_settings);
    if (print_settings)
        gtk_print_operation_set_print_settings(print, print_settings);
    G_UNLOCK(print_settings);

    gtk_print_operation_set_use_full_page(print, FALSE);
    gtk_print_operation_set_unit(print, GTK_UNIT_POINTS);
    gtk_print_operation_set_n_pages(print, 1);
    g_signal_connect(print, "draw_page", G_CALLBACK(draw_page_cb), html);

    res = gtk_print_operation_run(print, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
                                  GTK_WINDOW(html->window), NULL);

    if (res == GTK_PRINT_OPERATION_RESULT_APPLY) {
        G_LOCK(print_settings);
        if (print_settings)
            g_object_unref(print_settings);
        print_settings = g_object_ref(gtk_print_operation_get_print_settings(print));
        G_UNLOCK(print_settings);
    }

    g_object_unref(print);
}

#else /* !GTKHTML_USES_GTKPRINT */
void
gnc_html_print(gnc_html * html)
{
  PrintSession *ps;

  ps = gnc_print_session_create(FALSE);
  if (ps == NULL) {
    /* user cancelled */
    return;
  }

  gtk_html_print(GTK_HTML(html->html), ps->context);
  gnc_print_session_done(ps);
}
#endif /* GTKHTML_USES_GTKPRINT */

gnc_html_history * 
gnc_html_get_history(gnc_html * html)
{
  if (!html) return NULL;
  return html->history;
}


GtkWidget * 
gnc_html_get_widget(gnc_html * html)
{
  if (!html) return NULL;
  return html->container;
}

void
gnc_html_register_object_handler(const char * classid, 
                                 GncHTMLObjectCB hand)
{
  g_return_if_fail (classid != NULL);

  if(!gnc_html_object_handlers) {
    gnc_html_object_handlers = g_hash_table_new(g_str_hash, g_str_equal);
  }

  gnc_html_unregister_object_handler (classid);
  if (!hand)
    return;

  g_hash_table_insert(gnc_html_object_handlers, g_strdup(classid), hand);
}

void
gnc_html_unregister_object_handler(const char * classid)
{
  gchar * keyptr=NULL;
  gchar * valptr=NULL;
  gchar ** p_keyptr = &keyptr;
  gchar ** p_valptr = &valptr;

  if (!g_hash_table_lookup_extended(gnc_html_object_handlers,
                                    classid, 
                                    (gpointer *)p_keyptr,
                                    (gpointer *)p_valptr))
    return;

  g_hash_table_remove(gnc_html_object_handlers, classid);
  g_free(keyptr);
}

void
gnc_html_register_action_handler(const char * actionid, 
                                 GncHTMLActionCB hand)
{
  g_return_if_fail (actionid != NULL);

  if(!gnc_html_action_handlers) {
    gnc_html_action_handlers = g_hash_table_new(g_str_hash, g_str_equal);
  }

  gnc_html_unregister_action_handler (actionid);
  if (!hand)
    return;

  g_hash_table_insert(gnc_html_action_handlers, g_strdup(actionid), hand);
}

void
gnc_html_unregister_action_handler(const char * actionid)
{
  gchar * keyptr=NULL;
  gchar * valptr=NULL;
  gchar ** p_keyptr = &keyptr;
  gchar ** p_valptr = &valptr;

  g_return_if_fail (actionid != NULL);

  if (!g_hash_table_lookup_extended(gnc_html_action_handlers,
                                    actionid, 
                                    (gpointer *)p_keyptr, 
                                    (gpointer *)p_valptr))
    return;

  g_hash_table_remove(gnc_html_action_handlers, actionid);
  g_free(keyptr);
}

void
gnc_html_register_stream_handler(URLType url_type, GncHTMLStreamCB hand)
{
  g_return_if_fail (url_type != NULL && *url_type != '\0');

  if(!gnc_html_stream_handlers) {
    gnc_html_stream_handlers = g_hash_table_new(g_str_hash, g_str_equal);
  }

  gnc_html_unregister_stream_handler (url_type);
  if (!hand)
    return;

  g_hash_table_insert (gnc_html_stream_handlers, url_type, hand);
}

void
gnc_html_unregister_stream_handler(URLType url_type)
{
  g_hash_table_remove (gnc_html_stream_handlers, url_type);
}

void
gnc_html_register_url_handler (URLType url_type, GncHTMLUrlCB hand)
{
  g_return_if_fail (url_type != NULL && *url_type != '\0');

  if(!gnc_html_url_handlers)
{
    gnc_html_url_handlers = g_hash_table_new (g_str_hash, g_str_equal);
  }

  gnc_html_unregister_url_handler (url_type);
  if (!hand)
    return;

  g_hash_table_insert (gnc_html_url_handlers, url_type, hand);
}

void
gnc_html_unregister_url_handler (URLType url_type)
{
  g_hash_table_remove (gnc_html_url_handlers, url_type);
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
gnc_html_encode_string(const char * str)
{
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
gnc_html_decode_string(const char * str)
{
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
      if (1 == sscanf(ptr, "%02X", &hexval))
	decoded = g_string_append_c(decoded, (char)hexval);
      else
	decoded = g_string_append_c(decoded, ' ');
      ptr++;
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
gnc_html_unescape_newlines(const gchar * in)
{
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
gnc_html_escape_newlines(const gchar * in)
{
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
