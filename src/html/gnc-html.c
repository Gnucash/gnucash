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
//#include "gnc-html-graph-gog.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"

/* indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_HTML;

/* hashes for URLType -> protocol and protocol -> URLType */
static GHashTable * gnc_html_type_to_proto_hash = NULL;
GHashTable * gnc_html_proto_to_type_hash = NULL;

/* hashes an HTML <object classid="ID"> classid to a handler function */
GHashTable* gnc_html_object_handlers = NULL;

/* hashes an action name from a FORM definition to a handler function.
 * <form method=METHOD action=gnc-action:ACTION-NAME?ACTION-ARGS>
 * action-args is what gets passed to the handler. */
GHashTable* gnc_html_action_handlers = NULL;

/* hashes handlers for loading different URLType data */
GHashTable* gnc_html_stream_handlers = NULL;

/* hashes handlers for handling different URLType data */
GHashTable* gnc_html_url_handlers = NULL;

/* hashes an HTML <object classid="ID"> classid to a handler function */
extern GHashTable* gnc_html_object_handlers;

static char error_404_format[] =
"<html><body><h3>%s</h3><p>%s</body></html>";
static char error_404_title[] = N_("Not found");
static char error_404_body[] =
N_("The specified URL could not be loaded.");

G_DEFINE_ABSTRACT_TYPE(GncHtml, gnc_html, GTK_TYPE_BIN)

static void gnc_html_class_init( GncHtmlClass* klass );
static void gnc_html_dispose( GObject* obj );
static void gnc_html_finalize( GObject* obj );

//#define GNC_HTML_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE((o), GNC_TYPE_HTML, GncHtmlPrivate))
#define GNC_HTML_GET_PRIVATE(o) (GNC_HTML(o)->priv)

#include "gnc-html-p.h"

static void
gnc_html_class_init( GncHtmlClass* klass )
{
	GObjectClass *gobject_class = G_OBJECT_CLASS(klass);

	gobject_class->dispose = gnc_html_dispose;
	gobject_class->finalize = gnc_html_finalize;

	klass->show_url = NULL;
	klass->show_data = NULL;
	klass->reload = NULL;
	klass->copy = NULL;
	klass->export = NULL;
	klass->print = NULL;
	klass->cancel = NULL;
	klass->parse_url = NULL;
	klass->set_parent = NULL;
}

static void
gnc_html_init( GncHtml* self )
{
	GncHtmlPrivate* priv;
	priv = self->priv = g_new0( GncHtmlPrivate, 1 );

	priv->container = gtk_scrolled_window_new( NULL, NULL );
	gtk_scrolled_window_set_policy( GTK_SCROLLED_WINDOW(priv->container),
									GTK_POLICY_AUTOMATIC,
									GTK_POLICY_AUTOMATIC );
	priv->request_info = g_hash_table_new( g_str_hash, g_str_equal );
	priv->history = gnc_html_history_new();
}

static void
gnc_html_dispose( GObject* obj )
{
	GncHtml* self = GNC_HTML(obj);
	GncHtmlPrivate* priv = GNC_HTML_GET_PRIVATE(self);

	if( priv->container != NULL ) {
		g_object_unref( G_OBJECT(priv->container) );
		priv->container = NULL;
	}
	if( priv->request_info != NULL ) {
		g_hash_table_destroy( priv->request_info );
		priv->request_info = NULL;
	}
	if( priv->history != NULL ) {
		gnc_html_history_destroy( priv->history );
		priv->history = NULL;
	}

	G_OBJECT_CLASS(gnc_html_parent_class)->dispose( obj );
}

static void
gnc_html_finalize( GObject* obj )
{
	GncHtml* self = GNC_HTML(obj);

	if( self->priv != NULL ) {
		g_free( self->priv );
		self->priv = NULL;
	}

	G_OBJECT_CLASS(gnc_html_parent_class)->finalize( obj );
}

/***********************************************************************************/

static char*
extract_machine_name( const gchar* path )
{
	gchar machine_rexp[] = "^(//[^/]*)/*(.*)?$";
	regex_t compiled_m;
	regmatch_t match[4];
	gchar* machine = NULL;

	if( path == NULL ) return NULL;

	regcomp( &compiled_m, machine_rexp, REG_EXTENDED );

	/* step 1: split the machine name away from the path
	 * components */
	if( !regexec( &compiled_m, path, 4, match, 0 ) ) {
		/* $1 is the machine name */
		if( match[1].rm_so != -1 ) {
			machine = g_strndup( path+match[1].rm_so, match[1].rm_eo - match[1].rm_so );
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
gnc_html_parse_url( GncHtml* self, const gchar* url,
					gchar** url_location, gchar** url_label )
{
	gchar uri_rexp[] = "^(([^:][^:]+):)?([^#]+)?(#(.*))?$";
	regex_t compiled;
	regmatch_t match[6];
	gchar* protocol = NULL;
	gchar* path = NULL;
	gchar* label = NULL;
	gboolean found_protocol = FALSE;
	gboolean found_path = FALSE;
	gboolean found_label = FALSE;
	URLType retval;
	GncHtmlPrivate* priv = GNC_HTML_GET_PRIVATE(self);

	g_return_val_if_fail( GNC_IS_HTML(self), NULL );

	DEBUG( "parsing %s, base_location %s",
			url ? url : "(null)",
			self ? (priv->base_location ? priv->base_location
										: "(null base_location)")
				: "(null html)");

	regcomp( &compiled, uri_rexp, REG_EXTENDED );

	if( !regexec( &compiled, url, 6, match, 0 ) ) {
		if( match[2].rm_so != -1 ) {
			protocol = g_new0( gchar, match[2].rm_eo - match[2].rm_so + 1 );
			strncpy( protocol, url + match[2].rm_so, match[2].rm_eo - match[2].rm_so );
			protocol[match[2].rm_eo - match[2].rm_so] = 0;
			found_protocol = TRUE;
		}
		if( match[3].rm_so != -1 ) {
			path = g_new0( gchar, match[3].rm_eo - match[3].rm_so + 1 );
			strncpy( path, url+match[3].rm_so, match[3].rm_eo - match[3].rm_so );
			path[match[3].rm_eo - match[3].rm_so] = 0;
			found_path = TRUE;
		}
		if( match[5].rm_so != -1 ) {
			label = g_new0( gchar, match[5].rm_eo - match[5].rm_so + 1 );
			strncpy( label, url+match[5].rm_so, match[5].rm_eo - match[5].rm_so );
			label[match[5].rm_eo - match[5].rm_so] = 0;
			found_label = TRUE;
		}
	}

	regfree( &compiled );

	if( found_protocol ) {
		retval = g_hash_table_lookup( gnc_html_proto_to_type_hash, protocol );
		if( retval == NULL ) {
			PWARN( "unhandled URL type for '%s'", url ? url : "(null)" );
			retval = URL_TYPE_OTHER;
		}
	} else if( found_label && !found_path ) {
		retval = URL_TYPE_JUMP;
	} else {
		if( self ) {
			retval = priv->base_type;
		} else {
			retval = URL_TYPE_FILE;
		}
	}

	g_free( protocol );

	if( !safe_strcmp( retval, URL_TYPE_FILE ) ) {
		if( !found_protocol && path && self && priv->base_location ) {
			if( g_path_is_absolute( path ) ) {
				*url_location = g_strdup( path );
			} else {
				*url_location = g_build_filename( priv->base_location, path, (gchar*)NULL );
			}
			g_free( path );
		} else {
			*url_location = g_strdup( path );
			g_free( path );
		}

	} else if( !safe_strcmp( retval, URL_TYPE_JUMP ) ) {
		*url_location = NULL;
		g_free( path );

	} else {
		/* case URL_TYPE_OTHER: */

		if( !found_protocol && path && self && priv->base_location ) {
			if( g_path_is_absolute( path ) ) {
				*url_location = g_build_filename( extract_machine_name( priv->base_location ),
                           path, (gchar*)NULL );
			} else {
				*url_location = g_build_filename( priv->base_location, path, (gchar*)NULL );
			}
			g_free( path );
		} else {
			*url_location = g_strdup( path );
			g_free( path );
		}
	}

	*url_label = label;
	return retval;
}

/********************************************************************
 * gnc_html_show_data
 * display some HTML that the creator of the gnc-html got from
 * somewhere.
 ********************************************************************/

void
gnc_html_show_data( GncHtml* self, const gchar* data, int datalen )
{
	g_return_if_fail( GNC_IS_HTML(self) );

	GNC_HTML_GET_CLASS(self)->show_data( self, data, datalen );
}


/********************************************************************
 * gnc_html_show_url
 *
 * open a URL.  This is called when the user clicks a link or
 * for the creator of the gnc_html window to explicitly request
 * a URL.
 ********************************************************************/

void
gnc_html_show_url( GncHtml* self, URLType type,
                  const gchar* location, const gchar* label,
                  gboolean new_window_hint )
{
	g_return_if_fail( GNC_IS_HTML(self) );

	GNC_HTML_GET_CLASS(self)->show_url( self, type, location, label, new_window_hint );
}


/********************************************************************
 * gnc_html_reload
 * reload the current page
 ********************************************************************/

void
gnc_html_reload( GncHtml* self )
{
	g_return_if_fail( GNC_IS_HTML(self) );

	GNC_HTML_GET_CLASS(self)->reload( self );
}

/********************************************************************
 * gnc_html_cancel
 * cancel any outstanding HTML fetch requests.
 ********************************************************************/

void
gnc_html_cancel( GncHtml* self )
{
	g_return_if_fail( GNC_IS_HTML(self) );

	GNC_HTML_GET_CLASS(self)->cancel( self );
}


/********************************************************************
 * gnc_html_destroy
 * destroy the struct
 ********************************************************************/

void
gnc_html_destroy( GncHtml* self )
{
	g_object_unref( G_OBJECT(self) );
}

void
gnc_html_set_urltype_cb( GncHtml* self, GncHTMLUrltypeCB urltype_cb )
{
	GncHtmlPrivate* priv;
	g_return_if_fail( GNC_IS_HTML(self) );

	priv = GNC_HTML_GET_PRIVATE(self);
	priv->urltype_cb = urltype_cb;
}

void
gnc_html_set_load_cb( GncHtml* self, GncHTMLLoadCB load_cb, gpointer data )
{
	GncHtmlPrivate* priv;
	g_return_if_fail( GNC_IS_HTML(self) );

	priv = GNC_HTML_GET_PRIVATE(self);
	priv->load_cb = load_cb;
	priv->load_cb_data = data;
}

void
gnc_html_set_flyover_cb( GncHtml* self, GncHTMLFlyoverCB flyover_cb, gpointer data )
{
	GncHtmlPrivate* priv;
	g_return_if_fail( GNC_IS_HTML(self) );

	priv = GNC_HTML_GET_PRIVATE(self);
	priv->flyover_cb = flyover_cb;
	priv->flyover_cb_data = data;
}

void
gnc_html_set_button_cb( GncHtml* self, GncHTMLButtonCB button_cb, gpointer data )
{
	GncHtmlPrivate* priv;
	g_return_if_fail( GNC_IS_HTML(self) );

	priv = GNC_HTML_GET_PRIVATE(self);
	priv->button_cb = button_cb;
	priv->button_cb_data = data;
}

void
gnc_html_copy( GncHtml* self )
{
	g_return_if_fail( GNC_IS_HTML(self) );

	GNC_HTML_GET_CLASS(self)->copy( self );
}

/**************************************************************
 * gnc_html_export : wrapper around the builtin function in gtkhtml
 **************************************************************/

gboolean
gnc_html_export( GncHtml* self, const gchar* filepath )
{
	g_return_val_if_fail( GNC_IS_HTML(self), FALSE );

	return GNC_HTML_GET_CLASS(self)->export( self, filepath );
}

void
gnc_html_print( GncHtml* self )
{
	g_return_if_fail( GNC_IS_HTML(self) );

	GNC_HTML_GET_CLASS(self)->print( self );
}

gnc_html_history *
gnc_html_get_history( GncHtml* self )
{
	g_return_val_if_fail( GNC_IS_HTML(self), NULL );
	return GNC_HTML_GET_PRIVATE(self)->history;
}


GtkWidget *
gnc_html_get_widget( GncHtml* self )
{
	g_return_val_if_fail( GNC_IS_HTML(self), NULL );
	return GNC_HTML_GET_PRIVATE(self)->container;
}

void
gnc_html_set_parent( GncHtml* self, GtkWindow* parent )
{
	g_return_if_fail( GNC_IS_HTML(self) );
	GNC_HTML_GET_CLASS(self)->set_parent( self, parent );
}

/* Register the URLType if it doesn't already exist.
 * Returns TRUE if successful, FALSE if the type already exists.
 */
gboolean
gnc_html_register_urltype( URLType type, const char *protocol )
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

void
gnc_html_register_object_handler( const char * classid,
                                 GncHTMLObjectCB hand )
{
	g_return_if_fail( classid != NULL );

	if( gnc_html_object_handlers == NULL ) {
		gnc_html_object_handlers = g_hash_table_new( g_str_hash, g_str_equal );
	}

	gnc_html_unregister_object_handler( classid );
	if( hand != NULL ) {
		g_hash_table_insert( gnc_html_object_handlers, g_strdup( classid ), hand );
	}
}

void
gnc_html_unregister_object_handler( const gchar* classid )
{
	gchar* keyptr = NULL;
	gchar* valptr = NULL;
	gchar** p_keyptr = &keyptr;
	gchar** p_valptr = &valptr;

	if( g_hash_table_lookup_extended( gnc_html_object_handlers,
											 classid,
											(gpointer *)p_keyptr,
											(gpointer *)p_valptr) ) {
		g_hash_table_remove( gnc_html_object_handlers, classid );
		g_free( keyptr );
	}
}

void
gnc_html_register_action_handler( const gchar* actionid,
								GncHTMLActionCB hand )
{
	g_return_if_fail( actionid != NULL );

	if( gnc_html_action_handlers == NULL ) {
		gnc_html_action_handlers = g_hash_table_new( g_str_hash, g_str_equal );
	}

	gnc_html_unregister_action_handler( actionid );
	if( hand != NULL ) {
		g_hash_table_insert( gnc_html_action_handlers, g_strdup( actionid ), hand );
	}
}

void
gnc_html_unregister_action_handler( const gchar* actionid )
{
	gchar* keyptr = NULL;
	gchar* valptr = NULL;
	gchar** p_keyptr = &keyptr;
	gchar** p_valptr = &valptr;

	g_return_if_fail( actionid != NULL );

	if( g_hash_table_lookup_extended( gnc_html_action_handlers,
											actionid,
											(gpointer *)p_keyptr,
											(gpointer *)p_valptr) ) {
		g_hash_table_remove( gnc_html_action_handlers, actionid );
		g_free( keyptr );
	}
}

void
gnc_html_register_stream_handler( URLType url_type, GncHTMLStreamCB hand )
{
	g_return_if_fail( url_type != NULL && *url_type != '\0' );

	if( gnc_html_stream_handlers == NULL ) {
		gnc_html_stream_handlers = g_hash_table_new( g_str_hash, g_str_equal );
	}

	gnc_html_unregister_stream_handler( url_type );
	if( hand != NULL ) {
		g_hash_table_insert( gnc_html_stream_handlers, url_type, hand );
	}
}

void
gnc_html_unregister_stream_handler( URLType url_type )
{
	g_hash_table_remove( gnc_html_stream_handlers, url_type );
}

void
gnc_html_register_url_handler( URLType url_type, GncHTMLUrlCB hand )
{
	g_return_if_fail( url_type != NULL && *url_type != '\0' );

	if( gnc_html_url_handlers == NULL ) {
		gnc_html_url_handlers = g_hash_table_new( g_str_hash, g_str_equal );
	}

	gnc_html_unregister_url_handler( url_type );
	if( hand != NULL ) {
		g_hash_table_insert( gnc_html_url_handlers, url_type, hand );
	}
}

void
gnc_html_unregister_url_handler( URLType url_type )
{
	g_hash_table_remove( gnc_html_url_handlers, url_type );
}

void show_type_signals( GType t )
{
	guint* signals;
	guint n_ids;
	guint i;
	GSignalQuery sig;

	signals = g_signal_list_ids( t, &n_ids );
	for( i = 0; i < n_ids; i++ ) {
		g_signal_query( signals[i], &sig );
		printf( "Signal: %s\n", sig.signal_name );
	}
}
