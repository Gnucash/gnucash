/********************************************************************
 * gnc-html-extras.c -- display HTML with some special gnucash tags.*
 *                                                                  *
 * Copyright (C) 2009 Phil Longstaff <plongstaff@rogers.com>        *
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

#include "config.h"

//#include <gtk/gtk.h>
//#include <glib/gi18n.h>
//#include <glib/gstdio.h>
//#include <sys/types.h>
//#include <sys/stat.h>
//#include <stdlib.h>
//#include <string.h>

#include "gnc-engine.h"
#include "gnc-html-extras.h"
#include "gnc-html-graph-gog.h"

/* indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_HTML;

/* hashes for URLType -> protocol and protocol -> URLType */
static GHashTable * gnc_html_type_to_proto_hash = NULL;
GHashTable * gnc_html_proto_to_type_hash = NULL;

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
