/********************************************************************
 * gnc-network.c -- handlers for forms and objects relevant to      *
 * GnuCash Network functions                                        *
 * Copyright (C) 2001 Bill Gribble <grib@billgribble.com>           *
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
#include <gtkhtml/gtkhtml.h>
#include <gtkhtml/gtkhtml-embedded.h>

#include "global-options.h"
#include "gnc-ui.h"
#include "gnc-gpg.h"
#include "gnc-gui-query.h"
#include "gnc-html.h"
#include "gnc-network.h"
#include "messages.h"

/********************************************************************
 *  THIS IS PRIVACY-SENSITIVE CODE.  The actions handled here are run
 *  when gnucash needs to send HTML form information to a web server
 *  (the "GnuCash Network") and can modify the information being sent
 *  or send additional information.
 * 
 *  Don't panic; all that means is that when an HTML form is loaded
 *  into the Gnucash HTML browser, after the user clicks the "submit"
 *  button, if the "action" in the form definition looks like
 *  "gnc-action:ACTION-NAME?ACTION-ARGS", the action-name is used to
 *  find a handler and the form is submitted by that handler.  The
 *  only web server that will have such forms will be the GnuCash
 *  Network server.  Even if other hosts try to fool gnucash with
 *  GnuCash Network submit actions, we will only ever submit to the
 *  user's specified GnuCash Network server.
 * 
 *  Users: keep in mind that this is *not* a mechanism for executing
 *  arbitrary code in your gnucash; the only "actions" that can be
 *  taken must be compiled into the client, and you can disable them
 *  globally with a Preferences option ("Enable GnuCash Network"). 
 * 
 *  Developers: Until we have a formally-codified privacy policy,
 *  please follow these guidelines:
 * 
 *  1. When actions send information that the user has not explicitly
 *  entered, there should *always* be a dialog or other notification,
 *  including the extra information to be sent, with an opportunity to
 *  bail out.  A server-side confirmation is probably enough, but
 *  client-side confirm is better.
 * 
 *  2. Do not accept a complete URI to submit to as an argument to
 *  actions.  This might allow a malicious server to ask the gnucash
 *  client for private information to be sent to itself, if the user
 *  happened to go to that site from within the gnucash HTML browser
 *  and click a form "SUBMIT" button.  Always use the "GnuCash Network 
 *  server" from the global preferences option. 
 ********************************************************************/

static char * session_id = NULL;
static char * gnc_passphrase = NULL;

static int 
gnc_network_get_handler(gnc_html * html, 
                        const char * method, const char * action, 
                        GHashTable * formdata);
static int 
gnc_network_make_keypair_handler(gnc_html * html, 
                                 GtkHTMLEmbedded * eb, 
                                 gpointer data);

/********************************************************************
 * gnc_network_get/set_session_id
 ********************************************************************/

char *
gnc_network_get_session_id(void) {
  return session_id;
}

void
gnc_network_set_session_id(char * sid) {
  if(session_id) {
    g_free(session_id);
  }
  session_id = g_strdup(sid);
}

char * 
gnc_network_get_uid(void) {
  SCM val = gnc_lookup_option("__gnc_network", "uid", gh_str02scm("0"));
  if(val != SCM_BOOL_F) {
    return gh_scm2newstr(val, NULL);
  }
  else {
    return strdup("");
  }
}

/********************************************************************
 * gnc_network_build_url
 * build a full URL given the action 
 ********************************************************************/

char * 
gnc_network_build_url(const char * action) {
  char * server = 
    gnc_lookup_string_option("Network", "GnuCash Network server",
                             "www.gnumatic.com");
  char * retval = g_strconcat("http://", server, "/", action, NULL);
  g_free(server);
  return retval;
}


/********************************************************************
 * gnc_network_get/free_passphrase 
 ********************************************************************/

static void 
get_pp_string_cb(char * string, gpointer data) {
  if(!data) return;
  if(!string) {
    *((char **)data) = NULL;
  }
  else {
    *((char **)data) = g_strdup(string);    
  }
}

char * 
gnc_network_ask_passphrase(const char * prompt) {
  char      * pp = NULL;
  gint      retval;
  GtkWidget * dlg = 
    gnome_request_dialog(TRUE, prompt, "", 1024,
                         get_pp_string_cb, (gpointer)&pp, NULL);
  retval = gnome_dialog_run_and_close(GNOME_DIALOG(dlg));
  if(retval == 0) {
    return pp;
  }
  else {
    return NULL;
  }
}

char * 
gnc_network_get_passphrase(void) {
  if(gnc_passphrase) return gnc_passphrase;
  else {
    gnc_passphrase = 
      gnc_network_ask_passphrase(_("Enter GnuCash Network passphrase:"));
    return gnc_passphrase;
  }
}


/********************************************************************
 * gnc_network_auth_handler
 * <object classid="gnc-network-auth"> handler.  Sets the gnc-network
 * session ID from the encrypted "data" of the object. 
 ********************************************************************/

struct network_crypt_info {
  gnc_html * html;
  char     * cryptext;
  char     * cleartext;
  char     * url;
  char     * passphrase;
  int      state;
};

static void
gnc_network_auth_crypt_cb(char * cleartext, gpointer data) {
  struct network_crypt_info * na = data;
  na->cleartext = cleartext;
  na->state     = 2;
}

static gboolean
gnc_network_auth_check(gpointer data) {
  struct network_crypt_info * na = data;

  switch(na->state) {
  case 0:
    /* get pp and start decryption */
    na->passphrase = gnc_network_get_passphrase();
    na->cleartext  = NULL;
    na->state      = 1;
    gnc_gpg_decrypt_async(na->cryptext, strlen(na->cryptext), na->passphrase,
                          gnc_network_auth_crypt_cb, na);
    return TRUE;
    break;

  case 1: 
    /* waiting for decryption */
    return TRUE;
    break;

  case 2:
    /* done.. take action */
    if(na->cleartext && (strlen(na->cleartext) > 0)) {
      gnc_network_set_session_id(na->cleartext);
      gnc_html_show_url(na->html, URL_TYPE_ACTION, na->url, NULL, 0);
    }
    else {
      gnc_warning_dialog(_("GnuCash Network authorization failed."));      
      gnc_passphrase = NULL;
    }
    /* clean up */ 
    g_free(na->cryptext);
    g_free(na->cleartext);
    g_free(na->url);
    g_free(na);
    return FALSE;
  }

  return TRUE;
}


static int
gnc_network_auth_handler(gnc_html * html, GtkHTMLEmbedded * eb, 
                         gpointer data) {
  struct network_crypt_info * na = g_new0(struct network_crypt_info, 1);
  
  na->html      = html;
  na->cryptext  = gnc_html_unescape_newlines(eb->data);
  na->cleartext = NULL;
  na->url       = g_strdup(g_hash_table_lookup(eb->params, "url")); 
  na->state     = 0;

  gtk_timeout_add(100, gnc_network_auth_check, na);
 
  return TRUE;
}
  

/********************************************************************
 * gnc_network_crypt_handler : receives ascii-armored GPG
 * cryptext, decrypts it, and displays the result as HTML.
 ********************************************************************/

static gboolean
gnc_network_crypt_check(gpointer data) {
  struct network_crypt_info * na = data;

  switch(na->state) {
  case 0:
    /* get pp and start decryption */
    na->passphrase = gnc_network_get_passphrase();
    na->cleartext  = NULL;
    na->state      = 1;
    gnc_gpg_decrypt_async(na->cryptext, strlen(na->cryptext), na->passphrase,
                          gnc_network_auth_crypt_cb, na);
    return TRUE;
    break;

  case 1: 
    /* waiting for decryption */
    return TRUE;
    break;

  case 2:
    if(na->cleartext && (strlen(na->cleartext) > 0)) {
      gnc_html_show_data(na->html, na->cleartext, strlen(na->cleartext));
    }
      
    /* clean up */ 
    g_free(na->cryptext);
    g_free(na->cleartext);
    g_free(na->url);
    g_free(na);
    return FALSE;
  }
  return TRUE;
}

static int
gnc_network_crypt_handler(gnc_html * html, GtkHTMLEmbedded * eb, 
                          gpointer data) {
  struct network_crypt_info * na = g_new0(struct network_crypt_info, 1);
  
  na->html      = html;
  na->cryptext  = gnc_html_unescape_newlines(eb->data);
  na->cleartext = NULL;
  na->url       = g_strdup(g_hash_table_lookup(eb->params, "url")); 
  na->state     = 0;
  
  gtk_timeout_add(100, gnc_network_crypt_check, na);
  
  return TRUE;
}
  

/********************************************************************
 * gnc_make_keypair_handler : queries for a passphrase and
 * generates a keypair in the local keyring
 ********************************************************************/

struct make_keypair_info {
  gnc_html * html;
  GtkHTMLEmbedded * eb;
  int  state;
  char * username;
  char * uid;
  char * email;
  char * passphrase;
  char * next_action;
  char * cancel_action;
};


static void
gnc_network_free_make_keypair_info(struct make_keypair_info * mk) {
  g_free(mk->username);
  g_free(mk->email);
  g_free(mk->uid);
  g_free(mk->passphrase);
  g_free(mk->cancel_action);
  g_free(mk->next_action);
  g_free(mk);
}

static void
gnc_network_make_keypair_cb(char * output, gpointer data) {
  struct make_keypair_info * mk = data;
  mk->state = 3;
}

static gboolean
gnc_network_make_keypair_check(gpointer data) {
  struct make_keypair_info * mk = data;
  char       * verify_pp = NULL;
  GHashTable * fd = NULL; 

  switch(mk->state) {
  case 0:
    /* start: get first passphrase */ 
    mk->passphrase = gnc_network_ask_passphrase(_("Enter passphrase:"));
    
    if(mk->passphrase) {
      mk->state = 1;
      return TRUE;
    }
    else {
      mk->state = 4;
      return TRUE;
    }
    break;

  case 1:
    /* get another and check for equality */ 
    verify_pp = gnc_network_ask_passphrase(_("Verify passphrase:"));
    
    if(verify_pp) {
      if(!strcmp(mk->passphrase, verify_pp)) {
        gnc_gpg_make_keypair_async(mk->username, mk->uid, mk->email, 
                                   mk->passphrase, 
                                   gnc_network_make_keypair_cb, mk);
        mk->state = 2;
        return TRUE;
      }
      else {
        gnc_error_dialog(_("Passphrases did not match."));
        gnc_network_make_keypair_handler(mk->html, mk->eb, data);
        gnc_network_free_make_keypair_info(mk);
        return FALSE;
      }      
    }
    else {
      mk->state = 4;
      return TRUE;
    }
    break;
    
  case 2:
    /* waiting for GPG to finish */ 
    return TRUE;
    
  case 3:
    /* GPG is done making the key pair */ 
    fd = g_hash_table_new(g_str_hash, g_str_equal);      
    g_hash_table_insert(fd, g_strdup("uid"), mk->uid);
    g_hash_table_insert(fd, g_strdup("url"), mk->next_action);
    gnc_network_get_handler(mk->html, "get", "get", fd);
    gnc_html_free_form_data(fd);
    mk->uid = NULL;
    mk->next_action = NULL;
    gnc_network_free_make_keypair_info(mk);
    return FALSE;
    
  case 4:
    /* cancel of passphrase get dialog */ 
    fd = g_hash_table_new(g_str_hash, g_str_equal);
    g_hash_table_insert(fd, g_strdup("url"), mk->cancel_action); 
    gnc_network_get_handler(mk->html, "get", "get", fd);
    gnc_html_free_form_data(fd);
    mk->cancel_action = NULL;
    gnc_network_free_make_keypair_info(mk);
    return FALSE;
  }
  return TRUE;
}

static int 
gnc_network_make_keypair_handler(gnc_html * html, GtkHTMLEmbedded * eb, 
                                 gpointer data) {
  struct make_keypair_info * mk = g_new0(struct make_keypair_info, 1);
  mk->html     = html;
  mk->eb       = eb;
  mk->username = g_strdup(g_hash_table_lookup(eb->params, "username"));
  mk->uid      = g_strdup(g_hash_table_lookup(eb->params, "uid"));
  mk->email    = g_strdup(g_hash_table_lookup(eb->params, "email"));
  mk->next_action   = 
    g_strdup(g_hash_table_lookup(eb->params, "next-action"));
  mk->cancel_action = 
    g_strdup(g_hash_table_lookup(eb->params, "cancel-action"));
  mk->passphrase = NULL;
  mk->state    = 0;

  gtk_timeout_add(100, gnc_network_make_keypair_check, mk);
  
  return TRUE;
}


/********************************************************************
 * gnc_network_store_uid_handler : store a new GnuCash Network user id
 * (uid) locally.
 ********************************************************************/

static int
gnc_network_store_uid_handler(gnc_html * html, GtkHTMLEmbedded * eb, 
                              gpointer data) {
  char * uid = g_hash_table_lookup(eb->params, "uid");
  gnc_set_option("__gnc_network", "uid", gh_str02scm(uid));
  return TRUE;
}


/********************************************************************
 * gnc_network_get_with_auth_handler() fetch a gnucash network page
 * with an authorizing session ID.  This either uses the current
 * session key or gets a new one if the session_id is null or expired.
 ********************************************************************/

static int
gnc_network_get_with_auth_handler(gnc_html * html, 
                                  const char * method, const char * action,
                                  GHashTable * form_data) {
  char * new_action = 
    gnc_network_build_url("gnc-network-auth.php");

  if(!g_hash_table_lookup(form_data, "submit")) {
    g_hash_table_insert(form_data, g_strdup("submit"), 
                        g_strdup("gtkhtml-bug"));
  }

  g_hash_table_insert(form_data, g_strdup("sid"), 
                      g_strdup(gnc_network_get_session_id()));
  g_hash_table_insert(form_data, g_strdup("uid"), 
                      g_strdup(gnc_network_get_uid()));
  g_hash_table_insert(form_data, g_strdup("gnc_browser"),
                      g_strdup("true"));
  
  if(!strcasecmp(method, "get")) {
    gnc_html_generic_get_submit(html, new_action, form_data);              
  }
  else {
    gnc_html_generic_post_submit(html, new_action, form_data);              
  }
  g_free(new_action);
  
  return TRUE;
}


/********************************************************************
 * gnc_network_get_handler
 * generic unauthorized 'get' for gnc-network server
 ********************************************************************/

static int
gnc_network_get_handler(gnc_html * html, 
                        const char * method, const char * action, 
                        GHashTable * form_data) {
  char * new_action = 
    gnc_network_build_url("gnc-network-get.php");
  
  if(!g_hash_table_lookup(form_data, "submit")) {
    g_hash_table_insert(form_data, g_strdup("submit"), 
                        g_strdup("gtkhtml-bug"));
  }

  g_hash_table_insert(form_data, g_strdup("uid"),
                      g_strdup(gnc_network_get_uid()));
  g_hash_table_insert(form_data, g_strdup("gnc_browser"),
                      g_strdup("true"));
  
  /* gnc_html_merge_form_data(form_data, act_args); */
  if(!strcasecmp(method, "get")) {
    gnc_html_generic_get_submit(html, new_action, form_data);              
  }
  else {
    gnc_html_generic_post_submit(html, new_action, form_data);              
  }
  g_free(new_action);
  
  return TRUE;
}


/********************************************************************
 * gnc_network_send_info_handler() : submit the form arguments
 * from 'encoding', appending additional arguments describing the
 * gnucash client: gnucash version string and some features.
 ********************************************************************/

static int
gnc_network_send_info_handler(gnc_html * html, 
                              const char * method, const char * action, 
                              GHashTable * form_data) {
  char * new_action = NULL;
  char * version_string = NULL;
  char * feature_string = NULL;
  
  if(!g_hash_table_lookup(form_data, "submit")) {
    g_hash_table_insert(form_data, g_strdup("submit"), 
                        g_strdup("gtkhtml-bug"));
  }
  
  if(!method || !action 
     || strcmp(action, "get/info")) {
    return FALSE;
  }
  
  version_string  = gh_scm2newstr(gh_eval_str("gnc:version"), NULL);  

  feature_string = 
    g_strjoin(",",
#if USE_GUPPI
              "guppi",
#endif
              "gpg",
#if HAVE_OPENSSL
              "openssl",
#endif
              NULL);

  g_hash_table_insert(form_data, g_strdup("gnc_browser"), g_strdup("true"));
  g_hash_table_insert(form_data, g_strdup("gnc_version"), 
                      g_strdup(version_string));
  g_hash_table_insert(form_data, g_strdup("gnc_features"), 
                      feature_string);
  free(version_string);

  new_action = gnc_network_build_url("gnc-network-get.php");
  
  if(!strcasecmp(method, "get")) {
    gnc_html_generic_get_submit(html, new_action, form_data);
  }
  else {
    gnc_html_generic_post_submit(html, new_action, form_data);
  }    
  
  g_free(new_action);
  return TRUE;
}


/********************************************************************
 * gnc_network_send_pubkey_handler : send the public key as a multipart
 * POST submit
 ********************************************************************/

static int
gnc_network_submit_key_handler(gnc_html * html, 
                               const char * method, const char * action,
                               GHashTable * form_data) {
  char * keyid = g_hash_table_lookup(form_data, "uid");
  char * submit = g_hash_table_lookup(form_data, "submit");     
  char * keytext = NULL;
  char * new_action = 
    gnc_network_build_url("gnc-network-get.php");
  
  if(!g_hash_table_lookup(form_data, "submit")) {
    g_hash_table_insert(form_data, g_strdup("submit"), 
                        g_strdup("gtkhtml-bug"));
  }
  g_hash_table_insert(form_data, g_strdup("gnc_browser"), g_strdup("true"));

  if(!submit || !strcmp(submit, "OK")) {
    /* get the public key */
    if(keyid) {
      keytext = gnc_gpg_export(keyid);
    }
    
    /* stick it in the form data */
    g_hash_table_insert(form_data, g_strdup("pubkey"), keytext);
    
    /* submit as a multipart form */
    gnc_html_multipart_post_submit(html, new_action, form_data);
  }
  else {
    /* don't export the pubkey, just submit as is. */
    gnc_html_generic_post_submit(html, new_action, form_data);
  }
  g_free(new_action);
  return TRUE;
}


/********************************************************************
 * gnc_network_init() 
 * set up GnuCash Network handlers 
 ********************************************************************/

void  
gnc_network_init(void) {
  /* <object> handlers */
  gnc_html_register_object_handler("gnc-network-auth",
                                   gnc_network_auth_handler);
  gnc_html_register_object_handler("gnc-network-crypt",
                                   gnc_network_crypt_handler);
  gnc_html_register_object_handler("gnc-make-keypair",
                                   gnc_network_make_keypair_handler);
  gnc_html_register_object_handler("gnc-store-uid",
                                   gnc_network_store_uid_handler);

  /* <form> and gnc-action: handlers */ 
  gnc_html_register_action_handler("get", 
                                   gnc_network_get_handler);
  gnc_html_register_action_handler("get/auth", 
                                   gnc_network_get_with_auth_handler);
  gnc_html_register_action_handler("get/info", 
                                   gnc_network_send_info_handler);
  gnc_html_register_action_handler("submit-key",
                                   gnc_network_submit_key_handler); 
}




