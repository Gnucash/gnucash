/********************************************************************
 * gnc-html-actions.c -- form submission actions                    *
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

#include "config.h"
#include <string.h>
#include <glib.h>
#include <guile/gh.h>

#include "global-options.h"
#include "gnc-html-actions.h"
#include "gnc-html.h"

static int 
gnc_info_form_submit_handler(gnc_html * html, const char * method, 
                             const char * action, const char * args, 
                             GHashTable * form_data);

/********************************************************************
 * gnc_html_actions_init() : register the basic set of gnc-action:
 * form submission actions.
 ********************************************************************/

void
gnc_html_actions_init(void) {
  gnc_html_register_action_handler("gnc-info/form", 
                                   gnc_info_form_submit_handler);
}


/********************************************************************
 * gnc_info_form_submit_handler() : submit the form arguments from
 * 'encoding', appending additional arguments describing the gnucash
 * client: gnucash version string and some features.
 ********************************************************************/

static int
gnc_info_form_submit_handler(gnc_html * html, const char * method, 
                             const char * action, const char * args, 
                             GHashTable * form_data) {
  char * version_string = NULL;
  char * new_action = NULL;
  char * gnc_net_server = 
    gnc_lookup_string_option("Network", "GnuCash Network server", 
                             "www.gnumatic.com");
  char * feature_string = NULL;

  if(!method || !action 
     || strcmp(action, "gnc-info/form")) {
    return FALSE;
  }
  
  version_string  = gh_scm2newstr(gh_eval_str("gnc:version"), NULL);  

  feature_string = 
    g_strjoin(",",
#if USE_GUPPI
              "guppi",
#endif
#if USE_GPG 
              "gpg",
#endif
#if HAVE_OPENSSL
              "openssl",
#endif
              NULL);
  g_hash_table_insert(form_data, g_strdup("gnc_browser"), g_strdup("1"));
  g_hash_table_insert(form_data, g_strdup("gnc_version"), 
                      g_strdup(version_string));
  g_hash_table_insert(form_data, g_strdup("gnc_features"), 
                      feature_string);
  free(version_string);

  new_action = g_strconcat("http://", gnc_net_server, "/", args, NULL);
  
  if(!strcasecmp(method, "get")) {
    gnc_html_generic_get_submit(html, new_action, form_data);
  }
  else {
    gnc_html_generic_post_submit(html, new_action, form_data);
  }    
  
  g_free(gnc_net_server);
  g_free(new_action);
  return TRUE;
}
