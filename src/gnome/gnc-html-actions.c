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
 *  and can modify the information being sent or send additional
 *  information.
 * 
 *  Don't panic; all that means is that when an HTML form is loaded
 *  into the Gnucash HTML browser, after the user clicks the "submit"
 *  button, if the "action" in the form definition looks like
 *  "gnc-action:ACTION-NAME?ACTION-ARGS", the action-name is used to
 *  find a handler and the form is submitted by that handler.  The
 *  default action handlers allow Gnucash to send information about
 *  itself along with a form, to send the form data in a GPG-encrypted
 *  block, etc.  This is useful functionality, but there are real
 *  privacy concerns.
 * 
 *  Users: keep in mind that this is *not* a mechanism for executing
 *  arbitrary code in your gnucash; the only "actions" that can be
 *  taken must be compiled into the client, and you can disable them
 *  globally with a Preferences option.
 * 
 *  Developers: Until we have a formally-codified privacy policy,
 *  please follow these guidelines:
 * 
 *  1. When actions send information that the user has not explicitly
 *  entered, there should *always* be a dialog or other notification,
 *  including the extra information to be sent, with an opportunity to
 *  bail out.
 * 
 *  2. Do not use a accept a complete URI to submit to as an argument
 *  to actions.  This might allow a malicious server to ask the
 *  gnucash client for private information to be sent to itself, if
 *  the user happened to go to that site from within the gnucash HTML
 *  browser and click a form "SUBMIT" button.  Submit only to servers
 *  whose names the local user has specified as trusted servers for
 *  gnucash-related actions.
 ********************************************************************/

#include "config.h"
#include <string.h>
#include <glib.h>

#include "gnc-html-actions.h"
#include "gnc-html.h"

static int handle_gnc_info_form_submit(gnc_html * html, const char * method, 
                                       const char * action, const char * args, 
                                       const char * encoding);

/********************************************************************
 * gnc_html_actions_init() : register the basic set of gnc-action:
 * form submission actions.
 ********************************************************************/

void
gnc_html_actions_init(void) {
  gnc_html_register_action_handler("gnc-info/form", 
                                   handle_gnc_info_form_submit);
}


/********************************************************************
 * handle_gnc_info_form_submit() : submit the form arguments from
 * 'encoding', appending additional arguments describing the gnucash
 * client.  this is justa test and doesn't submit any real
 * information.
 ********************************************************************/

static int
handle_gnc_info_form_submit(gnc_html * html, const char * method, 
                            const char * action, const char * args, 
                            const char * encoding) {
  char * extra_encoding = NULL;
  char * new_encoding   = NULL;
  char * new_action = NULL;
  char * msg_1 = gnc_html_encode_string("gnucash version 1.5");
  char * msg_2 = gnc_html_encode_string("Hello, world");

  if(!method || !action 
     || strcmp(action, "gnc-info/form")) {
    return FALSE;
  }
  
  extra_encoding = g_strconcat("gnc_version=", msg_1, "&" 
                               "gnc_message=", msg_2, NULL);
  
  if(encoding) {
    new_encoding = g_strjoin("&", encoding, extra_encoding, NULL);
  }
  else {
    new_encoding = extra_encoding;
    extra_encoding = NULL;
  }

  new_action = g_strconcat("http://localhost/", args, NULL);
  
  if(!strcasecmp(method, "get")) {
    gnc_html_generic_get_submit(html, new_action, new_encoding);
  }
  else {
    gnc_html_generic_post_submit(html, new_action, new_encoding);
  }    
  
  g_free(extra_encoding);
  g_free(new_encoding);
  g_free(new_action);
  g_free(msg_1);
  g_free(msg_2);
  
  return TRUE;
}
