/********************************************************************
 * gnc-gpg.c -- encrypt/decrypt data using GPG and the gnucash      *
 * keyrings                                                         *
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

#if USE_GPG

#include <sys/types.h>
#include <sys/stat.h>
#include <gnome.h>
#include <unistd.h>
#include <glib.h>

#include <gtkhtml/gtkhtml.h>
#include <gtkhtml/gtkhtml-embedded.h>

#include "global-options.h"
#include "gnc-ui.h"
#include "gnc-html.h"
#include "gnc-gpg.h"

/* handlers for embedded <object> tags */ 
static int gnc_crypted_html_handler(gnc_html * html, GtkHTMLEmbedded * eb, 
                                    gpointer d);
static int gnc_make_keypair_handler(gnc_html * html, GtkHTMLEmbedded * eb, 
                                    gpointer d);

/* handlers for gnc-action: <form> submit actions */ 
static int gnc_submit_pubkey_handler(gnc_html * html, 
                                     const char * method, const char * action,
                                     const char * act_args, 
                                     GHashTable * form_data);

/********************************************************************
 * gnc_gpg_init : called at startup time.  adds handlers for GPG
 * related network transactions
 ********************************************************************/

void
gnc_gpg_init(void) {
  gnc_html_register_object_handler("gnc-crypted-html", 
                                   gnc_crypted_html_handler);
  gnc_html_register_object_handler("gnc-make-keypair",
                                   gnc_make_keypair_handler);

  gnc_html_register_action_handler("submit-pubkey",
                                   gnc_submit_pubkey_handler); 
}


static char * 
unescape_newlines(const gchar * in) {
  const char * ip = in;
  char * retval = g_strdup(in);
  char * op = retval;

  for(ip=in; *ip; ip++) {
    if((*ip == '\\') && (*(ip+1)=='n')) {
      *op = '\012';
      op++;
      ip++;
    }
    else {
      *op = *ip;
      op++;
    }
  }
  *op = 0;
  return retval;
}


/********************************************************************
 * gnc_get_passphrase : util to get a passphrase. 
 ********************************************************************/

static void 
get_pp_string_cb(char * string, gpointer data) {
  int l, i;

  if(!data) return;
  if(!string) {
    *((char **)data) = NULL;
  }
  else {
    *((char **)data) = g_strdup(string);    
  }
}

static char * 
gnc_get_passphrase(const char * prompt) {
  char      * pp = NULL;
  gint      retval;
  GtkWidget * dlg = 
    gnome_request_dialog(TRUE, prompt, "", 1024,
                         get_pp_string_cb, (gpointer)&pp, NULL);
  retval = gnome_dialog_run(GNOME_DIALOG(dlg));
  if(retval == 0) {
    return pp;
  }
  else {
    printf("get_passphrase canceled.\n");
    return NULL;
  }
}

static void
gnc_free_passphrase(char * pp) {
  int l, i;
  if(!pp) return;
  l = strlen(pp);
  for(i=0;i<l;i++) {
    pp[i]=0;
  }
  g_free(pp);
}


/********************************************************************
 * gnc_crypted_html_handler : receives ascii-armored GPG cryptext,
 * decrypts it, and displays the result as HTML. 
 ********************************************************************/

static int
gnc_crypted_html_handler(gnc_html * html, GtkHTMLEmbedded * eb, 
                         gpointer data) {
  int  retval = TRUE;
  char * cryptext  = unescape_newlines(eb->data);
  char * passphrase = gnc_get_passphrase("Enter passphrase:");
  char * cleartext;

  if(passphrase) {
    cleartext  = gnc_gpg_decrypt(cryptext, strlen(cryptext), passphrase);
    if(cleartext && cleartext[0]) {
      gnc_html_show_data(html, cleartext, strlen(cleartext));
    }
    else {
      retval = FALSE;
    }
    g_free(cleartext);
    g_free(cryptext);
    gnc_free_passphrase(passphrase);
  }
  return retval;
}


/********************************************************************
 * gnc_make_keypair_handler : queries for a passphrase and
 * generates a keypair in the local keyring
 ********************************************************************/

static int
gnc_make_keypair_handler(gnc_html * html, GtkHTMLEmbedded * eb, 
                         gpointer data) {
  char * first_pp      = NULL;
  char * verify_pp     = NULL;
  char * username      = g_hash_table_lookup(eb->params, "username");
  char * userid        = g_hash_table_lookup(eb->params, "userid");
  char * email         = g_hash_table_lookup(eb->params, "email");
  char * next_action   = g_hash_table_lookup(eb->params, "next-action");
  char * cancel_action = g_hash_table_lookup(eb->params, "cancel-action");
  int retval = TRUE;
  URLType type;
  char * location = NULL;
  char * label = NULL;

  first_pp = gnc_get_passphrase("Enter passphrase:");
  if(first_pp) {
    verify_pp = gnc_get_passphrase("Verify passphrase:");
  }
  else {
    type = gnc_html_parse_url(html, cancel_action, &location, &label);
    gnc_html_show_url(html, type, location, label, 0);
    g_free(location);
    g_free(label);
  }

  if(verify_pp) {
    if(!strcmp(first_pp, verify_pp)) {
      printf("Got passphrase '%s' (shhh! don't tell).  Generating keypair.\n",
             first_pp);
      gnc_gpg_make_keypair(username, userid, email, first_pp);
      printf("...done\n");
      gnc_free_passphrase(first_pp);
      gnc_free_passphrase(verify_pp);
      type = gnc_html_parse_url(html, next_action, &location, &label);
      gnc_html_show_url(html, type, location, label, 0);
      g_free(location);
      g_free(label);
    }
    else {
      gnc_free_passphrase(first_pp);
      gnc_free_passphrase(verify_pp);
      gnc_error_dialog(_("Passphrases did not match."));
      gnc_make_keypair_handler(html, eb, data);
    }
  }
  else {
    gnc_free_passphrase(first_pp);
    type = gnc_html_parse_url(html, cancel_action, &location, &label);
    gnc_html_show_url(html, type, location, label, 0);
    g_free(location);
    g_free(label);
  }

  return retval;
}


/********************************************************************
 * gnc_submit_pubkey_handler : send the public key as a multipart
 * POST submit
 ********************************************************************/

static int
gnc_submit_pubkey_handler(gnc_html * html, const char * method,
                          const char * action, const char * args, 
                          GHashTable * form_data) {
  char * keyid = g_hash_table_lookup(form_data, "gnc_keyid");
  char * submit = g_hash_table_lookup(form_data, "submit");     
  char * keytext = NULL;
  char * gnc_net_server = 
    gnc_lookup_string_option("Network", "GnuCash Network server", 
                             "www.gnumatic.com");
  char * new_action = g_strconcat("http://", gnc_net_server, "/", args, NULL);
  
  if(!strcmp(submit, "OK")) {
    /* get the public key */
    if(keyid) {
      keytext = gnc_gpg_export(keyid);
    }
    
    /* stick it in the form data */
    g_hash_table_insert(form_data, g_strdup("gnc_user_pubkey"), keytext);
    
    /* submit as a multipart form */
    gnc_html_multipart_post_submit(html, new_action, form_data);
  }
  else {
    /* don't export the pubkey, just submit as is. */
    printf("canceled\n");
    gnc_html_generic_post_submit(html, new_action, form_data);
  }
  return TRUE;
}

static char *
gnc_gpg_transform(const gchar * input, gint input_size, 
                  const char * passphrase, char ** gpg_argv) {
  int   pid;
  int   to_child[2];
  int   from_child[2];
  int   bytes;
  int   block_bytes;
  int   total_bytes;
  GList * data = NULL;
  GList * blockptr;
  char  * current_blk = NULL;
  char  * retval;
  char  * insert_pos;
  
  /* create a pipe for talking to the child gpg process */
  if((pipe(to_child) != 0) || (pipe(from_child) != 0)) {
    return NULL;
  }
  
  if((pid = fork()) != 0) {
    close(to_child[0]);
    close(from_child[1]);

    /* parent process. write passphrase to pipe first. */
    if(passphrase) {
      total_bytes = 0;    
      while(total_bytes < strlen(passphrase)) {
        bytes = write(to_child[1], 
                      passphrase+total_bytes, 
                      strlen(passphrase)-total_bytes); 
        if(bytes < 0) break;
        else {
          total_bytes += bytes;
        }
      }
    }

    /* now write data */ 
    total_bytes = 0;
    while(total_bytes < input_size) {
      bytes = write(to_child[1], input+total_bytes, input_size-total_bytes); 
      if(bytes < 0) break;
      else {
        total_bytes += bytes;
      }
    }
    close(to_child[1]);

    /* read transformed data back */ 
    current_blk = g_new0(char, 1024);
    block_bytes = 0;
    total_bytes = 0;

    while((bytes = read(from_child[0], current_blk, 1024-block_bytes)) > 0) {
      block_bytes += bytes;
      total_bytes += bytes;
      if(block_bytes == 1024) {
        data = g_list_append(data, current_blk);
        current_blk = g_new0(char, 1024);
        block_bytes = 0;
      }
    }
    
    /* collapse the output text into a single array */
    retval = g_new0(char, total_bytes);
    insert_pos = retval;
    for(blockptr = data; blockptr; blockptr = blockptr->next) {
      memcpy(insert_pos, blockptr->data, 1024);
      g_free(blockptr->data);
      blockptr->data = NULL;
      insert_pos += 1024;
    }
    memcpy(insert_pos, current_blk, block_bytes);
    g_free(current_blk);
    g_list_free(data);
    
    return retval;
  }
  else {
    close(to_child[1]);
    close(from_child[0]);
    
    /* child process. Set up pipes. */
    close(0);
    close(1);
    close(2);  /* stderr to /dev/null ... gpg is a chatty thing */

    /* hook up the pipes */
    dup(to_child[0]);
    dup(from_child[1]);

    /* new files aren't readable by anybody but the user */ 
    umask(077);

    if(execvp("gpg", gpg_argv)) {
      exit(-1);
    }
    return NULL;
  }
}


/********************************************************************
 *  gnc_gpg_encrypt
 *  transform a cleartext to a crypted text for the given recipient 
 ********************************************************************/

char * 
gnc_gpg_encrypt(const gchar * cleartext, int cleartext_size, 
                const gchar * recipient, const gchar * passphrase) {
  char * retval = NULL;
  char * argv[] = 
  {
    "gpg",
    "-q",
    "--batch",
    "-sea",
    "--keyring",
    "~/.gnucash/gnucash.pub",
    "--secret-keyring",
    "~/.gnucash/gnucash.sec",
    "-r",
    NULL,
    "--passphrase-fd",
    "0",
    NULL
  };
  
  argv[7] = (char *)recipient;

  retval = gnc_gpg_transform(cleartext, cleartext_size, passphrase, argv);
  return retval;
}


/********************************************************************
 *  gnc_gpg_decrypt
 *  transform a crypted text into a cleartext.
 ********************************************************************/

char * 
gnc_gpg_decrypt(const gchar * cryptext, int cryptext_size,
                const gchar * passphrase) {
  char * retval = NULL;

  char * argv[] = 
  { "gpg",
    "-q",
    "--batch",
    "-da",
    "--keyring",
    "~/.gnucash/gnucash.pub",
    "--secret-keyring",
    "~/.gnucash/gnucash.sec",
    "--passphrase-fd",
    "0",
    NULL
  };
  
  retval = gnc_gpg_transform(cryptext, cryptext_size, passphrase, argv);
  
  return retval;
}


/********************************************************************
 *  gnc_gpg_export
 *  get a public key (ASCII armored)
 ********************************************************************/

char * 
gnc_gpg_export(const gchar * keyname) {
  char * retval;
  char * argv[] = 
  { "gpg",
    "-q",
    "--export",
    "-a",
    NULL,
    NULL
  };
  argv[4] = keyname;
  retval = gnc_gpg_transform(NULL, 0, NULL, argv);
  return retval;
}


/********************************************************************
 *  gnc_gpg_make_keypair
 *  make a keypair for gnucash use 
 ********************************************************************/

void
gnc_gpg_make_keypair(const gchar * username,
                     const gchar * idstring,
                     const gchar * email,
                     const gchar * passphrase) {
  char * stdin = 
    g_strdup_printf("Key-Type: DSA\n"
                    "Key-Length: 1024\n"
                    "Subkey-Type: ELG-E\n"
                    "Subkey-Length: 1024\n"
                    "Name-Real: %s\n"
                    "Name-Comment: %s\n"
                    "Name-Email: %s\n"
                    "Passphrase: %s\n"
                    "%%commit\n",
                    username, idstring, email, passphrase);
  char * argv [] = 
  { "gpg",
    "--batch",
    "-q",
    "--gen-key",
    "--keyring",
    "~/.gnucash/gnucash.pub",
    "--secret-keyring",
    "~/.gnucash/gnucash.sec",
    NULL
  };

  char * retval = gnc_gpg_transform(stdin, strlen(stdin), NULL, argv);
  g_free(stdin);
}

#endif
