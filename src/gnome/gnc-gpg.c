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

#include "gnc-html.h"
#include "gnc-gpg.h"

static int handle_gpg_html(gnc_html * html, GtkHTMLEmbedded * eb, gpointer d);

/********************************************************************
 * gnc_gpg_init : called at startup time.  adds a handler for crypted
 * HTML to gnc-html.
 ********************************************************************/

void
gnc_gpg_init(void) {
  gnc_html_register_object_handler("gnc-crypted-html", handle_gpg_html);
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

/* we just want to take the data and stuff it into the gnc-html
 * widget, blowing away the active streams.  crypted-html contains a
 * complete HTML page. */
static int
handle_gpg_html(gnc_html * html, GtkHTMLEmbedded * eb, gpointer data) {
  int  retval;
  char * cryptext  = unescape_newlines(eb->data);
  char * cleartext = gnc_gpg_decrypt(cryptext, strlen(cryptext));
  GtkHTMLStream * handle;
  if(cleartext && cleartext[0]) {
    handle = gtk_html_begin(html);
    gtk_html_write(html, handle, cleartext, strlen(cleartext));
    gtk_html_end(html, handle, GTK_HTML_STREAM_OK);
    retval = TRUE;
  }
  else {
    retval = FALSE;
  }
  g_free(cleartext);
  g_free(cryptext);

  return retval;
}


static char *
gnc_gpg_transform(const gchar * input, gint input_size, 
                  char ** gpg_argv) {
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

    /* parent process. write cleartext to pipe. */
    total_bytes = 0;
    while(total_bytes < input_size) {
      bytes = write(to_child[1], input, input_size-total_bytes); 
      if(bytes < 0) break;
      else {
        total_bytes += bytes;
      }
    }
    close(to_child[1]);

    /* read encrypted data back */ 
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
                const gchar * recipient) {
  char * argv[11];
  char * retval = NULL;

  argv[0] = "gpg";
  argv[1] = "-q";
  argv[2] = "--batch";
  argv[3] = "-sea";
  argv[4] = "--keyring";
  argv[5] = "~/.gnucash/gnucash.pub";
  argv[6] = "--secret-keyring";
  argv[7] = "~/.gnucash/gnucash.sec";
  argv[8] = "-r";
  argv[9] = g_strdup(recipient);
  argv[10] = NULL;

  retval = gnc_gpg_transform(cleartext, cleartext_size, argv);
  g_free(argv[7]);
  return retval;
}


/********************************************************************
 *  gnc_gpg_decrypt
 *  transform a crypted text into a cleartext.
 ********************************************************************/

char * 
gnc_gpg_decrypt(const gchar * cryptext, int cryptext_size) {
  char * argv[9];
  char * retval = NULL;

  argv[0] = "gpg";
  argv[1] = "-q";
  argv[2] = "--batch";
  argv[3] = "-da";
  argv[4] = "--keyring";
  argv[5] = "~/.gnucash/gnucash.pub";
  argv[6] = "--secret-keyring";
  argv[7] = "~/.gnucash/gnucash.sec";
  argv[8] = NULL;
  
  retval = gnc_gpg_transform(cryptext, cryptext_size, argv);
  
  return retval;
}


/********************************************************************
 *  gnc_gpg_make_keypair
 *  make a passphraseless keypair for gnucash use 
 ********************************************************************/

void
gnc_gpg_make_keypair(const gchar * username,
                     const gchar * idstring,
                     const gchar * email) {
  char * stdin = 
    g_strdup_printf("Key-Type: DSA\n"
                    "Key-Length: 1024\n"
                    "Subkey-Type: ELG-E\n"
                    "Subkey-Length: 1024\n"
                    "Name-Real: %s\n"
                    "Name-Comment: %s\n"
                    "Name-Email: %s\n"
                    "%%pubring: ~/.gnucash/gnucash.pub\n"
                    "%%secring: ~/.gnucash/gnucash.sec\n"
                    "%%commit\n",
                    username, idstring, email);
  char * argv [4];
  
  argv[0] = "gpg";
  argv[1] = "--batch";
  argv[2] = "-q";
  argv[3] = "--gen-key";
  argv[4] = NULL;

  gnc_gpg_transform(stdin, strlen(stdin), argv);
  g_free(stdin);
}

#endif
