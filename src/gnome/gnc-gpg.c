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

#include <string.h>
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
#include "gnc-network.h"
#include "gnc-gpg.h"


/********************************************************************
 * gnc_gpg_transform(_async) : call GPG with specified input and args
 * the _async variety returns immediately and calls a callback when 
 * complete.   
 ********************************************************************/

struct gpg_transform_data {
  GString  * str;
  gint     tag;
  GncGPGCb cb;
  gpointer data;
};

static void 
gnc_gpg_transform_helper(gpointer data, gint fd, GdkInputCondition cond) {
  struct gpg_transform_data * td = data;
  int    bytes_read;
  char   buf[1025];
  char   * cstr;

  buf[1024] = 0;
  
  if(cond == GDK_INPUT_READ) {
    while((bytes_read = read(fd, buf, 1024)) == 1024) {
      g_string_append(td->str, buf);
    }
    if(bytes_read > 0) {
      buf[bytes_read] = 0;
      g_string_append(td->str, buf);
    }
    else {
      /* we're done.  call the callback */ 
      gdk_input_remove(td->tag);
      cstr = td->str->str;
      g_string_free(td->str, FALSE);
      (td->cb)(cstr, td->data);
      g_free(td);    
    }
  }
  else {
    gdk_input_remove(td->tag);
  }
}

static void
gnc_gpg_transform_async(const gchar * input, gint input_size, 
                        const char * passphrase, char ** gpg_argv,
                        GncGPGCb cb, gpointer cb_data) {
  int     pid;
  int     to_child[2];
  int     from_child[2];
  int     bytes;
  int     total_bytes;
  struct gpg_transform_data * td;

  /* create a pipe for talking to the child gpg process */
  if((pipe(to_child) != 0) || (pipe(from_child) != 0)) {
    return;
  }
  
  /* create the process. */ 
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
        if(bytes < 0) {
          break;
        }
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

    td          = g_new0(struct gpg_transform_data, 1);
    td->str     = g_string_new("");
    td->cb      = cb;
    td->data    = cb_data;
    
    /* read transformed data back in the input handler */ 
    td->tag     = gdk_input_add(from_child[0], GDK_INPUT_READ, 
                                gnc_gpg_transform_helper, td);
  }         
  else {
    close(to_child[1]);
    close(from_child[0]);
    
    /* child process. Set up pipes. */
    close(0);
    close(1);
    /* stderr to /dev/null ... gpg is a chatty thing */
    close(2);

    /* hook up the pipes */
    dup(to_child[0]);
    dup(from_child[1]);

    /* new files aren't readable by anybody but the user */ 
    umask(077);

    if(execvp("gpg", gpg_argv)) {
      char buf[1024];
      /* get data from parent */
      while((bytes = read(to_child[0], buf, 1024)) > 0) {        
        bytes = 0;
      }
      /* give 'em a little back */
      write(from_child[1], "\n", 1);
      _exit(0);
    }
  }
}

static void
gnc_gpg_transform_sync_helper(char * output, gpointer data) {
  const char ** status = data;
  status[1] = output;
  status[0] = (char *)1;
  
}

static char * 
gnc_gpg_transform(const gchar * input, gint input_size, 
                  const char * passphrase, char ** gpg_argv) {
  char * out[2] = { NULL, NULL };
  gnc_gpg_transform_async(input, input_size, passphrase, gpg_argv,
                          gnc_gpg_transform_sync_helper, (gpointer)out);
  while(!out[0]) {
    gtk_main_iteration();
  }
  return out[1];
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

void
gnc_gpg_encrypt_async(const gchar * cleartext, int cleartext_size, 
                      const gchar * recipient, const gchar * passphrase,
                      GncGPGCb cb, gpointer data) {
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
  
  gnc_gpg_transform_async(cleartext, cleartext_size, passphrase, argv,
                          cb, data);
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

void
gnc_gpg_decrypt_async(const gchar * cryptext, int cryptext_size,
                      const gchar * passphrase, 
                      void (* cb)(char * clrtxt, gpointer d),
                      gpointer data) {
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

  gnc_gpg_transform_async(cryptext, cryptext_size, passphrase, argv,
                          cb, data);
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
    "--keyring",
    "~/.gnucash/gnucash.pub",
    NULL,
    NULL
  };
  argv[6] = g_strdup_printf("(%s)", keyname);
  retval = gnc_gpg_transform(NULL, 0, NULL, argv);
  g_free(argv[6]);
  return retval;
}

void
gnc_gpg_export_async(const gchar * keyname, GncGPGCb cb, gpointer data) {
  char * retval;
  char * argv[] = 
  { "gpg",
    "-q",
    "--export",
    "-a",
    "--keyring",
    "~/.gnucash/gnucash.pub",
    NULL,
    NULL
  };
  argv[6] = g_strdup_printf("(%s)", keyname);
  gnc_gpg_transform_async(NULL, 0, NULL, argv, cb, data);
  g_free(argv[6]);
}


/********************************************************************
 *  gnc_gpg_make_keypair
 *  make a keypair for gnucash use 
 ********************************************************************/

char *
gnc_gpg_make_keypair(const gchar * username,
                     const gchar * idstring,
                     const gchar * email,
                     const gchar * passphrase) {
  char * gpg_input = 
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

  char * retval = gnc_gpg_transform(gpg_input, strlen(gpg_input), NULL, argv);
  g_free(gpg_input);
  return retval;
}

void
gnc_gpg_make_keypair_async(const gchar * username,
                           const gchar * idstring,
                           const gchar * email,
                           const gchar * passphrase,
                           GncGPGCb cb, gpointer data) {
  char * gpg_input = 
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

  gnc_gpg_transform_async(gpg_input, strlen(gpg_input), NULL, argv, cb, data);
  g_free(gpg_input);
}
