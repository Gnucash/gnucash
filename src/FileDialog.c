/********************************************************************\
 * FileDialog.c -- file-handling utility dialogs for gnucash.       * 
 *                                                                  *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998, 1999, 2000 Linas Vepstas                     *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <guile/gh.h>
#include <string.h>

#include "Backend.h"
#include "FileBox.h"
#include "FileDialog.h"
#include "Group.h"
#include "file-history.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "gnc-event.h"
#include "gnc-ui.h"
#include "messages.h"

/* FIXME: this is wrong.  This file should not need this include. */
#include "gnc-book-p.h"

/** GLOBALS *********************************************************/
/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static GNCBook *current_book = NULL;


/* ======================================================== */

static gboolean
show_book_error (GNCBackendError io_error, const char *newfile)
{
  gboolean uh_oh = TRUE;
  const char *fmt;
  char *buf = NULL;

  if (NULL == newfile) { newfile = _("(null)"); }

  switch (io_error)
  {
    case ERR_BACKEND_NO_ERR:
      uh_oh = FALSE;
      break;

    case ERR_BACKEND_NO_BACKEND:
      fmt = _("The URL \n    %s\n"
               "is not supported by this version of GnuCash.");
      buf = g_strdup_printf (fmt, newfile);
      gnc_error_dialog (buf);
      break;

    case ERR_BACKEND_LOCKED:
      fmt = _("The URL \n    %s\n"
               "is in use by another user.");
      buf = g_strdup_printf (fmt, newfile);
      gnc_error_dialog (buf);
      break;

    case ERR_BACKEND_MISC:
      fmt = _("An error occurred while processing\n    %s\n");
      buf = g_strdup_printf (fmt, newfile);
      gnc_error_dialog (buf);
      break;

    case ERR_FILEIO_FILE_BAD_READ:
      fmt = _("There was an error reading the file.\n"
              "Do you want to continue?");
      if (gnc_verify_dialog (fmt, TRUE)) { uh_oh = FALSE; }
      break;

    case ERR_FILEIO_FILE_EMPTY:
      fmt = _("The file \n    %s\n is empty.");
      buf = g_strdup_printf (fmt, newfile);
      gnc_error_dialog (buf);
      break;

    case ERR_FILEIO_FILE_NOT_FOUND:
      fmt = _("The file \n    %s\n could not be found.");
      buf = g_strdup_printf (fmt, newfile);
      gnc_error_dialog (buf);
      break;

    case ERR_FILEIO_FILE_TOO_NEW:
      fmt = _("This file appears to be from a newer version\n"
              "of GnuCash. You must upgrade GnuCash to read\n"
              "this file.");
      gnc_error_dialog (fmt);
      break;

    case ERR_FILEIO_FILE_TOO_OLD:
      fmt = _("This file is from an older version of GnuCash.\n"
              "Do you want to continue?");
      if (gnc_verify_dialog (fmt, TRUE)) { uh_oh = FALSE; }
      break;

    case ERR_SQL_BAD_LOCATION:
      fmt = _("Can't parse the database URL\n   %s\n");
      buf = g_strdup_printf (fmt, newfile);
      gnc_error_dialog (buf);
      break;

    case ERR_SQL_CANT_CONNECT:
      fmt = _("Can't connect to the database\n   %s\n"
              "The host, username or password were incorrect.");
      buf = g_strdup_printf (fmt, newfile);
      gnc_error_dialog (buf);
      break;

    default:
      PERR("FIXME: Unhandled error %d", io_error);
      fmt = _("An unknown I/O error occurred.");
      gnc_error_dialog (fmt);
      break;
  }

  if (buf) g_free(buf);
  return uh_oh;
}

/* ======================================================== */

static gboolean
gncLockFailHandler (const char *file)
{
  const char *format = _("Gnucash could not obtain the lock for\n"
                         "   %s.\n"
                         "That database may be in use by another user,\n"
                         "in which case you should not open the database.\n"
                         "\nDo you want to proceed with opening the database?");
  char *message;
  gboolean result;

  if (file == NULL)
    return FALSE;

  message = g_strdup_printf (format, file);
  result = gnc_verify_dialog (message, FALSE);
  g_free (message);

  return result;
}

/* ======================================================== */

static gboolean
gncCreateFailHandler (const char *file)
{
  const char *format = _("The database\n"
                         "   %s\n"
                         "doesn't seem to exist.  Do you want to create it?\n");
  char *message;
  gboolean result;

  if (file == NULL)
    return FALSE;

  message = g_strdup_printf (format, file);
  result = gnc_verify_dialog (message, FALSE);
  g_free (message);

  return result;
}

/* ======================================================== */

static void
gncAddHistory (GNCBook *book)
{
  char *url;

  if (!book) return;

  url = xaccResolveURL (gnc_book_get_url (book));

  if (strncmp (url, "file:", 5) == 0)
    gnc_history_add_file (url + 5);
  else
    gnc_history_add_file (url);

  g_free (url);
}

/* ======================================================== */

void
gncFileNew (void)
{
  GNCBook *book;

  /* If user attempts to start a new session before saving results of
   * the last one, prompt them to clean up their act. */
  if (!gncFileQuerySave ())
    return;

  book = gncGetCurrentBook ();

  /* close any ongoing file sessions, and free the accounts.
   * disable events so we don't get spammed by redraws. */
  gnc_engine_suspend_events ();

  gnc_book_destroy (book);
  current_book = NULL;

  /* start a new book */
  gncGetCurrentBook ();

  gnc_engine_resume_events ();
  gnc_gui_refresh_all ();
}

/* ======================================================== */

gboolean
gncFileQuerySave (void)
{
  GNCBook *book = gncGetCurrentBook();
  gncUIWidget app = gnc_get_ui_data();

  /* If user wants to mess around before finishing business with
   * the old file, give em a chance to figure out what's up.  
   * Pose the question as a "while" loop, so that if user screws
   * up the file-selection dialog, we don't blow em out of the water;
   * instead, give them another chance to say "no" to the verify box.
   */
  while (gnc_book_not_saved(book)) {
    GNCVerifyResult result;
    const char *message = _("Changes have been made since the last "
                            "Save. Save the data to file?");

    result = gnc_verify_cancel_dialog_parented (app, message, GNC_VERIFY_YES);

    if (result == GNC_VERIFY_CANCEL)
      return FALSE;

    if (result == GNC_VERIFY_NO)
      return TRUE;

    gncFileSave ();
  }

  return TRUE;
}

/* ======================================================== */
/* private utilities for file open; done in two stages */

static void
gncPostFileOpen (const char * filename)
{
  GNCBook *new_book;
  gboolean uh_oh = FALSE;
  AccountGroup *new_group;
  char * newfile;
  GNCBackendError io_err = ERR_BACKEND_NO_ERR;

  if (!filename) return;

  newfile = xaccResolveURL (filename); 
  if (!newfile)
  {
     show_book_error (ERR_FILEIO_FILE_NOT_FOUND, filename);
     return;
  }

  /* disable events while moving over to the new set of accounts; 
   * the mass deletetion of accounts and transactions during
   * switchover would otherwise cause excessive redraws. */
  gnc_engine_suspend_events ();

  /* Change the mouse to a busy cursor */
  gnc_set_busy_cursor (NULL);

  /* -------------- BEGIN CORE SESSION CODE ------------- */
  /* -- this code is almost identical in FileOpen and FileSaveAs -- */
  gnc_book_destroy (current_book);
  current_book = NULL;

  /* load the accounts from the users datafile */
  /* but first, check to make sure we've got a book going. */
  new_book = gnc_book_new ();

  new_group = NULL;

  gnc_book_begin (new_book, newfile, FALSE, FALSE);
  io_err = gnc_book_get_error (new_book);

  /* if file appears to be locked, ask the user ... */
  if (ERR_BACKEND_LOCKED == io_err)
  {
     if (gncLockFailHandler (newfile))
     {
        /* user told us to ignore locks. So ignore them. */
        gnc_book_begin (new_book, newfile, TRUE, FALSE);
     }
  }

  /* if the database doesn't exist, ask the user ... */
  else if (ERR_BACKEND_NO_SUCH_DB == io_err)
  {
     if (gncCreateFailHandler (newfile))
     {
        /* user told us to create a new database. Do it. */
        gnc_book_begin (new_book, newfile, FALSE, TRUE);
     }
  }

  /* Check for errors again, since above may have cleared the lock */
  /* If its still locked, don't bother with the message */
  io_err = gnc_book_get_error (new_book);
  if (ERR_BACKEND_LOCKED != io_err)
  {
    uh_oh = show_book_error (io_err, newfile);
  }

  if (!uh_oh)
  {
    gnc_book_load (new_book);

    /* check for i/o error, put up appropriate error dialog */
    io_err = gnc_book_get_error (new_book);
    uh_oh = show_book_error (io_err, newfile);

    new_group = gnc_book_get_group (new_book);
    if (uh_oh) new_group = NULL;

    /* Umm, came up empty-handed, but no error: 
     * The backend forgot to set an error. So make one up. */
    if (!uh_oh && !new_group) 
    {
      uh_oh = show_book_error (ERR_BACKEND_MISC, newfile);
    }
  }

  gnc_unset_busy_cursor (NULL);

  /* going down -- abandon ship */
  if (uh_oh) 
  {
    gnc_book_destroy (new_book);

    /* well, no matter what, I think it's a good idea to have a
     * topgroup around.  For example, early in the gnucash startup
     * sequence, the user opens a file; if this open fails for any
     * reason, we don't want to leave them high & dry without a
     * topgroup, because if the user continues, then bad things will
     * happen. */
    gncGetCurrentBook ();

    g_free (newfile);

    gnc_engine_resume_events ();
    gnc_gui_refresh_all ();

    return;
  }

  /* if we got to here, then we've successfully gotten a new session */
  /* close up the old file session (if any) */
  current_book = new_book;

  /* --------------- END CORE SESSION CODE -------------- */

  /* clean up old stuff, and then we're outta here. */
  gncAddHistory (new_book);

  /* run a file-opened hook. For now, the main thing it will do 
   * is notice if legacy currencies are being imported. */
  if (newfile)
  {
      gh_call2(gh_eval_str("gnc:hook-run-danglers"),
               gh_eval_str("gnc:*file-opened-hook*"),
               gh_str02scm(newfile)); 
  }
  g_free (newfile);

  gnc_engine_resume_events ();
  gnc_gui_refresh_all ();
}

/* ======================================================== */

void
gncFileOpen (void)
{
  const char * newfile;

  if (!gncFileQuerySave ())
    return;

  newfile = fileBox(_("Open"), NULL, gnc_history_get_last());
  gncPostFileOpen (newfile);

  /* This dialogue can show up early in the startup process. If the
   * user fails to pick a file (by e.g. hitting the cancel button), we
   * might be left with a null topgroup, which leads to nastiness when
   * user goes to create their very first account. So create one. */
  gncGetCurrentBook ();
}

void
gncFileOpenFile (const char * newfile)
{
  if (!newfile) return;

  if (!gncFileQuerySave ())
    return;

  gncPostFileOpen (newfile);
}

/* ======================================================== */
static gboolean been_here_before = FALSE;

void
gncFileSave (void)
{
  GNCBackendError io_err;
  const char * newfile;
  GNCBook *book;
  ENTER (" ");

  /* hack alert -- Somehow make sure all in-progress edits get committed! */

  /* If we don't have a filename/path to save to get one. */
  book = gncGetCurrentBook ();

  if (!gnc_book_get_file_path (book))
  {
    gncFileSaveAs();
    return;
  }

  /* use the current session to save to file */
  gnc_set_busy_cursor(NULL);
  gnc_book_save (book);
  gnc_unset_busy_cursor(NULL);

  /* Make sure everything's OK - disk could be full, file could have
     become read-only etc. */
  newfile = gnc_book_get_file_path (book);
  io_err = gnc_book_get_error (book);
  if (ERR_BACKEND_NO_ERR != io_err)
  {
    show_book_error (io_err, newfile);

    if (been_here_before) return;
    been_here_before = TRUE;
    gncFileSaveAs();   /* been_here prevents infinite recursion */
    been_here_before = FALSE;
    return;
  }

  gncAddHistory (book);

  gnc_book_mark_saved(book);
  LEAVE (" ");
}

/* ======================================================== */

void
gncFileSaveAs (void)
{
  AccountGroup *group;
  GNCPriceDB *pdb;
  GNCBook *new_book;
  GNCBook *book;
  const char *filename;
  char *newfile;
  const char *oldfile;
  GNCBackendError io_err = ERR_BACKEND_NO_ERR;

  ENTER(" ");
  filename = fileBox(_("Save"), "*.gnc", gnc_history_get_last());
  if (!filename) return;

  /* Check to see if the user specified the same file as the current
   * file. If so, then just do that, instead of the below, which
   * assumes a truly new name was given. */
  newfile = xaccResolveURL (filename);
  if (!newfile)
  {
     show_book_error (ERR_FILEIO_FILE_NOT_FOUND, filename);
     return;
  }

  book = gncGetCurrentBook ();
  oldfile = gnc_book_get_file_path (book);
  if (oldfile && (strcmp(oldfile, newfile) == 0))
  {
    g_free (newfile);
    gncFileSave ();
    return;
  }

  /* -- this session code is NOT identical in FileOpen and FileSaveAs -- */
  group = gnc_book_get_group(book);
  pdb = gnc_book_get_pricedb(book);

  new_book = gnc_book_new ();
  gnc_book_begin (new_book, newfile, FALSE, FALSE);

  io_err = gnc_book_get_error (new_book);

  /* if file appears to be locked, ask the user ... */
  if (ERR_BACKEND_LOCKED == io_err) 
  {
    if (gncLockFailHandler (newfile))
    {
       /* user told us to ignore locks. So ignore them. */
       gnc_book_begin (new_book, newfile, TRUE, FALSE);
    }
  }

  /* if the database doesn't exist, ask the user ... */
  else if (ERR_BACKEND_NO_SUCH_DB == io_err)
  {
     if (gncCreateFailHandler (newfile))
     {
        /* user told us to create a new database. Do it. */
        gnc_book_begin (new_book, newfile, FALSE, TRUE);
     }
  }

  /* check again for session errors (since above dialog may have 
   * cleared a file lock & moved things forward some more) 
   * This time, errors will be fatal.
   */
  io_err = gnc_book_get_error (new_book);
  if (ERR_BACKEND_NO_ERR != io_err) 
  {
    show_book_error (io_err, newfile);
    gnc_book_destroy (new_book);
    g_free (newfile);
    return;
  }

  /* if we got to here, then we've successfully gotten a new session */
  /* close up the old file session (if any) */
  gnc_book_set_group(book, NULL);
  gnc_book_set_pricedb(book, NULL);
  gnc_book_destroy (book);
  current_book = new_book;

  /* --------------- END CORE SESSION CODE -------------- */

  /* oops ... file already exists ... ask user what to do... */
  if (gnc_book_save_may_clobber_data (new_book))
  {
    const char *format = _("The file \n    %s\n already exists.\n"
                           "Are you sure you want to overwrite it?");
    char *tmpmsg;
    gboolean result;

    tmpmsg = g_strdup_printf (format, newfile);
    result = gnc_verify_dialog (tmpmsg, FALSE);
    g_free (tmpmsg);

    /* if user says cancel, we should break out */
    if (!result)
    {
      g_free (newfile);
      return;
    }

    /* Whoa-ok. Blow away the previous file. */
  }

  /* OK, save the data to the file ... */
  gnc_book_set_group(new_book, group);
  gnc_book_set_pricedb(new_book, pdb);
  gncFileSave ();

  g_free (newfile);
  LEAVE (" ");
}

/* ======================================================== */

void
gncFileQuit (void)
{
  GNCBook *book;

  book = gncGetCurrentBook ();

  /* disable events; otherwise the mass deletetion of accounts and
   * transactions during shutdown would cause massive redraws */
  gnc_engine_suspend_events ();

  gnc_book_destroy (book);
  current_book = NULL;

  gnc_engine_resume_events ();
  gnc_gui_refresh_all ();
}

/* ======================================================== */

AccountGroup *
gncGetCurrentGroup (void)
{
  AccountGroup *grp;
  GNCBook *book;

  book = gncGetCurrentBook ();

  grp = gnc_book_get_group (book);
  return grp;
}

/* ======================================================== */

GNCBook *
gncGetCurrentBook (void) 
{
  if (!current_book)
    current_book = gnc_book_new ();

  return current_book;
}

/********************* END OF FILE **********************************/
