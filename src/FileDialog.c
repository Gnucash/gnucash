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

#include "FileBox.h"
#include "FileDialog.h"
#include "FileIO.h"
#include "Group.h"
#include "TransLog.h"
#include "file-history.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "gnc-event.h"
#include "gnc-ui.h"
#include "messages.h"


/** GLOBALS *********************************************************/
/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static GNCBook *current_book = NULL;


/* ======================================================== */

static const char *
file_not_found_msg (void)
{
  return _("The file \n    %s\n could not be found.");
}

/* ======================================================== */

static gboolean
show_file_error (GNCFileIOError io_error, const char *newfile)
{
  gboolean uh_oh = FALSE;
  char *buf = NULL;

  switch (io_error)
  {
    case ERR_FILEIO_NONE:
      break;
    case ERR_FILEIO_MISC:
      buf = _("There was an error during file IO.");
      gnc_error_dialog (buf);
      uh_oh = TRUE;
      break;
    case ERR_FILEIO_FILE_NOT_FOUND:
      buf = g_strdup_printf (file_not_found_msg(), newfile);
      gnc_error_dialog (buf);
      g_free (buf);
      uh_oh = TRUE;
      break;
    case ERR_FILEIO_FILE_EMPTY:
      buf = _("The file \n    %s\n is empty.");
      buf = g_strdup_printf (buf, newfile);
      gnc_error_dialog (buf);
      g_free (buf);
      uh_oh = TRUE;
      break;
    case ERR_FILEIO_FILE_TOO_NEW:
      buf = _("This file appears to be from a newer version "
              "of GnuCash. You must upgrade GnuCash to read "
              "this file.");
      gnc_error_dialog (buf);
      uh_oh = TRUE;
      break;
    case ERR_FILEIO_FILE_TOO_OLD:
      buf = _("This file is from an older version of "
              "GnuCash.\nDo you want to continue?");
      if (!gnc_verify_dialog (buf, TRUE))
        uh_oh = TRUE;
      break;
    case ERR_FILEIO_FILE_BAD_READ:
      buf = _("There was an error reading the file.\n"
              "Do you want to continue?");
      if (!gnc_verify_dialog (buf, TRUE))
        uh_oh = TRUE;
      break;
    default:
      PERR("FIXME: Unhandled FileIO error in show_file_error.");
      uh_oh = TRUE;
      break;
  }

  return uh_oh;
}

/* ======================================================== */

static gboolean
show_book_error(GNCBook *book, const char *newfile, int norr)
{
  gboolean uh_oh = FALSE;
  char *buf = NULL;

  if (ETXTBSY == norr)
  {
    uh_oh = TRUE;
  }
  else if (ERANGE == norr)
  {
    buf = g_strdup_printf (file_not_found_msg(), newfile);
    gnc_error_dialog (buf);
    uh_oh = TRUE;
  }
  else if (norr)
  {
    const char *format = _("The filepath \n    %s\n"
                           "is not a valid location in the filesystem.");
    buf = g_strdup_printf (format, newfile);
    gnc_error_dialog (buf);
    uh_oh = TRUE;
  }

  g_free(buf);
  return uh_oh;
}

/* ======================================================== */

void
gncFileNew (void)
{
  GNCBook *book;
  AccountGroup *group;

  /* If user attempts to start a new session before saving results of
   * the last one, prompt them to clean up their act. */
  if (!gncFileQuerySave ())
    return;

  book = gncGetCurrentBook ();
  group = gnc_book_get_group (book);

  /* close any ongoing file sessions, and free the accounts.
   * disable logging and events so we don't get all that junk. */
  gnc_engine_suspend_events ();

  xaccLogDisable();
  gnc_book_destroy (book);
  current_book = NULL;
  xaccLogEnable();

  /* start a new book */
  gncGetCurrentBook ();

  gnc_engine_resume_events ();
  gnc_gui_refresh_all ();
}

/* ======================================================== */

gboolean
gncFileQuerySave (void)
{
  GNCBook *book;
  AccountGroup *group;
  gncUIWidget app;

  book = gncGetCurrentBook ();
  group = gnc_book_get_group (book);

  app = gnc_get_ui_data ();

  /* If user wants to mess around before finishing business with
   * the old file, give em a chance to figure out what's up.  
   * Pose the question as a "while" loop, so that if user screws
   * up the file-selection dialog, we don't blow em out of the water;
   * instead, give them another chance to say "no" to the verify box.
   */
  while (xaccGroupNotSaved (group))
  {
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

static gboolean
gncLockFailHandler (const char *file)
{
  const char *format = _("Gnucash could not obtain the lock for\n"
                         "   %s.\n"
                         "That file may be in use by another user,\n"
                         "in which case you should not open the file.\n"
                         "\nDo you want to proceed with opening the file?");
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
/* private utilities for file open; done in two stages */

static void
gncPostFileOpen (const char * filename)
{
  GNCBook *new_book;
  AccountGroup *old_group;
  gboolean uh_oh = FALSE;
  AccountGroup *new_group;
  char * newfile;
  int norr = 0;
  GNCFileIOError io_err = 0;

  if (!filename) return;

  newfile = xaccResolveURL (filename); 
  if (!newfile)
  {
     char *buf = g_strdup_printf (file_not_found_msg(), filename);
     gnc_error_dialog (buf);
     g_free(buf);
     return;
  }

  /* -------------- BEGIN CORE SESSION CODE ------------- */
  /* -- this code is almost identical in FileOpen and FileSaveAs -- */
  old_group = gnc_book_get_group (gncGetCurrentBook ());

  /* load the accounts from the users datafile */
  /* but first, check to make sure we've got a book going. */
  new_book = gnc_book_new ();

  /* disable logging and events while moving over to the new set of
   * accounts; the mass deletetion of accounts and transactions during
   * switchover is not something we want to keep in a journal. */
  gnc_engine_suspend_events ();

  gnc_set_busy_cursor (NULL);
  xaccLogDisable ();
  new_group = NULL;

  /* hack alert -- there has got to be a simpler way of dealing with 
   * errors than this spaghetti code!  I beleive that this would simplify
   * a whole lot if all functions returned void, and one *always* used
   * try-throw semantics, instead of this hodge-podge of testing 
   * return values.  -- linas jan 2001
   */
  gnc_book_begin (new_book, newfile, FALSE);
  norr = gnc_book_get_error (new_book);

  /* if file appears to be locked, ask the user ... */
  if (EBUSY == norr) 
  {
     norr = 0;
     if (gncLockFailHandler (newfile))
     {
        /* user told us to ignore locks. So ignore them. */
        gnc_book_begin (new_book, newfile, TRUE);
     }
  }

  /* for any other error, put up appropriate dialog */
  uh_oh = show_book_error (new_book, newfile, norr);

  if (!uh_oh)
  {
    if (gnc_book_load (new_book)) 
    {
       new_group = gnc_book_get_group (new_book);
    }

    /* for any other error, put up appropriate dialog */
    norr = gnc_book_get_error (new_book);
    uh_oh = show_book_error (new_book, newfile, norr);

    io_err = gnc_book_get_file_error (new_book);

    /* check for i/o error, put up appropriate error message */
    uh_oh += show_file_error(io_err, newfile);
    if (uh_oh) new_group = NULL;

    /* Umm, came up empty-handed, i.e. the file was not found. */
    /* This is almost certainly not what the user wanted. */
    if (!uh_oh && !new_group && (io_err == ERR_FILEIO_NONE)) 
    {
      char *buf = g_strdup_printf (file_not_found_msg(), newfile);
      gnc_error_dialog (buf);
      g_free (buf);
      uh_oh = TRUE;
    }
  }
  xaccLogEnable ();
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
  xaccLogDisable();
  xaccLogSetBaseName (newfile);

  gnc_book_destroy (current_book);
  current_book = new_book;

  xaccLogEnable();
  gnc_engine_resume_events ();
  gnc_gui_refresh_all ();

  /* --------------- END CORE SESSION CODE -------------- */

  /* clean up old stuff, and then we're outta here. */
  gnc_history_add_file (newfile);
  g_free (newfile);

  /* run a file-opened hook. For now, the main thing it will do 
   * is notice if legacy currencies are being imported. */
  {
    SCM run_danglers = gh_eval_str("gnc:hook-run-danglers");
    SCM hook = gh_eval_str("gnc:*file-opened-hook*");
    SCM filename;

    if (newfile)
    {
      filename = gh_str02scm(newfile);
      gh_call2(run_danglers, hook, filename); 
    }
  }  
}

/* ======================================================== */

void
gncFileOpen (void)
{
  const char * newfile;

  if (!gncFileQuerySave ())
    return;

  newfile = fileBox(_("Open"), NULL, NULL);
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
  GNCFileIOError io_err;
  const char * newfile;
  GNCBook *book;
  int uh_oh = 0;
  int norr;

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
  norr = gnc_book_get_error (book);
  if (norr)
  {
    const char *format = _("There was an error writing the file\n     %s"
                           "\n\n%s");
    char *message;

    newfile = gnc_book_get_file_path (book);
    if (newfile == NULL)
      newfile = "";

    message = g_strdup_printf(format, newfile, g_strerror(norr));
    gnc_error_dialog(message);
    g_free(message);

    if (been_here_before) return;

    been_here_before = TRUE;
    gncFileSaveAs();   /* been_here prevents infinite recuirsion */
    been_here_before = FALSE;

    return;
  }

  /* check for i/o error, put up appropriate error message.
   * NOTE: the file-writing routines never set the file io
   * error code, so this seems to be unneccesary. */
  io_err = gnc_book_get_file_error (book);
  newfile = gnc_book_get_file_path (book);
  gnc_history_add_file (newfile);

  uh_oh = show_file_error (io_err, newfile);

  /* going down -- abandon ship */
  if (uh_oh) return;

  xaccGroupMarkSaved (gnc_book_get_group (book));
}

/* ======================================================== */

void
gncFileSaveAs (void)
{
  AccountGroup *group;
  GNCBook *new_book;
  GNCBook *book;
  const char *filename;
  char *newfile;
  const char *oldfile;
  gboolean uh_oh = FALSE;
  int norr = 0;

  filename = fileBox(_("Save"), "*.gnc", NULL);
  if (!filename) return;

  /* Check to see if the user specified the same file as the current
   * file. If so, then just do that, instead of the below, which
   * assumes a truly new name was given. */
  newfile = xaccResolveURL (filename);
  if (!newfile)
  {
     char *buf = g_strdup_printf (file_not_found_msg(), filename);
     gnc_error_dialog (buf);
     g_free (buf);
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
  group = gnc_book_get_group (book);

  /* disable logging while we move over to the new set of accounts to
   * edit; the mass deletetion of accounts and transactions during
   * switchover is not something we want to keep in a journal. */
  xaccLogDisable ();
  new_book = gnc_book_new ();
  gnc_book_begin (new_book, newfile, FALSE);

  norr = gnc_book_get_error (new_book);
  /* if file appears to be locked, ask the user ... */
  if (EBUSY == norr) 
  {
    norr = 0;
    if (gncLockFailHandler (newfile))
    {
       /* user told us to ignore locks. So ignore them. */
       gnc_book_begin (new_book, newfile, TRUE);
    }
  }
  xaccLogEnable ();

  /* check for session errors (e.g. file locked by another user) */
  if (!norr) norr = gnc_book_get_error (new_book);
  uh_oh = show_book_error (new_book, newfile, norr);

  /* No check for file errors since we didn't read a file... */

  /* going down -- abandon ship */
  if (uh_oh)
  {
    xaccLogDisable ();
    gnc_book_destroy (new_book);
    xaccLogEnable ();

    g_free (newfile);

    return;
  }

  /* if we got to here, then we've successfully gotten a new session */
  /* close up the old file session (if any) */
  xaccLogDisable ();
  gnc_book_set_group (book, NULL);
  gnc_book_destroy (book);
  current_book = new_book;
  xaccLogEnable ();

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

    /* Whoa-ok. Blow away the previous file. Do not disable
     * logging. We want to capture the old file in the log, just in
     * case the user later decides it was all a big mistake. */
  }

  /* OK, save the data to the file ... */
  xaccLogSetBaseName (newfile);
  gnc_book_set_group (new_book, group);
  gncFileSave ();

  g_free (newfile);
}

/* ======================================================== */

void
gncFileQuit (void)
{
  GNCBook *book;

  book = gncGetCurrentBook ();

  /* disable logging and events; the mass deletetion of accounts and
   * transactions during shutdown is not something we want to keep in
   * a journal. */
  gnc_engine_suspend_events ();

  xaccLogDisable();

  gnc_book_destroy (book);
  current_book = NULL;

  gnc_engine_resume_events ();
  gnc_gui_refresh_all ();
}

/* ======================================================== */

AccountGroup *
gncGetCurrentGroup (void)
{
  GNCBook *book;

  book = gncGetCurrentBook ();

  return gnc_book_get_group (book);
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
