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

#include <string.h>
#include <glib.h>

#include "top-level.h"

#include "FileBox.h"
#include "FileDialog.h"
#include "FileIO.h"
#include "Group.h"
#include "messages.h"
#include "Session.h"
#include "TransLog.h"
#include "Destroy.h"
#include "util.h"
#include "ui-callbacks.h"
#include "file-history.h"

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

/** GLOBALS *********************************************************/
static Session *current_session = NULL;
static AccountGroup *topgroup = NULL; /* the current top of the hierarchy */

/* ======================================================== */

static const char *
file_not_found_msg (void)
{
  return _("The file \n    %s\n could not be found.");
}

/* ======================================================== */

static gboolean
show_file_error (int io_error, char *newfile)
{
  gboolean uh_oh = FALSE;
  char *buf = NULL;

  switch (io_error)
  {
    case ERR_FILEIO_NO_ERROR:
      break;
    case ERR_FILEIO_FILE_NOT_FOUND:
      buf = g_strdup_printf (file_not_found_msg(), newfile);
      gnc_error_dialog (buf);
      uh_oh = TRUE;
      break;
    case ERR_FILEIO_FILE_EMPTY:
      buf = _("The file \n    %s\n is empty.");
      buf = g_strdup_printf (buf, newfile);
      gnc_error_dialog (buf);
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
      break;
  }

  g_free (buf);

  return uh_oh;
}

/* ======================================================== */

static gboolean
show_session_error(Session *session, char *newfile)
{
  int norr = xaccSessionGetError (session);
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
    buf = (FMB_INVALID_MSG, newfile);
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
  Session *sess;
  AccountGroup *grp;

  /* If user attempts to start a new session before saving
   * results of the last one, prompt them to clean up their 
   * act. */
  if (!gncFileQuerySave ())
    return;

  sess = current_session;
  grp = xaccSessionGetGroup (sess);
  /* if session not yet started ... */
  if (!grp) grp = topgroup;

  /* when starting new everything, destroy old stuff first */
  /* destroy open windows first, before destroying the group itself */
  xaccGroupWindowDestroy (grp);

  /* close any ongoing file sessions, if any */
  xaccSessionEnd (sess);
  xaccSessionDestroy (sess);
  current_session = NULL;
  topgroup = NULL;

  /* disable logging while we move over to the new set of accounts to
   * edit; the mass deletion of accounts and transactions during
   * switchover is not something we want to keep in a journal.  */
  xaccLogDisable();
  xaccFreeAccountGroup (grp);
  xaccLogEnable();
  grp = xaccMallocAccountGroup();
  topgroup = grp;
}

/* ======================================================== */

gboolean
gncFileQuerySave (void)
{
  Session *sess;
  AccountGroup *grp;
  gncUIWidget app;

  sess = current_session;
  grp = xaccSessionGetGroup (sess);
  /* if session not yet started ... */
  if (!grp) grp = topgroup;

  app = gnc_get_ui_data();

  /* If user wants to mess around before finishing business with
   * the old file, give em a chance to figure out what's up.  
   * Pose the question as a "while" loop, so that if user screws
   * up the file-selection dialog, we don't blow em out of the water;
   * instead, give them another chance to say "no" to the verify box.
   */
  while ( xaccGroupNotSaved (grp) ) 
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
  Session *newsess;
  AccountGroup *oldgrp;
  gboolean uh_oh = FALSE;
  int io_error;
  AccountGroup *newgrp;
  char * newfile;

  if (!filename) return;
  newfile = xaccResolveFilePath (filename); 
  if (!newfile) {
     char *buf = g_strdup_printf (file_not_found_msg(), filename);
     gnc_error_dialog (buf);
     g_free(buf);
     return;
  }

  /* -------------- BEGIN CORE SESSION CODE ------------- */
  /* -- this code is almost identical in FileOpen and FileSaveAs -- */
  oldgrp = xaccSessionGetGroup (current_session);
  /* if session not yet started ... */
  if (!oldgrp) oldgrp = topgroup;

  /* load the accounts from the users datafile */
  /* but first, check to make sure we've got a session going ... */
  newsess = xaccMallocSession ();

  /* disable logging while we move over to the new set of accounts to
   * edit; the mass deletetion of accounts and transactions during
   * switchover is not something we want to keep in a journal.  */
  gnc_set_busy_cursor(NULL);
  xaccLogDisable();
  newgrp = xaccSessionBeginFile (newsess, newfile, gncLockFailHandler);
  xaccLogEnable();
  gnc_unset_busy_cursor(NULL);

  /* check for session errors, put up appropriate dialog */
  uh_oh = show_session_error (newsess, newfile);

  if (!uh_oh)
  {
    /* check for i/o error, put up appropriate error message */
    io_error = xaccGetFileIOError();
    uh_oh = show_file_error (io_error, newfile);
    if (uh_oh)
    {
      xaccFreeAccountGroup (newgrp);
      newgrp = NULL;
    }

    /* Umm, came up empty-handed, i.e. the file was not found. */
    /* This is almost certainly not what the user wanted. */
    if (!uh_oh && !newgrp && !io_error) 
    {
      char *buf = g_strdup_printf (file_not_found_msg(), newfile);	
      gnc_error_dialog (buf);
      g_free (buf);
      uh_oh = TRUE;
    }
  }

  /* going down -- abandon ship */
  if (uh_oh) 
  {
    xaccSessionEnd (newsess);
    xaccSessionDestroy (newsess);

    /* well, no matter what, I think its a good idea to have 
     * a topgroup around.  For example, early in the gnucash startup
     * sequence, the user opens a file; if this open fails for any 
     * reason, we don't want to leave them high & dry without a topgroup,
     * because if the user continues, then bad things will happen.
     */
    if (NULL == topgroup) 
    {
      topgroup = xaccMallocAccountGroup();
    }
    free (newfile);
    return;
  }

  /* if we got to here, then we've successfully gotten a new session */
  /* close up the old file session (if any) */
  xaccSessionEnd (current_session);
  xaccSessionDestroy (current_session);
  current_session = newsess;
  /* --------------- END CORE SESSION CODE -------------- */

  /* clean up old stuff, and then we're outta here. */
  xaccLogDisable();
  xaccLogSetBaseName (newfile);
  gnc_history_add_file (newfile);
  /* destroy open windows first, before destroying the group itself */
  xaccGroupWindowDestroy (oldgrp);
  xaccFreeAccountGroup (oldgrp);
  topgroup = newgrp;
  xaccLogEnable();

  free (newfile);
}

/* ======================================================== */

void
gncFileOpen (void)
{
  const char * newfile;

  if (!gncFileQuerySave ())
    return;

  newfile = fileBox( OPEN_STR, "*.gnc");
  gncPostFileOpen (newfile);

  /* This dialogue can show up early in the startup process.
   * If the user fails to pick a file (by e.g. hitting the cancel
   * button), we might be left with a null topgroup, which leads
   * to nastiness when user goes to create their very first account.
   * Don't leave their ass in a sling, give them what they need.
   */
  if (NULL == topgroup) 
  {
    topgroup = xaccMallocAccountGroup();
  }
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

void
gncFileQIFImport (void)
{
  /* pop up the QIF File Import dialog box */
  gnc_ui_qif_import_dialog_make();
}

/* ======================================================== */
static int been_here_before = 0;

void
gncFileSave (void)
{
  AccountGroup *newgrp = NULL;
  char * newfile;
  int io_error, norr, uh_oh = 0;

  /* hack alert -- Somehow make sure all in-progress edits get committed! */

  /* if no session exists, then we don't have a filename/path 
   * to save to. Get one now. */
  if ((NULL == current_session) || 
      (NULL == xaccSessionGetGroup (current_session)))
  {
    gncFileSaveAs();
    return;
  }

  /* use the current session to save to file */
  gnc_set_busy_cursor(NULL);
  xaccSessionSave (current_session);
  gnc_unset_busy_cursor(NULL);

  /* in theory, no error should have occured, but just in case, 
   * we're gonna check and handle ... */
  norr = xaccSessionGetError (current_session);
  if (norr)
  {
    char *message;

    newfile = xaccSessionGetFilePath(current_session);
    if (newfile == NULL)
      newfile = "";

    message = g_strdup_printf(FILE_EWRITE_MSG, newfile, g_strerror(norr));
    gnc_error_dialog(message);
    g_free(message);

    if (been_here_before) return;

    been_here_before = 1;
    gncFileSaveAs();   /* been_here prevents infinite recuirsion */
    been_here_before = 0;

    return;
  }

  /* check for i/o error, put up appropriate error message.
   * NOTE: the file-writing routines never set the file io
   * error code, so this seems to be unneccesary. */
  io_error = xaccGetFileIOError();
  newfile = xaccSessionGetFilePath(current_session);
  gnc_history_add_file(newfile);

  uh_oh = show_file_error (io_error, newfile);
  if (uh_oh)
  {
    xaccFreeAccountGroup (newgrp);
    newgrp = NULL;
  }

  /* going down -- abandon ship */
  if (uh_oh) return;

  xaccGroupMarkSaved (topgroup);
}

/* ======================================================== */

void
gncFileSaveAs (void)
{
  Session *newsess;
  AccountGroup *oldgrp;
  const char *filename;
  char *newfile;
  AccountGroup *newgrp;
  char * oldfile;
  int io_error;
  gboolean uh_oh = FALSE;

  filename = fileBox(SAVE_STR, "*.gnc");
  if (!filename) return;

  /* check to see if the user did something silly, 
   * like specifying the same file as the current file ... 
   * if so, then just do that, instead of the below,
   * which assumes a truly new name was given.
   */
  newfile = xaccResolveFilePath (filename);
  if (!newfile) {
     char *buf = g_strdup_printf (file_not_found_msg(), filename);
     gnc_error_dialog (buf);
     g_free (buf);
     return;
  }
  oldfile = xaccSessionGetFilePath (current_session);
  if (oldfile && !strcmp (oldfile, newfile)) 
  {
    free (newfile);
    gncFileSave ();
    return;
  }

  /* -------------- BEGIN CORE SESSION CODE ------------- */
  /* -- this code identical in FileOpen and FileSaveAs -- */
  oldgrp = xaccSessionGetGroup (current_session);
  /* if session not yet started ... */
  if (!oldgrp) oldgrp = topgroup;

  /* create a new session ... */
  newsess = xaccMallocSession ();

  /* disable logging while we move over to the new set of accounts to
   * edit; the mass deletetion of accounts and transactions during
   * switchover is not something we want to keep in a journal.  */
  xaccLogDisable();
  newgrp = xaccSessionBeginFile (newsess, newfile, gncLockFailHandler);
  xaccLogEnable();

  /* check for session errors (e.g. file locked by another user) */
  uh_oh = show_session_error (newsess, newfile);

  if (!uh_oh)
  {
    /* check for i/o error, put up appropriate error message */
    io_error = xaccGetFileIOError();
    uh_oh = show_file_error (io_error, newfile);
    if (uh_oh)
    {
      xaccFreeAccountGroup (newgrp);
      newgrp = NULL;
    }
  }

  /* going down -- abandon ship */
  if (uh_oh) 
  {
    xaccSessionEnd (newsess);
    xaccSessionDestroy (newsess);

    /* well, no matter what, I think its a good idea to have 
     * a topgroup around.  For example, early in the gnucash startup
     * sequence, the user opens a file ... if this open fails for any 
     * reason, we don't want to leave them high & dry without a topgroup,
     * because if user continues, then bad things will happen ...
     */
    if (NULL == topgroup) 
    {
      topgroup = xaccMallocAccountGroup();
    }
    free (newfile);

    gnc_refresh_main_window();

    return;
  }

  /* if we got to here, then we've successfully gotten a new session */
  /* close up the old file session (if any) */
  xaccSessionEnd (current_session);
  xaccSessionDestroy (current_session);
  current_session = newsess;
  /* --------------- END CORE SESSION CODE -------------- */

  /* oops ... file already exists ... ask user what to do... */
  if (newgrp) 
  {
    char *tmpmsg;
    gboolean result;

    tmpmsg = g_strdup_printf (FMB_EEXIST_MSG, newfile);
    result = gnc_verify_dialog (tmpmsg, FALSE);
    g_free (tmpmsg);

    /* if user says cancel, we should break out */
    if (!result)
      return;

    /* Whoa-ok. Blow away the previous file. 
     * Do not disable logging ... we want to capture the 
     * old file in the log, just in case the user later
     *  decides it was all a big mistake. */
    xaccSessionSetGroup (newsess, NULL);
    /* xaccLogDisable();  no don't disable, keep logging on */
    xaccFreeAccountGroup (newgrp);
  }

  /* OK, save the data to the file ... */
  xaccLogSetBaseName (newfile);
  xaccSessionSetGroup (newsess, oldgrp);
  gncFileSave ();
  free (newfile);

  gnc_refresh_main_window();
}

/* ======================================================== */

void
gncFileQuit (void)
{
  AccountGroup *grp;

  grp = xaccSessionGetGroup (current_session);
  /* if session not yet started ... */
  if (!grp) grp = topgroup;

  /* disable logging; the mass deletetion of accounts and transactions
   * during shutdown is not something we want to keep in a journal.  */
  xaccLogDisable();

  xaccSessionEnd (current_session);
  xaccSessionDestroy (current_session);
  current_session = NULL;
  xaccFreeAccountGroup (grp);
  topgroup = NULL;
}

/* ======================================================== */

AccountGroup *
gncGetCurrentGroup (void)
{
  AccountGroup *grp;
  grp = xaccSessionGetGroup (current_session); 
  if (grp) return grp;

  /* If we are here, then no session has yet been started ... */
  grp = topgroup;
  if (grp) return grp;

  /* if we are here, then topgroup not yet initialized ... */
  xaccLogEnable();
  topgroup = xaccMallocAccountGroup();

  return topgroup;
}

/* ======================================================== */

Session *
gncGetCurrentSession (void) 
{
  return (current_session);
}

/********************* END OF FILE **********************************/
