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
// static short module = MOD_GUI;

/** GLOBALS *********************************************************/
static Session *current_session = NULL;
static AccountGroup *topgroup = NULL; /* the current top of the hierarchy */

/********************************************************************\
 * fileMenubarCB -- handles file menubar choices                    * 
\********************************************************************/

#define SHOW_IO_ERR_MSG(io_error) {				\
  switch (io_error) {						\
     case ERR_FILEIO_NO_ERROR:					\
        break;							\
     case ERR_FILEIO_FILE_NOT_FOUND:				\
        sprintf (buf, FILE_NOT_FOUND_MSG, newfile);		\
        gnc_error_dialog (buf);	                		\
        uh_oh = 1;						\
        break;							\
     case ERR_FILEIO_FILE_EMPTY:				\
        sprintf (buf, FILE_EMPTY_MSG, newfile);			\
        gnc_error_dialog (buf);         			\
        uh_oh = 1;						\
        break;							\
     case ERR_FILEIO_FILE_TOO_NEW:				\
        gnc_error_dialog ( FILE_TOO_NEW_MSG);	        	\
        uh_oh = 1;						\
        break;							\
     case ERR_FILEIO_FILE_TOO_OLD:				\
        if (!gnc_verify_dialog( FILE_TOO_OLD_MSG, TRUE )) {     \
           xaccFreeAccountGroup (newgrp);			\
           newgrp = NULL;					\
           uh_oh = 1;						\
        }							\
        break;							\
     case ERR_FILEIO_FILE_BAD_READ:				\
        if (!gnc_verify_dialog( FILE_BAD_READ_MSG, TRUE )) {	\
           xaccFreeAccountGroup (newgrp);			\
           newgrp = NULL;					\
           uh_oh = 1;						\
        }							\
        break;							\
     default:							\
        break;							\
  }								\
}
      

#define SHOW_LOCK_ERR_MSG(session) 				\
    {								\
    int norr = xaccSessionGetError (session);			\
    if (ETXTBSY == norr)					\
      {								\
        sprintf (buf, FMB_LOCKED_MSG, newfile);			\
        gnc_error_dialog (buf);				        \
        uh_oh = 1;						\
      }								\
    else 							\
    if (ERANGE == norr)						\
      {								\
        sprintf (buf, FILE_NOT_FOUND_MSG, newfile);		\
        gnc_error_dialog (buf);				        \
        uh_oh = 1;						\
      }								\
    else 							\
    if (norr)							\
      {								\
        sprintf (buf, FMB_INVALID_MSG, newfile);		\
        gnc_error_dialog (buf);				        \
        uh_oh = 1;						\
      }								\
    }								\

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

    result = gnc_verify_cancel_dialog_parented( app,
                                                FMB_SAVE_MSG,
                                                GNC_VERIFY_YES );

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
  Session *newsess;
  AccountGroup *oldgrp;
  int io_error, uh_oh=0;
  char buf[BUFSIZE];
  AccountGroup *newgrp;
  char * newfile;

  if (!filename) return;
  newfile = xaccResolveFilePath (filename); 
  if (!newfile) {
     sprintf (buf, FILE_NOT_FOUND_MSG, filename);
     gnc_error_dialog (buf);
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
  newgrp = xaccSessionBeginFile (newsess, newfile);
  xaccLogEnable();
  gnc_unset_busy_cursor(NULL);

  /* check for session errors, put up appropriate dialog */
  SHOW_LOCK_ERR_MSG (newsess);

  if (!uh_oh)
  {
    /* check for i/o error, put up appropriate error message */
    io_error = xaccGetFileIOError();
    SHOW_IO_ERR_MSG(io_error);

    /* Umm, came up empty-handed, i.e. the file was not found. */
    /* This is almost certainly not what the user wanted. */
    if (!uh_oh && !newgrp && !io_error) 
    {
      sprintf (buf, FILE_NOT_FOUND_MSG, newfile);	
      gnc_error_dialog ( buf);
      uh_oh = 1;
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
  gnc_ui_qif_import_dialog_make(NULL);
}

/* ======================================================== */
static int been_here_before = 0;

void
gncFileSave (void)
{
  AccountGroup *newgrp = NULL;
  char * newfile;
  char buf[BUFSIZE];
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
  SHOW_IO_ERR_MSG(io_error);

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
  char buf[BUFSIZE];
  int io_error, uh_oh = 0;

  filename = fileBox(SAVE_STR, "*.gnc");
  if (!filename) return;

  /* check to see if the user did something silly, 
   * like specifying the same file as the current file ... 
   * if so, then just do that, instead of the below,
   * which assumes a truly new name was given.
   */
  newfile = xaccResolveFilePath (filename);
  if (!newfile) {
     sprintf (buf, FILE_NOT_FOUND_MSG, filename);
     gnc_error_dialog (buf);
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
  newgrp = xaccSessionBeginFile (newsess, newfile);
  xaccLogEnable();

  /* check for session errors (e.g. file locked by another user) */
  SHOW_LOCK_ERR_MSG (newsess);

  if (!uh_oh)
  {
    /* check for i/o error, put up appropriate error message */
    io_error = xaccGetFileIOError();
    SHOW_IO_ERR_MSG(io_error);
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
    tmpmsg = alloca (strlen (FMB_EEXIST_MSG) + strlen (newfile));
    sprintf (tmpmsg, FMB_EEXIST_MSG, newfile);
    /* if user says cancel, we should break out */
    if (! gnc_verify_dialog (tmpmsg, FALSE)) return;

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
  //  xaccGroupWindowDestroy (grp);
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
