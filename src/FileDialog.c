/********************************************************************\
 * FileDialog.c -- file-handling utility dialogs for gnucash.       * 
 *                                                                  *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998, 1999 Linas Vepstas                           *
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

#include <errno.h>

#include "config.h"

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

/* This static indicates the debugging module that this .o belongs to.  */
// static short module = MOD_GUI;

/** GLOBALS *********************************************************/
Session *current_session = NULL;
AccountGroup *topgroup = NULL;    /* the current top of the heriarchy */

/********************************************************************\
 * fileMenubarCB -- handles file menubar choices                    * 
\********************************************************************/

#define SHOW_IO_ERR_MSG(io_error) {				\
  switch (io_error) {						\
     case ERR_FILEIO_NO_ERROR:					\
        break;							\
     case ERR_FILEIO_FILE_NOT_FOUND:				\
        sprintf (buf, FILE_NOT_FOUND_MSG, newfile);		\
        errorBox (buf);	                			\
        uh_oh = 1;						\
        break;							\
     case ERR_FILEIO_FILE_EMPTY:				\
        sprintf (buf, FILE_EMPTY_MSG, newfile);			\
        errorBox (buf);         				\
        uh_oh = 1;						\
        break;							\
     case ERR_FILEIO_FILE_TOO_NEW:				\
        errorBox ( FILE_TOO_NEW_MSG);	        		\
        uh_oh = 1;						\
        break;							\
     case ERR_FILEIO_FILE_TOO_OLD:				\
        if (!verifyBox( FILE_TOO_OLD_MSG )) {		        \
           xaccFreeAccountGroup (newgrp);			\
           newgrp = NULL;					\
           uh_oh = 1;						\
        }							\
        break;							\
     case ERR_FILEIO_FILE_BAD_READ:				\
        if (!verifyBox( FILE_BAD_READ_MSG )) {	\
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
        errorBox (buf);				\
        uh_oh = 1;						\
      }								\
    else 							\
    if (norr)							\
      {								\
        sprintf (buf, FMB_INVALID_MSG, newfile);		\
        errorBox (buf);				\
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
   * results of the last one, prompt them to clean up thier 
   * act. */
  gncFileQuerySave ();

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
   * edit; the mass deletetion of accounts and transactions during
   * switchover is not something we want to keep in a journal.  */
  xaccLogDisable();
  xaccFreeAccountGroup (grp);
  xaccLogEnable();
  grp = xaccMallocAccountGroup();
  topgroup = grp;
  }

/* ======================================================== */

void
gncFileQuerySave (void)
  {
  Session *sess;
  AccountGroup *grp;
  
  sess = current_session;
  grp = xaccSessionGetGroup (sess);
  /* if session not yet started ... */
  if (!grp) grp = topgroup;

  /* If user wants to mess around before finishing business with
   * the old file, give em a chance to figure out what's up.  
   * Pose the question as a "while" loop, so that if user screws
   * up the file-selection dialog, we don't blow em out of the water;
   * instead, give them another chance to say "no" to the verify box.
   */
  while ( xaccAccountGroupNotSaved (grp) ) 
    {
    if( verifyBox( FMB_SAVE_MSG) ) 
      {
      gncFileSave ();
      }
      else
      return;
    }
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
  if (!newfile) return;

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
  xaccLogDisable();
  newgrp = xaccSessionBeginFile (newsess, newfile);
  xaccLogEnable();

  /* check for session errors, put up appropriate dialog */
  SHOW_LOCK_ERR_MSG (newsess);

  /* check for i/o error, put up appropriate error message */
  io_error = xaccGetFileIOError();
  SHOW_IO_ERR_MSG(io_error);

  /* Umm, came up empty-handed, i.e. the file was not found. */
  /* This is almost certainly not what the user wanted. */
  if (!newgrp && !io_error) 
    {
    sprintf (buf, FILE_NOT_FOUND_MSG, newfile);	
    errorBox ( buf);
    uh_oh = 1;
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
  char * newfile;
  
  gncFileQuerySave ();
  newfile = fileBox( OPEN_STR, "*.xac");
  gncPostFileOpen (newfile);

  /* This dialogue can show up early in the startup process.
   * If the user fails to pick a file (by e.g. hitting the cancel
   * button), we might be left with a null topgroup, which leads
   * to nastiness when user goes to create thier very first account.
   * Don't leave thier ass in a sling, give them what they need.
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
  gncFileQuerySave ();
  gncPostFileOpen (newfile);
  }

/* ======================================================== */

void
gncFileQIFImport (void)
  {
  char * newfile;
  char buf[BUFSIZE];
  int io_error, uh_oh = 0;
  AccountGroup *newgrp;
  
  newfile = fileBox( OPEN_STR, "*.qif");
  if (!newfile) return;
  
  /* load the accounts from the file the user specified */
  newgrp = xaccReadQIFAccountGroup (newfile);

  /* check for i/o error, put up appropriate error message */
  io_error = xaccGetQIFIOError();
  SHOW_IO_ERR_MSG(io_error);

  if (uh_oh) return;

  if( NULL == topgroup ) {
    /* no topgroup exists */
    topgroup = xaccMallocAccountGroup();
  }

  /* since quicken will not export all accounts 
   * into one file, we must merge them in one by one */
  xaccConcatGroups (topgroup, newgrp);
  xaccMergeAccounts (topgroup);
  xaccConsolidateGrpTransactions (topgroup);
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
  xaccSessionSave (current_session);

  /* in theory, no error should have occured, but just in case, 
   * we're gonna check and handle ... */
  norr = xaccSessionGetError (current_session);
  if (norr) 
    {
    if (been_here_before) return;
    been_here_before = 1;
    gncFileSaveAs();   /* been_here prevents infinite recuirsion */
    been_here_before = 0;
    return;
    }

  /* check for i/o error, put up appropriate error message */
  io_error = xaccGetFileIOError();
  newfile = xaccSessionGetFilePath(current_session);
  SHOW_IO_ERR_MSG(io_error);

  /* going down -- abandon ship */
  if (uh_oh) return;

  xaccAccountGroupMarkSaved (topgroup);
  }

/* ======================================================== */

void
gncFileSaveAs (void)
  {
  Session *newsess;
  AccountGroup *oldgrp;
  char * newfile;
  AccountGroup *newgrp;
  char * oldfile;
  char buf[BUFSIZE];
  int io_error, uh_oh = 0;
  
  newfile = fileBox( SAVE_STR, "*.xac");
  if (!newfile) return;

  /* check to see if the user did something silly, 
   * like specifying the same file as the current file ... 
   * if so, then just do that, instead of the below,
   * which assumes a tuly new name was given.
   */
  newfile = xaccResolveFilePath (newfile);
  assert (newfile);  /* deep doodoo if resolve failed */
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

  /* check for i/o error, put up appropriate error message */
  io_error = xaccGetFileIOError();
  SHOW_IO_ERR_MSG(io_error);

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

  /* oops ... file already exists ... ask user what to do... */
  if (newgrp) 
    {
    char *tmpmsg;
    tmpmsg = alloca (strlen (FMB_EEXIST_MSG) + strlen (newfile));
    sprintf (tmpmsg, FMB_EEXIST_MSG, newfile);
    /* if user says cancel, we should break out */
    if (! verifyBox ( tmpmsg)) return;

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
  
/********************* END OF FILE **********************************/
