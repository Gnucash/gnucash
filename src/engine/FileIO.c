/********************************************************************\
 * FileIO.c -- read and write file wrappers (old and new format)    *
 * Copyright (C) 1997-2000 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 1999-2000 Rob Browning                             *
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
 *                                                                  *
\********************************************************************/


#include <stdlib.h>
#include <string.h>

#include "DateUtils.h"
#include "FileIO.h"
#include "io-gncxml.h"
#include "io-gncbin.h"

AccountGroup *
xaccReadAccountGroupFile(const gchar *name, GNCFileIOError *error_result) 
{
  AccountGroup *result_grp;
  
  if(is_gncxml_file(name)) {
    if(gncxml_read(name, &result_grp)) {
      if(error_result) *error_result = ERR_FILEIO_NONE;
      return result_grp;
    } else {
      if(error_result) *error_result = ERR_FILEIO_MISC;
      return NULL;
    }
  } else {
    /* presume it's an old-style binary file */
    result_grp = xaccReadGncBinAccountGroupFile(name);
    
    if(result_grp) {
      if(error_result) *error_result = ERR_FILEIO_NONE;
      return result_grp;
    } else {
      if(error_result) *error_result = xaccGetGncBinFileIOError();
      return NULL;
    }
  }
  /* Should never get here */
  if(error_result) *error_result = ERR_FILEIO_MISC;
  return NULL;
}

gboolean
xaccWriteAccountGroupFile(const char *datafile,
                          AccountGroup *grp,
                          gboolean make_backup,
                          GNCFileIOError *error_result) 
{

  if(!datafile) {
    if(error_result) *error_result = ERR_FILEIO_MISC;
    return FALSE;
  }

  if(!gncxml_write(grp, datafile)) {
    if(error_result) *error_result = ERR_FILEIO_MISC;
    return FALSE;
  }

  if(!make_backup) {
    if(error_result) *error_result = ERR_FILEIO_NONE;
    return TRUE;
  } else {
    char * timestamp;
    int filenamelen;
    char * backup;
    
    /* also, write a time-stamped backup file */
    /* tag each filename with a timestamp */
    timestamp = xaccDateUtilGetStampNow ();
    
    filenamelen = strlen (datafile) + strlen (timestamp) + 6;
    
    backup = (char *) malloc (filenamelen);
    strcpy (backup, datafile);
    strcat (backup, ".");
    strcat (backup, timestamp);
    strcat (backup, ".xac");
    free (timestamp);
    
    if(gncxml_write(grp, backup)) {
      if(error_result) *error_result = ERR_FILEIO_NONE;
      free (backup);
      return TRUE;
    } else {
      if(error_result) *error_result = ERR_FILEIO_MISC;
      free (backup);
      return FALSE;
    }
  }
  /* Should never get here */
  if(error_result) *error_result = ERR_FILEIO_MISC;
  return FALSE;
}
