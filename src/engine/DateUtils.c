/********************************************************************\
 * DateUtils.c -- Date Handling Utilities                           *
 * Copyright (C) 1998 Linas Vepstas                                 *
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

#include <stdio.h>
#include <string.h>
#include <time.h>

#include <glib.h>

#include "config.h"

#include "DateUtils.h"

#define BUFFSIZE 100


#include <time.h>
#include <stdio.h>
#include <stdlib.h>

/* ======================================================== */
char *
xaccDateUtilGetStamp (time_t thyme)
{
   struct tm *stm;
   char buf[BUFFSIZE];
   char * retval;

   stm = localtime (&thyme);

   sprintf (buf, "%04d%02d%02d%02d%02d%02d",
      (stm->tm_year + 1900),
      (stm->tm_mon +1),
      stm->tm_mday,
      stm->tm_hour,
      stm->tm_min,
      stm->tm_sec
   );
   
   retval = g_strdup (buf);
   return retval;
}

/* ======================================================== */

char *
xaccDateUtilGetStampNow (void)
{
   time_t now;
   time (&now);
   return xaccDateUtilGetStamp (now);
}

/************************ END OF ************************************\
\************************* FILE *************************************/
