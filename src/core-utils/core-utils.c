/********************************************************************\
 * File: core-utils.c
 *
 * Copyright (C) 2001 Linux Developers Group
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
\********************************************************************/

#include <glib.h>

#include "core-utils.h"

#include <stdlib.h>
#include <string.h>

/********************************************************************\
 * see header for info.
\********************************************************************/

#ifndef HAVE_SETENV

int
gnc_setenv(const char *name, const char *value, int overwrite)
{
  const char *old_value = getenv(name);
  int result = 0;

  if ((name == NULL) || (value == NULL)) return -1;

  if(overwrite || (!old_value))
  {
    char *new_value = g_strdup_printf("%s=%s", name, value);
    if(putenv(new_value) != 0) result = -1;
    if(old_value)
    {
      /* for now, do nothing, but it would be nice if we could figure
         out a safe way to reclaim any memory that *we* allocated,
         taking in to account whether or not other code (in other
         system libs) is allowed to have cached a pointer into the
         value via getenv -- is that kosher?
         
         Also we have to *know* that we allocated the memory.
      */
    }
  }
  return result;
}

int
gnc_unsetenv(const char *name)
{
  int result = 0;
  char *putenv_str;
  
  if(name == NULL) return -1;
  if(strchr(name, '=') != NULL) return -1;
  if(*name == '\0') return -1;
  
  putenv_str = g_strdup_printf("%s=", name);
  if(!putenv_str) return -1;

  result = putenv(putenv_str);
  g_free(putenv_str);
  return result;
}

#endif
