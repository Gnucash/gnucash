/********************************************************************\
 * File: setenv.c
 * Renamed from: core-utils.c
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

#include "config.h"

#include <glib.h>
#include <glib/gprintf.h>
#include <stdlib.h>
#include <string.h>
#include "setenv.h"

/* This setenv() papers over the brokenness of of systems that only
 * have putenv() which takes ownership of the pointer you give it,
 * making it *very* difficult, if not impossible to avoid memory
 * leaks.  Note that right now, on systems that have setenv, this is
 * just setenv, and on other systems, we just leave the memory leak.
 * Later, we may try to make things a little better by keeping track
 * of the pointers we call putenv on in a hash table and if someone
 * calls gnc_setenv on an envt var that we've previously set, then
 * we'll free it after the change.  However, given the sloppy
 * semantics (or docs) for putenv, it's not even clear that this is
 * OK, since it's not clear that people aren't allowed to keep the
 * pointer from getenv around, as long as they don't try to modify
 * it... <shrug> */

#ifndef HAVE_SETENV

int
setenv(const char *name, const char *value, int overwrite)
{
    const char *old_value = getenv(name);
    int result = 0;

    if ((name == NULL) || (value == NULL)) return -1;

    if (overwrite || (!old_value))
    {
        char *new_value = g_strdup_printf("%s=%s", name, value);
        if (putenv(new_value) != 0) result = -1;
        if (old_value)
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
unsetenv(const char *name)
{
    int result = 0;
    char *putenv_str;

    if (name == NULL) return -1;
    if (strchr(name, '=') != NULL) return -1;
    if (*name == '\0') return -1;

    putenv_str = g_strdup_printf("%s=", name);
    if (!putenv_str) return -1;

    result = putenv(putenv_str);
    g_free(putenv_str);
    return result;
}

#endif
