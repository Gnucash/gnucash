/********************************************************************\
 * File: core-utils.h
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

#ifndef GNC_CORE_UTILS_H
#define GNC_CORE_UTILS_H

#include "config.h"

/* gnc_setenv() papers over the brokenness of of systems that only
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
#ifdef HAVE_SETENV

#define gnc_setenv setenv
#define gnc_unsetenv unsetenv

#elif defined HAVE_PUTENV

int gnc_setenv(const char *name, const char *value, int overwrite);
int gnc_unsetenv(const char *name);

#else
#error "Must have setenv or putenv."
#endif

#endif
