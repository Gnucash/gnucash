/* Copyright (C) 1996,1997,2000,2001, 2006, 2008 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/* This is the 'main' function for the `guile' executable.  It is not
   included in libguile.a.

   Eventually, we hope this file will be automatically generated,
   based on the list of installed, statically linked libraries on the
   system.  For now, please don't put interesting code in here.  */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#ifdef __MINGW32__
# define SCM_IMPORT 1
#endif
#include <libguile.h>

#ifdef HAVE_CONFIG_H
#include <libguile/scmconfig.h>
#endif
#include <ltdl.h>

#ifdef HAVE_WINSOCK2_H
#include <winsock2.h>
#endif

static void
inner_main (void *closure SCM_UNUSED, int argc, char **argv)
{
#ifdef __MINGW32__
  /* This is necessary to startup the Winsock API under Win32. */
  WSADATA WSAData;
  WSAStartup (0x0202, &WSAData);
#endif /* __MINGW32__ */

  /* module initializations would go here */
  scm_shell (argc, argv);

#ifdef __MINGW32__
  WSACleanup ();
#endif /* __MINGW32__ */
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0; /* never reached */
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
