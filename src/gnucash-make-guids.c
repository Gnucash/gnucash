/********************************************************************\
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

#include "config.h"

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>

#include "GNCIdP.h"

static void
usage (void)
{
  fprintf (stderr, "Usage: gnucash-make-guids [number-to-make (default is 100)]\n");
  exit (1);
}

int
main (int argc, char **argv)
{
  int num_to_make = 100;

  if (argc > 2)
    usage ();

  if (argc == 2)
  {
    num_to_make = atoi (argv[1]);
    if (num_to_make == 0)
      usage ();
  }

  while (num_to_make-- > 0)
  {
    GUID guid;
    char s[GUID_ENCODING_LENGTH + 1];

    xaccGUIDNew (&guid);

    guid_to_string_buff (&guid, s);
    printf ("%s\n", s);
  }

  exit (0);
  return 0;
}
