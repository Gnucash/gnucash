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

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

/* needed for db.h with 'gcc -ansi -pedantic' */
#ifndef _BSD_SOURCE
#  define _BSD_SOURCE 1
#endif

#ifdef PREFER_DB1
#ifdef HAVE_DB1_DB_H
# include <db1/db.h>
#else
# ifdef HAVE_DB_185_H
#  include <db_185.h>
# else
#  include <db.h>
# endif
#endif
#else
#ifdef HAVE_DB_185_H
# include <db_185.h>
#else
# ifdef HAVE_DB_H
#  include <db.h>
# else
#  include <db1/db.h>
# endif
#endif
#endif


#define ZERO(Dbt) memset (&(Dbt), sizeof (DBT), 0)

static DB *database;


static void
usage (const char *name)
{
  fprintf (stderr, "Usage: %s database key1 value1 key2 value2 ...\n", name);
  exit(1);
}

int
main (int argc, char *argv[])
{
  const char *db_name;
  int i;

  if (argc < 2)
    usage (argv[0]);

  if (argc % 2 != 0)
    usage (argv[0]);

  db_name = argv[1];

  database = dbopen (db_name, O_CREAT | O_RDWR, 0644, DB_HASH, NULL);
  if (!database)
  {
    fprintf (stderr, "Error opening database %s: %s\n",
             db_name, strerror (errno));
    exit (1);
  }

  for (i = 2; i < argc; i += 2)
  {
    DBT key;
    DBT value;

    ZERO (key);
    ZERO (value);

    key.data = argv[i];
    key.size = strlen (key.data);

    value.data = argv[i + 1];
    value.size = strlen (value.data);

    if (database->put (database, &key, &value, 0))
    {
      fprintf (stderr, "Error writing data.\n");
      exit (1);
    }
  }

  database->close (database);

  return 0;
}
