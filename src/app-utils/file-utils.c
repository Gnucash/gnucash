/********************************************************************\
 * file-utils.c -- simple file utilities                            *
 * Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>            *
 * Copyright (C) 1998 Rob Browning                                  *
 * Copyright (C) 1998-2000 Linas Vepstas <linas@linas.org>          *
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

#include "config.h"

#include <glib.h>
#include <glib/gstdio.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#else
# include <io.h>
# define close _close
# define lseek _lseek
# define read _read
#endif

#include "file-utils.h"
#include "gnc-engine.h"
#include "gnc-filepath-utils.h"
#include "gnc-gkeyfile-utils.h"
#include "gnc-uri-utils.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

/********************************************************************\
\********************************************************************/

/********************************************************************\
 * gncReadFile                                                      *
 *                                                                  *
 * Args:   filename - the name of the html file to read             *
 *         data - pointer to set to the buffer of data read in      *
 * Return: size of data read                                        *
 * Global: helpPath - the path to the help files                    *
\********************************************************************/
int
gncReadFile (const char * filename, char ** data)
{
    char *buf = NULL;
    char  *fullname;
    int   size = 0;
    int   fd;

    if (!filename || filename[0] == '\0') return 0;

    /* construct absolute path if we received a relative path */
    fullname = gnc_path_find_localized_html_file (filename);

    if (!fullname) return 0;

    /* Open file: */
    fd = g_open( fullname, O_RDONLY, 0 );

    g_free(fullname);
    fullname = NULL;

    if ( fd == -1 )
    {
        int norr = errno;
        PERR ("file %s: (%d) %s \n", filename, norr, strerror(norr));
        return 0;
    }

    /* Find size: */
    size = lseek( fd, 0, SEEK_END );
    lseek( fd, 0, SEEK_SET );

    /* Allocate memory */
    buf = g_new(char, size + 1);

    /* read in file */
    if ( read(fd, buf, size) == -1 )
    {
        g_free(buf);
        buf = NULL;
    }
    else
    {
        buf[size] = '\0';
    }

    close(fd);
    *data = buf;

    return size;
}

/***********************************************************************
 * gnc_getline -- read a line from the input file, up to and including
 *                the newline.
 *
 * Args:   line - pointer to hold the buffer for the whole line (allocated by
 *                this function)
 *         file - the file from which to read
 * Return: the number of bytes read
 *
 * The caller MUST g_free() the line returned from this call in all
 * cases where it is non-NULL!
 */

gint64
gnc_getline (gchar **line, FILE *file)
{
    char str[BUFSIZ];
    gint64 len;
    GString *gs;

    g_return_val_if_fail(line, -1);
    *line = NULL;
    g_return_val_if_fail(file, -1);

    gs = g_string_new("");

    while (fgets(str, sizeof(str), file) != NULL)
    {
        g_string_append(gs, str);

        len = strlen(str);
        if (str[len-1] == '\n')
            break;
    }

    len = gs->len;
    *line = gs->str;
    g_string_free(gs, FALSE);
    return len;
}

/* ----------------------- END OF FILE ---------------------  */
