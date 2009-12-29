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
#ifndef HAVE_GLIB_2_8
#include <gfileutils-2.8.h>
#endif
#include <libguile.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "file-utils.h"
#include "gnc-engine.h"
#include "gnc-filepath-utils.h"
#include "gnc-gkeyfile-utils.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUILE;

/********************************************************************\
\********************************************************************/

char *
gncFindFile (const char * filename)
{
    const gchar *full_filename = NULL;
    SCM find_doc_file;
    SCM scm_filename;
    SCM scm_result;

    if (!filename || *filename == '\0')
        return NULL;

    find_doc_file = scm_c_eval_string("gnc:find-doc-file");
    scm_filename = scm_makfrom0str ((char *) filename);
    scm_result = scm_call_1(find_doc_file, scm_filename);

    if (SCM_STRINGP(scm_result))
        full_filename = SCM_STRING_CHARS(scm_result);

    return g_strdup (full_filename);
}

/********************************************************************\
 * htmlRead                                                         *
 *                                                                  *
 * Args:   file - the name of the html file to read                 *
 *         data - pointer to set to the buffer of data read in      *
 * Return: size of data read                                        *
 * Global: helpPath - the path to the help files                    *
\********************************************************************/
int
gncReadFile (const char * file, char ** data)
{
    char *buf = NULL;
    char  *filename;
    int   size = 0;
    int   fd;

    /* construct absolute path -- twiddle the relative path we received */
    if (!file || file[0] == '\0') return 0;

    /* take absolute paths without searching */
    if (!g_path_is_absolute (file))
        filename = gncFindFile (file);
    else
        filename = g_strdup (file);

    if (!filename) return 0;

    /* Open file: */
    fd = g_open( filename, O_RDONLY, 0 );

    g_free(filename);
    filename = NULL;

    if ( fd == -1 )
    {
        int norr = errno;
        PERR ("file %s: (%d) %s \n", file, norr, strerror(norr));
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

/**
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


/* Update one state file file to fit the new constraints introduced by
 * glib 2.12.5.  Supposedly its always been illegal to use spaces in
 * key names, but it was never a problem with earlier releases of
 * glib. Glib 2.12.5 added hard enforcement of this rule, completely
 * ignoring any key/value pair where the key name contained an
 * "illegal" character.  Glib 2.12.7 relented and changed the hard
 * failure to a warning, but the point has been made.  Spaces in key
 * names must go.
 */
static gboolean
gnc_update_state_file_keys(const gchar *filename)
{
    gchar *contents, **lines, *line, **kv, **parts, *part, *newkey;
    GError *error = NULL;
    int i, j;

    if (!g_file_get_contents(filename, &contents, NULL, &error))
    {
        DEBUG("Error reading state file: %s", error->message);
        g_error_free(error);
        return FALSE;
    }

    lines = g_strsplit_set(contents, "\r\n", -1);
    g_free(contents);

    /* Strip spaces from non-comment lines, and rewrite the new text
     * over top of the old text.  The new line is guaranteed to be at
     * most the same number of characters as the old. */
    for (i = 0, line = lines[i++]; line; line = lines[i++])
    {
        if ((*line == '\0') || (*line == '#') || (*line == '['))
        {
            continue;
        }
        else
        {
            kv = g_strsplit(line, "=", 2);
            parts = g_strsplit(kv[0], " ", -1);
            for (j = 0, part = parts[j++]; part; part = parts[j++])
                part[0] = g_ascii_toupper(part[0]);
            newkey = g_strjoinv("", parts);
            g_sprintf(line, "%s=%s", newkey, kv[1] ? kv[1] : "");
            g_free(newkey);
            g_strfreev(parts);
            g_strfreev(kv);
        }
    }

    contents = g_strjoinv("\n", lines);
    if (!g_file_set_contents(filename, contents, -1, &error))
    {
        DEBUG("Error writing state file: %s", error->message);
        g_error_free(error);
        g_free(contents);
        return FALSE;
    }

    g_free(contents);
    return TRUE;
}

/*  Find the state file that corresponds to this URL and guid.  The
 *  URL is used to compute the base name of the file (which will be in
 *  ~/.gnucash/books) and the guid is used to differentiate when the
 *  user has multiple data files with the same name. */
GKeyFile *
gnc_find_state_file (const gchar *url,
                     const gchar *guid,
                     gchar **filename_p)
{
    gchar *basename, *original = NULL, *filename, *tmp, *file_guid;
    GKeyFile *key_file = NULL;
    GError *error = NULL;
    gboolean do_increment;
    gint i;

    ENTER("url %s, guid %s", url, guid);
    tmp = strchr(url, ':');
    if (tmp)
        url = tmp + 1;

    basename = g_path_get_basename(url);
    DEBUG("Basename %s", basename);
    original = gnc_build_book_path(basename);
    g_free(basename);
    DEBUG("Original %s", original);

    i = 1;
    while (1)
    {
        if (i == 1)
            filename = g_strdup(original);
        else
            filename = g_strdup_printf("%s_%d", original, i);
        DEBUG("Trying %s", filename);
        key_file = gnc_key_file_load_from_file(filename, FALSE, FALSE, &error);
        DEBUG("Result %p", key_file);

        if (error &&
                (error->domain == G_KEY_FILE_ERROR) &&
                (error->code == G_KEY_FILE_ERROR_PARSE))
        {
            /* Handle the case where glib was updated first, and is refusing
             * to read old state files. */
            if (gnc_update_state_file_keys(filename))
            {
                DEBUG("Trying %s again", filename);
                key_file = gnc_key_file_load_from_file(filename, FALSE, FALSE, NULL);
                DEBUG("Result %p", key_file);
            }
        }
        if (error)
        {
            g_error_free(error);
            error = NULL;
        }
        if (!key_file)
        {
            DEBUG("No key file by that name");
            break;
        }

        file_guid = g_key_file_get_string(key_file,
                                          STATE_FILE_TOP, STATE_FILE_BOOK_GUID,
                                          NULL);
        DEBUG("File GUID is %s", file_guid ? file_guid : "<not found>");
        if (safe_strcmp(guid, file_guid) == 0)
        {
            DEBUG("Matched !!!");
            g_free(file_guid);
            break;
        }
        g_free(file_guid);

        /* Handle the case where gnucash was updated first, and is trying
         * to find new key names in an old state files. */
        file_guid = g_key_file_get_string(key_file,
                                          STATE_FILE_TOP, STATE_FILE_BOOK_GUID_OLD,
                                          NULL);
        DEBUG("%s is %s", STATE_FILE_BOOK_GUID,
              file_guid ? file_guid : "<not found>");
        if (safe_strcmp(guid, file_guid) == 0)
        {
            DEBUG("Matched !!!");
            do_increment = !gnc_update_state_file_keys(filename);
        }
        else
        {
            do_increment = TRUE;
        }

        DEBUG("Clean up this pass");
        g_free(file_guid);
        g_key_file_free(key_file);
        g_free(filename);
        if (do_increment)
            i++;
    }

    DEBUG("Clean up");
    g_free(original);
    if (filename_p)
        *filename_p = filename;
    else
        g_free(filename);
    LEAVE("key_file %p, filename %s", key_file,
          filename_p ? *filename_p : "(none)");
    return key_file;
}

/* ----------------------- END OF FILE ---------------------  */
