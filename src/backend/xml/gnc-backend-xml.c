/********************************************************************
 * gnc-backend-xml.c: load and save data to XML files               *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/
/** @file gnc-backend-xml.c
 *  @brief load and save data to XML files
 *  @author Copyright (c) 2000 Gnumatic Inc.
 *  @author Copyright (c) 2002 Derek Atkins <warlord@MIT.EDU>
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an ordinary Unix filesystem file.
 */

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <libintl.h>
#include <locale.h>
#include <fcntl.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <regex.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#else
# ifdef _MSC_VER
typedef int ssize_t;
# endif
#endif
#include <errno.h>
#include <string.h>
#ifdef HAVE_DIRENT_H
# include <dirent.h>
#endif
#include <time.h>
#ifdef G_OS_WIN32
# include <io.h>
# define close _close
# define mktemp _mktemp
# define read _read
# define write _write
#endif
#include "platform.h"
#if COMPILER(MSVC)
# define g_fopen fopen
# define g_open _open
#endif

#include "qof.h"
#include "TransLog.h"
#include "gnc-engine.h"

#include "gnc-uri-utils.h"

#include "io-gncxml.h"
#include "io-gncxml-v2.h"
#include "gnc-backend-xml.h"
#include "gnc-prefs.h"

#include "gnc-address-xml-v2.h"
#include "gnc-bill-term-xml-v2.h"
#include "gnc-customer-xml-v2.h"
#include "gnc-employee-xml-v2.h"
#include "gnc-entry-xml-v2.h"
#include "gnc-invoice-xml-v2.h"
#include "gnc-job-xml-v2.h"
#include "gnc-order-xml-v2.h"
#include "gnc-owner-xml-v2.h"
#include "gnc-tax-table-xml-v2.h"
#include "gnc-vendor-xml-v2.h"

#ifndef HAVE_STRPTIME
# include "strptime.h"
#endif

static QofLogModule log_module = GNC_MOD_BACKEND;

static gboolean save_may_clobber_data (QofBackend *bend);

/* ================================================================= */

static gboolean
gnc_xml_be_get_file_lock (FileBackend *be)
{
    struct stat statbuf;
#ifndef G_OS_WIN32
    char *pathbuf = NULL, *path = NULL, *tmpbuf = NULL;
    size_t pathbuf_size = 0;
#endif
    int rc;
    QofBackendError be_err;

    rc = g_stat (be->lockfile, &statbuf);
    if (!rc)
    {
        /* oops .. file is locked by another user  .. */
        qof_backend_set_error ((QofBackend*)be, ERR_BACKEND_LOCKED);
        return FALSE;
    }

    be->lockfd = g_open (be->lockfile, O_RDWR | O_CREAT | O_EXCL , S_IRUSR | S_IWUSR);
    if (be->lockfd < 0)
    {
        /* oops .. we can't create the lockfile .. */
        switch (errno)
        {
        case EACCES:
        case EROFS:
        case ENOSPC:
            PWARN( "Unable to create the lockfile %s; may not have write priv",
                   be->lockfile );
            be_err = ERR_BACKEND_READONLY;
            break;
        default:
            be_err = ERR_BACKEND_LOCKED;
            break;
        }
        qof_backend_set_error ((QofBackend*)be, be_err);
        return FALSE;
    }

    /* OK, now work around some NFS atomic lock race condition
     * mumbo-jumbo.  We do this by linking a unique file, and
     * then examining the link count.  At least that's what the
     * NFS programmers guide suggests.
     * Note: the "unique filename" must be unique for the
     * triplet filename-host-process, otherwise accidental
     * aliases can occur.
     */

    /* apparently, even this code may not work for some NFS
     * implementations. In the long run, I am told that
     * ftp.debian.org
     *  /pub/debian/dists/unstable/main/source/libs/liblockfile_0.1-6.tar.gz
     * provides a better long-term solution.
     */

#ifndef G_OS_WIN32
    pathbuf_size = strlen (be->lockfile) + 100;
    pathbuf = (char *) malloc (pathbuf_size);
    if (pathbuf == NULL) {
      return FALSE;
    }
    strcpy (pathbuf, be->lockfile);
    path = strrchr (pathbuf, '.');
    while (snprintf (path, pathbuf_size - (path - pathbuf), ".%lx.%d.LNK", gethostid(), getpid())
            >= pathbuf_size - (path - pathbuf))
    {
        pathbuf_size += 100;
        tmpbuf = (char *) realloc (pathbuf, pathbuf_size);
        if (tmpbuf == NULL) {
          free(pathbuf);
          return FALSE;
        } else {
          pathbuf = tmpbuf;
        }
    }

    rc = link (be->lockfile, pathbuf);
    if (rc)
    {
        /* If hard links aren't supported, just allow the lock. */
        if (errno == EPERM || errno == ENOSYS
# ifdef EOPNOTSUPP
                || errno == EOPNOTSUPP
# endif
# ifdef ENOTSUP
                || errno == ENOTSUP
# endif
           )
        {
            be->linkfile = NULL;
            free (pathbuf);
            return TRUE;
        }

        /* Otherwise, something else is wrong. */
        qof_backend_set_error ((QofBackend*)be, ERR_BACKEND_LOCKED);
        g_unlink (pathbuf);
        free (pathbuf);
        close (be->lockfd);
        g_unlink (be->lockfile);
        return FALSE;
    }

    rc = g_stat (be->lockfile, &statbuf);
    if (rc)
    {
        /* oops .. stat failed!  This can't happen! */
        qof_backend_set_error ((QofBackend*)be, ERR_BACKEND_LOCKED);
        qof_backend_set_message ((QofBackend*)be, "Failed to stat lockfile %s",
                                 be->lockfile );
        g_unlink (pathbuf);
        free (pathbuf);
        close (be->lockfd);
        g_unlink (be->lockfile);
        return FALSE;
    }

    if (statbuf.st_nlink != 2)
    {
        qof_backend_set_error ((QofBackend*)be, ERR_BACKEND_LOCKED);
        g_unlink (pathbuf);
        free (pathbuf);
        close (be->lockfd);
        g_unlink (be->lockfile);
        return FALSE;
    }

    be->linkfile = g_strdup (pathbuf);
    free (pathbuf);

    return TRUE;

#else /* ifndef G_OS_WIN32 */
    /* On windows, there is no NFS and the open(,O_CREAT | O_EXCL)
       is sufficient for locking. */
    be->linkfile = NULL;
    return TRUE;
#endif /* ifndef G_OS_WIN32 */
}

/* ================================================================= */
#define XML_URI_PREFIX "xml://"
#define FILE_URI_PREFIX "file://"

static void
xml_session_begin(QofBackend *be_start, QofSession *session,
                  const char *book_id, gboolean ignore_lock,
                  gboolean create, gboolean force)
{
    FileBackend *be = (FileBackend*) be_start;

    ENTER (" ");

    /* Make sure the directory is there */
    be->fullpath = gnc_uri_get_path (book_id);

    if (NULL == be->fullpath)
    {
        qof_backend_set_error (be_start, ERR_FILEIO_FILE_NOT_FOUND);
        qof_backend_set_message (be_start, "No path specified");
        LEAVE("");
        return;
    }
    if (create && !force && save_may_clobber_data( be_start ) )
    {
        qof_backend_set_error (be_start, ERR_BACKEND_STORE_EXISTS);
        LEAVE("Might clobber, no force");
        return;
    }

    be->be.fullpath = be->fullpath;
    be->dirname = g_path_get_dirname (be->fullpath);

    {
        struct stat statbuf;
        int rc;

        /* Again check whether the directory can be accessed */
        rc = g_stat (be->dirname, &statbuf);
        if (rc != 0
#if COMPILER(MSVC)
                || (statbuf.st_mode & _S_IFDIR) == 0
#else
                || !S_ISDIR(statbuf.st_mode)
#endif
           )
        {
            /* Error on stat or if it isn't a directory means we
               cannot find this filename */
            qof_backend_set_error (be_start, ERR_FILEIO_FILE_NOT_FOUND);
            qof_backend_set_message (be_start, "Couldn't find directory for %s", be->fullpath);
            PWARN ("Couldn't find directory for %s", be->fullpath);
            g_free (be->fullpath);
            be->fullpath = NULL;
            g_free (be->dirname);
            be->dirname = NULL;
            LEAVE("");
            return;
        }

        /* Now check whether we can g_stat the file itself */
        rc = g_stat (be->fullpath, &statbuf);
        if ((rc != 0) && (!create))
        {
            /* Error on stat means the file doesn't exist */
            qof_backend_set_error (be_start, ERR_FILEIO_FILE_NOT_FOUND);
            qof_backend_set_message (be_start, "Couldn't find %s", be->fullpath);
            PWARN ("Couldn't find %s", be->fullpath);
            g_free (be->fullpath);
            be->fullpath = NULL;
            g_free (be->dirname);
            be->dirname = NULL;
            LEAVE("");
            return;
        }
        if (rc == 0
#if COMPILER(MSVC)
                && (statbuf.st_mode & _S_IFDIR) != 0
#else
                && S_ISDIR(statbuf.st_mode)
#endif
           )
        {
            qof_backend_set_error (be_start, ERR_FILEIO_UNKNOWN_FILE_TYPE);
            qof_backend_set_message(be_start, "Path %s is a directory",
                                    be->fullpath);
            PWARN("Path %s is a directory", be->fullpath);
            g_free (be->fullpath);
            be->fullpath = NULL;
            g_free (be->dirname);
            be->dirname = NULL;
            LEAVE("");
            return;
        }
    }


    /* ---------------------------------------------------- */
    /* We should now have a fully resolved path name.
     * Let's start logging */
    xaccLogSetBaseName (be->fullpath);
    PINFO ("logpath=%s", be->fullpath ? be->fullpath : "(null)");

    /* And let's see if we can get a lock on it. */
    be->lockfile = g_strconcat(be->fullpath, ".LCK", NULL);

    if (!ignore_lock && !gnc_xml_be_get_file_lock (be))
    {
        // We should not ignore the lock, but couldn't get it. The
        // be_get_file_lock() already set the appropriate backend_error in this
        // case, so we just return here.
        g_free (be->lockfile);
        be->lockfile = NULL;

        if (force)
        {
            QofBackendError berror = qof_backend_get_error(be_start);
            if (berror == ERR_BACKEND_LOCKED || berror == ERR_BACKEND_READONLY)
            {
                // Even though we couldn't get the lock, we were told to force
                // the opening. This is ok because the FORCE argument is
                // changed only if the caller wants a read-only book.
            }
            else
            {
                // Unknown error. Push it again on the error stack.
                qof_backend_set_error(be_start, berror);
            }
        }

        LEAVE("");
        return;
    }

    LEAVE (" ");
    return;
}

/* ================================================================= */

static void
xml_session_end(QofBackend *be_start)
{
    FileBackend *be = (FileBackend*)be_start;
    ENTER (" ");

    if ( be->book && qof_book_is_readonly( be->book ) )
    {
        qof_backend_set_error( (QofBackend*)be, ERR_BACKEND_READONLY );
        return;
    }

    if (be->linkfile)
        g_unlink (be->linkfile);

    if (be->lockfd > 0)
        close (be->lockfd);

    if (be->lockfile)
    {
        int rv;
#ifdef G_OS_WIN32
        /* On windows, we need to allow write-access before
           g_unlink() can succeed */
        rv = g_chmod (be->lockfile, S_IWRITE | S_IREAD);
#endif
        rv = g_unlink (be->lockfile);
        if (rv)
        {
            PWARN("Error on g_unlink(%s): %d: %s", be->lockfile,
                  errno, g_strerror(errno) ? g_strerror(errno) : "");
        }
    }

    g_free (be->dirname);
    be->dirname = NULL;

    g_free (be->fullpath);
    be->fullpath = NULL;

    g_free (be->lockfile);
    be->lockfile = NULL;

    g_free (be->linkfile);
    be->linkfile = NULL;
    LEAVE (" ");
}

static void
xml_destroy_backend(QofBackend *be)
{
    /* Stop transaction logging */
    xaccLogSetBaseName (NULL);

    qof_backend_destroy(be);
    g_free(be);
}

/* ================================================================= */
/* Write the financial data in a book to a file, returning FALSE on
   error and setting the error_result to indicate what went wrong if
   it's not NULL.  This function does not manage file locks in any
   way.

   If make_backup is true, write out a time-stamped copy of the file
   into the same directory as the indicated file, with a filename of
   "file.YYYYMMDDHHMMSS.gnucash" where YYYYMMDDHHMMSS is replaced with the
   current year/month/day/hour/minute/second. */

/* The variable buf_size must be a compile-time constant */
#define buf_size 1024

static gboolean
copy_file(const char *orig, const char *bkup)
{
    char buf[buf_size];
    int orig_fd;
    int bkup_fd;
    int flags = 0;
    ssize_t count_write;
    ssize_t count_read;

#ifdef G_OS_WIN32
    flags = O_BINARY;
#endif

    orig_fd = g_open(orig, O_RDONLY | flags, 0);
    if (orig_fd == -1)
    {
        return FALSE;
    }
    bkup_fd = g_open(bkup, O_WRONLY | O_CREAT | O_TRUNC | O_EXCL | flags, 0600);
    if (bkup_fd == -1)
    {
        close(orig_fd);
        return FALSE;
    }

    do
    {
        count_read = read(orig_fd, buf, buf_size);
        if (count_read == -1 && errno != EINTR)
        {
            close(orig_fd);
            close(bkup_fd);
            return FALSE;
        }

        if (count_read > 0)
        {
            count_write = write(bkup_fd, buf, count_read);
            if (count_write == -1)
            {
                close(orig_fd);
                close(bkup_fd);
                return FALSE;
            }
        }
    }
    while (count_read > 0);

    close(orig_fd);
    close(bkup_fd);

    return TRUE;
}

/* ================================================================= */

static gboolean
gnc_int_link_or_make_backup(FileBackend *be, const char *orig, const char *bkup)
{
    gboolean copy_success = FALSE;
    int err_ret =
#ifdef HAVE_LINK
        link (orig, bkup)
#else
        - 1
#endif
        ;
    if (err_ret != 0)
    {
#ifdef HAVE_LINK
        if (errno == EPERM || errno == ENOSYS
# ifdef EOPNOTSUPP
                || errno == EOPNOTSUPP
# endif
# ifdef ENOTSUP
                || errno == ENOTSUP
# endif
# ifdef ENOSYS
                || errno == ENOSYS
# endif
           )
#endif
        {
            copy_success = copy_file(orig, bkup);
        }

        if (!copy_success)
        {
            qof_backend_set_error((QofBackend*)be, ERR_FILEIO_BACKUP_ERROR);
            PWARN ("unable to make file backup from %s to %s: %s",
                   orig, bkup, g_strerror(errno) ? g_strerror(errno) : "");
            return FALSE;
        }
    }

    return TRUE;
}

/* ================================================================= */

static QofBookFileType
gnc_xml_be_determine_file_type(const char *path)
{
    gboolean with_encoding;
    QofBookFileType v2type;

    v2type = gnc_is_xml_data_file_v2(path, &with_encoding);
    if (v2type == GNC_BOOK_XML2_FILE)
    {
        if (with_encoding)
        {
            return GNC_BOOK_XML2_FILE;
        }
        else
        {
            return GNC_BOOK_XML2_FILE_NO_ENCODING;
        }
    }
    else if (v2type == GNC_BOOK_POST_XML2_0_0_FILE)
    {
        return GNC_BOOK_POST_XML2_0_0_FILE;
    }
    else if (v2type == GNC_BOOK_XML1_FILE)
    {
        return GNC_BOOK_XML1_FILE;
    }
    return GNC_BOOK_NOT_OURS;
}

static gboolean
gnc_determine_file_type (const char *uri)
{
    struct stat sbuf;
    int rc;
    FILE *t;
    gchar *filename;
    QofBookFileType xml_type;
    gboolean result;

    if (!uri)
    {
        return FALSE;
    }

    filename = gnc_uri_get_path ( uri );
    if (0 == g_strcmp0(filename, QOF_STDOUT))
    {
        result = FALSE;
        goto det_exit;
    }
    t = g_fopen( filename, "r" );
    if (!t)
    {
        PINFO (" new file");
        result = TRUE;
        goto det_exit;
    }
    fclose(t);
    rc = g_stat(filename, &sbuf);
    if (rc < 0)
    {
        result = FALSE;
        goto det_exit;
    }
    if (sbuf.st_size == 0)
    {
        PINFO (" empty file");
        result = TRUE;
        goto det_exit;
    }
    xml_type = gnc_is_xml_data_file_v2(filename, NULL);
    if ((xml_type == GNC_BOOK_XML2_FILE) ||
            (xml_type == GNC_BOOK_XML1_FILE) ||
            (xml_type == GNC_BOOK_POST_XML2_0_0_FILE))
    {
        result = TRUE;
        goto det_exit;
    }
    PINFO (" %s is not a gnc XML file", filename);
    result = FALSE;

det_exit:
    g_free ( filename );
    return result;
}

static gboolean
gnc_xml_be_backup_file(FileBackend *be)
{
    gboolean bkup_ret;
    char *timestamp;
    char *backup;
    const char *datafile;
    struct stat statbuf;
    int rc;

    datafile = be->fullpath;

    rc = g_stat (datafile, &statbuf);
    if (rc)
        return (errno == ENOENT);

    if (gnc_xml_be_determine_file_type(datafile) == GNC_BOOK_BIN_FILE)
    {
        /* make a more permanent safer backup */
        const char *back = "-binfmt.bkup";
        char *bin_bkup = g_new(char, strlen(datafile) + strlen(back) + 1);
        strcpy(bin_bkup, datafile);
        strcat(bin_bkup, back);
        bkup_ret = gnc_int_link_or_make_backup(be, datafile, bin_bkup);
        g_free(bin_bkup);
        if (!bkup_ret)
        {
            return FALSE;
        }
    }

    timestamp = gnc_date_timestamp ();
    backup = g_strconcat( datafile, ".", timestamp, GNC_DATAFILE_EXT, NULL );
    g_free (timestamp);

    bkup_ret = gnc_int_link_or_make_backup(be, datafile, backup);
    g_free(backup);

    return bkup_ret;
}

/* ================================================================= */

static gboolean
gnc_xml_be_write_to_file(FileBackend *fbe,
                         QofBook *book,
                         const gchar *datafile,
                         gboolean make_backup)
{
    QofBackend *be = &fbe->be;
    char *tmp_name;
    struct stat statbuf;
    int rc;
    QofBackendError be_err;

    ENTER (" book=%p file=%s", book, datafile);

    if (book && qof_book_is_readonly(book))
    {
        /* Are we read-only? Don't continue in this case. */
        qof_backend_set_error( (QofBackend*)be, ERR_BACKEND_READONLY );
        LEAVE("");
        return FALSE;
    }

    /* If the book is 'clean', recently saved, then don't save again. */
    /* XXX this is currently broken due to faulty 'Save As' logic. */
    /* if (FALSE == qof_book_session_not_saved (book)) return FALSE; */

    tmp_name = g_new(char, strlen(datafile) + 12);
    strcpy(tmp_name, datafile);
    strcat(tmp_name, ".tmp-XXXXXX");

    if (!mktemp(tmp_name))
    {
        qof_backend_set_error(be, ERR_BACKEND_MISC);
        qof_backend_set_message( be, "Failed to make temp file" );
        LEAVE("");
        return FALSE;
    }

    if (make_backup)
    {
        if (!gnc_xml_be_backup_file(fbe))
        {
            LEAVE("");
            return FALSE;
        }
    }

    if (gnc_book_write_to_xml_file_v2(book, tmp_name, gnc_prefs_get_file_save_compressed()))
    {
        /* Record the file's permissions before g_unlinking it */
        rc = g_stat(datafile, &statbuf);
        if (rc == 0)
        {
            /* We must never chmod the file /dev/null */
            g_assert(g_strcmp0(tmp_name, "/dev/null") != 0);

            /* Use the permissions from the original data file */
            if (g_chmod(tmp_name, statbuf.st_mode) != 0)
            {
                /* qof_backend_set_error(be, ERR_BACKEND_PERM); */
                /* qof_backend_set_message( be, "Failed to chmod filename %s", tmp_name ); */
                /* Even if the chmod did fail, the save
                   nevertheless completed successfully. It is
                   therefore wrong to signal the ERR_BACKEND_PERM
                   error here which implies that the saving itself
                   failed. Instead, we simply ignore this. */
                PWARN("unable to chmod filename %s: %s",
                      tmp_name ? tmp_name : "(null)",
                      g_strerror(errno) ? g_strerror(errno) : "");
#if VFAT_DOESNT_SUCK  /* chmod always fails on vfat/samba fs */
                /* g_free(tmp_name); */
                /* return FALSE; */
#endif
            }
#ifdef HAVE_CHOWN
            /* Don't try to change the owner. Only root can do
               that. */
            if (chown(tmp_name, -1, statbuf.st_gid) != 0)
            {
                /* qof_backend_set_error(be, ERR_BACKEND_PERM); */
                /* qof_backend_set_message( be, "Failed to chown filename %s", tmp_name ); */
                /* A failed chown doesn't mean that the saving itself
                failed. So don't abort with an error here! */
                PWARN("unable to chown filename %s: %s",
                      tmp_name ? tmp_name : "(null)",
                      strerror(errno) ? strerror(errno) : "");
#if VFAT_DOESNT_SUCK /* chown always fails on vfat fs */
                /* g_free(tmp_name);
                return FALSE; */
#endif
            }
#endif
        }
        if (g_unlink(datafile) != 0 && errno != ENOENT)
        {
            qof_backend_set_error(be, ERR_BACKEND_READONLY);
            PWARN("unable to unlink filename %s: %s",
                  datafile ? datafile : "(null)",
                  g_strerror(errno) ? g_strerror(errno) : "");
            g_free(tmp_name);
            LEAVE("");
            return FALSE;
        }
        if (!gnc_int_link_or_make_backup(fbe, tmp_name, datafile))
        {
            qof_backend_set_error(be, ERR_FILEIO_BACKUP_ERROR);
            qof_backend_set_message( be, "Failed to make backup file %s",
                                     datafile ? datafile : "NULL" );
            g_free(tmp_name);
            LEAVE("");
            return FALSE;
        }
        if (g_unlink(tmp_name) != 0)
        {
            qof_backend_set_error(be, ERR_BACKEND_PERM);
            PWARN("unable to unlink temp filename %s: %s",
                  tmp_name ? tmp_name : "(null)",
                  g_strerror(errno) ? g_strerror(errno) : "");
            g_free(tmp_name);
            LEAVE("");
            return FALSE;
        }
        g_free(tmp_name);

        /* Since we successfully saved the book,
         * we should mark it clean. */
        qof_book_mark_session_saved (book);
        LEAVE (" successful save of book=%p to file=%s", book, datafile);
        return TRUE;
    }
    else
    {
        if (g_unlink(tmp_name) != 0)
        {
            switch (errno)
            {
            case ENOENT:     /* tmp_name doesn't exist?  Assume "RO" error */
            case EACCES:
            case EPERM:
            case ENOSYS:
            case EROFS:
                be_err = ERR_BACKEND_READONLY;
                break;
            default:
                be_err = ERR_BACKEND_MISC;
                break;
            }
            qof_backend_set_error(be, be_err);
            PWARN("unable to unlink temp_filename %s: %s",
                  tmp_name ? tmp_name : "(null)",
                  g_strerror(errno) ? g_strerror(errno) : "");
            /* already in an error just flow on through */
        }
        else
        {
            /* Use a generic write error code */
            qof_backend_set_error(be, ERR_FILEIO_WRITE_ERROR);
            qof_backend_set_message( be, "Unable to write to temp file %s",
                                     tmp_name ? tmp_name : "NULL" );
        }
        g_free(tmp_name);
        LEAVE("");
        return FALSE;
    }
    LEAVE("");
    return TRUE;
}

/* ================================================================= */

/*
 * Clean up any lock files from prior crashes, and clean up old
 * backup and log files.
 */

static void
gnc_xml_be_remove_old_files(FileBackend *be)
{
    const gchar *dent;
    GDir *dir;
    struct stat lockstatbuf, statbuf;
    time64 now;

    if (g_stat (be->lockfile, &lockstatbuf) != 0)
        return;

    dir = g_dir_open (be->dirname, 0, NULL);
    if (!dir)
        return;

    now = gnc_time(NULL);
    while ((dent = g_dir_read_name(dir)) != NULL)
    {
        gchar *name;

        /* Ensure we only evaluate GnuCash related files. */
        if ( !(g_str_has_suffix(dent, ".LNK") ||
                g_str_has_suffix(dent, ".xac") /* old data file extension */ ||
                g_str_has_suffix(dent, GNC_DATAFILE_EXT) ||
                g_str_has_suffix(dent, GNC_LOGFILE_EXT)) )
            continue;

        name = g_build_filename(be->dirname, dent, (gchar*)NULL);

        /* Only evaluate files associated with the current data file. */
        if (!g_str_has_prefix(name, be->fullpath))
        {
            g_free(name);
            continue;
        }

        /* Never remove the current data file itself */
        if (g_strcmp0(name, be->fullpath) == 0)
        {
            g_free(name);
            continue;
        }

        /* Test if the current file is a lock file */
        if (g_str_has_suffix(name, ".LNK"))
        {
            /* Is a lock file. Skip the active lock file */
            if ((g_strcmp0(name, be->linkfile) != 0) &&
                    /* Only delete lock files older than the active one */
                    (g_stat(name, &statbuf) == 0) &&
                    (statbuf.st_mtime < lockstatbuf.st_mtime))
            {
                PINFO ("remove stale lock file: %s", name);
                g_unlink(name);
            }

            g_free(name);
            continue;
        }

        /* At this point we're sure the file's name is in one of these forms:
         * <fullpath/to/datafile><anything>.gnucash
         * <fullpath/to/datafile><anything>.xac
         * <fullpath/to/datafile><anything>.log
         *
         * To be a file generated by GnuCash, the <anything> part should consist
         * of 1 dot followed by 14 digits (0 to 9). Let's test this with a
         * regular expression.
         */
        {
            /* Find the start of the date stamp. This takes some pointer
             * juggling, but considering the above tests, this should always
             * be safe */
            regex_t pattern;
            gchar *stamp_start = name + strlen(be->fullpath);
            gchar *expression = g_strdup_printf ("^\\.[[:digit:]]{14}(\\%s|\\%s|\\.xac)$",
                                                 GNC_DATAFILE_EXT, GNC_LOGFILE_EXT);
            gboolean got_date_stamp = FALSE;

            if (regcomp(&pattern, expression, REG_EXTENDED | REG_ICASE) != 0)
                PWARN("Cannot compile regex for date stamp");
            else if (regexec(&pattern, stamp_start, 0, NULL, 0) == 0)
                got_date_stamp = TRUE;

            regfree(&pattern);
            g_free(expression);

            if (!got_date_stamp) /* Not a gnucash created file after all... */
            {
                g_free(name);
                continue;
            }
        }

        /* The file is a backup or log file. Check the user's retention preference
         * to determine if we should keep it or not
         */
        if (gnc_prefs_get_file_retention_policy() == XML_RETAIN_NONE)
        {
            PINFO ("remove stale file: %s  - reason: preference XML_RETAIN_NONE", name);
            g_unlink(name);
        }
        else if ((gnc_prefs_get_file_retention_policy() == XML_RETAIN_DAYS) &&
                 (gnc_prefs_get_file_retention_days() > 0))
        {
            int days;

            /* Is the backup file old enough to delete */
            if (g_stat(name, &statbuf) != 0)
            {
                g_free(name);
                continue;
            }
            days = (int)(difftime(now, statbuf.st_mtime) / 86400);

            PINFO ("file retention = %d days", gnc_prefs_get_file_retention_days());
            if (days >= gnc_prefs_get_file_retention_days())
            {
                PINFO ("remove stale file: %s  - reason: more than %d days old", name, days);
                g_unlink(name);
            }
        }
        g_free(name);
    }
    g_dir_close (dir);
}

static void
xml_sync_all(QofBackend* be, QofBook *book)
{
    FileBackend *fbe = (FileBackend *) be;
    ENTER ("book=%p, fbe->book=%p", book, fbe->book);

    /* We make an important assumption here, that we might want to change
     * in the future: when the user says 'save', we really save the one,
     * the only, the current open book, and nothing else. In any case the plans
     * for multiple books have been removed in the meantime and there is just one
     * book, no more.
     */
    if (NULL == fbe->book) fbe->book = book;
    if (book != fbe->book) return;

    if (qof_book_is_readonly( fbe->book ) )
    {
        /* Are we read-only? Don't continue in this case. */
        qof_backend_set_error( (QofBackend*)be, ERR_BACKEND_READONLY );
        return;
    }

    gnc_xml_be_write_to_file (fbe, book, fbe->fullpath, TRUE);
    gnc_xml_be_remove_old_files (fbe);
    LEAVE ("book=%p", book);
}

/* ================================================================= */
/* Routines to deal with the creation of multiple books.
 * The core design assumption here is that the book
 * begin-edit/commit-edit routines are used solely to write out
 * closed accounting periods to files.  They're not currently
 * designed to do anything other than this. (Although they could be).
 */

static char *
build_period_filepath (FileBackend *fbe, QofBook *book)
{
    int len;
    char *str, *p, *q;

    len = strlen (fbe->fullpath) + GUID_ENCODING_LENGTH + 14;
    str = g_new (char, len);
    strcpy (str, fbe->fullpath);

    /* XXX it would be nice for the user if we made the book
     * closing date and/or title part of the file-name. */
    p = strrchr (str, G_DIR_SEPARATOR);
    p++;
    p = stpcpy (p, "book-");
    p = guid_to_string_buff (qof_book_get_guid(book), p);
    p = stpcpy (p, "-");
    q = strrchr (fbe->fullpath, G_DIR_SEPARATOR);
    q++;
    p = stpcpy (p, q);
    p = stpcpy (p, ".gml");

    return str;
}

static void
xml_begin_edit (QofBackend *be, QofInstance *inst)
{
    if (0) build_period_filepath(0, 0);
#if BORKEN_FOR_NOW
    FileBackend *fbe = (FileBackend *) be;
    QofBook *book = gp;
    const char * filepath;

    QofIdTypeConst typ = QOF_INSTANCE(inst)->e_type;
    if (strcmp (GNC_ID_PERIOD, typ)) return;
    filepath = build_period_filepath(fbe, book);
    PINFO (" ====================== book=%p filepath=%s\n", book, filepath);

    if (NULL == fbe->primary_book)
    {
        PERR ("You should have saved the data "
              "at least once before closing the books!\n");
    }
    /* XXX To be anal about it, we should really be checking to see
     * if there already is a file with this book GncGUID, and disallowing
     * further progress.  This is because we are not allowed to
     * modify books that are closed (They should be treated as
     * 'read-only').
     */
#endif
}

static void
xml_rollback_edit (QofBackend *be, QofInstance *inst)
{
#if BORKEN_FOR_NOW
    QofBook *book = gp;

    if (strcmp (GNC_ID_PERIOD, typ)) return;
    PINFO ("book=%p", book);
#endif
}

static void
xml_commit_edit (QofBackend *be, QofInstance *inst)
{
    if (qof_instance_get_dirty(inst) && qof_get_alt_dirty_mode() &&
            !(qof_instance_get_infant(inst) && qof_instance_get_destroying(inst)))
    {
        qof_collection_mark_dirty(qof_instance_get_collection(inst));
        qof_book_mark_session_dirty(qof_instance_get_book(inst));
    }
#if BORKEN_FOR_NOW
    FileBackend *fbe = (FileBackend *) be;
    QofBook *book = gp;
    const char * filepath;

    if (strcmp (GNC_ID_PERIOD, typ)) return;
    filepath = build_period_filepath(fbe, book);
    PINFO (" ====================== book=%p filepath=%s\n", book, filepath);
    gnc_xml_be_write_to_file(fbe, book, filepath, FALSE);

    /* We want to force a save of the current book at this point,
     * because if we don't, and the user forgets to do so, then
     * there'll be the same transactions in the closed book,
     * and also in the current book. */
    gnc_xml_be_write_to_file (fbe, fbe->primary_book, fbe->fullpath, TRUE);
#endif
}

/* ---------------------------------------------------------------------- */


/* Load financial data from a file into the book, automatically
   detecting the format of the file, if possible.  Return FALSE on
   error, and set the error parameter to indicate what went wrong if
   it's not NULL.  This function does not manage file locks in any
   way. */

static void
gnc_xml_be_load_from_file (QofBackend *bend, QofBook *book, QofBackendLoadType loadType)
{
    QofBackendError error;
    gboolean rc;
    FileBackend *be = (FileBackend *) bend;

    if (loadType != LOAD_TYPE_INITIAL_LOAD) return;

    error = ERR_BACKEND_NO_ERR;
    be->book = book;

    switch (gnc_xml_be_determine_file_type(be->fullpath))
    {
    case GNC_BOOK_XML2_FILE:
        rc = qof_session_load_from_xml_file_v2 (be, book, GNC_BOOK_XML2_FILE);
        if (FALSE == rc)
        {
            PWARN( "Syntax error in Xml File %s", be->fullpath );
            error = ERR_FILEIO_PARSE_ERROR;
        }
        break;

    case GNC_BOOK_XML2_FILE_NO_ENCODING:
        error = ERR_FILEIO_NO_ENCODING;
        PWARN( "No character encoding in Xml File %s", be->fullpath );
        break;
    case GNC_BOOK_XML1_FILE:
        rc = qof_session_load_from_xml_file (book, be->fullpath);
        if (FALSE == rc)
        {
            PWARN( "Syntax error in Xml File %s", be->fullpath );
            error = ERR_FILEIO_PARSE_ERROR;
        }
        break;
    case GNC_BOOK_POST_XML2_0_0_FILE:
        error = ERR_BACKEND_TOO_NEW;
        PWARN( "Version of Xml file %s is newer than what we can read", be->fullpath );
        break;
    default:
        /* If file type wasn't known, check errno again to give the
        user some more useful feedback for some particular error
        conditions. */
        switch (errno)
        {
        case EACCES: /* No read permission */
            PWARN("No read permission to file");
            error = ERR_FILEIO_FILE_EACCES;
            break;
        case EISDIR: /* File is a directory - but on this error we don't arrive here */
            PWARN("Filename is a directory");
            error = ERR_FILEIO_FILE_NOT_FOUND;
            break;
        default:
            PWARN("File not any known type");
            error = ERR_FILEIO_UNKNOWN_FILE_TYPE;
            break;
        }
        break;
    }

    if (error != ERR_BACKEND_NO_ERR)
    {
        qof_backend_set_error(bend, error);
    }

    /* We just got done loading, it can't possibly be dirty !! */
    qof_book_mark_session_saved (book);
}

/* ---------------------------------------------------------------------- */

static gboolean
save_may_clobber_data (QofBackend *bend)
{
    struct stat statbuf;
    if (!bend->fullpath) return FALSE;

    /* FIXME: Make sure this doesn't need more sophisticated semantics
     * in the face of special file, devices, pipes, symlinks, etc. */
    if (g_stat(bend->fullpath, &statbuf) == 0) return TRUE;
    return FALSE;
}


static void
gnc_xml_be_write_accounts_to_file(QofBackend *be, QofBook *book)
{
    const gchar *datafile;

    datafile = ((FileBackend *)be)->fullpath;
    gnc_book_write_accounts_to_xml_file_v2(be, book, datafile);
}

/* ================================================================= */

static QofBackend*
gnc_backend_new(void)
{
    FileBackend *gnc_be;
    QofBackend *be;

    gnc_be = g_new0(FileBackend, 1);
    be = (QofBackend*) gnc_be;
    qof_backend_init(be);

    be->session_begin = xml_session_begin;
    be->session_end = xml_session_end;
    be->destroy_backend = xml_destroy_backend;

    be->load = gnc_xml_be_load_from_file;

    /* The file backend treats accounting periods transactionally. */
    be->begin = xml_begin_edit;
    be->commit = xml_commit_edit;
    be->rollback = xml_rollback_edit;

    /* The file backend always loads all data ... */
    be->compile_query = NULL;
    be->free_query = NULL;
    be->run_query = NULL;

    /* The file backend will never be multi-user... */
    be->events_pending = NULL;
    be->process_events = NULL;

    be->sync = xml_sync_all;
    be->load_config = NULL;
    be->get_config = NULL;

    be->export_fn = gnc_xml_be_write_accounts_to_file;

    gnc_be->dirname = NULL;
    gnc_be->fullpath = NULL;
    gnc_be->lockfile = NULL;
    gnc_be->linkfile = NULL;
    gnc_be->lockfd = -1;

    gnc_be->book = NULL;

    return be;
}

static void
business_core_xml_init(void)
{
    /* Initialize our pointers into the backend subsystem */
    gnc_address_xml_initialize ();
    gnc_billterm_xml_initialize ();
    gnc_customer_xml_initialize ();
    gnc_employee_xml_initialize ();
    gnc_entry_xml_initialize ();
    gnc_invoice_xml_initialize ();
    gnc_job_xml_initialize ();
    gnc_order_xml_initialize ();
    gnc_owner_xml_initialize ();
    gnc_taxtable_xml_initialize ();
    gnc_vendor_xml_initialize ();
}

static void
gnc_provider_free (QofBackendProvider *prov)
{
    prov->provider_name = NULL;
    prov->access_method = NULL;
    g_free (prov);
}

#ifndef GNC_NO_LOADABLE_MODULES
G_MODULE_EXPORT void
qof_backend_module_init(void)
{
    gnc_module_init_backend_xml();
}
#endif

void
gnc_module_init_backend_xml(void)
{
    QofBackendProvider *prov;
    prov = g_new0 (QofBackendProvider, 1);
    prov->provider_name = "GnuCash File Backend Version 2";
    prov->access_method = "file";
    prov->partial_book_supported = FALSE;
    prov->backend_new = gnc_backend_new;
    prov->provider_free = gnc_provider_free;
    prov->check_data_type = gnc_determine_file_type;
    qof_backend_register_provider (prov);

    prov = g_new0 (QofBackendProvider, 1);
    prov->provider_name = "GnuCash File Backend Version 2";
    prov->access_method = "xml";
    prov->partial_book_supported = FALSE;
    prov->backend_new = gnc_backend_new;
    prov->provider_free = gnc_provider_free;
    prov->check_data_type = gnc_determine_file_type;
    qof_backend_register_provider (prov);

    /* And the business objects */
    business_core_xml_init();
}

/* ========================== END OF FILE ===================== */
