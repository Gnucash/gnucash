/********************************************************************
 * gnc-xml-backend.cpp: Implement XML file backend.                 *
 * Copyright 2016 John Ralls <jralls@ceridwen.us>                   *
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

extern "C"
{
#include <config.h>
#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <regex.h>

#include <gnc-engine.h> //for GNC_MOD_BACKEND
#include <gnc-uri-utils.h>
#include <TransLog.h>
#include <gnc-prefs.h>

}

#include <sstream>

#include "gnc-xml-backend.hpp"
#include "gnc-backend-xml.h"
#include "io-gncxml-v2.h"
#include "io-gncxml.h"

#define XML_URI_PREFIX "xml://"
#define FILE_URI_PREFIX "file://"
static QofLogModule log_module = GNC_MOD_BACKEND;

bool
GncXmlBackend::check_path (const char* fullpath, bool create)
{
    GStatBuf statbuf;
    char* dirname = g_path_get_dirname (fullpath);
    /* Again check whether the directory can be accessed */
    auto rc = g_stat (dirname, &statbuf);
    if (rc != 0
#if COMPILER(MSVC)
        || (statbuf.st_mode & _S_IFDIR) == 0
#else
        || !S_ISDIR (statbuf.st_mode)
#endif
        )
    {
        /* Error on stat or if it isn't a directory means we
           cannot find this filename */
        set_error(ERR_FILEIO_FILE_NOT_FOUND);
        std::string msg {"Couldn't find directory for "};
        set_message(msg + fullpath);
        PWARN ("Couldn't find directory for %s", fullpath);
        g_free(dirname);
        return false;
    }

    /* Now check whether we can g_stat the file itself */
    rc = g_stat (fullpath, &statbuf);
    if ((rc != 0) && (!create))
    {
        /* Error on stat means the file doesn't exist */
        set_error(ERR_FILEIO_FILE_NOT_FOUND);
        std::string msg {"Couldn't find "};
        set_message(msg + fullpath);
        PWARN ("Couldn't find %s", fullpath);
        g_free(dirname);
        return false;
    }
    if (rc == 0
#if COMPILER(MSVC)
        && (statbuf.st_mode & _S_IFDIR) != 0
#else
        && S_ISDIR (statbuf.st_mode)
#endif
        )
    {
        set_error(ERR_FILEIO_UNKNOWN_FILE_TYPE);
        std::string msg {"Path "};
        msg += fullpath;
        set_message(msg +  " is a directory");
        PWARN ("Path %s is a directory", fullpath);
        g_free(dirname);
        return false;
    }
    return true;
}

void
GncXmlBackend::session_begin(QofSession* session, const char* book_id,
                       bool ignore_lock, bool create, bool force)
{
    /* Make sure the directory is there */
    m_fullpath = gnc_uri_get_path (book_id);

    if (m_fullpath.empty())
    {
        set_error(ERR_FILEIO_FILE_NOT_FOUND);
        set_message("No path specified");
        return;
    }
    if (create && !force && save_may_clobber_data())
    {
        set_error(ERR_BACKEND_STORE_EXISTS);
        PWARN ("Might clobber, no force");
        return;
    }

    if (!check_path(m_fullpath.c_str(), create))
        return;
    m_dirname = g_path_get_dirname (m_fullpath.c_str());



    /* ---------------------------------------------------- */
    /* We should now have a fully resolved path name.
     * Let's start logging */
    xaccLogSetBaseName (m_fullpath.c_str());
    PINFO ("logpath=%s", m_fullpath.empty() ? "(null)" : m_fullpath.c_str());

    /* And let's see if we can get a lock on it. */
    m_lockfile = m_fullpath + ".LCK";

    if (!ignore_lock && !get_file_lock())
    {
        // We should not ignore the lock, but couldn't get it. The
        // be_get_file_lock() already set the appropriate backend_error in this
        // case, so we just return here.
        m_lockfile.clear();

        if (force)
        {
            QofBackendError berror = get_error();
            if (berror == ERR_BACKEND_LOCKED || berror == ERR_BACKEND_READONLY)
            {
                // Even though we couldn't get the lock, we were told to force
                // the opening. This is ok because the FORCE argument is
                // changed only if the caller wants a read-only book.
            }
            else
            {
                // Unknown error. Push it again on the error stack.
                set_error(berror);
                return;
            }
        }
    }
    m_book = nullptr;
}

void
GncXmlBackend::session_end()
{
    if (m_book && qof_book_is_readonly (m_book))
    {
        set_error(ERR_BACKEND_READONLY);
        return;
    }

    if (!m_linkfile.empty())
        g_unlink (m_linkfile.c_str());

    if (m_lockfd > 0)
        close (m_lockfd);

    if (!m_lockfile.empty())
    {
        int rv;
#ifdef G_OS_WIN32
        /* On windows, we need to allow write-access before
           g_unlink() can succeed */
        rv = g_chmod (m_lockfile.c_str(), S_IWRITE | S_IREAD);
#endif
        rv = g_unlink (m_lockfile.c_str());
        if (rv)
        {
            PWARN ("Error on g_unlink(%s): %d: %s", m_lockfile.c_str(),
                   errno, g_strerror (errno) ? g_strerror (errno) : "");
        }
    }

    m_dirname.clear();
    m_fullpath.clear();
    m_lockfile.clear();
    m_linkfile.clear();
}

static QofBookFileType
determine_file_type (const std::string& path)
{
    gboolean with_encoding;
    QofBookFileType v2type;

    v2type = gnc_is_xml_data_file_v2 (path.c_str(), &with_encoding);
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

void
GncXmlBackend::load(QofBook* book, QofBackendLoadType loadType)
{

    QofBackendError error;

    if (loadType != LOAD_TYPE_INITIAL_LOAD) return;

    error = ERR_BACKEND_NO_ERR;
    m_book = book;

    int rc;
    switch (determine_file_type (m_fullpath))
    {
    case GNC_BOOK_XML2_FILE:
        rc = qof_session_load_from_xml_file_v2 (this, book,
                                                     GNC_BOOK_XML2_FILE);
        if (rc == FALSE)
        {
            PWARN ("Syntax error in Xml File %s", m_fullpath.c_str());
            error = ERR_FILEIO_PARSE_ERROR;
        }
        break;

    case GNC_BOOK_XML2_FILE_NO_ENCODING:
        error = ERR_FILEIO_NO_ENCODING;
        PWARN ("No character encoding in Xml File %s", m_fullpath.c_str());
        break;
    case GNC_BOOK_XML1_FILE:
        rc = qof_session_load_from_xml_file (book, m_fullpath.c_str());
        if (rc == FALSE)
        {
            PWARN ("Syntax error in Xml File %s", m_fullpath.c_str());
            error = ERR_FILEIO_PARSE_ERROR;
        }
        break;
    case GNC_BOOK_POST_XML2_0_0_FILE:
        error = ERR_BACKEND_TOO_NEW;
        PWARN ("Version of Xml file %s is newer than what we can read",
               m_fullpath.c_str());
        break;
    default:
        /* If file type wasn't known, check errno again to give the
        user some more useful feedback for some particular error
        conditions. */
        switch (errno)
        {
        case EACCES: /* No read permission */
            PWARN ("No read permission to file");
            error = ERR_FILEIO_FILE_EACCES;
            break;
        case EISDIR: /* File is a directory - but on this error we don't arrive here */
            PWARN ("Filename is a directory");
            error = ERR_FILEIO_FILE_NOT_FOUND;
            break;
        default:
            PWARN ("File not any known type");
            error = ERR_FILEIO_UNKNOWN_FILE_TYPE;
            break;
        }
        break;
    }

    if (error != ERR_BACKEND_NO_ERR)
    {
        set_error(error);
    }

    /* We just got done loading, it can't possibly be dirty !! */
    qof_book_mark_session_saved (book);
}

void
GncXmlBackend::sync(QofBook* book)
{
        /* We make an important assumption here, that we might want to change
     * in the future: when the user says 'save', we really save the one,
     * the only, the current open book, and nothing else. In any case the plans
     * for multiple books have been removed in the meantime and there is just one
     * book, no more.
     */
    if (m_book == nullptr) m_book = book;
    if (book != m_book) return;

    if (qof_book_is_readonly (m_book))
    {
        /* Are we read-only? Don't continue in this case. */
        set_error(ERR_BACKEND_READONLY);
        return;
    }

    write_to_file (true);
    remove_old_files();
}

bool
GncXmlBackend::save_may_clobber_data()
{
    if (m_fullpath.empty())
        return false;
    GStatBuf statbuf;
    auto rc = g_stat (m_fullpath.c_str(), &statbuf);
    return rc == 0;
}

void
GncXmlBackend::export_coa(QofBook* book)
{
    auto out = fopen(m_fullpath.c_str(), "w");
    if (out == NULL)
    {
        set_error(ERR_FILEIO_WRITE_ERROR);
        set_message(strerror(errno));
        return;
    }
    gnc_book_write_accounts_to_xml_filehandle_v2(this, book, out);
    fclose(out);
}

bool
GncXmlBackend::write_to_file (bool make_backup)
{
    QofBackendError be_err;

    ENTER (" book=%p file=%s", m_book, m_fullpath.c_str());

    if (m_book && qof_book_is_readonly (m_book))
    {
        /* Are we read-only? Don't continue in this case. */
        set_error(ERR_BACKEND_READONLY);
        LEAVE ("");
        return FALSE;
    }

    /* If the book is 'clean', recently saved, then don't save again. */
    /* XXX this is currently broken due to faulty 'Save As' logic. */
    /* if (FALSE == qof_book_session_not_saved (book)) return FALSE; */


    auto tmp_name = g_new (char, strlen (m_fullpath.c_str()) + 12);
    strcpy (tmp_name, m_fullpath.c_str());
    strcat (tmp_name, ".tmp-XXXXXX");

    /* Clang static analyzer flags this as a security risk, which is
     * theoretically true, but we can't use mkstemp because we need to
     * open the file ourselves because of compression. None of the alternatives
     * is any more secure.
     */
    if (!mktemp (tmp_name))
    {
        g_free (tmp_name);
        set_error(ERR_BACKEND_MISC);
        set_message("Failed to make temp file");
        LEAVE ("");
        return FALSE;
    }

    if (make_backup)
    {
        if (!backup_file ())
        {
            g_free (tmp_name);
            LEAVE ("");
            return FALSE;
        }
    }

    if (gnc_book_write_to_xml_file_v2 (m_book, tmp_name,
                                       gnc_prefs_get_file_save_compressed ()))
    {
        /* Record the file's permissions before g_unlinking it */
        GStatBuf statbuf;
        auto rc = g_stat (m_fullpath.c_str(), &statbuf);
        if (rc == 0)
        {
            /* We must never chmod the file /dev/null */
            g_assert (g_strcmp0 (tmp_name, "/dev/null") != 0);

            /* Use the permissions from the original data file */
            if (g_chmod (tmp_name, statbuf.st_mode) != 0)
            {
                /* set_error(ERR_BACKEND_PERM); */
                /* set_message("Failed to chmod filename %s", tmp_name ); */
                /* Even if the chmod did fail, the save
                   nevertheless completed successfully. It is
                   therefore wrong to signal the ERR_BACKEND_PERM
                   error here which implies that the saving itself
                   failed. Instead, we simply ignore this. */
                PWARN ("unable to chmod filename %s: %s",
                       tmp_name ? tmp_name : "(null)",
                       g_strerror (errno) ? g_strerror (errno) : "");
#if VFAT_DOESNT_SUCK  /* chmod always fails on vfat/samba fs */
                /* g_free(tmp_name); */
                /* return FALSE; */
#endif
            }
#ifdef HAVE_CHOWN
            /* Don't try to change the owner. Only root can do
               that. */
            if (chown (tmp_name, -1, statbuf.st_gid) != 0)
            {
                /* set_error(ERR_BACKEND_PERM); */
                /* set_message("Failed to chown filename %s", tmp_name ); */
                /* A failed chown doesn't mean that the saving itself
                failed. So don't abort with an error here! */
                PWARN ("unable to chown filename %s: %s",
                       tmp_name ? tmp_name : "(null)",
                       strerror (errno) ? strerror (errno) : "");
#if VFAT_DOESNT_SUCK /* chown always fails on vfat fs */
                /* g_free(tmp_name);
                return FALSE; */
#endif
            }
#endif
        }
        if (g_unlink (m_fullpath.c_str()) != 0 && errno != ENOENT)
        {
            set_error(ERR_BACKEND_READONLY);
            PWARN ("unable to unlink filename %s: %s",
                   m_fullpath.empty() ? "(null)" : m_fullpath.c_str(),
                   g_strerror (errno) ? g_strerror (errno) : "");
            g_free (tmp_name);
            LEAVE ("");
            return FALSE;
        }
        if (!link_or_make_backup (tmp_name, m_fullpath))
        {
            set_error(ERR_FILEIO_BACKUP_ERROR);
            std::string msg{"Failed to make backup file "};
            set_message(msg + (m_fullpath.empty() ? "NULL" : m_fullpath));
            g_free (tmp_name);
            LEAVE ("");
            return FALSE;
        }
        if (g_unlink (tmp_name) != 0)
        {
            set_error(ERR_BACKEND_PERM);
            PWARN ("unable to unlink temp filename %s: %s",
                   tmp_name ? tmp_name : "(null)",
                   g_strerror (errno) ? g_strerror (errno) : "");
            g_free (tmp_name);
            LEAVE ("");
            return FALSE;
        }
        g_free (tmp_name);

        /* Since we successfully saved the book,
         * we should mark it clean. */
        qof_book_mark_session_saved (m_book);
        LEAVE (" successful save of book=%p to file=%s", m_book,
               m_fullpath.c_str());
        return TRUE;
    }
    else
    {
        if (g_unlink (tmp_name) != 0)
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
            set_error(be_err);
            PWARN ("unable to unlink temp_filename %s: %s",
                   tmp_name ? tmp_name : "(null)",
                   g_strerror (errno) ? g_strerror (errno) : "");
            /* already in an error just flow on through */
        }
        else
        {
            /* Use a generic write error code */
            set_error(ERR_FILEIO_WRITE_ERROR);
            std::string msg{"Unable to write to temp file "};
            set_message(msg + (tmp_name ? tmp_name : "NULL"));
        }
        g_free (tmp_name);
        LEAVE ("");
        return FALSE;
    }
    g_free (tmp_name);
    LEAVE ("");
    return TRUE;
}

static bool
copy_file (const std::string& orig, const std::string& bkup)
{
    constexpr size_t buf_size = 1024;
    char buf[buf_size];
    int flags = 0;
    ssize_t count_write = 0;
    ssize_t count_read = 0;


#ifdef G_OS_WIN32
    flags = O_BINARY;
#endif

    auto orig_fd = g_open (orig.c_str(), O_RDONLY | flags, 0);
    if (orig_fd == -1)
    {
        return false;
    }
    auto bkup_fd = g_open (bkup.c_str(),
                           O_WRONLY | O_CREAT | O_TRUNC | O_EXCL | flags, 0600);
    if (bkup_fd == -1)
    {
        close (orig_fd);
        return FALSE;
    }

    do
    {
        count_read = read (orig_fd, buf, buf_size);
        if (count_read == -1 && errno != EINTR)
        {
            close (orig_fd);
            close (bkup_fd);
            return FALSE;
        }

        if (count_read > 0)
        {
            count_write = write (bkup_fd, buf, count_read);
            if (count_write == -1)
            {
                close (orig_fd);
                close (bkup_fd);
                return FALSE;
            }
        }
    }
    while (count_read > 0);

    close (orig_fd);
    close (bkup_fd);

    return TRUE;
}

bool
GncXmlBackend::link_or_make_backup (const std::string& orig,
                                    const std::string& bkup)
{
    gboolean copy_success = FALSE;
    int err_ret =
#ifdef HAVE_LINK
        link (orig.c_str(), bkup.c_str())
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
            copy_success = copy_file (orig.c_str(), bkup);
        }

        if (!copy_success)
        {
            set_error(ERR_FILEIO_BACKUP_ERROR);
            PWARN ("unable to make file backup from %s to %s: %s",
                   orig.c_str(), bkup.c_str(), g_strerror (errno) ? g_strerror (errno) : "");
            return false;
        }
    }

    return true;
}

bool
GncXmlBackend::get_file_lock ()
{
    GStatBuf statbuf;
#ifndef G_OS_WIN32
    char* pathbuf = NULL, *tmpbuf = NULL;
    size_t pathbuf_size = 0;
#endif
    QofBackendError be_err;

    auto rc = g_stat (m_lockfile.c_str(), &statbuf);
    if (!rc)
    {
        /* oops .. file is locked by another user  .. */
        set_error(ERR_BACKEND_LOCKED);
        return false;
    }

    m_lockfd = g_open (m_lockfile.c_str(), O_RDWR | O_CREAT | O_EXCL ,
                         S_IRUSR | S_IWUSR);
    if (m_lockfd < 0)
    {
        /* oops .. we can't create the lockfile .. */
        switch (errno)
        {
        case EACCES:
        case EROFS:
        case ENOSPC:
            be_err = ERR_BACKEND_READONLY;
            break;
        default:
            be_err = ERR_BACKEND_LOCKED;
            break;
        }
        if (errno != EEXIST) // Can't lock, but not because the file is locked
            PWARN ("Unable to create the lockfile %s: %s",
                   m_lockfile.c_str(), strerror(errno));
        set_error(be_err);
        return false;
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
    auto path = m_lockfile.find_last_of('.');
    std::stringstream linkfile;
    if (path != std::string::npos)
        linkfile << m_lockfile.substr(0, path);
    else
        linkfile << m_lockfile;
    linkfile << "." << gethostid() << "." << getpid() << ".LNK";
    rc = link (m_lockfile.c_str(), linkfile.str().c_str());
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
            return true;
        }

        /* Otherwise, something else is wrong. */
        set_error(ERR_BACKEND_LOCKED);
        g_unlink (linkfile.str().c_str());
        close (m_lockfd);
        g_unlink (m_lockfile.c_str());
        return false;
    }

    rc = g_stat (m_lockfile.c_str(), &statbuf);
    if (rc)
    {
        /* oops .. stat failed!  This can't happen! */
        set_error(ERR_BACKEND_LOCKED);
        std::string msg{"Failed to stat lockfile "};
        set_message(msg + m_lockfile);
        g_unlink (linkfile.str().c_str());
        close (m_lockfd);
        g_unlink (m_lockfile.c_str());
        return false;
    }

    if (statbuf.st_nlink != 2)
    {
        set_error(ERR_BACKEND_LOCKED);
        g_unlink (linkfile.str().c_str());
        close (m_lockfd);
        g_unlink (m_lockfile.c_str());
        return false;
    }

    m_linkfile = linkfile.str();
    return true;

#else /* ifndef G_OS_WIN32 */
    /* On windows, there is no NFS and the open(,O_CREAT | O_EXCL)
       is sufficient for locking. */
    return true;
#endif /* ifndef G_OS_WIN32 */
}

bool
GncXmlBackend::backup_file()
{
    GStatBuf statbuf;

    auto datafile = m_fullpath.c_str();

    auto rc = g_stat (datafile, &statbuf);
    if (rc)
        return (errno == ENOENT);

    if (determine_file_type (m_fullpath) == GNC_BOOK_BIN_FILE)
    {
        /* make a more permanent safer backup */
        auto bin_bkup = m_fullpath + "-binfmt.bkup";
        auto bkup_ret = link_or_make_backup (m_fullpath, bin_bkup);
        if (!bkup_ret)
        {
            return false;
        }
    }

    auto timestamp = gnc_date_timestamp ();
    auto backup = m_fullpath + "." + timestamp + GNC_DATAFILE_EXT;
    g_free (timestamp);

    return link_or_make_backup (datafile, backup);
}

/*
 * Clean up any lock files from prior crashes, and clean up old
 * backup and log files.
 */

void
GncXmlBackend::remove_old_files ()
{
    GStatBuf lockstatbuf, statbuf;

    if (g_stat (m_lockfile.c_str(), &lockstatbuf) != 0)
        return;

    auto dir = g_dir_open (m_dirname.c_str(), 0, NULL);
    if (!dir)
        return;

    auto now = gnc_time (NULL);
    const char* dent;
    while ((dent = g_dir_read_name (dir)) != NULL)
    {
        gchar* name;

        /* Ensure we only evaluate GnuCash related files. */
        if (! (g_str_has_suffix (dent, ".LNK") ||
               g_str_has_suffix (dent, ".xac") /* old data file extension */ ||
               g_str_has_suffix (dent, GNC_DATAFILE_EXT) ||
               g_str_has_suffix (dent, GNC_LOGFILE_EXT)))
            continue;

        name = g_build_filename (m_dirname.c_str(), dent, (gchar*)NULL);

        /* Only evaluate files associated with the current data file. */
        if (!g_str_has_prefix (name, m_fullpath.c_str()))
        {
            g_free (name);
            continue;
        }

        /* Never remove the current data file itself */
        if (g_strcmp0 (name, m_fullpath.c_str()) == 0)
        {
            g_free (name);
            continue;
        }

        /* Test if the current file is a lock file */
        if (g_str_has_suffix (name, ".LNK"))
        {
            /* Is a lock file. Skip the active lock file */
            if ((g_strcmp0 (name, m_linkfile.c_str()) != 0) &&
                /* Only delete lock files older than the active one */
                (g_stat (name, &statbuf) == 0) &&
                (statbuf.st_mtime < lockstatbuf.st_mtime))
            {
                PINFO ("remove stale lock file: %s", name);
                g_unlink (name);
            }

            g_free (name);
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
            gchar* stamp_start = name + strlen (m_fullpath.c_str());
            gchar* expression = g_strdup_printf ("^\\.[[:digit:]]{14}(\\%s|\\%s|\\.xac)$",
                                                 GNC_DATAFILE_EXT, GNC_LOGFILE_EXT);
            gboolean got_date_stamp = FALSE;

            if (regcomp (&pattern, expression, REG_EXTENDED | REG_ICASE) != 0)
                PWARN ("Cannot compile regex for date stamp");
            else if (regexec (&pattern, stamp_start, 0, NULL, 0) == 0)
                got_date_stamp = TRUE;

            regfree (&pattern);
            g_free (expression);

            if (!got_date_stamp) /* Not a gnucash created file after all... */
            {
                g_free (name);
                continue;
            }
        }

        /* The file is a backup or log file. Check the user's retention preference
         * to determine if we should keep it or not
         */
        if (gnc_prefs_get_file_retention_policy () == XML_RETAIN_NONE)
        {
            PINFO ("remove stale file: %s  - reason: preference XML_RETAIN_NONE", name);
            g_unlink (name);
        }
        else if ((gnc_prefs_get_file_retention_policy () == XML_RETAIN_DAYS) &&
                 (gnc_prefs_get_file_retention_days () > 0))
        {
            int days;

            /* Is the backup file old enough to delete */
            if (g_stat (name, &statbuf) != 0)
            {
                g_free (name);
                continue;
            }
            days = (int) (difftime (now, statbuf.st_mtime) / 86400);

            PINFO ("file retention = %d days", gnc_prefs_get_file_retention_days ());
            if (days >= gnc_prefs_get_file_retention_days ())
            {
                PINFO ("remove stale file: %s  - reason: more than %d days old", name, days);
                g_unlink (name);
            }
        }
        g_free (name);
    }
    g_dir_close (dir);
}
