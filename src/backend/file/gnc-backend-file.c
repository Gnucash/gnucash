/*********************************************************************
 * gnc-backend-file.c: load and save data to files
 *
 *
 *********************************************************************/

#define _GNU_SOURCE
#define __EXTENSIONS__

#include <stdio.h>
#include <fcntl.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <dirent.h>
#include <time.h>

#include "Backend.h"
#include "BackendP.h"
#include "Group.h"
#include "Scrub.h"
#include "TransLog.h"
#include "gnc-engine-util.h"
#include "gnc-pricedb-p.h"
#include "DateUtils.h"
#include "io-gncxml.h"
#include "io-gncbin.h"
#include "io-gncxml-v2.h"

#include "gnc-backend-api.h"
#include "gnc-session.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"

static short module = MOD_IO;

struct FileBackend_struct
{
    Backend be;

    char *dirname;
    char *fullpath;
    char *lockfile;
    char *linkfile;
    int lockfd;

    GNCSession *session;
};

typedef struct FileBackend_struct FileBackend;

typedef enum 
{
    GNC_BOOK_NOT_OURS,
    GNC_BOOK_BIN_FILE,
    GNC_BOOK_XML1_FILE,
    GNC_BOOK_XML2_FILE,
} GNCBookFileType;

static int file_retention_days = 0;

static void gnc_file_be_load_from_file(Backend *, GNCBook *);

static gboolean gnc_file_be_get_file_lock (FileBackend *be);
static gboolean gnc_file_be_write_to_file(FileBackend *be,
                                          gboolean make_backup);
static void gnc_file_be_remove_old_files(FileBackend *be);

void
gnc_file_be_set_retention_days (int days)
{
    file_retention_days = days;
}

int
gnc_file_be_get_retention_days (void)
{
    return file_retention_days;
}

static void
file_session_begin(Backend *be_start, GNCSession *session, const char *book_id,
                   gboolean ignore_lock, gboolean create_if_nonexistent)
{
    FileBackend* be;
    char *p;

    ENTER (" ");

    be = (FileBackend*) be_start;

    be->session = session;

    /* Make sure the directory is there */

    be->dirname = g_strdup (gnc_session_get_file_path (session));
    be->fullpath = g_strdup (be->dirname);
    p = strrchr (be->dirname, '/');
    if (p && p != be->dirname)
    {
        struct stat statbuf;
        int rc;

        *p = '\0';

        rc = stat (be->dirname, &statbuf);
        if (rc != 0 || !S_ISDIR(statbuf.st_mode))
        {
            xaccBackendSetError (be_start, ERR_FILEIO_FILE_NOT_FOUND);
            g_free (be->fullpath); be->fullpath = NULL;
            g_free (be->dirname); be->dirname = NULL;
            return;
        }
    }

    /* ---------------------------------------------------- */
    /* We should now have a fully resolved path name.
     * Lets see if we can get a lock on it. */

    be->lockfile = g_strconcat(be->fullpath, ".LCK", NULL);

    if (!ignore_lock && !gnc_file_be_get_file_lock (be))
    {
        xaccBackendSetError (be_start, ERR_BACKEND_LOCKED);
        g_free (be->lockfile); be->lockfile = NULL;
        return;
    }

    LEAVE (" ");
    return;
}


static void
file_session_end(Backend *be_start)
{
    FileBackend* be;

    be = (FileBackend*)be_start;

    if (be->linkfile)
        unlink (be->linkfile);

    if (be->lockfd > 0)
        close (be->lockfd);

    if (be->lockfile)
        unlink (be->lockfile);

    g_free (be->dirname);
    be->dirname = NULL;

    g_free (be->fullpath);
    be->fullpath = NULL;

    g_free (be->lockfile);
    be->lockfile = NULL;

    g_free (be->linkfile);
    be->linkfile = NULL;
}

static void
file_destroy_backend(Backend *be)
{
    g_free(be);
}

static void
file_sync_all(Backend* be, GNCBook *book)
{
    gnc_file_be_write_to_file((FileBackend*)be, TRUE);
    gnc_file_be_remove_old_files((FileBackend*)be);
}

Backend *
gnc_backend_new(void)
{
    FileBackend *fbe;
    Backend *be;
    
    fbe = g_new(FileBackend, 1);
    be = (Backend*)fbe;
    xaccInitBackend(be);
    
    be->session_begin = file_session_begin;
    be->book_load = gnc_file_be_load_from_file;
    be->price_load = NULL;
    be->session_end = file_session_end;
    be->destroy_backend = file_destroy_backend;

    /* The file backend will never have transactional
     * behaviour.  So these vectors are null. */

    be->account_begin_edit = NULL;
    be->account_commit_edit = NULL;
    be->trans_begin_edit = NULL;
    be->trans_commit_edit = NULL;
    be->trans_rollback_edit = NULL;
    be->price_begin_edit = NULL;
    be->price_commit_edit = NULL;

    /* the file backend always loads all data ... */
    be->run_query = NULL;
    be->price_lookup = NULL;

    be->counter = NULL;

    /* the file backend will never be multi-user... */
    be->events_pending = NULL;
    be->process_events = NULL;

    be->sync_all = file_sync_all;

    fbe->dirname = NULL;
    fbe->fullpath = NULL;
    fbe->lockfile = NULL;
    fbe->linkfile = NULL;
    fbe->lockfd = -1;

    fbe->session = NULL;

    return be;
}

/* ---------------------------------------------------------------------- */

static gboolean
gnc_file_be_get_file_lock (FileBackend *be)
{
    struct stat statbuf;
    char pathbuf[PATH_MAX];
    char *path = NULL;
    int rc;

    rc = stat (be->lockfile, &statbuf);
    if (!rc)
    {
        /* oops .. file is all locked up  .. */
        xaccBackendSetError ((Backend*)be, ERR_BACKEND_LOCKED);
        return FALSE;
    }

    be->lockfd = open (be->lockfile, O_RDWR | O_CREAT | O_EXCL , 0);
    if (be->lockfd < 0)
    {
        /* oops .. file is all locked up  .. */
        xaccBackendSetError ((Backend*)be, ERR_BACKEND_LOCKED);
        return FALSE;
    }

    /* OK, now work around some NFS atomic lock race condition 
     * mumbo-jumbo.  We do this by linking a unique file, and 
     * then examing the link count.  At least that's what the 
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

    strcpy (pathbuf, be->lockfile);
    path = strrchr (pathbuf, '.');
    sprintf (path, ".%lx.%d.LNK", gethostid(), getpid());

    rc = link (be->lockfile, pathbuf);
    if (rc)
    {
        /* If hard links aren't supported, just allow the lock. */
        if (errno == EOPNOTSUPP || errno == EPERM)
        {
            be->linkfile = NULL;
            return TRUE;
        }

        /* Otherwise, something else is wrong. */
        xaccBackendSetError ((Backend*)be, ERR_BACKEND_LOCKED);
        unlink (pathbuf);
        close (be->lockfd);
        unlink (be->lockfile);
        return FALSE;
    }

    rc = stat (be->lockfile, &statbuf);
    if (rc)
    {
        /* oops .. stat failed!  This can't happen! */
        xaccBackendSetError ((Backend*)be, ERR_BACKEND_LOCKED);
        unlink (pathbuf);
        close (be->lockfd);
        unlink (be->lockfile);
        return FALSE;
    }

    if (statbuf.st_nlink != 2)
    {
        xaccBackendSetError ((Backend*)be, ERR_BACKEND_LOCKED);
        unlink (pathbuf);
        close (be->lockfd);
        unlink (be->lockfile);
        return FALSE;
    }

    be->linkfile = g_strdup (pathbuf);

    return TRUE;
}

/* ---------------------------------------------------------------------- */

static gboolean
is_gzipped_file(const gchar *name)
{
    unsigned char buf[2];
    int fd = open(name, O_RDONLY);

    if(fd == 0)
    {
        return FALSE;
    }

    if(read(fd, buf, 2) != 2)
    {
        return FALSE;
    }

    if(buf[0] == 037 && buf[1] == 0213)
    {
        return TRUE;
    }
    
    return FALSE;
}
    
static GNCBookFileType
gnc_file_be_determine_file_type(const char *path)
{
    if(gnc_is_xml_data_file_v2(path)) {
        return GNC_BOOK_XML2_FILE;
    } else if(gnc_is_xml_data_file(path)) {
        return GNC_BOOK_XML1_FILE;
    } else if(is_gzipped_file(path)) {
        return GNC_BOOK_XML2_FILE;
    } else {
        return GNC_BOOK_BIN_FILE;
    }
}


/* Load financial data from a file into the book, automtically
   detecting the format of the file, if possible.  Return FALSE on
   error, and set the error parameter to indicate what went wrong if
   it's not NULL.  This function does not manage file locks in any
   way. */

static void
gnc_file_be_load_from_file (Backend *bend, GNCBook *book)
{
    GNCBackendError error = ERR_BACKEND_NO_ERR;
    gboolean rc;
    FileBackend *be = (FileBackend *) bend;

    switch (gnc_file_be_determine_file_type(be->fullpath))
    {
    case GNC_BOOK_XML2_FILE:
        rc = gnc_session_load_from_xml_file_v2 (be->session, NULL);
        if (FALSE == rc) error = ERR_FILEIO_PARSE_ERROR;
        break;

    case GNC_BOOK_XML1_FILE:
        rc = gnc_session_load_from_xml_file (be->session);
        if (FALSE == rc) error = ERR_FILEIO_PARSE_ERROR;
        break;

    case GNC_BOOK_BIN_FILE:
        /* presume it's an old-style binary file */
        gnc_session_load_from_binfile(be->session);
        error = gnc_get_binfile_io_error();
        break;

    default:
        PWARN("File not any known type");
        error = ERR_FILEIO_UNKNOWN_FILE_TYPE;
        break;
    }

    if(error != ERR_BACKEND_NO_ERR) 
    {
        xaccBackendSetError(bend, error);
        g_free(be->lockfile);
        be->lockfile = NULL;
    }
}

/* ---------------------------------------------------------------------- */

/* Write the financial data in a book to a file, returning FALSE on
   error and setting the error_result to indicate what went wrong if
   it's not NULL.  This function does not manage file locks in any
   way.

   If make_backup is true, write out a time-stamped copy of the file
   into the same directory as the indicated file, with a filename of
   "file.YYYYMMDDHHMMSS.xac" where YYYYMMDDHHMMSS is replaced with the
   current year/month/day/hour/minute/second. */

static gboolean
copy_file(const char *orig, const char *bkup)
{
    static int buf_size = 1024;
 char buf[buf_size];
    int orig_fd;
    int bkup_fd;
    ssize_t count_write;
    ssize_t count_read;

    orig_fd = open(orig, O_RDONLY);
    if(orig_fd == -1)
    {
        return FALSE;
    }
    bkup_fd = creat(bkup, 0600);
    if(bkup_fd == -1)
    {
        close(orig_fd);
        return FALSE;
    }

    do
    {
        count_read = read(orig_fd, buf, buf_size);
        if(count_read == -1 && errno != EINTR)
        {
            close(orig_fd);
            close(bkup_fd);
            return FALSE;
        }

        if(count_read > 0)
        {
            count_write = write(bkup_fd, buf, count_read);
            if(count_write == -1)
            {
                close(orig_fd);
                close(bkup_fd);
                return FALSE;
            }
        }
    } while(count_read > 0);

    close(orig_fd);
    close(bkup_fd);
    
    return TRUE;
}
        
static gboolean
gnc_int_link_or_make_backup(FileBackend *be, const char *orig, const char *bkup)
{
    int err_ret = link(orig, bkup);
    if(err_ret != 0)
    {
        if(errno == EPERM || errno == EOPNOTSUPP)
        {
            err_ret = copy_file(orig, bkup);
        }

        if(!err_ret)
        {
            xaccBackendSetError((Backend*)be, ERR_FILEIO_BACKUP_ERROR);
            PWARN ("unable to make file backup from %s to %s: %s", 
                    orig, bkup, strerror(errno) ? strerror(errno) : ""); 
            return FALSE;
        }
    }

    return TRUE;
}

static gboolean
gnc_file_be_backup_file(FileBackend *be)
{
    gboolean bkup_ret;
    char *timestamp;
    char *backup;
    const char *datafile;
    struct stat statbuf;
    int rc;

    datafile = be->fullpath;
    
    rc = stat (datafile, &statbuf);
    if (rc)
      return (errno == ENOENT);

    if(gnc_file_be_determine_file_type(datafile) == GNC_BOOK_BIN_FILE)
    {
        /* make a more permament safer backup */
        const char *back = "-binfmt.bkup";
        char *bin_bkup = g_new(char, strlen(datafile) + strlen(back) + 1);
        strcpy(bin_bkup, datafile);
        strcat(bin_bkup, back);
        bkup_ret = gnc_int_link_or_make_backup(be, datafile, bin_bkup);
        g_free(bin_bkup);
        if(!bkup_ret)
        {
            return FALSE;
        }
    }

    timestamp = xaccDateUtilGetStampNow ();
    backup = g_new (char, strlen (datafile) + strlen (timestamp) + 6);
    strcpy (backup, datafile);
    strcat (backup, ".");
    strcat (backup, timestamp);
    strcat (backup, ".xac");
    g_free (timestamp);

    bkup_ret = gnc_int_link_or_make_backup(be, datafile, backup);
    g_free(backup);

    return bkup_ret;
}

static int
gnc_file_be_select_files (const struct dirent *d)
{
    int len = strlen(d->d_name) - 4;

    if (len <= 0)
        return(0);
  
    return((strcmp(d->d_name + len, ".LNK") == 0) ||
	   (strcmp(d->d_name + len, ".xac") == 0) ||
	   (strcmp(d->d_name + len, ".log") == 0));
}

static void
gnc_file_be_remove_old_files(FileBackend *be)
{
    struct dirent **dent;
    struct stat lockstatbuf, statbuf;
    int pathlen, n;
    time_t now;

    if (stat (be->lockfile, &lockstatbuf) != 0)
        return;
    pathlen = strlen(be->fullpath);

    /*
     * Clean up any lockfiles from prior crashes, and clean up old
     * data and log files.  Scandir will do a fist pass on the
     * filenames and cull the directory down to just files with the
     * appropriate extensions.  Pity you can't pass user data into
     * scandir...
     */
    n = scandir(be->dirname, &dent, gnc_file_be_select_files, alphasort);
    if (n <= 0)
        return;

    now = time(NULL);
    while(n--) {
        char *name = g_strconcat(be->dirname, "/", dent[n]->d_name, NULL);
        int len = strlen(name) - 4;

        /* Is this file associated with the current data file */
        if (strncmp(name, be->fullpath, pathlen) == 0) {

            if ((strcmp(name + len, ".LNK") == 0) &&
		/* Is a lock file. Skip the active lock file */
                (strcmp(name, be->linkfile) != 0) &&
                /* Only delete lock files older than the active one */
                (stat(name, &statbuf) == 0) &&
                (statbuf.st_mtime <lockstatbuf.st_mtime)) {
	            unlink(name);
            } else if (file_retention_days != 0) {
	        time_t file_time;
	        struct tm file_tm;
	        int days;

                /* Is the backup file old enough to delete */
                memset(&file_tm, 0, sizeof(file_tm));
                strptime(name+pathlen+1, "%Y%m%d%H%M%S", &file_tm);
                file_time = mktime(&file_tm);
                days = (int)(difftime(now, file_time) / 86400);
                if (days > file_retention_days) {
                    unlink(name);
                }
            }
        }
        g_free(name);
        free(dent[n]);
    }
    free(dent);
}

    
static gboolean
gnc_file_be_write_to_file(FileBackend *be, gboolean make_backup)
{
    const gchar *datafile;
    char *tmp_name;
    GNCBook *book;

    book = gnc_session_get_book (be->session);

    datafile = be->fullpath;
    
    tmp_name = g_new(char, strlen(datafile) + 12);
    strcpy(tmp_name, datafile);
    strcat(tmp_name, ".tmp-XXXXXX");

    if(!mktemp(tmp_name))
    {
        xaccBackendSetError((Backend*)be, ERR_BACKEND_MISC);
        return FALSE;
    }
  
    if(make_backup)
    {
        if(!gnc_file_be_backup_file(be))
        {
            return FALSE;
        }
    }
  
    if(gnc_book_write_to_xml_file_v2(book, tmp_name)) 
    {
        if(unlink(datafile) != 0 && errno != ENOENT)
        {
            xaccBackendSetError((Backend*)be, ERR_BACKEND_MISC);
            PWARN("unable to unlink filename %s: %s",
                  datafile ? datafile : "(null)", 
                  strerror(errno) ? strerror(errno) : ""); 
            g_free(tmp_name);
            return FALSE;
        }
        if(!gnc_int_link_or_make_backup(be, tmp_name, datafile))
        {
            g_free(tmp_name);
            return FALSE;
        }
        if(unlink(tmp_name) != 0)
        {
            xaccBackendSetError((Backend*)be, ERR_BACKEND_MISC);
            PWARN("unable to unlink temp filename %s: %s", 
                   tmp_name ? tmp_name : "(null)", 
                   strerror(errno) ? strerror(errno) : ""); 
            g_free(tmp_name);
            return FALSE;
        }
        g_free(tmp_name);
        return TRUE;
    }
    else
    {
        if(unlink(tmp_name) != 0)
        {
            xaccBackendSetError((Backend*)be, ERR_BACKEND_MISC);
            PWARN("unable to unlink temp_filename %s: %s", 
                   tmp_name ? tmp_name : "(null)", 
                   strerror(errno) ? strerror(errno) : ""); 
            /* already in an error just flow on through */
        }
        g_free(tmp_name);
        return FALSE;
    }
}

