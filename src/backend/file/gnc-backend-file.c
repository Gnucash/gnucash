/*********************************************************************
 * gnc-backend-file.c
 *********************************************************************/

#include <fcntl.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

#include "Backend.h"
#include "BackendP.h"
#include "Group.h"
#include "NetIO.h"
#include "Scrub.h"
#include "TransLog.h"
#include "gnc-engine-util.h"
#include "gnc-pricedb-p.h"
#include "DateUtils.h"
#include "io-gncxml.h"
#include "io-gncbin.h"
#include "io-gncxml-v2.h"

#include "gnc-backend-api.h"
#include "gnc-book.h"
#include "gnc-book-p.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"

static short module = MOD_IO;

struct FileBackend_struct
{
    Backend be;

    char *fullpath;
    char *lockfile;
    char *linkfile;
    int lockfd;

    GNCBook *book;
};

typedef struct FileBackend_struct FileBackend;

typedef enum 
{
    GNC_BOOK_NOT_OURS,
    GNC_BOOK_BIN_FILE,
    GNC_BOOK_XML1_FILE,
    GNC_BOOK_XML2_FILE,
} GNCBookFileType;

static gboolean gnc_file_be_get_file_lock (FileBackend *be);
static gboolean gnc_file_be_load_from_file(FileBackend *be);
static gboolean gnc_file_be_write_to_file(FileBackend *be,
                                          gboolean make_backup);

static void
file_book_begin(Backend *be_start, GNCBook *book, const char *book_id, 
                gboolean ignore_lock, gboolean create_if_nonexistent)
{
    FileBackend* be;
    char *dirname;
    char *p;

    ENTER (" ");

    be = (FileBackend*)be_start;
    be->book = book;
  
    /* Make sure the directory is there */

    dirname = g_strdup (book->fullpath);
    be->fullpath = g_strdup (book->fullpath);
    p = strrchr (dirname, '/');
    if (p && p != dirname)
    {
        struct stat statbuf;
        int rc;

        *p = '\0';

        rc = stat (dirname, &statbuf);
        if (rc != 0 || !S_ISDIR(statbuf.st_mode))
        {
            xaccBackendSetError (be_start, ERR_FILEIO_FILE_NOT_FOUND);
            g_free (be->fullpath); be->fullpath = NULL;
            g_free (dirname);
            return;
        }
    }
    g_free (dirname);

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

static gboolean
file_load_file(Backend *be)
{
    if(!gnc_file_be_load_from_file((FileBackend*)be))
    {
        xaccBackendSetError(be, ERR_BACKEND_MISC);
        g_free(((FileBackend*)be)->lockfile); ((FileBackend*)be)->lockfile = NULL;
        return FALSE;
    }
    return TRUE;
}

static AccountGroup*
file_book_load(Backend *be)
{
    AccountGroup* ret = gnc_book_get_group(((FileBackend*)be)->book);
    if(ret == NULL)
    {
        if(!file_load_file(be))
        {
            PERR("file_load_file returned FALSE");
            return NULL;
        }
    }
    ret = gnc_book_get_group(((FileBackend*)be)->book);
    return ret;
}

static GNCPriceDB*
file_price_load(Backend *be)
{
    GNCPriceDB* ret = gnc_book_get_pricedb(((FileBackend*)be)->book);
    if(ret == NULL)
    {
        if(!file_load_file(be))
        {
            PERR("file_load_file returned FALSE");
            return NULL;
        }
    }
    ret = gnc_book_get_pricedb(((FileBackend*)be)->book);
    return ret;
}

static void
file_book_end(Backend *be_start)
{
    FileBackend* be;

    be = (FileBackend*)be_start;
    
    if (be->linkfile)
        unlink (be->linkfile);

    if (be->lockfd > 0)
        close (be->lockfd);

    if (be->lockfile)
        unlink (be->lockfile);

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
file_all_sync(Backend* be, AccountGroup *ag, GNCPriceDB *pricedb)
{
    gnc_file_be_write_to_file((FileBackend*)be, TRUE);
}

Backend *
gnc_backend_new(void)
{
    FileBackend *fbe;
    Backend *be;
    
    fbe = g_new(FileBackend, 1);
    be = (Backend*)fbe;
    xaccInitBackend(be);
    
    be->book_begin = file_book_begin;
    be->book_load = file_book_load;
    be->price_load = file_price_load;
    be->book_end = file_book_end;
    be->destroy_backend = file_destroy_backend;

/*     be->account_begin_edit = file_account_begin_edit; */
/*     be->account_commit_edit = file_account_commit_edit; */
/*     be->trans_begin_edit = file_trans_begin_edit; */
/*     be->trans_commit_edit = file_trans_commit_edit; */
/*     be->trans_rollback_edit = file_trans_rollback_edit; */
/*     be->price_begin_edit = file_price_begin_edit; */
/*     be->price_commit_edit = file_price_commit_edit; */

/*     be->run_query = file_run_query; */
/*     be->price_lookup = file_price_lookup; */
    be->all_sync = file_all_sync;

/*     be->events_pending = file_events_pending; */
/*     be->process_events = file_process_events; */

    fbe->fullpath = NULL;
    fbe->lockfile = NULL;
    fbe->linkfile = NULL;
    fbe->lockfd = -1;

    fbe->book = NULL;
    
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

static gboolean
happy_or_push_error(Backend *be, gboolean errret, GNCBackendError errcode)
{
    if(errret) {
        return TRUE;
    } else {
        xaccBackendSetError(be, errcode);
        return FALSE;
    }
}

static gboolean
gnc_file_be_load_from_file(FileBackend *be)
{
    switch (gnc_file_be_determine_file_type(be->fullpath))
    {
    case GNC_BOOK_XML2_FILE:
        return happy_or_push_error((Backend*)be,
                                   gnc_book_load_from_xml_file_v2(be->book,
                                                                  NULL),
                                   ERR_BACKEND_MISC);
    case GNC_BOOK_XML1_FILE:
        return happy_or_push_error((Backend*)be,
                                   gnc_book_load_from_xml_file(be->book),
                                   ERR_BACKEND_MISC);
    case GNC_BOOK_BIN_FILE:
    {
        /* presume it's an old-style binary file */
        GNCBackendError error;

        gnc_book_load_from_binfile(be->book);
        error = gnc_book_get_binfile_io_error();

        if(error == ERR_BACKEND_NO_ERR) {
            return TRUE;
        } else {
            xaccBackendSetError((Backend*)be, error);
            return FALSE;
        }
    }
    default:
        g_warning("File not any known type");
        xaccBackendSetError((Backend*)be, ERR_FILEIO_UNKNOWN_FILE_TYPE);
        return FALSE;
        break;
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
            xaccBackendSetError((Backend*)be, ERR_BACKEND_MISC);
/*                 g_strdup_printf("unable to make file backup from %s to %s: %s", */
/*                                 orig, bkup, */
/*                                 strerror(errno) ? strerror(errno) : "")); */
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

static gboolean
gnc_file_be_write_to_file(FileBackend *be, gboolean make_backup)
{
    const gchar *datafile;
    char *tmp_name;

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
  
    if(gnc_book_write_to_xml_file_v2(be->book, tmp_name)) 
    {
        if(unlink(datafile) != 0 && errno != ENOENT)
        {
            xaccBackendSetError((Backend*)be, ERR_BACKEND_MISC);
/*                 g_strdup_printf("unable to unlink filename %s: %s", */
/*                                 datafile ? datafile : "(null)", */
/*                                 strerror(errno) ? strerror(errno) : "")); */
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
/*                 g_strdup_printf("unable to unlink temp filename %s: %s", */
/*                                 tmp_name ? tmp_name : "(null)", */
/*                                 strerror(errno) ? strerror(errno) : "")); */
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
/*                 g_strdup_printf("unable to unlink temp_filename %s: %s", */
/*                                 tmp_name ? tmp_name : "(null)", */
/*                                 strerror(errno) ? strerror(errno) : "")); */
            /* already in an error just flow on through */
        }
        g_free(tmp_name);
        return FALSE;
    }
}

