/* gfileutils.c - File utility functions
 *
 *  Copyright 2000 Red Hat, Inc.
 *
 * GLib is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * GLib is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with GLib; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 *   Boston, MA 02111-1307, USA.
 */
/* Contains all #includes, but otherwise only relevant differences between
 * GLib 2.6 and GLib 2.8 */

#include "config.h"

#include <glib.h>
#include "gfileutils-2.8.h"

#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifndef G_OS_WIN32
#include <sys/wait.h>
#endif
#include <fcntl.h>
#include <stdlib.h>

#ifdef G_OS_WIN32
#include <windows.h>
#include <io.h>
#endif /* G_OS_WIN32 */

#ifndef S_ISLNK
#define S_ISLNK(x) 0
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

#include <glib/gstdio.h>
#define _(String) (String)

static gboolean
rename_file (const char *old_name,
	     const char *new_name,
	     GError **err)
{
  errno = 0;
  if (g_rename (old_name, new_name) == -1)
    {
      int save_errno = errno;
      gchar *display_old_name = g_filename_display_name (old_name);
      gchar *display_new_name = g_filename_display_name (new_name);

      g_set_error (err,
		   G_FILE_ERROR,
		   g_file_error_from_errno (save_errno),
		   "Failed to rename file '%s' to '%s': g_rename() failed: %s",
		   display_old_name,
		   display_new_name,
		   g_strerror (save_errno));

      g_free (display_old_name);
      g_free (display_new_name);
      
      return FALSE;
    }
  
  return TRUE;
}

static gboolean
set_umask_permissions (int	     fd,
		       GError      **err)
{
#ifdef G_OS_WIN32

  return TRUE;

#else
  /* All of this function is just to work around the fact that
   * there is no way to get the umask without changing it.
   *
   * We can't just change-and-reset the umask because that would
   * lead to a race condition if another thread tried to change
   * the umask in between the getting and the setting of the umask.
   * So we have to do the whole thing in a child process.
   */

  int save_errno;
  pid_t pid;

  pid = fork ();
  
  if (pid == -1)
    {
      save_errno = errno;
      g_set_error (err,
		   G_FILE_ERROR,
		   g_file_error_from_errno (save_errno),
		   _("Could not change file mode: fork() failed: %s"),
		   g_strerror (save_errno));
      
      return FALSE;
    }
  else if (pid == 0)
    {
      /* child */
      mode_t mask = umask (0666);

      errno = 0;
      if (fchmod (fd, 0666 & ~mask) == -1)
	_exit (errno);
      else
	_exit (0);

      return TRUE; /* To quiet gcc */
    }
  else
    { 
      /* parent */
      int status;

      errno = 0;
      if (waitpid (pid, &status, 0) == -1)
	{
	  save_errno = errno;

	  g_set_error (err,
		       G_FILE_ERROR,
		       g_file_error_from_errno (save_errno),
		       _("Could not change file mode: waitpid() failed: %s"),
		       g_strerror (save_errno));

	  return FALSE;
	}

      if (WIFEXITED (status))
	{
	  save_errno = WEXITSTATUS (status);

	  if (save_errno == 0)
	    {
	      return TRUE;
	    }
	  else
	    {
	      g_set_error (err,
			   G_FILE_ERROR,
			   g_file_error_from_errno (save_errno),
			   _("Could not change file mode: chmod() failed: %s"),
			   g_strerror (save_errno));
      
	      return FALSE;
	    }
	}
      else if (WIFSIGNALED (status))
	{
	  g_set_error (err,
		       G_FILE_ERROR,
		       G_FILE_ERROR_FAILED,
		       _("Could not change file mode: Child terminated by signal: %s"),
		       g_strsignal (WTERMSIG (status)));
		       
	  return FALSE;
	}
      else
	{
	  /* This shouldn't happen */
	  g_set_error (err,
		       G_FILE_ERROR,
		       G_FILE_ERROR_FAILED,
		       _("Could not change file mode: Child terminated abnormally"));
	  return FALSE;
	}
    }
#endif
}

static gchar *
write_to_temp_file (const gchar *contents,
		    gssize length,
		    const gchar *template,
		    GError **err)
{
  gchar *tmp_name;
  gchar *display_name;
  gchar *retval;
  FILE *file;
  gint fd;
  int save_errno;

  retval = NULL;
  
  tmp_name = g_strdup_printf ("%s.XXXXXX", template);

  errno = 0;
  fd = g_mkstemp (tmp_name);
  display_name = g_filename_display_name (tmp_name);
      
  if (fd == -1)
    {
      save_errno = errno;
      g_set_error (err,
		   G_FILE_ERROR,
		   g_file_error_from_errno (save_errno),
		   "Failed to create file '%s': %s",
		   display_name, g_strerror (save_errno));
      
      goto out;
    }

  if (!set_umask_permissions (fd, err))
    {
      close (fd);
      g_unlink (tmp_name);

      goto out;
    }
  
  errno = 0;
  file = fdopen (fd, "wb");
  if (!file)
    {
      save_errno = errno;
      g_set_error (err,
		   G_FILE_ERROR,
		   g_file_error_from_errno (save_errno),
		   "Failed to open file '%s' for writing: fdopen() failed: %s",
		   display_name,
		   g_strerror (save_errno));

      close (fd);
      g_unlink (tmp_name);
      
      goto out;
    }

  if (length > 0)
    {
      size_t n_written;
      
      errno = 0;

      n_written = fwrite (contents, 1, length, file);

      if (n_written < length)
	{
	  save_errno = errno;
      
 	  g_set_error (err,
		       G_FILE_ERROR,
		       g_file_error_from_errno (save_errno),
		       "Failed to write file '%s': fwrite() failed: %s",
		       display_name,
		       g_strerror (save_errno));

	  fclose (file);
	  g_unlink (tmp_name);
	  
	  goto out;
	}
    }
   
  errno = 0;
  if (fclose (file) == EOF)
    { 
      save_errno = 0;
      
      g_set_error (err,
		   G_FILE_ERROR,
		   g_file_error_from_errno (save_errno),
		   "Failed to close file '%s': fclose() failed: %s",
		   display_name, 
		   g_strerror (save_errno));

      g_unlink (tmp_name);
      
      goto out;
    }

  retval = g_strdup (tmp_name);
  
 out:
  g_free (tmp_name);
  g_free (display_name);
  
  return retval;
}

/**
 * g_file_set_contents:
 * @filename: name of a file to write @contents to, in the GLib file name
 *   encoding
 * @contents: string to write to the file
 * @length: length of @contents, or -1 if @contents is a nul-terminated string
 * @error: return location for a #GError, or %NULL
 *
 * Writes all of @contents to a file named @filename, with good error checking.
 * If a file called @filename already exists it will be overwritten.
 *
 * This write is atomic in the sense that it is first written to a temporary
 * file which is then renamed to the final name. Notes:
 * <itemizedlist>
 * <listitem>
 *    On Unix, if @filename already exists hard links to @filename will break.
 *    Also since the file is recreated, existing permissions, access control
 *    lists, metadata etc. may be lost. If @filename is a symbolic link,
 *    the link itself will be replaced, not the linked file.
 * </listitem>
 * <listitem>
 *   On Windows renaming a file will not remove an existing file with the
 *   new name, so on Windows there is a race condition between the existing
 *   file being removed and the temporary file being renamed.
 * </listitem>
 * <listitem>
 *   On Windows there is no way to remove a file that is open to some
 *   process, or mapped into memory. Thus, this function will fail if
 *   @filename already exists and is open.
 * </listitem>
 * </itemizedlist>
 *
 * If the call was sucessful, it returns %TRUE. If the call was not successful,
 * it returns %FALSE and sets @error. The error domain is #G_FILE_ERROR.
 * Possible error codes are those in the #GFileError enumeration.
 *
 * Return value: %TRUE on success, %FALSE if an error occurred
 *
 * Since: 2.8
 **/
gboolean
g_file_set_contents (const gchar *filename,
		     const gchar *contents,
		     gssize	     length,
		     GError	   **error)
{
  gchar *tmp_filename;
  gboolean retval;
  GError *rename_error = NULL;
  
  g_return_val_if_fail (filename != NULL, FALSE);
  g_return_val_if_fail (error == NULL || *error == NULL, FALSE);
  g_return_val_if_fail (contents != NULL || length == 0, FALSE);
  g_return_val_if_fail (length >= -1, FALSE);
  
  if (length == -1)
    length = strlen (contents);

  tmp_filename = write_to_temp_file (contents, length, filename, error);
  
  if (!tmp_filename)
    {
      retval = FALSE;
      goto out;
    }

  if (!rename_file (tmp_filename, filename, &rename_error))
    {
#ifndef G_OS_WIN32

      g_unlink (tmp_filename);
      g_propagate_error (error, rename_error);
      retval = FALSE;
      goto out;

#else /* G_OS_WIN32 */
      
      /* Renaming failed, but on Windows this may just mean
       * the file already exists. So if the target file
       * exists, try deleting it and do the rename again.
       */
      if (!g_file_test (filename, G_FILE_TEST_EXISTS))
	{
	  g_unlink (tmp_filename);
	  g_propagate_error (error, rename_error);
	  retval = FALSE;
	  goto out;
	}

      g_error_free (rename_error);
      
      if (g_unlink (filename) == -1)
	{
          gchar *display_filename = g_filename_display_name (filename);

	  int save_errno = errno;
	  
	  g_set_error (error,
		       G_FILE_ERROR,
		       g_file_error_from_errno (save_errno),
		       "Existing file '%s' could not be removed: g_unlink() failed: %s",
		       display_filename,
		       g_strerror (save_errno));

	  g_free (display_filename);
	  g_unlink (tmp_filename);
	  retval = FALSE;
	  goto out;
	}
      
      if (!rename_file (tmp_filename, filename, error))
	{
	  g_unlink (tmp_filename);
	  retval = FALSE;
	  goto out;
	}

#endif
    }

  retval = TRUE;
  
 out:
  g_free (tmp_filename);
  return retval;
}
