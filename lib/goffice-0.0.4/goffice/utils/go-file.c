/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-file.c :
 *
 * Copyright (C) 2004 Morten Welinder (terra@gnome.org)
 * Copyright (C) 2004 Yukihiro Nakai  <nakai@gnome.gr.jp>
 * Copyright (C) 2003, Red Hat, Inc.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <goffice/goffice-config.h>
#include "go-file.h"
#include "go-glib-extras.h"
#include <gsf/gsf-input-memory.h>
#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-output-stdio.h>
#ifdef WITH_GNOME
#include <libgnomevfs/gnome-vfs-utils.h>
#include <libgnomevfs/gnome-vfs-mime-utils.h>
#include <gsf-gnome/gsf-input-gnomevfs.h>
#include <gsf-gnome/gsf-output-gnomevfs.h>
#include <libgnome/gnome-url.h>
#else
#ifdef G_OS_WIN32
#include <windows.h>
#include <winreg.h>
#include <goffice/utils/win32-stub.h>
#endif
#endif

#include <string.h>
#include <stdlib.h>
#include <unistd.h>

/* ------------------------------------------------------------------------- */

char *
go_filename_from_uri (const char *uri)
{
#ifdef WITH_GNOME
	return gnome_vfs_get_local_path_from_uri (uri);
#else
	return g_filename_from_uri (uri, NULL, NULL);
#endif
}


char *
go_filename_to_uri (const char *filename)
{
	if (g_path_is_absolute (filename)) {
		char *uri;
		char *simp = g_strdup (filename);
		char *p, *q;

		for (p = q = simp; *p;) {
			if (p != simp &&
			    p[0] == G_DIR_SEPARATOR &&
			    p[1] == G_DIR_SEPARATOR) {
				/* "//" --> "/", except initially.  */
				p++;
				continue;
			}

			if (p[0] == G_DIR_SEPARATOR &&
			    p[1] == '.' &&
			    p[2] == G_DIR_SEPARATOR) {
				/* "/./" -> "/".  */
				p += 2;
				continue;
			}

			*q++ = *p++;
		}
		*q = 0;

		/* FIXME: Resolve ".." parts.  */
#ifdef WITH_GNOME
		uri = gnome_vfs_get_uri_from_local_path (simp);
#else
		uri = g_filename_to_uri (simp, NULL, NULL);
#endif
		g_free (simp);
		return uri;
	} else {
		char *uri;
		char *current_dir = g_get_current_dir ();
		char *abs_filename =
			g_build_filename (current_dir, filename, NULL);
		g_return_val_if_fail (g_path_is_absolute (abs_filename), NULL);
		uri = go_filename_to_uri (abs_filename);
		g_free (current_dir);
		g_free (abs_filename);
		return uri;
	}
}

gchar **
go_shell_argv_to_glib_encoding (gint argc, gchar const **argv)
{
#ifdef G_OS_WIN32
	gchar **args;
	gint i;

	args = g_new (gchar *, argc);
	if (G_WIN32_IS_NT_BASED ())
	{
		LPWSTR *wargs;
		gint narg;
		GIConv conv;

		wargs = CommandLineToArgvW (GetCommandLineW (), &narg);
		conv = g_iconv_open ("utf-8", "utf-16le");
		for (i = 0; i < narg; ++i)
			args[i] = g_convert_with_iconv ((const gchar *) wargs[i], wcslen (wargs[i]) << 1, conv, NULL, NULL, NULL);
		g_iconv_close (conv);
	}
	else
	{
		for (i = 0; i < argc; ++i)
			args[i] = g_locale_to_utf8 (argv[i], -1, NULL, NULL, NULL);
	}

	return args;
#else
	return (gchar **) argv;
#endif
}

char *
go_shell_arg_to_uri (const char *arg)
{
#ifdef WITH_GNOME
	return gnome_vfs_make_uri_from_shell_arg (arg);
#else
	if (g_path_is_absolute (arg))
		return go_filename_to_uri (arg);
	else {
		/* See if it's a file: uri.  */
		gchar *tmp = go_filename_from_uri (arg);
		if (tmp) {
			g_free (tmp);
			return g_strdup (arg);
		}
	}

	/* Just assume it's a filename.  */
	return go_filename_to_uri (arg);
#endif
}

/**
 * go_basename_from_uri:
 * @uri :
 *
 * Decode the final path component.  Returns as UTF-8 encoded.
 **/
char *
go_basename_from_uri (const char *uri)
{
#ifdef WITH_GNOME
	char *raw_uri = gnome_vfs_unescape_string (uri, G_DIR_SEPARATOR_S);
	char *basename = raw_uri ? g_path_get_basename (raw_uri) : NULL;
	g_free (raw_uri);
#else
	char *uri_basename = g_path_get_basename (uri);
	char *fake_uri = g_strconcat ("file:///", uri_basename, NULL);
	char *filename = go_filename_from_uri (fake_uri);
	char *basename = filename ? g_path_get_basename (filename) : NULL;
	g_free (uri_basename);
	g_free (fake_uri);
	g_free (filename);

#endif
	{
		char *basename_utf8 = basename
			? g_filename_to_utf8 (basename, -1, NULL, NULL, NULL)
			: NULL;
		g_free (basename);
		return basename_utf8;
	}
}

/**
 * go_dirname_from_uri:
 * @uri :
 * @brief: if TRUE, hide "file://" if present.
 *
 * Decode the all but the final path component.  Returns as UTF-8 encoded.
 **/
char *
go_dirname_from_uri (const char *uri, gboolean brief)
{
	char *dirname_utf8, *dirname;

#ifdef WITH_GNOME
	char *raw_uri = gnome_vfs_unescape_string (uri, G_DIR_SEPARATOR_S);
	dirname = raw_uri ? g_path_get_dirname (raw_uri) : NULL;
	g_free (raw_uri);
#else
	char *uri_dirname = g_path_get_dirname (uri);
	dirname = uri_dirname ? go_filename_from_uri (uri_dirname) : NULL;
	dirname = dirname ? g_strconcat ("file://", dirname, NULL) : NULL;
	g_free (uri_dirname);
#endif

	if (brief && dirname &&
	    g_ascii_strncasecmp (dirname, "file:///", 8) == 0) {
		char *temp = g_strdup (dirname + 7);
		g_free (dirname);
		dirname = temp;
	}

	dirname_utf8 = dirname
		? g_filename_to_utf8 (dirname, -1, NULL, NULL, NULL)
		: NULL;
	g_free (dirname);
	return dirname_utf8;
}

/* ------------------------------------------------------------------------- */

static gboolean
is_fd_uri (const char *uri, int *fd)
{
	unsigned long ul;
	char *end;

	if (g_ascii_strncasecmp (uri, "fd://", 5))
		return FALSE;
	uri += 5;
	if (!g_ascii_isdigit (*uri))
		return FALSE;  /* Space, for example.  */

	ul = strtoul (uri, &end, 10);
	if (*end != 0 || ul > INT_MAX)
		return FALSE;

	*fd = (int)ul;
	return TRUE;
}

/* ------------------------------------------------------------------------- */

static GsfInput *
open_plain_file (const char *path, GError **err)
{
	GsfInput *input = gsf_input_mmap_new (path, NULL);
	if (input != NULL)
		return input;
	/* Only report error if stdio fails too */
	return gsf_input_stdio_new (path, err);
}


/**
 * go_file_open :
 * @uri :
 * @err : #GError
 *
 * Try all available methods to open a file or return an error
 **/
GsfInput *
go_file_open (char const *uri, GError **err)
{
	char *filename;
	int fd;

	if (err != NULL)
		*err = NULL;
	g_return_val_if_fail (uri != NULL, NULL);

	if (uri[0] == G_DIR_SEPARATOR) {
		g_warning ("Got plain filename %s in go_file_open.", uri);
		return open_plain_file (uri, err);
	}

	filename = go_filename_from_uri (uri);
	if (filename) {
		GsfInput *result = open_plain_file (filename, err);
		g_free (filename);
		return result;
	}

	if (is_fd_uri (uri, &fd)) {
		int fd2 = dup (fd);
		FILE *fil = fd2 != -1 ? fdopen (fd2, "rb") : NULL;
		GsfInput *result = fil ? gsf_input_stdio_new_FILE (uri, fil, FALSE) : NULL;

		if (!result)
			g_set_error (err, gsf_output_error_id (), 0,
				     "Unable to read from %s", uri);
		return result;
	}

#ifdef WITH_GNOME
	return gsf_input_gnomevfs_new (uri, err);
#else
	g_set_error (err, gsf_input_error (), 0,
		     "Invalid or non-supported URI");
	return NULL;
#endif
}

GsfOutput *
go_file_create (char const *uri, GError **err)
{
	char *filename;
	int fd;

	g_return_val_if_fail (uri != NULL, NULL);

	filename = go_filename_from_uri (uri);
	if (filename) {
		GsfOutput *result = gsf_output_stdio_new (filename, err);
		g_free (filename);
		return result;
	}

	if (is_fd_uri (uri, &fd)) {
		int fd2 = dup (fd);
		FILE *fil = fd2 != -1 ? fdopen (fd2, "wb") : NULL;
		GsfOutput *result = fil ? gsf_output_stdio_new_FILE (uri, fil, FALSE) : NULL;

		if (!result)
			g_set_error (err, gsf_output_error_id (), 0,
				     "Unable to write to %s", uri);
		return result;
	}

#ifdef WITH_GNOME
	return gsf_output_gnomevfs_new (uri, err);
#else
	g_set_error (err, gsf_output_error_id (), 0,
		     "Invalid or non-supported URI");
	return NULL;
#endif
}

/* ------------------------------------------------------------------------- */
/* Adapted from gtkfilechooserdefault.c.  Unfortunately it is static there.  */

GSList *
go_file_split_urls (const char *data)
{
  GSList *uris;
  const char *p, *q;

  uris = NULL;

  p = data;

  /* We don't actually try to validate the URI according to RFC
   * 2396, or even check for allowed characters - we just ignore
   * comments and trim whitespace off the ends.  We also
   * allow LF delimination as well as the specified CRLF.
   *
   * We do allow comments like specified in RFC 2483.
   */
  while (p)
    {
      if (*p != '#')
	{
	  while (g_ascii_isspace (*p))
	    p++;

	  q = p;
	  while (*q && (*q != '\n') && (*q != '\r'))
	    q++;

	  if (q > p)
	    {
	      q--;
	      while (q > p && g_ascii_isspace (*q))
		q--;

	      if (q > p)
		uris = g_slist_prepend (uris, g_strndup (p, q - p + 1));
	    }
	}
      p = strchr (p, '\n');
      if (p)
	p++;
    }

  uris = g_slist_reverse (uris);
  return uris;
}

/* ------------------------------------------------------------------------- */

/*
 * go_url_decode: decode the result of go_url_encode.
 */
gchar*
go_url_decode (gchar const *text)
{
	GString *result;

	g_return_val_if_fail (text != NULL, NULL);
	g_return_val_if_fail (*text != '\0', NULL);

	result = g_string_new (NULL);
	while (*text) {
		unsigned char c = *text++;
		if (c == '%') {
			if (g_ascii_isxdigit (text[0]) && g_ascii_isxdigit (text[1])) {
				g_string_append_c (result,
						   (g_ascii_xdigit_value (text[0]) << 4) |
						   g_ascii_xdigit_value (text[1]));
				text += 2;
			} else {
				/* Bogus.  */
				return g_string_free (result, TRUE);
			}
		} else
			g_string_append_c (result, c);
	}

	return g_string_free (result, FALSE);
}

/**
 * go_url_encode: url-encode a string according to RFC 2368.
 */
gchar*
go_url_encode (gchar const *text)
{
	static const char hex[16] = "0123456789ABCDEF";
	GString* result;

	g_return_val_if_fail (text != NULL, NULL);
	g_return_val_if_fail (*text != '\0', NULL);

	result = g_string_new (NULL);
	while (*text) {
		unsigned char c = *text++;
		switch (c) {
		case '.': case '-': case '_': case '@':
			g_string_append_c (result, c);
			break;
		default:
			if (g_ascii_isalnum (c))
				g_string_append_c (result, c);
			else {
				g_string_append_c (result, '%');
				g_string_append_c (result, hex[c >> 4]);
				g_string_append_c (result, hex[c & 0xf]);
			}
		}
	}

	return g_string_free (result, FALSE);
}

#ifndef WITH_GNOME
static char *
check_program (char const *prog)
{
	if (NULL == prog)
		return NULL;
	if (g_path_is_absolute (prog)) {
		if (!g_file_test (prog, G_FILE_TEST_IS_EXECUTABLE))
			return NULL;
	} else if (!g_find_program_in_path (prog))
		return NULL;
	return g_strdup (prog);
}
#endif

GError *
go_url_show (gchar const *url)
{
#ifdef G_OS_WIN32
	ShellExecute (NULL, "open", url, NULL, NULL, SW_SHOWNORMAL);

	return NULL;
#else
	GError *err = NULL;
#ifdef WITH_GNOME
	gnome_url_show (url, &err);
	return err;
#else
	guint8 *browser = NULL;
	guint8 *clean_url = NULL;

	/* 1) Check BROWSER env var */
	browser = check_program (getenv ("BROWSER"));

	if (browser == NULL) {
		static char const * const browsers[] = {
			"sensible-browser",	/* debian */
			"epiphany",		/* primary gnome */
			"galeon",		/* secondary gnome */
			"encompass",
			"firefox",
			"mozilla-firebird",
			"mozilla",
			"netscape",
			"konqueror",
			"xterm -e w3m",
			"xterm -e lynx",
			"xterm -e links"
		};
		unsigned i;
		for (i = 0 ; i < G_N_ELEMENTS (browsers) ; i++)
			if (NULL != (browser = check_program (browsers[i])))
				break;
  	}

	if (browser != NULL) {
		gint    argc;
		gchar **argv = NULL;
		char   *cmd_line = g_strconcat (browser, " %1", NULL);

		if (g_shell_parse_argv (cmd_line, &argc, &argv, &err)) {
			/* check for '%1' in an argument and substitute the url
			 * otherwise append it */
			gint i;
			char *tmp;

			for (i = 1 ; i < argc ; i++)
				if (NULL != (tmp = strstr (argv[i], "%1"))) {
					*tmp = '\0';
					tmp = g_strconcat (argv[i],
						(clean_url != NULL) ? (char const *)clean_url : url,
						tmp+2, NULL);
					g_free (argv[i]);
					argv[i] = tmp;
					break;
				}

			/* there was actually a %1, drop the one we added */
			if (i != argc-1) {
				g_free (argv[argc-1]);
				argv[argc-1] = NULL;
			}
			g_spawn_async (NULL, argv, NULL, G_SPAWN_SEARCH_PATH,
				NULL, NULL, NULL, &err);
			g_strfreev (argv);
		}
		g_free (cmd_line);
	}
	g_free (browser);
	g_free (clean_url);
	return err;
#endif
#endif
}

/**
 * go_url_check_extension
 * @uri     : Uri
 * @std_ext : Standard extension for the content type
 * @new_uri : New uri
 *
 * Modifies given @uri by adding the extension @std_ext if needed.
 * If no @std_ext is given or @uri already has some extension,
 * it just copies @uri.
 *
 * Value in new_uri:  newly allocated string which you should free after
 *                    use, containing (optionally) modified uri.
 *
 * Return Value:  FALSE if the uri has an extension not matching @std_ext
 */
gboolean
go_url_check_extension (gchar const *uri,
			gchar const *std_ext,
			gchar **new_uri)
{
	gchar *base;
	gchar *user_ext;
	gboolean res;

	g_return_val_if_fail (uri != NULL, FALSE);
	g_return_val_if_fail (new_uri != NULL, FALSE);

	res      = TRUE;
	base     = g_path_get_basename (uri);
	user_ext = strrchr (base, '.');
	if (std_ext != NULL && strlen (std_ext) > 0 && user_ext == NULL)
		*new_uri = g_strconcat (uri, ".", std_ext, NULL);
	else {
		if (user_ext != NULL && std_ext != NULL)
			res = !go_utf8_collate_casefold (user_ext + 1, std_ext);
		*new_uri = g_strdup (uri);
	}
	g_free (base);

	return res;
}

gchar *
go_get_mime_type (gchar const *uri)
{
#ifdef WITH_GNOME
	return gnome_vfs_get_mime_type (uri);
#elif defined(G_OS_WIN32)
	LPWSTR wuri, mime_type;

	wuri = g_utf8_to_utf16 (uri, -1, NULL, NULL, NULL);
	if (wuri &&
	    FindMimeFromData_ (NULL, wuri, NULL, 0, NULL, 0, &mime_type, 0) == NOERROR)
	{
		g_free (wuri);
		return g_utf16_to_utf8 (mime_type, -1, NULL, NULL, NULL);
	}

	g_free (wuri);

	/* We try to determine mime using FindMimeFromData().
	 * However, we are not sure whether the functions will know about
	 * the necessary mime types. In the worst wase we fall back to
	 * "text/plain"
	 */
	return g_strdup ("text/plain");
#else
	return g_strdup ("application/octet-stream");
#endif
}

