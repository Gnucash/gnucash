/* **************************************************************************
 *            qoflog.c
 *
 *  Mon Nov 21 14:41:59 2005
 *  Author: Rob Clark (rclark@cs.hmc.edu)
 *  Copyright (C) 1997-2003 Linas Vepstas <linas@linas.org>
 *  Copyright  2005  Neil Williams <linux@codehelp.co.uk>
 *  Copyright 2007 Joshua Sled <jsled@asynchronous.org>
 *************************************************************************** */

/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301,  USA
 */

extern "C"
{
#include <config.h>

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <glib.h>
#include <glib/gstdio.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#else
# ifdef __GNUC__
#  warning "<unistd.h> required."
# endif
#endif
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "qof.log"
}

#include "qof.h"
#include "qoflog.h"

#define QOF_LOG_MAX_CHARS 50
#define QOF_LOG_MAX_CHARS_WITH_ALLOWANCE 100
#define QOF_LOG_INDENT_WIDTH 4
#define NUM_CLOCKS 10

static FILE *fout = NULL;
static gchar* function_buffer = NULL;
static gint qof_log_num_spaces = 0;
static GHashTable *log_table = NULL;
static GLogFunc previous_handler = NULL;
static gchar* qof_logger_format = NULL;

void
qof_log_indent(void)
{
    qof_log_num_spaces += QOF_LOG_INDENT_WIDTH;
}

void
qof_log_dedent(void)
{
    qof_log_num_spaces
    = (qof_log_num_spaces < QOF_LOG_INDENT_WIDTH)
      ? 0
      : qof_log_num_spaces - QOF_LOG_INDENT_WIDTH;
}

void
qof_log_set_file(FILE *outfile)
{
    if (!outfile)
    {
        fout = stderr;
        return;
    }
    fout = outfile;
}

void
qof_log_init(void)
{
    qof_log_init_filename(NULL);
}

static void
log4glib_handler(const gchar     *log_domain,
                 GLogLevelFlags  log_level,
                 const gchar     *message,
                 gpointer        user_data)
{
    QofLogLevel level = static_cast<QofLogLevel>(log_level);
    if (G_LIKELY(!qof_log_check(log_domain, level)))
        return;

    {
        char timestamp_buf[10];
        time64 now;
        struct tm now_tm;
        const char *format_24hour =
#ifdef G_OS_WIN32
            "%H:%M:%S"
#else
            "%T"
#endif
            ;
        const char *level_str = qof_log_level_to_string(level);
        now = gnc_time (NULL);
        gnc_localtime_r (&now, &now_tm);
        qof_strftime(timestamp_buf, 9, format_24hour, &now_tm);

        fprintf(fout, qof_logger_format,
                timestamp_buf,
                5, level_str,
                (log_domain == NULL ? "" : log_domain),
                qof_log_num_spaces, "",
                message,
                (g_str_has_suffix(message, "\n") ? "" : "\n"));
        fflush(fout);
    }

    /* chain?  ignore?  Only chain if it's going to be quiet...
    else
    {
         // chain
         previous_handler(log_domain, log_level, message, NULL);
    }
    */
}

void
qof_log_init_filename(const gchar* log_filename)
{
    gboolean warn_about_missing_permission = FALSE;
    if (log_table == NULL)
        log_table = g_hash_table_new_full(g_str_hash, g_str_equal,
                                          g_free, NULL);

    if (!qof_logger_format)
        qof_logger_format = g_strdup ("* %s %*s <%s> %*s%s%s"); //default format

    if (log_filename)
    {
        int fd;
        gchar *fname;

        if (fout != NULL && fout != stderr && fout != stdout)
            fclose(fout);

        fname = g_strconcat(log_filename, ".XXXXXX.log", NULL);

        if ((fd = g_mkstemp(fname)) != -1)
        {
#if PLATFORM(WINDOWS)
            /* MSVC compiler: Somehow the OS thinks file descriptor from above
             * still isn't open. So we open normally with the file name and that's it. */
            fout = g_fopen(fname, "wb");
#else
            /* We must not overwrite /dev/null */
            g_assert(g_strcmp0(log_filename, "/dev/null") != 0);

            /* Windows prevents renaming of open files, so the next command silently fails there
             * No problem, the filename on Winows will simply have the random characters */
            g_rename(fname, log_filename);
            fout = fdopen(fd, "w");
#endif
            if (!fout)
                warn_about_missing_permission = TRUE;
        }
        else
        {
            warn_about_missing_permission = TRUE;
            fout = stderr;
        }
        g_free(fname);
    }

    if (!fout)
        fout = stderr;

    // @@fixme really, the userdata is a struct { log_table, fout, previous_handler }
    if (previous_handler == NULL)
        previous_handler = g_log_set_default_handler(log4glib_handler, log_table);

    if (warn_about_missing_permission)
    {
        g_critical("Cannot open log output file \"%s\", using stderr.", log_filename);
    }
}

void
qof_log_shutdown (void)
{
    if (fout && fout != stderr && fout != stdout)
    {
        fclose(fout);
        fout = NULL;
    }

    if (function_buffer)
    {
        g_free(function_buffer);
        function_buffer = NULL;
    }

    if (log_table != NULL)
    {
        g_hash_table_destroy(log_table);
        log_table = NULL;
    }

    if (previous_handler != NULL)
    {
        g_log_set_default_handler(previous_handler, NULL);
        previous_handler = NULL;
    }
}

void
qof_log_set_level(QofLogModule log_module, QofLogLevel level)
{
    if (!log_module || level == 0)
    {
        return;
    }
    if (!log_table)
    {
        log_table = g_hash_table_new(g_str_hash, g_str_equal);
    }
    g_hash_table_insert(log_table, g_strdup((gchar*)log_module), GINT_TO_POINTER((gint)level));
}

const char *
qof_log_prettify (const char *name)
{
    gchar *p, *buffer, *begin;
    gint length;

    if (!name)
    {
        return "";
    }
/* Clang's __func__ displays the whole signature, like a good C++
 * compier should. Gcc displays only the name of the function. Strip
 * the extras from Clang's output so that log messages are the same
 * regardless of compiler.
 */
    buffer = g_strndup(name, QOF_LOG_MAX_CHARS_WITH_ALLOWANCE - 1);
    length = strlen(buffer);
    p = g_strstr_len (buffer, length, "(");
    if (p) *p = '\0';
    begin = g_strrstr (buffer, "*");
    if (begin == NULL)
        begin = g_strrstr (buffer, " ");
    else if (* (begin + 1) == ' ')
        ++ begin;
    if (begin != NULL)
        p = begin + 1;
    else
        p = buffer;

    if (function_buffer)
        g_free(function_buffer);
    function_buffer = g_strdup(p);
    g_free(buffer);
    return function_buffer;
}

void
qof_log_init_filename_special(const char *log_to_filename)
{
    if (g_ascii_strcasecmp("stderr", log_to_filename) == 0)
    {
        qof_log_init();
        qof_log_set_file(stderr);
    }
    else if (g_ascii_strcasecmp("stdout", log_to_filename) == 0)
    {
        qof_log_init();
        qof_log_set_file(stdout);
    }
    else
    {
        qof_log_init_filename(log_to_filename);
    }
}

void
qof_log_parse_log_config(const char *filename)
{
    const gchar *levels_group = "levels", *output_group = "output";
    GError *err = NULL;
    GKeyFile *conf = g_key_file_new();

    if (!g_key_file_load_from_file(conf, filename, G_KEY_FILE_NONE, &err))
    {
        g_warning("unable to parse [%s]: %s", filename, err->message);
        g_error_free(err);
        return;
    }

    g_debug("parsing log config from [%s]", filename);
    if (g_key_file_has_group(conf, levels_group))
    {
        gsize num_levels;
        unsigned int key_idx;
        gchar **levels;
        gint logger_max_name_length = 12;
        gchar *str = NULL;

        levels = g_key_file_get_keys(conf, levels_group, &num_levels, NULL);

        for (key_idx = 0; key_idx < num_levels && levels[key_idx] != NULL; key_idx++)
        {
            QofLogLevel level;
            gchar *logger_name = NULL, *level_str = NULL;

            logger_name = g_strdup(levels[key_idx]);
            logger_max_name_length = MAX (logger_max_name_length, (gint) strlen (logger_name));
            level_str = g_key_file_get_string(conf, levels_group, logger_name, NULL);
            level = qof_log_level_from_string(level_str);

            g_debug("setting log [%s] to level [%s=%d]", logger_name, level_str, level);
            qof_log_set_level(logger_name, level);

            g_free(logger_name);
            g_free(level_str);
        }

        str = g_strdup_printf ("%d", logger_max_name_length);
        if (qof_logger_format)
            g_free (qof_logger_format);
        qof_logger_format = g_strconcat ("* %s %*s <%-", str, ".", str, "s> %*s%s%s", NULL);

        g_free (str);
        g_strfreev(levels);
    }

    if (g_key_file_has_group(conf, output_group))
    {
        gsize num_outputs;
        unsigned int output_idx;
        gchar **outputs;

        outputs = g_key_file_get_keys(conf, output_group, &num_outputs, NULL);
        for (output_idx = 0; output_idx < num_outputs && outputs[output_idx] != NULL; output_idx++)
        {
            gchar *key = outputs[output_idx];
            gchar *value;

            if (g_ascii_strcasecmp("to", key) != 0)
            {
                g_warning("unknown key [%s] in [outputs], skipping", key);
                continue;
            }

            value = g_key_file_get_string(conf, output_group, key, NULL);
            g_debug("setting [output].to=[%s]", value);
            qof_log_init_filename_special(value);
            g_free(value);
        }
        g_strfreev(outputs);
    }

    g_key_file_free(conf);
}

gboolean
qof_log_check(QofLogModule log_domain, QofLogLevel log_level)
{
//#define _QLC_DBG(x) x
#define _QLC_DBG(x)
    GHashTable *log_levels = log_table;
    gchar *domain_copy = g_strdup(log_domain == NULL ? "" : log_domain);
    gchar *dot_pointer = domain_copy;
    static const QofLogLevel default_log_thresh = QOF_LOG_WARNING;
    QofLogLevel longest_match_level = default_log_thresh;

    {
        gpointer match_level;
        if (log_levels && (match_level = g_hash_table_lookup(log_levels, "")) != NULL)
            longest_match_level = (QofLogLevel)GPOINTER_TO_INT(match_level);
    }

_QLC_DBG( { printf("trying [%s] (%d):", log_domain, log_levels != NULL ? g_hash_table_size(log_levels) : 0); });
    if (G_LIKELY(log_levels))
    {
        // e.g., "a.b.c\0" -> "a\0b.c\0" -> "a.b\0c\0", "a.b.c\0"
        gpointer match_level;
        while ((dot_pointer = g_strstr_len(dot_pointer, strlen(dot_pointer), ".")) != NULL)
        {
            *dot_pointer = '\0';
            _QLC_DBG( { printf(" [%s]", domain_copy); });
            if (g_hash_table_lookup_extended(log_levels, domain_copy, NULL, &match_level))
            {
                longest_match_level = (QofLogLevel)GPOINTER_TO_INT(match_level);
                _QLC_DBG(printf("*"););
            }
            *dot_pointer = '.';
            dot_pointer++;
        }

        _QLC_DBG( { printf(" [%s]", domain_copy); });
        if (g_hash_table_lookup_extended(log_levels, domain_copy, NULL, &match_level))
        {
            longest_match_level = (QofLogLevel)GPOINTER_TO_INT(match_level);
            _QLC_DBG( { printf("*"); });
        }
    }
    _QLC_DBG( { printf(" found [%d]\n", longest_match_level); });
    g_free(domain_copy);

    return log_level <= longest_match_level;
}

void
qof_log_set_default(QofLogLevel log_level)
{
    qof_log_set_level("", log_level);
    qof_log_set_level("qof", log_level);
}

const gchar*
qof_log_level_to_string(QofLogLevel log_level)
{
    const char *level_str;
    switch (log_level)
    {
    case QOF_LOG_FATAL:
        level_str = "FATAL";
        break;
    case QOF_LOG_ERROR:
        level_str = "ERROR";
        break;
    case QOF_LOG_WARNING:
        level_str = "WARN";
        break;
    case QOF_LOG_MESSAGE:
        level_str = "MESSG";
        break;
    case QOF_LOG_INFO:
        level_str = "INFO";
        break;
    case QOF_LOG_DEBUG:
        level_str = "DEBUG";
        break;
    default:
        level_str = "OTHER";
        break;
    }
    return level_str;
}

QofLogLevel
qof_log_level_from_string(const gchar *str)
{
    if (g_ascii_strncasecmp("error", str, 5) == 0) return QOF_LOG_FATAL;
    if (g_ascii_strncasecmp("crit", str, 4) == 0) return QOF_LOG_ERROR;
    if (g_ascii_strncasecmp("warn", str, 4) == 0) return QOF_LOG_WARNING;
    if (g_ascii_strncasecmp("mess", str, 4) == 0) return QOF_LOG_MESSAGE;
    if (g_ascii_strncasecmp("info", str, 4) == 0) return QOF_LOG_INFO;
    if (g_ascii_strncasecmp("debug", str, 5) == 0) return QOF_LOG_DEBUG;
    return QOF_LOG_DEBUG;
}
