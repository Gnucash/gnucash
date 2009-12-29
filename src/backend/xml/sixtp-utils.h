/********************************************************************
 * sixtp-utils.h                                                    *
 * Copyright 2001 Gnumatic, Inc.                                    *
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
 *                                                                  *
 ********************************************************************/

#ifndef SIXTP_UTILS_H
#define SIXTP_UTILS_H

#include "qof.h"

#include "sixtp.h"

typedef struct
{
    Timespec ts;
    guint s_block_count;
    guint ns_block_count;
} TimespecParseInfo;

#define TIMESPEC_TIME_FORMAT  "%Y-%m-%d %H:%M:%S"
#define TIMESPEC_PARSE_TIME_FORMAT  "%Y-%m-%d %H:%M:%S"
#define TIMESPEC_SEC_FORMAT_MAX 256

gboolean isspace_str(const gchar *str, int nomorethan);

gboolean allow_and_ignore_only_whitespace(GSList *sibling_data,
        gpointer parent_data,
        gpointer global_data,
        gpointer *result,
        const char *text,
        int length);

gboolean generic_accumulate_chars(GSList *sibling_data,
                                  gpointer parent_data,
                                  gpointer global_data,
                                  gpointer *result,
                                  const char *text,
                                  int length);


void generic_free_data_for_children(gpointer data_for_children,
                                    GSList* data_from_children,
                                    GSList* sibling_data,
                                    gpointer parent_data,
                                    gpointer global_data,
                                    gpointer *result,
                                    const gchar *tag);

gchar * concatenate_child_result_chars(GSList *data_from_children);

gboolean string_to_double(const char *str, double *result);

gboolean string_to_gint64(const gchar *str, gint64 *v);

gboolean string_to_gint32(const gchar *str, gint32 *v);

gboolean hex_string_to_binary(const gchar *str,  void **v, guint64 *data_len);

gboolean generic_return_chars_end_handler(gpointer data_for_children,
        GSList* data_from_children,
        GSList* sibling_data,
        gpointer parent_data,
        gpointer global_data,
        gpointer *result,
        const gchar *tag);

sixtp* simple_chars_only_parser_new(sixtp_end_handler end_handler);

gboolean string_to_timespec_secs(const gchar *str, Timespec *ts);
gboolean string_to_timespec_nsecs(const gchar *str, Timespec *ts);

/* str must have length of at least TIMESPEC_SEC_FORMAT_MAX */
gboolean timespec_secs_to_given_string (const Timespec *ts, gchar *str);


gboolean generic_timespec_start_handler(GSList* sibling_data,
                                        gpointer parent_data,
                                        gpointer global_data,
                                        gpointer *data_for_children,
                                        gpointer *result,
                                        const gchar *tag, gchar **attrs);

gboolean timespec_parse_ok(TimespecParseInfo *info);

gboolean generic_timespec_secs_end_handler(
    gpointer data_for_children,
    GSList  *data_from_children, GSList *sibling_data,
    gpointer parent_data, gpointer global_data,
    gpointer *result, const gchar *tag);

gboolean generic_timespec_nsecs_end_handler(
    gpointer data_for_children,
    GSList  *data_from_children, GSList *sibling_data,
    gpointer parent_data, gpointer global_data,
    gpointer *result, const gchar *tag);


sixtp* generic_timespec_parser_new(sixtp_end_handler end_handler);

gboolean generic_guid_end_handler(
    gpointer data_for_children,
    GSList  *data_from_children, GSList *sibling_data,
    gpointer parent_data, gpointer global_data,
    gpointer *result, const gchar *tag);

sixtp* generic_guid_parser_new(void);

gboolean generic_gnc_numeric_end_handler(
    gpointer data_for_children,
    GSList  *data_from_children, GSList *sibling_data,
    gpointer parent_data, gpointer global_data,
    gpointer *result, const gchar *tag);

sixtp* generic_gnc_numeric_parser_new(void);

sixtp* restore_char_generator(sixtp_end_handler ender);



#endif /* _SIXTP_UTILS_H_ */
