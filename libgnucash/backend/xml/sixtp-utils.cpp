/********************************************************************
 * sixtp-utils.c                                                    *
 * Copyright (c) 2001 Gnumatic, Inc.                                *
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

#define __EXTENSIONS__
#include <guid.hpp>
extern "C"
{
#include <config.h>

#include <ctype.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <time.h>
#include <errno.h>

#ifndef HAVE_STRPTIME
#include "strptime.h"
#endif
#include <gnc-date.h>
}

#include "sixtp.h"
#include "sixtp-utils.h"

static QofLogModule log_module = GNC_MOD_IO;

gboolean
isspace_str (const gchar* str, int nomorethan)
{
    const gchar* cursor = str;
    while (*cursor && (nomorethan != 0))
    {
        if (!isspace (*cursor))
        {
            return (FALSE);
        }
        cursor++;
        nomorethan--;
    }
    return (TRUE);
}

gboolean
allow_and_ignore_only_whitespace (GSList* sibling_data,
                                  gpointer parent_data,
                                  gpointer global_data,
                                  gpointer* result,
                                  const char* text,
                                  int length)
{
    return (isspace_str (text, length));
}

gboolean
generic_accumulate_chars (GSList* sibling_data,
                          gpointer parent_data,
                          gpointer global_data,
                          gpointer* result,

                          const char* text,
                          int length)
{
    gchar* copytxt = g_strndup (text, length);
    g_return_val_if_fail (result, FALSE);

    *result = copytxt;
    return (TRUE);
}


void
generic_free_data_for_children (gpointer data_for_children,
                                GSList* data_from_children,
                                GSList* sibling_data,
                                gpointer parent_data,
                                gpointer global_data,
                                gpointer* result,
                                const gchar* tag)
{
    if (data_for_children) g_free (data_for_children);
}

gchar*
concatenate_child_result_chars (GSList* data_from_children)
{
    GSList* lp;
    gchar* name = g_strdup ("");

    g_return_val_if_fail (name, NULL);

    /* child data lists are in reverse chron order */
    data_from_children = g_slist_reverse (g_slist_copy (data_from_children));

    for (lp = data_from_children; lp; lp = lp->next)
    {
        sixtp_child_result* cr = (sixtp_child_result*) lp->data;
        if (cr->type != SIXTP_CHILD_RESULT_CHARS)
        {
            PERR ("result type is not chars");
            g_slist_free (data_from_children);
            g_free (name);
            return (NULL);
        }
        else
        {
            char* temp;
            temp = g_strconcat (name, (gchar*) cr->data, NULL);
            g_free (name);
            name = temp;
        }
    }
    g_slist_free (data_from_children);
    return (name);
}

/****************************************************************************/
/* string to data converters...
 */


/*********/
/* double
 */

gboolean
string_to_double (const char* str, double* result)
{
    char* endptr = 0x0;

    g_return_val_if_fail (str, FALSE);
    g_return_val_if_fail (result, FALSE);

    *result = strtod (str, &endptr);
    if (endptr == str) return (FALSE);

    return (TRUE);
}

/*********/
/* gint64
 */
/* Maybe there should be a comment here explaining why this function
   doesn't call g_ascii_strtoull, because it's not so obvious. -CAS */
gboolean
string_to_gint64 (const gchar* str, gint64* v)
{
    /* convert a string to a gint64. only whitespace allowed before and after. */
    long long int v_in;
    int num_read;

    g_return_val_if_fail (str, FALSE);

    /* must use "<" here because %n's effects aren't well defined */
    if (sscanf (str, " " QOF_SCANF_LLD "%n", &v_in, &num_read) < 1)
    {
        return (FALSE);
    }

    /*
     * Mac OS X version 10.1 and under has a silly bug where scanf
     * returns bad values in num_read if there is a space before %n. It
     * is fixed in the next release 10.2 afaik
     */
    while ((* ((gchar*)str + num_read) != '\0') &&
           isspace (* ((unsigned char*)str + num_read)))
        num_read++;

    if (v)
        *v = v_in;

    if (!isspace_str (str + num_read, -1)) return (FALSE);
    return (TRUE);
}

/*********/
/* gint32
 */

gboolean
string_to_gint32 (const gchar* str, gint32* v)
{
    /* convert a string to a gint32. only whitespace allowed before and after. */
    int num_read;
    int v_in;

    /* must use "<" here because %n's effects aren't well defined */
    if (sscanf (str, " %d%n", &v_in, &num_read) < 1)
    {
        return (FALSE);
    }
    while ((* ((gchar*)str + num_read) != '\0') &&
           isspace (* ((unsigned char*)str + num_read)))
        num_read++;

    if (v)
        *v = v_in;

    if (!isspace_str (str + num_read, -1)) return (FALSE);
    return (TRUE);
}

/************/
/* hex string
 */

gboolean
hex_string_to_binary (const gchar* str,  void** v, guint64* data_len)
{
    /* Convert a hex string to binary.  No whitespace allowed. */
    const gchar* cursor = str;
    guint64 str_len;
    gboolean error = FALSE;

    g_return_val_if_fail (str, FALSE);
    g_return_val_if_fail (v, FALSE);
    g_return_val_if_fail (data_len, FALSE);

    str_len = strlen (str);
    /* Since no whitespace is allowed and hex encoding is 2 text chars
       per binary char, the result must be half the input size and the
       input size must be even. */
    if ((str_len % 2) != 0) return (FALSE);
    *data_len = 0;
    *v = g_new0 (char, str_len / 2);

    g_return_val_if_fail (*v, FALSE);

    while (*cursor && * (cursor + 1))
    {
        gchar tmpstr[2];
        int tmpint;

        if (isspace (*cursor) || isspace (* (cursor + 1)))
        {
            error = TRUE;
        }
        else
        {
            int num_read;
            tmpstr[0] = *cursor;
            tmpstr[0] = * (cursor + 1);

            if ((sscanf (tmpstr, "%x%n", &tmpint, &num_read) < 1)
                || (num_read != 2))
            {
                error = TRUE;
            }
            else
            {
                * ((gchar*) (v + *data_len)) = tmpint;
                *data_len += 1;
                cursor += 2;
            }
        }
    }

    if (error || (*data_len != (str_len / 2)))
    {
        g_free (*v);
        *v = NULL;
        *data_len = 0;
        return (FALSE);
    }

    return (TRUE);
}

/***************************************************************************/
/* simple chars only parser - just grabs all it's contained chars and
   does what you specify in the end handler - if you pass NULL as the
   end handler to simple_chars_only_parser_new, the characters are just
   passed to the parent as a new string.

   input: NA
   returns: gchar array allocated via g_new, etc.

   start: NA
   chars: generic_accumulate_chars.
   end: varies - default is to concatenate all accumulated chars and return.

   cleanup-result: g_free (for chars)
   cleanup-chars: g_free (for chars)
   fail: NA
   result-fail: g_free (for chars)
   chars-fail: g_free (for chars)

 */

gboolean
generic_return_chars_end_handler (gpointer data_for_children,
                                  GSList* data_from_children,
                                  GSList* sibling_data,
                                  gpointer parent_data,
                                  gpointer global_data,
                                  gpointer* result,
                                  const gchar* tag)
{
    gchar* txt = NULL;

    txt = concatenate_child_result_chars (data_from_children);
    g_return_val_if_fail (txt, FALSE);
    *result = txt;
    return (TRUE);
}


sixtp*
simple_chars_only_parser_new (sixtp_end_handler end_handler)
{
    return sixtp_set_any (
               sixtp_new (), FALSE,
               SIXTP_END_HANDLER_ID, (end_handler
                                      ? end_handler
                                      : generic_return_chars_end_handler),
               SIXTP_CHARACTERS_HANDLER_ID, generic_accumulate_chars,
               SIXTP_CLEANUP_RESULT_ID, sixtp_child_free_data,
               SIXTP_CLEANUP_CHARS_ID, sixtp_child_free_data,
               SIXTP_RESULT_FAIL_ID, sixtp_child_free_data,
               SIXTP_CHARS_FAIL_ID, sixtp_child_free_data,
               SIXTP_NO_MORE_HANDLERS);
}

/****************************************************************************/
/* generic timespec handler for XML Version 1 files.

   A collection of node functions intended to parse a sub-node set
   that looks like this:

     <date-posted>
       <s>Mon, 05 Jun 2000 23:16:19 -0500</s>
       <ns>658864000</ns>
     </date-posted>

   and produce a time64.  The start handler for the top allocates
   the time64 and passes it to the children.  The <s> block sets
   the seconds and the <ns> block is ignored.  If
   all goes well, returns the time64 as the result.
*/

/* Top level timespec node:

   input: user end handler *
   returns: time64

   start: Allocates Time64ParseInfo* for data_for_children.
   characters: none (whitespace only).
   end: g_free Time64ParseInfo + any other actions

   cleanup-result: NA
   cleanup-chars: NA
   fail: g_free data_for_children.
   result-fail: g_free data_for_children.
   chars-fail: NA

 */

gboolean
generic_timespec_start_handler (GSList* sibling_data, gpointer parent_data,
                                gpointer global_data,
                                gpointer* data_for_children, gpointer* result,
                                const gchar* tag, gchar** attrs)
{
    Time64ParseInfo* tsp = g_new0 (Time64ParseInfo, 1);
    g_return_val_if_fail (tsp, FALSE);
    *data_for_children = tsp;
    return (TRUE);
}

/* You can't use this function directly.  You have to call it from
   your own end handler.  If it returns TRUE, *result will contain the
   new timespec.  Otherwise, you can presume that everything's been
   cleaned up properly and return FALSE.  */
gboolean
timespec_parse_ok (Time64ParseInfo* info)
{
    return info->s_block_count != 1;
}

/* generic_timespec_end_handler - must be customized and provided by
   the user. */

/* <s> (parent timespec-node)

   input: Time64ParseInfo *
   returns: NA

   start: NA
   characters: accumulate.
   end: convert characters to secs part of input time64 and inc s_block_count.

   cleanup-result: NA
   cleanup-chars: g_free data.
   fail: NA
   result-fail: NA
   chars-fail: g_free data.

 */

gboolean
generic_timespec_secs_end_handler (gpointer data_for_children,
                                   GSList*  data_from_children, GSList* sibling_data,
                                   gpointer parent_data, gpointer global_data,
                                   gpointer* result, const gchar* tag)
{
    gchar* txt = NULL;
    Time64ParseInfo* info = (Time64ParseInfo*) parent_data;

    g_return_val_if_fail (parent_data, FALSE);

    txt = concatenate_child_result_chars (data_from_children);
    g_return_val_if_fail (txt, FALSE);

    info->time = gnc_iso8601_to_time64_gmt (txt);
    g_free (txt);

// gnc_iso8601_to_time64_gmt returns INT64_MAX on failure.
    g_return_val_if_fail (info->time < INT64_MAX, FALSE);

    info->s_block_count++;
    return (TRUE);
}

/* <s> (parent timespec-node)

   input: Time64ParseInfo *
   returns: NA

   start: NA
   characters: accumulate.
   end: NA

   cleanup-result: NA
   cleanup-chars: NA.
   fail: NA
   result-fail: NA
   chars-fail: g_free data.

 */

gboolean
generic_timespec_nsecs_end_handler (gpointer data_for_children,
                                    GSList*  data_from_children, GSList* sibling_data,
                                    gpointer parent_data, gpointer global_data,
                                    gpointer* result, const gchar* tag)
{
    return TRUE;
}

static sixtp*
timespec_sixtp_new (sixtp_end_handler ender)
{
    return sixtp_set_any (
               sixtp_new (), FALSE,
               SIXTP_CHARACTERS_HANDLER_ID, generic_accumulate_chars,
               SIXTP_END_HANDLER_ID, ender,
               SIXTP_CLEANUP_CHARS_ID, sixtp_child_free_data,
               SIXTP_CHARS_FAIL_ID, sixtp_child_free_data,
               SIXTP_NO_MORE_HANDLERS);
}

sixtp*
generic_timespec_parser_new (sixtp_end_handler end_handler)
{
    sixtp* top_level =
        sixtp_set_any (sixtp_new (), FALSE,
                       SIXTP_START_HANDLER_ID, generic_timespec_start_handler,
                       SIXTP_CHARACTERS_HANDLER_ID, allow_and_ignore_only_whitespace,
                       SIXTP_END_HANDLER_ID, end_handler,
                       SIXTP_CLEANUP_RESULT_ID, sixtp_child_free_data,
                       SIXTP_FAIL_HANDLER_ID, generic_free_data_for_children,
                       SIXTP_RESULT_FAIL_ID, sixtp_child_free_data,
                       SIXTP_NO_MORE_HANDLERS);
    g_return_val_if_fail (top_level, NULL);

    if (!sixtp_add_some_sub_parsers (
            top_level, TRUE,
            "s", timespec_sixtp_new (generic_timespec_secs_end_handler),
            "ns", timespec_sixtp_new (generic_timespec_nsecs_end_handler),
            NULL, NULL))
    {
        return NULL;
    }

    return (top_level);
}

/****************************************************************************/
/* <?> generic guid handler...

   Attempts to parse the current accumulated characters data as a guid
   and return it.

   input: NA
   returns: GncGUID*

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and create and return GncGUID*, if possible.

   cleanup-result: g_free the GncGUID*
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: g_free the GncGUID*
   chars-fail: g_free the result string.

 */

gboolean
generic_guid_end_handler (gpointer data_for_children,
                          GSList*  data_from_children, GSList* sibling_data,
                          gpointer parent_data, gpointer global_data,
                          gpointer* result, const gchar* tag)
{
    gchar* txt = NULL;
    GncGUID* gid;
    gboolean ok;

    txt = concatenate_child_result_chars (data_from_children);
    g_return_val_if_fail (txt, FALSE);

    gid = g_new (GncGUID, 1);
    if (!gid)
    {
        g_free (txt);
        return (FALSE);
    }

    ok = string_to_guid (txt, gid);
    g_free (txt);

    if (!ok)
    {
        PERR ("couldn't parse GncGUID");
        g_free (gid);
        return (FALSE);
    }

    *result = gid;
    return (TRUE);
}

sixtp*
generic_guid_parser_new (void)
{
    return sixtp_set_any (
               sixtp_new (), FALSE,
               SIXTP_CHARACTERS_HANDLER_ID, generic_accumulate_chars,
               SIXTP_CLEANUP_CHARS_ID, sixtp_child_free_data,
               SIXTP_CHARS_FAIL_ID, sixtp_child_free_data,
               SIXTP_END_HANDLER_ID, generic_guid_end_handler,
               SIXTP_RESULT_FAIL_ID, sixtp_child_free_data,
               SIXTP_CLEANUP_RESULT_ID, sixtp_child_free_data,
               SIXTP_NO_MORE_HANDLERS);
}

/****************************************************************************/
/* <?> generic gnc_numeric handler...

   Attempts to parse the current accumulated characters data as a
   gnc_numeric and return it.

   input: NA
   returns: gnc_numeric*

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and create and return gnc_numeric*, if possible.

   cleanup-result: g_free the gnc_numeric*
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: g_free the gnc_numeric*
   chars-fail: g_free the result string.

 */

gboolean
generic_gnc_numeric_end_handler (gpointer data_for_children,
                                 GSList*  data_from_children, GSList* sibling_data,
                                 gpointer parent_data, gpointer global_data,
                                 gpointer* result, const gchar* tag)
{
    gnc_numeric* num = NULL;
    gchar* txt = NULL;
    gboolean ok = FALSE;

    txt = concatenate_child_result_chars (data_from_children);

    if (txt)
    {
        num = g_new (gnc_numeric, 1);
        if (num)
        {
            if (string_to_gnc_numeric (txt, num))
            {
                ok = TRUE;
                *result = num;
            }
        }
    }

    g_free (txt);
    if (!ok)
    {
        PERR ("couldn't parse numeric quantity");
        g_free (num);
    }

    return (ok);
}

sixtp*
generic_gnc_numeric_parser_new (void)
{
    return sixtp_set_any (
               sixtp_new (), FALSE,
               SIXTP_CHARACTERS_HANDLER_ID, generic_accumulate_chars,
               SIXTP_CLEANUP_CHARS_ID, sixtp_child_free_data,
               SIXTP_CHARS_FAIL_ID, sixtp_child_free_data,
               SIXTP_END_HANDLER_ID, generic_gnc_numeric_end_handler,
               SIXTP_RESULT_FAIL_ID, sixtp_child_free_data,
               SIXTP_CLEANUP_RESULT_ID, sixtp_child_free_data,
               SIXTP_NO_MORE_HANDLERS);
}

/***************************************************************************/

sixtp*
restore_char_generator (sixtp_end_handler ender)
{
    return sixtp_set_any (
               sixtp_new (), FALSE,
               SIXTP_CHARACTERS_HANDLER_ID, generic_accumulate_chars,
               SIXTP_END_HANDLER_ID, ender,
               SIXTP_CLEANUP_CHARS_ID, sixtp_child_free_data,
               SIXTP_CHARS_FAIL_ID, sixtp_child_free_data,
               SIXTP_NO_MORE_HANDLERS);
}

/***************************** END OF FILE *********************************/
