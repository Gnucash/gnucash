/********************************************************************\
 * QuickFill.h -- the quickfill tree data structure                 *
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
\********************************************************************/
/** @addtogroup GUI
@{
*/
/** @addtogroup QuickFill

   QuickFill is meant to be used by the GUI to auto-complete
   (e.g. tab-complete) typed user input.
   QuickFill is implemented as a hierarchical tree
   of partial matching strings.  The root of the tree contains
   all of the strings that user input should be matched to.
   Then, given a short string segment, QuickFill will return
   a subtree containing only those strings that start with desired
   substring.  As additional letters are added to the substring,
   QuickFill will thus narrow down to the unique matching string
   (or to nothing if no match).

   QuickFill works with national-language i18n'ed/l10n'ed multi-byte
   and wide-char strings, as well as plain-old C-locale strings.
   @{
*/
/**
   @file QuickFill.h
   @brief QuickFill is used to auto-complete typed user entries.
   @author Copyright (C) 1997 Robin D. Clark
   @author Copyright (C) 1998,2004 Linas Vepstas <linas@linas.org>
   @author Copyright (C) 2000 Dave Peticolas
 */

#ifndef QUICKFILL_H
#define QUICKFILL_H

#include <gdk/gdk.h>
#include <glib.h>

typedef enum
{
  QUICKFILL_LIFO,
  QUICKFILL_ALPHA
} QuickFillSort;

typedef struct _QuickFill QuickFill;


/* PROTOTYPES ******************************************************/

QuickFill *  gnc_quickfill_new (void);
void         gnc_quickfill_destroy (QuickFill *qf);
void         gnc_quickfill_purge (QuickFill *qf);

/** For the given node 'qf', return the best-guess matching string.
 */
const char * gnc_quickfill_string (QuickFill *qf);

/** Return the subnode of the tree whose strings all hold 'wc' as
 *  the next letter.  That is, if 'qf' holds all strings starting
 *  with the letter 'a', and we ask for the letter 'b', then this
 *  routine will return the node holding all strings that start
 *  with "ab".
 *
 *  The best-guess matching string can be retrieved with
 *  gnc_quickfill_string().
 */
QuickFill *  gnc_quickfill_get_char_match (QuickFill *qf, gunichar c);

/** Return a subnode in the tree whose strings all match the
 *  string 'str' as the next substring.  Thus, for example, if
 *  the argument 'qf' holds strings that start with "abc", and
 *  this routine is called with "def", then the returned node
 *  will hold strings that start with "abcdef".
 *
 *  The best-guess matching string can be retrieved with
 *  gnc_quickfill_string().
 *
 *  To convert a plain C-locale char * string to GdkWChar *,
 *  use the gnc_mbstowcs() routine.
 */
QuickFill *  gnc_quickfill_get_string_match (QuickFill *qf,
                                             const char *str);

/** Same as gnc_quickfill_get_string_match(), except that the
 *  string length is explicitly specified.
 */
QuickFill *  gnc_quickfill_get_string_len_match (QuickFill *qf,
                                                 const char *str, int len);

/** Walk a 'unique' part of the QuickFill tree.  This routine is
 *  typically used to assist in the tab-completion of strings.
 *  If the initial portion of the string is unique, but some later
 *  portion is not, this routine will advance to the first non-unique
 *  part of the string.  If len is non-NULL, then *len will be set
 *  to the length of the unique portion of the string.
 *
 *  Thus, for example, if the root node contains the strings
 *  "The Book" and "The Movie", then the returned len will be 4,
 *  and the returned node will distinguish "Book" and "Movie".
 *  Thus, for example, gnc_quickfill_get_char_match(.., 'B') on
 *  the result will identify "The Book".
 */
QuickFill *  gnc_quickfill_get_unique_len_match (QuickFill *qf, int *len);

/** Add the string "text" to the collection of searchable strings. */
void         gnc_quickfill_insert (QuickFill *root, const char *text,
                                   QuickFillSort sort_code);

void         gnc_quickfill_remove (QuickFill *root, const gchar *text,
                                   QuickFillSort sort_code);

/** @} */
/** @} */
#endif /* QUICKFILL_H */
