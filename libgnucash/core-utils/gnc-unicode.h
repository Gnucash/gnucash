/********************************************************************
 * gnc-icu-locale.h -- Localization with ICU.                        *
 *                                                                  *
 * Copyright (C) 2025 John Ralls <jralls@ceridwen.us                *
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
 * along with this program; if not, see                             *
 * <https://www.gnu.org/licenses/>                                  *
 ********************************************************************/

#pragma once
#include <stdbool.h>

/** @addtogroup Localization These functions perform string comparison
    and collation according to the Unicode Common Locale Data
    Repository rules. The CLDR specifies five levels of
    comparison.

    - The primary or base level considers all variant codepoints
    representing a character to be equivalent regardless of case or
    decorations like accents and vowel or tone marks.
    - The secondary level differentiates between letters with
    decorations but still ignores case.
    - The tertiary level differentiates based on case, decorations,
    and variants, for example A and Ⓐ.
    - The Quaternary level differentiates words with punctuation, for
    example "ab" and "a-b".
    - Identical differentiates all codepoints with no implicit
      normalization so a character constructed with combining marks
      will compare different from the same character represented as a
      single codepoint.
*/
#ifdef __cplusplus
extern "C"
{
#endif

/** Find the first Unicode-equivalent UTF-8-encoded substring in a
 * UTF-8-encoded string comparing characters at the CLDR primary
 * level, setting the starting position and length of the matching
 * part of the string.
 * @param needle The substring to search for
 * @param haystack the string to search in
 * @param output the position of needle in haystack
 * @param output the length of the match
 * @return true if needle is found in haystack
 */
bool gnc_unicode_has_substring_base_chars(const char* needle, const
                                          char* haystack, int* position,
                                          int* length);

/** Find the first Unicode-equivalent UTF-8-encoded substring in a
 * UTF-8-encoded string comparing characters at the CLDR secondary
 * level, setting the starting position and length of the matching
 * part of the string.
 * @param needle The substring to search for
 * @param haystack the string to search in
 * @param output the position of needle in haystack
 * @param output the length of the match
 * @return true if needle is found in haystack
 */
bool gnc_unicode_has_substring_accented_chars(const char* needle, const
                                              char* haystack, int* position,
                                              int* length);

/** Find the first Unicode-equivalent UTF-8-encoded substring in a
 * UTF-8-encoded string comparing characters at the CLDR tertiary
 * level, setting the starting position and length of the matching
 * part of the string.
 * @param needle The substring to search for
 * @param haystack the string to search in
 * @param output the position of needle in haystack
 * @param output the length of the match
 * @return true if needle is found in haystack
 */
bool gnc_unicode_has_substring_accented_case_sensitive(const char* needle, const
                                                       char* haystack, int* position,
                                                       int* length);
/** Find the first Unicode-equivalent UTF-8-encoded substring in a
 * UTF-8-encoded string comparing characters at the CLDR identical
 * level, setting the starting position and length of the matching
 * part of the string.
 * @param needle The substring to search for
 * @param haystack the string to search in
 * @param output the position of needle in haystack
 * @param output the length of the match
 * @return true if needle is found in haystack
 */
bool gnc_unicode_has_substring_identical(const char* needle, const
                                         char* haystack, int* position,
                                         int* length);
/** Compare two UTF-8 encoded strings for equivalence at the CLDR
 * primary level in the current locale. Errors are logged to
 * gnc.locale.
 * @param one a string
 * @param two another string
 * @return 0 if one and two are equivalent, -1 if one is less that
 * two, 1 if one is greater than two, -99 on error.
 */
int gnc_unicode_compare_base_chars(const char* one, const char* two);

/** Compare two UTF-8 encoded strings for equivalence at the CLDR
 * secondary level in the current locale. Errors are logged to
 * gnc.locale.
 * @param one a string
 * @param two another string
 * @return 0 if one and two are equivalent, -1 if one is less that
 * two, 1 if one is greater than two, -99 on error.
 */
int gnc_unicode_compare_accented_chars(const char* one, const char* two);

/** Compare two UTF-8 encoded strings for equivalence at the CLDR
 * tertiary level in the current locale. Errors are logged to
 * gnc.locale.
 * @param one a string
 * @param two another string
 * @return 0 if one and two are equivalent, -1 if one is less that
 * two, 1 if one is greater than two, -99 on error.
 */
int gnc_unicode_compare_accented_case_sensitive(const char* one, const char* two);
/** Compare two UTF-8 encoded strings for equivalence at the CLDR
 * identical level in the current locale.
 * @param one a string
 * @param two another string
 * @return 0 if one and two are equivalent, -1 if one is less that
 * two, 1 if one is greater than two, -99 on error.
 */
int gnc_unicode_compare_identical(const char* one, const char* two);
#ifdef __cplusplus
}
#endif
