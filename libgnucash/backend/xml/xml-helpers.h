/********************************************************************\
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


/* xml-helpers.h
 * Miscellaneous bogus helper routines.
 */

static inline void
maybe_add_int (xmlNodePtr ptr, const char* tag, gint val)
{
    if (val)
        xmlAddChild (ptr, int_to_dom_tree (tag, val));
}

static inline void
maybe_add_numeric (xmlNodePtr ptr, const char* tag, gnc_numeric val)
{
    if (!gnc_numeric_zero_p (val))
        xmlAddChild (ptr, gnc_numeric_to_dom_tree (tag, &val));
}

static inline void
maybe_add_string (xmlNodePtr ptr, const char* tag, const char* str)
{
    if (str && strlen (str) > 0)
        xmlAddChild (ptr, text_to_dom_tree (tag, str));
}

static inline void
maybe_add_guid (xmlNodePtr ptr, const char* tag, QofInstance* inst)
{
    if (inst)
        xmlAddChild (ptr, guid_to_dom_tree (tag,
                                            qof_instance_get_guid (inst)));
}

