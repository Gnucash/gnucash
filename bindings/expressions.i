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

%module sw_expressions
%{
#include <gnc-sx-instance-model.h>

SCM scm_init_sw_expressions_module (void);

static GDate
gnc_time64_to_GDate(SCM x)
{
    time64 time = scm_to_int64 (x);
    return time64_to_gdate(time);
}


static SCM
gnc_guid2scm(GncGUID guid)
{
    char string[GUID_ENCODING_LENGTH + 1];

    if (!guid_to_string_buff(&guid, string))
        return SCM_BOOL_F;

    return scm_from_utf8_string(string);
}

static SCM
gnc_numeric_to_scm(gnc_numeric arg)
{
    return gnc_numeric_check (arg) ? SCM_BOOL_F :
        scm_divide (scm_from_int64 (arg.num), scm_from_int64 (arg.denom));
}
%}

%typemap(out) GHashTable * {
  SCM table = scm_c_make_hash_table (g_hash_table_size($1) + 17);
  GHashTableIter iter;
  gpointer key, value;

  g_hash_table_iter_init (&iter, $1);
  while (g_hash_table_iter_next (&iter, &key, &value)) {
    const GncGUID* c_guid = (const GncGUID*) key;
    const gnc_numeric* c_numeric = (const gnc_numeric*) value;
    SCM scm_guid = gnc_guid2scm(*c_guid);
    SCM scm_numeric = gnc_numeric_to_scm(*c_numeric);

    scm_hash_set_x(table, scm_guid, scm_numeric);
  }
  g_hash_table_destroy($1);
  $result = table;
}
GHashTable* gnc_sx_all_instantiate_cashflow_all(GDate range_start, GDate range_end);
%clear GHashTable *;
