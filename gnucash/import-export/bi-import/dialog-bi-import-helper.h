/**
 * dialog-bi-import-helper.h -- Helper functions
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 *
 */



#include <glib/gi18n.h>
#include <glib.h>
#include <glib/gstdio.h>
#include "gncEntry.h"

gboolean text2bool( const gchar *text );
GncAmountType text2disc_type( const gchar *text );
GncDiscountHow text2disc_how( const gchar *text );
gboolean isDateValid(char * date_string);
