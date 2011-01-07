/**
 * helpers.c --
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
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "helpers.h"

//! \brief helper function
gboolean text2bool( const gchar *text )
{
	gboolean erg = FALSE;
	gchar *temp;

	if (!text)
		return erg;

	temp = g_strdup( text );
	g_strstrip( temp );
	if ((g_ascii_strcasecmp( temp, "yes" ) == 0) || (g_ascii_strcasecmp( temp, "true" ) == 0) ||
	    (g_ascii_strcasecmp( temp, "1" ) == 0) || (g_ascii_strcasecmp( temp, "x" ) == 0))
		erg = TRUE;
	g_free( temp );
	return erg;
}

//! \brief helper function
GncAmountType text2disc_type( const gchar *text )
{
	GncAmountType type = GNC_AMT_TYPE_PERCENT;
	gchar *temp;

	if (!text)
		return type;

	temp = g_strdup( text );
	g_strstrip( temp );
	if ((strlen(temp) > 0) && (g_ascii_strcasecmp( temp, "%" ) != 0))
		type = GNC_AMT_TYPE_VALUE;
	g_free( temp );
	return type;
}

//! \brief helper function
GncDiscountHow text2disc_how( const gchar *text )
{
	GncDiscountHow how = GNC_DISC_PRETAX;
	gchar *temp;

	if (!text)
		return how;

	temp = g_strdup( text );
	g_strstrip( temp );
	if (g_ascii_strcasecmp( temp, "=" ) == 0)
		how = GNC_DISC_SAMETIME;
	else if (g_ascii_strcasecmp( temp, ">" ) == 0)
		how = GNC_DISC_POSTTAX;
	g_free( temp );
	return how;
}
