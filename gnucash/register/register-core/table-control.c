/********************************************************************\
 * table-control.c -- table control object                          *
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

#include "config.h"

#include <glib.h>

#include "table-control.h"


TableControl *
gnc_table_control_new (void)
{
    TableControl *control;

    control = g_new0 (TableControl, 1);

    return control;
}

void
gnc_table_control_destroy (TableControl *control)
{
    if (!control) return;
    g_free (control);
}

void
gnc_table_control_allow_move (TableControl *control,
                              gboolean allow_move)
{
    if (!control) return;
    control->allow_move = allow_move;
}
