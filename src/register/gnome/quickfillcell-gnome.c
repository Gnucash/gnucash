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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

/* quickfillcell-gnome.c
 *
 * Implements gnome dependant quickfill cell functions.
 */

#include "config.h"

#include <gnome.h>

#include "quickfillcell.h"
#include "util.h"


static gncBoolean
QuickFillDirect (BasicCell *bcell,
                 const char *oldval,
                 char **newval_ptr,
                 int *cursor_position,
                 int *start_selection,
                 int *end_selection,
                 void *gui_data)
{
        QuickFillCell *cell = (QuickFillCell *) bcell;
        GdkEventKey *event = gui_data;
        QuickFill *match;
        int prefix_len;

        if (event->type != GDK_KEY_PRESS)
                return GNC_F;

        switch (event->keyval) {
                case GDK_slash:
                        if (!(event->state & GDK_MOD1_MASK))
                                return GNC_F;
                        break;
                case GDK_Tab:
                case GDK_ISO_Left_Tab:
                        if (!(event->state & GDK_CONTROL_MASK))
                                return GNC_F;
                        break;
                default:
                        return GNC_F;
        }

        match = xaccGetQuickFillStrLen(cell->qfRoot, oldval, *cursor_position);
        if (match == NULL)
                return GNC_T;

        match = xaccGetQuickFillUniqueLen(match, &prefix_len);
        if (match == NULL)
                return GNC_T;

        if ((match->text != NULL) &&
            (strncmp(match->text, oldval, strlen(oldval)) == 0) && 
            (strcmp(match->text, oldval) != 0))
        {
                *newval_ptr = strdup(match->text);
                assert(*newval_ptr != NULL);
                xaccSetBasicCellValue(bcell, *newval_ptr);
        }

        *cursor_position += prefix_len;
        *start_selection = *cursor_position;
        *end_selection = -1;

        return GNC_T;
}


void
xaccQuickFillGUIInit (QuickFillCell *cell)
{
        if (cell == NULL)
                return;

        cell->cell.direct_update = QuickFillDirect;
}

/* =============== end of file =================== */

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
