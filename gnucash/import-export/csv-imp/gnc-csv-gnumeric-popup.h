/* The following is code copied from Gnumeric 1.7.8 licensed under the
 * GNU General Public License version 2 and/or version 3. It is from the file
 * gnumeric/src/gui-util.h, and it has been modified slightly to work
 * within GnuCash. */
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


/* Miguel de Icaza is not sure specifically who from the Gnumeric
 * community is the copyright owner of the code below, so, on his
 * recommendation, here is the full list of Gnumeric authors.
 *
 * Miguel de Icaza, creator.
 * Jody Goldberg, maintainer.
 * Harald Ashburner, Options pricers
 * Sean Atkinson, functions and X-Base importing.
 * Michel Berkelaar, Simplex algorithm for Solver (LP Solve).
 * Jean Brefort, Core charting engine.
 * Grandma Chema Celorio, Tester and sheet copy.
 * Frank Chiulli, OLE support.
 * Kenneth Christiansen, i18n, misc stuff.
 * Zbigniew Chyla, plugin system, i18n.
 * J.H.M. Dassen (Ray), debian packaging.
 * Jeroen Dirks, Simplex algorithm for Solver (LP Solve).
 * Tom Dyas, plugin support.
 * Gergo Erdi, Gnumeric hacker.
 * John Gotts, rpm packaging.
 * Andreas J. Guelzow, Gnumeric hacker.
 * Jon K. Hellan, Gnumeric hacker.
 * Ross Ihaka, special functions.
 * Jukka-Pekka Iivonen, numerous functions and tools.
 * Jakub Jelinek, Gnumeric hacker.
 * Chris Lahey, number format engine.
 * Adrian Likins, documentation, debugging.
 * Takashi Matsuda, original text plugin.
 * Michael Meeks, Excel and OLE2 importing.
 * Lutz Muller, SheetObject improvements.
 * Emmanuel Pacaud, Many plot types for charting engine.
 * Federico M. Quintero, canvas support.
 * Mark Probst, Guile support.
 * Rasca, HTML, troff, LaTeX exporters.
 * Vincent Renardias, original CSV support, French localization.
 * Ariel Rios, Guile support.
 * Uwe Steinmann, Paradox Importer.
 * Arturo Tena, OLE support.
 * Almer S. Tigelaar, Gnumeric hacker.
 * Bruno Unna, Excel bits.
 * Daniel Veillard, XML support.
 * Vladimir Vuksan, financial functions.
 * Morten Welinder, Gnumeric hacker and leak plugging demi-god.
 */

#ifndef GNC_CSV_GNUMERIC_POPUP
#define GNC_CSV_GNUMERIC_POPUP

#include <gtk/gtk.h>

typedef struct
{
    char const *name;
    char const *pixmap;
    int display_filter;
    int sensitive_filter;

    int index;
} GnumericPopupMenuElement;

typedef gboolean (*GnumericPopupMenuHandler) (GnumericPopupMenuElement const *e,
        gpointer user_data);

/* Use this on menus that are popped up */
void gnumeric_popup_menu (GtkMenu *menu, GdkEventButton *event);

void gnumeric_create_popup_menu (GnumericPopupMenuElement const *elements,
                                 GnumericPopupMenuHandler handler,
                                 gpointer user_data,
                                 int display_filter,
                                 int sensitive_filter,
                                 GdkEventButton *event);


#endif
