/********************************************************************\
 * The following is code copied from Gnumeric 1.7.8 src/gui-util.c, *
 * and it has been modified slightly to work within GnuCash.        *
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

#include "gnc-csv-gnumeric-popup.h"

#include <glib/gi18n.h>

static void
popup_item_activate (GtkWidget *item, gpointer *user_data)
{
    GnumericPopupMenuElement const *elem =
        g_object_get_data (G_OBJECT (item), "descriptor");
    GnumericPopupMenuHandler handler =
        g_object_get_data (G_OBJECT (item), "handler");

    g_return_if_fail (elem != NULL);
    g_return_if_fail (handler != NULL);

//FIXME gtk4    if (handler (elem, user_data))
//        gtk_window_destroy (GTK_WINDOW(gtk_widget_get_root (item)));
}

static void
gnumeric_create_popup_menu_list (GSList *elements,
                                 GnumericPopupMenuHandler handler,
                                 gpointer user_data,
                                 int display_filter,
                                 int sensitive_filter,
                                 const GdkEvent *event)
{
//FIXME gtk4
#ifdef skip
    GtkWidget *menu = gtk_menu_new ();
    GtkWidget *item;

    for (; elements != NULL ; elements = elements->next)
    {
        GnumericPopupMenuElement const *element = elements->data;
        char const * const name = element->name;
        char const * const pix_name = element->pixmap;

        if (element->display_filter != 0 &&
                !(element->display_filter & display_filter))
            continue;

        if (name != NULL && *name != '\0')
        {
            GtkWidget *label = gtk_label_new_with_mnemonic (name);
            GtkWidget *box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 2);

            item = gtk_menu_item_new();
            gtk_box_set_homogeneous (GTK_BOX (box), FALSE);
            gtk_widget_set_hexpand (GTK_WIDGET(box), FALSE);
            gtk_widget_set_halign (GTK_WIDGET(box), GTK_ALIGN_START);

            if (pix_name != NULL)
            {
                GtkWidget *image = gtk_image_new_from_icon_name (pix_name);
                gtk_image_set_icon_size (GTK_IMAGE(image), GTK_ICON_SIZE_NORMAL);

                gtk_box_append (GTK_BOX(box), GTK_WIDGET(image));
                gtk_widget_set_visible (GTK_WIDGET(image), TRUE);
            }
            gtk_box_prepend (GTK_BOX(box), GTK_WIDGET(label));
            gtk_box_append (GTK_BOX(item), GTK_WIDGET(box));

            if (element->sensitive_filter != 0 &&
                    (element->sensitive_filter & sensitive_filter))
                gtk_widget_set_sensitive (GTK_WIDGET (item), FALSE);
        }
        else
        {
            /* separator */
            item = gtk_separator_menu_item_new ();
        }
//FIXME gtk4        gtk_widget_show_all (item);

        if (element->index != 0)
        {
            g_signal_connect (G_OBJECT (item),
                              "activate",
                              G_CALLBACK (&popup_item_activate), user_data);
            g_object_set_data (
                G_OBJECT (item), "descriptor", (gpointer)(element));
            g_object_set_data (
                G_OBJECT (item), "handler", (gpointer)handler);
        }

        gtk_widget_set_visible (GTK_WIDGET(item), TRUE);
        gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
    }

//FIXME gtk4    gnumeric_popup_menu (GTK_MENU (menu), event);
#endif
}

void
gnumeric_create_popup_menu (GnumericPopupMenuElement const *elements,
                            GnumericPopupMenuHandler handler,
                            gpointer user_data,
                            int display_filter, int sensitive_filter,
                            const GdkEvent *event)
{
    int i;
    GSList *tmp = NULL;

    for (i = 0; elements [i].name != NULL; i++)
        tmp = g_slist_prepend (tmp, (gpointer)(elements + i));

    tmp = g_slist_reverse (tmp);
    gnumeric_create_popup_menu_list (tmp, handler, user_data,
                                     display_filter, sensitive_filter, event);
    g_slist_free (tmp);
}
//FIXME gtk4
#ifdef skip
static void
kill_popup_menu (GtkWidget *widget, GtkMenu *menu)
{
    g_return_if_fail (menu != NULL);
    g_return_if_fail (GTK_IS_MENU (menu));

    g_object_unref (G_OBJECT (menu));
}
#endif
/**
 * gnumeric_popup_menu :
 * @menu : #GtkMenu
 * @event : #GdkEventButton optionally NULL
 *
 * Bring up a popup and if @event is non-NULL ensure that the popup is on the
 * right screen.
 **/
void
gnumeric_popup_menu (GMenu *menu, const GdkEvent *event)
{
//FIXME gtk4
#ifdef skip
    g_return_if_fail (menu != NULL);
    g_return_if_fail (GTK_IS_MENU (menu));

    g_object_ref_sink (menu);

    if (event)
        gtk_menu_set_screen (menu,
                             gdk_window_get_screen (gdk_event_get_window(event)));

    g_signal_connect (G_OBJECT (menu),
                      "hide",
                      G_CALLBACK (kill_popup_menu), menu);

    /* Do NOT pass the button used to create the menu.
     * instead pass 0.  Otherwise bringing up a menu with
     * the right button will disable clicking on the menu with the left.
     */
    gtk_menu_popup_at_pointer (GTK_MENU(menu), event);
#endif
}
