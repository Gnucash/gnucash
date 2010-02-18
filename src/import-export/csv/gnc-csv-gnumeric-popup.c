/* The following is code copied from Gnumeric 1.7.8 licensed under the
 * GNU General Public License version 2. It is from the file
 * gnumeric/src/gui-util.c, and it has been modified slightly to work
 * within GnuCash. */

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

    if (handler (elem, user_data))
        gtk_widget_destroy (gtk_widget_get_toplevel (item));
}

static void
gnumeric_create_popup_menu_list (GSList *elements,
                                 GnumericPopupMenuHandler handler,
                                 gpointer user_data,
                                 int display_filter,
                                 int sensitive_filter,
                                 GdkEventButton *event)
{
    GtkWidget *menu, *item;
    char const *trans;

    menu = gtk_menu_new ();

    for (; elements != NULL ; elements = elements->next)
    {
        GnumericPopupMenuElement const *element = elements->data;
        char const * const name = element->name;
        char const * const pix_name = element->pixmap;

        item = NULL;

        if (element->display_filter != 0 &&
                !(element->display_filter & display_filter))
            continue;

        if (name != NULL && *name != '\0')
        {
            trans = _(name);
            item = gtk_image_menu_item_new_with_mnemonic (trans);
            if (element->sensitive_filter != 0 &&
                    (element->sensitive_filter & sensitive_filter))
                gtk_widget_set_sensitive (GTK_WIDGET (item), FALSE);
            if (pix_name != NULL)
            {
                GtkWidget *image = gtk_image_new_from_stock (pix_name,
                                   GTK_ICON_SIZE_MENU);
                gtk_widget_show (image);
                gtk_image_menu_item_set_image (
                    GTK_IMAGE_MENU_ITEM (item),
                    image);
            }
        }
        else
        {
            /* separator */
            item = gtk_menu_item_new ();
            gtk_widget_set_sensitive (item, FALSE);
        }

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

        gtk_widget_show (item);
        gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
    }

    gnumeric_popup_menu (GTK_MENU (menu), event);
}

void
gnumeric_create_popup_menu (GnumericPopupMenuElement const *elements,
                            GnumericPopupMenuHandler handler,
                            gpointer user_data,
                            int display_filter, int sensitive_filter,
                            GdkEventButton *event)
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

static void
kill_popup_menu (GtkWidget *widget, GtkMenu *menu)
{
    g_return_if_fail (menu != NULL);
    g_return_if_fail (GTK_IS_MENU (menu));

    g_object_unref (G_OBJECT (menu));
}

/**
 * gnumeric_popup_menu :
 * @menu : #GtkMenu
 * @event : #GdkEventButton optionally NULL
 *
 * Bring up a popup and if @event is non-NULL ensure that the popup is on the
 * right screen.
 **/
void
gnumeric_popup_menu (GtkMenu *menu, GdkEventButton *event)
{
    g_return_if_fail (menu != NULL);
    g_return_if_fail (GTK_IS_MENU (menu));

#if GLIB_CHECK_VERSION(2,10,0) && GTK_CHECK_VERSION(2,8,14)
    g_object_ref_sink (menu);
#else
    g_object_ref (menu);
    gtk_object_sink (GTK_OBJECT (menu));
#endif

    if (event)
        gtk_menu_set_screen (menu,
                             gdk_drawable_get_screen (event->window));

    g_signal_connect (G_OBJECT (menu),
                      "hide",
                      G_CALLBACK (kill_popup_menu), menu);

    /* Do NOT pass the button used to create the menu.
     * instead pass 0.  Otherwise bringing up a menu with
     * the right button will disable clicking on the menu with the left.
     */
    gtk_menu_popup (menu, NULL, NULL, NULL, NULL, 0,
                    (event != NULL) ? event->time
                    : gtk_get_current_event_time());
}
