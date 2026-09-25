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

/* A binding is owned by the two controllers attached to a cell/editor. */
typedef struct
{
    gint refcount;
    GtkWidget *anchor;
    GnumericPopupMenuElement const *elements;
    GnumericPopupMenuHandler handler;
    gpointer user_data;
    int display_filter;
    int sensitive_filter;
} GnumericPopupBinding;

typedef struct
{
    GtkPopover *popover;
    GnumericPopupBinding binding;
} GnumericPopupContext;

static GnumericPopupBinding *
popup_binding_ref (GnumericPopupBinding *binding)
{
    g_atomic_int_inc (&binding->refcount);
    return binding;
}

static void
popup_binding_unref (GnumericPopupBinding *binding)
{
    if (g_atomic_int_dec_and_test (&binding->refcount))
        g_free (binding);
}

static void
popup_closed_cb (GtkPopover *popover, gpointer user_data)
{
    (void)user_data;
    if (gtk_widget_get_parent (GTK_WIDGET (popover)))
        gtk_widget_unparent (GTK_WIDGET (popover));
}
static void
popup_context_free (GnumericPopupContext *context)
{
    g_free (context);
}

static gboolean
popup_escape_cb (GtkWidget *widget, GVariant *args, gpointer user_data)
{
    GnumericPopupContext *context = user_data;
    (void)widget;
    (void)args;

    gtk_popover_popdown (context->popover);
    return TRUE;
}

static void
popup_add_escape_shortcut (GtkPopover *popover, GnumericPopupContext *context)
{
    GtkShortcutController *shortcuts = GTK_SHORTCUT_CONTROLLER (gtk_shortcut_controller_new ());
    GtkShortcut *shortcut = gtk_shortcut_new (
        GTK_SHORTCUT_TRIGGER (gtk_keyval_trigger_new (GDK_KEY_Escape, 0)),
        GTK_SHORTCUT_ACTION (gtk_callback_action_new (popup_escape_cb, context, NULL)));

    gtk_shortcut_controller_set_scope (shortcuts, GTK_SHORTCUT_SCOPE_LOCAL);
    gtk_shortcut_controller_add_shortcut (shortcuts, shortcut);
    gtk_widget_add_controller (GTK_WIDGET (popover), GTK_EVENT_CONTROLLER (shortcuts));
}

static void
popup_item_clicked_cb (GtkButton *button, GnumericPopupContext *context)
{
    GnumericPopupMenuElement const *element =
        g_object_get_data (G_OBJECT (button), "gnumeric-popup-element");
    gboolean close_popup;

    g_return_if_fail (element);
    g_return_if_fail (context->binding.handler);

    /* The handler can close the editor or its window. Keep the popover and
     * its context alive until its return before using either again. */
    g_object_ref (context->popover);
    close_popup = context->binding.handler (element, context->binding.user_data);
    if (close_popup && gtk_widget_get_parent (GTK_WIDGET (context->popover)))
        gtk_popover_popdown (context->popover);
    g_object_unref (context->popover);
}

static GtkWidget *
popup_button_new (GnumericPopupMenuElement const *element,
                  GnumericPopupContext *context)
{
    GtkWidget *button = gtk_button_new ();
    GtkWidget *box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    GtkWidget *label = gtk_label_new_with_mnemonic (element->name);

    gtk_widget_set_halign (box, GTK_ALIGN_FILL);
    gtk_label_set_xalign (GTK_LABEL (label), 0.0f);
    gtk_widget_set_hexpand (label, TRUE);
    gtk_label_set_mnemonic_widget (GTK_LABEL (label), button);
    if (element->pixmap)
    {
        GtkWidget *image = gtk_image_new_from_icon_name (element->pixmap);
        gtk_box_append (GTK_BOX (box), image);
    }
    gtk_box_append (GTK_BOX (box), label);
    gtk_button_set_child (GTK_BUTTON (button), box);
    gtk_widget_set_sensitive (button, element->index != 0 &&
        !(element->sensitive_filter &&
          (element->sensitive_filter & context->binding.sensitive_filter)));
    if (element->index != 0)
    {
        g_object_set_data (G_OBJECT (button), "gnumeric-popup-element", (gpointer)element);
        g_signal_connect (button, "clicked", G_CALLBACK (popup_item_clicked_cb), context);
    }
    return button;
}

static void
popup_present (GtkWidget *anchor, GnumericPopupBinding const *binding,
               const GdkRectangle *pointing_to)
{
    GnumericPopupContext *context;
    GtkWidget *box;
    GtkWidget *first_button = NULL;

    g_return_if_fail (GTK_IS_WIDGET (anchor));
    g_return_if_fail (binding && binding->elements && binding->handler);

    context = g_new0 (GnumericPopupContext, 1);
    context->binding = *binding;
    context->popover = GTK_POPOVER (gtk_popover_new ());
    box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_widget_add_css_class (box, "menu");
    gtk_popover_set_child (context->popover, box);
    gtk_popover_set_autohide (context->popover, TRUE);
    if (pointing_to)
        gtk_popover_set_pointing_to (context->popover, pointing_to);
    gtk_widget_set_parent (GTK_WIDGET (context->popover), anchor);
    popup_add_escape_shortcut (context->popover, context);
    g_signal_connect_data (context->popover, "closed", G_CALLBACK (popup_closed_cb),
                           context, (GClosureNotify)popup_context_free, 0);

    for (guint i = 0; binding->elements[i].name; i++)
    {
        GnumericPopupMenuElement const *element = binding->elements + i;
        GtkWidget *item;

        if (element->display_filter &&
            !(element->display_filter & binding->display_filter))
            continue;
        if (!*element->name)
        {
            gtk_box_append (GTK_BOX (box), gtk_separator_new (GTK_ORIENTATION_HORIZONTAL));
            continue;
        }
        item = popup_button_new (element, context);
        gtk_box_append (GTK_BOX (box), item);
        if (!first_button && gtk_widget_get_sensitive (item))
            first_button = item;
    }

    gtk_popover_popup (context->popover);
    if (first_button)
        gtk_widget_grab_focus (first_button);
}

void
gnumeric_popup_menu (GtkWidget *anchor, GMenuModel *menu,
                     const GdkRectangle *pointing_to)
{
    GtkPopover *popover;

    g_return_if_fail (GTK_IS_WIDGET (anchor));
    g_return_if_fail (G_IS_MENU_MODEL (menu));

    popover = GTK_POPOVER (gtk_popover_menu_new_from_model (menu));
    if (pointing_to)
        gtk_popover_set_pointing_to (popover, pointing_to);
    gtk_widget_set_parent (GTK_WIDGET (popover), anchor);
    g_signal_connect (popover, "closed", G_CALLBACK (popup_closed_cb), NULL);
    gtk_popover_popup (popover);
}

void
gnumeric_create_popup_menu (GtkWidget *anchor,
                            GnumericPopupMenuElement const *elements,
                            GnumericPopupMenuHandler handler,
                            gpointer user_data,
                            int display_filter, int sensitive_filter,
                            const GdkRectangle *pointing_to)
{
    GnumericPopupBinding binding =
    {
        .refcount = 1,
        .anchor = anchor,
        .elements = elements,
        .handler = handler,
        .user_data = user_data,
        .display_filter = display_filter,
        .sensitive_filter = sensitive_filter
    };

    popup_present (anchor, &binding, pointing_to);
}

static void
popup_context_pressed_cb (GtkGestureClick *gesture, int n_press,
                          double x, double y, gpointer user_data)
{
    GnumericPopupBinding *binding = user_data;
    GdkRectangle rectangle = { (int)x, (int)y, 1, 1 };
    (void)gesture;
    (void)n_press;

    popup_present (binding->anchor, binding, &rectangle);
}

static gboolean
popup_keyboard_cb (GtkWidget *widget, GVariant *args, gpointer user_data)
{
    GnumericPopupBinding *binding = user_data;
    GdkRectangle rectangle = { 0, 0, MAX (1, gtk_widget_get_width (widget)),
                               MAX (1, gtk_widget_get_height (widget)) };
    (void)args;

    popup_present (binding->anchor, binding, &rectangle);
    return TRUE;
}

static void
popup_add_shortcut (GtkShortcutController *controller, guint keyval,
                    GdkModifierType modifiers, GnumericPopupBinding *binding)
{
    GtkShortcut *shortcut = gtk_shortcut_new (
        GTK_SHORTCUT_TRIGGER (gtk_keyval_trigger_new (keyval, modifiers)),
        GTK_SHORTCUT_ACTION (gtk_callback_action_new (popup_keyboard_cb,
                                                       popup_binding_ref (binding),
                                                       (GDestroyNotify)popup_binding_unref)));
    gtk_shortcut_controller_add_shortcut (controller, shortcut);
}

void
gnumeric_popup_menu_attach (GtkWidget *anchor,
                            GnumericPopupMenuElement const *elements,
                            GnumericPopupMenuHandler handler,
                            gpointer user_data,
                            int display_filter, int sensitive_filter)
{
    GnumericPopupBinding *binding;
    GtkGesture *gesture;
    GtkShortcutController *shortcuts;

    g_return_if_fail (GTK_IS_WIDGET (anchor));
    g_return_if_fail (elements && handler);

    binding = g_new0 (GnumericPopupBinding, 1);
    binding->refcount = 1;
    binding->anchor = anchor;
    binding->elements = elements;
    binding->handler = handler;
    binding->user_data = user_data;
    binding->display_filter = display_filter;
    binding->sensitive_filter = sensitive_filter;

    gesture = gtk_gesture_click_new ();
    gtk_gesture_single_set_button (GTK_GESTURE_SINGLE (gesture), GDK_BUTTON_SECONDARY);
    g_signal_connect_data (gesture, "pressed", G_CALLBACK (popup_context_pressed_cb),
                           popup_binding_ref (binding),
                           (GClosureNotify)popup_binding_unref, 0);
    gtk_widget_add_controller (anchor, GTK_EVENT_CONTROLLER (gesture));

    shortcuts = GTK_SHORTCUT_CONTROLLER (gtk_shortcut_controller_new ());
    gtk_shortcut_controller_set_scope (shortcuts, GTK_SHORTCUT_SCOPE_LOCAL);
    popup_add_shortcut (shortcuts, GDK_KEY_Menu, 0, binding);
    popup_add_shortcut (shortcuts, GDK_KEY_F10, GDK_SHIFT_MASK, binding);
    gtk_widget_add_controller (anchor, GTK_EVENT_CONTROLLER (shortcuts));
    popup_binding_unref (binding);
}
