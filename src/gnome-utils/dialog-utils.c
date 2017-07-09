/********************************************************************\
 * dialog-utils.c -- utility functions for creating dialogs         *
 *                   for GnuCash                                    *
 * Copyright (C) 1999-2000 Linas Vepstas                            *
 * Copyright (C) 2005 David Hampton <hampton@employees.org>         *
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

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <glib/gi18n.h>
#include <gmodule.h>
#ifdef HAVE_DLFCN_H
# include <dlfcn.h>
#endif

#include "dialog-utils.h"
#include "gnc-commodity.h"
#include "gnc-path.h"
#include "gnc-engine.h"
#include "gnc-euro.h"
#include "gnc-ui-util.h"
#include "gnc-prefs.h"
#include "gnc-combott.h"
#include "guile-util.h"
#include "gnc-main-window.h"
#include <gnc-gdate-utils.h>

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_GUI;

#define GNC_PREF_LAST_GEOMETRY "last-geometry"

const gchar *css_default_color = "* { color: currentColor }";
const gchar *css_red_color     = "* { color: rgb(75%, 0%, 0%) }";

/********************************************************************\
 * gnc_set_label_color                                              *
 *   sets the color of the label given the value                    *
 *                                                                  *
 * Args: label - gtk label widget                                   *
 *       value - value to use to set color                          *
 * Returns: none                                                    *
 \*******************************************************************/
void
gnc_set_label_color(GtkWidget *label, gnc_numeric value)
{
    gboolean deficit;
    GtkStyleContext *stylecontext;
    GtkCssProvider *provider;

    if (!gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED))
        return;

    provider = GTK_CSS_PROVIDER(g_object_get_data (G_OBJECT (label), "custom-provider"));

    if (!provider)
    {
        provider = gtk_css_provider_new();
        gtk_css_provider_load_from_data (provider, css_default_color, -1, NULL);
        stylecontext = gtk_widget_get_style_context (label);
        gtk_style_context_add_provider (stylecontext, GTK_STYLE_PROVIDER (provider),
                                        GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
        g_object_set_data (G_OBJECT (label), "custom-provider", provider);
    }

    deficit = gnc_numeric_negative_p (value);

    if (deficit)
        gtk_css_provider_load_from_data (provider, css_red_color, -1, NULL);
    else
        gtk_css_provider_load_from_data (provider, css_default_color, -1, NULL);
}


/********************************************************************\
 * gnc_restore_window_size                                          *
 *   restores the position and size of the given window, if these   *
 *   these parameters have been saved earlier. Does nothing if no   *
 *   saved values are found.                                        *
 *                                                                  *
 * Args: group - the preferences group to look in for saved coords  *
 *       window - the window for which the coords are to be         *
 *                restored                                          *
 * Returns: nothing                                                 *
 \*******************************************************************/
void
gnc_restore_window_size(const char *group, GtkWindow *window)
{
    gint wpos[2], wsize[2];
    GVariant *geometry;

    ENTER("");

    g_return_if_fail(group != NULL);
    g_return_if_fail(window != NULL);

    if (!gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_SAVE_GEOMETRY))
        return;

    geometry = gnc_prefs_get_value (group, GNC_PREF_LAST_GEOMETRY);
    if (g_variant_is_of_type (geometry, (const GVariantType *) "(iiii)") )
    {
        gint screen_width;
        gint screen_height;
#if GTK_CHECK_VERSION(3,22,0)
        GdkWindow *win = gdk_screen_get_root_window (gtk_window_get_screen (window));
        GdkMonitor *mon = gdk_display_get_monitor_at_window (gtk_widget_get_display (GTK_WIDGET(window)), win);
        GdkRectangle monitor_size;

        gdk_monitor_get_geometry (mon, &monitor_size);

        screen_width = monitor_size.width;
        screen_height = monitor_size.height;
#else
        screen_width = gdk_screen_width(); //default screen
        screen_height = gdk_screen_height(); //default screen
#endif
        g_variant_get (geometry, "(iiii)",
                       &wpos[0],  &wpos[1],
                       &wsize[0], &wsize[1]);
        DEBUG("geometry from preferences - wpos[0]: %d, wpos[1]: %d, wsize[0]: %d, wsize[1]: %d",
              wpos[0],  wpos[1], wsize[0], wsize[1]);

        /* (-1, -1) means no geometry was saved (default preferences value) */
        if ((wpos[0] != -1) && (wpos[1] != -1))
        {
            /* Keep the window on screen if possible */
            if (screen_width != 0)
                wpos[0] = wpos[0] % screen_width;
            if (screen_height != 0)
                wpos[1] = wpos[1] % screen_height;
            DEBUG("geometry after screen adaption - wpos[0]: %d, wpos[1]: %d, wsize[0]: %d, wsize[1]: %d",
                  wpos[0],  wpos[1], wsize[0], wsize[1]);

            gtk_window_move(window, wpos[0], wpos[1]);
        }

        /* Don't attempt to restore invalid sizes */
        if ((wsize[0] > 0) && (wsize[1] > 0))
            gtk_window_resize(window, wsize[0], wsize[1]);
    }
    g_variant_unref (geometry);

    LEAVE("");
}


/********************************************************************\
 * gnc_save_window_size                                             *
 *   save the window position and size into options whose names are *
 *   prefixed by the group name.                                   *
 *                                                                  *
 * Args: group - preferences group to save the options in           *
 *       window - the window for which current position and size    *
 *                are to be saved                                   *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_save_window_size(const char *group, GtkWindow *window)
{
    gint wpos[2], wsize[2];
    GVariant *geometry;

    g_return_if_fail(group != NULL);
    g_return_if_fail(window != NULL);

    if (!gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_SAVE_GEOMETRY))
        return;

    gtk_window_get_position(GTK_WINDOW(window), &wpos[0], &wpos[1]);
    gtk_window_get_size(GTK_WINDOW(window), &wsize[0], &wsize[1]);
    geometry = g_variant_new ("(iiii)", wpos[0],  wpos[1],
                              wsize[0], wsize[1]);
    gnc_prefs_set_value (group, GNC_PREF_LAST_GEOMETRY, geometry);
    /* Don't unref geometry here, it is consumed by gnc_prefs_set_value */
}


/********************************************************************\
 * gnc_window_adjust_for_screen                                     *
 *   adjust the window size if it is bigger than the screen size.   *
 *                                                                  *
 * Args: window - the window to adjust                              *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_window_adjust_for_screen(GtkWindow * window)
{
#if GTK_CHECK_VERSION(3,22,0)
    GdkWindow *win;
    GdkMonitor *mon;
    GdkRectangle monitor_size;
#endif
    gint screen_width;
    gint screen_height;
    gint width;
    gint height;

    if (window == NULL)
        return;

    g_return_if_fail(GTK_IS_WINDOW(window));
    if (gtk_widget_get_window (GTK_WIDGET(window)) == NULL)
        return;

#if GTK_CHECK_VERSION(3,22,0)
    win = gdk_screen_get_root_window (gtk_window_get_screen (window));
    mon = gdk_display_get_monitor_at_window (gtk_widget_get_display (GTK_WIDGET(window)), win);
    gdk_monitor_get_geometry (mon, &monitor_size);

    screen_width = monitor_size.width;
    screen_height = monitor_size.height;
#else
    screen_width = gdk_screen_width();
    screen_height = gdk_screen_height();
#endif
    width = gdk_window_get_width (gtk_widget_get_window (GTK_WIDGET(window)));
    height = gdk_window_get_height (gtk_widget_get_window (GTK_WIDGET(window)));

    if ((width <= screen_width) && (height <= screen_height))
        return;

    width = MIN(width, screen_width - 10);
    width = MAX(width, 0);

    height = MIN(height, screen_height - 10);
    height = MAX(height, 0);

    gdk_window_resize(gtk_widget_get_window (GTK_WIDGET(window)), width, height);
    gtk_widget_queue_resize(GTK_WIDGET(window));
}

/********************************************************************\
 * Sets the alignament of a Label Widget, GTK3 version specific.    *
 *                                                                  *
 * Args: widget - the label widget to set alignment on              *
 *       xalign - x alignment                                       *
 *       yalign - y alignment                                       *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_label_set_alignment (GtkWidget *widget, gfloat xalign, gfloat yalign)
{
#if GTK_CHECK_VERSION(3,16,0)
    gtk_label_set_xalign (GTK_LABEL (widget), xalign);
    gtk_label_set_yalign (GTK_LABEL (widget), yalign);
#else
    gtk_misc_set_alignment (GTK_MISC (widget), xalign, yalign);
#endif
}

/********************************************************************\
 * Get the preference for showing tree view grid lines              *
 *                                                                  *
 * Args: none                                                       *
 * Returns:  GtkTreeViewGridLines setting                           *
\********************************************************************/
GtkTreeViewGridLines
gnc_tree_view_get_grid_lines_pref (void)
{
    GtkTreeViewGridLines grid_lines;
    gboolean h_lines = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, "grid-lines-horizontal");
    gboolean v_lines = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, "grid-lines-vertical");

    if (h_lines)
    {
        if (v_lines)
            grid_lines = GTK_TREE_VIEW_GRID_LINES_BOTH;
        else
            grid_lines =  GTK_TREE_VIEW_GRID_LINES_HORIZONTAL;
    }
    else if (v_lines)
        grid_lines = GTK_TREE_VIEW_GRID_LINES_VERTICAL;
    else
        grid_lines = GTK_TREE_VIEW_GRID_LINES_NONE;
    return grid_lines;
}

/********************************************************************\
 * Add a style context to a Widget so it can be altered with css    *
 *                                                                  *
 * Args:    widget - widget to add css style too                    *
 *       gnc_class - character string for css class name            *
 * Returns:  nothing                                                *
\********************************************************************/
void
gnc_widget_set_style_context (GtkWidget *widget, const char *gnc_class)
{
    GtkStyleContext *context = gtk_widget_get_style_context (widget);
    gtk_style_context_add_class (context, gnc_class);
}

gboolean
gnc_handle_date_accelerator (GdkEventKey *event,
                             struct tm *tm,
                             const char *date_str)
{
    GDate gdate;

    g_return_val_if_fail (event != NULL, FALSE);
    g_return_val_if_fail (tm != NULL, FALSE);
    g_return_val_if_fail (date_str != NULL, FALSE);

    if (event->type != GDK_KEY_PRESS)
        return FALSE;

    if ((tm->tm_mday <= 0) || (tm->tm_mon == -1) || (tm->tm_year == -1))
        return FALSE;

    // Make sure we have a valid date before we proceed
    if (!g_date_valid_dmy (tm->tm_mday, tm->tm_mon + 1, tm->tm_year + 1900))
        return FALSE;

    g_date_set_dmy (&gdate,
                    tm->tm_mday,
                    tm->tm_mon + 1,
                    tm->tm_year + 1900);

    /*
     * Check those keys where the code does different things depending
     * upon the modifiers.
     */
    switch (event->keyval)
    {
    case GDK_KEY_KP_Add:
    case GDK_KEY_plus:
    case GDK_KEY_equal:
        if (event->state & GDK_SHIFT_MASK)
            g_date_add_days (&gdate, 7);
        else if (event->state & GDK_MOD1_MASK)
            g_date_add_months (&gdate, 1);
        else if (event->state & GDK_CONTROL_MASK)
            g_date_add_years (&gdate, 1);
        else
            g_date_add_days (&gdate, 1);
        g_date_to_struct_tm (&gdate, tm);
        return TRUE;

    case GDK_KEY_minus:
    case GDK_KEY_KP_Subtract:
    case GDK_KEY_underscore:
        if ((strlen (date_str) != 0) && (dateSeparator () == '-'))
        {
            const char *c;
            gunichar uc;
            int count = 0;

            /* rough check for existing date */
            c = date_str;
            while (*c)
            {
                uc = g_utf8_get_char (c);
                if (uc == '-')
                    count++;
                c = g_utf8_next_char (c);
            }

            if (count < 2)
                return FALSE;
        }

        if (event->state & GDK_SHIFT_MASK)
            g_date_subtract_days (&gdate, 7);
        else if (event->state & GDK_MOD1_MASK)
            g_date_subtract_months (&gdate, 1);
        else if (event->state & GDK_CONTROL_MASK)
            g_date_subtract_years (&gdate, 1);
        else
            g_date_subtract_days (&gdate, 1);
        g_date_to_struct_tm (&gdate, tm);
        return TRUE;

    default:
        break;
    }

    /*
     * Control and Alt key combinations should be ignored by this
     * routine so that the menu system gets to handle them.  This
     * prevents weird behavior of the menu accelerators (i.e. work in
     * some widgets but not others.)
     */
    if (event->state & (GDK_CONTROL_MASK | GDK_MOD1_MASK))
        return FALSE;

    /* Now check for the remaining keystrokes. */
    switch (event->keyval)
    {
    case GDK_KEY_braceright:
    case GDK_KEY_bracketright:
        /* increment month */
        g_date_add_months (&gdate, 1);
        break;

    case GDK_KEY_braceleft:
    case GDK_KEY_bracketleft:
        /* decrement month */
        g_date_subtract_months (&gdate, 1);
        break;

    case GDK_KEY_M:
    case GDK_KEY_m:
        /* beginning of month */
        g_date_set_day (&gdate, 1);
        break;

    case GDK_KEY_H:
    case GDK_KEY_h:
        /* end of month */
        g_date_set_day (&gdate, 1);
        g_date_add_months (&gdate, 1);
        g_date_subtract_days (&gdate, 1);
        break;

    case GDK_KEY_Y:
    case GDK_KEY_y:
        /* beginning of year */
        g_date_set_day (&gdate, 1);
        g_date_set_month (&gdate, 1);
        break;

    case GDK_KEY_R:
    case GDK_KEY_r:
        /* end of year */
        g_date_set_day (&gdate, 1);
        g_date_set_month (&gdate, 1);
        g_date_add_years (&gdate, 1);
        g_date_subtract_days (&gdate, 1);
        break;

    case GDK_KEY_T:
    case GDK_KEY_t:
        /* today */
        gnc_gdate_set_today (&gdate);
        break;

    default:
        return FALSE;
    }

    g_date_to_struct_tm (&gdate, tm);

    return TRUE;
}


/*--------------------------------------------------------------------------
 *   GtkBuilder support functions
 *-------------------------------------------------------------------------*/

GModule *allsymbols = NULL;

/* gnc_builder_add_from_file:
 *
 *   a convenience wrapper for gtk_builder_add_objects_from_file.
 *   It takes care of finding the directory for glade files and prints a
 *   warning message in case of an error.
 */
gboolean
gnc_builder_add_from_file (GtkBuilder *builder, const char *filename, const char *root)
{
    GError* error = NULL;
    char *fname;
    gchar *gnc_builder_dir;
    gboolean result;

    g_return_val_if_fail (builder != NULL, FALSE);
    g_return_val_if_fail (filename != NULL, FALSE);
    g_return_val_if_fail (root != NULL, FALSE);

    gnc_builder_dir = gnc_path_get_gtkbuilderdir ();
    fname = g_build_filename(gnc_builder_dir, filename, (char *)NULL);
    g_free (gnc_builder_dir);

    {
        gchar *localroot = g_strdup(root);
        gchar *objects[] = { localroot, NULL };
        result = gtk_builder_add_objects_from_file (builder, fname, objects, &error);
        if (!result)
        {
            PWARN ("Couldn't load builder file: %s", error->message);
            g_error_free (error);
        }
        g_free (localroot);
    }

    g_free (fname);

    return result;
}


/*---------------------------------------------------------------------
 * The following function is built from a couple of glade functions.
 *--------------------------------------------------------------------*/
void
gnc_builder_connect_full_func(GtkBuilder *builder,
                              GObject *signal_object,
                              const gchar *signal_name,
                              const gchar *handler_name,
                              GObject *connect_object,
                              GConnectFlags flags,
                              gpointer user_data)
{
    GCallback func;
    GCallback *p_func = &func;

    if (allsymbols == NULL)
    {
        /* get a handle on the main executable -- use this to find symbols */
        allsymbols = g_module_open(NULL, 0);
    }

    if (!g_module_symbol(allsymbols, handler_name, (gpointer *)p_func))
    {
#ifdef HAVE_DLSYM
        /* Fallback to dlsym -- necessary for *BSD linkers */
        func = dlsym(RTLD_DEFAULT, handler_name);
#else
        func = NULL;
#endif
        if (func == NULL)
        {
            PWARN("ggaff: could not find signal handler '%s'.", handler_name);
            return;
        }
    }

    if (connect_object)
        g_signal_connect_object (signal_object, signal_name, func,
                                 connect_object, flags);
    else
        g_signal_connect_data(signal_object, signal_name, func,
                              user_data, NULL , flags);
}
/*--------------------------------------------------------------------------
 * End of GtkBuilder utilities
 *-------------------------------------------------------------------------*/


void
gnc_gtk_dialog_add_button (GtkWidget *dialog, const gchar *label, const gchar *icon_name, guint response)
{
    GtkWidget *button;

    button = gtk_button_new_with_mnemonic(label);
    if (icon_name)
    {
        GtkWidget *image;

        image = gtk_image_new_from_icon_name (icon_name, GTK_ICON_SIZE_BUTTON);
        gtk_button_set_image (GTK_BUTTON(button), image);
        g_object_set (button, "always-show-image", TRUE, NULL);
    }
    g_object_set (button, "can-default", TRUE, NULL);
    gtk_widget_show_all(button);
    gtk_dialog_add_action_widget(GTK_DIALOG(dialog), button, response);
}

static void
gnc_perm_button_cb (GtkButton *perm, gpointer user_data)
{
    gboolean perm_active;

    perm_active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(perm));
    gtk_widget_set_sensitive(user_data, !perm_active);
}

gint
gnc_dialog_run (GtkDialog *dialog, const gchar *pref_name)
{
    GtkWidget *perm, *temp;
    gboolean ask = TRUE;
    gint response;

    /* Does the user want to see this question? If not, return the
     * previous answer. */
    response = gnc_prefs_get_int(GNC_PREFS_GROUP_WARNINGS_PERM, pref_name);
    if (response != 0)
        return response;
    response = gnc_prefs_get_int(GNC_PREFS_GROUP_WARNINGS_TEMP, pref_name);
    if (response != 0)
        return response;

    /* Add in the checkboxes to find out if the answer should be remembered. */
#if 0
    if (GTK_IS_MESSAGE_DIALOG(dialog))
    {
        GtkMessageType type;
        g_object_get(dialog, "message-type", &type, (gchar*)NULL);
        ask = (type == GTK_MESSAGE_QUESTION);
    }
    else
    {
        ask = FALSE;
    }
#endif
    perm = gtk_check_button_new_with_mnemonic
           (ask
            ? _("Remember and don't _ask me again.")
            : _("Don't _tell me again."));
    temp = gtk_check_button_new_with_mnemonic
           (ask
            ? _("Remember and don't ask me again this _session.")
            : _("Don't tell me again this _session."));
    gtk_widget_show(perm);
    gtk_widget_show(temp);
    gtk_box_pack_start (GTK_BOX (gtk_dialog_get_content_area (dialog)), perm, TRUE, TRUE, 0);
    gtk_box_pack_start (GTK_BOX (gtk_dialog_get_content_area (dialog)), temp, TRUE, TRUE, 0);
    g_signal_connect(perm, "clicked", G_CALLBACK(gnc_perm_button_cb), temp);

    /* OK. Present the dialog. */
    response = gtk_dialog_run(dialog);
    if ((response == GTK_RESPONSE_NONE) || (response == GTK_RESPONSE_DELETE_EVENT))
    {
        return GTK_RESPONSE_CANCEL;
    }

    if (response != GTK_RESPONSE_CANCEL)
    {
        /* Save the answer? */
        if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(perm)))
        {
            gnc_prefs_set_int(GNC_PREFS_GROUP_WARNINGS_PERM, pref_name, response);
        }
        else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(temp)))
        {
            gnc_prefs_set_int(GNC_PREFS_GROUP_WARNINGS_TEMP, pref_name, response);
        }
    }
    return response;
}

/* If this is a new book, this function can be used to display book options
 * dialog so user can specify options, before any transactions can be
 * imported/entered, since the book options can affect how transactions are
 * created. Note: This dialog is modal! */
gboolean
gnc_new_book_option_display (GtkWidget *parent)
{
    GtkWidget *window;
    gint result = GTK_RESPONSE_HELP;

    window = gnc_book_options_dialog_cb (TRUE, _( "New Book Options"));
    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW(window), GTK_WINDOW(parent));

    if (window)
    {
        /* close dialog and proceed unless help button selected */
        while (result == GTK_RESPONSE_HELP)
        {
            result = gtk_dialog_run(GTK_DIALOG(window));
        }
        return FALSE;
    }
    return TRUE;
}

/* This function returns a widget for selecting a cost policy
 */
GtkWidget *
gnc_cost_policy_select_new (void)
{
    GtkWidget *cost_policy_widget = NULL;
    GList *list_of_policies = NULL;

    list_of_policies = gnc_get_valid_policy_list();

    g_return_val_if_fail(g_list_length (list_of_policies) >= 0, NULL);
    if (list_of_policies)
    {
        GtkListStore *store;
        GtkTreeIter  iter;
        const char *description;
        const char *hintstring;
        GList *l = NULL;

        store = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);
        /* Add values to the list store, entry and tooltip */
        for (l = list_of_policies; l != NULL; l = l->next)
        {
            GNCPolicy *pcy = l->data;
            description = PolicyGetDescription(pcy);
            hintstring = PolicyGetHint(pcy);
            gtk_list_store_append (store, &iter);
            gtk_list_store_set
                   (store,
                    &iter,
                    0,
                    (description && *description) ? _(description) : "",
                    1,
                    (hintstring && *hintstring) ? _(hintstring) : "",
                    -1);
        }
        g_list_free(list_of_policies);
        /* Create the new Combo with tooltip and add the store */
        cost_policy_widget = GTK_WIDGET(gnc_combott_new());
        g_object_set( G_OBJECT( cost_policy_widget ),
                      "model",
                      GTK_TREE_MODEL(store),
                      NULL );
        g_object_unref(store);
    }
    return cost_policy_widget;
}

