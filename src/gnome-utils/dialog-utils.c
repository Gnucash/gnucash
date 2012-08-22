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
#include "gnc-gconf-utils.h"

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_GUI;

#define WINDOW_POSITION		"window_position"
#define WINDOW_GEOMETRY		"window_geometry"

/********************************************************************\
 * gnc_get_toolbar_style                                            *
 *   returns the current toolbar style for gnucash toolbars         *
 *                                                                  *
 * Args: none                                                       *
 * Returns: toolbar style                                           *
 \*******************************************************************/
GtkToolbarStyle
gnc_get_toolbar_style(void)
{
    GtkToolbarStyle tbstyle = GTK_TOOLBAR_BOTH;
    char *style_string;

    style_string = gnc_gconf_get_string(GCONF_GENERAL,
                                        KEY_TOOLBAR_STYLE, NULL);
    if (!style_string || strcmp(style_string, "system") == 0)
    {
        if (style_string)
            g_free(style_string);
        style_string = gnc_gconf_get_string(DESKTOP_GNOME_INTERFACE,
                                            KEY_TOOLBAR_STYLE, NULL);
    }

    if (style_string == NULL)
        return GTK_TOOLBAR_BOTH;
    tbstyle = gnc_enum_from_nick(GTK_TYPE_TOOLBAR_STYLE, style_string,
                                 GTK_TOOLBAR_BOTH);
    g_free(style_string);

    return tbstyle;
}


/********************************************************************\
 * gnc_get_deficit_color                                            *
 *   fill in the 3 color values for the color of deficit values     *
 *                                                                  *
 * Args: color - color structure                                    *
 * Returns: none                                                    *
 \*******************************************************************/
void
gnc_get_deficit_color(GdkColor *color)
{
    color->red   = 50000;
    color->green = 0;
    color->blue  = 0;
}


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
    GdkColormap *cm;
    GtkStyle *style;

    if (!gnc_gconf_get_bool(GCONF_GENERAL, "red_for_negative", NULL))
        return;

    cm = gtk_widget_get_colormap(GTK_WIDGET(label));
    gtk_widget_ensure_style(GTK_WIDGET(label));
    style = gtk_widget_get_style(GTK_WIDGET(label));

    style = gtk_style_copy(style);

    deficit = gnc_numeric_negative_p (value);

    if (deficit)
    {
        gnc_get_deficit_color(&style->fg[GTK_STATE_NORMAL]);
        gdk_colormap_alloc_color(cm, &style->fg[GTK_STATE_NORMAL], FALSE, TRUE);
    }
    else
        style->fg[GTK_STATE_NORMAL] = style->black;

    gtk_widget_set_style(label, style);

    g_object_unref(style);
}


/********************************************************************\
 * gnc_restore_window_size                                          *
 *   returns the window size to use for the given option prefix,    *
 *   if window sizes are being saved, otherwise returns 0 for both. *
 *                                                                  *
 * Args: prefix - the option name prefix                            *
 *       width  - pointer to width                                  *
 *       height - pointer to height                                 *
 * Returns: nothing                                                 *
 \*******************************************************************/
void
gnc_restore_window_size(const char *section, GtkWindow *window)
{
    GSList *coord_list;
    gint coords[2];

    ENTER("");

    g_return_if_fail(section != NULL);
    g_return_if_fail(window != NULL);

    if (!gnc_gconf_get_bool(GCONF_GENERAL, KEY_SAVE_GEOMETRY, NULL))
        return;

    coord_list = gnc_gconf_get_list(section, WINDOW_POSITION,
                                    GCONF_VALUE_INT, NULL);
    if (coord_list)
    {
        gint screen_width = gdk_screen_width();
        gint screen_height = gdk_screen_height();

        coords[0] = GPOINTER_TO_INT(g_slist_nth_data(coord_list, 0));
        coords[1] = GPOINTER_TO_INT(g_slist_nth_data(coord_list, 1));
        DEBUG("from gconf - coords[0]: %d, coords[1]: %d", coords[0], coords[1]);

        /* Keep the window on screen if possible */
        if (screen_width != 0)
            coords[0] = coords[0] % screen_width;
        if (screen_height != 0)
            coords[1] = coords[1] % screen_height;
        DEBUG("after screen adaption - coords[0]: %d, coords[1]: %d", coords[0], coords[1]);

        gtk_window_move(window, coords[0], coords[1]);
        g_slist_free(coord_list);
    }

    coord_list = gnc_gconf_get_list(section, WINDOW_GEOMETRY,
                                    GCONF_VALUE_INT, NULL);
    if (coord_list)
    {
        coords[0] = GPOINTER_TO_INT(g_slist_nth_data(coord_list, 0));
        coords[1] = GPOINTER_TO_INT(g_slist_nth_data(coord_list, 1));
        DEBUG("coords[0]: %d, coords[1]: %d", coords[0], coords[1]);
        if ((coords[0] != 0) && (coords[1] != 0))
            gtk_window_resize(window, coords[0], coords[1]);
        g_slist_free(coord_list);
    }

    LEAVE("");
}


/********************************************************************\
 * gnc_save_window_size                                             *
 *   save the window size into options whose names are determined   *
 *   by the string prefix.                                          *
 *                                                                  *
 * Args: prefix - determines the options used to save the values    *
 *       width  - width of the window to save                       *
 *       height - height of the window to save                      *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_save_window_size(const char *section, GtkWindow *window)
{
    GSList *coord_list = NULL;
    gint coords[2];

    g_return_if_fail(section != NULL);
    g_return_if_fail(window != NULL);

    if (!gnc_gconf_get_bool(GCONF_GENERAL, KEY_SAVE_GEOMETRY, NULL))
        return;

    gtk_window_get_size(GTK_WINDOW(window), &coords[0], &coords[1]);
    coord_list = g_slist_append(coord_list, GUINT_TO_POINTER(coords[0]));
    coord_list = g_slist_append(coord_list, GUINT_TO_POINTER(coords[1]));
    gnc_gconf_set_list(section, WINDOW_GEOMETRY, GCONF_VALUE_INT,
                       coord_list, NULL);
    g_slist_free(coord_list);
    coord_list = NULL;

    gtk_window_get_position(GTK_WINDOW(window), &coords[0], &coords[1]);
    coord_list = g_slist_append(coord_list, GUINT_TO_POINTER(coords[0]));
    coord_list = g_slist_append(coord_list, GUINT_TO_POINTER(coords[1]));
    gnc_gconf_set_list(section, WINDOW_POSITION, GCONF_VALUE_INT,
                       coord_list, NULL);
    g_slist_free(coord_list);
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
    gint screen_width;
    gint screen_height;
    gint width;
    gint height;

    if (window == NULL)
        return;

    g_return_if_fail(GTK_IS_WINDOW(window));
    if (GTK_WIDGET(window)->window == NULL)
        return;

    screen_width = gdk_screen_width();
    screen_height = gdk_screen_height();
    gdk_drawable_get_size(GTK_WIDGET(window)->window, &width, &height);

    if ((width <= screen_width) && (height <= screen_height))
        return;

    width = MIN(width, screen_width - 10);
    width = MAX(width, 0);

    height = MIN(height, screen_height - 10);
    height = MAX(height, 0);

    gdk_window_resize(GTK_WIDGET(window)->window, width, height);
    gtk_widget_queue_resize(GTK_WIDGET(window));
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
    case GDK_KP_Add:
    case GDK_plus:
    case GDK_equal:
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

    case GDK_minus:
    case GDK_KP_Subtract:
    case GDK_underscore:
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
    case GDK_braceright:
    case GDK_bracketright:
        /* increment month */
        g_date_add_months (&gdate, 1);
        break;

    case GDK_braceleft:
    case GDK_bracketleft:
        /* decrement month */
        g_date_subtract_months (&gdate, 1);
        break;

    case GDK_M:
    case GDK_m:
        /* beginning of month */
        g_date_set_day (&gdate, 1);
        break;

    case GDK_H:
    case GDK_h:
        /* end of month */
        g_date_set_day (&gdate, 1);
        g_date_add_months (&gdate, 1);
        g_date_subtract_days (&gdate, 1);
        break;

    case GDK_Y:
    case GDK_y:
        /* beginning of year */
        g_date_set_day (&gdate, 1);
        g_date_set_month (&gdate, 1);
        break;

    case GDK_R:
    case GDK_r:
        /* end of year */
        g_date_set_day (&gdate, 1);
        g_date_set_month (&gdate, 1);
        g_date_add_years (&gdate, 1);
        g_date_subtract_days (&gdate, 1);
        break;

    case GDK_T:
    case GDK_t:
    {
        /* today */
        GTime gtime;

        gtime = time (NULL);
        g_date_set_time_t (&gdate, gtime);
        break;
    }

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
gnc_gtk_dialog_add_button (GtkWidget *dialog, const gchar *label, const gchar *stock_id, guint response)
{
    GtkWidget *button;

    button = gtk_button_new_with_mnemonic(label);
    if (stock_id)
    {
        GtkWidget *image;

        image = gtk_image_new_from_stock (stock_id, GTK_ICON_SIZE_BUTTON);
        gtk_button_set_image(GTK_BUTTON(button), image);
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
gnc_dialog_run (GtkDialog *dialog, const gchar *gconf_key)
{
    GtkWidget *perm, *temp;
    gboolean ask = TRUE;
    gint response;

    /* Does the user want to see this question? If not, return the
     * previous answer. */
    response = gnc_gconf_get_int(GCONF_WARNINGS_PERM, gconf_key, NULL);
    if (response != 0)
        return response;
    response = gnc_gconf_get_int(GCONF_WARNINGS_TEMP, gconf_key, NULL);
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
    gtk_box_pack_start(GTK_BOX(dialog->vbox), perm, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(dialog->vbox), temp, TRUE, TRUE, 0);
    g_signal_connect(perm, "clicked", G_CALLBACK(gnc_perm_button_cb), temp);

    /* OK. Present the dialog. */
    response = gtk_dialog_run(dialog);
    if ((response == GTK_RESPONSE_NONE) || (response == GTK_RESPONSE_DELETE_EVENT))
    {
        return GTK_RESPONSE_NO;
    }

    if (response != GTK_RESPONSE_CANCEL)
    {
        /* Save the answer? */
        if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(perm)))
        {
            gnc_gconf_set_int(GCONF_WARNINGS_PERM, gconf_key, response, NULL);
        }
        else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(temp)))
        {
            gnc_gconf_set_int(GCONF_WARNINGS_TEMP, gconf_key, response, NULL);
        }
    }
    return response;
}
