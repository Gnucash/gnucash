/********************************************************************\
 * gnc-mdi-util.c -- utility functions for gnome/mdi                *
 * Copyright (C) 2001 Linux Developers Group                        *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"
#include "global-options.h"
#include "gnc-html.h"
#include "gnc-mdi-utils.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"


static GNCMDIInfo *gnc_mdi_current = NULL;


gncUIWidget
gnc_ui_get_toplevel (void)
{
  GList *containers = gtk_container_get_toplevels ();
  GnomeApp *app = NULL;
  
  for (; containers; containers = containers->next)
  {
    GtkWidget *w = containers->data;
    GNCMDIInfo *gnc_mdi;

    if (!GNOME_IS_APP (w))
      continue;

    app = GNOME_APP (w);

    gnc_mdi = gtk_object_get_data (GTK_OBJECT (w), "gnc_mdi");
    if (!gnc_mdi)
      continue;

    app = gnome_mdi_get_active_window (gnc_mdi->mdi);

    break;
  }

  if (app)
    return GTK_WIDGET (app);

  return NULL;
}

static void
gnc_mdi_child_set_title (GNCMDIChildInfo *childwin)
{
  GNCBook *book;
  const char *filename;
  char *title;

  if (!childwin || !childwin->app)
    return;

  filename = gnc_book_get_url (gnc_get_current_book ());

  if (!filename)
    filename = _("<no file>");
  else if (strncmp ("file:", filename, 5) == 0)
    filename += 5;

  if (!childwin->title)
    title = g_strdup_printf ("%s (%s)", childwin->gnc_mdi->title, filename);
  else
    title = g_strdup_printf ("%s - %s (%s)", childwin->title,
                             childwin->gnc_mdi->title, filename);

  gtk_window_set_title (GTK_WINDOW(childwin->app), title);

  g_free (title);
}

static void 
gnc_mdi_app_destroyed_cb (GnomeApp * app, gpointer user_data)
{
  GNCMDIInfo * mainwin = user_data;
  GnomeMDI * mdi = mainwin->mdi;

  if (mainwin->shutdown && (g_list_length (mdi->windows) == 0))
    mainwin->shutdown (0);
  else
  {
    GtkWidget *toolbar = gtk_object_get_user_data (GTK_OBJECT (app));
    GList * child; 

    if (toolbar)
      gtk_widget_unref (toolbar);

    gtk_object_set_user_data (GTK_OBJECT (app), NULL);

    for (child = mainwin->children; child; child = child->next)
    {
      GNCMDIChildInfo * mc = child->data;

      if (mc && mc->toolbar && mc->app && (mc->app == app))
      {
        /* we need to pull the toolbar out to prevent its being destroyed */
        gtk_widget_ref (mc->toolbar);
        gtk_container_remove (GTK_CONTAINER(mc->toolbar->parent), mc->toolbar);
      }
    }
  }
}

static void
gnc_mdi_app_created_cb (GnomeMDI * mdi, GnomeApp * app, gpointer data)
{
  GNCMDIInfo * mainwin = data;

  /* enable save and restore of menubar positions */
  gnome_app_enable_layout_config (app, TRUE);

  /* flag the app as gnc mdi created */
  gtk_object_set_data (GTK_OBJECT (app), "gnc_mdi", mainwin);

  /* add a signal to preserve the toolbar on destroy */ 
  gtk_signal_connect (GTK_OBJECT (app), "destroy", 
                      GTK_SIGNAL_FUNC (gnc_mdi_app_destroyed_cb),
                      mainwin);
}

static void
gnc_mdi_destroy_cb (GtkObject * w, gpointer data)
{
  GNCMDIInfo * gnc_mdi = data;

  gnc_mdi->mdi = NULL;

  if (gnc_mdi->shutdown)
    gnc_mdi->shutdown (0);

  if (gnc_mdi_current == gnc_mdi)
    gnc_mdi_current = NULL;

  g_free (gnc_mdi);
}

static void
gnc_mdi_child_changed_cb (GnomeMDI * mdi, GnomeMDIChild * not_used,
                          gpointer data)
{
  GNCMDIInfo      * mainwin = data;
  GNCMDIChildInfo * childwin = NULL;
  GnomeUIInfo      * hintinfo;
  GtkWidget        * oldbar;
  GnomeApp         * new_app = NULL; 
  GnomeDockItemBehavior behavior;

  if (mdi && mdi->active_child)
  {
    childwin = gtk_object_get_user_data (GTK_OBJECT(mdi->active_child));
    new_app = gnome_mdi_get_app_from_view (childwin->contents);
  }

  behavior = GNOME_DOCK_ITEM_BEH_EXCLUSIVE;
  if (!gnome_preferences_get_toolbar_detachable ())
    behavior |= GNOME_DOCK_ITEM_BEH_LOCKED;

  if (childwin && childwin->toolbar)
  {
    if (childwin->app && (childwin->app == new_app))
    {
      oldbar = gtk_object_get_user_data (GTK_OBJECT(new_app));
      if (oldbar && (oldbar != childwin->toolbar))
      {
        if (oldbar->parent)
          gtk_widget_hide (GTK_WIDGET(oldbar)->parent);        
        gtk_widget_show (GTK_WIDGET(childwin->toolbar)->parent);
      }
    }
    else if (childwin->app)
    {
      oldbar = gtk_object_get_user_data (GTK_OBJECT(new_app));
      if (oldbar && oldbar->parent && (oldbar != childwin->toolbar))
        gtk_widget_hide (GTK_WIDGET(oldbar)->parent);        

      /* we need to move the toolbar to a new App (mdi mode probably
       * changed) */
      if (GTK_WIDGET(childwin->toolbar)->parent)
      {
        gtk_widget_ref (GTK_WIDGET(childwin->toolbar));
        gtk_container_remove (GTK_CONTAINER
                              (GTK_WIDGET(childwin->toolbar)->parent),
                              GTK_WIDGET(childwin->toolbar));
      }

      childwin->app = new_app;
      gnome_app_add_toolbar (GNOME_APP(childwin->app), 
                             GTK_TOOLBAR(childwin->toolbar),
                             "Toolbar", behavior,
                             GNOME_DOCK_TOP, 1, 0, 0);

      gtk_toolbar_set_style (GTK_TOOLBAR(childwin->toolbar), 
                             gnc_get_toolbar_style ());
    }
    else
    {
      oldbar = gtk_object_get_user_data (GTK_OBJECT(new_app));
      if (oldbar && oldbar->parent && (oldbar != childwin->toolbar))
        gtk_widget_hide (GTK_WIDGET(oldbar)->parent);        

      childwin->app = new_app;
      gnome_app_add_toolbar (GNOME_APP(childwin->app), 
                             GTK_TOOLBAR(childwin->toolbar),
                             "Toolbar", behavior,
                             GNOME_DOCK_TOP, 1, 0, 0);

      gtk_toolbar_set_style (GTK_TOOLBAR(childwin->toolbar), 
                             gnc_get_toolbar_style ());
    }

    oldbar = gtk_object_get_user_data (GTK_OBJECT(new_app));
    if (oldbar)
      gtk_widget_unref (oldbar);

    if (childwin->toolbar)
      gtk_widget_ref (childwin->toolbar);

    gtk_object_set_user_data (GTK_OBJECT(new_app), childwin->toolbar);
  }

  /* set the window title */ 
  gnc_mdi_child_set_title (childwin);

  /* install menu hints if relevant */
  if (mdi->active_child)
  {
    /* the arg to this callback is SUPPOSED to be the last active child, 
     * but it gets to be NULL under some circumstances */
    hintinfo = gnome_mdi_get_menubar_info (new_app);
    if (hintinfo)
      gnome_app_install_menu_hints (new_app, hintinfo);
    
    hintinfo = gnome_mdi_get_child_menu_info (new_app);
    if (hintinfo)
      gnome_app_install_menu_hints (new_app, hintinfo);
  }
}

static void
gnc_mdi_configure_toolbar_cb (gpointer data)
{
  GNCMDIInfo * mi = data; 
  GtkToolbarStyle tbstyle;
  GList * child;

  tbstyle = gnc_get_toolbar_style ();

  for (child = mi->children; child; child = child->next)
  {
    GNCMDIChildInfo * mc = child->data;

    if (mc && mc->toolbar)
      gtk_toolbar_set_style (GTK_TOOLBAR(mc->toolbar), tbstyle);
  }
}

static void
gnc_mdi_configure_mdi_cb (gpointer data)
{
  GNCMDIInfo * mi = data; 

  gnome_mdi_set_mode (mi->mdi, gnc_get_mdi_mode ());
}

static int
gnc_ui_info_size (GnomeUIInfo *ui_info)
{
  int size;

  if (!ui_info) return 0;

  for (size = 0; ui_info[size].type != GNOME_APP_UI_ENDOFINFO; size++)
    ;

  return size;
}

static GnomeUIInfo *
gnc_ui_info_concat (GnomeUIInfo *first_info, ...)
{
  GnomeUIInfo end = GNOMEUIINFO_END;
  GnomeUIInfo *ui_info;
  GnomeUIInfo *next_info;
  va_list ap;
  int index;

  va_start (ap, first_info);

  next_info = first_info;
  index = 0;

  while (TRUE)
  {
    index += gnc_ui_info_size (next_info);

    next_info = va_arg (ap, GnomeUIInfo *);
    if (!next_info)
      break;
  }

  ui_info = g_new0 (GnomeUIInfo, index + 1);

  va_end (ap);

  va_start (ap, first_info);

  next_info = first_info;
  index = 0;

  while (TRUE)
  {
    int i, len;

    len = gnc_ui_info_size (next_info);
    for (i = 0; i < len; i++, index++)
      ui_info[index] = next_info[i];

    next_info = va_arg (ap, GnomeUIInfo *);
    if (!next_info)
      break;
  }

  ui_info[index] = end;

  va_end (ap);

  return ui_info;
}

GNCMDIInfo *
gnc_mdi_new (const char *app_name,
             const char *title,
             GnomeUIInfo *toolbar_prefix,
             GnomeUIInfo *toolbar_suffix,
             GNCShutdownFunc shutdown,
             GNCMDICanRestoreCB can_restore_cb,
             GNCMDIRestoreCB restore_cb)
{
  GNCMDIInfo * gnc_mdi;

  if (gnc_mdi_current)
    return gnc_mdi_current;

  g_return_val_if_fail (app_name != NULL, NULL);
  g_return_val_if_fail (title != NULL, NULL);
  g_return_val_if_fail (can_restore_cb != NULL, NULL);
  g_return_val_if_fail (restore_cb != NULL, NULL);

  gnc_mdi = g_new0 (GNCMDIInfo, 1);

  gnc_mdi->app_name = g_strdup (app_name);
  gnc_mdi->title = g_strdup (title);
  gnc_mdi->toolbar_prefix = gnc_ui_info_concat (toolbar_prefix, NULL);
  gnc_mdi->toolbar_suffix = gnc_ui_info_concat (toolbar_suffix, NULL);
  gnc_mdi->shutdown = shutdown;
  gnc_mdi->can_restore_cb = can_restore_cb;
  gnc_mdi->restore_cb = restore_cb;

  gnc_mdi->mdi = GNOME_MDI (gnome_mdi_new (app_name, title));

  gtk_signal_connect (GTK_OBJECT(gnc_mdi->mdi), "destroy",
                      GTK_SIGNAL_FUNC(gnc_mdi_destroy_cb),
                      gnc_mdi);

  gtk_signal_connect (GTK_OBJECT(gnc_mdi->mdi), "app_created",
                      GTK_SIGNAL_FUNC(gnc_mdi_app_created_cb),
                      gnc_mdi);

  gtk_signal_connect (GTK_OBJECT(gnc_mdi->mdi), "child_changed",
                      GTK_SIGNAL_FUNC(gnc_mdi_child_changed_cb),
                      gnc_mdi);

  gnc_mdi->toolbar_change_callback_id =
    gnc_register_option_change_callback (gnc_mdi_configure_toolbar_cb, 
                                         gnc_mdi,
                                         "General", "Toolbar Buttons");

  gnc_mdi->mdi_change_callback_id =
    gnc_register_option_change_callback (gnc_mdi_configure_mdi_cb, 
                                         gnc_mdi,
                                         "General", "Application MDI mode");

  gnome_mdi_set_mode (gnc_mdi->mdi, gnc_get_mdi_mode ());

  gnc_mdi_current = gnc_mdi;

  return gnc_mdi;
}

static char * 
gnc_mdi_child_save_func (GnomeMDIChild * child, gpointer user_data)
{
  return g_strdup (child->name);
}

void
gnc_mdi_add_child (GNCMDIInfo * wind, GNCMDIChildInfo * child)
{
  g_return_if_fail (wind != NULL);
  g_return_if_fail (child != NULL);

  wind->children = g_list_append (wind->children, child);
  child->gnc_mdi = wind;

  if (GNOME_IS_MDI_GENERIC_CHILD (child->child))
  {
    GnomeMDIGenericChild *mdi_child;

    mdi_child = GNOME_MDI_GENERIC_CHILD (child->child);

    gnome_mdi_generic_child_set_config_func (mdi_child,
                                             gnc_mdi_child_save_func, NULL);
  }
}

void
gnc_mdi_remove_child (GNCMDIInfo * gnc_mdi, GNCMDIChildInfo * child)
{
  if (!gnc_mdi || !child) return;

  gnc_mdi->children = g_list_remove (gnc_mdi->children, child);
}

void
gnc_mdi_child_refresh (GNCMDIChildInfo *child)
{
  g_return_if_fail (child != NULL);

  gnome_mdi_child_set_name (child->child, child->child->name);
  gnome_mdi_update_child (child->gnc_mdi->mdi, child->child);

  /* pesky child_set_name tries to change the window title. Set it back. */
  if ((child->gnc_mdi->mdi->active_child == child->child) && child->app)
    gnc_mdi_child_set_title (child);
}

GNCMDIInfo *
gnc_mdi_get_current (void)
{
  return gnc_mdi_current;
}

gboolean
gnc_mdi_has_apps (void)
{
  GList *toplevels;

  for (toplevels = gtk_container_get_toplevels ();
       toplevels;
       toplevels = toplevels->next)
  {
    GNCMDIInfo *gnc_mdi;

    if (!GNOME_IS_APP (toplevels->data))
      continue;

    if (GTK_OBJECT_DESTROYED (toplevels->data))
      continue;

    gnc_mdi = gtk_object_get_data (GTK_OBJECT (toplevels->data), "gnc_mdi");
    if (!gnc_mdi)
      continue;

    return TRUE;
  }

  return FALSE;
}

void
gnc_app_set_title (GnomeApp *app)
{
  GNCMDIChildInfo *childwin;
  GNCMDIInfo *mainwin;
  GnomeMDIChild *child;
  GtkWidget *view;

  g_return_if_fail (app != NULL);

  mainwin = gtk_object_get_data (GTK_OBJECT (app), "gnc_mdi");
  if (!mainwin || !mainwin->mdi)
    return;

  view = gnome_mdi_get_view_from_window (mainwin->mdi, app);
  if (!view) return;

  child = gnome_mdi_get_child_from_view (view);
  if (!child) return;

  childwin = gtk_object_get_user_data (GTK_OBJECT (child));
  if (!childwin) return;

  gnc_mdi_child_set_title (childwin);
}

void
gnc_mdi_destroy (GNCMDIInfo * gnc_mdi)
{
  if (!gnc_mdi) return;

  gnc_mdi->shutdown = NULL;

  if (gnc_mdi->mdi)
    gtk_object_destroy (GTK_OBJECT (gnc_mdi->mdi));
}

void
gnc_mdi_save (GNCMDIInfo * gnc_mdi, char * filename)
{
  char * encoded;
  char * session_name;

  if (!gnc_mdi)
    return;

  encoded = gnc_html_encode_string (filename);
  session_name = g_strdup_printf ("/%s/MDI : %s",
                                  gnc_mdi->app_name,
                                  encoded ? encoded : "");
  g_free (encoded);

  if (filename && *filename != '\0')
    gnome_mdi_save_state (GNOME_MDI (gnc_mdi->mdi), session_name);

  g_free (session_name);
}

void
gnc_mdi_restore (GNCMDIInfo * gnc_mdi, const char * filename)
{
  char * encoded;
  char * session_name;
  GList * old_children;
  GList * c;

  old_children = g_list_copy (gnc_mdi->mdi->children);
  encoded = gnc_html_encode_string (filename);
  session_name = g_strdup_printf ("/%s/MDI : %s",
                                  gnc_mdi->app_name,
                                  encoded ? encoded : "");
  g_free (encoded);

  if (!filename ||
      *filename == '\0' ||
      !gnc_mdi->can_restore_cb (filename))
    gnc_mdi->restore_cb (NULL);
  else if (!gnome_mdi_restore_state (GNOME_MDI(gnc_mdi->mdi),
                                     session_name, gnc_mdi->restore_cb) ||
           gnc_mdi->mdi->children == NULL)
    gnc_mdi->restore_cb (NULL);

  g_free (session_name);

  for (c = old_children; c ; c = c->next)
    gnome_mdi_remove_child (gnc_mdi->mdi, GNOME_MDI_CHILD(c->data), TRUE);

  g_list_free (old_children);
}

void
gnc_mdi_create_child_toolbar (GNCMDIInfo * mi, GNCMDIChildInfo * child)
{
  GnomeUIInfo end = GNOMEUIINFO_END;
  GnomeUIInfo * tbinfo;
  GnomeUIInfo * cur;
  GtkToolbar  * tb;

  g_return_if_fail (mi != NULL);
  g_return_if_fail (child != NULL);

  tbinfo = gnc_ui_info_concat (mi->toolbar_prefix,
                               child->toolbar_info,
                               mi->toolbar_suffix,
                               NULL);

  g_free (child->toolbar_info);
  child->toolbar_info = tbinfo;

  tb = GTK_TOOLBAR (gtk_toolbar_new (GTK_ORIENTATION_HORIZONTAL,
                                     GTK_TOOLBAR_BOTH));

  child->toolbar = GTK_WIDGET (tb);

  gnome_app_fill_toolbar (tb, tbinfo, NULL);
}
