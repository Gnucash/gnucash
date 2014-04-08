/* MenuBar.c -- GTK menu functions for xacc (X-Accountant) Copyright
   (C) 1998 Jeremy Collins <linux@cyberramp.net>
   (C) 1998 Rob Browning <rlb@cs.utexas.edu>
                                                                   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.
                                                                   
   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
                                                                   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
                                                                   
   Author: Jeremy Collins
   Internet: linux@cyberramp.net
*/

#include <gtk/gtk.h>
#include <strings.h>
#include "MenuBar.h"

#if 0
static GHashTable *entry_ht = NULL;

static gint
menus_install_accel(GtkWidget * widget, gchar * signal_name, gchar
                    key, gchar modifiers, gchar * path)
{
  char accel[64];
  char *t1, t2[2];
  
  accel[0] = '\0';
  if (modifiers & GDK_CONTROL_MASK) strcat(accel, "<control>");
  if (modifiers & GDK_SHIFT_MASK) strcat(accel, "<shift>");
  if (modifiers & GDK_MOD1_MASK) strcat(accel, "<alt>");
  
  t2[0] = key;
  t2[1] = '\0';
  strcat(accel, t2);
  
  if (entry_ht) {
    t1 = g_hash_table_lookup(entry_ht, path);
    g_free(t1);
  } else {
    entry_ht = g_hash_table_new(g_str_hash, g_str_equal);
  }
  g_hash_table_insert(entry_ht, path, g_strdup(accel));
  
  return TRUE;
}

static void
menus_remove_accel(GtkWidget * widget, gchar * signal_name, gchar *path)
{
  char *t;
  
  if (entry_ht) {
   t = g_hash_table_lookup(entry_ht, path);
    g_free(t);
    
    g_hash_table_insert(entry_ht, path, g_strdup(""));
  }
}

#endif

MenuBarGroup *menuBarGroupCreate()
{
  GtkMenuFactory * f;
  f = gtk_menu_factory_new(GTK_MENU_FACTORY_MENU_BAR);
  return(f);
}

void
menuBarGroupAddItems(MenuBarGroup *group,
                     GtkMenuEntry items[], guint nitems)
{
#if 0
  int i;
  char *accelerator;

  if (entry_ht) {
    for (i = 0; i < nitems; i++) {
      accelerator = g_hash_table_lookup(entry_ht, items[i].path);
      if (accelerator) {
        if (accelerator[0] == '\0')
          items[i].accelerator = NULL;
        else
          items[i].accelerator = accelerator;
      }
    }
  }
#endif

  gtk_menu_factory_add_entries(group, items, nitems);
    
#if 0
  for(i = 0; i < nitems; i++)
  {
    if (items[i].widget) {
      gtk_signal_connect(GTK_OBJECT(items[i].widget), "install_accelerator",
                         GTK_SIGNAL_FUNC(menus_install_accel),
                         items[i].path);
      gtk_signal_connect(GTK_OBJECT(items[i].widget), "remove_accelerator",
                         GTK_SIGNAL_FUNC(menus_remove_accel),
                         items[i].path);
    }
  }
#endif

}

MenuBar *menuBarCreate(MenuBarGroup *group, const gchar menuBarName[])
{
  GtkMenuFactory *subfactory = gtk_menu_factory_new(GTK_MENU_FACTORY_MENU_BAR);
  gtk_menu_factory_add_subfactory(group, subfactory, menuBarName);

  return(subfactory);
}


GtkWidget *menuBarGroupFindItem(MenuBarGroup *group, const gchar path[])
{
  GtkMenuPath *mp;
  mp = gtk_menu_factory_find(group, path);
  if(mp) {
    return(mp->widget);
  } else {
    return NULL;
  }
}

GtkWidget    *menuBarGetWidget(MenuBar *mb)
{
  return mb->widget;
}

/*
  Local Variables:
  tab-width: 2
  indent-tabs-mode: nil
  mode: c-mode
  c-indentation-style: gnu
  eval: (c-set-offset 'block-open '-)
  End:
*/
