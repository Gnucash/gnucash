/********************************************************************\
 * gnc-mdi-utils.h -- utility functions for gnome/mdi               *
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

#ifndef GNC_MDI_UTILS_H
#define GNC_MDI_UTILS_H

#include <gnome.h>
#include <guile/gh.h>

typedef struct gnc_mdi_child_info GNCMDIChildInfo;

typedef void (*GNCShutdownFunc) (int exit_status);
typedef gboolean (*GNCMDICanRestoreCB) (const char * filename);
typedef GnomeMDIChild * (*GNCMDIRestoreCB) (const char *config_string);

typedef struct
{
  GnomeMDI * mdi;

  char     * app_name;
  char     * title;

  GnomeUIInfo *toolbar_prefix;
  GnomeUIInfo *toolbar_suffix;

  int      component_id;

  SCM      toolbar_change_callback_id;
  SCM      mdi_change_callback_id;

  GList    * children;

  GNCShutdownFunc shutdown;

  GNCMDICanRestoreCB can_restore_cb;
  GNCMDIRestoreCB restore_cb;
} GNCMDIInfo;

struct gnc_mdi_child_info
{
  GnomeMDIChild   * child;
  GNCMDIInfo      * gnc_mdi;
  GtkWidget       * contents;
  GnomeApp        * app;

  GtkWidget       * toolbar;  
  GnomeUIInfo     * toolbar_info;
  GnomeUIInfo     * menu_info;

  int             component_id;
  void            * user_data;
  char            * title;
};


GNCMDIInfo * gnc_mdi_new (const char *app_name,
                          const char *title,
                          GnomeUIInfo *toolbar_prefix,
                          GnomeUIInfo *toolbar_suffix,
                          GNCShutdownFunc shutdown,
                          GNCMDICanRestoreCB can_restore_cb,
                          GNCMDIRestoreCB restore_cb);
void gnc_mdi_destroy (GNCMDIInfo * gnc_mdi);

void gnc_mdi_add_child (GNCMDIInfo * wind, GNCMDIChildInfo * child);
void gnc_mdi_remove_child (GNCMDIInfo * gnc_mdi, GNCMDIChildInfo * child);
void gnc_mdi_child_refresh (GNCMDIChildInfo *child);

GNCMDIInfo * gnc_mdi_get_current (void);
gboolean gnc_mdi_has_apps (void);

void gnc_app_set_title (GnomeApp *app);

void gnc_mdi_save (GNCMDIInfo * gnc_mdi, char * filename);
void gnc_mdi_restore (GNCMDIInfo * gnc_mdi, const char * filename);

void gnc_mdi_create_child_toolbar (GNCMDIInfo * mi, GNCMDIChildInfo * child);

#endif
