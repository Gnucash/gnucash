/*******************************************************************\
 * window-main.h -- private GNOME main window functions             *
 * Copyright (C) 1997 Robin D. Clark                                *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#ifndef __WINDOW_MAINP_H__
#define __WINDOW_MAINP_H__


typedef struct _GNCMainInfo GNCMainInfo;
struct _GNCMainInfo
{
  GtkWidget *account_tree;
  GtkWidget *assets_label;
  GtkWidget *profits_label;

  SCM main_window_change_callback_id;
  SCM toolbar_change_callback_id;

  GSList *account_sensitives;
};


/** PROTOTYPES ******************************************************/
static void gnc_ui_refresh_statusbar(void);
static void gnc_ui_exit_cb(GtkWidget *widget, gpointer data);
static void gnc_ui_about_cb(GtkWidget *widget, gpointer data);
static void gnc_ui_help_cb(GtkWidget *widget, gpointer data);
static void gnc_ui_add_account(GtkWidget *widget, gpointer data);
static void gnc_ui_delete_account_cb(GtkWidget *widget, gpointer data);
static void gnc_ui_mainWindow_toolbar_open(GtkWidget *widget, gpointer data);
static void gnc_ui_mainWindow_toolbar_open_subs(GtkWidget *widget,
                                                gpointer data);
static void gnc_ui_mainWindow_toolbar_edit(GtkWidget *widget, gpointer data);
static void gnc_ui_mainWindow_reconcile(GtkWidget *widget, gpointer data);
static void gnc_ui_mainWindow_transfer(GtkWidget *widget, gpointer data);
static void gnc_ui_mainWindow_adjust_balance(GtkWidget *widget, gpointer data);
static void gnc_ui_mainWindow_scrub(GtkWidget *widget, gpointer data);
static void gnc_ui_mainWindow_scrub_sub(GtkWidget *widget, gpointer data);
static void gnc_ui_mainWindow_scrub_all(GtkWidget *widget, gpointer data);
static void gnc_ui_options_cb(GtkWidget *widget, gpointer data);
static void gnc_ui_filemenu_cb(GtkWidget *widget, gpointer menuItem);

static GNCMainInfo * gnc_get_main_info();

static gboolean gnc_ui_mainWindow_delete_cb(GtkWidget *widget,
                                            GdkEvent *event,
                                            gpointer user_data);

static gboolean gnc_ui_mainWindow_destroy_event_cb(GtkWidget *widget,
                                                   GdkEvent *event,
                                                   gpointer user_data);

static void gnc_ui_mainWindow_destroy_cb(GtkObject *object,
                                         gpointer user_data);

#endif
