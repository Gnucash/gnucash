/********************************************************************\
 * gnc-file-dialog.c -- the file dialog box                         *
 * Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>            *
 * Copyright (C) 1998-99 Rob Browning <rlb@cs.utexas.edu>           *
 * Copyright (C) 2000 Linas Vepstas  <linas@linas.org>              *
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
\********************************************************************/

#include "config.h"

#include <gnome.h>

#include "gnc-engine-util.h"
#include "gnc-file-dialog.h"
#include "gnc-ui.h"
#include "messages.h"

/* This static indicates the debugging module that this .o belongs to.   */
static short module = MOD_GUI;


/********************************************************************\
 * gnc_file_dialog                                                  * 
 *   Pops up a file selection dialog (either a "Save As" or an      * 
 *   "Open"), and returns the name of the file the user selected.   *
 *   (This function does not return until the user selects a file   * 
 *   or presses "Cancel" or the window manager destroy button)      * 
 *                                                                  * 
 * Args:   title        - the title of the window                   *
 *         filter       - the file filter to use                    * 
 *         default_name - the default name to use                   *
 * Return: containing the name of the file the user selected        *
\********************************************************************/

char *
gnc_file_dialog (const char * title,
                 const char * filter,
                 const char *default_name)
{
  GtkFileSelection *file_box;
  const char *internal_name;
  char *file_name = NULL;
  gint response;

  ENTER("\n");

  /* Set a default title if nothing was passed in */  
  if (title == NULL)
    title = _("Open");

  file_box = GTK_FILE_SELECTION(gtk_file_selection_new(title));

  if (default_name)
    gtk_file_selection_set_filename(file_box, default_name);

  /* hack alert - this was filtering directory names as well as file 
   * names, so I think we should not do this by default (rgmerk) */
#if 0
  if (filter != NULL)
    gtk_file_selection_complete(file_box, filter);
#endif

  gtk_window_set_modal(GTK_WINDOW(file_box), TRUE);
  gtk_window_set_transient_for(GTK_WINDOW(file_box),
			       GTK_WINDOW(gnc_ui_get_toplevel()));
  response = gtk_dialog_run(GTK_DIALOG(file_box));

  if (response == GTK_RESPONSE_OK) {
    /* look for constructs like postgres://foo */
    internal_name = gtk_entry_get_text(GTK_ENTRY(file_box->selection_entry));
    if (strstr (internal_name, "://") == 0) {
      /* nope, a local file name */
      internal_name = gtk_file_selection_get_filename(file_box);
    }
    file_name = g_strdup(internal_name);
  }
  gtk_widget_destroy(GTK_WIDGET(file_box));
  LEAVE("%s", file_name);
  return file_name;
}
