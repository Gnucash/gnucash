/********************************************************************\
 * top-level.c -- main for xacc (X-Accountant)                           *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
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
\********************************************************************/

#include <stdlib.h>
#include <assert.h>
#include <guile/gh.h>
#include <gnome.h>

#include "config.h"

#include "window-main.h"
#include "Account.h"
#include "FileIO.h"
#include "FileBox.h"
#include "FileDialog.h"
#include "Group.h"
#include "MainWindow.h"
#include "messages.h"
#include "Transaction.h"
#include "TransLog.h"
#include "util.h"
#include "top-level.h"

/** PROTOTYPES ******************************************************/

/** GLOBALS *********************************************************/
/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static GtkWidget *app = NULL;

static int gnome_is_running = FALSE;
static int gnome_is_initialized = FALSE;
static int gnome_is_terminating = FALSE;

/* ============================================================== */

int 
gnucash_ui_is_running() {
  return gnome_is_running;
}

/* ============================================================== */

int 
gnucash_ui_is_terminating() {
  return gnome_is_terminating;
}

/* ============================================================== */

gncUIWidget
gnc_get_ui_data() {
  return app;
}

/* ============================================================== */

/* These gnucash_ui_init and gnucash_ui functions are just hacks to get
   the guile stuff up and running.  Expect a more formal definition of
   what they should do soon, and expect that the open/select functions
   will be merged with the code in FMB_OPEN in MainWindow.c */

int
gnucash_ui_init()
{
  int fake_argc = 1;
  char *fake_argv[] = {"gnucash"};

  ENTER ("gnucash_ui_init");

  /* We're going to have to have other ways to handle X and GUI
     specific args...

     For now, use fake_argv and fake_argc...
  */
  if (!gnome_is_initialized) {
    gnome_init("GnuCash", NULL, fake_argc, fake_argv);
    gnome_is_initialized = TRUE;

    app = gnome_app_new ( "GnuCash", "GnuCash" );

    {
      /* Use a nicer font IMO, if available */
      char font[] = "-adobe-helvetica-medium-r-normal--*-100-*-*-*-*-*-*";
      GtkStyle *st = gtk_widget_get_default_style();
      GdkFont *f = gdk_font_load(font);
      if(st && f) {
	st->font = f;
      }
    }

    /* Make the main window. */
    mainWindow();
  }

  LEAVE ("gnucash_ui_init");

  return 0;
}

/* ============================================================== */

void
gnc_ui_shutdown (void)
{
  if (gnome_is_running && !gnome_is_terminating) {
    gnome_is_terminating = TRUE;
    gtk_widget_hide(app);
    gtk_main_quit();
  }
}

/* ============================================================== */

void
gnc_ui_destroy (void)
{
  if (!gnome_is_initialized)
    return;

  if (app != NULL) {
    gtk_widget_destroy(app);
    app = NULL;
  }
}

/* ============================================================== */

int
gnc_ui_main()
{
  /* Initialize gnome */
  gnucash_ui_init();

  gnc_refresh_main_window();
  gtk_widget_show(app);

  gnome_is_running = TRUE;

  /* Enter gnome event loop */
  gtk_main();

  gnome_is_running = FALSE;
  gnome_is_terminating = FALSE;

  return 0;
}

/* hack alert -- all we do below is rename some functions ... fix this someday */
/* ============================================================== */

int
gnucash_ui_open_file(const char name[]) {
  gncFileOpenFile(name);
  return (1);
}

/* ============================================================== */

int
gnucash_ui_select_file() {
  gncFileOpen();
  return (1);
}

/****************** END OF FILE **********************/
