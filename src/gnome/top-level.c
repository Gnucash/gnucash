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
#include <gnome.h>

#include "config.h"

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

GtkWidget *app;

gncUIWidget gnc_get_ui_data() {
  return app;
}

/* These gnucash_lowlev and gnucash_ui functions are just hacks to get
   the guile stuff up and running.  Expect a more formal definition of
   what they should do soon, and expect that the open/select functions
   will be merged with the code in FMB_OPEN in MainWindow.c */

int
gnucash_lowlev_app_init()
{
#if DEBUG_MEMORY
  char *blk;
  DEBUG("Initializing memory");
  blk = (char *)_malloc(8192);
  _free(blk);
  printf(" coresize = %d\n",_coresize());
  DEBUG("Done initializing memory");
#endif
  ENTER ("gnucash_lowlev_app_init");
  
  printf ("\n\n");
  printf ("This is a development version.  It may or may not work \n");
  printf ("Report bugs and other problems to http://www.gnucash.org/ \n");
  printf ("The last stable version was xacc-1.2.0 \n");
  printf ("The next stable version will be gnucash-1.4.x \n");
  printf ("\n\n");
  
  {
    int fake_argc = 1;
    char *fake_argv[] = {"gnucash"};
   
    /* We're going to have to have other ways to handle X and GUI
       specific args...

       For now, use fake_argv and fake_argc...
    */
    gnome_init("GnuCash", NULL, fake_argc, fake_argv);  
    app = gnome_app_new ( "gnucash", "GnuCash" );
    
    {
      /* Use a nicer font IMO, if available */
      char font[] = "-adobe-helvetica-medium-r-normal--*-100-*-*-*-*-*-*";
      GtkStyle *st = gtk_widget_get_default_style();
      GdkFont *f = gdk_font_load(font);
      if(st && f) {
        st->font = f;
      }
    }
  } 

  LEAVE ("gnucash_lowlev_app_init");
  return 0;
}

int
gnucash_lowlev_app_main()
{  
  /* Make main window */
  mainWindow();
  
  gtk_widget_realize (app);
  
  /* Enter gnome event loop */
  gtk_main();

  return 0;
}

/* hack alert -- all we do below is rename some functions ... fix this someday */

int
gnucash_ui_open_file(const char name[]) {
  gncFileOpenFile(name);
  return (1);
}

int
gnucash_ui_select_file() {
  gncFileOpen();
  return (1);
}

/****************** END OF FILE **********************/
