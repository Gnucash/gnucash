/*-*-top-level-c-*-**************************************************\
 * main.c -- main for xacc (X-Accountant)                           *
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
#include "Group.h"
#include "top-level.h"
#include "MainWindow.h"
#include "messages.h"
#include "Transaction.h"
#include "util.h"
#include "xtutil.h"

/** PROTOTYPES ******************************************************/

/** GLOBALS *********************************************************/

char    *datafile = NULL;
AccountGroup   *toplevel;
//Boolean  realized = False;   /* Has the toplevel been realized? */
GtkWidget *app;

AccountGroup *topgroup = NULL;

/* These gnucash_lowlev and gnucash_ui functions are just hacks to get
   the guile stuff up and running.  Expect a more formal definition of
   what they should do soo, and expect that the open/select functions
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
  
  printf ("\n\n");
  printf ("This is a BETA development version.  We think everything works \n");
  printf ("but we're not sure yet.  Feel free to try this, but please do \n");
  printf ("make backups of your data.  Please report bugs and other prblems \n");
  printf ("to http://www.gnucash.org/ \n");
  printf ("The last stable version was xacc-1.0.18 \n");
  printf ("The next stable version will be gnucash-1.2.x \n");
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
  return 0;
}

int
gnucash_lowlev_app_main()
{  
  /* Make main window */
  gnc_ui_mainWindow(topgroup);
  
  /* Draw toplevel */
  gtk_widget_realize (app);
  //realized = TRUE;
  
  /* Enter gnome event loop */
  gtk_main();

  return 0;
}

int
gnucash_ui_open_file(const char name[]) {
  /* FIXME: This should eventually be merged with the FMB_OPEN code in
     MainWindow.c */
  /* FIXME: This code looks gui-independent to me ...
   *  (nothing in thisroutine depends on motif/gtk/etc.)
   */
  
  datafile = name;
  
  if( NULL != datafile ) 
  {
    xaccLogSetBaseName (datafile);
    topgroup = xaccReadAccountGroup (datafile); /* load accounts from file */
  }
  
  /* FIXME: this should not really be here.  We should make
     gnc_ui_select_file more independent.  You should be able to call
     it at any time and have it DTRT.  Code should be stolen from
     MainWindow.c FMB_OPEN.  In fact, that whole callback routine
     should probably be broken up into sub-functions that we can use
     from the Guile level.  */
  if( NULL == topgroup )
  {
    topgroup = xaccMallocAccountGroup();   /* the file could not be found */
  }
  return (topgroup != NULL);
  
}

int
gnucash_ui_select_file() {

  datafile = fileBox( toplevel, OPEN_STR, "*.xac" );
  gnucash_ui_open_file(datafile);
  return (topgroup != NULL);
}

/****************** END OF FILE **********************/
