/********************************************************************\
 * main.c -- main for xacc (X-Accountant)                           *
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

#include <Xm/Xm.h>

#include "BuildMenu.h"
#include "Data.h"
#include "FileBox.h"
#include "FileIO.h"
#include "main.h"
#include "RegWindow.h"
#include "util.h"

/** PROTOTYPES ******************************************************/

/** GLOBALS *********************************************************/
char    *datafile = NULL;
Widget   toplevel = 0;
Boolean  realized = False;   /* Has the toplevel been realized? */
XtAppContext app;

/* The names of the different types of accounts.  For resource
 * specification. Must match the enums in Account.h */
String accRes[] ={
  "bank",
  "cash",
  "asset",
  "credit",
  "liability",
  "portfolio",
  "mutual",
  "income",
  "expense",
  "equity"
};

/** FALLBACK RESOURCES **********************************************/
/* NOTE: These will eventually be moved out into a site-default file,
 *       but default-resources are here for now, for convenience */
String fbRes[] = {
  "*fontList:                 -*-helvetica-bold-r-normal--*-120-*-*-*-*-*-*",
  "*Background:               grey",
  "*text.fontList:            -*-helvetica-medium-r-normal--*-100-*-*-*-*-*-*",
  /* Help stuff" */
  "*help*geometry:            530x480-0-0",
/*  "*help*View*Background:     #ffffff",  */
  /* MenuBar stuff: */
  "*menubar*marginHeight:     1",
  "*menubar*marginWidth:      1",

  /* Register window account type specific stuff: */ 
  "*regbank.oddRowBackground:      #aaccff",
  "*regcash.oddRowBackground:      #ccffcc",
  "*regasset.oddRowBackground:     #aaffcc",
  "*regcredit.oddRowBackground:    #ffffaa",
  "*regliability.oddRowBackground: #ffcccc",
  "*regportfolio.oddRowBackground: #ccffff",
  "*regmutual.oddRowBackground:    #ccffff",
  "*regincome.oddRowBackground:    #aaccff",
  "*regexpense.oddRowBackground:   #ffcccc",
  "*regequity.oddRowBackground:    #ffffaa",
  "*regportfolio.evenRowBackground:grey",
  "*regmutual.evenRowBackground:   grey",
  /* Other register window account stuff: */
  "*reg*fontList:            -*-helvetica-medium-r-normal--*-100-*-*-*-*-*-*",
  "*reg*evenRowBackground:   white",
  "*reg*shadowType:          SHADOW_IN",
  "*reg*shadowThickness:     1",
  "*reg*cellShadowThickness: 1",
  "*reg*cellShadowType:      SHADOW_IN",
  "*reg*cellMarginWidth:     1",
  "*reg*cellMarginHeight:    0",
  /* Reconcile window matrices stuff: */
  "*recn*fontList:            -*-helvetica-medium-r-normal--*-100-*-*-*-*-*-*",
  "*recn*.oddRowBackground:   white",
  "*recn*.evenRowBackground:  white",
  "*recn*shadowType:          SHADOW_ETCHED_IN",
  "*recn*shadowThickness:     1",
  "*recn*cellShadowThickness: 1",
  "*recn*cellShadowType:      SHADOW_ETCHED_IN",
  "*recn*cellMarginWidth:     0",
  "*recn*cellMarginHeight:    0",
  /* combobox -- don't want the cell to be outlined */
  "*reg*popbox*shadowThickness:     0",
  NULL,
  };

/********************************************************************\
 * main                                                             *
 *  the entry point for the program... sets up the top level widget * 
 *  and calls the mainWindow() function which creates the main      * 
 *  window.                                                         * 
 *                                                                  * 
 * Args:   argc, the number of command line arguments, and argv,    * 
 *         the array of command line args                           * 
 * Return:                                                          * 
 * Global: topgroup - the data from the datafile                    *
 *         datafile - the name of the user's datafile               *
 *         toplevel - the toplevel widget, for creating new windows *
 *         app      - the XtAppContext                              *
\********************************************************************/
int 
main( int argc, char *argv[] )
  {
#ifdef DEBUGMEMORY
  char *blk;
  DEBUG("Initializing memory");
  blk = (char *)_malloc(8192);
  _free(blk);
  printf(" coresize = %d\n",_coresize());
  DEBUG("Done initializing memory");
#endif
  
  toplevel = XtVaAppInitialize( &app, "Xacc", NULL, 0,
				&argc, argv, fbRes,
				NULL );
  
  /* read in the filename (should be the first arg after all
   * the X11 stuff */
  if( argc > 1 )
    datafile = argv[1];
  else
    datafile = fileBox( toplevel, OPEN );
  
  if( datafile != NULL )
    topgroup = readData(datafile);     /* load the accounts data from datafile*/
    /* topgroup = xaccReadQIFData(datafile);     /* load the accounts data from datafile*/
  
  if( NULL == topgroup )
    {
    topgroup = mallocAccountGroup();           /* the file could not be found */
    topgroup->new = True;
    }
  
  /* Make main window */
  mainWindow(toplevel);
  
  /* Draw toplevel */
  XtRealizeWidget(toplevel);
  realized = TRUE;
  
  /* Enter event loop */
  XtAppMainLoop(app);
  }

