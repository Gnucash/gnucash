/********************************************************************\
 * MainWindow.c -- the main window, and associated helper functions * 
 *                 and callback functions for xacc (X-Accountant)   *
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
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/MainW.h>
#include <Xm/Label.h>
#include <Xm/LabelGP.h>
#include <Xm/List.h>
#include <Xm/RowColumn.h>
#include <Xbae/Matrix.h>
#include "main.h"
#include "util.h"
#include "Data.h"
#include "Account.h"
#include "FileIO.h"
#include "FileBox.h"
#include "BuildMenu.h"
#include "MainWindow.h"
#include "RegWindow.h"
#include "XferWindow.h"
#include "HelpWindow.h"

/** PROTOTYPES ******************************************************/
void closeMainWindow( Widget mw, XtPointer cd, XtPointer cb );
void listCB( Widget mw, XtPointer cd, XtPointer cb );
void fileMenubarCB( Widget mw, XtPointer cd, XtPointer cb );
void accountMenubarCB( Widget mw, XtPointer cd, XtPointer cb );
void helpMenubarCB( Widget mw, XtPointer cd, XtPointer cb );

/** GLOBALS *********************************************************/
extern Data   *data;
extern char   *datafile;
extern Widget toplevel;
int    row;              /* The selected row of accountlist */
Widget accountlist;
char   *type[] = { "Bank","Cash","Asset","Credit Card",
		   "Liability","Portfolio","Mutual Fund" };
/* Pixel values are used to color the balance field text 
 * when computing the balance */
#ifndef USE_NO_COLOR
#  define POSITIVE_BALANCE "black"
#  define NEGATIVE_BALANCE "red"
Pixel   posPixel, negPixel;
Boolean havePixels = False;
#endif

/********************************************************************\
 * refreshMainWindow                                                *
 *                                                                  *
 * Args:   none                                                     *
 * Return: none                                                     *
 * Global: data        - the data from the datafile                 *
 *         accountlist - the widget that has the list of accounts   *
\********************************************************************/
void
refreshMainWindow( void )
  {
  int   i,j,nrows;
  char  buf[BUFSIZE];
  
  XtVaGetValues( accountlist, XmNrows, &nrows, NULL );
  XbaeMatrixDeleteRows( accountlist, 0, nrows );
  
  /* Add all the accounts to the list */
  for( i=0; i<data->numAcc; i++ )
    {
    String rows[3];
    Transaction *trans=NULL;
    Account *acc = getAccount( data, i );
    double dbalance = 0.0;
    double share_balance = 0.0;
    
    j=0;
    while( (trans = getTransaction(acc,j++)) != NULL ) {
      share_balance += xaccGetAmount (acc, trans);
      dbalance = share_balance * trans->share_price;
    }
    
    if( 0.0 > dbalance )
      sprintf( buf,"-$%.2f\0", DABS(dbalance) );
    else
      sprintf( buf,"$%.2f\0", DABS(dbalance) );
    
    rows[0] = acc->accountName;
    rows[1] = type[acc->type];
    rows[2] = XtNewString(buf);
    XtVaGetValues( accountlist, XmNrows, &nrows, NULL );
    XbaeMatrixAddRows( accountlist, nrows, rows, NULL, NULL, 1 );
    
#ifndef USE_NO_COLOR
    /* Set the color of the text, depending on whether the
     * balance is negative or positive */
    if( 0.0 > dbalance )
      XbaeMatrixSetCellColor( accountlist, nrows, 2, negPixel );
    else
      XbaeMatrixSetCellColor( accountlist, nrows, 2, posPixel );    
#endif
    }
  }

/********************************************************************\
 * mainWindow -- the main window... (normally) the first window     *
 *   that pops up.  Has list of accounts that the user can open.    *
 *   Opening an account produces a register window.  The user       *
 *   can also create new accounts and edit existing accounts.       *
 *                                                                  *
 * Args:   parent   - the parent of the window to be created        *
 * Return: none                                                     *
 * Global: data        - the data from the datafile                 *
 *         accountlist - the widget that has the list of accounts   *
\********************************************************************/
void
mainWindow( Widget parent )
  {
  Widget   mainwindow,menubar,actionform,controlform,pane,widget;
  int      position;
  
  /******************************************************************\
   * Set up the menubar                                             *
  \******************************************************************/
  MenuItem fileMenu[] = {
    { "New File...",   &xmPushButtonWidgetClass, 'N', NULL, NULL, 
      fileMenubarCB, (XtPointer)FMB_NEW,   (MenuItem *)NULL },
    { "Open File...      ",&xmPushButtonWidgetClass, 'O', NULL, NULL, 
      fileMenubarCB, (XtPointer)FMB_OPEN,  (MenuItem *)NULL },
    { "",              &xmSeparatorWidgetClass,    0, NULL, NULL, 
      NULL,         NULL,                  (MenuItem *)NULL },
    { "Save",          &xmPushButtonWidgetClass, 'S', NULL, NULL, 
      fileMenubarCB, (XtPointer)FMB_SAVE,  (MenuItem *)NULL },
    { "Save As...",    &xmPushButtonWidgetClass, 'A', NULL, NULL, 
      fileMenubarCB, (XtPointer)FMB_SAVEAS,(MenuItem *)NULL },
    { "",              &xmSeparatorWidgetClass,    0, NULL, NULL, 
      NULL,         NULL,                  (MenuItem *)NULL },
    { "Quit",          &xmPushButtonWidgetClass, 'Q', NULL, NULL, 
      fileMenubarCB, (XtPointer)FMB_QUIT,  (MenuItem *)NULL },
    NULL,
  };

  MenuItem accountMenu[] = {
    { "New Account...",     &xmPushButtonWidgetClass, 'N', NULL, NULL, 
      accountMenubarCB, (XtPointer)AMB_NEW,  (MenuItem *)NULL },
    { "Open Account",       &xmPushButtonWidgetClass, 'O', NULL, NULL, 
      accountMenubarCB, (XtPointer)AMB_OPEN, (MenuItem *)NULL },
    { "Edit Account...",    &xmPushButtonWidgetClass, 'E', NULL, NULL, 
      accountMenubarCB, (XtPointer)AMB_EDIT, (MenuItem *)NULL },
    { "Delete Account...",  &xmPushButtonWidgetClass, 'D', NULL, NULL,
      accountMenubarCB, (XtPointer)AMB_DEL,  (MenuItem *)NULL },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL,
      NULL,         NULL,                    (MenuItem *)NULL },
    { "Transfer",           &xmPushButtonWidgetClass, 'C', NULL, NULL, 
      accountMenubarCB, (XtPointer)AMB_TRNS,  (MenuItem *)NULL },
    { "Report",             &xmPushButtonWidgetClass, 'R', NULL, NULL, 
      accountMenubarCB, (XtPointer)AMB_RPRT,  (MenuItem *)NULL },
#if 0
    { "Edit Categories...", &xmPushButtonWidgetClass, 'C', NULL, NULL, 
      accountMenubarCB, (XtPointer)AMB_CAT,  (MenuItem *)NULL },
#endif
    NULL,
  };
  
  MenuItem helpMenu[] = {
    { "About...",           &xmPushButtonWidgetClass, 'A', NULL, NULL, 
      helpMenubarCB, (XtPointer)HMB_ABOUT, (MenuItem *)NULL },
    { "Help...",            &xmPushButtonWidgetClass, 'H', NULL, NULL, 
      helpMenubarCB, (XtPointer)HMB_MAIN,  (MenuItem *)NULL },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL,
      NULL,         NULL,                    (MenuItem *)NULL },
    { "License...",         &xmPushButtonWidgetClass, 'L', NULL, NULL, 
      helpMenubarCB, (XtPointer)HMB_LIC,   (MenuItem *)NULL },
    NULL,
  };
  
  mainwindow = XtVaCreateManagedWidget( "mainwindow", 
					xmMainWindowWidgetClass, parent, 
					XmNdeleteResponse,       XmDESTROY,
/*linas hack */
                                 XmNwidth,     650,
                                 XmNheight,    300,

					NULL );
  
  /* Umm... this doesn't seem to be getting called */
  XtAddCallback( mainwindow, XmNdestroyCallback, 
		 closeMainWindow, (XtPointer)NULL );
  
  menubar = XmCreateMenuBar( mainwindow, "menubar", NULL, 0 );  
  
  BuildMenu( menubar, XmMENU_PULLDOWN, "File",   'F', False, 0, fileMenu );
  BuildMenu( menubar, XmMENU_PULLDOWN, "Account",'A', False, 0, accountMenu );
  BuildMenu( menubar, XmMENU_PULLDOWN, "Help",   'H', False, 0, helpMenu );

  XtManageChild( menubar );
  
  /******************************************************************\
   * If they haven't already been initialize, initialize the Pixel  *
   * values that are used for foreground colors for the balance     *
  \******************************************************************/
#ifndef USE_NO_COLOR
  if( !havePixels )
    {
    XrmValue colorValue, pixelValue;
    
    colorValue.size = strlen(POSITIVE_BALANCE);
    colorValue.addr = (XtPointer)POSITIVE_BALANCE;
    pixelValue.size = sizeof(Pixel);
    pixelValue.addr = (XtPointer)0;
    
    XtConvertAndStore( parent,
                       XtRString, &colorValue,
                       XtRPixel,  &pixelValue );
    
    posPixel = (*(Pixel *)pixelValue.addr);
    
    colorValue.size = strlen(NEGATIVE_BALANCE);
    colorValue.addr = (XtPointer)NEGATIVE_BALANCE;
    pixelValue.size = sizeof(Pixel);
    pixelValue.addr = (XtPointer)0;
    
    XtConvertAndStore( parent,
                       XtRString, &colorValue,
                       XtRPixel,  &pixelValue );
    
    negPixel = (*(Pixel *)pixelValue.addr);
    
    havePixels = True;
    }
#endif
  
  /* Create a PanedWindow Manager for the dialog box... the paned 
   * window is the parent of the two forms which comprise the two
   * areas of the dialog box */
  pane = XtVaCreateWidget( "pane", 
                           xmPanedWindowWidgetClass, mainwindow,
                           XmNsashWidth,     1,
                           XmNsashHeight,    1,
                           XmNtraversalOn,   False,
                           NULL );
  
  
  /******************************************************************\
   * The account list -- the top part of the window                 *
  \******************************************************************/

  /* form to help place the accountlist */
  actionform = XtVaCreateWidget( "form", 
				 xmFormWidgetClass, pane,
				 NULL );
  
  /* Create a matrix widget to hold the account list... the
   * listCB helps make this matrix think it is a list.  We
   * use the matrix instead of a list to get the accounts
   * up in columns */
    {
    String   labels[3]          = {"Account Name","Type","Balance"};
    short    colWidths[]        = {16,10,8};
    unsigned char alignments[3] = {XmALIGNMENT_BEGINNING,
				   XmALIGNMENT_CENTER,
				   XmALIGNMENT_END};
    
    accountlist
      = XtVaCreateWidget( "list",
			  xbaeMatrixWidgetClass,  actionform,
			  XmNvisibleRows,         7,
			  XmNcolumns,             3,
			  XmNcolumnWidths,        colWidths,
			  XmNcolumnAlignments,    alignments,
			  XmNcolumnLabels,        labels,
			  XmNtraversalOn,         False,
			  XmNfill,                True,
			  XmNcellMarginHeight,    0,
			  XmNcellMarginWidth,     0,
			  XmNgridType,            XmGRID_NONE,
			  XmNcellShadowThickness, 0,
			  XmNverticalScrollBarDisplayPolicy,XmDISPLAY_STATIC,
			  XmNtopAttachment,       XmATTACH_FORM,
			  XmNleftAttachment,      XmATTACH_FORM,
			  XmNbottomAttachment,    XmATTACH_FORM,
			  XmNrightAttachment,     XmATTACH_FORM,
			  NULL);
    XtAddCallback( accountlist, XmNenterCellCallback,
                   listCB, (XtPointer)NULL );
    
    /* If the user double-clicks on an account in the list, open
     * up the detail view (ie the regWindow, or whatever) for
     * that type of account */
    XtAddCallback( accountlist, XmNdefaultActionCallback, 
                   accountMenubarCB, (XtPointer)AMB_OPEN );
    }
 
  refreshMainWindow();
  XtManageChild(accountlist);
  
  /******************************************************************\
   * The button area -- has buttons to create a new account, or     *
   * delete an account, or whatever other button I think up         *
   * NOTE: the buttons are just shortcuts to the account menubar,   *
   *       and this is why all the callbacks are accountMenubarCB   *
  \******************************************************************/

  /* create form that will contain most everything in this window...
   * The fractionbase divides the form into segments, so we have
   * better control over where to put the buttons */
  controlform = XtVaCreateWidget( "form", 
				  xmFormWidgetClass, pane,
				  XmNfractionBase,   5,
				  NULL );
  
  position = 0;                    /* puts the buttons in the right place */
  
  /* The "Open" button */
  widget = XtVaCreateManagedWidget( "Open", 
				    xmPushButtonWidgetClass, controlform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+1,
				    XmNshowAsDefault,      True,
				    NULL );

  XtAddCallback( widget, XmNactivateCallback, 
		 accountMenubarCB, (XtPointer)AMB_OPEN );

  /* The "New" button, to create a new account */
  position ++;
  widget = XtVaCreateManagedWidget( "New", 
				    xmPushButtonWidgetClass, controlform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+1,
				    XmNshowAsDefault,      True,
				    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 accountMenubarCB, (XtPointer)AMB_NEW );
  
  /* The "Edit" button */
  position ++;
  widget = XtVaCreateManagedWidget( "Edit", 
				    xmPushButtonWidgetClass, controlform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+1,
				    XmNshowAsDefault,      True,
				    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 accountMenubarCB, (XtPointer)AMB_EDIT );
  
  /* The "Delete" button */
  position ++;
  widget = XtVaCreateManagedWidget( "Delete", 
				    xmPushButtonWidgetClass, controlform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+1,
				    XmNshowAsDefault,      True,
				    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 accountMenubarCB, (XtPointer)AMB_DEL );
  
  
  /* Fix button area of the pane to its current size, and not let 
   * it resize. */
  XtManageChild( controlform );
    {
    Dimension h;
    XtVaGetValues( widget, XmNheight, &h, NULL );
    XtVaSetValues( controlform, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL );
    }
  
  /* Fix action area of the pane to its current size, and not let 
   * it resize. */
  XtManageChild( actionform );
    {
    Dimension h;
    XtVaGetValues( accountlist, XmNheight, &h, NULL );
    XtVaSetValues( actionform, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL );
    }
    
  /******************************************************************/
  XtManageChild(pane);
  }

/********************************************************************\
 * closeMainWindow                                                  *
 *   frees memory allocated for an regWindow, and other cleanup     * 
 *   stuff                                                          * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd -                                                     * 
 *         cb -                                                     * 
 * Return: none                                                     *
\********************************************************************/
void 
closeMainWindow( Widget mw, XtPointer cd, XtPointer cb )
  {
  
  DEBUG("closed MainWindow");
  DEBUGCMD(printf(" coresize = %d\n",_coresize()));
  exit(0);
  }

/********************************************************************\
 * listCB -- makes the matrix widget behave like a list widget      * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd -                                                     * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: accountlist - the widget that has the list of accounts   *
\********************************************************************/
void
listCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  XbaeMatrixEnterCellCallbackStruct *cbs =
    (XbaeMatrixEnterCellCallbackStruct *)cb;
  int rows = XbaeMatrixNumRows(accountlist);
  
  cbs->doit = False;
  cbs->map  = False;
  
  row = cbs->row;
  XbaeMatrixDeselectAll(accountlist);
  XbaeMatrixSelectRow( accountlist, row );
  }


/********************************************************************\
 * fileMenubarCB -- handles file menubar choices                    * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd -                                                     * 
 *         cb - const that lets us know which choice was selected   * 
 * Return: none                                                     * 
 * Global: data        - the data from the datafile                 *
 *         datafile    - the name of the user's datafile            *
 *         toplevel    - the toplevel widget                        *
\********************************************************************/
void
fileMenubarCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  int button = (int)cd;
  
  /*
   * which of the file menubar options was chosen
   *   FMB_NEW    -  New datafile
   *   FMB_OPEN   -  Open datfile
   *   FMB_SAVE   -  Save datafile
   *   FMB_SAVEAS -  Save datafile As
   *   FMB_QUIT   -  Quit
   */
  
  switch( button )
    {
    case FMB_NEW:
      DEBUG("FMB_NEW");
      if( (!(data->saved)) && (datafile != NULL) )
        {
        char *msg = SAVE_MSG;
        if( verifyBox(toplevel,msg) )
          fileMenubarCB( mw, (XtPointer)FMB_SAVE, cb );
        }
      datafile = NULL;
      freeData(data);
      data = mallocData();
      data->new = True;            /* so we have to do a "SaveAs" when
                                    * the file is first saved */
      break;
    case FMB_OPEN:
      DEBUG("FMB_OPEN");
      if( (!(data->saved)) && (datafile != NULL) )
        {
        char *msg = SAVE_MSG;
        if( verifyBox(toplevel,msg) )
          fileMenubarCB( mw, (XtPointer)FMB_SAVE, cb );
        }
      freeData(data);
      while( (datafile = fileBox(toplevel,OPEN)) == NULL )
        printf("Bad File\n");
      
      /* load the accounts from the users datafile */
      data = readData(datafile);
      
      if( data == NULL )
        {
        /* the file could not be found */
        data = mallocData();
        }
      
      break;
    case FMB_SAVE:
      DEBUG("FMB_SAVE");
      /* ??? Somehow make sure all in-progress edits get committed! */
      writeData( datafile, data );
      data->saved = True;
      break;
    case FMB_SAVEAS:
      DEBUG("FMB_SAVEAS");
      while( (datafile = fileBox(toplevel,OPEN)) == NULL )
        printf("Bad File\n");
      fileMenubarCB( mw, (XtPointer)FMB_SAVE, cb );
      break;
    case FMB_QUIT:
      DEBUG("FMB_QUIT");
      {
      Account *acc;
      int i=0;
      while( (acc=getAccount(data,i++)) != NULL )
        {
        if( acc->regData != NULL )
          {
          /* ??? */
          acc->regData = NULL;
          }
        if( acc->recnData != NULL )
          {
          /* ??? */
          acc->recnData = NULL;
          }
        }
      
      if( (!(data->saved)) && (datafile != NULL) )
        {
        char *msg = SAVE_MSG;
        if( verifyBox(toplevel,msg) )
          fileMenubarCB( mw, (XtPointer)FMB_SAVE, cb );
        }
      
      freeData(data);
      XtUnmapWidget(toplevel);     /* make it disappear quickly */
      XtDestroyWidget(toplevel);
      return;                      /* to avoid the refreshMainWindow */
      }
      break;
    default:
      DEBUG("We shouldn't be here!");
    }
  refreshMainWindow();
  }

/********************************************************************\
 * accountMenubarCB -- handles account menubar choices              * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - const that lets us know which choice was selected   * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: data        - the data from the datafile                 *
 *         row         - the selected row number                    *
 *         toplevel    - the toplevel widget                        *
\********************************************************************/
void
accountMenubarCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  int button = (int)cd;
  int *posList;
  int numPos;
  
  /*
   * which of the file menubar options was chosen
   *   AMB_NEW    -  New account
   *   AMB_OPEN   -  Open account
   *   AMB_EDIT   -  Edit account
   *   AMB_DEL    -  Delete account
   *   AMB_CAT    -  Edit catagories
   */
  
  switch( button )
    {
    case AMB_NEW:
      DEBUG("AMB_NEW");
      accWindow(toplevel);
      break;
    case AMB_OPEN:
      DEBUG("AMB_OPEN");
        {
        Account *acc = getAccount(data,row);
        if( acc->regData == NULL )
          acc->regData = regWindow( toplevel, acc );
        }
      break;
    case AMB_EDIT:
      DEBUG("AMB_EDIT");
      editAccWindow( toplevel, getAccount(data,row) );
      break;
    case AMB_DEL:
      DEBUG("AMB_DEL");
        {
        char *msg = "Are you sure you want to delete this account?";
        if( verifyBox(toplevel,msg) )
          {
          freeAccount( removeAccount(data,row) );
          refreshMainWindow();
          }
        }
      break;
    case AMB_TRNS:
      DEBUG("AMB_TRNS");
      xferWindow(toplevel);
      break;
    case AMB_RPRT:
      DEBUG("AMB_RPRT");
      simpleReportWindow(toplevel);
      break;
    case AMB_CAT:
      DEBUG("AMB_CAT");
      break;
    default:
      DEBUG("We shouldn't be here!");
    }
  }

/********************************************************************\
 * helpMenubarCB -- handles help menubar choices                    * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - const that lets us know which choice was selected   * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: toplevel    - the toplevel widget                        *
\********************************************************************/
void
helpMenubarCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  int button = (int)cd;
  int *posList;
  int numPos;
  
  /*
   * which of the file menubar options was chosen
   *   HMB_ABOUT   -  About this program
   *   HMB_MAIN    -  Display help window for the main window
   *   HMB_REGWIN  -  Display help window for the Register Window
   *   HMB_RECNWIN -  Display help window for the Reconcile Window
   *   HMB_LIC     -  GPL Licence info
   */
  
  switch( button )
    {
    case HMB_ABOUT:
      DEBUG("HMB_ABOUT");
      helpWindow( toplevel, "About", HH_ABOUT );
      break;
    case HMB_REGWIN:
      /* When the user selects "Help" in the RegWindow */
      DEBUG("HMB_REGWIN");
      helpWindow( toplevel, "Help", HH_REGWIN );
      break;
    case HMB_RECNWIN:
      /* When the user selects "Help" in the RecnWindow */
      DEBUG("HMB_RECNWIN");
      helpWindow( toplevel, "Help", HH_RECNWIN );
      break;
    case HMB_ADJBWIN:
      /* When the user selects "Help" in the AdjBWindow */
      DEBUG("HMB_ADJBWIN");
      helpWindow( toplevel, "Help", HH_ADJBWIN );
      break;
    case HMB_MAIN:
      /* When the user selects "Help" in the MainWindow */
      DEBUG("HMB_HELP");
      helpWindow( toplevel, "Help", HH_MAIN );
      break;
    case HMB_LIC:
      /* The GNU Public License */
      DEBUG("HMB_LIC");
      helpWindow( toplevel, "License", HH_GPL );
      break;
    default:
      DEBUG("We shouldn't be here!");
    }
  }
