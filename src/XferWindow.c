/********************************************************************\
 * XferWindow.c -- the transfer window for xacc (X-Accountant)      *
 *                     (for transferring between accounts)          *
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
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/LabelGP.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <string.h>

#include "BuildMenu.h"
#include "Account.h"
#include "Data.h"
#include "main.h"
#include "util.h"


typedef struct _menuData
{
  int choice;
  int *accNum;
} MenuData;

typedef struct _xferwindow
{
  Widget date;
  Widget desc;
  Widget amount;
  Widget memo;
  int to;                          /* to and from hold the index of */
  int from;                        /* the chosen to/from accounts   */
  int numMenuData;
  MenuData **menuData;
} XferWindow;

/** GLOBALS *********************************************************/
extern Data   *data;

/** PROTOTYPES ******************************************************/
void closeXferWindow( Widget mw, XtPointer cd, XtPointer cb );
void menuCB( Widget mw, XtPointer cd, XtPointer cb );
void xferCB( Widget mw, XtPointer cd, XtPointer cb );

/********************************************************************\
 * xfewWindow                                                       *
 *   opens up a window to do an automatic transfer between accounts * 
 *                                                                  * 
 * Args:   parent   - the parent of the window to be created        * 
 * Return: none                                                     *
 * Global: data     - the accounts, and stuff...                    *
\********************************************************************/
void 
xferWindow( Widget parent )
  {
  Date       date;
  char       buf[BUFSIZE];
  Widget     dialog, form, widget, label, buttonform, menu;
  MenuItem   *accountMenu;
  XferWindow *xferData;
  int        position,i;
  int        initial = 0;
  
  setBusyCursor( parent );
  
  xferData = (XferWindow *)_malloc(sizeof(XferWindow));
  xferData->to   = initial;
  xferData->from = initial;
  xferData->menuData    = NULL;
  xferData->numMenuData = 0;
  
  /* force the size of the dialog so it is not resizable */
  dialog = XtVaCreatePopupShell( "dialog", 
                                 xmDialogShellWidgetClass, parent,
                                 XmNtitle,            "Transfer Money",
                                 XmNdeleteResponse,   XmDESTROY,
                                 XmNwidth,     450,
                                 XmNminWidth,  450,
                                 XmNmaxWidth,  450,
                                 XmNheight,    230,
                                 XmNminHeight, 230,
                                 XmNmaxHeight, 230,
                                 NULL );
  
  XtAddCallback( dialog, XmNdestroyCallback, 
                 closeXferWindow, (XtPointer)xferData );
  
  /* The form to put everything in the dialog in */
  form = XtVaCreateWidget( "form", xmFormWidgetClass, dialog, NULL );
  /******************************************************************\
	 * Text fields....                                                *
	\******************************************************************/  
  label = 
  XtVaCreateManagedWidget( "Date",
                           xmLabelGadgetClass, form,
                           XmNtopAttachment,   XmATTACH_FORM,
                           XmNtopOffset,       10,
                           XmNleftAttachment,  XmATTACH_POSITION,
                           XmNleftPosition,    65,
                           NULL );
  
  todaysDate(&date);
  sprintf(buf,"%2d/%2d/%4d\0", date.month, date.day, date.year);
  
  xferData->date = 
  XtVaCreateManagedWidget( "text",
                           xmTextWidgetClass,  form,
                           XmNvalue,           buf,
                           XmNtopAttachment,   XmATTACH_FORM,
                           XmNtopOffset,       10,
                           XmNrightAttachment, XmATTACH_FORM,
                           XmNrightOffset,     10,
                           XmNleftAttachment,  XmATTACH_WIDGET,
                           XmNleftWidget,      label,
                           NULL );
  /* The dateCB ensures that the data that the user enters in
   * in a valid date format. */
  XtAddCallback( xferData->date, XmNmodifyVerifyCallback,
                 dateCB, (XtPointer)NULL );
  
  label = 
  XtVaCreateManagedWidget( "Description",
                           xmLabelGadgetClass, form,
                           XmNtopAttachment,   XmATTACH_WIDGET,
                           XmNtopWidget,       xferData->date,
                           XmNtopOffset,       10,
                           XmNleftAttachment,  XmATTACH_FORM,
                           XmNleftOffset,      10,
                           NULL );
  xferData->desc = 
  XtVaCreateManagedWidget( "text",
                           xmTextWidgetClass,  form,
                           XmNvalue,           "Transfer",
                           XmNtopAttachment,   XmATTACH_WIDGET,
                           XmNtopWidget,       xferData->date,
                           XmNtopOffset,       10,
                           XmNleftAttachment,  XmATTACH_WIDGET,
                           XmNleftWidget,      label,
                           XmNrightAttachment, XmATTACH_POSITION,
                           XmNrightPosition,   65,
                           NULL );

  label = 
  XtVaCreateManagedWidget( "$",
                           xmLabelGadgetClass, form,
                           XmNtopAttachment,   XmATTACH_WIDGET,
                           XmNtopWidget,       xferData->date,
                           XmNtopOffset,       10,
                           XmNleftAttachment,  XmATTACH_POSITION,
                           XmNleftPosition,    65,
                           NULL );
  xferData->amount = 
  XtVaCreateManagedWidget( "text",
                           xmTextWidgetClass,  form,
                           XmNtopAttachment,   XmATTACH_WIDGET,
                           XmNtopWidget,       xferData->date,
                           XmNtopOffset,       10,
                           XmNleftAttachment,  XmATTACH_WIDGET,
                           XmNleftWidget,      label,
                           XmNrightAttachment, XmATTACH_FORM,
                           XmNrightOffset,     10,
                           NULL );
  /* The amountCB ensures the data entered in the amount field is
   * in a valid format */
  XtAddCallback( xferData->amount, XmNmodifyVerifyCallback,
                 amountCB, (XtPointer)NULL );
  
  label = 
  XtVaCreateManagedWidget( "Memo",
                           xmLabelGadgetClass, form,
                           XmNtopAttachment,   XmATTACH_WIDGET,
                           XmNtopWidget,       xferData->desc,
                           XmNtopOffset,       10,
                           XmNleftAttachment,  XmATTACH_FORM,
                           XmNleftOffset,      10,
                           NULL );
  xferData->memo = 
  XtVaCreateManagedWidget( "text",
                           xmTextWidgetClass,  form,
                           XmNmaxLength,       40,
                           XmNcolumns,         40,
                           XmNtopAttachment,   XmATTACH_WIDGET,
                           XmNtopWidget,       xferData->desc,
                           XmNtopOffset,       10,
                           XmNleftAttachment,  XmATTACH_WIDGET,
                           XmNleftWidget,      label,
                           XmNrightAttachment, XmATTACH_POSITION,
                           XmNrightPosition,   65,
                           NULL );
  
  /******************************************************************\
   * The popup menus that let the user choose the account to        *
   * transfer to and the account to transfer from                   *
  \******************************************************************/
  accountMenu = (MenuItem *)_malloc((data->numAcc+1)*sizeof(MenuItem));
  
  /* We have to keep track of the menuData stuff so we can free this
   * memory when the transfer window is closed... the even slots in
   * this array are used by the "Frow" menu, and the odd slots are
   * used by the "To" menu.  (The even/odd way was a simple way to
   * still use i as a index to the array... it makes sense to me,
   * at least)  */
  xferData->menuData = (MenuData **)_malloc(2*data->numAcc*sizeof(MenuData *));
  xferData->numMenuData = 2 * data->numAcc;
  
  for( i=0; i<xferData->numMenuData; i++ )
    xferData->menuData[i] = NULL;
  
  for( i=0; i<data->numAcc; i++ )
    {
    Account *acc = getAccount( data, i );
    
    /* This account menu uses the even menuData slots (ie (2*i) ) */
    xferData->menuData[2*i] = (MenuData *)_malloc(sizeof(MenuData));
    xferData->menuData[2*i]->choice = i;
    xferData->menuData[2*i]->accNum = &(xferData->from);
    
    accountMenu[i].label         = acc->accountName;
    accountMenu[i].wclass        = &xmPushButtonWidgetClass;
    accountMenu[i].mnemonic      = 0;
    accountMenu[i].accelerator   = NULL;
    accountMenu[i].accel_text    = NULL;
    accountMenu[i].callback      = menuCB;
    accountMenu[i].callback_data = xferData->menuData[2*i];
    accountMenu[i].subitems      = (MenuItem *)NULL;
    }
  accountMenu[i] .label= NULL;
  
  widget = BuildMenu( form, XmMENU_OPTION, "From:", 'F', 
                      False, initial, accountMenu );
  
  XtVaSetValues( widget,
                 XmNtopAttachment,  XmATTACH_WIDGET,
                 XmNtopWidget,      xferData->memo,
                 XmNtopOffset,      10,
                 XmNleftAttachment, XmATTACH_FORM,
                 XmNleftOffset,     10,
                 NULL );
  
  XtManageChild(widget);
  
  for( i=0; i<data->numAcc; i++ )
    {
    Account *acc = getAccount( data, i );
    
    /* This account menu uses the odd menuData slots (ie (2*i)+1 ) */
    xferData->menuData[2*i+1] = (MenuData *)_malloc(sizeof(MenuData));
    xferData->menuData[2*i+1]->choice = i;
    xferData->menuData[2*i+1]->accNum = &(xferData->to);
    
    accountMenu[i].label         = acc->accountName;
    accountMenu[i].wclass        = &xmPushButtonWidgetClass;
    accountMenu[i].mnemonic      = 0;
    accountMenu[i].accelerator   = NULL;
    accountMenu[i].accel_text    = NULL;
    accountMenu[i].callback      = menuCB;
    accountMenu[i].callback_data = xferData->menuData[2*i+1];
    accountMenu[i].subitems      = (MenuItem *)NULL;
    }
  accountMenu[i] .label= NULL;
  
  widget = BuildMenu( form, XmMENU_OPTION, "To:", 'T', 
                      False, initial, accountMenu );
  
  XtVaSetValues( widget,
                 XmNtopAttachment,  XmATTACH_WIDGET,
                 XmNtopWidget,      xferData->memo,
                 XmNtopOffset,      10,
                 XmNleftAttachment, XmATTACH_POSITION,
                 XmNleftPosition,   50,
                 NULL );
  
  XtManageChild(widget);
  
  _free(accountMenu);
  
  /******************************************************************\
   * The buttons at the bottom...                                   *
  \******************************************************************/
  
  buttonform = XtVaCreateWidget( "form", 
                                 xmFormWidgetClass,   form,
                                 XmNfractionBase,     5,
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        widget,
                                 XmNtopOffset,        10,
                                 XmNbottomAttachment, XmATTACH_FORM,
                                 XmNbottomOffset,     10,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 NULL );
  
  position = 1;                    /* puts the buttons in the right place */
  
  /* The "Cancel" button */
  widget = XtVaCreateManagedWidget( "Cancel", 
                                    xmPushButtonWidgetClass, buttonform,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNbottomAttachment,   XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_POSITION,
                                    XmNleftPosition,       position,
                                    XmNrightAttachment,    XmATTACH_POSITION,
                                    XmNrightPosition,      ++position,
                                    XmNshowAsDefault,      True,
                                    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
                 destroyShellCB, (XtPointer)dialog );  
  
  /* The "Transfer" button creates the transfer */
  widget = XtVaCreateManagedWidget( "Transfer", 
                                    xmPushButtonWidgetClass, buttonform,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNbottomAttachment,   XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_POSITION,
                                    XmNleftPosition,       position,
                                    XmNrightAttachment,    XmATTACH_POSITION,
                                    XmNrightPosition,      ++position,
                                    XmNshowAsDefault,      True,
                                    NULL );
  

  XtAddCallback( widget, XmNactivateCallback, 
                 xferCB, (XtPointer)xferData );
  XtAddCallback( widget, XmNactivateCallback, 
                 destroyShellCB, (XtPointer)dialog );  
    
  XtManageChild(buttonform);
  
  /******************************************************************/
  XtManageChild(form);
  
  XtPopup( dialog, XtGrabNone );
  
  unsetBusyCursor( parent );
  }

/********************************************************************\
 * closeXferWindow                                                  *
 *   frees memory allocated for an XferWindow, and other cleanup    * 
 *   stuff                                                          * 
 *                                                                  * 
 * Args:   mw -                                                     * 
 *         cd - xferData - the struct for the XferWindow that is    * 
 *              being closed                                        * 
 *         cb -                                                     * 
 * Return: none                                                     *
\********************************************************************/
void 
closeXferWindow( Widget mw, XtPointer cd, XtPointer cb )
  {
  int i;
  XferWindow *xferData = (XferWindow *)cd;
  
  for( i=0; i<xferData->numMenuData; i++ )
    _free(xferData->menuData[i]);
  
  _free(xferData->menuData);
  
  _free(xferData);
  
  DEBUG("close XferWindow");
  }

/********************************************************************\
 * menuCB -- keeps track of the to and from menues                  * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - menuData - has the menu choice and a pointer to     *
 *              either the to of from fields in XferWindow          * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
void
menuCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  MenuData *menuData = (MenuData *)cd;
  
  *(menuData->accNum) = menuData->choice;
  }

/********************************************************************\
 * xferCB -- creates the transfer between accounts                  * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - xferData - the struct of data associated with       *
 *              the XfewWindow                                      * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: data        - the data from the datafile                 *
\********************************************************************/
void
xferCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  XferWindow  *xferData = (XferWindow *)cd;
  Transaction *toTrans, *fromTrans;
  Account *acc;
  String  str;
  int     dollar=0,cent=0,pos=0;

  data->saved = False;
  
  /* "toTrans" is the transfer transaction that goes into the to account,
   * and "fromTrans" goes into the from account */
  toTrans   = (Transaction *)_malloc(sizeof(Transaction));
  fromTrans = (Transaction *)_malloc(sizeof(Transaction));
  
  /* Create the "toTrans" transaction */
  str = XmTextGetString(xferData->date);
  todaysDate(&(toTrans->date));
  sscanf( str, "%d/%d/%d", &(toTrans->date.month), 
          &(toTrans->date.day), &(toTrans->date.year) );
  str = XmTextGetString(xferData->amount);
  sscanf( str, "%d.%2d", &dollar, &cent );
  toTrans->amount      = 100*dollar + cent;
  toTrans->num         = XtNewString("");
  
  /* Get the memo, and add the "from" account name to it */
  {
  String  memo = NULL;
  /* Note, don't use _malloc/_free here, because otherwise it
   * would mess up the memory accounting if memory debugging
   * is turned on */
  acc = getAccount(data,xferData->from);
  str = XmTextGetString(xferData->memo);
  memo = (String)malloc(strlen(str)+
                        strlen(acc->accountName)+
                        strlen("[From: ] ") );
  sprintf( memo, "[From: %s] %s\0", acc->accountName, str );
  toTrans->memo = memo;
  free(str);
  }
  
  toTrans->description = XmTextGetString(xferData->desc);
  toTrans->catagory    = 0;
  toTrans->reconciled  = NREC;
  
  acc = getAccount(data,xferData->to);
  pos = insertTransaction( acc, toTrans );
  
  /* Refresh the "to" account register window */
  regRefresh(acc->regData);
  /* Refresh the "to" account reconcile window */
  recnRefresh(acc->recnData);
  
  /* Create the "fromTrans" transaction */
  str = XmTextGetString(xferData->date);
  todaysDate(&(fromTrans->date));
  sscanf( str, "%d/%d/%d", &(fromTrans->date.month), 
          &(fromTrans->date.day), &(fromTrans->date.year) );
  fromTrans->amount      = -1*toTrans->amount;
  fromTrans->num         = XtNewString("");
  
  /* Get the memo, and add the "to" account name to it */
  {
  String  memo = NULL;
  /* Note, don't use _malloc/_free here, because otherwise it
   * would mess up the memory accounting if memory debugging
   * is turned on */
  acc = getAccount(data,xferData->to);
  str = XmTextGetString(xferData->memo);
  memo = (String)malloc(strlen(str)+
                        strlen(acc->accountName)+
                        strlen("[To: ] ") );
  sprintf( memo, "[To: %s] %s\0", acc->accountName, str );
  fromTrans->memo = memo;
  free(str);
  }
  
  fromTrans->description = XmTextGetString(xferData->desc);
  fromTrans->catagory    = 0;
  fromTrans->reconciled  = NREC;
  
  acc = getAccount(data,xferData->from);
  pos = insertTransaction( acc, fromTrans );
  
  /* Refresh the "from" account register window */
  regRefresh(acc->regData);
  /* Refresh the "from" account reconcile window */
  recnRefresh(acc->recnData);
  
  refreshMainWindow();
  }
