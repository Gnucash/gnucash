/********************************************************************\
 * AccountMenu.c -- the account menu for xacc (X-Accountant)        *
 *                  (general utility)                               *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997 Linas Vepstas                                 *
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
#include <Xm/RowColumn.h>

#include "config.h"

#include "Account.h"
#include "BuildMenu.h"
#include "Data.h"
#include "util.h"

typedef struct _accMenuEntry
{
  Account *option;
  char * label;
  struct _accountMenu *am;
} AccMenuEntry;

typedef struct _accountMenu
{
  Widget menu_widget;
  Widget pulldown_widget;
  Account *choice;
  int numMenuEntries;
  AccMenuEntry **menuEntry;

  /* user callbacks */
  XtCallbackProc callback;
  XtPointer client_data;
} AccountMenu;

/********************************************************************\
\********************************************************************/

void
xaccFreeAccountMenu (AccountMenu * menu) {
   int i;

   for( i=0; i<menu->numMenuEntries; i++ ) {
      if (menu->menuEntry[i]) _free (menu->menuEntry[i]);
      menu->menuEntry[i] = NULL;
   }

   _free (menu->menuEntry);
   menu->menuEntry = NULL;
   menu->numMenuEntries = 0;

   _free (menu);
}

/********************************************************************\
\********************************************************************/

Widget 
xaccGetAccountMenuWidget (AccountMenu *menu) {
   return (menu->menu_widget);
}

Account *
xaccGetAccountMenuSelection (AccountMenu *menu) {
   return (menu->choice);
}

/********************************************************************\
 * menuCB -- keeps track of the menu choice                         * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - menuEntry - has the menu option and a pointer to    *
 *              where the selection will be stored                  * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
static
void
xaccAccountMenuCB( Widget mw, XtPointer cd, XtPointer cb )
{
  XmString labelStr;
  AccMenuEntry *menuEntry = (AccMenuEntry *)cd;
  AccountMenu *menu;

  menu = (AccountMenu *) (menuEntry->am);
  
  menu->choice = menuEntry->option;

  labelStr = XmStringCreateSimple (menuEntry->label);
  XtVaSetValues (menu->pulldown_widget,
                 XmNlabelString, labelStr,
                 NULL );
  XmStringFree (labelStr);

  if (menu->callback) {
    (*(menu->callback)) (menu->menu_widget, menu->client_data, (XtPointer) menu->choice);
  }

}

/********************************************************************\
 * build menus recuresively                                         *
\********************************************************************/

MenuItem *
xaccBuildAccountSubMenu (AccountGroup *grp, 
                         AccountMenu *accData, 
                         int *offset,
                         int pad)
{
  MenuItem   *menuList;
  int        i;
  int        nacc;
  char * tmp;
  
  if (NULL == grp) return NULL;
  
  nacc = grp->numAcc;

  menuList = (MenuItem *) _malloc((2*nacc+pad+1)*sizeof(MenuItem));
  
  for( i=0; i<pad; i++ )
    {
    accData->menuEntry[*offset] = (AccMenuEntry *) _malloc (sizeof (AccMenuEntry));
    accData->menuEntry[*offset]->option = NULL;
    accData->menuEntry[*offset]->label = "(none)";
    accData->menuEntry[*offset]->am = (struct _accountMenu *) accData;
    
    tmp = (char *) _malloc (strlen ("(none)")+1);
    strcpy (tmp, "(none)");
    menuList[i].label         = tmp;
    menuList[i].wclass        = &xmPushButtonWidgetClass;
    menuList[i].mnemonic      = 0;
    menuList[i].sensitive     = True;
    menuList[i].accelerator   = NULL;
    menuList[i].accel_text    = NULL;
    menuList[i].callback      = xaccAccountMenuCB;
    menuList[i].callback_data = accData->menuEntry[*offset];

    (*offset) ++;
    menuList[i].subitems      = (MenuItem *)NULL;
    }
  for( i=0; i<nacc; i++ )
    {
    Account *acc = getAccount( grp, i );
    
    accData->menuEntry[*offset] = (AccMenuEntry *) _malloc (sizeof (AccMenuEntry));
    accData->menuEntry[*offset]->option = acc;
    accData->menuEntry[*offset]->label = acc->accountName;
    accData->menuEntry[*offset]->am = (struct _accountMenu *) accData;
    
    tmp = (char *) _malloc (strlen (acc->accountName) + 1);
    strcpy (tmp, acc->accountName);
    menuList[i+pad].label         = tmp;
    menuList[i+pad].wclass        = &xmPushButtonWidgetClass;
    menuList[i+pad].mnemonic      = 0;
    menuList[i+pad].sensitive     = True;
    menuList[i+pad].accelerator   = NULL;
    menuList[i+pad].accel_text    = NULL;
    menuList[i+pad].callback      = xaccAccountMenuCB;
    menuList[i+pad].callback_data = accData->menuEntry[*offset];
    menuList[i+pad].subitems      = (MenuItem *) NULL;

    (*offset) ++;
    if (acc->children) {
       pad ++;
       accData->menuEntry[*offset] = (AccMenuEntry *) _malloc (sizeof (AccMenuEntry));
       accData->menuEntry[*offset]->option = acc;
       accData->menuEntry[*offset]->label = acc->accountName;
       accData->menuEntry[*offset]->am = (struct _accountMenu *) accData;
       
       /* submenu lists in the menu will be distintively bracketed for easy visual id */
       tmp = (char *) _malloc (strlen (acc->accountName) + 3);
       strcpy (tmp, "[");
       strcat (tmp, acc->accountName);
       strcat (tmp, "]");
       menuList[i+pad].label         = tmp;
       menuList[i+pad].wclass        = &xmPushButtonWidgetClass;
       menuList[i+pad].mnemonic      = 0;
       menuList[i+pad].sensitive     = True;
       menuList[i+pad].accelerator   = NULL;
       menuList[i+pad].accel_text    = NULL;
       menuList[i+pad].callback      = xaccAccountMenuCB;
       menuList[i+pad].callback_data = accData->menuEntry[*offset];

       (*offset) ++;
       menuList[i+pad].subitems      = xaccBuildAccountSubMenu (acc->children, accData, offset, 0);
       }
    }
  menuList[i+pad].label= NULL;
  
  return (menuList);
}

/********************************************************************\
 * free menus recuresively                                          *
\********************************************************************/

void
xaccFreeAccountSubMenu (MenuItem *menuList)
{
  int        i;
  
  if (NULL == menuList) return;

  i = 0;
  while (NULL != menuList[i].label) {
    _free (menuList[i].label);
    menuList[i].label = NULL;
    xaccFreeAccountSubMenu (menuList[i].subitems);
    i++;
  }
  
  _free (menuList);
}

/********************************************************************\
\********************************************************************/
void
xaccAccountMenuAddCallback (AccountMenu *menu, 
                            XtCallbackProc cb,
                            XtPointer cd )
{
  menu -> callback = cb;
  menu -> client_data = cd;
}

/********************************************************************\
\********************************************************************/

AccountMenu *
xaccBuildAccountMenu (AccountGroup *grp, Widget parent, char * label) 
{
  Widget     bar_widget;
  MenuItem   *menuList;
  AccountMenu *accData;
  int        i;
  int        offset = 0;
  int        nacc;
  int        pad = 1;
  
  if (NULL == grp) return NULL;
  
  nacc = xaccGetNumAccounts (grp);
  nacc *= 2;  /* quick hack to make room for doubled account labels. */

  accData = (AccountMenu *) _malloc (sizeof (AccountMenu));
  accData -> choice = NULL;
  accData -> callback = NULL;
  accData -> client_data = NULL;
  
  accData->menuEntry = (AccMenuEntry **)_malloc((nacc+pad)*sizeof(AccMenuEntry *));
  accData->numMenuEntries = nacc+pad;
  
  for (i=0; i<accData->numMenuEntries; i++) {
    accData->menuEntry[i] = NULL;
  }

  /* top-level widget will be a menu widget; this is the only thing
   * that will accept cascading menus as a parent */
  bar_widget = XmCreateMenuBar( parent, "accmenubar", NULL, 0);
  accData->menu_widget = bar_widget;

  XtVaSetValues (bar_widget,
                 XmNorientation, XmVERTICAL,
                 /* XmNshadowThickness, 0, */
                 NULL );

  XtManageChild (bar_widget);

  offset = 0;
  menuList = xaccBuildAccountSubMenu (grp, accData, &offset, pad);
  
  accData->pulldown_widget = 
        BuildMenu( bar_widget, XmMENU_PULLDOWN, label, 'F', False, 0, menuList );

  XtVaSetValues (accData->pulldown_widget,
                 XmNshadowThickness, 0,
                 NULL );

  
  xaccFreeAccountSubMenu (menuList);
  
  return (accData);
}

/* ********************** END OF FILE *************************/
