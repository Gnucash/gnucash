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

#include "Account.h"
#include "BuildMenu.h"
#include "Data.h"
#include "util.h"

typedef struct _accMenuEntry
{
  int option;
  int *chosen;
} AccMenuEntry;

typedef struct _accountMenu
{
  Widget menu_widget;
  int choice;
  int numMenuEntries;
  AccMenuEntry **menuEntry;
} AccountMenu;

/********************************************************************\
\********************************************************************/

void
xaccFreeAccountMenu (AccountMenu * menu) {
   int i;

   for( i=0; i<menu->numMenuEntries; i++ ) {
      _free (menu->menuEntry[i]);
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

int
xaccGetAccountMenuSelection (AccountMenu *menu) {
   return (menu->choice);
}

/********************************************************************\
 * menuCB -- keeps track of the to and from menues                  * 
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
  AccMenuEntry *menuEntry = (AccMenuEntry *)cd;
  
  *(menuEntry->chosen) = menuEntry->option;
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
  
  if (NULL == grp) return NULL;
  
  nacc = grp->numAcc;

  menuList = (MenuItem *) _malloc((nacc+pad+1)*sizeof(MenuItem));
  
  for( i=0; i<pad; i++ )
    {
    accData->menuEntry[*offset] = (AccMenuEntry *) _malloc (sizeof (AccMenuEntry));
    accData->menuEntry[*offset]->option = -1;
    accData->menuEntry[*offset]->chosen = &(accData->choice);
    
    menuList[i].label         = "(none)";
    menuList[i].wclass        = &xmPushButtonWidgetClass;
    menuList[i].mnemonic      = 0;
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
    accData->menuEntry[*offset]->option = xaccGetAccountID (acc);
    accData->menuEntry[*offset]->chosen = &(accData->choice);
    
    menuList[i+pad].label         = acc->accountName;
    menuList[i+pad].wclass        = &xmPushButtonWidgetClass;
    menuList[i+pad].mnemonic      = 0;
    menuList[i+pad].accelerator   = NULL;
    menuList[i+pad].accel_text    = NULL;
    menuList[i+pad].callback      = xaccAccountMenuCB;
    menuList[i+pad].callback_data = accData->menuEntry[*offset];
    menuList[i+pad].subitems      = (MenuItem *) NULL;

    (*offset) ++;
    if (acc->children) {
       pad ++;
       accData->menuEntry[*offset] = (AccMenuEntry *) _malloc (sizeof (AccMenuEntry));
       accData->menuEntry[*offset]->option = xaccGetAccountID (acc);
       accData->menuEntry[*offset]->chosen = &(accData->choice);
       
       menuList[i+pad].label         = acc->accountName;
       menuList[i+pad].wclass        = &xmPushButtonWidgetClass;
       menuList[i+pad].mnemonic      = 0;
       menuList[i+pad].accelerator   = NULL;
       menuList[i+pad].accel_text    = NULL;
       menuList[i+pad].callback      = xaccAccountMenuCB;
       menuList[i+pad].callback_data = accData->menuEntry[*offset];
       menuList[i+pad].subitems      = (MenuItem *) NULL;

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
    xaccFreeAccountSubMenu (menuList[i].subitems);
    i++;
  }
  
  _free (menuList);
}

/********************************************************************\
\********************************************************************/

AccountMenu *
xaccBuildAccountMenu (AccountGroup *grp, Widget parent, char * label) 
{
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
  accData ->choice = -1;
  
  accData->menuEntry = (AccMenuEntry **)_malloc((nacc+pad)*sizeof(AccMenuEntry *));
  accData->numMenuEntries = nacc+pad;
  
  offset = 0;
  menuList = xaccBuildAccountSubMenu (grp, accData, &offset, pad);
  
  accData->menu_widget = BuildMenu( parent, XmMENU_OPTION, label, 'F', 
                           False, 0, menuList );
  
  xaccFreeAccountSubMenu (menuList);
  
  return (accData);
}

/* ********************** END OF FILE *************************/
