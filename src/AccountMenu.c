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
\********************************************************************/

AccountMenu *
xaccBuildAccountMenu (AccountGroup *grp, Widget parent) 
{
  MenuItem   *accountMenu;
  AccountMenu *accData;
  int        i;
  
  if (NULL == grp) return NULL;
  

  /******************************************************************\
   * The popup menus that let the user choose the account to        *
   * transfer to and the account to transfer from                   *
  \******************************************************************/
  accData = (AccountMenu *) _malloc (sizeof (AccountMenu));
  accountMenu = (MenuItem *) _malloc((grp->numAcc+1)*sizeof(MenuItem));
  
  accData->menuEntry = (AccMenuEntry **)_malloc(grp->numAcc*sizeof(AccMenuEntry *));
  accData->numMenuEntries = grp->numAcc;
  
  for( i=0; i<accData->numMenuEntries; i++ )
    {
    Account *acc = getAccount( grp, i );
    
    accData->menuEntry[i] = (AccMenuEntry *) _malloc (sizeof (AccMenuEntry));
    accData->menuEntry[i]->option = i;
    accData->menuEntry[i]->chosen = &(accData->choice);
    
    accountMenu[i].label         = acc->accountName;
    accountMenu[i].wclass        = &xmPushButtonWidgetClass;
    accountMenu[i].mnemonic      = 0;
    accountMenu[i].accelerator   = NULL;
    accountMenu[i].accel_text    = NULL;
    accountMenu[i].callback      = xaccAccountMenuCB;
    accountMenu[i].callback_data = accData->menuEntry[i];
    accountMenu[i].subitems      = (MenuItem *)NULL;
    }
  accountMenu[i] .label= NULL;
  
  accData->menu_widget = BuildMenu( parent, XmMENU_OPTION, "From:", 'F', 
                           False, 0, accountMenu );
  
  _free(accountMenu);
  
}

/* ********************** END OF FILE *************************/
