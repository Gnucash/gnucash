/********************************************************************\
 * Action.c -- account actions for xacc (X-Accountant)              *
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
\********************************************************************/

#include <Xm/Xm.h>

#include "config.h"

#include "Action.h"
#include "main.h"
#include "PopBox.h"
#include "util.h"

/********************************************************************\
 * actionBox                                                        *
 *   creates the action widget                                      *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 * Return: PopBox  - the action GUI structure                       *
\********************************************************************/

PopBox *
actionBox (Widget parent, int width, int drop_down_width)
{
   PopBox *popGUI;

   popGUI = popBox (parent, width, drop_down_width);

   /* build the action menu */
   AddPopBoxMenuItem (popGUI, BUY_STR);
   AddPopBoxMenuItem (popGUI, SELL_STR);
   AddPopBoxMenuItem (popGUI, PRICE_STR);
   AddPopBoxMenuItem (popGUI, INT_STR);
   AddPopBoxMenuItem (popGUI, DIV_STR);
   AddPopBoxMenuItem (popGUI, LTCG_STR);
   AddPopBoxMenuItem (popGUI, STCG_STR);
   AddPopBoxMenuItem (popGUI, DIST_STR);
   AddPopBoxMenuItem (popGUI, SPLIT_STR);
   AddPopBoxMenuItem (popGUI, DEPOSIT_STR);
   AddPopBoxMenuItem (popGUI, WITHDRAW_STR);

   return popGUI;
}

/************************* END OF FILE ******************************/
