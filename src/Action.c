/********************************************************************\
 * action.c -- account actions for xacc (X-Accountant)              *
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
#include <ComboBox.h>
#include <Xbae/Matrix.h>
#include "Action.h"
#include "util.h"

/** STRUCTS *********************************************************/
typedef struct _ActionBox {
   Widget combobox;
   Widget reg;       /* the parent register widget */
   int currow;
   int curcol;
} ActionBox;

/** PROTOTYPES ******************************************************/

void selectCB (Widget w, XtPointer cd, XtPointer cb );


/********************************************************************\
 * actionBox                                                        *
 *   creates the action widget                                      *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 * Return: actionData - the action GUI structure                    *
\********************************************************************/

#define ADD_MENU_ITEM(menustr) {					\
   str = XmStringCreateLtoR (menustr, XmSTRING_DEFAULT_CHARSET);	\
   XmComboBoxAddItem(combobox, str, 0); XmStringFree(str);		\
}


ActionBox *
actionBox (Widget parent)
{
   Widget combobox;
   XmString str;
   ActionBox *actionData;

   /* malloc the action GUI structure */
   actionData = (ActionBox *) _malloc (sizeof (ActionBox));
   actionData->currow = -1;
   actionData->curcol = -1;
   actionData->reg = parent;

   /* create the action GUI */
   combobox = XtVaCreateManagedWidget("actionbox", xmComboBoxWidgetClass, parent, 
                       XmNshadowThickness, 0, /* don't draw a shadow, use bae shadows */
                       XmNeditable, False,    /* user can only pick from list */
                       XmNsorted, False,  
                       XmNshowLabel, False, 
                       XmNmarginHeight, 0,
                       XmNmarginWidth, 0,
                       XmNselectionPolicy, XmSINGLE_SELECT,
                       XmNvalue, "",

/* hack alert -- the width of the combobox should be relative to the font, should
   be relative to the size of the cell in which it will fit. */
                       XmNwidth, 43,
                       NULL);

   actionData -> combobox = combobox;

   /* build the action menu */
   ADD_MENU_ITEM("Buy");
   ADD_MENU_ITEM("Sell");
   ADD_MENU_ITEM("Price");
   ADD_MENU_ITEM("Div");
   ADD_MENU_ITEM("LTCG");
   ADD_MENU_ITEM("STCG");
   ADD_MENU_ITEM("Dist");
   ADD_MENU_ITEM("Split");

   /* add callbacks to detect a selection */
   XtAddCallback (combobox, XmNselectionCallback, selectCB, (XtPointer)actionData);
   XtAddCallback (combobox, XmNunselectionCallback, selectCB, (XtPointer)actionData);


   return actionData;
}

/********************************************************************\
\********************************************************************/

void SetActionBox (ActionBox *ab, int row, int col)
{
   String choice;
   XmString choosen;

   /* if the drop-down menu is showing, hide it now */
   XmComboBoxHideList (ab->combobox);

   /* if there is an old widget, remove it */
   if ((0 <= ab->currow) && (0 <= ab->curcol)) {
      XbaeMatrixSetCellWidget (ab->reg, ab->currow, ab->curcol, NULL);
   }
   ab->currow = row;
   ab->curcol = col;

   /* if the new position is valid, go to it, 
    * otherwise, unmanage the widget */
   if ((0 <= ab->currow) && (0 <= ab->curcol)) {

      /* Get the current cell contents, and set the
       * combobox menu selction to match the contents */
      choice = XbaeMatrixGetCell (ab->reg, ab->currow, ab->curcol);

      /* do a menu selection only if the cell ain't empty. */
      if (0x0 != choice[0]) {
         /* convert String to XmString ... arghhh */
         choosen = XmCvtCTToXmString (choice);
         XmComboBoxSelectItem (ab->combobox, choosen, False);
         XmStringFree (choosen);
      } else {
         XmComboBoxClearItemSelection (ab->combobox);
      }

      /* set the cell widget */
      XbaeMatrixSetCellWidget (ab->reg, row, col, ab->combobox);

      if (!XtIsManaged (ab->combobox)) {
         XtManageChild (ab->combobox);
      }

      /* drop down the menu so that its ready to go. */
      XmComboBoxShowList (ab->combobox);
   } else {
      XtUnmanageChild (ab->combobox); 
  }
}

/********************************************************************\
 * selectCB -- get the user's selection, put the string into the    *
 *             cell.                                                *
 *                                                                  *
 * Args:   w - the widget that called us                            *
 *         cd - actionData - the data struct for this combobox      *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/

void selectCB (Widget w, XtPointer cd, XtPointer cb )

{
    ActionBox *ab = (ActionBox *) cd;
    XmComboBoxSelectionCallbackStruct *selection = 
               (XmComboBoxSelectionCallbackStruct *) cb;
    char * choice;

    choice = XmCvtXmStringToCT (selection->value);
    if (0x0 == choice) choice = "";

    XbaeMatrixSetCell (ab->reg, ab->currow, ab->curcol, choice); 

    /* a diffeent way of getting the user's selection ... */
    /* text = XmComboBoxGetString (ab->combobox); */
}
/************************* END OF FILE ******************************/
