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
#include "Action.h"
#include "util.h"

/** STRUCTS *********************************************************/
typedef struct _ActionBox {
   Widget combobox;
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
ActionBox *
actionBox (Widget parent)
{
   Widget combobox;
   XmString str;
   ActionBox *actionData;

   /* malloc the action GUI structure */
   actionData = (ActionBox *) _malloc (sizeof (ActionBox));

   /* create the action GUI */
   combobox = XtVaCreateManagedWidget("combobox", xmComboBoxWidgetClass, parent, 
                       XmNeditable, False,
                       XmNsorted, True,  
                       XmNshowLabel, False, 
                       NULL);

   actionData -> combobox = combobox;

   /* build the action menu */
   str = XmStringCreateLtoR ("Buy", XmSTRING_DEFAULT_CHARSET);
   XmComboBoxAddItem(combobox, str, 0); XmStringFree(str);

   str = XmStringCreateLtoR ("Sell", XmSTRING_DEFAULT_CHARSET);
   XmComboBoxAddItem(combobox, str, 0); XmStringFree(str);

   /* add callbacks to detect a selection */
   XtAddCallback (combobox, XmNselectionCallback, selectCB, (XtPointer)actionData);

   XtManageChild (combobox);

   return actionData;
}

/********************************************************************\
 * selectCB -- figure out the user's selection                      *
 *                                                                  *
 * Args:   w - the widget that called us                            *
 *         cd - actionData - the data struct for this combobox      *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/

void selectCB (Widget w, XtPointer cd, XtPointer cb )

{
    ActionBox *actionData = (ActionBox *) cd;
    XmComboBoxSelectionCallbackStruct *selection = 
               (XmComboBoxSelectionCallbackStruct *) cb;

    printf (" choosed %s \n", selection->value);

    /* text = XmComboBoxGetString(ComboBox1); */
}
/************************* END OF FILE ******************************/
