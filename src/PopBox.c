/********************************************************************\
 * Pop.c -- generic ComboBox for xacc (X-Accountant)                *
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

#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <ComboBox.h>
#include <Xbae/Matrix.h>

#include "config.h"

#include "util.h"

/** STRUCTS *********************************************************/
typedef struct _PopBox {
   Widget combobox;
   Widget reg;       /* the parent register widget */
   int currow;
   int curcol;
} PopBox;

/** PROTOTYPES ******************************************************/

static void selectCB (Widget w, XtPointer cd, XtPointer cb );
static void dropDownCB (Widget w, XtPointer cd, XtPointer cb );

/********************************************************************\
 * popBox                                                           *
 *   creates the pop widget                                         *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 * Return: popData - the pop GUI structure                          *
\********************************************************************/

PopBox *
popBox (Widget parent, int width, int drop_width)
{
   Widget combobox;
   PopBox *popData;

   /* malloc the pop GUI structure */
   popData = (PopBox *) _malloc (sizeof (PopBox));
   popData->currow = -1;
   popData->curcol = -1;
   popData->reg = parent;

   /* create the pop GUI */
   combobox = XtVaCreateManagedWidget("popbox", xmComboBoxWidgetClass, parent, 
                       XmNshadowThickness, 0, /* don't draw a shadow, use bae shadows */
                       XmNeditable, False,    /* user can only pick from list */
                       XmNsorted, False,  
                       XmNshowLabel, False, 
                       XmNmarginHeight, 0,
                       XmNmarginWidth, 0,
                       XmNselectionPolicy, XmSINGLE_SELECT,
                       XmNvalue, "",
                       XmNwidth, width,
                       NULL);

   if (10 < drop_width) {
      XtVaSetValues (combobox, XmNdropDownWidth, drop_width, NULL);
   }

   popData -> combobox = combobox;

   /* add callbacks to detect a selection */
   XtAddCallback (combobox, XmNselectionCallback, selectCB, (XtPointer)popData);
   XtAddCallback (combobox, XmNunselectionCallback, selectCB, (XtPointer)popData);
   XtAddCallback (combobox, XmNdropDownCallback, dropDownCB, (XtPointer)popData);

   return popData;
}

/********************************************************************\
 * AddPopBoxMenuItem                                                *
 *   add a menu item to the pop box                                 *
 *                                                                  *
 * Args:   PopBox  - the pop GUI structure                          *
 *         menustr -- the menu entry to be added                    *
 * Return: void                                                     *
\********************************************************************/

void AddPopBoxMenuItem (PopBox *ab, char * menustr)
{
   XmString str;
   str = XmStringCreateLtoR (menustr, XmSTRING_DEFAULT_CHARSET);
   XmComboBoxAddItem (ab->combobox, str, 0); 
   XmStringFree (str);
}

/********************************************************************\
 * SetPopBox                                                        *
 *   moves the ComboBox to the indicated column, row                *
 *                                                                  *
 * Args:   PopBox  - the pop GUI structure                          *
 *         row     -- the row of the Xbae Matrix                    *
 *         col     -- the col of the Xbae Matrix                    *
 * Return: void                                                     *
\********************************************************************/


void SetPopBox (PopBox *ab, int row, int col)
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
      if (choice) {
         if (0x0 != choice[0]) {
            /* convert String to XmString ... arghhh */
            choosen = XmCvtCTToXmString (choice);
            XmComboBoxSelectItem (ab->combobox, choosen, False);
            XmStringFree (choosen);
         } else {
            XmComboBoxClearItemSelection (ab->combobox);
         } 
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
\********************************************************************/

void freePopBox (PopBox *ab)
{
  if (!ab) return;
  SetPopBox (ab, -1, -1);
  XtDestroyWidget (ab->combobox);
  _free (ab);
}

/********************************************************************\
 * selectCB -- get the user's selection, put the string into the    *
 *             cell.                                                *
 *                                                                  *
 * Args:   w - the widget that called us                            *
 *         cd - popData - the data struct for this combobox         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/

static void selectCB (Widget w, XtPointer cd, XtPointer cb )

{
    PopBox *ab = (PopBox *) cd;
    XmComboBoxSelectionCallbackStruct *selection = 
               (XmComboBoxSelectionCallbackStruct *) cb;
    char * choice = 0x0;

    /* check the reason, because the unslect callback 
     * doesn't even have a value field! */
    if ( (XmCR_SINGLE_SELECT == selection->reason) ||
         (XmCR_SINGLE_SELECT == selection->reason) ) {
       choice = XmCvtXmStringToCT (selection->value);
    }
    if (!choice) choice = "";

    XbaeMatrixSetCell (ab->reg, ab->currow, ab->curcol, choice); 

    /* a diffeent way of getting the user's selection ... */
    /* text = XmComboBoxGetString (ab->combobox); */
}

/********************************************************************\
 * fix traversal by going back to the register window
 * when the pull-down menu goes away.  We do NOT want to
 * go to the default next tab group, which is probably 
 * some button not in theregister window.
\********************************************************************/

static void dropDownCB (Widget w, XtPointer cd, XtPointer cb )

{
    PopBox *ab = (PopBox *) cd;
    XmComboBoxDropDownCallbackStruct *ddcb = 
               (XmComboBoxDropDownCallbackStruct *) cb;


   if (XmCR_HIDE_LIST == ddcb->reason) {
      XmProcessTraversal(ab->reg, XmTRAVERSE_CURRENT);
   }
 
#ifdef USE_COMPLEX_TRAVERSAL_LOGIC
   /* continue traversal only under certain special curcumstances */
   if (XmCR_HIDE_LIST == ddcb->reason) {
      if (ddcb->event) {

         /* don't leave if only a focus-out */
         if (FocusOut == ddcb->event->type) {
            XmProcessTraversal(ab->combobox, XmTRAVERSE_CURRENT);
         } else 

         /* if user selected something, then go to next cell */
         if (ButtonRelease == ddcb->event->type) {
            XmProcessTraversal(ab->reg, XmTRAVERSE_CURRENT);
         } else 

         /* if user hit the tab key, go to next cell */
         if ((KeyPress == ddcb->event->type) || (KeyRelease == ddcb->event->type)) {
            KeySym sim;
            XKeyEvent *kev = (XKeyEvent *) ddcb->event;
            sim = XLookupKeysym (kev, 0);
            if (XK_Tab == sim) {   /* did the user hit the tab key ?? */
               XmProcessTraversal(ab->reg, XmTRAVERSE_CURRENT);
            }
         }
      }
   }
#endif /* USE_COMPLEX_TRAVERSAL_LOGIC */
}
/************************* END OF FILE ******************************/
