

#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <ComboBox.h>

#include "combocell.h"

/* Some GUI-private date that is inappropriate for 
 * the public interface.  In this impelmentation, 
 * it holds XtMotif data that we need.
 */

typedef struct _PopBox {
   Widget combobox;
   Widget parent;       /* the parent table widget */
   int currow;
   int curcol;
} PopBox;


static void selectCB (Widget w, XtPointer cd, XtPointer cb );
static void dropDownCB (Widget w, XtPointer cd, XtPointer cb );
static void realizeCombo (struct _BasicCell *bcell, void *w, int width);
static void moveCombo (struct _BasicCell *bcell, int phys_row, int phys_col);
static void destroyCombo (struct _BasicCell *bcell);
static void setComboValue (struct _BasicCell *bcell, const char *value);
static const char * enterCombo (struct _BasicCell *bcell, const char *value);
static const char * leaveCombo (struct _BasicCell *bcell, const char *value);

#define SET(cell,str) { 			\
   if ((cell)->value) free ((cell)->value);	\
   (cell)->value = strdup (str);		\
}

/* =============================================== */

ComboCell *xaccMallocComboCell (void)
{
   ComboCell * cell;
   cell = (ComboCell *) malloc (sizeof (ComboCell));
   xaccInitComboCell (cell);
   return cell;
}

void xaccInitComboCell (ComboCell *cell)
{
   xaccInitBasicCell ( &(cell->cell));
   cell->cell.realize = realizeCombo;
   cell->cell.set_value = setComboValue;
   cell->menuitems = (char **) malloc (sizeof (char *));
   cell->menuitems[0] = NULL;
}

/* =============================================== */

void 
xaccAddComboCellMenuItem (ComboCell *cell, char * menustr)
{
   int n = 0;
   char ** oldarr;

   if (!cell) return;
   if (!menustr) return;

   oldarr = cell->menuitems;
   while (oldarr[n]) n ++;

   cell->menuitems = (char **) malloc ((n+2) *sizeof (char *));      

   n = 0;
   while (oldarr[n]) {
      cell->menuitems[n] = oldarr[n];
      n++;
   }
   cell->menuitems[n] = strdup (menustr);
   cell->menuitems[n+1] = NULL;

   free (oldarr);

   /* if we are adding the menu item to a cell that 
    * is already realized, then alose add it to the 
    * widget directly.
    */
   if (!cell->cell.realize) {
      PopBox *box;
      XmString str;

      box = (PopBox *) cell->cell.gui_private;
      str = XmStringCreateLtoR (menustr, XmSTRING_DEFAULT_CHARSET);
      XmComboBoxAddItem (box->combobox, str, 0); 
      XmStringFree (str);
   }
}

/* =============================================== */
/* not only do we set the cell contents, but we 
 * make the gui reflect the right value too.
 */

void 
xaccSetComboCellValue (ComboCell *cell, const char * str)
{
   PopBox * box;

   SET (&(cell->cell), str);
   box = (PopBox *) (cell->cell.gui_private);

   if (str) {
      if (0x0 != str[0]) {
         XmString choosen;
         /* convert String to XmString ... arghhh */
         choosen = XmCvtCTToXmString ((char *) str);
         XmComboBoxSelectItem (box->combobox, choosen, False);
         XmStringFree (choosen);
      } else {
         XmComboBoxClearItemSelection (box->combobox);
      } 
   } else {
      XmComboBoxClearItemSelection (box->combobox);
   }
}

/* =============================================== */

static void
setComboValue (struct _BasicCell *_cell, const char *str)
{
   ComboCell * cell = (ComboCell *) _cell;
   xaccSetComboCellValue (cell, str);
}

/* =============================================== */

static
void realizeCombo (struct _BasicCell *bcell, void *w, int pixel_width)
{
   ComboCell *cell;
   PopBox *box;
   Widget parent;
   Widget combobox;
   int width, drop_width;

   parent = (Widget) w;
   cell = (ComboCell *) bcell;

   /* initialize gui-specific, private data */
   box = (PopBox *) malloc (sizeof (PopBox));
   box->parent   = parent;
   box->currow   = -1;
   box->curcol   = -1;

   cell->cell.gui_private = (void *) box;

   /* to mark cell as realized, remove the realize method */
   cell->cell.realize = NULL;
   cell->cell.move = moveCombo;
   cell->cell.enter_cell = enterCombo;
   cell->cell.leave_cell = leaveCombo;
   cell->cell.destroy = destroyCombo;

   /* the combobox wants width in pixels, not in chars.
    * It would be nice if ComboBox supported the XmNunits 
    * resource, but it doesn't.  Also, while we are at it,
    * increase the size of the drop-down box as well. 
    */
   width = pixel_width;
   drop_width =  (int) (1.3 * ((float) width));
   if (15 > drop_width) drop_width = 15;

   /* create the pop GUI */
   combobox = XtVaCreateManagedWidget
                      ("combocell", xmComboBoxWidgetClass, parent, 
                       XmNshadowThickness, 0, /* don't draw a shadow, 
                                               * use bae shadows */
                       XmNeditable, False,    /* user can only pick from list */
                       XmNsorted, False,  
                       XmNshowLabel, False, 
                       XmNmarginHeight, 0,
                       XmNmarginWidth, 0,
                       XmNselectionPolicy, XmSINGLE_SELECT,
                       XmNvalue, "",
                       XmNwidth, width,
                       XmNdropDownWidth, drop_width,
                       NULL);

   box->combobox = combobox;

   /* add callbacks to detect a selection */
   XtAddCallback (combobox, XmNselectionCallback, selectCB, (XtPointer)cell);
   XtAddCallback (combobox, XmNunselectionCallback, selectCB, (XtPointer)cell);
   XtAddCallback (combobox, XmNdropDownCallback, dropDownCB, (XtPointer)box);

   /* unmap the widget by moving it to an invlid location */
   moveCombo (bcell, -1, -1);

   /* add menu items */
   if (cell->menuitems) {
      char * menustr;
      int i=0;
      
      menustr = cell->menuitems[i];
      while (menustr) {
         XmString str;
         str = XmStringCreateLtoR (menustr, XmSTRING_DEFAULT_CHARSET);
         XmComboBoxAddItem (box->combobox, str, 0); 
         XmStringFree (str);
         i++;
         menustr = cell->menuitems[i];
      }
   }
}

/* =============================================== */

static
void moveCombo (struct _BasicCell *bcell, int phys_row, int phys_col)
{
   ComboCell *cell;
   PopBox *box;

   cell = (ComboCell *) bcell;
   box = (PopBox *) (cell->cell.gui_private);

   /* if the drop-down menu is showing, hide it now */
   XmComboBoxHideList (box->combobox);

   /* if there is an old widget, remove it */
   if ((0 <= box->currow) && (0 <= box->curcol)) {
      XbaeMatrixSetCellWidget (box->parent, box->currow, box->curcol, NULL);
   }
   box->currow = phys_row;
   box->curcol = phys_col;

   XtUnmanageChild (box->combobox); 
}

/* =============================================== */

static
const char * enterCombo (struct _BasicCell *bcell, const char *value)
{
   int phys_row, phys_col;
   String choice;
   XmString choosen;
   ComboCell *cell;
   PopBox *box;

   cell = (ComboCell *) bcell;
   box = (PopBox *) (cell->cell.gui_private);

   phys_row = box->currow;
   phys_col = box->curcol;

   /* if the new position is valid, go to it, 
    * otherwise, unmanage the widget */
   if ((0 <= phys_row) && (0 <= phys_col)) {

      /* Get the current cell contents, and set the
       * combobox menu selection to match the contents. 
       * We could use the value passed in, but things should
       * be consitent, so we don't need it. */
      choice = cell->cell.value;

      /* do a menu selection only if the cell ain't empty. */
      if (choice) {
         if (0x0 != choice[0]) {
            /* convert String to XmString ... arghhh */
            choosen = XmCvtCTToXmString (choice);
            XmComboBoxSelectItem (box->combobox, choosen, False);
            XmStringFree (choosen);
         } else {
            XmComboBoxClearItemSelection (box->combobox);
         } 
      } else {
         XmComboBoxClearItemSelection (box->combobox);
      }

      /* set the cell widget */
      XbaeMatrixSetCellWidget (box->parent, phys_row, phys_col, box->combobox);

      if (!XtIsManaged (box->combobox)) {
         XtManageChild (box->combobox);
      }

      /* drop down the menu so that its ready to go. */
      XmComboBoxShowList (box->combobox); 
   } else {
      XtUnmanageChild (box->combobox); 
   }

   return NULL;
}

/* =============================================== */

static
const char * leaveCombo (struct _BasicCell *bcell, const char *value)
{
   ComboCell *cell;
   PopBox *box;

   cell = (ComboCell *) bcell;
   box = (PopBox *) (cell->cell.gui_private);

   /* if the drop-down menu is showing, hide it now */
   XmComboBoxHideList (box->combobox);

   /* if there is an old widget, remove it */
   if ((0 <= box->currow) && (0 <= box->curcol)) {
      XbaeMatrixSetCellWidget (box->parent, box->currow, box->curcol, NULL);
   }

   XtUnmanageChild (box->combobox); 

   return NULL;
}

/* =============================================== */

static
void destroyCombo (struct _BasicCell *bcell)
{
   ComboCell *cell;
   PopBox *box;

   cell = (ComboCell *) bcell;
   box = (PopBox *) (cell->cell.gui_private);

   moveCombo (bcell, -1, -1);

   XtDestroyWidget (box->combobox);
   free (box);

   /* allow the widget to be created again */
   cell->cell.gui_private = NULL;
   cell->cell.realize = realizeCombo;
   cell->cell.move = NULL;
   cell->cell.enter_cell = NULL;
   cell->cell.leave_cell = NULL;
   cell->cell.destroy = NULL;
}

/* =============================================== */

static void selectCB (Widget w, XtPointer cd, XtPointer cb )

{
   ComboCell *cell;
   PopBox *box;
   char * choice = 0x0;

   XmComboBoxSelectionCallbackStruct *selection = 
               (XmComboBoxSelectionCallbackStruct *) cb;

   cell = (ComboCell *) cd;
   box = (PopBox *) (cell->cell.gui_private);

   /* check for a valid mapping of the widget.  
    * Note that if the combo box value is set to 
    * a string that is not in the combo box menu
    * (for example, the empty string ""), then the 
    * combobox will issue an XmCR_UNSELECT event.
    * This typically happens while loading the array.
    * We want to ignore these. */
   if ((0 > box->currow) || (0 > box->curcol)) return;

   /* check the reason, because the unslect callback 
    * doesn't even have a value field! */
   if ( (XmCR_SINGLE_SELECT == selection->reason) ||
        (XmCR_BROWSE_SELECT == selection->reason) ) {
      choice = XmCvtXmStringToCT (selection->value);
   } else 
   if (XmCR_UNSELECT == selection->reason) {
      choice = XtNewString ("");
   } else {
      return;
   }

   XbaeMatrixSetCell (box->parent, box->currow, box->curcol, choice); 
   SET (&(cell->cell), choice);
   XtFree (choice);

   /* a diffeent way of getting the user's selection ... */
   /* text = XmComboBoxGetString (ab->combobox); */
}

/* =============================================== */

/*********************************************************\
 * fix traversal by going back to the register window
 * when the pull-down menu goes away.  We do NOT want to
 * go to the default next tab group, which is probably 
 * some button not in the register window.
\*********************************************************/

static void dropDownCB (Widget w, XtPointer cd, XtPointer cb )

{
    PopBox *ab = (PopBox *) cd;
    XmComboBoxDropDownCallbackStruct *ddcb = 
               (XmComboBoxDropDownCallbackStruct *) cb;

   if (XmCR_HIDE_LIST == ddcb->reason) {
      XmProcessTraversal(ab->parent, XmTRAVERSE_CURRENT); 
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
         if ((KeyPress == ddcb->event->type) || (KeyRelease == ddcb->event->type
)) {
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

/* =============== end of file =================== */
