

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
   PopBox *box;

   xaccInitBasicCell ( &(cell->cell));

   cell->gui = (struct _PopBox *) box;
}

/* =============================================== */

void RealizeCombo (struct _BasicCell *bcell, void *w)
{
   ComboCell *cell;
   PopBox *box;
   Widget parent;
   Widget combobox;

   int width = 10;   /* hack alert --- */
   int drop_width = 10;

   parent = (Widget) w;
   cell = (ComboCell *) bcell;

   box = (PopBox *) malloc (sizeof (PopBox));
   box->parent   = parent;
   box->currow   = -1;
   box->curcol   = -1;

   cell->gui = (struct _PopBox *) box;

   /* create the pop GUI */
   combobox = XtVaCreateManagedWidget
                      ("popbox", xmComboBoxWidgetClass, parent, 
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
                       NULL);

   box->combobox = combobox;

   if (10 < drop_width) {
      XtVaSetValues (combobox, XmNdropDownWidth, drop_width, NULL);
   }

   box->combobox = combobox;

   /* add callbacks to detect a selection */
   XtAddCallback (combobox, XmNselectionCallback, selectCB, (XtPointer)box);
   XtAddCallback (combobox, XmNunselectionCallback, selectCB, (XtPointer)box);
   XtAddCallback (combobox, XmNdropDownCallback, dropDownCB, (XtPointer)box);
}

/* =============================================== */

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

printf ("yo combo select %s \n", choice);

    /* XbaeMatrixSetCell (ab->reg, ab->currow, ab->curcol, choice); */

    /* a diffeent way of getting the user's selection ... */
    /* text = XmComboBoxGetString (ab->combobox); */
}

/* =============================================== */

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
