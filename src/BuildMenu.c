/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 * Permission to use, copy, and modify this program without
 * restriction is hereby granted, as long as this copyright
 * notice appears in each copy of the program source code.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* Modified by Rob Clark on Nov 26, 1995
 *   Changes made: reorginization of code, and addition choice of initial 
 *   option menu selection
 *                    - moved the creation of cascade buttion for 
 *                      option and pulldown menu till after the 
 *                      items in the menu are created... this way 
 *                      I can specify a widget (item in menu) to 
 *                      be the inital choice for option menues!
 *                    - Added argument (int)initial to 
 *                      facilitate choice of initial choice.
 *                    - modified the for loop which adds menu items.
 *                      If the menu item number (0,1,2,..) of the
 *                      menu item being created corresponds to the
 *                      (int)initial specified by the caller of 
 *                      the function, then make note of the widget 
 *                      id of menu item so we can use it to set 
 *                      initial menu choice.  If the menu item 
 *                      created is the first menu item, make note
 *                      of it's widget id, in case (int)initial 
 *                      specified by user is bigger than the number 
 *                      of menu items.
 *                    - Added check to see if initial_choice was set
 *                      equal to a widget id.  If (int)initial is 
 *                      bigger than number of menu items, then  set
 *                      initial_choice to = first menu item's widget
 *                      id, to prevent bad things from happening.
 */

#include <Xm/Xm.h>
#include <X11/cursorfont.h>
#include <string.h>

#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/FileSB.h>
#include <Xm/MessageB.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/ScrolledW.h>
#include <Xm/DrawnB.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeBG.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/Scale.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/SeparatoG.h>

#include "BuildMenu.h"

/* Build popup, option and pulldown menus, depending on the menu_type.
 * It may be XmMENU_PULLDOWN, XmMENU_OPTION or  XmMENU_POPUP.  Pulldowns
 * return the CascadeButton that pops up the menu.  Popups return the menu.
 * Option menus are created, but the RowColumn that acts as the option
 * "area" is returned unmanaged. (The user must manage it.)
 * Pulldown menus are built from cascade buttons, so this function
 * also builds pullright menus.  The function also adds the right
 * callback for PushButton or ToggleButton menu items.
 */
Widget BuildMenu( Widget parent, int menu_type, char *menu_title, 
		  char menu_mnemonic, Boolean tear_off, int initial, 
		  MenuItem *items)
  {
  Widget    menu,
            cascade, 
            widget,
            first_menuitem,
            initial_choice;
  int       i;
  XmString  str;

  if( menu_type == XmMENU_PULLDOWN || menu_type == XmMENU_OPTION )
    menu = XmCreatePulldownMenu( parent, "_pulldown", NULL, 0 );
  else if( menu_type == XmMENU_POPUP )
    menu = XmCreatePopupMenu( parent, "_popup", NULL, 0 );
  else 
    {
    XtWarning ("Invalid menu type passed to BuildMenu()");
    return NULL;
    }
  if(tear_off)
    XtVaSetValues( menu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL );

  /* Now add the menu items */
  for( i=0; items[i].label != NULL; i++ ) 
    {
    /* If subitems exist, create the pull-right menu by calling this
     * function recursively.  Since the function returns a cascade
     * button, the widget returned is used..
     */
    if( items[i].subitems )
      {
      if( menu_type == XmMENU_OPTION ) 
	{
	XtWarning("You can't have submenus from option menu items.");
	continue;
	} else {
printf ("creating submen label %s \n", items[i].label);
        widget = BuildMenu( menu, XmMENU_PULLDOWN, items[i].label, 
			    items[i].mnemonic, tear_off, 0, 
			    items[i].subitems);
        }
      }
    else
      {
      widget = XtVaCreateManagedWidget( items[i].label,
					*(items[i].wclass), menu,
					NULL );
      }

    /* If the current widget (menu item) being created corresponds to the 
     * int that the user specified as initial choice, then set initial_choice
     * = widget.  The (int)initial specified by caller of function is the 
     * number of the menu item (ie, 0th, 1st, 2nd,...), but when we create 
     * the cascade button a little later, we need a widget id to specify as 
     * the inital choice.
     */
    if( i == initial )
      initial_choice = widget;
    /* If this is the first menu item, set first_menuitem = widget
     * This is incase (int)initial > (number of menu items), we need to
     * set (Widget)initial_choice equal to the first menu item
     */
    if( i == 0 )
      first_menuitem = widget;

    /* Whether the item is a real item or a cascade button with a
     * menu, it can still have a mnemonic.
     */
    if(items[i].mnemonic)
      XtVaSetValues( widget, XmNmnemonic, items[i].mnemonic, NULL );

    /* any item can have an accelerator, except cascade menus. But,
     * we don't worry about that; we know better in our declarations.
     */
    if(items[i].accelerator) 
      {
      str = XmStringCreateLocalized(items[i].accel_text);
      XtVaSetValues( widget,
		     XmNaccelerator, items[i].accelerator,
		     XmNacceleratorText, str,
		     NULL );
      XmStringFree(str);
      }

    if( items[i].callback )
      {
      if( (items[i].wclass == &xmToggleButtonWidgetClass) ||
	 (items[i].wclass == &xmToggleButtonGadgetClass) )
	{
	XtAddCallback( widget, XmNvalueChangedCallback,
		      items[i].callback, items[i].callback_data );
	}
      else
	{
	XtAddCallback( widget, XmNactivateCallback,
		      items[i].callback, items[i].callback_data );
	}
      }
    }

  /* If the (int)initial set by user is greater than the number of menu
   * items, initial_choice will not have been set in the for loop where
   * the menu items are added.  If this is the case, we will default to
   * the first menu item
   */
  if( initial > (i-1) )
    initial_choice = first_menuitem;

  /* Pulldown menus require a cascade button to be made */
  if( menu_type == XmMENU_PULLDOWN ) 
    {
    str = XmStringCreateLocalized (menu_title);
    cascade = XtVaCreateManagedWidget( menu_title,
				       xmCascadeButtonGadgetClass, parent,
				       XmNsubMenuId,   menu,
				       XmNlabelString, str,
				       XmNmnemonic,    menu_mnemonic,
				       NULL );
    XmStringFree (str);
    } 
  else if( menu_type == XmMENU_OPTION ) 
    {
    /* Option menus are a special case, but not hard to handle */
    Arg args[3];
    int n = 0;
    str = XmStringCreateLocalized(menu_title);
    XtSetArg( args[n], XmNsubMenuId, menu ); n++;
    XtSetArg( args[n], XmNlabelString, str ); n++;
    XtSetArg( args[n], XmNmenuHistory, initial_choice ); n++;
    /* This really isn't a cascade, but this is the widget handle
     * we're going to return at the end of the function.
     */
    cascade = XmCreateOptionMenu( parent, menu_title, args, n );
    XmStringFree(str);
    }

  /* for popup menus, just return the menu; pulldown menus, return
   * the cascade button; option menus, return the thing returned
   * from XmCreateOptionMenu().  This isn't a menu, or a cascade button!
   */
  return (menu_type == XmMENU_POPUP) ? menu : cascade;
  }
