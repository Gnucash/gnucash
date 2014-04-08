/********************************************************************\
 * Reports.c -- generate an account report window                   *
 * Copyright (C) 1997 Robin D. Clark                                *
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
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/LabelGP.h>

#include "config.h"

#include "Reports.h"
#include "util.h"


/********************************************************************\
 * simpleReportWindow                                               *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 * Return: none                                                     *
\********************************************************************/
void 
simpleReportWindow( Widget parent )
  {
  Widget    dialog, form, frame, rc, widget, 
            label, buttonform;
  setBusyCursor( parent );
  
  /* force the size of the dialog so it is not resizable */
  dialog = XtVaCreatePopupShell( "dialog", 
				 xmDialogShellWidgetClass, parent,
				 XmNtitle,            "Report",
				 XmNdeleteResponse,   XmDESTROY,
				 XmNwidth,     350,
				 XmNminWidth,  350,
				 XmNmaxWidth,  350,
				 XmNheight,    300,
				 XmNminHeight, 300,
				 XmNmaxHeight, 300,
				 NULL );
  
  /* The form to put everything in the dialog in */
  form = XtVaCreateWidget( "form", xmFormWidgetClass, dialog, NULL );
  
  /******************************************************************\
   * The report type area                                           *
  \******************************************************************/
  
   /* Makes a nice looking frame */
  frame = XtVaCreateManagedWidget( "frame", 
				   xmFrameWidgetClass, form,
				   XmNtopAttachment,   XmATTACH_FORM,
				   XmNleftAttachment,  XmATTACH_FORM,
				   XmNleftOffset,      20,
				   XmNrightAttachment, XmATTACH_FORM,
				   XmNrightOffset,     20,
				   NULL);
  
  
  /******************************************************************\
   * The buttons at the bottom...                                   *
  \******************************************************************/
  
  buttonform = XtVaCreateWidget( "buttonform", 
				 xmFormWidgetClass,   form,
				 XmNfractionBase,     5,
				 XmNtopAttachment,    XmATTACH_WIDGET,
				 XmNtopWidget,        frame,
				 XmNtopOffset,        10,
				 XmNbottomAttachment, XmATTACH_FORM,
				 XmNbottomOffset,     10,
				 XmNleftAttachment,   XmATTACH_FORM,
				 XmNrightAttachment,  XmATTACH_FORM,
				 NULL );
  
  
  /* The "Ok" button */
  widget = XtVaCreateManagedWidget( "Ok", 
				    xmPushButtonWidgetClass, buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       1,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      2,
				    XmNshowAsDefault,      True,
				    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 destroyShellCB, (XtPointer)dialog );  
  
  XtManageChild(buttonform);
  
  /******************************************************************/
  XtManageChild(form);
  
  XtPopup( dialog, XtGrabNone );
  
  unsetBusyCursor( parent );
  }
