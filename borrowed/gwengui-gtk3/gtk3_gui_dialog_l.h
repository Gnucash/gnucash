/***************************************************************************
    begin       : Sun May 16 2010
    copyright   : (C) 2010 by Martin Preuss
    email       : martin@libchipcard.de

 ***************************************************************************
 *          Please see toplevel file COPYING for license details           *
 ***************************************************************************/

#ifndef GTK3_GUI_DIALOG_L_H
#define GTK3_GUI_DIALOG_L_H


#include "gtk3_gui.h"


#include <gtk/gtk.h>

#include <gwenhywfar/dialog_be.h>


#define GTK3_GUI_DIALOG_DEFAULT_BOX_SPACING 3


typedef struct {
  GWEN_DIALOG *dialog;
  GtkWindow *window;
  int response;
  GMainLoop *loop;
  int destroyed;
} RunInfo;



void Gtk3Gui_Dialog_Extend(GWEN_DIALOG *dlg);
void Gtk3Gui_Dialog_Unextend(GWEN_DIALOG *dlg);

int Gtk3Gui_Dialog_Setup(GWEN_DIALOG *dlg, GtkWidget *parentWindow);


GtkWidget *Gtk3Gui_Dialog_GetMainWidget(const GWEN_DIALOG *dlg);


int GTK3_Gui_Dialog_Run(GWEN_DIALOG *dlg, int timeout);
void Gtk3Gui_Dialog_Leave(GWEN_DIALOG *dlg, int result);

int Gtk3Gui_GetRawText(const char *text, GWEN_BUFFER *tbuf);

#endif


