/***************************************************************************
    begin       : Sun May 16 2010
    copyright   : (C) 2010 by Martin Preuss
    email       : martin@libchipcard.de

 ***************************************************************************
 *          Please see toplevel file COPYING for license details           *
 ***************************************************************************/

#ifndef GTK3_GUI_DIALOG_P_H
#define GTK3_GUI_DIALOG_P_H


#include "gtk3_gui_dialog_l.h"


typedef struct GTK3_GUI_DIALOG GTK3_GUI_DIALOG;
struct GTK3_GUI_DIALOG {
  GWEN_DIALOG_SETINTPROPERTY_FN setIntPropertyFn;
  GWEN_DIALOG_GETINTPROPERTY_FN getIntPropertyFn;
  GWEN_DIALOG_SETCHARPROPERTY_FN setCharPropertyFn;
  GWEN_DIALOG_GETCHARPROPERTY_FN getCharPropertyFn;

  GtkWidget *mainWidget;

  int response;
  GMainLoop *loop;
  int destroyed;

  gulong unmap_handler;
  gulong destroy_handler;
  gulong delete_handler;

};

static void GWENHYWFAR_CB Gtk3Gui_Dialog_FreeData(void *bp, void *p);


static int GWENHYWFAR_CB Gtk3Gui_Dialog_SetIntProperty(GWEN_DIALOG *dlg,
    GWEN_WIDGET *w,
    GWEN_DIALOG_PROPERTY prop,
    int index,
    int value,
    int doSignal);


static int GWENHYWFAR_CB Gtk3Gui_Dialog_GetIntProperty(GWEN_DIALOG *dlg,
    GWEN_WIDGET *w,
    GWEN_DIALOG_PROPERTY prop,
    int index,
    int defaultValue);

static int GWENHYWFAR_CB Gtk3Gui_Dialog_SetCharProperty(GWEN_DIALOG *dlg,
    GWEN_WIDGET *w,
    GWEN_DIALOG_PROPERTY prop,
    int index,
    const char *value,
    int doSignal);

static const char * GWENHYWFAR_CB Gtk3Gui_Dialog_GetCharProperty(GWEN_DIALOG *dlg,
    GWEN_WIDGET *w,
    GWEN_DIALOG_PROPERTY prop,
    int index,
    const char *defaultValue);


static int Gtk3Gui_Dialog_SetupTree(GWEN_WIDGET *w);


#endif


