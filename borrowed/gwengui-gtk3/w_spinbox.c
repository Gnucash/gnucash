/***************************************************************************
    begin       : Sun May 16 2010
    copyright   : (C) 2010 by Martin Preuss
    email       : martin@libchipcard.de

 ***************************************************************************
 *          Please see toplevel file COPYING for license details           *
 ***************************************************************************/



typedef struct W_SPINBOX W_SPINBOX;
struct W_SPINBOX {
  GtkAdjustment *adjustment;
};


GWEN_INHERIT(GWEN_WIDGET, W_SPINBOX)



static GWENHYWFAR_CB
int Gtk3Gui_WSpinBox_SetIntProperty(GWEN_WIDGET *w,
                                    GWEN_DIALOG_PROPERTY prop,
                                    int index,
                                    int value,
                                    int doSignal) {
  GtkWidget *g;
  W_SPINBOX *xw;

  assert(w);
  xw=GWEN_INHERIT_GETDATA(GWEN_WIDGET, W_SPINBOX, w);
  assert(xw);

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(g);

  switch(prop) {
  case GWEN_DialogProperty_Enabled:
    gtk_widget_set_sensitive(GTK_WIDGET(g), (value==0)?FALSE:TRUE);
    return 0;

  case GWEN_DialogProperty_Focus:
    gtk_widget_grab_focus(GTK_WIDGET(g));
    return 0;

  case GWEN_DialogProperty_Width:
  case GWEN_DialogProperty_Height:
    /* just ignore these for now */
    return 0;

  case GWEN_DialogProperty_Value:
    gtk_adjustment_set_value(GTK_ADJUSTMENT(xw->adjustment), value);
    return 0;

  case GWEN_DialogProperty_MinValue:
    gtk_adjustment_set_lower(GTK_ADJUSTMENT(xw->adjustment), value);
    return 0;

  case GWEN_DialogProperty_MaxValue:
    gtk_adjustment_set_upper(GTK_ADJUSTMENT(xw->adjustment), value);
    return 0;

  default:
    break;
  }

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return GWEN_ERROR_INVALID;
}




static GWENHYWFAR_CB
int Gtk3Gui_WSpinBox_GetIntProperty(GWEN_WIDGET *w,
                                    GWEN_DIALOG_PROPERTY prop,
                                    int index,
                                    int defaultValue) {
  GtkWidget *g;
  W_SPINBOX *xw;

  assert(w);
  xw=GWEN_INHERIT_GETDATA(GWEN_WIDGET, W_SPINBOX, w);
  assert(xw);

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(g);

  switch(prop) {
  case GWEN_DialogProperty_Enabled:
    return (gtk_widget_get_sensitive(GTK_WIDGET(g))==TRUE)?1:0;

  case GWEN_DialogProperty_Focus:
    return (gtk_widget_has_focus(GTK_WIDGET(g))==TRUE)?1:0;
    return 0;

  case GWEN_DialogProperty_Width:
  case GWEN_DialogProperty_Height:
    /* just ignore these for now */
    return 0;

  case GWEN_DialogProperty_Value:
    return gtk_adjustment_get_value(GTK_ADJUSTMENT(xw->adjustment));

  case GWEN_DialogProperty_MinValue:
    return gtk_adjustment_get_lower(GTK_ADJUSTMENT(xw->adjustment));

  case GWEN_DialogProperty_MaxValue:
    return gtk_adjustment_get_upper(GTK_ADJUSTMENT(xw->adjustment));

  default:
    break;
  }

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return defaultValue;
}



static GWENHYWFAR_CB
int Gtk3Gui_WSpinBox_SetCharProperty(GWEN_WIDGET *w,
                                     GWEN_DIALOG_PROPERTY prop,
                                     int index,
                                     const char *value,
                                     int doSignal) {
  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return GWEN_ERROR_INVALID;
}



static GWENHYWFAR_CB
const char* Gtk3Gui_WSpinBox_GetCharProperty(GWEN_WIDGET *w,
    GWEN_DIALOG_PROPERTY prop,
    int index,
    const char *defaultValue) {
  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return defaultValue;
}



static void GWENHYWFAR_CB Gtk3Gui_WSpinBox_FreeData(void *bp, void *p) {
  W_SPINBOX *xw;

  xw=(W_SPINBOX*) p;
  GWEN_FREE_OBJECT(xw);
}



static void Gtk3Gui_WSpinBox_Changed_handler(GtkAdjustment *adjustment, gpointer data) {
  GWEN_WIDGET *w;
  int rv;

  w=data;
  assert(w);
  rv=GWEN_Dialog_EmitSignal(GWEN_Widget_GetDialog(w),
                            GWEN_DialogEvent_TypeValueChanged,
                            GWEN_Widget_GetName(w));
  if (rv==GWEN_DialogEvent_ResultAccept)
    Gtk3Gui_Dialog_Leave(GWEN_Widget_GetTopDialog(w), 1);
  else if (rv==GWEN_DialogEvent_ResultReject)
    Gtk3Gui_Dialog_Leave(GWEN_Widget_GetTopDialog(w), 0);
}



static int Gtk3Gui_WSpinBox_Setup(GWEN_WIDGET *w) {
  GtkWidget *g;
  GWEN_WIDGET *wParent;
  W_SPINBOX *xw;

  GWEN_NEW_OBJECT(W_SPINBOX, xw);
  GWEN_INHERIT_SETDATA(GWEN_WIDGET, W_SPINBOX, w, xw, Gtk3Gui_WSpinBox_FreeData);

  wParent=GWEN_Widget_Tree_GetParent(w);

  xw->adjustment=GTK_ADJUSTMENT(gtk_adjustment_new(0.0, 0.0, 100.0, 1.0, 5.0, 5.0));
  g=gtk_spin_button_new(xw->adjustment, 1.0, 0);
  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_REAL, (void*) g);
  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_CONTENT, (void*) g);

  GWEN_Widget_SetSetIntPropertyFn(w, Gtk3Gui_WSpinBox_SetIntProperty);
  GWEN_Widget_SetGetIntPropertyFn(w, Gtk3Gui_WSpinBox_GetIntProperty);
  GWEN_Widget_SetSetCharPropertyFn(w, Gtk3Gui_WSpinBox_SetCharProperty);
  GWEN_Widget_SetGetCharPropertyFn(w, Gtk3Gui_WSpinBox_GetCharProperty);

  g_signal_connect(g,
                   "value-changed",
                   G_CALLBACK (Gtk3Gui_WSpinBox_Changed_handler),
                   w);

  if (wParent)
    GWEN_Widget_AddChildGuiWidget(wParent, w);

  return 0;
}


