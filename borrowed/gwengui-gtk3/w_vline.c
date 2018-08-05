/***************************************************************************
    begin       : Sun May 16 2010
    copyright   : (C) 2010 by Martin Preuss
    email       : martin@libchipcard.de

 ***************************************************************************
 *          Please see toplevel file COPYING for license details           *
 ***************************************************************************/





static GWENHYWFAR_CB
int Gtk3Gui_WVLine_SetIntProperty(GWEN_WIDGET *w,
                                  GWEN_DIALOG_PROPERTY prop,
                                  int index,
                                  int value,
                                  int doSignal) {
  GtkWidget *g;

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(g);

  switch(prop) {
  case GWEN_DialogProperty_Enabled:
    gtk_widget_set_sensitive(GTK_WIDGET(g), (value==0)?FALSE:TRUE);
    return 0;

  case GWEN_DialogProperty_Focus:
    gtk_widget_grab_focus(GTK_WIDGET(g));
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
int Gtk3Gui_WVLine_GetIntProperty(GWEN_WIDGET *w,
                                  GWEN_DIALOG_PROPERTY prop,
                                  int index,
                                  int defaultValue) {
  GtkWidget *g;

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(g);

  switch(prop) {
  case GWEN_DialogProperty_Enabled:
    return (gtk_widget_get_sensitive(GTK_WIDGET(g))==TRUE)?1:0;

  case GWEN_DialogProperty_Focus:
    return (gtk_widget_has_focus(GTK_WIDGET(g))==TRUE)?1:0;
    return 0;

  default:
    break;
  }

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return defaultValue;
}



static int Gtk3Gui_WVLine_Setup(GWEN_WIDGET *w) {
  GtkWidget *g;
  uint32_t flags;
  GWEN_WIDGET *wParent;

  flags=GWEN_Widget_GetFlags(w);
  wParent=GWEN_Widget_Tree_GetParent(w);

  g=gtk_separator_new(GTK_ORIENTATION_VERTICAL);
  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_REAL, (void*) g);
  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_CONTENT, (void*) g);

  GWEN_Widget_SetSetIntPropertyFn(w, Gtk3Gui_WVLine_SetIntProperty);
  GWEN_Widget_SetGetIntPropertyFn(w, Gtk3Gui_WVLine_GetIntProperty);

  if (wParent)
    GWEN_Widget_AddChildGuiWidget(wParent, w);

  return 0;
}


