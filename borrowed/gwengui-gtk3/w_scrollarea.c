/***************************************************************************
    begin       : Sun May 16 2010
    copyright   : (C) 2010 by Martin Preuss
    email       : martin@libchipcard.de

 ***************************************************************************
 *          Please see toplevel file COPYING for license details           *
 ***************************************************************************/





static GWENHYWFAR_CB
int Gtk3Gui_WScrollArea_SetIntProperty(GWEN_WIDGET *w,
                                       GWEN_DIALOG_PROPERTY prop,
                                       int index,
                                       int value,
                                       int doSignal) {
  GtkWidget *g;  /* text view */
  GtkWidget *gs; /* scrollable window */

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_CONTENT));
  assert(g);
  gs=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(gs);

  switch(prop) {
  case GWEN_DialogProperty_Enabled:
    gtk_widget_set_sensitive(GTK_WIDGET(gs), (value==0)?FALSE:TRUE);
    return 0;

  case GWEN_DialogProperty_Focus:
    gtk_widget_grab_focus(GTK_WIDGET(gs));
    return 0;

  case GWEN_DialogProperty_Width:
  case GWEN_DialogProperty_Height:
    /* just ignore these for now */
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
int Gtk3Gui_WScrollArea_GetIntProperty(GWEN_WIDGET *w,
                                       GWEN_DIALOG_PROPERTY prop,
                                       int index,
                                       int defaultValue) {
  GtkWidget *g;  /* text view */
  GtkWidget *gs; /* scrollable window */

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_CONTENT));
  assert(g);
  gs=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(gs);

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_CONTENT));
  assert(g);

  switch(prop) {
  case GWEN_DialogProperty_Enabled:
    return (gtk_widget_get_sensitive(GTK_WIDGET(gs))==TRUE)?1:0;

  case GWEN_DialogProperty_Focus:
    return (gtk_widget_has_focus(GTK_WIDGET(gs))==TRUE)?1:0;
    return 0;

  case GWEN_DialogProperty_Width:
  case GWEN_DialogProperty_Height:
    /* just ignore these for now */
    return 0;

  default:
    break;
  }

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return defaultValue;
}



static GWENHYWFAR_CB
int Gtk3Gui_WScrollArea_SetCharProperty(GWEN_WIDGET *w,
                                        GWEN_DIALOG_PROPERTY prop,
                                        int index,
                                        const char *value,
                                        int doSignal) {
  GtkWidget *g;         /* scrollable window */
  GtkWidget *gContent;  /* vbox */

  gContent=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_CONTENT));
  assert(gContent);
  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(g);

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return GWEN_ERROR_INVALID;
}



static GWENHYWFAR_CB
const char* Gtk3Gui_WScrollArea_GetCharProperty(GWEN_WIDGET *w,
    GWEN_DIALOG_PROPERTY prop,
    int index,
    const char *defaultValue) {
  GtkWidget *gs; /* scrollable window */
  GtkWidget *g;  /* vbox */

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_CONTENT));
  assert(g);
  gs=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(gs);

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return defaultValue;
}



static GWENHYWFAR_CB
int Gtk3Gui_WScrollArea_AddChildGuiWidget(GWEN_WIDGET *w, GWEN_WIDGET *wChild) {
  GtkWidget *g;
  GtkWidget *gChild;
  uint32_t cflags;

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_CONTENT));
  assert(g);

  gChild=GTK_WIDGET(GWEN_Widget_GetImplData(wChild, GTK3_DIALOG_WIDGET_REAL));
  assert(gChild);

  cflags=GWEN_Widget_GetFlags(wChild);

  gtk_box_pack_start(GTK_BOX(g), gChild,
                     (cflags & GWEN_WIDGET_FLAGS_FILLY)?TRUE:FALSE,
                     (cflags & GWEN_WIDGET_FLAGS_FILLY)?TRUE:FALSE,
                     0);

  return 0;
}



static int Gtk3Gui_WScrollArea_Setup(GWEN_WIDGET *w) {
  GtkWidget *g;
  GtkWidget *gContent;
  GWEN_WIDGET *wParent;

  wParent=GWEN_Widget_Tree_GetParent(w);

  /* create widget */
  g=gtk_scrolled_window_new(NULL, NULL);
  gContent=gtk_box_new(GTK_ORIENTATION_VERTICAL,
                       GTK3_GUI_DIALOG_DEFAULT_BOX_SPACING);

  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_REAL, (void*) g);
  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_CONTENT, (void*) gContent);

  GWEN_Widget_SetSetIntPropertyFn(w, Gtk3Gui_WScrollArea_SetIntProperty);
  GWEN_Widget_SetGetIntPropertyFn(w, Gtk3Gui_WScrollArea_GetIntProperty);
  GWEN_Widget_SetSetCharPropertyFn(w, Gtk3Gui_WScrollArea_SetCharProperty);
  GWEN_Widget_SetGetCharPropertyFn(w, Gtk3Gui_WScrollArea_GetCharProperty);
  GWEN_Widget_SetAddChildGuiWidgetFn(w, Gtk3Gui_WScrollArea_AddChildGuiWidget);

  if (wParent)
    GWEN_Widget_AddChildGuiWidget(wParent, w);

  return 0;
}


