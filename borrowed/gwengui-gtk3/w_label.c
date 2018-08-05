/***************************************************************************
    begin       : Sun May 16 2010
    copyright   : (C) 2010 by Martin Preuss
    email       : martin@libchipcard.de

 ***************************************************************************
 *          Please see toplevel file COPYING for license details           *
 ***************************************************************************/





static GWENHYWFAR_CB
int Gtk3Gui_WLabel_SetIntProperty(GWEN_WIDGET *w,
                                  GWEN_DIALOG_PROPERTY prop,
                                  int index,
                                  int value,
                                  int doSignal) {
  GtkLabel *g;

  g=GTK_LABEL(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
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

  default:
    break;
  }

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return GWEN_ERROR_INVALID;
}




static GWENHYWFAR_CB
int Gtk3Gui_WLabel_GetIntProperty(GWEN_WIDGET *w,
                                  GWEN_DIALOG_PROPERTY prop,
                                  int index,
                                  int defaultValue) {
  GtkLabel *g;

  g=GTK_LABEL(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
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

  default:
    break;
  }

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return defaultValue;
}



static GWENHYWFAR_CB
int Gtk3Gui_WLabel_SetCharProperty(GWEN_WIDGET *w,
                                   GWEN_DIALOG_PROPERTY prop,
                                   int index,
                                   const char *value,
                                   int doSignal) {
  GtkLabel *g;
  GWEN_BUFFER *tbuf;

  g=GTK_LABEL(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(g);

  tbuf=GWEN_Buffer_new(0, 128, 0, 1);
  if (value && *value)
    Gtk3Gui_GetRawText(value, tbuf);

  switch(prop) {
  case GWEN_DialogProperty_Title:
    gtk_label_set_text(g, GWEN_Buffer_GetStart(tbuf));
    GWEN_Buffer_free(tbuf);
    return 0;
  default:
    break;
  }

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  GWEN_Buffer_free(tbuf);
  return GWEN_ERROR_INVALID;
}



static GWENHYWFAR_CB
const char* Gtk3Gui_WLabel_GetCharProperty(GWEN_WIDGET *w,
    GWEN_DIALOG_PROPERTY prop,
    int index,
    const char *defaultValue) {
  GtkLabel *g;

  g=GTK_LABEL(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(g);

  switch(prop) {
  case GWEN_DialogProperty_Title:
    return gtk_label_get_label(g);
  default:
    break;
  }

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return defaultValue;
}



static int Gtk3Gui_WLabel_Setup(GWEN_WIDGET *w) {
  GtkWidget *g;
  const char *s;
  uint32_t flags;
  GWEN_WIDGET *wParent;
  GWEN_BUFFER *tbuf;

  flags=GWEN_Widget_GetFlags(w);
  wParent=GWEN_Widget_Tree_GetParent(w);
  s=GWEN_Widget_GetText(w, 0);

  tbuf=GWEN_Buffer_new(0, 128, 0, 1);
  if (s && *s)
    Gtk3Gui_GetRawText(s, tbuf);

  g=gtk_label_new(GWEN_Buffer_GetStart(tbuf));
  GWEN_Buffer_free(tbuf);
  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_REAL, (void*) g);
  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_CONTENT, (void*) g);

  GWEN_Widget_SetSetIntPropertyFn(w, Gtk3Gui_WLabel_SetIntProperty);
  GWEN_Widget_SetGetIntPropertyFn(w, Gtk3Gui_WLabel_GetIntProperty);
  GWEN_Widget_SetSetCharPropertyFn(w, Gtk3Gui_WLabel_SetCharProperty);
  GWEN_Widget_SetGetCharPropertyFn(w, Gtk3Gui_WLabel_GetCharProperty);

  if (wParent)
    GWEN_Widget_AddChildGuiWidget(wParent, w);

  return 0;
}


