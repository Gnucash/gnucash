/***************************************************************************
    begin       : Sun May 16 2010
    copyright   : (C) 2010 by Martin Preuss
    email       : martin@libchipcard.de

 ***************************************************************************
 *          Please see toplevel file COPYING for license details           *
 ***************************************************************************/





static GWENHYWFAR_CB
int Gtk3Gui_WTextBrowser_SetIntProperty(GWEN_WIDGET *w,
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
int Gtk3Gui_WTextBrowser_GetIntProperty(GWEN_WIDGET *w,
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
int Gtk3Gui_WTextBrowser_SetCharProperty(GWEN_WIDGET *w,
    GWEN_DIALOG_PROPERTY prop,
    int index,
    const char *value,
    int doSignal) {
  GtkWidget *g;  /* text view */
  GtkWidget *gs; /* scrollable window */
  GWEN_BUFFER *tbuf;

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_CONTENT));
  assert(g);
  gs=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(gs);

  tbuf=GWEN_Buffer_new(0, 128, 0, 1);
  if (value && *value)
    Gtk3Gui_GetRawText(value, tbuf);

  switch(prop) {
  case GWEN_DialogProperty_Value: {
    GtkTextBuffer *tb;
    GtkAdjustment *va;

    tb=gtk_text_view_get_buffer(GTK_TEXT_VIEW(g));
    assert(tb);
    gtk_text_buffer_set_text(tb, GWEN_Buffer_GetStart(tbuf), -1);

    /* scroll to end */
    va=gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(gs));
    if (va)
      gtk_adjustment_set_value(va, gtk_adjustment_get_upper(va));
    GWEN_Buffer_free(tbuf);

    return 0;
  }
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
const char* Gtk3Gui_WTextBrowser_GetCharProperty(GWEN_WIDGET *w,
    GWEN_DIALOG_PROPERTY prop,
    int index,
    const char *defaultValue) {
  GtkWidget *g;  /* text view */
  GtkWidget *gs; /* scrollable window */

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_CONTENT));
  assert(g);
  gs=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(gs);

  switch(prop) {
  case GWEN_DialogProperty_Value: {
    GtkTextBuffer *tb;
    GtkTextIter startIter;
    GtkTextIter endIter;
    gchar *s;

    tb=gtk_text_view_get_buffer(GTK_TEXT_VIEW(g));
    assert(tb);

    gtk_text_buffer_get_start_iter(tb, &startIter);
    gtk_text_buffer_get_end_iter(tb, &endIter);

    s=gtk_text_buffer_get_text(tb, &startIter, &endIter, FALSE);
    if (s) {
      GWEN_Widget_SetText(w, GTK3_DIALOG_STRING_VALUE, s);
      g_free(s);
      return GWEN_Widget_GetText(w, GTK3_DIALOG_STRING_VALUE);
    }
    return defaultValue;
  }

  default:
    break;
  }

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return defaultValue;
}



static int Gtk3Gui_WTextBrowser_Setup(GWEN_WIDGET *w) {
  GtkWidget *gs;
  GtkWidget *g;
  const char *s;
  uint32_t flags;
  GWEN_WIDGET *wParent;

  flags=GWEN_Widget_GetFlags(w);
  wParent=GWEN_Widget_Tree_GetParent(w);
  s=GWEN_Widget_GetText(w, 0);

  /* create widget */
  gs=gtk_scrolled_window_new(NULL, NULL);
  g=gtk_text_view_new();

  if (s && *s) {
    GWEN_BUFFER *tbuf;

    tbuf=GWEN_Buffer_new(0, 128, 0, 1);
    Gtk3Gui_GetRawText(s, tbuf);
    gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(g)), GWEN_Buffer_GetStart(tbuf), -1);
    GWEN_Buffer_free(tbuf);
  }

  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_REAL, (void*) gs);
  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_CONTENT, (void*) g);

  GWEN_Widget_SetSetIntPropertyFn(w, Gtk3Gui_WTextBrowser_SetIntProperty);
  GWEN_Widget_SetGetIntPropertyFn(w, Gtk3Gui_WTextBrowser_GetIntProperty);
  GWEN_Widget_SetSetCharPropertyFn(w, Gtk3Gui_WTextBrowser_SetCharProperty);
  GWEN_Widget_SetGetCharPropertyFn(w, Gtk3Gui_WTextBrowser_GetCharProperty);

  if (wParent)
    GWEN_Widget_AddChildGuiWidget(wParent, w);

  return 0;
}


