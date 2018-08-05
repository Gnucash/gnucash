/***************************************************************************
    begin       : Sun May 16 2010
    copyright   : (C) 2010 by Martin Preuss
    email       : martin@libchipcard.de

 ***************************************************************************
 *          Please see toplevel file COPYING for license details           *
 ***************************************************************************/



typedef struct W_PROGRESSBAR W_PROGRESSBAR;
struct W_PROGRESSBAR {
  int minValue;
  int maxValue;
  int currentValue;
};


GWEN_INHERIT(GWEN_WIDGET, W_PROGRESSBAR)



static GWENHYWFAR_CB
int Gtk3Gui_WProgressBar_SetIntProperty(GWEN_WIDGET *w,
                                        GWEN_DIALOG_PROPERTY prop,
                                        int index,
                                        int value,
                                        int doSignal) {
  GtkProgressBar *g;
  W_PROGRESSBAR *xw;

  assert(w);
  xw=GWEN_INHERIT_GETDATA(GWEN_WIDGET, W_PROGRESSBAR, w);
  assert(xw);

  g=GTK_PROGRESS_BAR(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
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

  case GWEN_DialogProperty_Value: {

    xw->currentValue=value;
    if (xw->maxValue) {
      gdouble d;
      char numbuf[32];

      d=(gdouble)(xw->currentValue-xw->minValue)/(gdouble)(xw->maxValue);
      gtk_progress_bar_set_fraction(g, d);
      snprintf(numbuf, sizeof(numbuf)-1, "%d %%", (int)(d*100.0));
      numbuf[sizeof(numbuf)-1]=0;
      gtk_progress_bar_set_text(g, numbuf);
    }
    else {
      gtk_progress_bar_set_fraction(g, 0.0);
      gtk_progress_bar_set_text(g, "");
    }
    return 0;
  }

  case GWEN_DialogProperty_MinValue: {
    xw->minValue=value;
    if (xw->maxValue) {
      gdouble d;
      char numbuf[32];

      d=(gdouble)(xw->currentValue-xw->minValue)/(gdouble)(xw->maxValue);
      gtk_progress_bar_set_fraction(g, d);
      snprintf(numbuf, sizeof(numbuf)-1, "%d %%", (int)(d*100.0));
      numbuf[sizeof(numbuf)-1]=0;
      gtk_progress_bar_set_text(g, numbuf);
    }
    else {
      gtk_progress_bar_set_fraction(g, 0.0);
      gtk_progress_bar_set_text(g, "");
    }
    return 0;
  }

  case GWEN_DialogProperty_MaxValue: {
    xw->maxValue=value;
    if (xw->maxValue) {
      gdouble d;
      char numbuf[32];

      d=(gdouble)(xw->currentValue-xw->minValue)/(gdouble)(xw->maxValue);
      gtk_progress_bar_set_fraction(g, d);
      snprintf(numbuf, sizeof(numbuf)-1, "%d %%", (int)(d*100.0));
      numbuf[sizeof(numbuf)-1]=0;
      gtk_progress_bar_set_text(g, numbuf);
      return 0;
    }
    else {
      gtk_progress_bar_set_fraction(g, 0.0);
      gtk_progress_bar_set_text(g, "");
    }
  }

  default:
    break;
  }

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return GWEN_ERROR_INVALID;
}




static GWENHYWFAR_CB
int Gtk3Gui_WProgressBar_GetIntProperty(GWEN_WIDGET *w,
                                        GWEN_DIALOG_PROPERTY prop,
                                        int index,
                                        int defaultValue) {
  GtkProgressBar *g;
  W_PROGRESSBAR *xw;

  assert(w);
  xw=GWEN_INHERIT_GETDATA(GWEN_WIDGET, W_PROGRESSBAR, w);
  assert(xw);

  g=GTK_PROGRESS_BAR(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
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
    return xw->currentValue;

  case GWEN_DialogProperty_MinValue:
    return xw->minValue;

  case GWEN_DialogProperty_MaxValue:
    return xw->maxValue;

  default:
    break;
  }

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return defaultValue;
}



static GWENHYWFAR_CB
int Gtk3Gui_WProgressBar_SetCharProperty(GWEN_WIDGET *w,
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
const char* Gtk3Gui_WProgressBar_GetCharProperty(GWEN_WIDGET *w,
    GWEN_DIALOG_PROPERTY prop,
    int index,
    const char *defaultValue) {
  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return defaultValue;
}



static void GWENHYWFAR_CB Gtk3Gui_WProgressBar_FreeData(void *bp, void *p) {
  W_PROGRESSBAR *xw;

  xw=(W_PROGRESSBAR*) p;
  GWEN_FREE_OBJECT(xw);
}



static int Gtk3Gui_WProgressBar_Setup(GWEN_WIDGET *w) {
  GtkWidget *g;
  const char *s;
  uint32_t flags;
  GWEN_WIDGET *wParent;
  W_PROGRESSBAR *xw;

  GWEN_NEW_OBJECT(W_PROGRESSBAR, xw);
  GWEN_INHERIT_SETDATA(GWEN_WIDGET, W_PROGRESSBAR, w, xw, Gtk3Gui_WProgressBar_FreeData);

  flags=GWEN_Widget_GetFlags(w);
  wParent=GWEN_Widget_Tree_GetParent(w);
  s=GWEN_Widget_GetText(w, 0);

  g=gtk_progress_bar_new();
  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_REAL, (void*) g);
  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_CONTENT, (void*) g);

  GWEN_Widget_SetSetIntPropertyFn(w, Gtk3Gui_WProgressBar_SetIntProperty);
  GWEN_Widget_SetGetIntPropertyFn(w, Gtk3Gui_WProgressBar_GetIntProperty);
  GWEN_Widget_SetSetCharPropertyFn(w, Gtk3Gui_WProgressBar_SetCharProperty);
  GWEN_Widget_SetGetCharPropertyFn(w, Gtk3Gui_WProgressBar_GetCharProperty);

  if (wParent)
    GWEN_Widget_AddChildGuiWidget(wParent, w);

  return 0;
}


