/***************************************************************************
    begin       : Sun May 16 2010
    copyright   : (C) 2010 by Martin Preuss
    email       : martin@libchipcard.de

 ***************************************************************************
 *          Please see toplevel file COPYING for license details           *
 ***************************************************************************/



typedef struct {
  int sortByRow;
  int allocatedColumns;
  int allocatedRows;

  int currentColumn;
  int currentRow;
}  GTK3_GRIDLAYOUT_WIDGET;


GWEN_INHERIT(GWEN_WIDGET, GTK3_GRIDLAYOUT_WIDGET)



static GWENHYWFAR_CB
int Gtk3Gui_WGridLayout_SetIntProperty(GWEN_WIDGET *w,
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
int Gtk3Gui_WGridLayout_GetIntProperty(GWEN_WIDGET *w,
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



static GWENHYWFAR_CB
int Gtk3Gui_WGridLayout_SetCharProperty(GWEN_WIDGET *w,
                                        GWEN_DIALOG_PROPERTY prop,
                                        int index,
                                        const char *value,
                                        int doSignal) {
  GtkWidget *g;

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(g);

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return GWEN_ERROR_INVALID;
}



static GWENHYWFAR_CB
const char* Gtk3Gui_WGridLayout_GetCharProperty(GWEN_WIDGET *w,
    GWEN_DIALOG_PROPERTY prop,
    int index,
    const char *defaultValue) {
  GtkWidget *g;

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(g);

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return defaultValue;
}



static GWENHYWFAR_CB
int Gtk3Gui_WGridLayout_AddChildGuiWidget(GWEN_WIDGET *w, GWEN_WIDGET *wChild) {
  GTK3_GRIDLAYOUT_WIDGET *xw;
  GtkWidget *g;
  GtkWidget *gChild;
  int x;
  int y;

  assert(w);
  xw=GWEN_INHERIT_GETDATA(GWEN_WIDGET, GTK3_GRIDLAYOUT_WIDGET, w);
  assert(xw);


  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(g);

  gChild=GTK_WIDGET(GWEN_Widget_GetImplData(wChild, GTK3_DIALOG_WIDGET_REAL));
  assert(gChild);

  if (xw->sortByRow) {
    /* fill rows, enter next column if column full */
    y=(xw->currentRow)++;
    if (y>=xw->allocatedRows) {
      xw->currentRow=0;
      y=(xw->currentRow)++;
      xw->currentColumn++;
    }

    x=xw->currentColumn;
    if (x>=xw->allocatedColumns) {
      xw->allocatedColumns=x+1;
    }
  }
  else {
    /* fill columns, enter next row if row full */
    x=(xw->currentColumn)++;
    if (x>=xw->allocatedColumns) {
      xw->currentColumn=0;
      x=(xw->currentColumn)++;
      xw->currentRow++;
    }

    y=xw->currentRow;
    if (y>=xw->allocatedRows) {
      xw->allocatedRows=y+1;
    }
  }

  gtk_grid_attach(GTK_GRID(g), gChild, x, y, 1, 1);

  return 0;
}




static GWENHYWFAR_CB
void Gtk3Gui_WGridLayout_FreeData(void *bp, void *p) {
  GTK3_GRIDLAYOUT_WIDGET *xw;

  xw=(GTK3_GRIDLAYOUT_WIDGET*) p;

  GWEN_FREE_OBJECT(xw);
}



static int Gtk3Gui_WGridLayout_Setup(GWEN_WIDGET *w) {
  GtkWidget *g;
  GWEN_WIDGET *wParent;
  GTK3_GRIDLAYOUT_WIDGET *xw;
  int rows;
  int cols;

  GWEN_NEW_OBJECT(GTK3_GRIDLAYOUT_WIDGET, xw);
  GWEN_INHERIT_SETDATA(GWEN_WIDGET, GTK3_GRIDLAYOUT_WIDGET, w, xw, Gtk3Gui_WGridLayout_FreeData);

  wParent=GWEN_Widget_Tree_GetParent(w);
  cols=GWEN_Widget_GetColumns(w);
  rows=GWEN_Widget_GetRows(w);

  if (rows>0) {
    xw->sortByRow=1;
    xw->allocatedRows=rows;
    xw->allocatedColumns=1;
  }
  else {
    xw->sortByRow=0;
    xw->allocatedColumns=cols;
    xw->allocatedRows=1;
  }

  g=gtk_grid_new();

  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_REAL, (void*) g);
  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_CONTENT, (void*) g);

  GWEN_Widget_SetSetIntPropertyFn(w, Gtk3Gui_WGridLayout_SetIntProperty);
  GWEN_Widget_SetGetIntPropertyFn(w, Gtk3Gui_WGridLayout_GetIntProperty);
  GWEN_Widget_SetSetCharPropertyFn(w, Gtk3Gui_WGridLayout_SetCharProperty);
  GWEN_Widget_SetGetCharPropertyFn(w, Gtk3Gui_WGridLayout_GetCharProperty);
  GWEN_Widget_SetAddChildGuiWidgetFn(w, Gtk3Gui_WGridLayout_AddChildGuiWidget);

  if (wParent)
    GWEN_Widget_AddChildGuiWidget(wParent, w);

  return 0;
}


