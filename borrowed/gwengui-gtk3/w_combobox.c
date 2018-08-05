/***************************************************************************
    begin       : Sun May 16 2010
    copyright   : (C) 2010 by Martin Preuss
    email       : martin@libchipcard.de

 ***************************************************************************
 *          Please see toplevel file COPYING for license details           *
 ***************************************************************************/


typedef struct W_COMBOBOX W_COMBOBOX;
struct W_COMBOBOX {
  GWEN_STRINGLIST *entries;
};


GWEN_INHERIT(GWEN_WIDGET, W_COMBOBOX)




static GWENHYWFAR_CB
int Gtk3Gui_WComboBox_SetIntProperty(GWEN_WIDGET *w,
                                     GWEN_DIALOG_PROPERTY prop,
                                     int index,
                                     int value,
                                     int doSignal) {
  GtkWidget *g;
  W_COMBOBOX *xw;

  assert(w);
  xw=GWEN_INHERIT_GETDATA(GWEN_WIDGET, W_COMBOBOX, w);
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

  case GWEN_DialogProperty_Value:
    gtk_combo_box_set_active(GTK_COMBO_BOX(g), value);
    return 0;

  case GWEN_DialogProperty_ClearValues: {
    GtkListStore *store;

    store=GTK_LIST_STORE(gtk_combo_box_get_model(GTK_COMBO_BOX(g)));
    assert(store);
    gtk_list_store_clear(store);
    GWEN_StringList_Clear(xw->entries);
    return 0;
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
int Gtk3Gui_WComboBox_GetIntProperty(GWEN_WIDGET *w,
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

  case GWEN_DialogProperty_Value: {
    gint i;

    i=gtk_combo_box_get_active(GTK_COMBO_BOX(g));
    if (i==-1)
      return defaultValue;
    else
      return i;
  }

  case GWEN_DialogProperty_ValueCount: {
    GtkListStore *store;
    gint i;

    store=GTK_LIST_STORE(gtk_combo_box_get_model(GTK_COMBO_BOX(g)));
    assert(store);
    i=gtk_tree_model_iter_n_children(GTK_TREE_MODEL(store), NULL);
    return i;
  }

  default:
    break;
  }

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function is not appropriate for this type of widget (%s)",
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return defaultValue;
}



static GWENHYWFAR_CB
int Gtk3Gui_WComboBox_SetCharProperty(GWEN_WIDGET *w,
                                      GWEN_DIALOG_PROPERTY prop,
                                      int index,
                                      const char *value,
                                      int doSignal) {
  GtkWidget *g;
  W_COMBOBOX *xw;

  assert(w);
  xw=GWEN_INHERIT_GETDATA(GWEN_WIDGET, W_COMBOBOX, w);
  assert(xw);

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(g);

  switch(prop) {
  case GWEN_DialogProperty_Value:
    /* undefined */
    break;

  case GWEN_DialogProperty_AddValue: {
    GtkListStore *store;
    GtkTreeIter iter;

    store=GTK_LIST_STORE(gtk_combo_box_get_model(GTK_COMBO_BOX(g)));
    assert(store);

    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter, 0, value, -1);
    GWEN_StringList_AppendString(xw->entries, value, 0, 0);
    return 0;
  }

  case GWEN_DialogProperty_ClearValues: {
    GtkListStore *store;

    store=GTK_LIST_STORE(gtk_combo_box_get_model(GTK_COMBO_BOX(g)));
    assert(store);
    gtk_list_store_clear(store);
    GWEN_StringList_Clear(xw->entries);
    return 0;
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
const char* Gtk3Gui_WComboBox_GetCharProperty(GWEN_WIDGET *w,
    GWEN_DIALOG_PROPERTY prop,
    int index,
    const char *defaultValue) {
  GtkWidget *g;
  W_COMBOBOX *xw;

  assert(w);
  xw=GWEN_INHERIT_GETDATA(GWEN_WIDGET, W_COMBOBOX, w);
  assert(xw);

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_REAL));
  assert(g);

  switch(prop) {
  case GWEN_DialogProperty_Value: {
    const char *s;

    s=GWEN_StringList_StringAt(xw->entries, index);
    if (s && *s)
      return s;
    else
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



static void changed_handler(GtkWidget *comboBox, gpointer data) {
  GWEN_WIDGET *w;
  int rv;

  w=data;
  assert(w);
  rv=GWEN_Dialog_EmitSignal(GWEN_Widget_GetDialog(w),
                            GWEN_DialogEvent_TypeActivated,
                            GWEN_Widget_GetName(w));
  if (rv==GWEN_DialogEvent_ResultAccept)
    Gtk3Gui_Dialog_Leave(GWEN_Widget_GetTopDialog(w), 1);
  else if (rv==GWEN_DialogEvent_ResultReject)
    Gtk3Gui_Dialog_Leave(GWEN_Widget_GetTopDialog(w), 0);
}



static void GWENHYWFAR_CB Gtk3Gui_WComboBox_FreeData(void *bp, void *p) {
  W_COMBOBOX *xw;

  xw=(W_COMBOBOX*) p;
  GWEN_StringList_free(xw->entries);
  GWEN_FREE_OBJECT(xw);
}


static
int Gtk3Gui_WComboBox_Setup(GWEN_WIDGET *w) {
  W_COMBOBOX *xw;
  GtkWidget *g;
  GtkCellRenderer *cr;
  GtkListStore *store;
  uint32_t flags;
  GWEN_WIDGET *wParent;
  gulong changed_handler_id;

  flags=GWEN_Widget_GetFlags(w);
  wParent=GWEN_Widget_Tree_GetParent(w);

  /* create widget */
  store=gtk_list_store_new(1, G_TYPE_STRING);
  if (flags & GWEN_WIDGET_FLAGS_READONLY)
    g=gtk_combo_box_new_with_model(GTK_TREE_MODEL(store));
  else
  {
    /* TODO: why the heck does *this* combo box have two columns in the list?? */
    g=gtk_combo_box_new_with_model_and_entry(GTK_TREE_MODEL(store));
    gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX(g), 0);
  }
  g_object_unref(store);

  cr=gtk_cell_renderer_text_new();
  gtk_cell_layout_pack_start(GTK_CELL_LAYOUT (g), cr, TRUE);
  gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(g), cr, "text", 0, NULL);

  GWEN_NEW_OBJECT(W_COMBOBOX, xw);
  GWEN_INHERIT_SETDATA(GWEN_WIDGET, W_COMBOBOX, w, xw, Gtk3Gui_WComboBox_FreeData);
  xw->entries=GWEN_StringList_new();

  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_REAL, (void*) g);
  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_CONTENT, (void*) g);

  GWEN_Widget_SetSetIntPropertyFn(w, Gtk3Gui_WComboBox_SetIntProperty);
  GWEN_Widget_SetGetIntPropertyFn(w, Gtk3Gui_WComboBox_GetIntProperty);
  GWEN_Widget_SetSetCharPropertyFn(w, Gtk3Gui_WComboBox_SetCharProperty);
  GWEN_Widget_SetGetCharPropertyFn(w, Gtk3Gui_WComboBox_GetCharProperty);

  changed_handler_id=g_signal_connect(g,
                                      "changed",
                                      G_CALLBACK (changed_handler),
                                      w);

  if (wParent)
    GWEN_Widget_AddChildGuiWidget(wParent, w);

  return 0;
}


