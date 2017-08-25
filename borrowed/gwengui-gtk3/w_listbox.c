/***************************************************************************
    begin       : Fri Jul 09 2010
    copyright   : (C) 2010 by Martin Preuss
    email       : martin@libchipcard.de

 ***************************************************************************
 *          Please see toplevel file COPYING for license details           *
 ***************************************************************************/


#define W_LISTBOX_MAX_TYPES 256



static GWENHYWFAR_CB
int Gtk3Gui_WListBox_SetIntProperty(GWEN_WIDGET *w,
                                    GWEN_DIALOG_PROPERTY prop,
                                    int index,
                                    int value,
                                    int doSignal) {
  GtkWidget *g;

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_CONTENT));
  assert(g);

  switch(prop) {
  case GWEN_DialogProperty_Enabled:
    gtk_widget_set_sensitive(GTK_WIDGET(g), (value==0)?FALSE:TRUE);
    return 0;

  case GWEN_DialogProperty_Focus:
    gtk_widget_grab_focus(GTK_WIDGET(g));
    return 0;

  case GWEN_DialogProperty_Value: {
    GtkTreePath *path;

    path=gtk_tree_path_new_from_indices(value, -1);
    gtk_tree_view_set_cursor(GTK_TREE_VIEW(g), path, NULL, FALSE);
    gtk_tree_path_free(path);
    return 0;
  }

  case GWEN_DialogProperty_SelectionMode: {
    GtkTreeSelection *sel;

    sel=gtk_tree_view_get_selection(GTK_TREE_VIEW(g));
    if (sel) {
      switch(value) {
      case GWEN_Dialog_SelectionMode_None:
        gtk_tree_selection_set_mode(sel, GTK_SELECTION_NONE);
        return 0;
      case GWEN_Dialog_SelectionMode_Single:
        gtk_tree_selection_set_mode(sel, GTK_SELECTION_SINGLE);
        return 0;
      case GWEN_Dialog_SelectionMode_Multi:
        gtk_tree_selection_set_mode(sel, GTK_SELECTION_MULTIPLE);
        return 0;
      }
      DBG_ERROR(GWEN_LOGDOMAIN, "Unknown SelectionMode %d", value);
      return GWEN_ERROR_INVALID;
    }
    break;
  }

  case GWEN_DialogProperty_ColumnWidth: {
    GtkTreeViewColumn *col;

    col=gtk_tree_view_get_column(GTK_TREE_VIEW(g), index);
    if (col) {
      gtk_tree_view_column_set_fixed_width(col, value);
      return 0;
    }

    /* no width */
    return GWEN_ERROR_INVALID;
  }

  case GWEN_DialogProperty_SortDirection: {
    GtkTreeViewColumn *col;
    int i;
    int cols;

    /* remove sort indicator from all columns */
    cols=GWEN_Widget_GetColumns(w);
    for (i=0; i<cols; i++) {
      col=gtk_tree_view_get_column(GTK_TREE_VIEW(g), index);
      if (col) {
        if (gtk_tree_view_column_get_sort_indicator(col)==TRUE)
          gtk_tree_view_column_set_sort_indicator(col, FALSE);
      }
    }

    if (value!=GWEN_DialogSortDirection_None) {
      /* set sort indicator on given column */
      col=gtk_tree_view_get_column(GTK_TREE_VIEW(g), index);
      if (col) {
        switch(value) {
        case GWEN_DialogSortDirection_Up:
          gtk_tree_view_column_set_sort_order(col, GTK_SORT_ASCENDING);
          break;
        case GWEN_DialogSortDirection_Down:
          gtk_tree_view_column_set_sort_order(col, GTK_SORT_DESCENDING);
          break;
        default:
          break;
        }
      }
    }

    return 0;
  }

  case GWEN_DialogProperty_ClearValues: {
    GtkListStore *sto;

    sto=GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(g)));
    if (sto)
      gtk_list_store_clear(sto);
    return 0;
  }

  case GWEN_DialogProperty_Sort:
    /* NOOP, we use auto-sorting for now (TODO: figure out how to manually sort) */
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
int Gtk3Gui_WListBox_GetIntProperty(GWEN_WIDGET *w,
                                    GWEN_DIALOG_PROPERTY prop,
                                    int index,
                                    int defaultValue) {
  GtkWidget *g;

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_CONTENT));
  assert(g);

  switch(prop) {
  case GWEN_DialogProperty_Enabled:
    return (gtk_widget_get_sensitive(GTK_WIDGET(g))==TRUE)?1:0;

  case GWEN_DialogProperty_Focus:
    return (gtk_widget_has_focus(GTK_WIDGET(g))==TRUE)?1:0;
    return 0;

  case GWEN_DialogProperty_Value: {
    GtkTreePath *path=NULL;

    gtk_tree_view_get_cursor(GTK_TREE_VIEW(g), &path, NULL);
    if (path!=NULL) {
      gint *idxlist;

      idxlist=gtk_tree_path_get_indices(path);
      if (idxlist!=NULL) {
        int res;

        res=idxlist[0];
        gtk_tree_path_free(path);
        return res;
      }
      gtk_tree_path_free(path);
    }

    /* no cursor */
    return -1;
  }

  case GWEN_DialogProperty_ColumnWidth: {
    GtkTreeViewColumn *col;

    col=gtk_tree_view_get_column(GTK_TREE_VIEW(g), index);
    if (col)
      return gtk_tree_view_column_get_width(col);

    /* no width */
    return -1;
  }

  case GWEN_DialogProperty_SortDirection: {
    GtkTreeViewColumn *col;

    col=gtk_tree_view_get_column(GTK_TREE_VIEW(g), index);
    if (col) {
      if (gtk_tree_view_column_get_sort_indicator(col)==TRUE) {
        switch(gtk_tree_view_column_get_sort_order(col)) {
        case GTK_SORT_ASCENDING:
          return GWEN_DialogSortDirection_Up;
        case GTK_SORT_DESCENDING:
          return GWEN_DialogSortDirection_Down;
        default:
          break;
        }
      }
      /*break; <- this is wrong here, isn't it? */
    }

    return GWEN_DialogSortDirection_None;
  }

  default:
    break;
  }

  DBG_WARN(GWEN_LOGDOMAIN,
           "Function %d is not appropriate for this type of widget (%s)",
           prop,
           GWEN_Widget_Type_toString(GWEN_Widget_GetType(w)));
  return defaultValue;
}



static GWENHYWFAR_CB
int Gtk3Gui_WListBox_SetCharProperty(GWEN_WIDGET *w,
                                     GWEN_DIALOG_PROPERTY prop,
                                     int index,
                                     const char *value,
                                     int doSignal) {
  GtkWidget *g;

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_CONTENT));
  assert(g);

  switch(prop) {
  case GWEN_DialogProperty_Title: {
    int cols=0;
    if (value && *value) {
      int i;
      int l;

      l=strlen(value);
      cols=1;
      for (i=0; i<l; i++) {
        if (value[i]=='\t')
          cols++;
      }
    }

    if (cols) {
      GType types[W_LISTBOX_MAX_TYPES];
      GtkListStore *sto;
      int i;
      const char *s;
      GtkTreeViewColumn *col;
      char *vcopy;
      char *p;

      if (cols>W_LISTBOX_MAX_TYPES)
        cols=W_LISTBOX_MAX_TYPES;
      for (i=0; i<cols; i++)
        types[i]=G_TYPE_STRING;
      sto=gtk_list_store_newv(cols, types);
      s=value;

      /* clear current headers in tree view */
      while( (col=gtk_tree_view_get_column(GTK_TREE_VIEW(g), 0)) )
        gtk_tree_view_remove_column(GTK_TREE_VIEW(g), col);

      /* set new model */
      gtk_tree_view_set_model(GTK_TREE_VIEW(g), GTK_TREE_MODEL(sto));

      /* insert new headers */
      i=0;
      vcopy=strdup(value);
      p=vcopy;
      while(*p && i<cols) {
        char *pT;
        GtkCellRenderer *renderer;

        pT=strchr(p, '\t');
        if (pT)
          *pT=0;

        renderer=gtk_cell_renderer_text_new();
        col=gtk_tree_view_column_new();
        gtk_tree_view_column_set_title(col, p);
        gtk_tree_view_column_pack_start(col, renderer, TRUE);
        gtk_tree_view_column_set_sort_column_id(col, i);
        gtk_tree_view_column_set_resizable(col, TRUE);
        gtk_tree_view_column_set_sizing(col, GTK_TREE_VIEW_COLUMN_FIXED);
        gtk_tree_view_column_set_attributes(col, renderer, "text", i, NULL);

        gtk_tree_view_append_column(GTK_TREE_VIEW(g), col);

        if (pT)
          p=pT+1;
        else
          /* no more tab, all columns done */
          break;
        i++;
      }
      free(vcopy);
      GWEN_Widget_SetColumns(w, cols);
      gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(g), TRUE);
    }
    else {
      DBG_ERROR(GWEN_LOGDOMAIN, "No columns (empty title)");
      return GWEN_ERROR_INVALID;
    }

    return 0;
  }

  case GWEN_DialogProperty_ClearValues: {
    GtkListStore *sto;

    sto=GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(g)));
    if (sto)
      gtk_list_store_clear(sto);
    return 0;
  }

  case GWEN_DialogProperty_AddValue: {
    GtkListStore *sto;

    sto=GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(g)));
    if (sto) {
      GtkTreeIter iter;
      int cols;
      int i;
      char *vcopy;
      char *p;

      cols=GWEN_Widget_GetColumns(w);

      vcopy=strdup(value);
      p=vcopy;
      i=0;
      gtk_list_store_append(sto, &iter);
      while(*p && i<cols) {
        char *pT;
        GValue val= {0};

        g_value_init(&val, G_TYPE_STRING);

        pT=strchr(p, '\t');
        if (pT)
          *pT=0;
        g_value_set_string(&val, p);
        gtk_list_store_set_value(sto, &iter, i, &val);
        g_value_unset(&val);

        if (pT)
          p=pT+1;
        else
          /* no more tabs, all columns done */
          break;
        i++;
      }
      free(vcopy);
    }

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
const char* Gtk3Gui_WListBox_GetCharProperty(GWEN_WIDGET *w,
    GWEN_DIALOG_PROPERTY prop,
    int index,
    const char *defaultValue) {
  GtkWidget *g;

  g=GTK_WIDGET(GWEN_Widget_GetImplData(w, GTK3_DIALOG_WIDGET_CONTENT));
  assert(g);

  switch(prop) {
  case GWEN_DialogProperty_Title: {
    GList *cols;

    cols=gtk_tree_view_get_columns(GTK_TREE_VIEW(g));
    if (cols) {
      GList *le;
      GWEN_BUFFER *tbuf;
      int cnt=0;

      tbuf=GWEN_Buffer_new(0, 256, 0, 1);
      le=g_list_first(cols);
      while(le) {
        const gchar *s;

        if (cnt)
          GWEN_Buffer_AppendByte(tbuf, '\t');
        s=gtk_tree_view_column_get_title(GTK_TREE_VIEW_COLUMN(le->data));
        if (s && *s)
          GWEN_Buffer_AppendString(tbuf, s);
        cnt++;
        le=g_list_next(le);
      } /* while */
      GWEN_Widget_SetText(w, GTK3_DIALOG_STRING_TITLE, GWEN_Buffer_GetStart(tbuf));
      GWEN_Buffer_free(tbuf);

      g_list_free(cols);
      return GWEN_Widget_GetText(w, GTK3_DIALOG_STRING_TITLE);
    }
    return defaultValue;
  }

  case GWEN_DialogProperty_Value: {
    GtkTreePath *path;

    path=gtk_tree_path_new_from_indices(index, -1);
    if (path!=NULL) {
      GtkListStore *sto;
      GtkTreeIter iter;

      sto=GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(g)));
      if (gtk_tree_model_get_iter(GTK_TREE_MODEL(sto), &iter, path)) {
        GList *cols;

        cols=gtk_tree_view_get_columns(GTK_TREE_VIEW(g));
        if (cols) {
          GList *le;
          GWEN_BUFFER *tbuf;
          int cnt=0;

          tbuf=GWEN_Buffer_new(0, 256, 0, 1);
          le=g_list_first(cols);
          while(le) {
            gchar *s;

            if (cnt)
              GWEN_Buffer_AppendByte(tbuf, '\t');
            gtk_tree_model_get(GTK_TREE_MODEL(sto), &iter, cnt, &s, -1, NULL);
            if (s) {
              GWEN_Buffer_AppendString(tbuf, s);
              g_free(s);
            }
            cnt++;
            le=g_list_next(le);
          } /* while */
          GWEN_Widget_SetText(w, GTK3_DIALOG_STRING_VALUE, GWEN_Buffer_GetStart(tbuf));
          GWEN_Buffer_free(tbuf);

          g_list_free(cols);
          return GWEN_Widget_GetText(w, GTK3_DIALOG_STRING_VALUE);
        }
      }

      gtk_tree_path_free(path);
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



static void Gtk3Gui_WListBox_CursorChanged_handler(GtkTreeView *g, gpointer data) {
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



static int Gtk3Gui_WListBox_Setup(GWEN_WIDGET *w) {
  GtkWidget *g;
  GtkWidget *gScroll;
  uint32_t flags;
  GWEN_WIDGET *wParent;
  gulong changed_handler_id;

  flags=GWEN_Widget_GetFlags(w);
  wParent=GWEN_Widget_Tree_GetParent(w);

  gScroll=gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(gScroll),
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);
  g=gtk_tree_view_new();
  gtk_tree_view_set_headers_clickable(GTK_TREE_VIEW(g), TRUE);
/* gtk_tree_view_set_rules_hint is deprecated in gtk-3.14 on the
 * grounds that it's really up to the theme and the user whether the
 * treeview should be drawn with alternating background colors. */
G_GNUC_BEGIN_IGNORE_DEPRECATIONS
  gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(g), TRUE);
G_GNUC_END_IGNORE_DEPRECATIONS
  gtk_container_add(GTK_CONTAINER(gScroll), GTK_WIDGET(g));

  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_REAL, (void*) gScroll);
  GWEN_Widget_SetImplData(w, GTK3_DIALOG_WIDGET_CONTENT, (void*) g);

  GWEN_Widget_SetSetIntPropertyFn(w, Gtk3Gui_WListBox_SetIntProperty);
  GWEN_Widget_SetGetIntPropertyFn(w, Gtk3Gui_WListBox_GetIntProperty);
  GWEN_Widget_SetSetCharPropertyFn(w, Gtk3Gui_WListBox_SetCharProperty);
  GWEN_Widget_SetGetCharPropertyFn(w, Gtk3Gui_WListBox_GetCharProperty);

  changed_handler_id=g_signal_connect(g,
                                      "cursor-changed",
                                      G_CALLBACK (Gtk3Gui_WListBox_CursorChanged_handler),
                                      w);

  if (wParent)
    GWEN_Widget_AddChildGuiWidget(wParent, w);

  return 0;
}


