
#include <guile/gh.h>
#include <gnome.h>

#include "dialog-options.h"
#include "util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

/* Note that in general, passing SCM values to the GTK/GNOME callbacks
   as the "data" value requires you to make sure that that pointer is
   also known in some other way to Guile.  Otherwise the GC won't know
   about it, and may accidentally garbage collect it while you're
   still using it.  Here we should be safe, though, because all of the
   SCM values are also stored on the Guile side as part of the
   configuration-item which is stored in the main configuration-item
   list.  If you notice any SCM values that you think aren't
   protected, let me know.  */

/*
  TODO:

  Need options_dirty to keep from calling all the setters on a close
  even if nothing has changed.

  Right now the semantics are that changes take effect for most values
  immediately, and when you click apply for strings.  This may not be
  what we want in the long run...

  Add callback for OK button.

  Add verify functions for strings

  Add string handlers...

  Change this around so that each UI button, etc only calls:

    (gnc:options-dialog-ok)
    (gnc:options-dialog-apply)
    (gnc:options-dialog-close)

    (gnc:options-dialog-item-get-ui-value)
    (gnc:options-dialog-item-refresh-ui-value)
    
*/


static GnomePropertyBox *options_dialog = NULL;

static gint
show_option_documentation(GtkWidget *widget,
                          GdkEventButton *event,
                          gpointer data) {
  gnome_ok_dialog((char *) data);
  return 0;
}

static gint
call_boolean_ui_apply_func(GtkWidget *widget, gpointer data) {

  SCM setter_func = (SCM) data;
  if(GTK_TOGGLE_BUTTON(widget)->active) {
    gh_call1(setter_func, SCM_BOOL_T);
  } else {
    gh_call1(setter_func, SCM_BOOL_F);
  } 
  return 0;
}

static void
extract_configuration_item_parts(SCM item,
                                 char **item_type,
                                 char **item_name,
                                 char **documentation,
                                 SCM *value_getter,
                                 SCM *value_setter,
                                 SCM *default_value_getter,
                                 GtkWidget **w) {
  /* Cache these */
  static SCM type_getter;
  static SCM name_getter;
  static SCM documentation_getter;
  static SCM get_value_getter;
  static SCM get_value_setter;
  static SCM get_default_value_getter;
  static SCM widget_getter;
  static gboolean getters_inited = FALSE;
  
  if(!getters_inited) {
    type_getter = gh_eval_str("gnc:configuration-option-type");
    name_getter = gh_eval_str("gnc:configuration-option-name");
    documentation_getter = gh_eval_str("gnc:configuration-option-documentation");
    get_value_getter = gh_eval_str("gnc:configuration-option-getter");
    get_value_setter = gh_eval_str("gnc:configuration-option-setter");
    get_default_value_getter =
      gh_eval_str("gnc:configuration-option-default-getter");
    widget_getter =
      gh_eval_str("gnc:configuration-option-widget-get");
    
    getters_inited = TRUE;
  }

  if(item_type) {
    *item_type = gh_symbol2newstr(gh_call1(type_getter, item), NULL);
  }
  if(item_name) {
    *item_name = gh_scm2newstr(gh_call1(name_getter, item), NULL);
  }
  if(documentation) {
    *documentation = gh_scm2newstr(gh_call1(documentation_getter, item), NULL);
  }
  if(value_getter) {
    *value_getter = gh_call1(get_value_getter, item);
  }
  if(value_setter) {
    *value_setter = gh_call1(get_value_setter, item);
  }
  if(default_value_getter) {
    *default_value_getter = gh_call1(get_default_value_getter, item);
  }
  if(w) {
    *w = (GtkWidget *) gh_scm2ulong(gh_call1(widget_getter, item));
  }
}

static gint
call_reset_to_default_func(GtkWidget *widget,
                           GdkEventButton *event,
                           gpointer data) {
  SCM item = (SCM) data;
  SCM value_setter;
  SCM default_value_getter;
  
  extract_configuration_item_parts(item,
                                   NULL,
                                   NULL,
                                   NULL,
                                   NULL,
                                   &value_setter,
                                   &default_value_getter,
                                   NULL);

  {
    SCM default_value = gh_call0(default_value_getter);
    gh_call1(value_setter, default_value);
  }
  return 0;
}

void
_gnc_options_dialog_item_refresh_ui_(SCM item) {

  if(options_dialog) {
    
    char *item_type;
    SCM value_getter;
    GtkWidget *w;
    
    extract_configuration_item_parts(item,
                                     &item_type,
                                     NULL,
                                     NULL,
                                     &value_getter,
                                     NULL,
                                     NULL,
                                     &w);
    
    if(strcmp(item_type, "boolean")) {
      gboolean current_state = gh_scm2bool(gh_call0(value_getter));
      gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(w), current_state);    
    } else {
      PERR ("_gnc_options_dialog_item_refresh_ui_(): "
            "Unknown type for refresh.  Ignoring.\n");
    }
    free(item_type);
  }
}

SCM
_gnc_options_dialog_item_get_ui_value_(SCM item) {
  SCM result = 0;
  if(options_dialog) {    
    char *item_type;
    SCM value_getter;
    GtkWidget *w;
    
    extract_configuration_item_parts(item,
                                     &item_type,
                                     NULL,
                                     NULL,
                                     &value_getter,
                                     NULL,
                                     NULL,
                                     &w);
    
    if(strcmp(item_type, "boolean")) {
      result = gh_bool2scm(GTK_TOGGLE_BUTTON(w)->active);
    } else {
      PERR ("_gnc_options_dialog_item_get_ui_value_(): "
            "Unknown type for refresh.  Ignoring.\n");
      result = SCM_UNDEFINED;
    }
    free(item_type);
  }
  return(result);
}

void
_gnc_options_dialog_add_item_(GtkBox *page_w, SCM item) {

  char *item_type;
  char *item_name;
  char *item_documentation;
  SCM item_value_getter;
  SCM item_value_setter;
  SCM item_default_value_getter;
  gboolean known_type = FALSE;
  
  extract_configuration_item_parts(item,
                                   &item_type,
                                   &item_name,
                                   &item_documentation,
                                   &item_value_getter,
                                   &item_value_setter,
                                   &item_default_value_getter,
                                   NULL);
  {
    /* Horizontal box to hold all the UI items. */
    GtkWidget *hbox = gtk_hbox_new(FALSE, 1);
    GtkWidget *value_widget = NULL;
    
    gtk_box_pack_start(GTK_BOX(page_w), GTK_WIDGET(hbox), FALSE, FALSE, 3);
    
    if(strcmp(item_type, "boolean") == 0) {
      gboolean current_state;

      value_widget = gtk_check_button_new_with_label(item_name);
      current_state = gh_scm2bool(gh_call0(item_value_getter));
      
      gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(value_widget),
                                  current_state);
      gtk_signal_connect (GTK_OBJECT (value_widget), 
                          "toggled",
                          (GtkSignalFunc) call_boolean_ui_apply_func,
                          (gpointer) item_value_setter);
      known_type = TRUE;
    } else {
      gchar *label_name = g_strdup_printf("%s: <unknown-type>", item_name);
      value_widget = gtk_label_new(label_name);
    }
    
    gtk_widget_show(value_widget);
    gtk_box_pack_start(GTK_BOX(hbox), value_widget, FALSE, FALSE, 3);

    {
      static SCM item_widget_setter;
      static gboolean setter_inited = FALSE;
      if(!setter_inited) {
        item_widget_setter = gh_eval_str("gnc:configuration-option-widget-set!");
        setter_inited = TRUE;
      }
      /* HACK: gh_ulong2scm might not always be OK for pointers... */
      gh_call2(item_widget_setter, item,
               gh_ulong2scm((unsigned long) value_widget));
    } 
    
    {
      GtkWidget *help_button = gtk_button_new_with_label("Help");
      gtk_widget_show(help_button);
      gtk_box_pack_end(GTK_BOX(hbox), help_button, FALSE, FALSE, 3);
      gtk_signal_connect (GTK_OBJECT (help_button), 
                          "button_press_event",
                          (GtkSignalFunc) show_option_documentation,
                          (gpointer) g_strdup(item_documentation));
    }

    
    if(known_type) {
      GtkWidget *default_button = gtk_button_new_with_label("Set to default");
      gtk_widget_show(default_button);
      gtk_box_pack_end(GTK_BOX(hbox), default_button, FALSE, FALSE, 3);  
      gtk_signal_connect (GTK_OBJECT (default_button), 
                          "button_press_event",
                          (GtkSignalFunc) call_reset_to_default_func,
                          (gpointer) item);
    }
    gtk_widget_show(hbox);    
  }
  free(item_type);
  free(item_name);
  free(item_documentation);
}

GtkWidget *
_gnc_options_dialog_add_page_(const char label[]) {
  
  GtkWidget *page_label;
  GtkWidget *page_content_box;

  page_label = gtk_label_new(label);
  gtk_widget_show(page_label);
  page_content_box = gtk_vbox_new ( FALSE, 1 );
  gtk_widget_show (page_content_box);
  
  gnome_property_box_append_page(options_dialog, page_content_box, page_label);
  
  //gtk_widget_set_usize ( GTK_WIDGET(box2), 225, 225 ); 
  return(page_content_box);
}

static void
build_options_dialog_contents() {
  const char builder_name[] = "gnc_:build-options-dialog";

  /* FIXME: We should be using this, but it doesn't work.  I think
     it's a module namespace issue...*/
  /*SCM scm_builder = gh_lookup(builder_name);*/

  SCM scm_builder = gh_eval_str((char *) builder_name);
  
  if(gh_procedure_p(scm_builder)) {
    gh_call0(scm_builder);
  } else {
    fprintf(stderr, "gnucash: function lookup failed %s\n", builder_name);
    exit(1);
  }
}

static gint
options_dialog_close_cb(GtkWidget *widget,
                        GdkEventButton *event,
                        gpointer data) {
  gh_eval_str("(gnc:options-dialog-cancel-clicked)");
  return 0;
}

#if 0
static gint
options_dialog_ok_cb(GtkWidget *widget,
                        GdkEventButton *event,
                        gpointer data) {
  gh_eval_str("(gnc:options-dialog-ok-clicked)");

  /* Need to return a value that determines if we succeed...  If we
     don't, don't close the window...*/

  return 0;
}
#endif


/* Options dialog... this should house all of the config options     */
/* like where the docs reside, and whatever else is deemed necessary */
void
gnc_show_options_dialog()  {

  if(!options_dialog) {
    gh_eval_str("(gnc:options-dialog-clear-cancel-actions)");
    //gh_eval_str("(gnc:options-dialog-clear-apply-actions)");

    options_dialog = GNOME_PROPERTY_BOX(gnome_property_box_new());
    gnome_dialog_close_hides (GNOME_DIALOG(options_dialog), TRUE);

    build_options_dialog_contents();
    
    gtk_widget_set_usize ( GTK_WIDGET(options_dialog), 500, 400 );

    gtk_signal_connect (GTK_OBJECT (options_dialog->cancel_button), 
                        "button_press_event",
                        (GtkSignalFunc) options_dialog_close_cb,
                        (gpointer) NULL);
  }
  gtk_widget_show(GTK_WIDGET(options_dialog));  
  gdk_window_raise(GTK_WIDGET(options_dialog)->window);
}

