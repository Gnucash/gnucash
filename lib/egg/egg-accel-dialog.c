#include "egg-accel-dialog.h"

static void egg_accel_dialog_init       (EggAccelDialog *self);
static void egg_accel_dialog_class_init (EggAccelDialogClass *class);

GType
egg_accel_dialog_get_type (void)
{
  static GtkType type = 0;

  if (!type)
    {
      static const GTypeInfo type_info =
      {
        sizeof (EggAccelDialogClass),
        (GBaseInitFunc) NULL,
        (GBaseFinalizeFunc) NULL,
        (GClassInitFunc) egg_accel_dialog_class_init,
        (GClassFinalizeFunc) NULL,
        NULL,
        
        sizeof (EggAccelDialog),
        0, /* n_preallocs */
        (GInstanceInitFunc) egg_accel_dialog_init,
      };

      type = g_type_register_static (GTK_TYPE_DIALOG,
                                     "EggAccelDialog",
                                     &type_info, 0);
    }
  return type;
}

static void
egg_accel_dialog_class_init (EggAccelDialogClass *class)
{
}

static void accel_path_selection_changed (GtkTreeSelection *selection,
					  EggAccelDialog   *self);
static void accel_path_set (GtkWidget *button, EggAccelDialog *self);
static void accel_path_reset (GtkWidget *button, EggAccelDialog *self);

static void
egg_accel_dialog_init (EggAccelDialog *self)
{
  GtkCellRenderer *renderer;
  GtkWidget *swin;
  GtkWidget *table;

  /* set up the list store for all the accelerators */
  self->accel_store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);
  gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (self->accel_store),
					0, GTK_SORT_ASCENDING);
  egg_accel_dialog_rescan_accels (self);


  swin = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (swin),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (self)->vbox), swin,
		      TRUE, TRUE, 0);
  gtk_widget_show (swin);

  /* set up a two column view of the model in browse selection mode */
  self->accel_view = gtk_tree_view_new_with_model (GTK_TREE_MODEL(self->accel_store));
  gtk_tree_view_set_rules_hint (GTK_TREE_VIEW (self->accel_view), TRUE);
  gtk_tree_selection_set_mode (gtk_tree_view_get_selection (GTK_TREE_VIEW (self->accel_view)),
			       GTK_SELECTION_BROWSE);
  g_signal_connect_object (gtk_tree_view_get_selection (GTK_TREE_VIEW (self->accel_view)),
			   "changed", G_CALLBACK (accel_path_selection_changed),
			   G_OBJECT (self), 0);

  renderer = gtk_cell_renderer_text_new ();
  g_object_set (G_OBJECT (renderer), "xalign", 0.0, NULL);
  gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (self->accel_view),
					       -1, "Path", renderer,
					       "text", 0,
					       NULL);
  gtk_tree_view_column_set_sort_column_id (gtk_tree_view_get_column (GTK_TREE_VIEW (self->accel_view), 0), 0);

  renderer = gtk_cell_renderer_text_new ();
  g_object_set (G_OBJECT (renderer), "xalign", 0.0, NULL);
  gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (self->accel_view),
					       -1, "Accel", renderer,
					       "text", 1,
					       NULL);
  gtk_tree_view_column_set_sort_column_id (gtk_tree_view_get_column (GTK_TREE_VIEW (self->accel_view), 1), 1);

  gtk_container_add (GTK_CONTAINER (swin), self->accel_view);
  gtk_widget_show (self->accel_view);

  table = gtk_table_new (2, 4, FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (table), 2);
  gtk_table_set_row_spacings (GTK_TABLE (table), 2);
  gtk_table_set_col_spacings (GTK_TABLE (table), 2);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (self)->vbox), table, FALSE,TRUE, 0);
  gtk_widget_show (table);

  /* widgets for editing accels */
  self->shift_toggle = gtk_check_button_new_with_mnemonic ("S_hift");
  gtk_table_attach (GTK_TABLE (table), self->shift_toggle,
		    0, 1,  0, 1,
		    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (self->shift_toggle);
  self->ctrl_toggle = gtk_check_button_new_with_mnemonic ("_Ctrl");
  gtk_table_attach (GTK_TABLE (table), self->ctrl_toggle,
		    1, 2,  0, 1,
		    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (self->ctrl_toggle);
  self->alt_toggle = gtk_check_button_new_with_mnemonic ("_Alt");
  gtk_table_attach (GTK_TABLE (table), self->alt_toggle,
		    2, 3,  0, 1,
		    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (self->alt_toggle);
  self->key_entry = gtk_entry_new ();
  gtk_table_attach (GTK_TABLE (table), self->key_entry,
		    0, 3,  1, 2,
		    GTK_FILL|GTK_EXPAND, GTK_FILL, 0, 0);
  gtk_widget_show (self->key_entry);

  /* buttons for changing path */
  self->set_button = gtk_button_new_with_mnemonic ("_Set");
  gtk_table_attach (GTK_TABLE (table), self->set_button,
		    3, 4,  0, 1,
		    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (self->set_button);
  self->reset_button = gtk_button_new_with_mnemonic ("_Reset");
  gtk_table_attach (GTK_TABLE (table), self->reset_button,
		    3, 4,  1, 2,
		    GTK_FILL, GTK_FILL, 0, 0);
  /*gtk_widget_show (self->reset_button);*/

  g_signal_connect_object (self->set_button, "clicked",
			   G_CALLBACK (accel_path_set), G_OBJECT (self), 0);
  g_signal_connect_object (self->reset_button, "clicked",
			   G_CALLBACK (accel_path_reset), G_OBJECT (self), 0);


  self->ok_button = gtk_dialog_add_button (GTK_DIALOG (self),
					   GTK_STOCK_OK,
					   GTK_RESPONSE_OK);
  gtk_widget_grab_default (self->ok_button);
}

static void
accel_map_foreach (gpointer     data,
		   const gchar *accel_path,
		   guint        accel_key,
		   guint        accel_mods,
		   gboolean     changed)
{
  EggAccelDialog *self = data;
  GtkTreeIter iter;
  gchar *accel_name;

  gtk_list_store_append (self->accel_store, &iter);
  if (accel_key != 0)
    accel_name = gtk_accelerator_name (accel_key, accel_mods);
  else
    accel_name = "";

  gtk_list_store_set (self->accel_store, &iter,
		      0, accel_path,
		      1, accel_name,
		      -1);
  if (accel_key != 0)
    g_free(accel_name);
}

void
egg_accel_dialog_rescan_accels (EggAccelDialog *self)
{
  g_return_if_fail (EGG_IS_ACCEL_DIALOG (self));

  gtk_list_store_clear (self->accel_store);
  gtk_accel_map_foreach (self, accel_map_foreach);
}

/* make sure the currently selected accel is up to date */
static void
refresh_selected_row (EggAccelDialog *self)
{
  GtkTreeSelection *selection;
  GtkTreeIter iter;

  g_return_if_fail (EGG_IS_ACCEL_DIALOG (self));

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (self->accel_view));
  if (gtk_tree_selection_get_selected (selection, NULL, &iter)) {
    char *accel_path;

    /* get the accel path for the selected row */
    gtk_tree_model_get (GTK_TREE_MODEL (self->accel_store), &iter,
			0, &accel_path, -1);
    if (accel_path) {
      GtkAccelKey key;

      if (gtk_accel_map_lookup_entry (accel_path, &key)) {
	char *accel_name;

	accel_name = gtk_accelerator_name (key.accel_key, key.accel_mods);
	gtk_list_store_set (self->accel_store, &iter, 1, accel_name, -1);
	g_free (accel_name);
      }
      g_free (accel_path);
    }
  }
}

static void
accel_path_selection_changed (GtkTreeSelection *selection,
			      EggAccelDialog   *self)
{
  GtkTreeIter iter;

  /* just make sure the selected row is up to date */
  refresh_selected_row (self);

  if (gtk_tree_selection_get_selected (selection, NULL, &iter)) {
    char *accel_path;

    /* get the accel path for the selected row */
    gtk_tree_model_get (GTK_TREE_MODEL (self->accel_store), &iter,
			0, &accel_path, -1);
    if (accel_path) {
      GtkAccelKey key;

      if (gtk_accel_map_lookup_entry (accel_path, &key)) {
	gchar *keyname;

	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (self->shift_toggle),
				      (key.accel_mods & GDK_SHIFT_MASK)!=0);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (self->ctrl_toggle),
				      (key.accel_mods & GDK_CONTROL_MASK)!=0);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (self->alt_toggle),
				      (key.accel_mods & GDK_MOD1_MASK)!=0);
	keyname = gdk_keyval_name (key.accel_key);
	if (keyname)
	  gtk_entry_set_text (GTK_ENTRY (self->key_entry), keyname);
	else
	  gtk_entry_set_text (GTK_ENTRY (self->key_entry), "");
      }
    }
    g_free (accel_path);
  }
}

static void
accel_path_set (GtkWidget *button, EggAccelDialog *self)
{
  GtkTreeSelection *selection;
  GtkTreeIter iter;
  gboolean changed = FALSE;

  g_return_if_fail (EGG_IS_ACCEL_DIALOG (self));

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (self->accel_view));
  if (gtk_tree_selection_get_selected (selection, NULL, &iter)) {
    char *accel_path;

    /* get the accel path for the selected row */
    gtk_tree_model_get (GTK_TREE_MODEL (self->accel_store), &iter,
			0, &accel_path, -1);
    if (accel_path) {
      GdkModifierType accel_mods = 0;
      const gchar *key_name;
      guint accel_key = 0;

      /* get modifiers */
      if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(self->shift_toggle)))
	accel_mods |= GDK_SHIFT_MASK;
      if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(self->ctrl_toggle)))
	accel_mods |= GDK_CONTROL_MASK;
      if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(self->alt_toggle)))
	accel_mods |= GDK_MOD1_MASK;

      key_name = gtk_entry_get_text (GTK_ENTRY (self->key_entry));
      /* check to see if entyr is empty -- if so, unset accel */
      if (key_name[0] != '\0') {
	accel_key = gdk_keyval_from_name (key_name);

	if (accel_key) {
	  changed = gtk_accel_map_change_entry (accel_path,
						accel_key, accel_mods,
						TRUE);
	}
      } else
	changed = gtk_accel_map_change_entry (accel_path, 0, 0, TRUE);
      
      g_free (accel_path);
    }
  }
  if (!changed)
    gdk_beep ();
  accel_path_selection_changed (selection, self);
}

static void
accel_path_reset (GtkWidget *button, EggAccelDialog *self)
{
  GtkTreeSelection *selection;
  GtkTreeIter iter;
  gboolean changed = FALSE;

  g_return_if_fail (EGG_IS_ACCEL_DIALOG (self));

  g_message ("don't know how to reset to defaults :(");
  return;

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (self->accel_view));
  if (gtk_tree_selection_get_selected (selection, NULL, &iter)) {
    char *accel_path;

    /* get the accel path for the selected row */
    gtk_tree_model_get (GTK_TREE_MODEL (self->accel_store), &iter,
			0, &accel_path, -1);
    if (accel_path) {
      changed = gtk_accel_map_change_entry (accel_path, 0, 0, TRUE);
      g_free (accel_path);
    }
  }
  if (!changed)
    gdk_beep ();
  accel_path_selection_changed (selection, self);
}

GtkWidget *
egg_accel_dialog_new (void)
{
  return g_object_new(EGG_TYPE_ACCEL_DIALOG, NULL);
}
