#include <gnome.h>


void
gnc_ui_qif_account_picker_new_cb       (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_print_check_dialog_ok_cb        (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_print_check_dialog_cancel_cb    (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_print_check_dialog_help_cb      (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_find_transactions_dialog_early_date_toggle_cb
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
gnc_ui_find_transactions_dialog_late_date_toggle_cb
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
gnc_ui_find_transactions_dialog_search_type_cb
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
gnc_ui_find_transactions_dialog_ok_cb  (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_find_transactions_dialog_cancel_cb
                                        (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_find_transactions_dialog_help_cb
                                        (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_select_commodity_namespace_changed_cb
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
gnc_ui_select_commodity_ok_cb          (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_select_commodity_new_cb         (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_select_commodity_cancel_cb      (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_commodity_ok_cb                 (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_commodity_cancel_cb             (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_commodity_help_cb               (GtkButton       *button,
                                        gpointer         user_data);

gboolean
gnc_ui_commodity_druid_cancel_cb       (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
gnc_ui_commodity_druid_finish_cb       (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
gnc_ui_qif_import_cancel_cb            (GnomeDruid      *gnomedruid,
                                        gpointer         user_data);

gboolean
gnc_ui_qif_import_generic_next_cb      (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

gboolean
gnc_ui_qif_import_load_file_next_cb    (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

gboolean
gnc_ui_qif_import_generic_back_cb      (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
gnc_ui_qif_import_select_file_cb       (GtkButton       *button,
                                        gpointer         user_data);

gboolean
gnc_ui_qif_import_date_format_next_cb  (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

gboolean
gnc_ui_qif_import_default_acct_back_cb (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

gboolean
gnc_ui_qif_import_default_acct_next_cb (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
gnc_ui_qif_import_loaded_files_prepare_cb
                                        (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
gnc_ui_qif_import_select_loaded_file_cb
                                        (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data);

void
gnc_ui_qif_import_load_another_cb      (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_qif_import_unload_file_cb       (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_qif_import_accounts_prepare_cb  (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
gnc_ui_qif_import_account_line_select_cb
                                        (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data);

void
gnc_ui_qif_import_categories_prepare_cb
                                        (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
gnc_ui_qif_import_category_line_select_cb
                                        (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data);

gboolean
gnc_ui_qif_import_memo_next_cb         (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
gnc_ui_qif_import_memo_prepare_cb      (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
gnc_ui_qif_import_memo_line_select_cb  (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data);

gboolean
gnc_ui_qif_import_currency_next_cb     (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
gnc_ui_qif_import_commodity_prepare_cb (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
gnc_ui_qif_import_duplicate_new_select_cb
                                        (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data);

void
gnc_ui_qif_import_duplicate_old_select_cb
                                        (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data);

void
gnc_ui_qif_import_finish_cb            (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
gnc_help_window_search_button_cb       (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_help_window_search_help_button_cb  (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_column_view_edit_add_cb            (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_column_view_edit_remove_cb         (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_edit_column_view_move_up_cb        (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_edit_column_view_move_down_cb      (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_column_view_edit_size_cb           (GtkButton       *button,
                                        gpointer         user_data);
