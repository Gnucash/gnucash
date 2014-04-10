#ifndef GNM_CONF_H
#define GNM_CONF_H

#include <numbers.h>
#include <gnumeric.h>
//#include <print-info.h>

typedef struct {
	struct {
		GSList	const *extra_dirs;
		char	*sys_dir;
		char	*usr_dir;
	} autoformat;

	struct {
		char const *name;
		float size;
		gboolean is_bold, is_italic;
	} default_font;

	gint     	 file_history_max;
	GSList const	*file_history_files;
	guint    	 num_of_recent_funcs;
	GSList const	*recent_funcs;

	GSList const	*plugin_file_states;
	GSList const	*plugin_extra_dirs;
	GSList const	*active_plugins;
	gboolean	 activate_new_plugins;

	gboolean	 show_sheet_name;
	guint		 max_descriptor_width;
	gint		 undo_size;
	gint		 undo_max_number;

	gint		 initial_sheet_number;
	float		 horizontal_window_fraction;
	float		 vertical_window_fraction;
	float		 zoom;

	gint		 xml_compression_level;
	gboolean 	 file_overwrite_default_answer;
	gboolean 	 file_ask_single_sheet_save;

	gboolean 	 sort_default_by_case;
	gboolean 	 sort_default_retain_formats;
	gboolean 	 sort_default_ascending;
	gint     	 sort_max_initial_clauses;

	gboolean	 print_all_sheets; /* vs print only selected */
	gchar           *printer_config;
	GSList const    *printer_header;
	GSList const    *printer_footer;
	GSList const    *printer_header_formats_left;
	GSList const    *printer_header_formats_middle;
	GSList const    *printer_header_formats_right;
	GnmStyle        *printer_decoration_font;
	gboolean         print_center_horizontally;
	gboolean         print_center_vertically;
	gboolean         print_grid_lines;
	gboolean         print_even_if_only_styles;
	gboolean         print_black_and_white;
	gboolean         print_titles;
	gboolean         print_order_right_then_down;
	gboolean         print_scale_percentage;
	float            print_scale_percentage_value;
	gint             print_scale_width;
	gint             print_scale_height;
	gchar           *print_repeat_top;
	gchar           *print_repeat_left;
  //PrintMargins     print_tb_margins;
	
	float		 horizontal_dpi;
	float		 vertical_dpi;
	gboolean	 auto_complete;
	gboolean	 transition_keys;
	gboolean	 live_scrolling;
	gint		 recalc_lag;
	gboolean	 unfocused_range_selection;
	gboolean         prefer_clipboard_selection;  /* As opposed to "primary".  */
	gboolean	 latex_use_utf8;
} GnmAppPrefs;
extern GnmAppPrefs const *gnm_app_prefs;

void     gnm_conf_init (gboolean fast);
void     gnm_conf_shutdown (void);
void     gnm_conf_sync (void);

/* autocorrect */
void     gnm_gconf_set_autocorrect_init_caps (gboolean val);
void     gnm_gconf_set_autocorrect_first_letter (gboolean val);
void     gnm_gconf_set_autocorrect_names_of_days (gboolean val);
void     gnm_gconf_set_autocorrect_replace (gboolean val);

/* autocomplete */
void     gnm_gconf_set_autocomplete (gboolean val);

/* autoformat */
void     gnm_gconf_set_autoformat_sys_dirs (char const * string);
void     gnm_gconf_set_autoformat_usr_dirs (char const * string);

/* file history */
void     gnm_gconf_set_file_history_files (GSList *list);
void     gnm_gconf_set_file_history_number (gint value);

/* plugins */
void     gnm_gconf_set_plugin_file_states (GSList *list);
void     gnm_gconf_set_plugin_extra_dirs (GSList *list);
void     gnm_gconf_set_active_plugins (GSList *list);
void     gnm_gconf_set_activate_new_plugins (gboolean val);

/* undo */
void     gnm_gconf_set_show_sheet_name (gboolean val);
void     gnm_gconf_set_max_descriptor_width (gint val);
void     gnm_gconf_set_undo_size (gint val);
void     gnm_gconf_set_undo_max_number (gint val);

/* xml/files */
void     gnm_gconf_set_recent_funcs (GSList *list);
void     gnm_gconf_set_xml_compression (gint value);
void     gnm_gconf_set_file_overwrite (gboolean value);
void     gnm_gconf_set_file_single_sheet_save (gboolean value);

/* print-setup & printing */
void     gnm_gconf_set_all_sheets (gboolean val);
void     gnm_gconf_set_printer_config (gchar *str);
void     gnm_gconf_set_printer_header (gchar const *left, gchar const *middle, 
				       gchar const *right);
void     gnm_gconf_set_printer_footer (gchar const *left, gchar const *middle, 
				       gchar const *right);
void     gnm_gconf_set_print_center_horizontally (gboolean val);
void     gnm_gconf_set_print_center_vertically (gboolean val);
void     gnm_gconf_set_print_grid_lines (gboolean val);
void     gnm_gconf_set_print_even_if_only_styles (gboolean val);
void     gnm_gconf_set_print_black_and_white (gboolean val);
void     gnm_gconf_set_print_titles (gboolean val);
void     gnm_gconf_set_print_order_right_then_down (gboolean val);
void     gnm_gconf_set_print_scale_percentage (gboolean val);
void     gnm_gconf_set_print_scale_percentage_value (gnm_float val);
//void     gnm_gconf_set_print_tb_margins (PrintMargins const *pm);
void     gnm_gconf_set_print_header_formats (GSList *left, GSList *middle, 
					     GSList *right);

/* gui */
void     gnm_gconf_set_gui_window_x (gnm_float val);
void     gnm_gconf_set_gui_window_y (gnm_float val);
void     gnm_gconf_set_gui_zoom (gnm_float val);
void     gnm_gconf_set_gui_transition_keys (gboolean value);
void     gnm_gconf_set_gui_livescrolling (gboolean value);
void     gnm_gconf_set_gui_resolution_h (gnm_float val);
void     gnm_gconf_set_gui_resolution_v (gnm_float val);

/* default font */
void     gnm_gconf_set_default_font_size (gnm_float val);
void     gnm_gconf_set_default_font_name (char const *str);
void     gnm_gconf_set_default_font_bold (gboolean val);
void     gnm_gconf_set_default_font_italic (gboolean val);

/* hf font */
void     gnm_gconf_set_hf_font (GnmStyle const *mstyle);

/* sorting */
void     gnm_gconf_set_sort_dialog_max_initial (gint value);
void     gnm_gconf_set_sort_retain_form (gboolean value);
void     gnm_gconf_set_sort_by_case (gboolean value);
void     gnm_gconf_set_sort_ascending (gboolean value);

/* workbook */
void     gnm_gconf_set_workbook_nsheets (gint value);
void     gnm_gconf_set_unfocused_rs (gboolean value);

/* function selector and formula guru */
void     gnm_gconf_set_num_recent_functions (gint value);

/* standard plugins */
void     gnm_gconf_set_latex_use_utf8 (gboolean value);

/* application interface */
void     gnm_gconf_set_prefer_clipboard  (gboolean value);

/**************************************************************/

char	*go_conf_get_short_desc     (char const *key);
char	*go_conf_get_long_desc      (char const *key);
GType	 go_conf_get_type	    (char const *key);
char	*go_conf_get_value_as_str   (char const *key);
gboolean go_conf_set_value_from_str (char const *key, char const *val_str);

gboolean go_conf_get_bool	(char const *key);
int	 go_conf_get_int	(char const *key);
double	 go_conf_get_double	(char const *key);
char	*go_conf_get_string	(char const *key);
GSList	*go_conf_get_str_list	(char const *key);

gboolean go_conf_load_bool	(char const *key, gboolean default_val);
int	 go_conf_load_int	(char const *key, int minima, int maxima, int default_val);
double	 go_conf_load_double	(char const *key, double minima, double maxima, double default_val);
char	*go_conf_load_string	(char const *key);
GSList	*go_conf_load_str_list	(char const *key);

void	 go_conf_set_bool	(char const *key, gboolean val);
void	 go_conf_set_int	(char const *key, gint val);
void	 go_conf_set_double	(char const *key, gnm_float val);
void	 go_conf_set_string	(char const *key, char const *str);
void	 go_conf_set_str_list	(char const *key, GSList *list);

void	 go_conf_sync		(void);

typedef void (*GOConfMonitorFunc) (char const *key, gpointer data);
void	 go_conf_remove_monitor	(guint monitor_id);
guint	 go_conf_add_monitor	(char const *key,
				 GOConfMonitorFunc monitor, gpointer data);

#endif /* GNM_CONF_H */
