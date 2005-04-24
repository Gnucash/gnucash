#ifndef GNUMERIC_APPLICATION_H
#define GNUMERIC_APPLICATION_H

#include "gnumeric.h"
#include <glib-object.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#define GNM_APP_TYPE	(gnm_app_get_type ())
typedef gboolean (*GnmWbIterFunc) (Workbook *, gpointer data);

GType	     gnm_app_get_type (void);
GObject     *gnm_app_get_app (void);

/* List of active workbooks */
void         gnm_app_workbook_list_add     (Workbook *wb);
void         gnm_app_workbook_list_remove  (Workbook *wb);
GList *      gnm_app_workbook_list 	   (void);
Workbook    *gnm_app_workbook_get_by_name  (char const *name);
Workbook    *gnm_app_workbook_get_by_index (int i);
gboolean     gnm_app_workbook_foreach	   (GnmWbIterFunc func, gpointer data);

GSList const*gnm_app_history_get_list	   (gboolean force_reload);
void	     gnm_app_history_add	   (char const *filename);

/* Prefs */
gboolean     gnm_app_use_auto_complete    (void);
gboolean     gnm_app_use_transition_keys  (void);
void         gnm_app_set_transition_keys  (gboolean);
gboolean     gnm_app_live_scrolling	  (void);
int	     gnm_app_auto_expr_recalc_lag (void);

/* stuff that should move */
GdkPixbuf   *gnm_app_get_pixbuf		  (char const *name);
void         gnm_app_release_pref_dialog  (void);
gpointer     gnm_app_get_pref_dialog	  (void);
void         gnm_app_set_pref_dialog	  (gpointer dialog);

double	     gnm_app_display_dpi_get	  (gboolean horizontal);
double	     gnm_app_dpi_to_pixels	  (void);

/* Clipboard */
void		 gnm_app_clipboard_clear	  (gboolean drop_selection);
void		 gnm_app_clipboard_cut_copy	  (WorkbookControl *wbc, gboolean is_cut,
						   SheetView *sv, GnmRange const *area,
						   gboolean animate_range);
void		 gnm_app_clipboard_cut_copy_obj	  (WorkbookControl *wbc, gboolean is_cut,
						   SheetView *sv, GSList *objects);
void		 gnm_app_clipboard_unant	  (void);
gboolean	 gnm_app_clipboard_is_empty	  (void);
gboolean	 gnm_app_clipboard_is_cut	  (void);
Sheet		*gnm_app_clipboard_sheet_get	  (void);
SheetView	*gnm_app_clipboard_sheet_view_get (void);
GnmCellRegion	*gnm_app_clipboard_contents_get	  (void);
GnmRange const	*gnm_app_clipboard_area_get	  (void);

/**********************************************************************
 * Temporary home for extra actions until we rework this in 1.5
 * with libgoffice
 **/
 
typedef struct _GnmAction GnmAction;
typedef void (*GnmActionHandler) (GnmAction const *action, WorkbookControl *wbc,
				  gpointer user_data);
struct _GnmAction {
	char *id;	 /* id of the function that will handle this */
	char *label;	 /* untranslated, gettext domain will be passed later */
	char *icon_name; /* optionally NULL */
	/* simplistic for now :
	 * is the action always available (File -> New) or only available
	 * when we are not editing (Cell -> Format)
	 * Later on this needs to be more comprehensive with things like
	 * per-sheetobject flags
	 **/
	gboolean always_available;

	GnmActionHandler	handler;
};
typedef struct {
	GSList	   *actions;
	char	   *layout;
	char const *domain;
	gpointer    user_data;
} GnmAppExtraUI;

GnmAction *gnm_action_new  (char const *name, char const *label,
			    char const *icon, gboolean always_available,
			    GnmActionHandler handler);
void	   gnm_action_free (GnmAction *action);

GnmAppExtraUI *gnm_app_add_extra_ui (GSList *actions, char *layout,
				     char const *domain,
				     gpointer user_data);
void	   gnm_app_remove_extra_ui  (GnmAppExtraUI *extra_ui);
void	   gnm_app_foreach_extra_ui (GFunc func, gpointer data);

/**********************************************************************/

/* internal implementation util */
void _gnm_app_flag_windows_changed (void);

#endif /* GNUMERIC_APPLICATION_H */
