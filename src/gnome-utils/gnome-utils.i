%module sw_gnome_utils
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <gtk/gtk.h>
#include <glib-object.h>
#include <dialog-options.h>
#include <dialog-utils.h>
#include <druid-utils.h>
#include <gnc-amount-edit.h>
#include <gnc-date-edit.h>
#include <gnc-file.h>
#include <gnc-gnome-utils.h>
#include <gnc-gui-query.h>
#include <gnc-main-window.h>
#include <gnc-window.h>
#include <gnc-menu-extensions.h>
#include <gnc-plugin-file-history.h>
#include <gnc-ui.h>
#include <gnc-splash.h>

SCM scm_init_sw_gnome_utils_module (void);
%}

%import "base-typemaps.i"

GNCOptionWin * gnc_options_dialog_new(gchar *title);
void gnc_options_dialog_destroy(GNCOptionWin * win);
void gnc_options_dialog_build_contents(GNCOptionWin *propertybox,
                                       GNCOptionDB  *odb);
void gnc_options_dialog_set_scm_callbacks (GNCOptionWin *win,
        SCM apply_cb, SCM close_cb);

gboolean
gnc_verify_dialog(GtkWidget *parent, gboolean yes_is_default,
		  const gchar *format, ...);

void
gnc_warning_dialog(GtkWidget *parent,
                   const gchar *format, ...);
void
gnc_error_dialog(GtkWidget *parent,
		 const char *format, ...);
void
gnc_info_dialog(GtkWidget *parent,
		const char *format, ...);

void gnc_add_scm_extension (SCM extension);

void gnc_set_busy_cursor (GtkWidget *w, gboolean update_now);
void gnc_unset_busy_cursor (GtkWidget *w);
void gnc_window_show_progress (const char *message, double percentage);

gboolean gnucash_ui_is_running(void);
