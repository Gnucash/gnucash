/* Copyright (C) 2005 Chris Shoemaker <c.shoemaker@cox.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */


#ifndef GNC_DIALOG_H
#define GNC_DIALOG_H

#include <time.h>

GType gnc_dialog_get_type (void);

/* type macros */
#define GNC_TYPE_DIALOG            (gnc_dialog_get_type ())
#define GNC_DIALOG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                      GNC_TYPE_DIALOG, GncDialog))
#define GNC_DIALOG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), \
                                      GNC_TYPE_DIALOG, GncDialogClass))
#define GNC_IS_DIALOG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                      GNC_TYPE_DIALOG))
#define GNC_IS_DIALOG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), \
                                      GNC_TYPE_DIALOG))
#define GNC_DIALOG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), \
                                      GNC_TYPE_DIALOG, GncDialogClass))

typedef struct _GncDialog GncDialog;
typedef struct _GncDialogClass GncDialogClass;

/**** PROTOTYPES *************************************************/

/* filename is a glade filenames; and root is the name of the root
   widget you want to show. */
GncDialog *gnc_dialog_new(const char *filename,
                          const char *root);


typedef gboolean (*GncDialogCallback) (GncDialog *d, gpointer user_data);

/* The apply callback is optional, but useful dialogs will probably
 * supply one.  The apply callback should return FALSE if any values
 * are invalid.  In that case, the dialog will not close automatically
 * after the user clicks OK, and the changed state will not be marked
 * clean.  If you supply NULL, for the apply callback, no "OK" button
 * will be visible.
 *
 * The close callback is optional.  If you provide a close callback,
 * its return value should be TRUE if you want to proceed with the
 * close.  There's no destroy notifier for user_data, but you can
 * treat the close_cb as one.  So if you must pass this function its
 * own copy of user_data, free it from within close_cb.
 *
 * The help callback return value is not checked.
 *
 * Any callback may be NULL, in which case it's not used.  If help_cb
 * is NULL, no help button is shown.
 */
void gnc_dialog_set_cb(GncDialog *d,
                       GncDialogCallback apply_cb,
                       GncDialogCallback close_cb,
                       GncDialogCallback help_cb,
                       gpointer user_data);

/* By default, GncDialog works best in asynchronous environments,
 * where your program execution flow isn't waiting for the dialog to
 * close.  But, if you're using the dialog to do something like fetch
 * a value you want to return on the stack, then you have to block the
 * current thread until the dialog is closed.  Calling this function
 * will do exactly that.
 */
void gnc_dialog_block_until_close(GncDialog *d);

/* This is a catch-all interface to whatever kind of widgets may have
 * been specified in the glade file.  Once you have you widget you can
 * use whatever interface that widget offers to set and get widget
 * state.  You _have_ to use if the widget type isn't supported by the
 * type-specific or type-generic interfaces below.
 */
GtkWidget *gnc_dialog_get_widget(GncDialog *d, const gchar* name);

void gnc_dialog_set_sensitive(GncDialog *d, const gchar* name, gboolean sen);

/* Infers val type from widget type *
*/

/* Type-generic getter/setter: Be careful with these.  They are NOT
 * type safe.  Also, if they prove to be more trouble than they're
 * worth, they'll go away.
 *
 * These functions try to use the widget type to infer the type of
 * data pointed at by val.  They will return FALSE if they are unable
 * to infer value type.  The inferences made are:
 *
 * Widget Type ---> Value Type
 * ===========      ==========
 * GnomeDateEdit     GDate *
 * GtkSpinButton     gdouble *
 * GtkToggleButton   gboolean *
 * GtkEntry          gchar *
 * GtkLabel          gchar *
 * GtkTextView       GtkTextBuffer *
 * GtkComboBox       gint *
 *
 * WARNING: For the given widget type you must cast the corresponding
 * value type to/from the passed gpointer.  Having mis-matched widget
 * and value types will likely cause a revolt among the electrons.
 *
 */
gboolean gnc_dialog_set(GncDialog *d, const char* name, const gpointer val);
gpointer gnc_dialog_get(GncDialog *d, const char* name);

/* Type-specific getter/setters */
gboolean     gnc_dialog_set_string(GncDialog *d, const char* name,
                               const gchar* val);
gchar* gnc_dialog_get_string(GncDialog *d, const char* name);

gboolean gnc_dialog_set_double(GncDialog *d, const char* name, gdouble val);
gdouble  gnc_dialog_get_double(GncDialog *d, const char* name);

gboolean gnc_dialog_set_int(GncDialog *d, const char* name, gint val);
gint  gnc_dialog_get_int(GncDialog *d, const char* name);

gboolean gnc_dialog_set_date(GncDialog *d, const char* name, time_t val);
time_t   gnc_dialog_get_date(GncDialog *d, const char* name);

gboolean gnc_dialog_set_index(GncDialog *d, const char* name, gint val);
gint     gnc_dialog_get_index(GncDialog *d, const char* name);

gboolean gnc_dialog_set_boolean(GncDialog *d, const char* name, gboolean val);
gboolean gnc_dialog_get_boolean(GncDialog *d, const char* name);

/* Possible TODO: there are more types that could be added here.

Maybe currency/gnc_commodity *

*/

gpointer gnc_dialog_get_custom(GncDialog *d, const char* name);
gboolean gnc_dialog_set_custom(GncDialog *d, const char* name, gpointer val);
gboolean gnc_dialog_fill_custom(GncDialog *d, const char* name);

/* should return true for success */
typedef gboolean (*GncDialogSetter) (gpointer widget, gpointer val);
typedef gpointer (*GncDialogGetter) (gpointer widget);
//typedef gboolean (*GncDialogFiller) (gpointer widget, gpointer data);

void gnc_dialog_register_custom(GType widgetType, GncDialogGetter getter,
                                GncDialogSetter setter,
                                GncDialogSetter filler);

void gnc_dialog_unregister_custom(GType widgetType);
void gnc_dialog_register_testing_types(void);

#endif
