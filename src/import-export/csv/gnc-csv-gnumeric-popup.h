/* The following is code copied from Gnumeric 1.7.8 licensed under the
 * GNU General Public License version 2. It is from the file
 * gnumeric/src/gui-util.h, and it has been modified slightly to work
 * within GnuCash. */

#ifndef GNC_CSV_GNUMERIC_POPUP
#define GNC_CSV_GNUMERIC_POPUP

#include <gtk/gtk.h>

typedef struct {
	char const *name;
	char const *pixmap;
	int display_filter;
	int sensitive_filter;

	int index;
} GnumericPopupMenuElement;

typedef gboolean (*GnumericPopupMenuHandler) (GnumericPopupMenuElement const *e,
					      gpointer user_data);

/* Use this on menus that are popped up */
void gnumeric_popup_menu (GtkMenu *menu, GdkEventButton *event);

void gnumeric_create_popup_menu (GnumericPopupMenuElement const *elements,
				 GnumericPopupMenuHandler handler,
				 gpointer user_data,
				 int display_filter,
				 int sensitive_filter,
				 GdkEventButton *event);


#endif
