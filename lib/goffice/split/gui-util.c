/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gnumeric-util.c:  Various GUI utility functions.
 *
 * Author:
 *  Miguel de Icaza (miguel@gnu.org)
 */

#include <config.h>
#include <glib/gi18n.h>
#include "gnumeric.h"
#include "gui-util.h"

//#include "workbook-control-gui-priv.h"
#include "gutils.h"
//#include "parse-util.h"
#include "style.h"
#include "style-color.h"
#include "error-info.h"
#include "value.h"
#include "number-match.h"
#include "format.h"
#include "application.h"
//#include "workbook.h"
//#include "libgnumeric.h"

#include <goffice/gui-utils/go-combo-color.h>
#include <glade/glade.h>
#include <gtk/gtklabel.h>
#include <gtk/gtkfilechooser.h>
#include <gtk/gtkmain.h>
#include <gtk/gtkscrolledwindow.h>
#include <gtk/gtkeventbox.h>
#include <gtk/gtkradiobutton.h>
#include <gtk/gtkalignment.h>
#include <gtk/gtkstock.h>
#include <gtk/gtkimage.h>
#include <gtk/gtkframe.h>
#include <gtk/gtkwidget.h>
#include <gtk/gtkimagemenuitem.h>
#include <gtk/gtkbbox.h>
#include <gtk/gtkhbox.h>
#include <atk/atkrelation.h>
#include <atk/atkrelationset.h>
#include <gdk/gdkkeysyms.h>

#include <string.h>

gboolean
gnumeric_dialog_question_yes_no (GtkWindow *parent,
                                 gchar const *message,
                                 gboolean default_answer)
{
	GtkWidget *dialog = gtk_message_dialog_new (parent,
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_QUESTION,
		GTK_BUTTONS_YES_NO,
		message);
	gtk_dialog_set_default_response (GTK_DIALOG (dialog),
		default_answer ? GTK_RESPONSE_YES : GTK_RESPONSE_NO);
	return gnumeric_dialog_run (parent, 
				    GTK_DIALOG (dialog)) == GTK_RESPONSE_YES;
}
/*
 * TODO:
 * Get rid of trailing newlines /whitespace.
 */
void
gnumeric_notice (GtkWindow *parent, GtkMessageType type, char const *str)
{
	GtkWidget *dialog;

	dialog = gtk_message_dialog_new (parent,
                                         GTK_DIALOG_DESTROY_WITH_PARENT, type,
					 GTK_BUTTONS_OK, str);
	gtk_label_set_use_markup (GTK_LABEL (GTK_MESSAGE_DIALOG (dialog)->label), TRUE);

	gnumeric_dialog_run (parent, GTK_DIALOG (dialog));
}

void
gnumeric_notice_nonmodal (GtkWindow *parent, GtkWidget **ref, GtkMessageType type, char const *str)
{
	GtkWidget *dialog;

	if (*ref != NULL)
		gtk_widget_destroy (*ref);

	*ref = dialog = gtk_message_dialog_new (parent, GTK_DIALOG_DESTROY_WITH_PARENT, type,
					 GTK_BUTTONS_OK, str);

	g_signal_connect_object (G_OBJECT (dialog),
		"response",
		G_CALLBACK (gtk_widget_destroy), G_OBJECT (dialog), 0);
	g_signal_connect (G_OBJECT (dialog),
		"destroy",
		G_CALLBACK (gtk_widget_destroyed), ref);

	gtk_widget_show (dialog);

	return;
}

#if 0
static void
fsel_response_cb (GtkFileChooser *dialog,
		  gint response_id,
		  gboolean *result)
{
  if (response_id == GTK_RESPONSE_OK) {
	  char *uri = gtk_file_chooser_get_uri (dialog);

	  if (uri) {
		  g_free (uri);
		  *result = TRUE;
	  }
  }

  gtk_main_quit ();
}

static gint
gu_delete_handler (GtkDialog *dialog,
		   GdkEventAny *event,
		   gpointer data)
{
	gtk_dialog_response (dialog, GTK_RESPONSE_CANCEL);
	return TRUE; /* Do not destroy */
}
#endif // 0 - unused, jsled

#if 0
gboolean
gnumeric_dialog_file_selection (WorkbookControlGUI *wbcg, GtkWidget *w)
{
	/* Note: wbcg will be NULL if called (indirectly) from gog-style.c  */
	gboolean result = FALSE;
	gulong delete_handler;

	g_return_val_if_fail (GTK_IS_FILE_CHOOSER (w), FALSE);

	gtk_window_set_modal (GTK_WINDOW (w), TRUE);
	if (wbcg)
		gnumeric_set_transient (wbcg_toplevel (wbcg), 
					GTK_WINDOW (w));
	g_signal_connect (w, "response",
			  G_CALLBACK (fsel_response_cb), &result);
	delete_handler =
		g_signal_connect (w,
				  "delete_event",
				  G_CALLBACK (gu_delete_handler),
				  NULL);

	gtk_widget_show_all (w);
	gtk_grab_add (w);
	gtk_main ();

	g_signal_handler_disconnect (w, delete_handler);

	return result;
}
#endif // 0


static gint
cb_modal_dialog_keypress (GtkWidget *w, GdkEventKey *e)
{
	if(e->keyval == GDK_Escape) {
		gtk_dialog_response (GTK_DIALOG (w), GTK_RESPONSE_CANCEL);
		return TRUE;
	}

	return FALSE;
}

/**
 * gnumeric_dialog_run
 *
 * Pop up a dialog as child of a window.
 */
gint
gnumeric_dialog_run (GtkWindow *parent, GtkDialog *dialog)
{
	gint      result;

	g_return_val_if_fail (GTK_IS_DIALOG (dialog), GTK_RESPONSE_NONE);

	if (parent) {
		g_return_val_if_fail (GTK_IS_WINDOW (parent), GTK_RESPONSE_NONE);

		gnumeric_set_transient (parent, GTK_WINDOW (dialog));
	}

	g_signal_connect (G_OBJECT (dialog),
		"key-press-event",
		G_CALLBACK (cb_modal_dialog_keypress), NULL);

	while ((result = gtk_dialog_run (dialog)) >= 0)
	       ;
	gtk_widget_destroy (GTK_WIDGET (dialog));
	return result;
}

#define ERROR_INFO_MAX_LEVEL 9
#define ERROR_INFO_TAG_NAME "errorinfotag%i"

static void
insert_error_info (GtkTextBuffer* text, ErrorInfo *error, gint level)
{
	gchar *message = (gchar *) error_info_peek_message (error);
	GSList *details_list, *l;
	GtkTextIter start, last;
	gchar *tag_name = g_strdup_printf (ERROR_INFO_TAG_NAME,
					   MIN (level, ERROR_INFO_MAX_LEVEL));
	if (message == NULL)
		message = g_strdup (_("Multiple errors\n"));
	else
		message = g_strdup_printf ("%s\n", message);
	gtk_text_buffer_get_bounds (text, &start, &last);
	gtk_text_buffer_insert_with_tags_by_name (text, &last,
						  message, -1,
						  tag_name, NULL);
	g_free (tag_name);
	g_free (message);
	details_list = error_info_peek_details (error);
	for (l = details_list; l != NULL; l = l->next) {
		ErrorInfo *detail_error = l->data;
		insert_error_info (text, detail_error, level + 1);
	}
	return;
}

/**
 * gnumeric_error_info_dialog_new
 *
 */
GtkWidget *
gnumeric_error_info_dialog_new (ErrorInfo *error)
{
	GtkWidget *dialog;
	GtkWidget *scrolled_window;
	GtkTextView *view;
	GtkTextBuffer *text;
	GtkMessageType mtype;
	gchar *message;
	gint bf_lim = 1;
	gint i;
	GdkScreen *screen;

	g_return_val_if_fail (error != NULL, NULL);

	message = (gchar *) error_info_peek_message (error);
	if (message == NULL)
		bf_lim++;

	mtype = GTK_MESSAGE_ERROR;
	if (error_info_peek_severity (error) < GNM_ERROR)
		mtype = GTK_MESSAGE_WARNING;
	dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_DESTROY_WITH_PARENT,
					 mtype, GTK_BUTTONS_CLOSE, " ");
	screen = gtk_widget_get_screen (dialog);
	gtk_widget_set_size_request (dialog,
				     gdk_screen_get_width (screen) / 3,
				     gdk_screen_get_width (screen) / 4);
	scrolled_window = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
					GTK_POLICY_AUTOMATIC,
					GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type
		(GTK_SCROLLED_WINDOW (scrolled_window),
		 GTK_SHADOW_ETCHED_IN);
	view = GTK_TEXT_VIEW (gtk_text_view_new ());
	gtk_text_view_set_wrap_mode (view, GTK_WRAP_WORD);
	gtk_text_view_set_editable (view, FALSE);
	gtk_text_view_set_cursor_visible (view, FALSE);

	gtk_text_view_set_pixels_below_lines
		(view, gtk_text_view_get_pixels_inside_wrap (view) + 3);
	text = gtk_text_view_get_buffer (view);
	for (i = ERROR_INFO_MAX_LEVEL; i-- > 0;) {
		gchar *tag_name = g_strdup_printf (ERROR_INFO_TAG_NAME, i);
		gtk_text_buffer_create_tag
			(text, tag_name,
			 "left_margin", i * 12,
			 "right_margin", i * 12,
			 "weight", ((i < bf_lim)
				    ? PANGO_WEIGHT_BOLD
				    : PANGO_WEIGHT_NORMAL),
			 NULL);
		g_free (tag_name);
	}
	insert_error_info (text, error, 0);

	gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET (view));
	gtk_widget_show_all (GTK_WIDGET (scrolled_window));
	gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), scrolled_window, TRUE, TRUE, 0);

	gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);
	return dialog;
}

/**
 * gnumeric_error_info_dialog_show
 *
 */
void
gnumeric_error_info_dialog_show (GtkWindow *parent, ErrorInfo *error)
{
	GtkWidget *dialog = gnumeric_error_info_dialog_new (error);
	gnumeric_dialog_run (parent, GTK_DIALOG (dialog));
}

static void
cb_parent_mapped (GtkWidget *parent, GtkWindow *window)
{
	if (GTK_WIDGET_MAPPED (window)) {
		gtk_window_present (window);
		g_signal_handlers_disconnect_by_func (G_OBJECT (parent),
			G_CALLBACK (cb_parent_mapped), window);
	}
}

/**
 * gnumeric_set_transient
 * @wbcg	: The calling window
 * @window      : the transient window
 *
 * Make the window a child of the workbook in the command context, if there is
 * one.
 * The function duplicates the positioning functionality in
 * gnome_dialog_set_parent, but does not require the transient window to be
 * a GnomeDialog.
 */
void
gnumeric_set_transient (GtkWindow *toplevel, GtkWindow *window)
{
/* FIXME:                                                                     */
/* 	GtkWindowPosition position = gnome_preferences_get_dialog_position(); */
	GtkWindowPosition position = GTK_WIN_POS_CENTER_ON_PARENT;

	g_return_if_fail (GTK_IS_WINDOW (toplevel));
	g_return_if_fail (GTK_IS_WINDOW (window));

	gtk_window_set_transient_for (window, toplevel);

	if (position == GTK_WIN_POS_NONE)
		position = GTK_WIN_POS_CENTER_ON_PARENT;
	gtk_window_set_position (window, position);

	if (!GTK_WIDGET_MAPPED (toplevel))
		g_signal_connect_after (G_OBJECT (toplevel),
			"map",
			G_CALLBACK (cb_parent_mapped), window);
}

typedef struct {
	WorkbookControlGUI *wbcg;
	GtkWidget	   *dialog;
	char const *key;
	gboolean freed;
} KeyedDialogContext;

#if 0
static void
cb_free_keyed_dialog_context (KeyedDialogContext *ctxt)
{
	if (ctxt->freed)
		return;
	ctxt->freed = TRUE;

	/*
	 * One of these causes a recursive call which will do nothing due to
	 * ->freed.
	 */
	g_object_set_data (G_OBJECT (ctxt->wbcg), ctxt->key, NULL);
	g_object_set_data (G_OBJECT (ctxt->dialog), "KeyedDialog", NULL);
	g_free (ctxt);
}

static gint
cb_keyed_dialog_keypress (GtkWidget *dialog, GdkEventKey *event,
			  G_GNUC_UNUSED gpointer user)
{
	if (event->keyval == GDK_Escape) {
		gtk_object_destroy (GTK_OBJECT (dialog));
		return TRUE;
	}
	return FALSE;
}
#endif // 0 - unused, jsled

#if 0
/**
 * gnumeric_keyed_dialog
 *
 * @wbcg    A WorkbookControlGUI
 * @dialog  A transient window
 * @key     A key to identify the dialog
 *
 * Make dialog a transient child of wbcg, attaching to wbcg object data to
 * identify the dialog. The object data makes it possible to ensure that
 * only one dialog of a kind can be displayed for a wbcg. Deallocation of
 * the object data is managed here.
 **/
void
gnumeric_keyed_dialog (WorkbookControlGUI *wbcg, GtkWindow *dialog, const char *key)
{
	KeyedDialogContext *ctxt;

	g_return_if_fail (IS_WORKBOOK_CONTROL_GUI (wbcg));
	g_return_if_fail (GTK_IS_WINDOW (dialog));
	g_return_if_fail (key != NULL);

	wbcg_set_transient (wbcg, dialog);

	ctxt = g_new (KeyedDialogContext, 1);
	ctxt->wbcg   = wbcg;
	ctxt->dialog = GTK_WIDGET (dialog);
	ctxt->key  = key;
	ctxt->freed = FALSE;
	g_object_set_data_full (G_OBJECT (wbcg),
		key, ctxt, (GDestroyNotify) cb_free_keyed_dialog_context);
	g_object_set_data_full (G_OBJECT (dialog),
		"KeyedDialog", ctxt, (GDestroyNotify) cb_free_keyed_dialog_context);
	g_signal_connect (G_OBJECT (dialog),
		"key_press_event",
		G_CALLBACK (cb_keyed_dialog_keypress), NULL);
}
#endif // 0

/**
 * gnumeric_dialog_raise_if_exists
 *
 * @wbcg    A WorkbookControlGUI
 * @key     A key to identify the dialog
 *
 * Raise the dialog identified by key if it is registered on the wbcg.
 * Returns TRUE if dialog found, FALSE if not.
 **/
gpointer
gnumeric_dialog_raise_if_exists (WorkbookControlGUI *wbcg, const char *key)
{
	KeyedDialogContext *ctxt;

	g_return_val_if_fail (wbcg != NULL, NULL);
	g_return_val_if_fail (key != NULL, NULL);

	/* Ensure we only pop up one copy per workbook */
	ctxt = g_object_get_data (G_OBJECT (wbcg), key);
	if (ctxt && GTK_IS_WINDOW (ctxt->dialog)) {
		gdk_window_raise (ctxt->dialog->window);
		return ctxt->dialog;
	} else
		return NULL;
}

static gboolean
cb_activate_default (GtkWindow *window)
{
	/*
	 * gtk_window_activate_default has a bad habit of trying
	 * to activate the focus widget.
	 */
	return window->default_widget &&
		GTK_WIDGET_IS_SENSITIVE (window->default_widget) &&
		gtk_window_activate_default (window);
}


/**
 * gnumeric_editable_enters: Make the "activate" signal of an editable click
 * the default dialog button.
 * @window: dialog to affect.
 * @editable: Editable to affect.
 *
 * This is a literal copy of gnome_dialog_editable_enters, but not restricted
 * to GnomeDialogs.
 *
 * Normally if there's an editable widget (such as #GtkEntry) in your
 * dialog, pressing Enter will activate the editable rather than the
 * default dialog button. However, in most cases, the user expects to
 * type something in and then press enter to close the dialog. This
 * function enables that behavior.
 *
 **/
void
gnumeric_editable_enters (GtkWindow *window, GtkWidget *w)
{
	g_return_if_fail (GTK_IS_WINDOW(window));

#if 0
	/* because I really do not feel like changing all the calls to this routine */
	if (IS_GNM_EXPR_ENTRY (w))
		w = GTK_WIDGET (gnm_expr_entry_get_entry (GNM_EXPR_ENTRY (w)));
#endif // 0

	g_signal_connect_swapped (G_OBJECT (w),
		"activate",
		G_CALLBACK (cb_activate_default), window);
}

int
gtk_radio_group_get_selected (GSList *radio_group)
{
	GSList *l;
	int i, c;

	g_return_val_if_fail (radio_group != NULL, 0);

	c = g_slist_length (radio_group);

	for (i = 0, l = radio_group; l; l = l->next, i++){
		GtkRadioButton *button = l->data;

		if (GTK_TOGGLE_BUTTON (button)->active)
			return c - i - 1;
	}

	return 0;
}


int
gnumeric_glade_group_value (GladeXML *gui, const char *group[])
{
	int i;
	for (i = 0; group[i]; i++) {
		GtkWidget *w = glade_xml_get_widget (gui, group[i]);
		if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w)))
			return i;
	}
	return -1;
}

static void
kill_popup_menu (GtkWidget *widget, GtkMenu *menu)
{
	g_return_if_fail (menu != NULL);
	g_return_if_fail (GTK_IS_MENU (menu));

	g_object_unref (G_OBJECT (menu));
}

void
gnumeric_popup_menu (GtkMenu *menu, GdkEventButton *event)
{
	g_return_if_fail (menu != NULL);
	g_return_if_fail (GTK_IS_MENU (menu));

	g_object_ref (menu);
	gtk_object_sink (GTK_OBJECT (menu));

	g_signal_connect (G_OBJECT (menu),
		"hide",
		G_CALLBACK (kill_popup_menu), menu);

	/* Do NOT pass the button used to create the menu.
	 * instead pass 0.  Otherwise bringing up a menu with
	 * the right button will disable clicking on the menu with the left.
	 */
	gtk_menu_popup (menu, NULL, NULL, NULL, NULL, 0, 
			(event != NULL) ? event->time 
			: gtk_get_current_event_time());
}


GtkWidget *
gnumeric_create_tooltip (void)
{
	GtkWidget *tip, *label, *frame;
	static GtkRcStyle*rc_style = NULL;

	if (rc_style == NULL) {
		int i;
		rc_style = gtk_rc_style_new ();

		for (i = 5; --i >= 0 ; ) {
			rc_style->color_flags[i] = GTK_RC_BG;
			rc_style->bg[i] = gs_yellow;
		}
	}

	tip = gtk_window_new (GTK_WINDOW_POPUP);
	if (rc_style != NULL)
		gtk_widget_modify_style (tip, rc_style);

	frame = gtk_frame_new (NULL);
	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_OUT);
	label = gtk_label_new ("");

	gtk_container_add (GTK_CONTAINER (tip), frame);
	gtk_container_add (GTK_CONTAINER (frame), label);

	return label;
}

void
gnumeric_position_tooltip (GtkWidget *tip, int horizontal)
{
	GtkRequisition req;
	int  x, y;

	gtk_widget_size_request (tip, &req);
	gdk_window_get_pointer (NULL, &x, &y, NULL);

	if (horizontal){
		x -= req.width / 2;
		y -= req.height + 20;
	} else {
		x -= req.width + 20;
		y -= req.height / 2;
	}

	if (x < 0)
		x = 0;
	if (y < 0)
		y = 0;

	gtk_window_move (GTK_WINDOW (gtk_widget_get_toplevel (tip)), x, y);
}

/**
 * gnm_glade_xml_new :
 * @cc : #GnmCmdContext
 * @gladefile :
 *
 * Simple utility to open glade files
 **/
GladeXML *
gnm_glade_xml_new (GnmCmdContext *cc, char const *gladefile,
		   char const *root, char const *domain)
{
	GladeXML *gui;
	char *f;

	g_return_val_if_fail (gladefile != NULL, NULL);

	if (!g_path_is_absolute (gladefile)) {
		char *d = gnm_sys_glade_dir ();
		f = g_build_filename (d, gladefile, NULL);
		g_free (d);
	} else
		f = g_strdup (gladefile);

	gui = glade_xml_new (f, root, domain);
	if (gui == NULL && cc != NULL) {
		char *msg = g_strdup_printf (_("Unable to open file '%s'"), f);
		gnm_cmd_context_error_system (cc, msg);
		g_free (msg);
	}
	g_free (f);

	return gui;
}

static gint
cb_non_modal_dialog_keypress (GtkWidget *w, GdkEventKey *e)
{
	if(e->keyval == GDK_Escape) {
		gtk_widget_destroy (w);
		return TRUE;
	}

	return FALSE;
}

void
gnumeric_non_modal_dialog (GtkWindow *toplevel, GtkWindow *dialog)
{
	gnumeric_set_transient (toplevel, dialog);
	g_signal_connect (G_OBJECT (dialog),
		"key-press-event",
		G_CALLBACK (cb_non_modal_dialog_keypress), NULL);
}

static void
popup_item_activate (GtkWidget *item, gpointer *user_data)
{
	GnumericPopupMenuElement const *elem =
		g_object_get_data (G_OBJECT (item), "descriptor");
	GnumericPopupMenuHandler handler =
		g_object_get_data (G_OBJECT (item), "handler");

	g_return_if_fail (elem != NULL);
	g_return_if_fail (handler != NULL);

	if (handler (elem, user_data))
		gtk_widget_destroy (gtk_widget_get_toplevel (item));
}

static void
gnumeric_create_popup_menu_list (GSList *elements,
				 GnumericPopupMenuHandler handler,
				 gpointer user_data,
				 int display_filter,
				 int sensitive_filter,
				 GdkEventButton *event)
{
	GtkWidget *menu, *item;
	char const *trans;

	menu = gtk_menu_new ();

	for (; elements != NULL ; elements = elements->next) {
		GnumericPopupMenuElement const *element = elements->data;
		char const * const name = element->name;
		char const * const pix_name = element->pixmap;

		item = NULL;

		if (element->display_filter != 0 &&
		    !(element->display_filter & display_filter))
			continue;

		if (name != NULL && *name != '\0') {
			trans = _(name);
			item = gtk_image_menu_item_new_with_mnemonic (trans);
			if (element->sensitive_filter != 0 &&
			    (element->sensitive_filter & sensitive_filter))
				gtk_widget_set_sensitive (GTK_WIDGET (item), FALSE);
			if (pix_name != NULL) {
				GtkWidget *image = gtk_image_new_from_stock (pix_name,
                                        GTK_ICON_SIZE_MENU);
				gtk_widget_show (image);
				gtk_image_menu_item_set_image (
					GTK_IMAGE_MENU_ITEM (item),
					image);
			}
		} else {
			/* separator */
			item = gtk_menu_item_new ();
			gtk_widget_set_sensitive (item, FALSE);
		}

		if (element->index != 0) {
			g_signal_connect (G_OBJECT (item),
				"activate",
				G_CALLBACK (&popup_item_activate), user_data);
			g_object_set_data (
				G_OBJECT (item), "descriptor", (gpointer)(element));
			g_object_set_data (
				G_OBJECT (item), "handler", (gpointer)handler);
		}

		gtk_widget_show (item);
		gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
	}

	gnumeric_popup_menu (GTK_MENU (menu), event);
}

void
gnumeric_create_popup_menu (GnumericPopupMenuElement const *elements,
			    GnumericPopupMenuHandler handler,
			    gpointer user_data,
			    int display_filter, int sensitive_filter,
			    GdkEventButton *event)
{
	int i;
	GSList *tmp = NULL;

	for (i = 0; elements [i].name != NULL; i++)
		tmp = g_slist_prepend (tmp, (gpointer)(elements + i));

	tmp = g_slist_reverse (tmp);
	gnumeric_create_popup_menu_list (tmp, handler, user_data,
		display_filter, sensitive_filter, event);
	g_slist_free (tmp);
}

/**
 * go_combo_color_get_style_color :
 *
 * A utility wrapper to map between gal's colour combo and gnumeric's StyleColors.
 */
GnmColor *
go_combo_color_get_style_color (GtkWidget *go_combo_color)
{
	GnmColor *sc = NULL;
	guint16   r, g, b;
	GOColor color = go_combo_color_get_color (GO_COMBO_COLOR (go_combo_color), NULL);
	if (UINT_RGBA_A (color) >= 0x80) {
		r  = UINT_RGBA_R (color); r |= (r << 8);
		g  = UINT_RGBA_G (color); g |= (g << 8);
		b  = UINT_RGBA_B (color); b |= (b << 8);
		sc = style_color_new (r, g, b);
	}
	return sc;
}

#ifdef WITH_GNOME
#include <libgnome/gnome-help.h>
#endif
void
gnumeric_help_display (char const *link)
{
        g_return_if_fail (link != NULL);
#ifdef WITH_GNOME
	gnome_help_display ("gnumeric", link, NULL);
#else
	g_warning ("TODO : launch help browser for %s", link);
#endif
}

static void
cb_help (GtkWidget *button, char const *link)
{
	gnumeric_help_display (link);
}

void
gnumeric_init_help_button (GtkWidget *w, char const *link)
{
	GtkWidget *parent = gtk_widget_get_parent (w);
	if (GTK_IS_BUTTON_BOX (parent))
		gtk_button_box_set_child_secondary (GTK_BUTTON_BOX (parent),
						    w, TRUE);

	g_signal_connect (G_OBJECT (w),
		"clicked",
		G_CALLBACK (cb_help), (gpointer) link);
}

static void
gnumeric_help_pbox_goto (void *ignore, int ignore2, char const *link)
{
	gnumeric_help_display (link);
}

void
gnumeric_pbox_init_help (GtkWidget *dialog, char const *link)
{
	g_signal_connect (G_OBJECT (dialog),
		"help",
		G_CALLBACK (gnumeric_help_pbox_goto), (gpointer)link);
}

char *
gnumeric_textview_get_text (GtkTextView *text_view)
{
	GtkTextIter    start, end;
	GtkTextBuffer *buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text_view));

	g_return_val_if_fail (buf != NULL, NULL);

	gtk_text_buffer_get_start_iter (buf, &start);
	gtk_text_buffer_get_end_iter (buf, &end);
	return gtk_text_buffer_get_text (buf, &start, &end, FALSE);
}

void
gnumeric_textview_set_text (GtkTextView *text_view, char const *txt)
{
	gtk_text_buffer_set_text (
		gtk_text_view_get_buffer (GTK_TEXT_VIEW (text_view)),
		txt, -1);
}

void
focus_on_entry (GtkEntry *entry)
{
	if (entry == NULL)
		return;
	gtk_widget_grab_focus (GTK_WIDGET(entry));
	gtk_editable_set_position (GTK_EDITABLE (entry), 0);
	gtk_editable_select_region (GTK_EDITABLE (entry), 0, entry->text_length);
}

gboolean
entry_to_float_with_format_default (GtkEntry *entry, gnm_float *the_float, gboolean update,
				    GnmFormat *format, gnm_float num)
{
	char const *text = gtk_entry_get_text (entry);
	gboolean need_default = (text == NULL);

	if (!need_default) {
		char *new_text = g_strdup (text);
		need_default = (0 ==  strlen (g_strstrip(new_text)));
		g_free (new_text);
	}

	if (need_default && !update) {
		*the_float = num;
		return FALSE;
	}

	if (need_default)
		float_to_entry (entry, num);

	return entry_to_float_with_format (entry, the_float, update, format);
}

gboolean
entry_to_float_with_format (GtkEntry *entry, gnm_float *the_float, gboolean update,
			    GnmFormat *format)
{
	GnmValue *value = format_match_number (gtk_entry_get_text (entry), format, NULL);

	*the_float = 0.0;
	if (!value)
		return TRUE;

	if (!VALUE_IS_NUMBER (value)) {
		value_release (value);
		return TRUE;
	}

	*the_float = value_get_as_float (value);
	if (update) {
		char *tmp = format_value (format, value, NULL, 16, NULL);
		gtk_entry_set_text (entry, tmp);
		g_free (tmp);
	}

	value_release (value);
	return FALSE;
}

/**
 * entry_to_int:
 * @entry:
 * @the_int:
 * @update:
 *
 * retrieve an int from an entry field parsing all reasonable formats
 *
 **/
gboolean
entry_to_int (GtkEntry *entry, gint *the_int, gboolean update)
{
	GnmValue *value = format_match_number (gtk_entry_get_text (entry), NULL, NULL);

	*the_int = 0;
	if (!value)
		return TRUE;

	if (value->type != VALUE_INTEGER) {
		value_release (value);
		return TRUE;
	}

	*the_int = value_get_as_int (value);
	if (update) {
		char *tmp = format_value (NULL, value, NULL, 16, NULL);
		gtk_entry_set_text (entry, tmp);
		g_free (tmp);
	}

	value_release (value);
	return FALSE;
}

/**
 * float_to_entry:
 * @entry:
 * @the_float:
 *
 **/
void
float_to_entry (GtkEntry *entry, gnm_float the_float)
{
	GnmValue *val = value_new_float (the_float);
	char *text = format_value (NULL, val, NULL, 16, NULL);
	value_release(val);
	if (text != NULL) {
		gtk_entry_set_text (entry, text);
		g_free (text);
	}
}

/**
 * int_to_entry:
 * @entry:
 * @the_float:
 *
 *
  **/
void
int_to_entry (GtkEntry *entry, gint the_int)
{
	GnmValue *val  = value_new_int (the_int);
	char *text = format_value (NULL, val, NULL, 16, NULL);
	value_release(val);
	if (text != NULL) {
		gtk_entry_set_text (entry, text);
		g_free (text);
	}
}

char *
gnumeric_icondir (char const *filename)
{
	//return g_build_filename (gnumeric_icon_dir, filename, NULL);
	return g_build_filename( "FIXME-icondir", filename, NULL );
}

GtkWidget *
gnumeric_load_image (char const *filename)
{
	char *path = gnumeric_icondir (filename);
	GtkWidget *image = gtk_image_new_from_file (path);
	g_free (path);

	if (image)
		gtk_widget_show (image);

	return image;
}

/**
 * gnumeric_load_pixbuf : utility routine to create pixbufs from file named @name.
 * looking in the gnumeric icondir.
 **/
GdkPixbuf *
gnumeric_load_pixbuf (char const *filename)
{
	char *path = gnumeric_icondir (filename);
	GdkPixbuf *pixbuf = gdk_pixbuf_new_from_file (path, NULL);
	g_free (path);
	return pixbuf;
}


/**
 * gnm_pixbuf_tile: created a pixbuf consistent of the source pixbuf tiled
 * enough times to fill out the space needed.
 *
 * The fractional tiles are spead evenly left-right and top-bottom.
 */
GdkPixbuf *
gnm_pixbuf_tile (const GdkPixbuf *src, int w, int h)
{
	int src_w = gdk_pixbuf_get_width (src);
	int src_h = gdk_pixbuf_get_height (src);

	int tile_x = w / src_w;  /* Number of full tiles  */
	int tile_y = h / src_h;

	int left_x = w - tile_x * src_w;
	int left_y = h - tile_y * src_h;

	int dst_y = 0;
	int stripe_y;
	GdkPixbuf *dst = gdk_pixbuf_new (gdk_pixbuf_get_colorspace (src),
					 gdk_pixbuf_get_has_alpha (src),
					 gdk_pixbuf_get_bits_per_sample (src),
					 w, h);

	for (stripe_y = -1; stripe_y <= tile_y; stripe_y++) {
		int dst_x = 0;
		int stripe_x;
		int this_h, start_y = 0;

		if (stripe_y == -1) {
			this_h = (left_y + 1) / 2;
			start_y = src_h - this_h;
		} else if (stripe_y == tile_y)
			this_h = left_y / 2;
		else
			this_h = src_h;

		if (this_h == 0)
			continue;

		for (stripe_x = -1; stripe_x <= tile_x; stripe_x++) {
			int this_w, start_x = 0;

			if (stripe_x == -1) {
				this_w = (left_x + 1) / 2;
				start_x = src_w - this_w;
			} else if (stripe_x == tile_x)
				this_w = left_x / 2;
			else
				this_w = src_w;

			if (this_w == 0)
				continue;

			gdk_pixbuf_copy_area (src, start_x, start_y,
					      this_w, this_h,
					      dst,
					      dst_x, dst_y);

			dst_x += this_w;
		}

		dst_y += this_h;
	}

	return dst;
}


static void
add_atk_relation (GtkWidget *w0, GtkWidget *w1, AtkRelationType type)
{
	AtkObject *atk0 = gtk_widget_get_accessible(w0);
	AtkObject *atk1 = gtk_widget_get_accessible(w1);
	AtkRelationSet *relation_set = atk_object_ref_relation_set (atk0);
	AtkRelation *relation = atk_relation_new (&atk1, 1, type);
	atk_relation_set_add (relation_set, relation);
	g_object_unref (relation_set);
	g_object_unref (relation);
}

/**
 * gnm_setup_label_atk :
 * @label : #GtkWidget
 * @target : #GtkWidget
 *
 * A convenience routine to setup label-for/labeled-by relationship between a
 * pair of widgets
 **/
void
gnm_setup_label_atk (GtkWidget *label, GtkWidget *target)
{
	 add_atk_relation (label, target, ATK_RELATION_LABEL_FOR);
	 add_atk_relation (target, label, ATK_RELATION_LABELLED_BY);
}


int
gnm_measure_string (PangoContext *context, const PangoFontDescription *font_desc, const char *str)
{
	PangoLayout *layout = pango_layout_new (context);
	int width;

	pango_layout_set_text (layout, str, -1);
	pango_layout_set_font_description (layout, font_desc);
	pango_layout_get_pixel_size (layout, &width, NULL);

	g_object_unref (layout);

	return width;
}

static void
cb_focus_to_entry (GtkWidget *button, GtkWidget *entry)
{
	if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (button)))
		gtk_widget_grab_focus (entry);
}

static gboolean
cb_activate_button (GtkWidget *button)
{
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
	return FALSE;
}

void
gnm_link_button_and_entry (GtkWidget *button, GtkWidget *entry)
{
	g_signal_connect (G_OBJECT (button),
			  "clicked", G_CALLBACK (cb_focus_to_entry),
			  entry);
	g_signal_connect_swapped (G_OBJECT (entry),
			  "focus_in_event",
			  G_CALLBACK (cb_activate_button),
			  button);
}

/* ------------------------------------------------------------------------- */

void
gnm_widget_set_cursor (GtkWidget *w, GdkCursor *cursor)
{
	gdk_window_set_cursor (w->window, cursor);
}

void
gnm_widget_set_cursor_type (GtkWidget *w, GdkCursorType ct)
{
	GdkDisplay *display = gdk_drawable_get_display (w->window);
	GdkCursor *cursor = gdk_cursor_new_for_display (display, ct);
	gnm_widget_set_cursor (w, cursor);
	gdk_cursor_unref (cursor);
}

GdkCursor *
gnm_fat_cross_cursor (GdkDisplay *display)
{
	/* We don't actually own a ref, but that's ok.  */
	static GdkPixbuf *pixbuf = NULL;

	if (!pixbuf)
		pixbuf = gnm_app_get_pixbuf ("cursor_cross");

	return gdk_cursor_new_from_pixbuf (display, pixbuf, 17, 17);
}

/* ------------------------------------------------------------------------- */

/**
 * gnumeric_button_new_with_stock_image
 *
 * Code from gedit
 *
 * Creates a new GtkButton with custom label and stock image.
 * 
 * text : button label
 * sotck_id : id for stock icon
 *
 * return : newly created button
 *
 **/

GtkWidget* 
gnumeric_button_new_with_stock_image (const gchar* text, const gchar* stock_id)
{
	GtkWidget *button;
	GtkStockItem item;
	GtkWidget *label;
	GtkWidget *image;
	GtkWidget *hbox;
	GtkWidget *align;

	button = gtk_button_new ();

	if (GTK_BIN (button)->child)
		gtk_container_remove (GTK_CONTAINER (button),
				      GTK_BIN (button)->child);

	if (gtk_stock_lookup (stock_id, &item)) {
		label = gtk_label_new_with_mnemonic (text);

		gtk_label_set_mnemonic_widget (GTK_LABEL (label), GTK_WIDGET (button));

		image = gtk_image_new_from_stock (stock_id, GTK_ICON_SIZE_BUTTON);
		hbox = gtk_hbox_new (FALSE, 2);

		align = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);

		gtk_box_pack_start (GTK_BOX (hbox), image, FALSE, FALSE, 0);
		gtk_box_pack_end (GTK_BOX (hbox), label, FALSE, FALSE, 0);

		gtk_container_add (GTK_CONTAINER (button), align);
		gtk_container_add (GTK_CONTAINER (align), hbox);
		gtk_widget_show_all (align);

		return button;
	}

	label = gtk_label_new_with_mnemonic (text);
	gtk_label_set_mnemonic_widget (GTK_LABEL (label), GTK_WIDGET (button));

	gtk_misc_set_alignment (GTK_MISC (label), 0.5, 0.5);

	gtk_widget_show (label);
	gtk_container_add (GTK_CONTAINER (button), label);

	return button;
}

/**
 * gnumeric_dialog_add_button
 *
 * Code from gedit
 *
 * Creates and adds a button with stock image to the action area of an existing dialog.
 * 
 * dialog : dialog you want to add a button
 * text : button label
 * sotck_id : stock icon id
 * response_id : respond id when button clicked
 *
 * return : newly created button
 *
 **/

GtkWidget*
gnumeric_dialog_add_button (GtkDialog *dialog, const gchar* text, const gchar* stock_id,
			    gint response_id)
{
	GtkWidget *button;

	g_return_val_if_fail (GTK_IS_DIALOG (dialog), NULL);
	g_return_val_if_fail (text != NULL, NULL);
	g_return_val_if_fail (stock_id != NULL, NULL);

	button = gnumeric_button_new_with_stock_image (text, stock_id);
	g_return_val_if_fail (button != NULL, NULL);

	GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);

	gtk_widget_show (button);

	gtk_dialog_add_action_widget (dialog, button, response_id);	

	return button;
}

/**
 * gnumeric_message_dialog_new :
 *
 * A convenience fonction to build HIG compliant message dialogs.
 *
 *   parent : transient parent, or NULL for none.
 *   flags 
 *   type : type of dialog
 *   primary_message : message displayed in bold
 *   secondary_message : message displayed below
 *
 *   return : a GtkDialog, without buttons.
 **/

GtkWidget *
gnumeric_message_dialog_new (GtkWindow * parent,
			     GtkDialogFlags flags,
			     GtkMessageType type,
			     gchar const * primary_message,
			     gchar const * secondary_message)
{
	GtkWidget * dialog;
	GtkWidget * label;
	GtkWidget * image;
	GtkWidget * hbox;
	gchar * message;
	const gchar *stock_id = NULL;
	GtkStockItem item;

	dialog = gtk_dialog_new_with_buttons ("", parent, flags, NULL);

	if (dialog) {
		image = gtk_image_new (); 

		switch (type) {
		case GTK_MESSAGE_INFO:
			stock_id = GTK_STOCK_DIALOG_INFO;
			break;

		case GTK_MESSAGE_QUESTION:
			stock_id = GTK_STOCK_DIALOG_QUESTION;
			break;

		case GTK_MESSAGE_WARNING:
			stock_id = GTK_STOCK_DIALOG_WARNING;
			break;

		case GTK_MESSAGE_ERROR:
			stock_id = GTK_STOCK_DIALOG_ERROR;
			break;

		default:
			g_warning ("Unknown GtkMessageType %d", type);
			break;
		}

		if (stock_id == NULL)
			stock_id = GTK_STOCK_DIALOG_INFO;

		if (gtk_stock_lookup (stock_id, &item)) {
			gtk_image_set_from_stock (GTK_IMAGE (image), stock_id,
						  GTK_ICON_SIZE_DIALOG);

			gtk_window_set_title (GTK_WINDOW (dialog), item.label);
		} else
			g_warning ("Stock dialog ID doesn't exist?");  

		if (primary_message) {
			if (secondary_message) {
				message = g_strdup_printf ("<b>%s</b>\n\n%s",
							   primary_message,
							   secondary_message);
			} else {
				message = g_strdup_printf ("<b>%s</b>",
							   primary_message);
			}
		} else {
			message = g_strdup_printf (secondary_message);
		}
		label = gtk_label_new (message);
		g_free (message);

		hbox = gtk_hbox_new (FALSE, 0);
		gtk_box_pack_start (GTK_BOX (hbox), image, FALSE, TRUE, 0);
		gtk_box_pack_start_defaults (GTK_BOX (hbox),
					     label);
		gtk_box_pack_start_defaults (GTK_BOX (GTK_DIALOG (dialog)->vbox),
					     hbox);

		gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
		gtk_label_set_line_wrap (GTK_LABEL (label), TRUE);
		gtk_misc_set_alignment (GTK_MISC (label), 0.0 , 0.0);
		gtk_misc_set_alignment (GTK_MISC (label), 0.0 , 0.0);
		gtk_box_set_spacing (GTK_BOX (hbox), 12);
		gtk_container_set_border_width (GTK_CONTAINER (hbox), 6);
		gtk_box_set_spacing (GTK_BOX (GTK_DIALOG (dialog)->vbox), 12);
		gtk_container_set_border_width (GTK_CONTAINER (dialog), 6);
		gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);
		gtk_window_set_resizable (GTK_WINDOW(dialog), FALSE);
		gtk_widget_show_all (GTK_WIDGET (GTK_DIALOG (dialog)->vbox));
	}

	return dialog;
}

GdkPixbuf*
gnm_pixbuf_intelligent_scale (GdkPixbuf *buf, guint width, guint height)
{
	GdkPixbuf *scaled;
	int w, h;
	unsigned long int ow = gdk_pixbuf_get_width (buf);
	unsigned long int oh = gdk_pixbuf_get_height (buf);

	if (ow <= width && oh <= height)
		scaled = g_object_ref (buf);
	else
	{
		if (ow * height > oh * width)
		{
			w = width;
			h = width * (((double)oh)/(double)ow);
		}
		else
		{
			h = height;
			w = height * (((double)ow)/(double)oh);
		}
			
		scaled = gdk_pixbuf_scale_simple (buf, w, h, GDK_INTERP_BILINEAR);
	}
	
	return scaled;
}

void
gnm_widget_disable_focus (GtkWidget *w)
{
	if (GTK_IS_CONTAINER (w))
		gtk_container_foreach (GTK_CONTAINER (w),
			(GtkCallback) gnm_widget_disable_focus, NULL);
	GTK_WIDGET_UNSET_FLAGS (w, GTK_CAN_FOCUS);
}


gboolean 
gnm_tree_model_iter_prev (GtkTreeModel *model, GtkTreeIter* iter)
{
	GtkTreePath *path = gtk_tree_model_get_path (model, iter);

	if (gtk_tree_path_prev (path) &&
	    gtk_tree_model_get_iter (model, iter, path)) {
		gtk_tree_path_free (path);
		return TRUE;
	}
	gtk_tree_path_free (path);
	return FALSE;
}
