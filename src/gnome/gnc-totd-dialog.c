/*
 * gnc-totd-dialog.c : dialog to display a "tip of the day"
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */ 

#include "config.h"

#include <glade/glade.h>
#include <gtk/gtkdialog.h>
#include <gtk/gtkcheckbutton.h>

#include "gnc-totd-dialog.h"
#include "dialog-utils.h"
#include "global-options.h"
#include "tip-of-the-day.h"

#include "messages.h"

static void gnc_totd_dialog_class_init (GncTotdDialogClass *klass);
static void gnc_totd_dialog_init (GncTotdDialog *dialog);
static void gnc_totd_dialog_dispose (GObject *object);
static void gnc_totd_dialog_finalize (GObject *object);

static void gnc_totd_dialog_next_activated (GtkButton *button, GncTotdDialog *dialog);
static void gnc_totd_dialog_previous_activated (GtkButton *button, GncTotdDialog *dialog);
static void gnc_totd_dialog_button_toggled (GtkToggleButton *button, GncTotdDialog *dialog);

#define GNC_RESPONSE_NEXT 0
#define GNC_RESPONSE_PREVIOUS 1

struct GncTotdDialogPrivate {
	GtkCheckButton *show_checkbutton;
	GtkTextView *tip_textview;
};

static GtkDialogClass *parent_class = NULL;

GType
gnc_totd_dialog_get_type (void)
{
	static GType gnc_totd_dialog_type = 0;

	if (gnc_totd_dialog_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncTotdDialogClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_totd_dialog_class_init,
			NULL,
			NULL,
			sizeof (GncTotdDialog),
			0,
			(GInstanceInitFunc) gnc_totd_dialog_init
		};
		
		gnc_totd_dialog_type = g_type_register_static (GTK_TYPE_DIALOG,
							       "GncTotdDialog",
							       &our_info, 0);
	}

	return gnc_totd_dialog_type;
}

GtkWidget *
gnc_totd_dialog_new (GtkWindow *parent)
{
	GncTotdDialog *dialog;

	dialog = g_object_new (GNC_TYPE_TOTD_DIALOG, NULL);

	if (parent != NULL) {
		gtk_window_set_transient_for (GTK_WINDOW (dialog), parent);
	}

	return GTK_WIDGET (dialog);
}

void
gnc_totd_dialog_create_and_run (void)
{
	GtkWidget *dialog;

	dialog = gnc_totd_dialog_new (NULL);
	gtk_dialog_run (GTK_DIALOG (dialog));
	gtk_widget_destroy (dialog);
}

static void
gnc_totd_dialog_class_init (GncTotdDialogClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_totd_dialog_finalize;
	object_class->dispose = gnc_totd_dialog_dispose;
}

static GtkWidget *
gnc_totd_dialog_create_button (const gchar *image_id, const gchar *text)
{
	GtkWidget *image;
	GtkWidget *label;
	GtkWidget *hbox;
	GtkWidget *alignment;
	GtkWidget *button;

	image = gtk_image_new_from_stock (image_id, GTK_ICON_SIZE_BUTTON);
	gtk_widget_show (image);
	label = gtk_label_new_with_mnemonic (text);
	gtk_widget_show (label);

	hbox = gtk_hbox_new (FALSE, 2);
	gtk_box_pack_start (GTK_BOX (hbox), image, FALSE, FALSE, 0);
	gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);
	gtk_widget_show (hbox);

	alignment = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
	gtk_container_add (GTK_CONTAINER (alignment), hbox);
	gtk_widget_show (alignment);
	
	button = gtk_button_new ();
	gtk_container_add (GTK_CONTAINER (button), alignment);
	gtk_widget_show (button);
	
	return button;
}

static void
gnc_totd_dialog_init (GncTotdDialog *dialog)
{
	GladeXML *xml;
	gchar *new_tip;
	GtkWidget *button;

	dialog->priv = g_new0 (GncTotdDialogPrivate, 1);

	button = gnc_totd_dialog_create_button (GTK_STOCK_GO_BACK, _("_Previous Tip"));
	g_signal_connect (G_OBJECT (button), "clicked",
			  G_CALLBACK (gnc_totd_dialog_previous_activated), dialog);
	gtk_container_add (GTK_CONTAINER (GTK_DIALOG(dialog)->action_area), button);

	button = gnc_totd_dialog_create_button (GTK_STOCK_GO_FORWARD, _("_Next Tip"));
	g_signal_connect (G_OBJECT (button), "clicked",
			  G_CALLBACK (gnc_totd_dialog_next_activated), dialog);
	gtk_container_add (GTK_CONTAINER (GTK_DIALOG(dialog)->action_area), button);

	gtk_dialog_add_button (GTK_DIALOG (dialog), GTK_STOCK_CLOSE,
			       GTK_RESPONSE_CLOSE);

	xml = gnc_glade_xml_new ("totd.glade", "totd_vbox");

	gtk_container_add (GTK_CONTAINER (GTK_DIALOG(dialog)->vbox),
			   glade_xml_get_widget (xml, "totd_vbox"));

	dialog->priv->show_checkbutton = GTK_CHECK_BUTTON (glade_xml_get_widget (xml, "show_checkbutton"));
	g_signal_connect (G_OBJECT (dialog->priv->show_checkbutton), "toggled",
			  G_CALLBACK (gnc_totd_dialog_button_toggled), dialog);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (dialog->priv->show_checkbutton),
				      gnc_lookup_boolean_option("General", 
								"Display \"Tip of the Day\"",
								TRUE));

	dialog->priv->tip_textview = GTK_TEXT_VIEW (glade_xml_get_widget (xml, "tip_textview"));
	new_tip = gnc_get_current_tip();
	gtk_text_buffer_set_text (gtk_text_view_get_buffer (dialog->priv->tip_textview),
				  new_tip, -1);
	g_free (new_tip);

	g_object_unref (xml);

	gtk_widget_set_size_request (GTK_WIDGET (dialog), -1, -1);
}

static void
gnc_totd_dialog_finalize (GObject *object)
{
	g_free (GNC_TOTD_DIALOG (object)->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
gnc_totd_dialog_dispose (GObject *object)
{
	G_OBJECT_CLASS (parent_class)->dispose (object);
}

static void
gnc_totd_dialog_next_activated (GtkButton *button, GncTotdDialog *dialog)
{
	gchar *new_tip;
	
	gnc_increment_tip ();
	new_tip = gnc_get_current_tip();
	gtk_text_buffer_set_text (gtk_text_view_get_buffer (dialog->priv->tip_textview),
				  new_tip, -1);
	g_free (new_tip);
}

static void
gnc_totd_dialog_previous_activated (GtkButton *button, GncTotdDialog *dialog)
{
	gchar *new_tip;
	
	gnc_decrement_tip ();
	new_tip = gnc_get_current_tip();
	gtk_text_buffer_set_text (gtk_text_view_get_buffer (dialog->priv->tip_textview),
				  new_tip, -1);
	g_free (new_tip);
}

static void
gnc_totd_dialog_button_toggled (GtkToggleButton *button, GncTotdDialog *dialog)
{
	gnc_set_boolean_option("General",
			       "Display \"Tip of the Day\"",
			       gtk_toggle_button_get_active (button));
	gnc_option_refresh_ui_by_name("General", "Display \"Tip of the Day\"");
}
