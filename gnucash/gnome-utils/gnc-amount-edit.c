/********************************************************************\
 * gnc-amount-edit.h -- amount editor widget                        *
 *                                                                  *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/
/*
  @NOTATION@
 */

#include <config.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "gnc-amount-edit.h"
#include "gnc-exp-parser.h"
#include "gnc-locale-utils.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "dialog-utils.h"
#include "gnc-ui.h"

#ifdef G_OS_WIN32
# include <gdk/gdkwin32.h>
#endif

/* Signal codes */
enum
{
    AMOUNT_CHANGED,
    LAST_SIGNAL
};

static guint amount_edit_signals [LAST_SIGNAL] = { 0 };

static void gnc_amount_edit_init (GNCAmountEdit *gae);
static void gnc_amount_edit_class_init (GNCAmountEditClass *klass);
static void gnc_amount_edit_changed (GtkEditable *gae,
                                     gpointer user_data);
static void gnc_amount_edit_paste_clipboard (GNCAmountEdit *gae,
                                             gpointer user_data);
static gint gnc_amount_edit_key_press (GtkWidget   *widget,
                                       GdkEventKey *event);

static GtkEntryClass *parent_class;

GType
gnc_amount_edit_get_type (void)
{
    static GType amount_edit_type = 0;

    if (amount_edit_type == 0)
    {
        GTypeInfo amount_edit_info =
        {
            sizeof (GNCAmountEditClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_amount_edit_class_init,
            NULL,
            NULL,
            sizeof (GNCAmountEdit),
            0,
            (GInstanceInitFunc) gnc_amount_edit_init
        };

        amount_edit_type = g_type_register_static (GTK_TYPE_ENTRY,
                                                   "GNCAmountEdit",
                                                    &amount_edit_info,
                                                    0);
    }
    return amount_edit_type;
}

static void
gnc_amount_edit_class_init (GNCAmountEditClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    GtkWidgetClass *widget_class = GTK_WIDGET_CLASS(klass);
 /* GtkEditableClass *editable_class = GTK_EDITABLE_CLASS(g_type_interface_peek
                                                         (klass, GTK_TYPE_EDITABLE)); */

    parent_class = g_type_class_peek_parent (klass);

    amount_edit_signals [AMOUNT_CHANGED] =
        g_signal_new ("amount_changed",
                      G_OBJECT_CLASS_TYPE(object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET(GNCAmountEditClass, amount_changed),
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);

    widget_class->key_press_event = gnc_amount_edit_key_press;

 /* editable_class->changed = gnc_amount_edit_changed; */
}

static void
gnc_amount_edit_init (GNCAmountEdit *gae)
{
    gae->need_to_parse = FALSE;
    gae->amount = gnc_numeric_zero ();
    gae->print_info = gnc_default_print_info (FALSE);
    gae->fraction = 0;
    gae->evaluate_on_enter = FALSE;
    gae->block_changed = FALSE;

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(gae), "gnc-id-amount-edit");

    g_signal_connect (G_OBJECT(gae), "changed",
                      G_CALLBACK(gnc_amount_edit_changed), gae);

    g_signal_connect (G_OBJECT(gae), "paste-clipboard",
                      G_CALLBACK(gnc_amount_edit_paste_clipboard), NULL);
}

static void
gnc_amount_edit_changed (GtkEditable *editable, gpointer user_data)
{
    GNCAmountEdit *gae = GNC_AMOUNT_EDIT(user_data);
     /*GTK_EDITABLE_CLASS(parent_class)->changed(editable);*/
    if (gae->block_changed)
        g_signal_stop_emission_by_name (G_OBJECT(gae), "changed");

    GNC_AMOUNT_EDIT(editable)->need_to_parse = TRUE;
}

static void
gnc_amount_edit_paste_clipboard (GNCAmountEdit *gae, gpointer user_data)
{
    GtkClipboard *clipboard = gtk_widget_get_clipboard (GTK_WIDGET(gae),
                                                        GDK_SELECTION_CLIPBOARD);
    gchar *text = gtk_clipboard_wait_for_text (clipboard);

    if (text)
    {
        gchar *filtered_text = gnc_filter_text_for_control_chars (text);
        gchar *existing = gtk_editable_get_chars (GTK_EDITABLE(gae), 0, -1);
        gint start_pos, end_pos;
        gint position;

        if (!filtered_text)
        {
            g_free (text);
            g_free (existing);
            return;
        }

        position = gtk_editable_get_position (GTK_EDITABLE(gae));

        if (gtk_editable_get_selection_bounds (GTK_EDITABLE(gae),
                                               &start_pos, &end_pos))
        {
            gint old_len = g_utf8_strlen (existing, -1);
            gchar *begin = g_utf8_substring (existing, 0, start_pos);
            gchar *end = g_utf8_substring (existing, end_pos, old_len);
            gchar *changed_text = g_strdup_printf ("%s%s%s", begin, filtered_text, end);

            gae->block_changed = TRUE;
            gtk_editable_delete_text (GTK_EDITABLE(gae), 0, -1);
            gae->block_changed = FALSE;

            gtk_editable_insert_text (GTK_EDITABLE(gae),
                                      changed_text, -1, &position);

            position = start_pos + g_utf8_strlen (filtered_text, -1);

            g_free (begin);
            g_free (end);
            g_free (changed_text);
        }
        else
            gtk_editable_insert_text (GTK_EDITABLE(gae),
                                      filtered_text, -1, &position);

        gtk_editable_set_position (GTK_EDITABLE(gae), position);

        g_signal_stop_emission_by_name (G_OBJECT(gae), "paste-clipboard");

        g_free (text);
        g_free (existing);
        g_free (filtered_text);
    }
}

static gint
gnc_amount_edit_key_press (GtkWidget *widget, GdkEventKey *event)
{
    GNCAmountEdit *gae = GNC_AMOUNT_EDIT(widget);
    gint result;

#ifdef G_OS_WIN32
    /* gdk never sends GDK_KEY_KP_Decimal on win32. See #486658 */
    if (event->hardware_keycode == VK_DECIMAL)
        event->keyval = GDK_KEY_KP_Decimal;
#endif
    if (event->keyval == GDK_KEY_KP_Decimal)
    {
        if (gae->print_info.monetary)
        {
            struct lconv *lc = gnc_localeconv ();
            event->keyval = lc->mon_decimal_point[0];
            event->string[0] = lc->mon_decimal_point[0];
        }
    }

    result = (* GTK_WIDGET_CLASS(parent_class)->key_press_event)(widget, event);

    switch (event->keyval)
    {
    case GDK_KEY_Return:
        if (gae->evaluate_on_enter)
            break;
        if (event->state & (GDK_MODIFIER_INTENT_DEFAULT_MOD_MASK))
            break;
        return result;
    case GDK_KEY_KP_Enter:
        break;
    default:
        return result;
    }

    gnc_amount_edit_evaluate (gae);

    return TRUE;
}

GtkWidget *
gnc_amount_edit_new (void)
{
    GNCAmountEdit *gae;

    gae = g_object_new (GNC_TYPE_AMOUNT_EDIT, NULL);
    gtk_widget_show (GTK_WIDGET(gae));

    return GTK_WIDGET(gae);
}

gint
gnc_amount_edit_expr_is_valid (GNCAmountEdit *gae, gnc_numeric *amount,
                               gboolean empty_ok)
{
    const char *string;
    char *error_loc;
    gboolean ok;

    g_return_val_if_fail (gae != NULL, -1);
    g_return_val_if_fail (GNC_IS_AMOUNT_EDIT(gae), -1);

    string = gtk_entry_get_text (GTK_ENTRY(gae));
    if (!string || *string == '\0')
    {
        *amount = gnc_numeric_zero ();
        if (empty_ok)
            return -1; /* indicate an empty field */
        else
            return 0; /* indicate successfully parsed as 0 */
    }

    error_loc = NULL;
    ok = gnc_exp_parser_parse (string, amount, &error_loc);

    if (ok)
        return 0;

    /* Not ok */
    if (error_loc != NULL)
        return  error_loc - string;
    else
        return 1;
}

gboolean
gnc_amount_edit_evaluate (GNCAmountEdit *gae)
{
    gint result;
    gnc_numeric amount;

    g_return_val_if_fail (gae != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_AMOUNT_EDIT(gae), FALSE);


    if (!gae->need_to_parse)
        return TRUE;

    result = gnc_amount_edit_expr_is_valid (gae, &amount, FALSE);

    if (result == -1)  /* field was empty and may remain so */
        return TRUE;

    if (result == 0)  /* parsing successful */
    {
        gnc_numeric old_amount = gae->amount;

        if (gae->fraction > 0)
            amount = gnc_numeric_convert (amount, gae->fraction, GNC_HOW_RND_ROUND_HALF_UP);

        gnc_amount_edit_set_amount (gae, amount);

        if (!gnc_numeric_equal (amount, old_amount))
            g_signal_emit (gae, amount_edit_signals [AMOUNT_CHANGED], 0);

        return TRUE;
    }

    /* Parse error */
    gtk_editable_set_position (GTK_EDITABLE(gae), result);
    return FALSE;
}

gnc_numeric
gnc_amount_edit_get_amount (GNCAmountEdit *gae)
{
    g_return_val_if_fail (gae != NULL, gnc_numeric_zero ());
    g_return_val_if_fail (GNC_IS_AMOUNT_EDIT(gae), gnc_numeric_zero ());

    gnc_amount_edit_evaluate (gae);

    return gae->amount;
}

double
gnc_amount_edit_get_damount (GNCAmountEdit *gae)
{
    g_return_val_if_fail (gae != NULL, 0.0);
    g_return_val_if_fail (GNC_IS_AMOUNT_EDIT(gae), 0.0);

    gnc_amount_edit_evaluate (gae);

    return gnc_numeric_to_double (gae->amount);
}

void
gnc_amount_edit_set_amount (GNCAmountEdit *gae, gnc_numeric amount)
{
    const char * amount_string;

    g_return_if_fail (gae != NULL);
    g_return_if_fail (GNC_IS_AMOUNT_EDIT(gae));
    g_return_if_fail (!gnc_numeric_check (amount));

    /* Update the display. */
    amount_string = xaccPrintAmount (amount, gae->print_info);
    gtk_entry_set_text (GTK_ENTRY(gae), amount_string);

    gae->amount = amount;
    gae->need_to_parse = FALSE;
}

void
gnc_amount_edit_set_damount (GNCAmountEdit *gae, double damount)
{
    gnc_numeric amount;
    int fraction;

    g_return_if_fail (gae != NULL);
    g_return_if_fail (GNC_IS_AMOUNT_EDIT(gae));

    if (gae->fraction > 0)
        fraction = gae->fraction;
    else
        fraction = 100000;

    amount = double_to_gnc_numeric (damount, fraction, GNC_HOW_RND_ROUND_HALF_UP);

    gnc_amount_edit_set_amount (gae, amount);
}

void
gnc_amount_edit_set_print_info (GNCAmountEdit *gae,
                                GNCPrintAmountInfo print_info)
{
    g_return_if_fail (gae != NULL);
    g_return_if_fail (GNC_IS_AMOUNT_EDIT(gae));

    gae->print_info = print_info;
    gae->print_info.use_symbol = 0;
}

void
gnc_amount_edit_set_fraction (GNCAmountEdit *gae, int fraction)
{
    g_return_if_fail (gae != NULL);
    g_return_if_fail (GNC_IS_AMOUNT_EDIT(gae));

    fraction = MAX (0, fraction);

    gae->fraction = fraction;
}

GtkWidget *
gnc_amount_edit_gtk_entry (GNCAmountEdit *gae)
{
    g_return_val_if_fail (gae != NULL, NULL);
    g_return_val_if_fail (GNC_IS_AMOUNT_EDIT(gae), NULL);

    return GTK_WIDGET(gae);
}

void
gnc_amount_edit_set_evaluate_on_enter (GNCAmountEdit *gae,
                                       gboolean evaluate_on_enter)
{
    g_return_if_fail (gae != NULL);
    g_return_if_fail (GNC_IS_AMOUNT_EDIT(gae));

    gae->evaluate_on_enter = evaluate_on_enter;
}
