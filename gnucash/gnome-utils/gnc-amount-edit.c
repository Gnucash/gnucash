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
#include <glib/gi18n.h>
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
    ACTIVATE,
    CHANGED,
    AMOUNT_CHANGED,
    LAST_SIGNAL
};

static guint amount_edit_signals [LAST_SIGNAL] = { 0 };

static void gnc_amount_edit_init (GNCAmountEdit *gae);
static void gnc_amount_edit_class_init (GNCAmountEditClass *klass);
static void gnc_amount_edit_changed (GtkEditable *gae,
                                     gpointer user_data);
static void gnc_amount_edit_paste_clipboard (GtkEntry *entry,
                                             gpointer user_data);
static gint gnc_amount_edit_key_press (GtkWidget   *widget,
                                       GdkEventKey *event,
                                       gpointer user_data);

#define GNC_AMOUNT_EDIT_PATH "gnc-amount-edit-path"

G_DEFINE_TYPE (GNCAmountEdit, gnc_amount_edit, GTK_TYPE_BOX)

static void
gnc_amount_edit_finalize (GObject *object)
{
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_AMOUNT_EDIT(object));

    G_OBJECT_CLASS (gnc_amount_edit_parent_class)->finalize (object);
}

static void
gnc_amount_edit_dispose (GObject *object)
{
    GNCAmountEdit *gae;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_AMOUNT_EDIT(object));

    gae = GNC_AMOUNT_EDIT(object);

    if (gae->disposed)
        return;

    gae->disposed = TRUE;

    gtk_widget_destroy (GTK_WIDGET(gae->entry));
    gae->entry = NULL;

    gtk_widget_destroy (GTK_WIDGET(gae->image));
    gae->image = NULL;

    G_OBJECT_CLASS (gnc_amount_edit_parent_class)->dispose (object);
}

static void
gnc_amount_edit_class_init (GNCAmountEditClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    GtkWidgetClass *widget_class = GTK_WIDGET_CLASS(klass);

    object_class->dispose = gnc_amount_edit_dispose;
    object_class->finalize = gnc_amount_edit_finalize;

    amount_edit_signals [ACTIVATE] =
        g_signal_new ("activate",
                      G_OBJECT_CLASS_TYPE(object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET(GNCAmountEditClass, activate),
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);

    amount_edit_signals [CHANGED] =
        g_signal_new ("changed",
                      G_OBJECT_CLASS_TYPE(object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET(GNCAmountEditClass, changed),
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);

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
}

static void
gnc_amount_edit_init (GNCAmountEdit *gae)
{
    gtk_orientable_set_orientation (GTK_ORIENTABLE(gae),
                                    GTK_ORIENTATION_HORIZONTAL);

    gae->entry = GTK_ENTRY(gtk_entry_new());
    gae->need_to_parse = FALSE;
    gae->amount = gnc_numeric_zero ();
    gae->print_info = gnc_default_print_info (FALSE);
    gae->fraction = 0;
    gae->evaluate_on_enter = FALSE;
    gae->validate_on_change = FALSE;
    gae->block_changed = FALSE;
    gae->error_position = -1;
    gae->show_warning_symbol = TRUE;

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(gae), "gnc-id-amount-edit");

    g_signal_connect (G_OBJECT(gae->entry), "key-press-event",
                      G_CALLBACK(gnc_amount_edit_key_press), gae);

    g_signal_connect (G_OBJECT(gae->entry), "changed",
                      G_CALLBACK(gnc_amount_edit_changed), gae);

    g_signal_connect (G_OBJECT(gae->entry), "paste-clipboard",
                      G_CALLBACK(gnc_amount_edit_paste_clipboard), gae);
}

static void
gnc_amount_edit_changed (GtkEditable *editable, gpointer user_data)
{
    GNCAmountEdit *gae = GNC_AMOUNT_EDIT(user_data);

    gae->need_to_parse = TRUE;

    if (gae->block_changed)
        return;

    if (gae->validate_on_change)
    {
        gnc_numeric amount;
        gnc_amount_edit_expr_is_valid (gae, &amount, TRUE);
    }

    g_signal_emit (gae, amount_edit_signals [CHANGED], 0);
}

static void
gnc_amount_edit_paste_clipboard (GtkEntry *entry, gpointer user_data)
{
    GNCAmountEdit *gae = GNC_AMOUNT_EDIT(user_data);
    GtkClipboard *clipboard = gtk_widget_get_clipboard (GTK_WIDGET(entry),
                                                        GDK_SELECTION_CLIPBOARD);
    gchar *text = gtk_clipboard_wait_for_text (clipboard);

    if (text)
    {
        gchar *filtered_text = gnc_filter_text_for_control_chars (text);
        gchar *existing = gtk_editable_get_chars (GTK_EDITABLE(entry), 0, -1);
        gint start_pos, end_pos;
        gint position;

        if (!filtered_text)
        {
            g_free (text);
            g_free (existing);
            return;
        }

        if (gtk_widget_get_visible (GTK_WIDGET(gae->image)))
        {
            gtk_widget_hide (GTK_WIDGET(gae->image));
            gtk_widget_set_tooltip_text (GTK_WIDGET(gae->image), NULL);
        }

        position = gtk_editable_get_position (GTK_EDITABLE(entry));

        if (gtk_editable_get_selection_bounds (GTK_EDITABLE(entry),
                                               &start_pos, &end_pos))
        {
            gint old_len = g_utf8_strlen (existing, -1);
            gchar *begin = g_utf8_substring (existing, 0, start_pos);
            gchar *end = g_utf8_substring (existing, end_pos, old_len);
            gchar *changed_text = g_strdup_printf ("%s%s%s", begin, filtered_text, end);

            gae->block_changed = TRUE;
            gtk_editable_delete_text (GTK_EDITABLE(entry), 0, -1);
            gae->block_changed = FALSE;

            gtk_editable_insert_text (GTK_EDITABLE(entry),
                                      changed_text, -1, &position);

            position = start_pos + g_utf8_strlen (filtered_text, -1);

            g_free (begin);
            g_free (end);
            g_free (changed_text);
        }
        else
            gtk_editable_insert_text (GTK_EDITABLE(entry),
                                      filtered_text, -1, &position);

        gtk_editable_set_position (GTK_EDITABLE(entry), position);

        g_signal_stop_emission_by_name (G_OBJECT(entry), "paste-clipboard");

        g_free (text);
        g_free (existing);
        g_free (filtered_text);
    }
}

static gint
gnc_amount_edit_key_press (GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    GNCAmountEdit *gae = GNC_AMOUNT_EDIT(user_data);
    gint result;

    if (gtk_widget_get_visible (GTK_WIDGET(gae->image)))
    {
        gtk_widget_hide (GTK_WIDGET(gae->image));
        gtk_widget_set_tooltip_text (GTK_WIDGET(gae->image), NULL);
    }

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

    result = (* GTK_WIDGET_GET_CLASS(widget)->key_press_event)(widget, event);

    switch (event->keyval)
    {
    case GDK_KEY_Return:
        if (gae->evaluate_on_enter)
            break;
        else
            g_signal_emit (gae, amount_edit_signals [ACTIVATE], 0);
        if (event->state & (GDK_MODIFIER_INTENT_DEFAULT_MOD_MASK))
            break;
        return result;
    case GDK_KEY_KP_Enter:
        if (!gae->evaluate_on_enter)
            g_signal_emit (gae, amount_edit_signals [ACTIVATE], 0);
        break;
    default:
        return result;
    }

    gnc_amount_edit_evaluate (gae);

    g_signal_emit (gae, amount_edit_signals [ACTIVATE], 0);

    return TRUE;
}

GtkWidget *
gnc_amount_edit_new (void)
{
    GNCAmountEdit *gae = g_object_new (GNC_TYPE_AMOUNT_EDIT, NULL);

    gtk_box_pack_start (GTK_BOX(gae), GTK_WIDGET(gae->entry), TRUE, TRUE, 0);
    gtk_entry_set_width_chars (GTK_ENTRY(gae->entry), 12);
    gae->image = gtk_image_new_from_icon_name ("dialog-warning", GTK_ICON_SIZE_SMALL_TOOLBAR);
    gtk_box_pack_start (GTK_BOX(gae), GTK_WIDGET(gae->image), FALSE, FALSE, 6);
    gtk_widget_set_no_show_all (GTK_WIDGET(gae->image), TRUE);
    gtk_widget_hide (GTK_WIDGET(gae->image));
    gtk_widget_show_all (GTK_WIDGET(gae));

    return GTK_WIDGET(gae);
}

static gint
get_original_error_position (const gchar *string, const gchar *symbol,
                             gint error_pos)
{
    gchar **split_text;
    gint split_text_len;
    gint temp = 0;
    gint ret = 0;
    gint x = 0;
    gboolean found = FALSE;

    if (error_pos == 0)
        return ret;

    if (!symbol)
        return error_pos;

    if (g_strrstr (string, symbol) == NULL)
        return error_pos;

    split_text = g_strsplit (string, symbol, -1);
    split_text_len = g_strv_length (split_text);

    for (x; x < split_text_len; x++)
    {
         gint s_len = g_utf8_strlen (split_text[x], -1);

         temp = temp + s_len;

         if (error_pos <= temp)
         {
             found = TRUE;
             break;
         }
    }

    if (found)
       ret = error_pos + (x * g_utf8_strlen (symbol, -1));
    else
       ret = g_utf8_strlen (string, -1) - 1;

    g_strfreev (split_text);
    return ret;
}

gint
gnc_amount_edit_expr_is_valid (GNCAmountEdit *gae, gnc_numeric *amount,
                               gboolean empty_ok)
{
    const char *string;
    char *error_loc;
    gboolean ok;
    const gnc_commodity *comm;
    char *filtered_string;
    gint error_position;
    const gchar *symbol = NULL;

    g_return_val_if_fail (gae != NULL, -1);
    g_return_val_if_fail (GNC_IS_AMOUNT_EDIT(gae), -1);

    string = gtk_entry_get_text (GTK_ENTRY(gae->entry));

    if (gtk_widget_get_visible (GTK_WIDGET(gae->image)))
    {
        gtk_widget_hide (GTK_WIDGET(gae->image));
        gtk_widget_set_tooltip_text (GTK_WIDGET(gae->image), NULL);
    }

    comm = gae->print_info.commodity;

    gae->error_position = -1;

    filtered_string = gnc_filter_text_for_currency_commodity (comm, string, &symbol);

    if (!filtered_string || *filtered_string == '\0')
    {
        *amount = gnc_numeric_zero ();
        if (empty_ok)
            return -1; /* indicate an empty field */
        else
            return 0; /* indicate successfully parsed as 0 */
    }

    error_loc = NULL;
    ok = gnc_exp_parser_parse (filtered_string, amount, &error_loc);

    if (ok)
    {
        g_free (filtered_string);
        return 0;
    }

    /* Not ok */
    if (error_loc != NULL)
    {
        gint filtered_error = get_original_error_position (string, symbol,
                                                          (error_loc - filtered_string));
        gae->error_position = filtered_error + ERROR_POSITION_BASE;
    }
    else
        gae->error_position = 1;

    if (gae->show_warning_symbol)
    {
        gchar *err_msg = gnc_amount_edit_get_error_message (gae);
        gtk_widget_set_tooltip_text (GTK_WIDGET(gae->image), err_msg);
        gtk_widget_show (GTK_WIDGET(gae->image));
        gtk_widget_queue_resize (GTK_WIDGET(gae->entry));
        g_free (err_msg);
    }

    g_free (filtered_string);
    return gae->error_position;
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

        gtk_editable_set_position (GTK_EDITABLE(gae->entry), -1);

        return TRUE;
    }

    /* Parse error */
    gtk_editable_set_position (GTK_EDITABLE(gae->entry), result - ERROR_POSITION_BASE);
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

    if (gtk_widget_get_visible (GTK_WIDGET(gae->image)))
    {
        gtk_widget_hide (GTK_WIDGET(gae->image));
        gtk_widget_set_tooltip_text (GTK_WIDGET(gae->image), NULL);
    }

    /* Update the display. */
    amount_string = xaccPrintAmount (amount, gae->print_info);
    gtk_entry_set_text (GTK_ENTRY(gae->entry), amount_string);

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

    return GTK_WIDGET(gae->entry);
}

void
gnc_amount_edit_set_evaluate_on_enter (GNCAmountEdit *gae,
                                       gboolean evaluate_on_enter)
{
    g_return_if_fail (gae != NULL);
    g_return_if_fail (GNC_IS_AMOUNT_EDIT(gae));

    gae->evaluate_on_enter = evaluate_on_enter;
}

void
gnc_amount_edit_set_validate_on_change (GNCAmountEdit *gae,
                                        gboolean validate_on_change)
{
    g_return_if_fail (gae != NULL);
    g_return_if_fail (GNC_IS_AMOUNT_EDIT(gae));

    gae->validate_on_change = validate_on_change;
}

void
gnc_amount_edit_select_region (GNCAmountEdit *gae,
                               gint start_pos,
                               gint end_pos)
{
    g_return_if_fail (gae != NULL);
    g_return_if_fail (GNC_IS_AMOUNT_EDIT(gae));

    gtk_editable_select_region (GTK_EDITABLE(gae->entry),
                                start_pos,
                                end_pos);
}

gchar *
gnc_amount_edit_get_error_message (GNCAmountEdit *gae)
{
    g_return_val_if_fail (gae != NULL, NULL);

    if (gae->error_position <= 0) // no error
        return NULL;
    else if (gae->error_position == 1) // error but no error position
        return g_strdup_printf (_("An error occurred while processing '%s'"),
                                gtk_entry_get_text (GTK_ENTRY(gae->entry)));
    else // error and position of error
        return g_strdup_printf (_("An error occurred while processing '%s' at position %d"),
                                gtk_entry_get_text (GTK_ENTRY(gae->entry)),
                                gae->error_position - ERROR_POSITION_BASE);
}

void
gnc_amount_edit_show_warning_symbol (GNCAmountEdit *gae, gboolean show)
{
    g_return_if_fail (gae != NULL);
    g_return_if_fail (GNC_IS_AMOUNT_EDIT(gae));

    gae->show_warning_symbol = show;
}
