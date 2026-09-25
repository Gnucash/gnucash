/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * A charmap selector widget.
 *
 *  Copyright (C) 2003-2005 Andreas J. Guelzow
 *
 *  based on code by:
 *  Copyright (C) 2000 Marco Pesenti Gritti
 *  from the galeon code base
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#include <config.h>
#include "go-charmap-sel.h"
#include "go-optionmenu.h"
#include "go-glib-extras.h"
#include <glib/gi18n-lib.h>
#include <gdk/gdk.h>
#include <string.h>
#include <stdlib.h>

#define CS(x) GO_CHARMAP_SEL (x)

#define CHARMAP_NAME_KEY "Name of Character Encoding"

/* ------------------------------------------------------------------------- */

typedef enum
{
    LG_ARABIC,
    LG_BALTIC,
    LG_CENTRAL_EUROPEAN,
    LG_CHINESE,
    LG_CYRILLIC,
    LG_GREEK,
    LG_HEBREW,
    LG_INDIAN,
    LG_JAPANESE,
    LG_KOREAN,
    LG_TURKISH,
    LG_UNICODE,
    LG_VIETNAMESE,
    LG_WESTERN,
    LG_OTHER,
    LG_LAST
} LanguageGroup;

typedef struct
{
    char const *group_name;
    LanguageGroup const lgroup;
    /* Generated stuff follows.  */
    char *collate_key;
} LGroupInfo;

static LGroupInfo lgroups[] =
{
{ N_("Arabic"), LG_ARABIC },
{ N_("Baltic"), LG_BALTIC },
{ N_("Central European"), LG_CENTRAL_EUROPEAN },
{ N_("Chinese"), LG_CHINESE },
{ N_("Cyrillic"), LG_CYRILLIC },
{ N_("Greek"), LG_GREEK },
{ N_("Hebrew"), LG_HEBREW },
{ N_("Indian"), LG_INDIAN },
{ N_("Japanese"), LG_JAPANESE },
{ N_("Korean"), LG_KOREAN },
{ N_("Turkish"), LG_TURKISH },
{ N_("Unicode"), LG_UNICODE },
{ N_("Vietnamese"), LG_VIETNAMESE },
{ N_("Western"), LG_WESTERN },
{ N_("Other"), LG_OTHER },
{ NULL, LG_LAST } };

static int lgroups_order(const void *_a, const void *_b)
{
    const LGroupInfo *a = (const LGroupInfo *) _a;
    const LGroupInfo *b = (const LGroupInfo *) _b;

    return strcmp(a->collate_key, b->collate_key);
}

/* ------------------------------------------------------------------------- */

typedef enum
{
    CI_MINOR, CI_MAJOR
} CharsetImportance;

typedef struct
{
    gchar const *charset_title;
    gchar const *aliases;
    LanguageGroup const lgroup;
    CharsetImportance const imp;
    /* Generated stuff follows.  */
    char *collate_key;
    char *to_utf8_iconv_name, *from_utf8_iconv_name;
} CharsetInfo;

static CharsetInfo charset_trans_array[] =
        {
        { N_("Arabic (IBM-864)"), "IBM864", LG_ARABIC, CI_MINOR },
        { N_("Arabic (IBM-864-I)"), "IBM864i", LG_ARABIC, CI_MINOR },
        { N_("Arabic (ISO-8859-6)"), "ISO-8859-6", LG_ARABIC, CI_MINOR },
        { N_("Arabic (ISO-8859-6-E)"), "ISO-8859-6-E", LG_ARABIC, CI_MINOR },

        { N_("Arabic (ISO-8859-6-I)"), "ISO-8859-6-I", LG_ARABIC, CI_MINOR },
        { N_("Arabic (MacArabic)"), "x-mac-arabic", LG_ARABIC, CI_MINOR },
        { N_("Arabic (Windows-1256)"), "windows-1256", LG_ARABIC, CI_MINOR },
        { N_("Armenian (ARMSCII-8)"), "armscii-8", LG_OTHER, CI_MINOR },
        { N_("Baltic (ISO-8859-13)"), "ISO-8859-13", LG_BALTIC, CI_MINOR },
        { N_("Baltic (ISO-8859-4)"), "ISO-8859-4", LG_BALTIC, CI_MINOR },
        { N_("Baltic (Windows-1257)"), "windows-1257", LG_BALTIC, CI_MINOR },
        { N_("Celtic (ISO-8859-14)"), "ISO-8859-14", LG_OTHER, CI_MINOR },
        { N_("Central European (IBM-852)"), "IBM852", LG_CENTRAL_EUROPEAN,
                CI_MINOR },
        { N_("Central European (ISO-8859-2)"), "ISO-8859-2",
                LG_CENTRAL_EUROPEAN, CI_MINOR },
        { N_("Central European (MacCE)"), "x-mac-ce", LG_CENTRAL_EUROPEAN,
                CI_MINOR },
        { N_("Central European (Windows-1250)"), "windows-1250",
                LG_CENTRAL_EUROPEAN, CI_MINOR },
        { N_("Chinese Simplified (GB18030)"), "gb18030", LG_CHINESE, CI_MINOR },
        { N_("Chinese Simplified (GB2312)"), "GB2312", LG_CHINESE, CI_MINOR },
        { N_("Chinese Simplified (GBK)"), "x-gbk", LG_CHINESE, CI_MINOR },
        { N_("Chinese Simplified (HZ)"), "HZ-GB-2312", LG_CHINESE, CI_MINOR },
        { N_("Chinese Simplified (Windows-936)"), "windows-936", LG_CHINESE,
                CI_MINOR },
        { N_("Chinese Traditional (Big5)"), "Big5", LG_CHINESE, CI_MINOR },
        { N_("Chinese Traditional (Big5-HKSCS)"), "Big5-HKSCS", LG_CHINESE,
                CI_MINOR },
                { N_("Chinese Traditional (EUC-TW)"), "x-euc-tw", LG_CHINESE,
                        CI_MINOR },
                { N_("Croatian (MacCroatian)"), "x-mac-croatian",
                        LG_CENTRAL_EUROPEAN, CI_MINOR },
                { N_("Cyrillic (IBM-855)"), "IBM855", LG_CYRILLIC, CI_MINOR },
                { N_("Cyrillic (ISO-8859-5)"), "ISO-8859-5", LG_CYRILLIC,
                        CI_MINOR },
                { N_("Cyrillic (ISO-IR-111)"), "ISO-IR-111", LG_CYRILLIC,
                        CI_MINOR },
                { N_("Cyrillic (KOI8-R)"), "KOI8-R", LG_CYRILLIC, CI_MINOR },
                { N_("Cyrillic (MacCyrillic)"), "x-mac-cyrillic", LG_CYRILLIC,
                        CI_MINOR },
                { N_("Cyrillic (Windows-1251)"), "windows-1251", LG_CYRILLIC,
                        CI_MINOR },
                { N_("Russian (CP-866)"), "IBM866", LG_CYRILLIC, CI_MINOR },
                { N_("Ukrainian (KOI8-U)"), "KOI8-U", LG_CYRILLIC, CI_MINOR },
                { N_("Ukrainian (MacUkrainian)"), "x-mac-ukrainian",
                        LG_CYRILLIC, CI_MINOR },
                { N_("English (ASCII)"), "ANSI_X3.4-1968#ASCII", LG_WESTERN,
                        CI_MAJOR },
                { N_("Farsi (MacFarsi)"), "x-mac-farsi", LG_OTHER, CI_MINOR },
                { N_("Georgian (GEOSTD8)"), "geostd8", LG_OTHER, CI_MINOR },
                { N_("Greek (ISO-8859-7)"), "ISO-8859-7", LG_GREEK, CI_MINOR },
                { N_("Greek (MacGreek)"), "x-mac-greek", LG_GREEK, CI_MINOR },
                { N_("Greek (Windows-1253)"), "windows-1253", LG_GREEK, CI_MINOR },
                { N_("Gujarati (MacGujarati)"), "x-mac-gujarati", LG_INDIAN,
                        CI_MINOR },
                { N_("Gurmukhi (MacGurmukhi)"), "x-mac-gurmukhi", LG_INDIAN,
                        CI_MINOR },
                { N_("Hebrew (IBM-862)"), "IBM862", LG_HEBREW, CI_MINOR },
                { N_("Hebrew (ISO-8859-8-E)"), "ISO-8859-8-E", LG_HEBREW,
                        CI_MINOR },
                { N_("Hebrew (ISO-8859-8-I)"), "ISO-8859-8-I", LG_HEBREW,
                        CI_MINOR },
                { N_("Hebrew (MacHebrew)"), "x-mac-hebrew", LG_HEBREW, CI_MINOR },
                { N_("Hebrew (Windows-1255)"), "windows-1255", LG_HEBREW,
                        CI_MINOR },
                { N_("Hindi (MacDevanagari)"), "x-mac-devanagari", LG_INDIAN,
                        CI_MINOR },
                { N_("Icelandic (MacIcelandic)"), "x-mac-icelandic", LG_OTHER,
                        CI_MINOR },
                { N_("Japanese (EUC-JP)"), "EUC-JP", LG_JAPANESE, CI_MINOR },
                { N_("Japanese (ISO-2022-JP)"), "ISO-2022-JP", LG_JAPANESE,
                        CI_MINOR },
                { N_("Japanese (Shift_JIS)"), "CP932", LG_JAPANESE, CI_MINOR },
                { N_("Korean (EUC-KR)"), "EUC-KR", LG_KOREAN, CI_MINOR },
                { N_("Korean (ISO-2022-KR)"), "ISO-2022-KR", LG_KOREAN, CI_MINOR },
                { N_("Korean (JOHAB)"), "x-johab", LG_KOREAN, CI_MINOR },
                { N_("Korean (UHC)"), "x-windows-949", LG_KOREAN, CI_MINOR },
                { N_("Nordic (ISO-8859-10)"), "ISO-8859-10", LG_OTHER, CI_MINOR },
                { N_("Romanian (MacRomanian)"), "x-mac-romanian", LG_OTHER,
                        CI_MINOR },
                { N_("Romanian (ISO-8859-16)"), "ISO-8859-16", LG_OTHER,
                        CI_MINOR },
                { N_("South European (ISO-8859-3)"), "ISO-8859-3", LG_OTHER,
                        CI_MINOR },
                { N_("Thai (TIS-620)"), "TIS-620", LG_OTHER, CI_MINOR },
                { N_("Turkish (IBM-857)"), "IBM857", LG_TURKISH, CI_MINOR },
                { N_("Turkish (ISO-8859-9)"), "ISO-8859-9", LG_TURKISH, CI_MINOR },
                { N_("Turkish (MacTurkish)"), "x-mac-turkish", LG_TURKISH,
                        CI_MINOR },
                { N_("Turkish (Windows-1254)"), "windows-1254", LG_TURKISH,
                        CI_MINOR },
                { N_("Unicode (UTF-7)"), "UTF-7", LG_UNICODE, CI_MINOR },
                { N_("Unicode (UTF-8)"), "UTF-8", LG_UNICODE, CI_MAJOR },
                { N_("Unicode (UTF-16BE)"), "UTF-16BE", LG_UNICODE, CI_MINOR },
                { N_("Unicode (UTF-16LE)"), "UTF-16LE", LG_UNICODE, CI_MINOR },
                { N_("Unicode (UTF-32BE)"), "UTF-32BE", LG_UNICODE, CI_MINOR },
                { N_("Unicode (UTF-32LE)"), "UTF-32LE", LG_UNICODE, CI_MINOR },
                { N_("User Defined"), "x-user-defined", LG_OTHER, CI_MINOR },
                { N_("Vietnamese (TCVN)"), "x-viet-tcvn5712", LG_VIETNAMESE,
                        CI_MINOR },
                { N_("Vietnamese (VISCII)"), "VISCII", LG_VIETNAMESE, CI_MINOR },
                { N_("Vietnamese (VPS)"), "x-viet-vps", LG_VIETNAMESE, CI_MINOR },
                { N_("Vietnamese (Windows-1258)"), "windows-1258",
                        LG_VIETNAMESE, CI_MINOR },
                { N_("Visual Hebrew (ISO-8859-8)"), "ISO-8859-8", LG_HEBREW,
                        CI_MINOR },
                { N_("Western (IBM-850)"), "IBM850", LG_WESTERN, CI_MINOR },
                { N_("Western (ISO-8859-1)"), "ISO-8859-1", LG_WESTERN, CI_MAJOR },
                { N_("Western (ISO-8859-15)"), "ISO-8859-15", LG_WESTERN,
                        CI_MINOR },
                { N_("Western (MacRoman)"), "x-mac-roman", LG_WESTERN, CI_MINOR },
                { N_("Western (Windows-1252)"), "windows-1252", LG_WESTERN,
                        CI_MINOR },
                /* charsets without possibly translatable names */
                { "T61.8bit", "T61.8bit", LG_OTHER, CI_MINOR },
                { "x-imap4-modified-utf7", "x-imap4-modified-utf7", LG_UNICODE,
                        CI_MINOR },
                { "x-u-escaped", "x-u-escaped", LG_OTHER, CI_MINOR },
                { NULL, NULL, LG_LAST, 0 } };

static int charset_order(const void *_a, const void *_b)
{
    const CharsetInfo *a = (const CharsetInfo *) _a;
    const CharsetInfo *b = (const CharsetInfo *) _b;

    if (a->lgroup != b->lgroup)
        return (int) b->lgroup - (int) a->lgroup;

    if (a->imp != b->imp)
        return (int) b->imp - (int) a->imp;

    return strcmp(a->collate_key, b->collate_key);
}

/* ------------------------------------------------------------------------- */

/* name -> CharsetInfo* mapping */
static GHashTable *encoding_hash;

struct _GOCharmapSel
{
    GtkBox box;
    GOOptionMenu *encodings;
    GtkPopover *encodings_menu;
    GtkStack *encodings_stack;
    GHashTable *encoding_items;
    GOCharmapSelTestDirection test;
};


/* Signals we emit */
enum
{
    CHARMAP_CHANGED, LAST_SIGNAL
};

enum
{
    PROP_0, PROP_TEST_DIRECTION
};

static guint cs_signals[LAST_SIGNAL] =
{ 0 };

static void cs_set_property(GObject *object, guint prop_id, const GValue *value,
        GParamSpec *pspec);

static void cs_get_property(GObject *object, guint prop_id, GValue *value,
        GParamSpec *pspec);

G_DEFINE_TYPE (GOCharmapSel, go_charmap_sel, GTK_TYPE_BOX)

static gboolean iconv_supported(const char *to, const char *from)
{
    GIConv ic = g_iconv_open(to, from);
    if (ic == NULL || ic == (GIConv) -1)
        return FALSE;

    g_iconv_close(ic);
    return TRUE;
}

const char *
go_charmap_sel_get_encoding_name (G_GNUC_UNUSED GOCharmapSel *cs,
        const char *encoding)
{
    CharsetInfo const *ci;

    g_return_val_if_fail (encoding != NULL, NULL);

    ci = g_hash_table_lookup (encoding_hash, encoding);
    return ci ? _(ci->charset_title) : NULL;
}

static char const *
get_locale_encoding_name(GOCharmapSel *cs)
{
    char const *locale_encoding;
    char const *name;

    g_get_charset(&locale_encoding);
    name = go_charmap_sel_get_encoding_name(cs, locale_encoding);
    return name ? name : locale_encoding;
}

static void encodings_changed_cb(GOOptionMenu *optionmenu, GOCharmapSel *cs)
{
    g_return_if_fail(GO_IS_CHARMAP_SEL(cs));
    g_return_if_fail(optionmenu == cs->encodings);

    g_signal_emit(G_OBJECT(cs), cs_signals[CHARMAP_CHANGED], 0,
            go_charmap_sel_get_encoding(cs));
}

static void set_menu_to_default(GOCharmapSel *cs, gint item)
{
    GSList sel =
    { GINT_TO_POINTER(item - 1), NULL };

    g_return_if_fail(cs != NULL && GO_IS_CHARMAP_SEL(cs));

    go_option_menu_set_history(cs->encodings, &sel);
}

static gboolean cs_mnemonic_activate(GtkWidget *w, G_GNUC_UNUSED gboolean group_cycling)
{
    GOCharmapSel *cs = GO_CHARMAP_SEL(w);
    gtk_widget_grab_focus(GTK_WIDGET(cs->encodings));
    return TRUE;
}

static void cs_emphasize_label(GtkLabel *label)
{
    char *text = g_markup_printf_escaped("<b>%s</b>",
            gtk_label_get_label(label));
    gtk_label_set_use_markup(label, TRUE);
    gtk_label_set_label(label, text);
    g_free(text);
}

static GtkWidget *
cs_create_leaf(const char *title, const char *encoding, gboolean emphasize)
{
    GtkWidget *row = gtk_list_box_row_new();
    GtkWidget *box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
    GtkWidget *indicator = gtk_image_new_from_icon_name("object-select-symbolic");
    GtkWidget *label = gtk_label_new(title);

    gtk_widget_set_visible(indicator, FALSE);
    gtk_widget_set_valign(indicator, GTK_ALIGN_CENTER);
    gtk_box_append(GTK_BOX(box), indicator);

    gtk_label_set_xalign(GTK_LABEL(label), 0.0f);
    gtk_widget_set_hexpand(label, TRUE);
    if (emphasize)
        cs_emphasize_label(GTK_LABEL(label));
    gtk_box_append(GTK_BOX(box), label);

    gtk_list_box_row_set_child(GTK_LIST_BOX_ROW(row), box);
    gtk_list_box_row_set_activatable(GTK_LIST_BOX_ROW(row), TRUE);
    gtk_list_box_row_set_selectable(GTK_LIST_BOX_ROW(row), TRUE);
    g_object_set_data_full(G_OBJECT(row), "option-menu-text", g_strdup(title),
                           g_free);
    g_object_set_data(G_OBJECT(row), "go-option-menu-selected-indicator",
                      indicator);
    g_object_set_data(G_OBJECT(row), "go-option-menu-selectable",
                      GINT_TO_POINTER(TRUE));
    if (encoding)
        g_object_set_data(G_OBJECT(row), CHARMAP_NAME_KEY, (gpointer) encoding);

    return row;
}

static GtkWidget *
cs_create_navigation_row(const char *title, const char *page)
{
    GtkWidget *row = gtk_list_box_row_new();
    GtkWidget *box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
    GtkWidget *label = gtk_label_new(title);
    GtkWidget *arrow = gtk_image_new_from_icon_name("go-next-symbolic");

    gtk_label_set_xalign(GTK_LABEL(label), 0.0f);
    gtk_widget_set_hexpand(label, TRUE);
    gtk_box_append(GTK_BOX(box), label);
    gtk_widget_set_valign(arrow, GTK_ALIGN_CENTER);
    gtk_box_append(GTK_BOX(box), arrow);

    gtk_list_box_row_set_child(GTK_LIST_BOX_ROW(row), box);
    gtk_list_box_row_set_activatable(GTK_LIST_BOX_ROW(row), TRUE);
    gtk_list_box_row_set_selectable(GTK_LIST_BOX_ROW(row), TRUE);
    g_object_set_data_full(G_OBJECT(row), "go-option-menu-page", g_strdup(page),
                           g_free);

    return row;
}

static GtkWidget *
cs_create_separator_row(void)
{
    GtkWidget *row = gtk_list_box_row_new();
    GtkWidget *separator = gtk_separator_new(GTK_ORIENTATION_HORIZONTAL);

    gtk_list_box_row_set_child(GTK_LIST_BOX_ROW(row), separator);
    gtk_list_box_row_set_activatable(GTK_LIST_BOX_ROW(row), FALSE);
    gtk_list_box_row_set_selectable(GTK_LIST_BOX_ROW(row), FALSE);

    return row;
}

static gboolean
cs_activate_menu_row(GOCharmapSel *cs, GtkListBoxRow *row)
{
    const char *page = g_object_get_data(G_OBJECT(row), "go-option-menu-page");

    if (page)
    {
        GtkWidget *next = gtk_stack_get_child_by_name(cs->encodings_stack, page);

        gtk_stack_set_visible_child_name(cs->encodings_stack, page);
        if (GTK_IS_LIST_BOX(next))
        {
            GtkListBoxRow *first = gtk_list_box_get_row_at_index(GTK_LIST_BOX(next), 1);
            if (!first)
                first = gtk_list_box_get_row_at_index(GTK_LIST_BOX(next), 0);
            if (first)
                gtk_list_box_select_row(GTK_LIST_BOX(next), first);
        }
        return TRUE;
    }

    if (g_object_get_data(G_OBJECT(row), "go-option-menu-selectable"))
    {
        go_option_menu_activate_item(cs->encodings, GTK_WIDGET(row));
        return TRUE;
    }

    return FALSE;
}

static void
cs_menu_row_activated(G_GNUC_UNUSED GtkListBox *list, GtkListBoxRow *row,
                      GOCharmapSel *cs)
{
    cs_activate_menu_row(cs, row);
}

static void
cs_menu_click_pressed(GtkGestureClick *gesture,
                      G_GNUC_UNUSED gint n_press,
                      G_GNUC_UNUSED gdouble x,
                      gdouble y,
                      GOCharmapSel *cs)
{
    GtkWidget *widget = gtk_event_controller_get_widget(
            GTK_EVENT_CONTROLLER(gesture));
    GtkListBoxRow *row = gtk_list_box_get_row_at_y(GTK_LIST_BOX(widget),
                                                    (gint) y);

    if (row && cs_activate_menu_row(cs, row))
        gtk_gesture_set_state(GTK_GESTURE(gesture), GTK_EVENT_SEQUENCE_CLAIMED);
}

static gboolean
cs_menu_key_pressed(GtkEventControllerKey *controller, guint keyval,
                    G_GNUC_UNUSED guint keycode,
                    G_GNUC_UNUSED GdkModifierType state,
                    GOCharmapSel *cs)
{
    GtkWidget *widget;
    GtkListBoxRow *row;

    switch (keyval)
    {
    case GDK_KEY_KP_Enter:
    case GDK_KEY_Return:
    case GDK_KEY_KP_Space:
    case GDK_KEY_space:
        widget = gtk_event_controller_get_widget(GTK_EVENT_CONTROLLER(controller));
        row = gtk_list_box_get_selected_row(GTK_LIST_BOX(widget));
        if (row && cs_activate_menu_row(cs, row))
        {
            gtk_event_controller_reset(GTK_EVENT_CONTROLLER(controller));
            return TRUE;
        }
        break;
    }

    return FALSE;
}

static GtkWidget *
cs_create_menu_page(GOCharmapSel *cs)
{
    GtkWidget *list = gtk_list_box_new();
    GtkGesture *click_gesture;
    GtkEventController *key_controller;

    gtk_list_box_set_selection_mode(GTK_LIST_BOX(list), GTK_SELECTION_SINGLE);
    g_signal_connect(list, "row-activated", G_CALLBACK(cs_menu_row_activated), cs);

    click_gesture = gtk_gesture_click_new();
    gtk_gesture_single_set_button(GTK_GESTURE_SINGLE(click_gesture),
                                  GDK_BUTTON_PRIMARY);
    gtk_event_controller_set_propagation_phase(GTK_EVENT_CONTROLLER(click_gesture),
                                               GTK_PHASE_CAPTURE);
    g_signal_connect(click_gesture, "pressed",
                     G_CALLBACK(cs_menu_click_pressed), cs);
    gtk_widget_add_controller(list, GTK_EVENT_CONTROLLER(click_gesture));

    key_controller = gtk_event_controller_key_new();
    gtk_event_controller_set_propagation_phase(key_controller, GTK_PHASE_CAPTURE);
    g_signal_connect(key_controller, "key-pressed",
                     G_CALLBACK(cs_menu_key_pressed), cs);
    gtk_widget_add_controller(list, key_controller);

    return list;
}

static void
cs_register_leaf(GOCharmapSel *cs, gint root_index, gint child_index,
                 GtkWidget *item)
{
    GSList root = { GINT_TO_POINTER(root_index), NULL };
    GSList child = { GINT_TO_POINTER(child_index), NULL };

    if (child_index >= 0)
        root.next = &child;

    go_option_menu_register_item(cs->encodings, &root, item);
}

static void
go_charmap_sel_init(GOCharmapSel *cs)
{
    gtk_orientable_set_orientation(GTK_ORIENTABLE(cs), GTK_ORIENTATION_HORIZONTAL);

    cs->test = GO_CHARMAP_SEL_TO_UTF8;
    cs->encoding_items = g_hash_table_new(g_str_hash, g_str_equal);
    cs->encodings = GO_OPTION_MENU(go_option_menu_new());

    g_signal_connect(cs->encodings, "changed",
                     G_CALLBACK(encodings_changed_cb), cs);
    gtk_widget_set_hexpand(GTK_WIDGET(cs->encodings), TRUE);
    gtk_box_append(GTK_BOX(cs), GTK_WIDGET(cs->encodings));
}

static void
cs_popover_closed(G_GNUC_UNUSED GtkPopover *popover, GOCharmapSel *cs)
{
    if (cs->encodings_stack)
        gtk_stack_set_visible_child_name(cs->encodings_stack, "root");
}

static void
cs_build_menu(GOCharmapSel *cs)
{
    GtkWidget *root;
    GtkWidget *stack;
    GtkWidget *menu;
    LGroupInfo const *lgroup = lgroups;
    gint root_index = 0;

    g_hash_table_remove_all(cs->encoding_items);

    menu = gtk_popover_new();
    gtk_popover_set_position(GTK_POPOVER(menu), GTK_POS_BOTTOM);
    stack = gtk_stack_new();
    root = cs_create_menu_page(cs);
    gtk_stack_add_named(GTK_STACK(stack), root, "root");
    gtk_popover_set_child(GTK_POPOVER(menu), stack);

    go_option_menu_set_menu(cs->encodings, menu);
    cs->encodings_menu = GTK_POPOVER(menu);
    cs->encodings_stack = GTK_STACK(stack);
    g_signal_connect(menu, "closed", G_CALLBACK(cs_popover_closed), cs);

    while (lgroup->group_name)
    {
        CharsetInfo const *charset_trans = charset_trans_array;
        GtkWidget *submenu = cs_create_menu_page(cs);
        gint child_index = 0;

        gtk_list_box_append(GTK_LIST_BOX(submenu),
                            cs_create_navigation_row(_("Back"), "root"));

        while (charset_trans->lgroup != LG_LAST)
        {
            if (charset_trans->lgroup == lgroup->lgroup)
            {
                const char *name =
                        cs->test == GO_CHARMAP_SEL_TO_UTF8 ?
                        charset_trans->to_utf8_iconv_name :
                        charset_trans->from_utf8_iconv_name;

                if (name)
                {
                    GtkWidget *item = cs_create_leaf(_(charset_trans->charset_title),
                            name, charset_trans->imp == CI_MAJOR);

                    gtk_list_box_append(GTK_LIST_BOX(submenu), item);
                    cs_register_leaf(cs, root_index, child_index, item);
                    g_hash_table_insert(cs->encoding_items, (gpointer) name, item);
                    child_index++;
                }
            }
            charset_trans++;
        }

        if (child_index > 0)
        {
            char *page = g_strdup_printf("group-%d", root_index);

            gtk_stack_add_named(GTK_STACK(stack), submenu, page);
            gtk_list_box_append(GTK_LIST_BOX(root),
                    cs_create_navigation_row(_(lgroup->group_name), page));
            g_free(page);
            root_index++;
        }
        else
            g_object_unref(submenu);

        lgroup++;
    }

    gtk_list_box_append(GTK_LIST_BOX(root), cs_create_separator_row());
    root_index++;

    {
        char *title = g_strconcat(_("Locale: "), get_locale_encoding_name(cs), NULL);
        GtkWidget *item = cs_create_leaf(title, NULL, TRUE);

        gtk_list_box_append(GTK_LIST_BOX(root), item);
        cs_register_leaf(cs, root_index, -1, item);
        g_free(title);
        root_index++;
    }

    set_menu_to_default(cs, root_index);
}

static void
go_charmap_sel_dispose(GObject *object)
{
    GOCharmapSel *cs = GO_CHARMAP_SEL(object);

    g_clear_pointer(&cs->encoding_items, g_hash_table_unref);
    cs->encodings_menu = NULL;
    cs->encodings_stack = NULL;

    G_OBJECT_CLASS(go_charmap_sel_parent_class)->dispose(object);
}
static void go_charmap_sel_class_init(GOCharmapSelClass *klass)
{
    CharsetInfo *ci;
    size_t i;

    GObjectClass *gobject_class = G_OBJECT_CLASS(klass);
    GtkWidgetClass *widget_klass = GTK_WIDGET_CLASS(klass);
    widget_klass->mnemonic_activate = cs_mnemonic_activate;

    gobject_class->dispose = go_charmap_sel_dispose;
    gobject_class->set_property = cs_set_property;
    gobject_class->get_property = cs_get_property;

    cs_signals[CHARMAP_CHANGED] = g_signal_new("charmap_changed",
    GO_TYPE_CHARMAP_SEL, G_SIGNAL_RUN_LAST,
            0,
            NULL, NULL, g_cclosure_marshal_VOID__POINTER, G_TYPE_NONE, 1,
            G_TYPE_POINTER);

    g_object_class_install_property(gobject_class, PROP_TEST_DIRECTION,
            g_param_spec_uint("TestDirection", _("Conversion Direction"),
                    _("This value determines which iconv test to perform."),
                    (guint) GO_CHARMAP_SEL_TO_UTF8,
                    (guint) GO_CHARMAP_SEL_FROM_UTF8,
                    (guint) GO_CHARMAP_SEL_TO_UTF8, G_PARAM_READWRITE));

    /* ---------------------------------------- */
    /* Sort the groups by translated name.  */

    for (i = 0; i < G_N_ELEMENTS(lgroups) - 2; i++)
    {
        const char *cgroup_name = lgroups[i].group_name;
        const char *group_name = _(cgroup_name);
        lgroups[i].collate_key = g_utf8_collate_key(group_name, -1);
        if (!lgroups[i].collate_key)
        {
            g_warning("Failed to generate collation key for [%s] [%s]",
                    cgroup_name, group_name);
            lgroups[i].collate_key = g_strdup(group_name);
        }
    }
    qsort(lgroups, G_N_ELEMENTS(lgroups) - 2, sizeof(lgroups[0]),
            lgroups_order);
    for (i = 0; i < G_N_ELEMENTS(lgroups) - 2; i++)
    {
        g_free(lgroups[i].collate_key);
        lgroups[i].collate_key = NULL;
    }

    /* ---------------------------------------- */
    /* Sort charsets by group/importance/title.  */

    for (i = 0; i < G_N_ELEMENTS(charset_trans_array) - 1; i++)
    {
        const char *ctitle = charset_trans_array[i].charset_title;
        const char *title = _(ctitle);
        charset_trans_array[i].collate_key = g_utf8_collate_key(title, -1);
        if (!charset_trans_array[i].collate_key)
        {
            g_warning("Failed to generate collation key for [%s] [%s]", ctitle,
                    title);
            charset_trans_array[i].collate_key = g_strdup(title);
        }
    }
    qsort(charset_trans_array, G_N_ELEMENTS(charset_trans_array) - 1,
            sizeof(charset_trans_array[0]), charset_order);
    for (i = 0; i < G_N_ELEMENTS(charset_trans_array) - 1; i++)
    {
        g_free(charset_trans_array[i].collate_key);
        charset_trans_array[i].collate_key = NULL;
    }

    /* ---------------------------------------- */

    encoding_hash = g_hash_table_new_full(go_ascii_strcase_hash,
            go_ascii_strcase_equal, (GDestroyNotify) g_free,
            NULL);

    for (ci = charset_trans_array; ci->charset_title; ci++)
    {
        const char *aliases = ci->aliases;
        char *autoaliases = NULL;

        if (strchr(aliases, '#') == NULL)
        {
            /* Sigh.  This sucks quite a lot.  */
            if (strncmp(aliases, "ISO-", 4) == 0)
            {
                autoaliases = g_strconcat(aliases, "#ISO", aliases + 4, "#ISO_",
                        aliases + 4,
                        NULL);
            }

            if (autoaliases)
                aliases = autoaliases;
        }

        ci->to_utf8_iconv_name = ci->from_utf8_iconv_name = NULL;
        while (aliases)
        {
            const char *sep = strchr(aliases, '#');
            char *alias;

            if (sep)
            {
                alias = g_strndup(aliases, sep - aliases);
                aliases = sep + 1;
            }
            else
            {
                alias = g_strdup(aliases);
                aliases = NULL;
            }

            if (ci->to_utf8_iconv_name == NULL
                    && iconv_supported("UTF-8", alias))
            {
                ci->to_utf8_iconv_name = g_strdup(alias);
            }

            if (ci->from_utf8_iconv_name == NULL
                    && iconv_supported(alias, "UTF-8"))
            {
                ci->from_utf8_iconv_name = g_strdup(alias);
            }

            g_hash_table_insert(encoding_hash, alias, ci);
        }

        g_free(autoaliases);
    }
}

GtkWidget *
go_charmap_sel_new(GOCharmapSelTestDirection test)
{
    return g_object_new(GO_TYPE_CHARMAP_SEL, "TestDirection", test, NULL);
}

gchar const *
go_charmap_sel_get_encoding(GOCharmapSel *cs)
{
    GtkWidget *selection;
    char const *locale_encoding;
    char const *encoding;

    g_get_charset(&locale_encoding);

    g_return_val_if_fail(GO_IS_CHARMAP_SEL(cs), locale_encoding);

    selection = go_option_menu_get_history(cs->encodings);
    encoding = selection ? g_object_get_data(G_OBJECT(selection),
                                             CHARMAP_NAME_KEY) : NULL;
    return encoding ? encoding : locale_encoding;
}

gboolean
go_charmap_sel_set_encoding(GOCharmapSel *cs, const char *enc)
{
    CharsetInfo const *ci;
    GtkWidget *item;

    g_return_val_if_fail(GO_IS_CHARMAP_SEL(cs), FALSE);
    g_return_val_if_fail(enc != NULL, FALSE);

    ci = g_hash_table_lookup(encoding_hash, enc);
    if (!ci)
        return FALSE;

    enc = cs->test == GO_CHARMAP_SEL_TO_UTF8 ? ci->to_utf8_iconv_name :
                                               ci->from_utf8_iconv_name;
    if (!enc)
        return FALSE;

    item = g_hash_table_lookup(cs->encoding_items, enc);
    if (!item)
        return FALSE;

    go_option_menu_set_active_item(cs->encodings, item);
    return TRUE;
}
static void cs_set_property(GObject *object, guint prop_id, const GValue *value,
        GParamSpec *pspec)
{
    GOCharmapSel *cs = GO_CHARMAP_SEL(object);

    switch (prop_id)
    {
    case PROP_TEST_DIRECTION:
        cs->test = g_value_get_uint(value);
        cs_build_menu(cs);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void cs_get_property(GObject *object, guint prop_id, GValue *value,
        GParamSpec *pspec)
{
    GOCharmapSel *cs = GO_CHARMAP_SEL(object);

    switch (prop_id)
    {
    case PROP_TEST_DIRECTION:
        g_value_set_uint(value, (guint) cs->test);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}
