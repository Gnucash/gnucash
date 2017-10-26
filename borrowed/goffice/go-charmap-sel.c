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
    GtkMenu *encodings_menu;
    GOCharmapSelTestDirection test;
};

typedef struct
{
    GtkBoxClass parent_class;

    gboolean (*charmap_changed)(GOCharmapSel *cs, char const *new_charmap);
} GOCharmapSelClass;

typedef GOCharmapSel Cs;
typedef GOCharmapSelClass CsClass;

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

static gboolean cs_mnemonic_activate(GtkWidget *w, gboolean group_cycling)
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

static void cs_init(GOCharmapSel *cs)
{
    gtk_orientable_set_orientation (GTK_ORIENTABLE(cs), GTK_ORIENTATION_HORIZONTAL);

    cs->test = GO_CHARMAP_SEL_TO_UTF8;

    cs->encodings = GO_OPTION_MENU(go_option_menu_new());

    g_signal_connect(G_OBJECT(cs->encodings), "changed",
            G_CALLBACK(encodings_changed_cb), cs);
    gtk_box_pack_start(GTK_BOX(cs), GTK_WIDGET(cs->encodings), TRUE, TRUE, 0);
}

static void cs_build_menu(GOCharmapSel *cs)
{
    GtkWidget *item;
    GtkMenu *menu;
    LGroupInfo const *lgroup = lgroups;
    gint lg_cnt = 0;

    menu = GTK_MENU(gtk_menu_new());

    while (lgroup->group_name)
    {
        CharsetInfo const *charset_trans;
        GtkMenu *submenu = NULL;

        charset_trans = charset_trans_array;

        while (charset_trans->lgroup != LG_LAST)
        {
            GtkWidget *subitem;
            if (charset_trans->lgroup == lgroup->lgroup)
            {
                const char *name =
                        (cs->test == GO_CHARMAP_SEL_TO_UTF8) ?
                                charset_trans->to_utf8_iconv_name :
                                charset_trans->from_utf8_iconv_name;
                if (name)
                {
                    if (!submenu)
                        submenu = GTK_MENU(gtk_menu_new());
                    subitem = gtk_check_menu_item_new_with_label(
                            _(charset_trans->charset_title));
                    gtk_check_menu_item_set_draw_as_radio(
                            GTK_CHECK_MENU_ITEM(subitem), TRUE);
                    gtk_widget_show(subitem);
                    gtk_menu_shell_append(GTK_MENU_SHELL(submenu), subitem);
                    if (charset_trans->imp == CI_MAJOR)
                        cs_emphasize_label(
                                GTK_LABEL(gtk_bin_get_child(GTK_BIN(subitem))));
                    g_object_set_data(G_OBJECT(subitem), CHARMAP_NAME_KEY,
                            (gpointer) name);
                }
                else if (0)
                {
                    g_print("Unsupported: %s\n", charset_trans->aliases);
                }
            }
            charset_trans++;
        }
        if (submenu)
        {
            GtkWidget *item = gtk_menu_item_new_with_label(
                    _(lgroup->group_name));

            gtk_menu_item_set_submenu(GTK_MENU_ITEM(item), GTK_WIDGET(submenu));
            gtk_widget_show(item);
            gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
            lg_cnt++;
        }
        lgroup++;
    }
    item = gtk_separator_menu_item_new();
    gtk_widget_show(item);
    gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
    lg_cnt++;

    {
        char *locale_encoding_menu_title = g_strconcat(_("Locale: "),
                get_locale_encoding_name(cs),
                NULL);
        item = gtk_check_menu_item_new_with_label(locale_encoding_menu_title);
        gtk_check_menu_item_set_draw_as_radio(GTK_CHECK_MENU_ITEM(item), TRUE);
        g_free(locale_encoding_menu_title);
        gtk_widget_show(item);
        gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
        lg_cnt++;
        cs_emphasize_label(GTK_LABEL(gtk_bin_get_child(GTK_BIN(item))));
    }

    go_option_menu_set_menu(cs->encodings, GTK_WIDGET(menu));
    cs->encodings_menu = menu;
    set_menu_to_default(cs, lg_cnt);
}

static void cs_class_init(GtkWidgetClass *widget_klass)
{
    CharsetInfo *ci;
    size_t i;

    GObjectClass *gobject_class = G_OBJECT_CLASS(widget_klass);
    widget_klass->mnemonic_activate = cs_mnemonic_activate;

    gobject_class->set_property = cs_set_property;
    gobject_class->get_property = cs_get_property;

    cs_signals[CHARMAP_CHANGED] = g_signal_new("charmap_changed",
    GO_TYPE_CHARMAP_SEL, G_SIGNAL_RUN_LAST,
            G_STRUCT_OFFSET(GOCharmapSelClass, charmap_changed),
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

GType
go_charmap_sel_get_type (void)
{
    static GType go_charmap_sel_type = 0;

    if (go_charmap_sel_type == 0)
    {
        GTypeInfo go_charmap_sel_info =
        {
            sizeof (GOCharmapSelClass),
            NULL,
            NULL,
            (GClassInitFunc) cs_class_init,
            NULL,
            NULL,
            sizeof (GOCharmapSel),
            0,
            (GInstanceInitFunc) cs_init
        };

        go_charmap_sel_type = g_type_register_static (GTK_TYPE_BOX,
                           "GOCharmapSel",
                           &go_charmap_sel_info, 0);
    }

    return go_charmap_sel_type;
}

GtkWidget *
go_charmap_sel_new(GOCharmapSelTestDirection test)
{
    return g_object_new(GO_TYPE_CHARMAP_SEL, "TestDirection", test, NULL);
}

gchar const *
go_charmap_sel_get_encoding(GOCharmapSel *cs)
{
    GtkMenuItem *selection;
    char const *locale_encoding;
    char const *encoding;

    g_get_charset(&locale_encoding);

    g_return_val_if_fail(GO_IS_CHARMAP_SEL(cs), locale_encoding);

    selection = GTK_MENU_ITEM(go_option_menu_get_history(cs->encodings));
    encoding = (char const *) g_object_get_data(G_OBJECT(selection),
    CHARMAP_NAME_KEY);
    return encoding ? encoding : locale_encoding;
}

struct cb_find_entry
{
    const char *enc;
    gboolean found;
    int i;
    GSList *path;
};

static void cb_find_entry(GtkMenuItem *w, struct cb_find_entry *cl)
{
    GtkWidget *sub;

    if (cl->found)
        return;

    sub = gtk_menu_item_get_submenu(w);
    if (sub)
    {
        GSList *tmp = cl->path = g_slist_prepend(cl->path,
                GINT_TO_POINTER(cl->i));
        cl->i = 0;

        gtk_container_foreach(GTK_CONTAINER(sub), (GtkCallback) cb_find_entry,
                cl);
        if (cl->found)
            return;

        cl->i = GPOINTER_TO_INT(cl->path->data);
        cl->path = cl->path->next;
        g_slist_free_1(tmp);
    }
    else
    {
        const char *this_enc = g_object_get_data(G_OBJECT(w), CHARMAP_NAME_KEY);
        if (this_enc && strcmp(this_enc, cl->enc) == 0)
        {
            cl->found = TRUE;
            cl->path = g_slist_prepend(cl->path, GINT_TO_POINTER(cl->i));
            cl->path = g_slist_reverse(cl->path);
            return;
        }
    }
    cl->i++;
}

gboolean go_charmap_sel_set_encoding(GOCharmapSel *cs, const char *enc)
{
    struct cb_find_entry cl;
    CharsetInfo const *ci;

    g_return_val_if_fail(GO_IS_CHARMAP_SEL(cs), FALSE);
    g_return_val_if_fail(enc != NULL, FALSE);

    ci = g_hash_table_lookup(encoding_hash, enc);
    if (!ci)
        return FALSE;

    enc = ci->to_utf8_iconv_name;
    if (!enc)
        return FALSE;

    cl.enc = enc;
    cl.found = FALSE;
    cl.i = 0;
    cl.path = NULL;

    gtk_container_foreach(GTK_CONTAINER(cs->encodings_menu),
            (GtkCallback) cb_find_entry, &cl);
    if (!cl.found)
        return FALSE;

    go_option_menu_set_history(cs->encodings, cl.path);
    g_slist_free(cl.path);

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
