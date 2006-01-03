/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * A locale selector widget.
 *
 *  Copyright (C) 2003 Andreas J. Guelzow
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

#include <goffice/goffice-config.h>
#include "go-locale-sel.h"
#include "go-optionmenu.h"
#include <goffice/utils/go-glib-extras.h>
#include <gtk/gtkcheckmenuitem.h>
#include <gtk/gtkhbox.h>
#include <gtk/gtkmenu.h>
#include <gtk/gtkmenuitem.h>
#include <gtk/gtkseparatormenuitem.h>
#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>
#include <string.h>
#include <stdlib.h>
#include <locale.h>

#define LS(x) GO_LOCALE_SEL (x)

#define LOCALE_NAME_KEY "Name of Locale"

/* ------------------------------------------------------------------------- */

typedef enum {
	LG_WESTERN_EUROPE,
	LG_EASTERN_EUROPE,
	LG_NORTH_AMERICA,
	LG_SOUTHCENTRAL_AMERICA,
	LG_ASIA,
	LG_MIDDLE_EAST,
	LG_AFRICA,
	LG_AUSTRALIA,
	LG_OTHER,
	LG_LAST
} LocaleGroup;

typedef struct
{
        char const *group_name;
	LocaleGroup const lgroup;
}
LGroupInfo;

static LGroupInfo lgroups[] = {
	{N_("Western Europe"), LG_WESTERN_EUROPE},
	{N_("Eastern Europe"), LG_EASTERN_EUROPE},
	{N_("North America"), LG_NORTH_AMERICA},
	{N_("South & Central America"), LG_SOUTHCENTRAL_AMERICA},
	{N_("Asia"), LG_ASIA},
	{N_("Africa"), LG_AFRICA},
	{N_("Australia"), LG_AUSTRALIA},
	{N_("Other"), LG_OTHER},
	{NULL, LG_LAST}
};

static int
lgroups_order (const void *_a, const void *_b)
{
	const LGroupInfo *a = (const LGroupInfo *)_a;
	const LGroupInfo *b = (const LGroupInfo *)_b;

	return g_utf8_collate (_(a->group_name), _(b->group_name));
}

/* ------------------------------------------------------------------------- */

typedef struct {
	gchar const *locale_title;
	gchar const *locale;
	LocaleGroup const lgroup;
	gboolean available;
} LocaleInfo;

static LocaleInfo locale_trans_array[] = {
	/*
	 * The format here is "Country/Language (locale)" or just
	 * "Country (locale)" when there is only one choice or one
	 * very dominant language.
	 *
	 * Note: lots of people get very emotional over this.  Please
	 * err on the safe side, if any.
	 */
	{N_("United States/English (C)"),                "C",     LG_NORTH_AMERICA},
	{N_("South Africa Afrikaans (af_ZA)"),           "af_ZA", LG_AFRICA },
	{N_("Ethiopia/Amharic (am_ET)"),                 "am_ET", LG_AFRICA },
	{N_("United Arab Emirates (ar_AE)"),             "ar_AE", LG_ASIA },
	{N_("Bahrain (ar_BH)"),                          "ar_BH", LG_ASIA },
	{N_("Algeria (ar_DZ)"),                          "ar_DZ", LG_AFRICA },
	{N_("Egypt (ar_EG)"),                            "ar_EG", LG_AFRICA },
	{N_("India/Arabic (ar_IN)"),                     "ar_IN", LG_ASIA },
	{N_("Iraq (ar_IQ)"),                             "ar_IQ", LG_ASIA },
	{N_("Jordan (ar_JO)"),                           "ar_JO", LG_ASIA },
	{N_("Kuwait (ar_KW)"),                           "ar_KW", LG_ASIA },
	{N_("Lebanon (ar_LB)"),                          "ar_LB", LG_ASIA },
	{N_("Libya (ar_LY)"),                            "ar_LY", LG_AFRICA },
	{N_("Morocco (ar_MA)"),                          "ar_MA", LG_AFRICA },
	{N_("Oman (ar_OM)"),                             "ar_OM", LG_ASIA },
	{N_("Qatar (ar_QA)"),                            "ar_QA", LG_ASIA },
	{N_("Saudi Arabia (ar_SA)"),                     "ar_SA", LG_ASIA },
	{N_("Sudan (ar_SD)"),                            "ar_SD", LG_AFRICA },
	{N_("Syria (ar_SY)"),                            "ar_SY", LG_ASIA },
	{N_("Tunisia (ar_TN)"),                          "ar_TN", LG_AFRICA },
	{N_("Yemen (ar_YE)"),                            "ar_YE", LG_ASIA },
	{N_("Azerbaijan (az_AZ)"),                       "az_AZ", LG_ASIA },
	{N_("Belarus (be_BY)"),                          "be_BY", LG_EASTERN_EUROPE },
	{N_("Bulgaria (bg_BG)"),                         "bg_BG", LG_EASTERN_EUROPE },
	{N_("Bangladesh (bn_BD)"),                       "bn_BD", LG_ASIA },
	{N_("India/Bengali (bn_IN)"),                    "bn_IN", LG_ASIA },
	{N_("France/Breton (br_FR)"),                    "br_FR", LG_WESTERN_EUROPE },
	{N_("Bosnia and Herzegowina (bs_BA)"),           "bs_BA", LG_EASTERN_EUROPE },
	{N_("Spain/Catalan (ca_ES)"),                    "ca_ES", LG_WESTERN_EUROPE },
	{N_("Czech Republic (cs_CZ)"),                   "cs_CZ", LG_EASTERN_EUROPE },
	{N_("Great Britain/Welsh (cy_GB)"),              "cy_GB", LG_WESTERN_EUROPE },
	{N_("Denmark (da_DK)"),                          "da_DK", LG_WESTERN_EUROPE },
	{N_("Austria (de_AT)"),                          "de_AT", LG_WESTERN_EUROPE },
	{N_("Belgium/German (de_BE)"),                   "de_BE", LG_WESTERN_EUROPE },
	{N_("Switzerland/German (de_CH)"),               "de_CH", LG_WESTERN_EUROPE },
	{N_("Germany (de_DE)"),                          "de_DE", LG_WESTERN_EUROPE},
	{N_("Luxembourg/German (de_LU)"),                "de_LU", LG_WESTERN_EUROPE },
	{N_("Greece (el_GR)"),                           "el_GR", LG_WESTERN_EUROPE },
	{N_("Australia (en_AU)"),                        "en_AU", LG_AUSTRALIA },
	{N_("Botswana (en_BW)"),                         "en_BW", LG_AFRICA },
	{N_("Canada/English (en_CA)"),                   "en_CA", LG_NORTH_AMERICA},
	{N_("Great Britain (en_GB)"),                    "en_GB", LG_WESTERN_EUROPE},
	{N_("Hong Kong/English (en_HK)"),                "en_HK", LG_ASIA },
	{N_("Ireland (en_IE)"),                          "en_IE", LG_WESTERN_EUROPE },
	{N_("India/English (en_IN)"),                    "en_IN", LG_ASIA },
	{N_("New Zealand (en_NZ)"),                      "en_NZ", LG_AUSTRALIA },
	{N_("Philippines (en_PH)"),                      "en_PH", LG_ASIA },
	{N_("Singapore/English (en_SG)"),                "en_SG", LG_ASIA },
	{N_("United States/English (en_US)"),            "en_US", LG_NORTH_AMERICA},
	{N_("South Africa/English (en_ZA)"),             "en_ZA", LG_AFRICA },
	{N_("Zimbabwe (en_ZW)"),                         "en_ZW", LG_AFRICA },
	{N_("Esperanto (eo_EO)"),                        "eo_EO", LG_OTHER },
	{N_("Argentina (es_AR)"),                        "es_AR", LG_SOUTHCENTRAL_AMERICA },
	{N_("Bolivia (es_BO)"),                          "es_BO", LG_SOUTHCENTRAL_AMERICA },
	{N_("Chile (es_CL)"),                            "es_CL", LG_SOUTHCENTRAL_AMERICA },
	{N_("Colombia (es_CO)"),                         "es_CO", LG_SOUTHCENTRAL_AMERICA },
	{N_("Costa Rica (es_CR)"),                       "es_CR", LG_SOUTHCENTRAL_AMERICA },
	{N_("Dominican Republic (es_DO)"),               "es_DO", LG_SOUTHCENTRAL_AMERICA },
	{N_("Ecuador (es_EC)"),                          "es_EC", LG_SOUTHCENTRAL_AMERICA },
	{N_("Spain (es_ES)"),                            "es_ES", LG_WESTERN_EUROPE },
	{N_("Guatemala (es_GT)"),                        "es_GT", LG_SOUTHCENTRAL_AMERICA },
	{N_("Honduras (es_HN)"),                         "es_HN", LG_SOUTHCENTRAL_AMERICA },
	{N_("Mexico (es_MX)"),                           "es_MX", LG_SOUTHCENTRAL_AMERICA },
	{N_("Nicaragua (es_NI)"),                        "es_NI", LG_SOUTHCENTRAL_AMERICA },
	{N_("Panama (es_PA)"),                           "es_PA", LG_SOUTHCENTRAL_AMERICA },
	{N_("Peru (es_PE)"),                             "es_PE", LG_SOUTHCENTRAL_AMERICA },
	{N_("Puerto Rico (es_PR)"),                      "es_PR", LG_SOUTHCENTRAL_AMERICA },
	{N_("Paraguay (es_PY)"),                         "es_PY", LG_SOUTHCENTRAL_AMERICA },
	{N_("El Salvador (es_SV)"),                      "es_SV", LG_SOUTHCENTRAL_AMERICA },
	{N_("United States/Spanish (es_US)"),            "es_US", LG_NORTH_AMERICA },
	{N_("Uruguay (es_UY)"),                          "es_UY", LG_SOUTHCENTRAL_AMERICA },
	{N_("Venezuela (es_VE)"),                        "es_VE", LG_SOUTHCENTRAL_AMERICA },
	{N_("Estonia (et_EE)"),                          "et_EE", LG_EASTERN_EUROPE },
	{N_("Spain/Basque (eu_ES)"),                     "eu_ES", LG_WESTERN_EUROPE },
	{N_("Iran (fa_IR)"),                             "fa_IR", LG_ASIA },
	{N_("Finland/Finnish (fi_FI)"),                  "fi_FI", LG_WESTERN_EUROPE },
	{N_("Faroe Islands (fo_FO)"),                    "fo_FO", LG_WESTERN_EUROPE },
	{N_("Belgium/French (fr_BE)"),                   "fr_BE", LG_WESTERN_EUROPE },
	{N_("Canada/French (fr_CA)"),                    "fr_CA", LG_NORTH_AMERICA },
	{N_("Switzerland/French (fr_CH)"),               "fr_CH", LG_WESTERN_EUROPE },
	{N_("France (fr_FR)"),                           "fr_FR", LG_WESTERN_EUROPE },
	{N_("Ireland/Gaelic (ga_IE)"),                   "ga_IE", LG_WESTERN_EUROPE },
	{N_("Great Britain/Scottish Gaelic (gd_GB)"),    "gd_GB", LG_WESTERN_EUROPE },
	{N_("Spain/Galician (gl_ES)"),                   "gl_ES", LG_WESTERN_EUROPE },
	{N_("Great Britain/Manx Gaelic (gv_GB)"),        "gv_GB", LG_WESTERN_EUROPE },
	{N_("India/Hindu (hi_IN)"),                      "hi_IN", LG_ASIA },
	{N_("Croatia (hr_HR)"),                          "hr_HR", LG_EASTERN_EUROPE },
	{N_("Hungary (hu_HU)"),                          "hu_HU", LG_EASTERN_EUROPE },
	{N_("Armenia (hy_AM)"),                          "hy_AM", LG_EASTERN_EUROPE },
	{N_("(i18n)"),                                   "i18n",  LG_OTHER },
	{N_("Indonesia (id_ID)"),                        "id_ID", LG_ASIA },
	{N_("Iceland (is_IS)"),                          "is_IS", LG_WESTERN_EUROPE },
	{N_("(iso14651_t1)"),                            "iso14651_t1", LG_OTHER },
	{N_("Switzerland/Italian (it_CH)"),              "it_CH", LG_WESTERN_EUROPE },
	{N_("Italy (it_IT)"),                            "it_IT", LG_WESTERN_EUROPE },
	{N_("Israel/Hebrew (iw_IL)"),                    "iw_IL", LG_ASIA },
	{N_("Japan (ja_JP)"),                            "ja_JP", LG_ASIA },
	{N_("Georgia (ka_GE)"),                          "ka_GE", LG_EASTERN_EUROPE },
	{N_("Greenland (kl_GL)"),                        "kl_GL", LG_WESTERN_EUROPE },
	{N_("Korea (ko_KR)"),                            "ko_KR", LG_ASIA },
	{N_("Great Britain/Cornish (kw_GB)"),            "kw_GB", LG_WESTERN_EUROPE },
	{N_("Lithuania (lt_LT)"),                        "lt_LT", LG_EASTERN_EUROPE },
	{N_("Latvia (lv_LV)"),                           "lv_LV", LG_EASTERN_EUROPE },
	{N_("New Zealand/Maori (mi_NZ)"),                "mi_NZ", LG_AUSTRALIA },
	{N_("Macedonia (mk_MK)"),                        "mk_MK", LG_EASTERN_EUROPE },
	{N_("India/Marathi (mr_IN)"),                    "mr_IN", LG_ASIA },
	{N_("Malaysia (ms_MY)"),                         "ms_MY", LG_ASIA },
	{N_("Malta (mt_MT)"),                            "mt_MT", LG_WESTERN_EUROPE },
	{N_("Belgium/Flemish (nl_BE)"),                  "nl_BE", LG_WESTERN_EUROPE },
	{N_("The Netherlands (nl_NL)"),                  "nl_NL", LG_WESTERN_EUROPE },
	{N_("Norway/Nynorsk (nn_NO)"),                   "nn_NO", LG_WESTERN_EUROPE },
	{N_("Norway/Bokmal (no_NO)"),                    "no_NO", LG_WESTERN_EUROPE},
	{N_("France/Occitan (oc_FR)"),                   "oc_FR", LG_WESTERN_EUROPE },
	{N_("Poland (pl_PL)"),                           "pl_PL", LG_EASTERN_EUROPE },
	{N_("Brazil (pt_BR)"),                           "pt_BR", LG_SOUTHCENTRAL_AMERICA },
	{N_("Portugal (pt_PT)"),                         "pt_PT", LG_WESTERN_EUROPE },
	{N_("Romania (ro_RO)"),                          "ro_RO", LG_EASTERN_EUROPE },
	{N_("Russia (ru_RU)"),                           "ru_RU", LG_EASTERN_EUROPE },
	{N_("Ukraine/Russian (ru_UA)"),                  "ru_UA", LG_EASTERN_EUROPE },
	{N_("Norway/Saami (se_NO)"),                     "se_NO", LG_WESTERN_EUROPE },
	{N_("Slovakia (sk_SK)"),                         "sk_SK", LG_EASTERN_EUROPE },
	{N_("Slovenia (sl_SI)"),                         "sl_SI", LG_EASTERN_EUROPE },
	{N_("Albania (sq_AL)"),                          "sq_AL", LG_EASTERN_EUROPE },
	{N_("Yugoslavia (sr_YU)"),                       "sr_YU", LG_EASTERN_EUROPE },
	{N_("Finland/Swedish (sv_FI)"),                  "sv_FI", LG_WESTERN_EUROPE },
	{N_("Sweden (sv_SE)"),                           "sv_SE", LG_WESTERN_EUROPE },
	{N_("India/Tamil (ta_IN)"),                      "ta_IN", LG_ASIA },
	{N_("India/Telugu (te_IN)"),                     "te_IN", LG_ASIA },
	{N_("Tajikistan (tg_TJ)"),                       "tg_TJ", LG_ASIA },
	{N_("Thailand (th_TH)"),                         "th_TH", LG_ASIA },
	{N_("Eritrea (ti_ER)"),                          "ti_ER", LG_AFRICA },
	{N_("Ethiopia/Tigrinya (ti_ET)"),                "ti_ET", LG_AFRICA },
	{N_("Philippines/Tagalog (tl_PH)"),              "tl_PH", LG_ASIA },
	{N_("Turkey (tr_TR)"),                           "tr_TR", LG_ASIA },
	{N_("Russia/Tatar (tt_RU)"),                     "tt_RU", LG_EASTERN_EUROPE },
	{N_("Ukraine (uk_UA)"),                          "uk_UA", LG_EASTERN_EUROPE },
	{N_("Pakistan (ur_PK)"),                         "ur_PK", LG_ASIA },
	{N_("Uzbekistan (uz_UZ)"),                       "uz_UZ", LG_ASIA },
	{N_("Vietnam (vi_VN)"),                          "vi_VN", LG_ASIA },
	{N_("Belgium/Walloon (wa_BE)"),                  "wa_BE", LG_WESTERN_EUROPE },
	{N_("United States/Yiddish (yi_US)"),            "yi_US", LG_NORTH_AMERICA },
	{N_("China (zh_CN)"),                            "zh_CN", LG_ASIA },
	{N_("Hong Kong/Chinese (zh_HK)"),                "zh_HK", LG_ASIA },
	{N_("Singapore/Chinese (zh_SG)"),                "zh_SG", LG_ASIA },
	{N_("Taiwan (zh_TW)"),                           "zh_TW", LG_ASIA },
	{NULL,                                           NULL,    LG_LAST}
};

/* What is this?  See also iw_IL. {N_("(he_IL)"),"he_IL", LG_ASIA }, */
/* {N_("(lug_UG)"),                                 "lug_UG",LG_OTHER }, */


static int
locale_order (const void *_a, const void *_b)
{
	const LocaleInfo *a = (const LocaleInfo *)_a;
	const LocaleInfo *b = (const LocaleInfo *)_b;

	if (a->lgroup != b->lgroup)
		return b->lgroup - a->lgroup;

	return g_utf8_collate (_(a->locale_title), _(b->locale_title));
}

/* ------------------------------------------------------------------------- */

/* name -> LocaleInfo* mapping */
static GHashTable *locale_hash;

struct _GOLocaleSel {
	GtkHBox box;
	GOOptionMenu *locales;
	GtkMenu *locales_menu;
};

typedef struct {
	GtkHBoxClass parent_class;

	gboolean (* locale_changed) (GOLocaleSel *ls, char const *new_locale);
} GOLocaleSelClass;


typedef GOLocaleSel Ls;
typedef GOLocaleSelClass LsClass;

/* Signals we emit */
enum {
	LOCALE_CHANGED,
	LAST_SIGNAL
};

enum {
	PROP_0
};




static guint ls_signals[LAST_SIGNAL] = { 0 };

static void ls_set_property      (GObject          *object,
				  guint             prop_id,
				  const GValue     *value,
				  GParamSpec       *pspec);

static void ls_get_property      (GObject          *object,
				  guint             prop_id,
				  GValue           *value,
				  GParamSpec       *pspec);

const char *
go_locale_sel_get_locale_name (G_GNUC_UNUSED GOLocaleSel *ls,
				 const char *locale)
{
	LocaleInfo const *ci;

	g_return_val_if_fail (locale != NULL, NULL);

	ci = g_hash_table_lookup (locale_hash, locale);
	return ci ? _(ci->locale_title) : NULL;
}

static char*
get_locale_name (GOLocaleSel *ls)
{
	char const *cur_locale, *name;
	char *locale, *p;

	/*
	 * We cannot use LC_ALL here because a composite locale may have
	 * a string that is a mile wide (and not be intented for humans
	 * anyway).  Why use LC_MESSAGES?  Good question, but it actuality
	 * I doubt it will matter.  It's an arbitrary choice.
	 */
	cur_locale = setlocale (LC_MESSAGES, NULL);
	if (!cur_locale) cur_locale = "C";  /* Just in case.  */
	locale = g_strdup (cur_locale);

	/* Get rid of charsets.  */
	p = strchr (locale, '.');
	if (p)
		*p = 0;
	p = strchr (locale, '@');
	if (p)
		*p = 0;

	name = go_locale_sel_get_locale_name (ls, locale);
	if (name) {
		g_free (locale);
		return g_strdup (name);
	} else {
		/* Just in case we get something really wide.  */
		const char *ellipsis = "...";
		if ((size_t)g_utf8_strlen (locale, -1) > 50 + strlen (ellipsis))
			strcpy (g_utf8_offset_to_pointer (locale, 50), ellipsis);

		return locale;
	}
}

static void
locales_changed_cb (GOOptionMenu *optionmenu, GOLocaleSel *ls)
{
	char * locale;

	g_return_if_fail (IS_GO_LOCALE_SEL (ls));
	g_return_if_fail (optionmenu == ls->locales);

	locale = go_locale_sel_get_locale (ls);

	g_signal_emit (G_OBJECT (ls),
		       ls_signals[LOCALE_CHANGED],
		       0, locale);
	g_free (locale);
}

static void
set_menu_to_default (GOLocaleSel *ls, gint item)
{
	GSList sel = { GINT_TO_POINTER (item - 1), NULL};

	g_return_if_fail (ls != NULL && IS_GO_LOCALE_SEL (ls));

	go_option_menu_set_history (ls->locales, &sel);
}

static gboolean
ls_mnemonic_activate (GtkWidget *w, gboolean group_cycling)
{
	GOLocaleSel *ls = GO_LOCALE_SEL (w);
	gtk_widget_grab_focus (GTK_WIDGET (ls->locales));
	return TRUE;
}


static void
ls_build_menu (GOLocaleSel *ls)
{
        GtkWidget *item;
	GtkMenu *menu;
	LGroupInfo const *lgroup = lgroups;
	gint lg_cnt = 0;

        menu = GTK_MENU (gtk_menu_new ());

	while (lgroup->group_name) {
		LocaleInfo const *locale_trans;
		GtkMenu *submenu;
		gint cnt = 0;

		item = gtk_menu_item_new_with_label (_(lgroup->group_name));

		submenu = GTK_MENU (gtk_menu_new ());
		locale_trans = locale_trans_array;

		while (locale_trans->lgroup != LG_LAST) {
			GtkWidget *subitem;
			if (locale_trans->lgroup == lgroup->lgroup && locale_trans->available) {
					subitem = gtk_check_menu_item_new_with_label
						(_(locale_trans->locale_title));
					gtk_widget_show (subitem);
					gtk_menu_shell_append (GTK_MENU_SHELL (submenu),  subitem);
					g_object_set_data (G_OBJECT (subitem), LOCALE_NAME_KEY,
							   (gpointer)(locale_trans->locale));
					cnt++;
			}
			locale_trans++;
		}
		if (cnt > 0) {
			gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), GTK_WIDGET (submenu));
			gtk_widget_show (item);
			gtk_menu_shell_append (GTK_MENU_SHELL (menu),  item);
			lg_cnt++;
		} else {
			g_object_unref (item);
		}
                lgroup++;
        }
	item = gtk_separator_menu_item_new ();
	gtk_widget_show (item);
	gtk_menu_shell_append (GTK_MENU_SHELL (menu),  item);
	lg_cnt++;

	{
		char *locale_name = get_locale_name (ls);
		char *locale_menu_title = g_strconcat (_("Current Locale: "),
						       locale_name, NULL);
		g_free (locale_name);
		item = gtk_check_menu_item_new_with_label (locale_menu_title);
		g_free (locale_menu_title);
		gtk_widget_show (item);
		gtk_menu_shell_append (GTK_MENU_SHELL (menu),  item);
		lg_cnt++;
	}

	go_option_menu_set_menu (ls->locales, GTK_WIDGET (menu));
	ls->locales_menu = menu;
	set_menu_to_default (ls, lg_cnt);
}

static void
ls_init (GOLocaleSel *ls)
{
	ls->locales = GO_OPTION_MENU (go_option_menu_new ());
	ls_build_menu (ls);

	g_signal_connect (G_OBJECT (ls->locales), "changed",
                          G_CALLBACK (locales_changed_cb), ls);
        gtk_box_pack_start (GTK_BOX (ls), GTK_WIDGET (ls->locales),
                            TRUE, TRUE, 0);
}

static void
ls_class_init (GtkWidgetClass *widget_klass)
{
	LocaleInfo *ci;
	char *oldlocale;

	GObjectClass *gobject_class = G_OBJECT_CLASS (widget_klass);
	widget_klass->mnemonic_activate = ls_mnemonic_activate;

	gobject_class->set_property = ls_set_property;
	gobject_class->get_property = ls_get_property;

	ls_signals[LOCALE_CHANGED] =
		g_signal_new ("locale_changed",
			      GO_LOCALE_SEL_TYPE,
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GOLocaleSelClass, locale_changed),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1, G_TYPE_POINTER);

	qsort (lgroups, G_N_ELEMENTS (lgroups) - 2, sizeof (lgroups[0]),
	       lgroups_order);
	qsort (locale_trans_array, G_N_ELEMENTS (locale_trans_array) - 1,
	       sizeof (locale_trans_array[0]), locale_order);

	locale_hash =
		g_hash_table_new_full (go_ascii_strcase_hash,
				       go_ascii_strcase_equal,
				       (GDestroyNotify)g_free,
				       NULL);

	oldlocale = g_strdup (setlocale (LC_ALL, NULL));
	for (ci = locale_trans_array; ci->locale_title; ci++) {
		ci->available = (setlocale (LC_ALL, ci->locale) != NULL);
		g_hash_table_insert (locale_hash, (char *)ci->locale, ci);
	}

	/* Handle the POSIX/C alias.  */
	{
		LocaleInfo *ci = g_hash_table_lookup (locale_hash, "C");
		g_assert (ci != NULL);
		g_hash_table_insert (locale_hash, (char *)"POSIX", ci);
	}

	setlocale (LC_ALL, oldlocale);
	g_free (oldlocale);
}

GSF_CLASS (GOLocaleSel, go_locale_sel,
	   ls_class_init, ls_init, GTK_TYPE_HBOX)

GtkWidget *
go_locale_sel_new (void)
{
	return g_object_new (GO_LOCALE_SEL_TYPE, NULL);
}

gchar *
go_locale_sel_get_locale (GOLocaleSel *ls)
{
	GtkMenuItem *selection;
	char const *cur_locale;
	char const *locale;

	char *cur_locale_cp = NULL;
	char **parts;

	cur_locale = setlocale (LC_ALL, NULL);
	if (cur_locale) {
		parts = g_strsplit (cur_locale,".",2);
		cur_locale_cp = g_strdup (parts[0]);
		g_strfreev (parts);
	}

 	g_return_val_if_fail (IS_GO_LOCALE_SEL (ls), cur_locale_cp);

 	selection = GTK_MENU_ITEM (go_option_menu_get_history (ls->locales));
	locale = (char const *) g_object_get_data (G_OBJECT (selection),
						     LOCALE_NAME_KEY);
	return locale ? g_strdup (locale) : cur_locale_cp;
}

struct cb_find_entry {
	const char *locale;
	gboolean found;
	int i;
	GSList *path;
};

static void
cb_find_entry (GtkMenuItem *w, struct cb_find_entry *cl)
{
	GtkWidget *sub;

	if (cl->found)
		return;

	sub = gtk_menu_item_get_submenu (w);
	if (sub) {
		GSList *tmp = cl->path = g_slist_prepend (cl->path, GINT_TO_POINTER (cl->i));
		cl->i = 0;

		gtk_container_foreach (GTK_CONTAINER (sub), (GtkCallback)cb_find_entry, cl);
		if (cl->found)
			return;

		cl->i = GPOINTER_TO_INT (cl->path->data);
		cl->path = cl->path->next;
		g_slist_free_1 (tmp);
	} else {
		const char *this_locale =
			g_object_get_data (G_OBJECT (w), LOCALE_NAME_KEY);
		if (this_locale && strcmp (this_locale, cl->locale) == 0) {
			cl->found = TRUE;
			cl->path = g_slist_prepend (cl->path, GINT_TO_POINTER (cl->i));
			cl->path = g_slist_reverse (cl->path);
			return;
		}
	}
	cl->i++;
}

gboolean
go_locale_sel_set_locale (GOLocaleSel *ls, const char *locale)
{
	struct cb_find_entry cl;
	LocaleInfo const *ci;

	g_return_val_if_fail (IS_GO_LOCALE_SEL (ls), FALSE);
	g_return_val_if_fail (locale != NULL, FALSE);

	ci = g_hash_table_lookup (locale_hash, locale);
	if (!ci)
		return FALSE;

	locale = ci->locale;
	if (!locale)
		return FALSE;

	cl.locale = locale;
	cl.found = FALSE;
	cl.i = 0;
	cl.path = NULL;

	gtk_container_foreach (GTK_CONTAINER (ls->locales_menu),
			       (GtkCallback)cb_find_entry,
			       &cl);
	if (!cl.found)
		return FALSE;

	go_option_menu_set_history (ls->locales, cl.path);
	g_slist_free (cl.path);

	return TRUE;
}


void
go_locale_sel_set_sensitive (GOLocaleSel *ls, gboolean sensitive)
{
	g_return_if_fail (IS_GO_LOCALE_SEL (ls));

	gtk_widget_set_sensitive (GTK_WIDGET (ls->locales), sensitive);
}

static void
ls_set_property (GObject      *object,
		 guint         prop_id,
		 const GValue *value,
		 GParamSpec   *pspec)
{
	GOLocaleSel *ls;
	ls = GO_LOCALE_SEL (object);

	switch (prop_id)
	{
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}


static void
ls_get_property (GObject     *object,
		 guint        prop_id,
		 GValue      *value,
		 GParamSpec  *pspec)
{
	GOLocaleSel *ls;

	ls = GO_LOCALE_SEL (object);

	switch (prop_id)
	{
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}
