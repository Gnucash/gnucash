/********************************************************************\
 * option-util.h -- GNOME<->guile option interface                  *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
 * Copyright (C) 2000 Dave Peticolas                                *
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

#ifndef OPTION_UTIL_H
#define OPTION_UTIL_H

#include <glib.h>
#include <libguile.h>
#include "guile-mappings.h"

#include "gnc-commodity.h"
#include "qof.h"
#include "GNCId.h"
#include "gnc-ui-common.h"


typedef struct gnc_option GNCOption;
typedef struct gnc_option_section GNCOptionSection;
typedef struct gnc_option_db GNCOptionDB;

typedef int GNCOptionDBHandle;

typedef SCM (*GNCOptionGetUIValue) (GNCOption *option);
typedef void (*GNCOptionSetUIValue) (GNCOption *option,
                                     gboolean use_default);
typedef void (*GNCOptionSetSelectable) (GNCOption *option,
                                        gboolean selectable);
typedef void (*GNCOptionChangeCallback) (gpointer user_data);

/***** Prototypes ********************************************************/

gboolean gnc_option_get_changed (GNCOption *option);
void gnc_option_set_changed (GNCOption *option, gboolean changed);

gncUIWidget gnc_option_get_widget (GNCOption *option);
void gnc_option_set_widget (GNCOption *option, gncUIWidget widget);

SCM  gnc_option_get_ui_value(GNCOption *option);
void gnc_option_set_ui_value(GNCOption *option, gboolean use_default);
void gnc_option_set_selectable (GNCOption *option, gboolean selectable);

GNCOptionDB * gnc_option_db_new(SCM guile_options);
void          gnc_option_db_destroy(GNCOptionDB *odb);

/* Create an option DB for a particular type, and save/load from a kvp.
 * This assumes the gnc:*kvp-option-path* location for the options
 * in the kvp.
 */
GNCOptionDB * gnc_option_db_new_for_type(QofIdType id_type);
void gnc_option_db_load_from_kvp(GNCOptionDB* odb, kvp_frame *slots);
void gnc_option_db_save_to_kvp(GNCOptionDB* odb, kvp_frame *slots);

void gnc_register_kvp_option_generator(QofIdType id_type, SCM generator);
SCM gnc_make_kvp_options(QofIdType id_type);

void gnc_option_db_set_ui_callbacks (GNCOptionDB *odb,
                                     GNCOptionGetUIValue get_ui_value,
                                     GNCOptionSetUIValue set_ui_value,
                                     GNCOptionSetSelectable set_selectable);

SCM gnc_option_db_register_change_callback(GNCOptionDB *odb,
        GNCOptionChangeCallback callback,
        gpointer data,
        const char *section,
        const char *name);

void gnc_option_db_unregister_change_callback_id(GNCOptionDB *odb,
        SCM callback_id);

char * gnc_option_section(GNCOption *option);
char * gnc_option_name(GNCOption *option);
char * gnc_option_type(GNCOption *option);
char * gnc_option_sort_tag(GNCOption *option);
char * gnc_option_documentation(GNCOption *option);
SCM    gnc_option_getter(GNCOption *option);
SCM    gnc_option_setter(GNCOption *option);
SCM    gnc_option_default_getter(GNCOption *option);
SCM    gnc_option_value_validator(GNCOption *option);
SCM    gnc_option_widget_changed_proc_getter(GNCOption *option);
SCM    gnc_option_get_option_data(GNCOption *option);

int    gnc_option_num_permissible_values(GNCOption *option);
int    gnc_option_permissible_value_index(GNCOption *option, SCM value);
SCM    gnc_option_permissible_value(GNCOption *option, int index);
char * gnc_option_permissible_value_name(GNCOption *option, int index);
char * gnc_option_permissible_value_description(GNCOption *option, int index);

gboolean gnc_option_show_time(GNCOption *option);

gboolean gnc_option_multiple_selection(GNCOption *option);
GList * gnc_option_get_account_type_list(GNCOption *option);

gboolean gnc_option_get_range_info(GNCOption *option,
                                   double *lower_bound,
                                   double *upper_bound,
                                   int    *num_decimals,
                                   double *step_size);

gdouble  gnc_option_color_range(GNCOption *option);
gdouble  gnc_option_use_alpha(GNCOption *option);
guint32  gnc_option_get_color_argb(GNCOption *option);
gboolean gnc_option_get_color_info(GNCOption *option,
                                   gboolean use_default,
                                   gdouble *red,
                                   gdouble *green,
                                   gdouble *blue,
                                   gdouble *alpha);

void gnc_option_call_option_widget_changed_proc (GNCOption *option);

void gnc_option_set_default(GNCOption *option);

guint gnc_option_db_num_sections(GNCOptionDB *odb);

const char * gnc_option_section_name(GNCOptionSection *section);
guint  gnc_option_section_num_options(GNCOptionSection *section);

GNCOptionSection * gnc_option_db_get_section(GNCOptionDB *odb, gint i);

GNCOption * gnc_get_option_section_option(GNCOptionSection *section, int i);

GNCOption * gnc_option_db_get_option_by_name(GNCOptionDB *odb,
        const char *section_name,
        const char *name);

GNCOption * gnc_option_db_get_option_by_SCM(GNCOptionDB *odb,
        SCM guile_option);

gboolean gnc_option_db_dirty(GNCOptionDB *odb);
void     gnc_option_db_clean(GNCOptionDB *odb);

gboolean gnc_option_db_get_changed(GNCOptionDB *odb);
void gnc_option_db_commit(GNCOptionDB *odb);

char * gnc_option_db_get_default_section(GNCOptionDB *odb);

SCM gnc_option_db_lookup_option(GNCOptionDB *odb,
                                const char *section,
                                const char *name,
                                SCM default_value);

gboolean gnc_option_db_lookup_boolean_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        gboolean default_value);

char * gnc_option_db_lookup_string_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        const char *default_value);

char * gnc_option_db_lookup_font_option(GNCOptionDB *odb,
                                        const char *section,
                                        const char *name,
                                        const char *default_value);

char * gnc_option_db_lookup_multichoice_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        const char *default_value);

time_t gnc_option_db_lookup_date_option(GNCOptionDB *odb,
                                        const char *section,
                                        const char *name,
                                        gboolean *is_relative,
                                        Timespec *set_ab_value,
                                        char **set_rel_value,
                                        Timespec *default_value);

gdouble gnc_option_db_lookup_number_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        gdouble default_value);

gboolean gnc_option_db_lookup_color_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        gdouble *red,
        gdouble *green,
        gdouble *blue,
        gdouble *alpha);

guint32 gnc_option_db_lookup_color_option_argb(GNCOptionDB *odb,
        const char *section,
        const char *name,
        guint32 default_value);

GSList * gnc_option_db_lookup_list_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        GSList *default_value);

void gnc_free_list_option_value(GSList *list);

gnc_commodity *
gnc_option_db_lookup_currency_option(GNCOptionDB *odb,
                                     const char *section,
                                     const char *name,
                                     gnc_commodity *default_value);

void gnc_option_db_set_option_default(GNCOptionDB *odb,
                                      const char *section,
                                      const char *name);

gboolean gnc_option_db_set_option(GNCOptionDB *odb,
                                  const char *section,
                                  const char *name,
                                  SCM value);

gboolean gnc_option_db_set_number_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        gdouble value);

gboolean gnc_option_db_set_boolean_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        gboolean value);

gboolean gnc_option_db_set_string_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        const char *value);

char * gnc_option_date_option_get_subtype(GNCOption *option);

char * gnc_date_option_value_get_type (SCM option_value);
Timespec gnc_date_option_value_get_absolute (SCM option_value);
SCM gnc_date_option_value_get_relative (SCM option_value);

void gnc_option_db_set_option_selectable_by_name(SCM guile_options,
        const char *section,
        const char *name,
        gboolean selectable);

gboolean gnc_dateformat_option_value_parse(SCM value, QofDateFormat *format,
        GNCDateMonthFormat *months,
        gboolean *years, char **custom);
SCM gnc_dateformat_option_set_value(QofDateFormat format, GNCDateMonthFormat months,
                                    gboolean years, const char *custom);


void gnc_option_db_register_option(GNCOptionDBHandle handle,
                                   SCM guile_option);

/* private */
void gncp_option_invoke_callback(GNCOptionChangeCallback callback,
                                 gpointer data);

/* Reset all the widgets in one section to their default values */
void gnc_option_db_section_reset_widgets (GNCOptionSection *section);

/* Reset all the widgets to their default values */
void gnc_option_db_reset_widgets (GNCOptionDB *odb);

#endif /* OPTION_UTIL_H */
