/********************************************************************\
 * option-util.h -- GNOME<->guile option interface                  *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef __OPTION_UTIL_H__
#define __OPTION_UTIL_H__

#include "config.h"

#include <gnome.h>
#include <guile/gh.h>

#include "gnc-common.h"
#include "date.h"


typedef struct _GNCOption GNCOption;
struct _GNCOption
{
  /* Handle to the scheme-side option */
  SCM guile_option;

  /* Identifier for unregistering */
  SCM guile_option_id;

  /* Flag to indicate change by the UI */
  gboolean changed;

  /* The widget which is holding this option */
  GtkWidget *widget;
};

typedef struct _GNCOptionSection GNCOptionSection;
typedef struct _GNCOptionDB GNCOptionDB;

typedef int GNCOptionDBHandle;

typedef void (*OptionChangeCallback)(void * user_data);

/***** Prototypes ********************************************************/

GNCOptionDB * gnc_option_db_new(SCM guile_options);
void          gnc_option_db_destroy(GNCOptionDB *odb);

SCM gnc_option_db_register_change_callback(GNCOptionDB *odb,
                                           OptionChangeCallback callback,
                                           void *data,
                                           char *section,
                                           char *name);

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

int    gnc_option_num_permissible_values(GNCOption *option);
int    gnc_option_permissible_value_index(GNCOption *option, SCM value);
SCM    gnc_option_permissible_value(GNCOption *option, int index);
char * gnc_option_permissible_value_name(GNCOption *option, int index);
char * gnc_option_permissible_value_description(GNCOption *option, int index);

gboolean gnc_option_show_time(GNCOption *option);

gboolean gnc_option_multiple_selection(GNCOption *option);

gboolean gnc_option_get_range_info(GNCOption *option,
                                   double *lower_bound,
                                   double *upper_bound,
                                   int    *num_decimals,
                                   double *step_size);

gdouble  gnc_option_color_range(GNCOption *option);
gdouble  gnc_option_use_alpha(GNCOption *option);
uint32   gnc_option_get_color_argb(GNCOption *option);
gboolean gnc_option_get_color_info(GNCOption *option,
                                   gboolean use_default,
                                   gdouble *red,
                                   gdouble *green,
                                   gdouble *blue,
                                   gdouble *alpha);

void gnc_option_set_default(GNCOption *option);

guint gnc_option_db_num_sections(GNCOptionDB *odb);

char * gnc_option_section_name(GNCOptionSection *section);
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

void gnc_option_db_commit(GNCOptionDB *odb);

gboolean gnc_option_db_lookup_boolean_option(GNCOptionDB *odb,
                                             const char *section,
                                             const char *name,
                                             gboolean default_value);

char * gnc_option_db_lookup_string_option(GNCOptionDB *odb,
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
                                        Timespec *set_value,
                                        Timespec *default_value);

gdouble gnc_option_db_lookup_number_range_option(GNCOptionDB *odb,
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

uint32 gnc_option_db_lookup_color_option_argb(GNCOptionDB *odb,
                                              const char *section,
                                              const char *name,
                                              uint32 default_value);

void gnc_option_db_set_option_default(GNCOptionDB *odb,
                                      const char *section,
                                      const char *name);

gboolean gnc_option_db_set_number_range_option(GNCOptionDB *odb,
                                               const char *section,
                                               const char *name,
                                               gdouble value);

/* private */

void _gnc_option_db_register_option(GNCOptionDBHandle handle,
                                    SCM guile_option);

void _gnc_option_invoke_callback(OptionChangeCallback callback, void *data);

/* These should be in src/guile or src/g-wrap, but they use glib */

SCM     gnc_account_list_to_scm(GList *account_list);
GList * gnc_scm_to_account_list(SCM scm_list);

SCM _gnc_get_current_accounts();

#endif /* __OPTION_UTIL_H__ */
