/********************************************************************\
 * option-util.c -- GNOME<->guile option interface                  *
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

#include "top-level.h"

#include "guile-util.h"
#include "option-util.h"
#include "dialog-options.h"
#include "ui-callbacks.h"
#include "util.h"


/****** Structures *************************************************/

struct _GNCOptionSection
{
  char * section_name;

  GSList * options;
};

struct _GNCOptionDB
{
  GSList *option_sections;

  gboolean options_dirty;

  GSList *change_callbacks;
};

typedef struct _ChangeCBInfo ChangeCBInfo;
struct _ChangeCBInfo
{
  OptionChangeCallback callback;
  gpointer user_data;
};

typedef struct _Getters Getters;
struct _Getters
{
  SCM section;
  SCM name;
  SCM type;
  SCM sort_tag;
  SCM documentation;
  SCM getter;
  SCM setter;
  SCM default_getter;
  SCM value_validator;
  SCM permissible_values;
};


/****** Globals ****************************************************/

static Getters getters = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

/* This static indicates the debugging module this .o belongs to.  */
static short module = MOD_GUI;

static GPtrArray *option_dbs = NULL;

/*******************************************************************/


static GNCOptionDBHandle
gnc_option_db_get_handle(GNCOptionDB *odb)
{
  int i;

  for (i = 0; i < option_dbs->len; i++)
    if (odb == g_ptr_array_index(option_dbs, i))
      return i;

  return -1;
}


/********************************************************************\
 * gnc_option_db_new                                                *
 *   allocate a new option database and initialize its values       *
 *                                                                  *
 * Args: none                                                       *
 * Returns: a new option database                                   *
\********************************************************************/
GNCOptionDB *
gnc_option_db_new()
{
  GNCOptionDB *odb;

  odb = g_new0(GNCOptionDB, 1);

  odb->option_sections = NULL;
  odb->options_dirty = FALSE;
  odb->change_callbacks = NULL;

  if (option_dbs == NULL)
    option_dbs = g_ptr_array_new();

  g_ptr_array_add(option_dbs, odb);

  return odb;
}


/********************************************************************\
 * gnc_option_db_init                                               *
 *   initialize the options structures from the guile side          *
 *                                                                  *
 * Args: odb     - the option database to initialize                *
 *       options - the guile options to initialize with             * 
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_option_db_init(GNCOptionDB *odb, SCM options)
{
  SCM func = gh_eval_str("gnc:send-options");
  GNCOptionDBHandle handle;

  handle = gnc_option_db_get_handle(odb);
  if (handle < 0)
  {
    PERR("Can't find option database in list.\n");
    return;
  }

  gh_call2(func, gh_int2scm(handle), options);
}


static void
gnc_option_free_cbinfo(gpointer data, gpointer not_used)
{
  g_free(data);
}

/********************************************************************\
 * gnc_option_db_destroy                                            *
 *   unregister the scheme options and free all the memory          *
 *   associated with an option database, including the database     *
 *   itself                                                         *
 *                                                                  *
 * Args: options database to destroy                                *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_option_db_destroy(GNCOptionDB *odb)
{
  GSList *section_node;
  GSList *option_node;
  GNCOptionSection *section;
  GNCOption *option;

  if (odb == NULL)
    return;

  section_node = odb->option_sections;
  while (section_node != NULL)
  {
    section = section_node->data;

    option_node = section->options;
    while (option_node != NULL)
    {
      option = option_node->data;

      /* Should we check return value? */
      gnc_unregister_c_side_scheme_ptr(option->guile_option_id);

      option_node = option_node->next;
    }

    /* Free the option list */
    if (section->options != NULL)
      g_slist_free(section->options);

    section_node = section_node->next;
  }

  g_slist_foreach(odb->change_callbacks, gnc_option_free_cbinfo, NULL);

  g_slist_free(odb->option_sections);
  g_slist_free(odb->change_callbacks);

  odb->option_sections = NULL;
  odb->change_callbacks = NULL;
  odb->options_dirty = FALSE;

  if (!g_ptr_array_remove(option_dbs, odb))
  {
    PERR("Option database not present in list.\n");
  }

  if (option_dbs->len == 0)
  {
    g_ptr_array_free(option_dbs, FALSE);
    option_dbs = NULL;
  }

  g_free(odb);
}


/********************************************************************\
 * gnc_option_db_register_change_callback                           *
 *   register a callback to be called whenever an option changes    *
 *   this is rather heavy-weight, since all handlers will be called *
 *   whenever any option changes. We may need to refine it later.   *
 *                                                                  *
 * Args: odb      - the option database to register with            *
 *       callback - the callback function to register               *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_option_db_register_change_callback(GNCOptionDB *odb,
                                       OptionChangeCallback callback,
                                       gpointer data)
{
  ChangeCBInfo *cbinfo;

  assert(odb != NULL);

  cbinfo = g_new0(ChangeCBInfo, 1);
  cbinfo->callback = callback;
  cbinfo->user_data = data;

  odb->change_callbacks = g_slist_prepend(odb->change_callbacks, cbinfo);
}


static void
gnc_call_option_change_callbacks(GNCOptionDB *odb)
{
  GSList *node = odb->change_callbacks;
  ChangeCBInfo *cbinfo;

  while (node != NULL)
  {
    cbinfo = node->data;
    (cbinfo->callback)(cbinfo->user_data);

    node = node->next;
  }
}


static void
initialize_getters()
{
  static gboolean getters_initialized = FALSE;

  if (getters_initialized)
    return;

  getters.section = gh_eval_str("gnc:option-section");
  getters.name = gh_eval_str("gnc:option-name");
  getters.type = gh_eval_str("gnc:option-type");
  getters.sort_tag = gh_eval_str("gnc:option-sort-tag");
  getters.documentation =
    gh_eval_str("gnc:option-documentation");
  getters.getter = gh_eval_str("gnc:option-getter");
  getters.setter = gh_eval_str("gnc:option-setter");
  getters.default_getter =
    gh_eval_str("gnc:option-default-getter");
  getters.value_validator =
    gh_eval_str("gnc:option-value-validator");
  getters.permissible_values =
    gh_eval_str("gnc:option-permissible-values");
    
  getters_initialized = TRUE;
}


/********************************************************************\
 * gnc_option_section                                               *
 *   returns the malloc'ed section name of the option, or NULL      *
 *   if it can't be retrieved.                                      *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: malloc'ed char * or NULL                                *
\********************************************************************/
char *
gnc_option_section(GNCOption *option)
{
  initialize_getters();

  return gnc_guile_call1_to_string(getters.section, option->guile_option);
}


/********************************************************************\
 * gnc_option_name                                                  *
 *   returns the malloc'ed name of the option, or NULL              *
 *   if it can't be retrieved.                                      *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: malloc'ed char * or NULL                                *
\********************************************************************/
char *
gnc_option_name(GNCOption *option)
{
  initialize_getters();

  return gnc_guile_call1_to_string(getters.name, option->guile_option);
}


/********************************************************************\
 * gnc_option_type                                                  *
 *   returns the malloc'ed type of the option, or NULL              *
 *   if it can't be retrieved.                                      *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: malloc'ed char * or NULL                                *
\********************************************************************/
char *
gnc_option_type(GNCOption *option)
{
  initialize_getters();

  return gnc_guile_call1_symbol_to_string(getters.type,
					  option->guile_option);
}


/********************************************************************\
 * gnc_option_sort_tag                                              *
 *   returns the malloc'ed sort tag of the option, or NULL          *
 *   if it can't be retrieved.                                      *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: malloc'ed char * or NULL                                *
\********************************************************************/
char *
gnc_option_sort_tag(GNCOption *option)
{
  initialize_getters();

  return gnc_guile_call1_to_string(getters.sort_tag, option->guile_option);
}


/********************************************************************\
 * gnc_option_documentation                                         *
 *   returns the malloc'ed sort tag of the option, or NULL          *
 *   if it can't be retrieved.                                      *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: malloc'ed char * or NULL                                *
\********************************************************************/
char *
gnc_option_documentation(GNCOption *option)
{
  initialize_getters();

  return gnc_guile_call1_to_string(getters.documentation,
				   option->guile_option);
}


/********************************************************************\
 * gnc_option_getter                                                *
 *   returns the SCM handle for the option getter function.         *
 *   This value should be tested with gh_procedure_p before use.    *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: SCM handle to function                                  *
\********************************************************************/
SCM
gnc_option_getter(GNCOption *option)
{
  initialize_getters();

  return gnc_guile_call1_to_procedure(getters.getter,
				      option->guile_option);
}


/********************************************************************\
 * gnc_option_setter                                                *
 *   returns the SCM handle for the option setter function.         *
 *   This value should be tested with gh_procedure_p before use.    *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: SCM handle to function                                  *
\********************************************************************/
SCM
gnc_option_setter(GNCOption *option)
{
  initialize_getters();

  return gnc_guile_call1_to_procedure(getters.setter,
				      option->guile_option);
}


/********************************************************************\
 * gnc_option_default_getter                                        *
 *   returns the SCM handle for the option default_getter function. *
 *   This value should be tested with gh_procedure_p before use.    *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: SCM handle to function                                  *
\********************************************************************/
SCM
gnc_option_default_getter(GNCOption *option)
{
  initialize_getters();

  return gnc_guile_call1_to_procedure(getters.default_getter,
				      option->guile_option);
}


/********************************************************************\
 * gnc_option_value_validator                                       *
 *   returns the SCM handle for the option value validator function.*
 *   This value should be tested with gh_procedure_p before use.    *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: SCM handle to function                                  *
\********************************************************************/
SCM
gnc_option_value_validator(GNCOption *option)
{
  initialize_getters();

  return gnc_guile_call1_to_procedure(getters.value_validator,
				      option->guile_option);
}


/********************************************************************\
 * gnc_option_value_num_permissible_values                          *
 *   returns the number of permissible values in the option, or     *
 *   -1 if there are no values available.                           *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: number of permissible options or -1                     *
\********************************************************************/
int
gnc_option_value_num_permissible_values(GNCOption *option)
{
  SCM values;

  initialize_getters();

  values = gnc_guile_call1_to_list(getters.permissible_values,
                                   option->guile_option);

  if (values == SCM_UNDEFINED)
    return -1;

  return gh_length(values);
}


/********************************************************************\
 * gnc_option_value_permissible_value_index                         *
 *   returns the index of the permissible value matching the        *
 *   provided value, or -1 if it couldn't be found                  *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 *       value  - the SCM handle of the value                       *
 * Returns: index of permissible value, or -1                       *
\********************************************************************/
int
gnc_option_value_permissible_value_index(GNCOption *option, SCM search_value)
{
  SCM values, vector, value;
  int num_values, i;

  if (!gh_symbol_p(search_value))
    return -1;

  initialize_getters();

  values = gnc_guile_call1_to_list(getters.permissible_values,
                                   option->guile_option);

  if (values == SCM_UNDEFINED)
    return -1;

  num_values = gh_length(values);

  for (i = 0; i < num_values; i++)
  {
    vector = gh_list_ref(values, gh_int2scm(i));
    if (!gh_vector_p(vector))
      continue;

    value = gh_vector_ref(vector, gh_int2scm(0));

    if (gh_eq_p(value, search_value))
      return i;
  }

  return -1;
}


/********************************************************************\
 * gnc_option_value_permissible_value                               *
 *   returns the SCM handle to the indexth permissible value in the *
 *   option, or SCM_UNDEFINED if the index was out of range or      *
 *   there was some other problem.                                  *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 *       index  - the index of the permissible value                *
 * Returns: SCM handle to option value or SCM_UNDEFINED             *
\********************************************************************/
SCM
gnc_option_value_permissible_value(GNCOption *option, int index)
{
  SCM values, vector, value;

  if (index < 0)
    return SCM_UNDEFINED;

  initialize_getters();

  values = gnc_guile_call1_to_list(getters.permissible_values,
                                   option->guile_option);

  if (values == SCM_UNDEFINED)
    return SCM_UNDEFINED;

  if (index >= gh_length(values))
    return SCM_UNDEFINED;

  vector = gh_list_ref(values, gh_int2scm(index));
  if (!gh_vector_p(vector))
    return SCM_UNDEFINED;

  value = gh_vector_ref(vector, gh_int2scm(0));
  if (!gh_symbol_p(value))
    return SCM_UNDEFINED;

  return value;
}


/********************************************************************\
 * gnc_option_value_permissible_value_name                          *
 *   returns the malloc'd name of the indexth permissible value in  *
 *   the option, or NULL if the index was out of range or there are *
 *   no values available.                                           *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 *       index  - the index of the permissible value                *
 * Returns: malloc'd name of permissible value or NULL              *
\********************************************************************/
char *
gnc_option_value_permissible_value_name(GNCOption *option, int index)
{
  SCM values, vector, name;

  if (index < 0)
    return NULL;

  initialize_getters();

  values = gnc_guile_call1_to_list(getters.permissible_values,
                                   option->guile_option);

  if (values == SCM_UNDEFINED)
    return NULL;

  if (index >= gh_length(values))
    return NULL;

  vector = gh_list_ref(values, gh_int2scm(index));
  if (!gh_vector_p(vector))
    return NULL;

  name = gh_vector_ref(vector, gh_int2scm(1));
  if (!gh_string_p(name))
    return NULL;

  return gh_scm2newstr(name, NULL);
}


/********************************************************************\
 * gnc_option_value_permissible_value_description                   *
 *   returns the malloc'd description of the indexth permissible    *
 *   value in the option, or NULL if the index was out of range or  *
 *   there are no values available.                                 *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 *       index  - the index of the permissible value                *
 * Returns: malloc'd description of permissible value or NULL       *
\********************************************************************/
char *
gnc_option_value_permissible_value_description(GNCOption *option, int index)
{
  SCM values, vector, help;

  if (index < 0)
    return NULL;

  initialize_getters();

  values = gnc_guile_call1_to_list(getters.permissible_values,
                                   option->guile_option);

  if (values == SCM_UNDEFINED)
    return NULL;

  if (index >= gh_length(values))
    return NULL;

  vector = gh_list_ref(values, gh_int2scm(index));
  if (!gh_vector_p(vector))
    return NULL;

  help = gh_vector_ref(vector, gh_int2scm(2));
  if (!gh_string_p(help))
    return NULL;

  return gh_scm2newstr(help, NULL);
}


static gint
compare_sections(gconstpointer a, gconstpointer b)
{
  const GNCOptionSection *sa = a;
  const GNCOptionSection *sb = b;

  return safe_strcmp(sa->section_name, sb->section_name);
}

static gint
compare_option_tags(gconstpointer a, gconstpointer b)
{
  GNCOption *oa = (GNCOption *) a;
  GNCOption *ob = (GNCOption *) b;
  char *tag_a = gnc_option_sort_tag(oa);
  char *tag_b = gnc_option_sort_tag(ob);
  gint result;

  result = safe_strcmp(tag_a, tag_b);

  if (tag_a != NULL)
    free(tag_a);

  if (tag_b != NULL)
    free(tag_b);

  return result;
}

#if 0
static gint
compare_option_names(gconstpointer a, gconstpointer b)
{
  GNCOption *oa = (GNCOption *) a;
  GNCOption *ob = (GNCOption *) b;
  char *name_a = gnc_option_name(oa);
  char *name_b = gnc_option_name(ob);
  gint result;

  result = safe_strcmp(name_a, name_b);

  if (name_a != NULL)
    free(name_a);

  if (name_b != NULL)
    free(name_b);

  return result;
}
#endif


/********************************************************************\
 * gnc_option_db_dirty                                              *
 *   returns true if guile has registered more options into the     *
 *   database since the last time the database was cleaned.         *
 *                                                                  *
 * Returns: dirty flag                                              *
\********************************************************************/
gboolean
gnc_option_db_dirty(GNCOptionDB *odb)
{
  assert(odb != NULL);

  return odb->options_dirty;
}


/********************************************************************\
 * gnc_option_db_clean                                              *
 *   resets the dirty flag of the option database                   *
 *                                                                  *
\********************************************************************/
void
gnc_option_db_clean(GNCOptionDB *odb)
{
  assert(odb != NULL);

  odb->options_dirty = FALSE;
}


/********************************************************************\
 * _gnc_option_db_register_option                                   *
 *   registers an option with an option database. Intended to be    *
 *   called from guile.                                             *
 *                                                                  *
 * Args: odb    - the option database                               *
 *       option - the guile option                                  *
 * Returns: nothing                                                 *
\********************************************************************/
void
_gnc_option_db_register_option(GNCOptionDBHandle handle, SCM guile_option)
{
  GNCOptionDB *odb;
  GNCOption *option;
  GNCOptionSection *section;

  odb = g_ptr_array_index(option_dbs, handle);

  assert(odb != NULL);

  odb->options_dirty = TRUE;

  /* Make the option structure */
  option = g_new0(GNCOption, 1);
  option->guile_option = guile_option;
  option->changed = FALSE;
  option->widget = NULL;

  /* Prevent guile from garbage collecting the option */
  option->guile_option_id = gnc_register_c_side_scheme_ptr(guile_option);
  if (option->guile_option_id == SCM_UNDEFINED)
  {
    g_free(option);
    PERR("_gnc_option_db_register_option: couldn't register\n");
    return;
  }

  /* Make the section structure */
  section = g_new0(GNCOptionSection, 1);
  section->section_name = gnc_option_section(option);
  section->options = NULL;

  /* See if the section is already there */
  {
    GSList *old;

    old = g_slist_find_custom(odb->option_sections, section, compare_sections);

    if (old != NULL)
    {
      if (section->section_name != NULL)
	free(section->section_name);
      g_free(section);
      section = old->data;
    }
    else
      odb->option_sections = g_slist_insert_sorted(odb->option_sections,
                                                   section, compare_sections);
  }

  section->options = g_slist_insert_sorted(section->options, option,
					   compare_option_tags);
}


/********************************************************************\
 * gnc_option_db_num_sections                                       *
 *   returns the number of option sections registered so far in the *
 *   database                                                       *
 *                                                                  *
 * Args: odb - the database to count sections for                   *
 * Returns: number of option sections                               *
\********************************************************************/
guint
gnc_option_db_num_sections(GNCOptionDB *odb)
{
  return g_slist_length(odb->option_sections);
}


/********************************************************************\
 * gnc_option_db_get_section                                        *
 *   returns the ith option section in the database, or NULL        *
 *                                                                  *
 * Args: odb - the option database                                  *
 *       i   - index of section                                     *
 * Returns: ith option sectioin                                     *
\********************************************************************/
GNCOptionSection *
gnc_option_db_get_section(GNCOptionDB *odb, gint i)
{
  return g_slist_nth_data(odb->option_sections, i);
}


/********************************************************************\
 * gnc_option_section_name                                          *
 *   returns the name of the options section                        *
 *                                                                  *
 * Args: section - section to get name of                           *
 * Returns: name of option section                                  *
\********************************************************************/
char *
gnc_option_section_name(GNCOptionSection *section)
{
  return section->section_name;
}


/********************************************************************\
 * gnc_option_section_num_options                                   *
 *   returns the number of options in a given section               *
 *                                                                  *
 * Args: section - section to count options for                     *
 * Returns: number of options in section                            *
\********************************************************************/
guint
gnc_option_section_num_options(GNCOptionSection *section)
{
  return g_slist_length(section->options);
}


/********************************************************************\
 * gnc_get_option_section_option                                    *
 *   returns the ith option in a given section                      *
 *                                                                  *
 * Args: section - section to retrieve option for                   *
 *       i       - index of option                                  *
 * Returns: ith option in section                                   *
\********************************************************************/
GNCOption *

gnc_get_option_section_option(GNCOptionSection *section, int i)
{
  return g_slist_nth_data(section->options, i);
}


/********************************************************************\
 * gnc_option_db_get_option_by_name                                 *
 *   returns an option given section name and name                  *
 *                                                                  *
 * Args: odb          - option database to search in                *
 *       section_name - name of section to search for               *
 *       name         - name to search for                          *
 * Returns: given option, or NULL if none                           *
\********************************************************************/
GNCOption *
gnc_option_db_get_option_by_name(GNCOptionDB *odb, char *section_name,
                                 char *name)
{
  GSList *section_node;
  GSList *option_node;
  GNCOptionSection section_key;
  GNCOptionSection *section;
  GNCOption *option;
  gint result;
  char *node_name;

  section_key.section_name = section_name;

  section_node = g_slist_find_custom(odb->option_sections, &section_key,
				     compare_sections);

  if (section_node == NULL)
    return NULL;

  section = section_node->data;
  option_node = section->options;

  while (option_node != NULL)
  {
    option = option_node->data;

    node_name = gnc_option_name(option);
    result = safe_strcmp(name, node_name);
    free(node_name);

    if (result == 0)
      return option;

    option_node = option_node->next;
  }

  return NULL;
}


/********************************************************************\
 * gnc_option_db_get_option_by_SCM                                  *
 *   returns an option given SCM handle. Uses section and name.     *
 *                                                                  *
 * Args: odb          - option database to search in                *
 *       guile_option - SCM handle of option                        *
 * Returns: given option, or NULL if none                           *
\********************************************************************/
GNCOption *
gnc_option_db_get_option_by_SCM(GNCOptionDB *odb, SCM guile_option)
{
  GNCOption option_key;
  GNCOption *option;
  char *section_name;
  char *name;

  option_key.guile_option = guile_option;

  section_name = gnc_option_section(&option_key);
  name = gnc_option_name(&option_key);

  option = gnc_option_db_get_option_by_name(odb, section_name, name);

  if (section_name != NULL)
    free(section_name);

  if (name != NULL)
    free(name);

  return option;
}


static void
gnc_commit_option(GNCOption *option)
{
  SCM validator, setter, value;
  SCM result, ok;

  /* Validate the ui's value */
  value = gnc_option_get_ui_value(option);
  validator = gnc_option_value_validator(option);

  result = gh_call1(validator, value);
  if (!gh_list_p(result))
  {
    PERR("gnc_commit_option: bad validation result\n");
    return;
  }

  /* First element determines validity */
  ok = gh_car(result);
  if (!gh_boolean_p(ok))
  {
    PERR("gnc_commit_option: bad validation result\n");
    return;
  }

  if (gh_scm2bool(ok))
  {
    /* Second element is value to use */
    value = gh_cadr(result);
    setter = gnc_option_setter(option);

    gh_call1(setter, value);
  }
  else
  {
    SCM oops;
    char *section, *name, *message, *full;

    /* Second element is error message */
    oops = gh_cadr(result);
    if (!gh_string_p(oops))
    {
      PERR("gnc_commit_option: bad validation result\n");
      return;
    }

    message = gh_scm2newstr(oops, NULL);
    name = gnc_option_name(option);
    section = gnc_option_section(option);

    full = g_strdup_printf("There is a problem with option %s:%s.\n%s",
			   section, name, message);

    gnc_error_dialog(full);

    g_free(full);

    if (message != NULL)
      free(message);
    if (name != NULL)
      free(name);
    if (section != NULL)
      free(section);
  }
}


/********************************************************************\
 * gnc_option_db_commit                                             *
 *   commits the options which have changed, and which are valid    *
 *   for those which are not valid, error dialogs are shown.        *
 *                                                                  *
 * Args: odb - option database to commit                            *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_option_db_commit(GNCOptionDB *odb)
{
  GSList *section_node;
  GSList *option_node;
  GNCOptionSection *section;
  GNCOption *option;
  gboolean changed_something = FALSE;

  assert(odb != NULL);

  section_node = odb->option_sections;
  while (section_node != NULL)
  {
    section = section_node->data;

    option_node = section->options;
    while (option_node != NULL)
    {
      option = option_node->data;

      if (option->changed)
      {
	gnc_commit_option(option_node->data);
	changed_something = TRUE;
	option->changed = FALSE;
      }

      option_node = option_node->next;
    }

    section_node = section_node->next;
  }

  if (changed_something)
    gnc_call_option_change_callbacks(odb);
}


/********************************************************************\
 * gnc_option_db_lookup_boolean_option                              *
 *   looks up a boolean option. If present, returns its value,      *
 *   otherwise returns the default.                                 *
 *                                                                  *
 * Args: odb     - option database to search in                     *
 *       section - section name of option                           *
 *       name    - name of option                                   *
 *       default - default value if not found                       *
 * Return: gboolean option value                                    *
\********************************************************************/
gboolean
gnc_option_db_lookup_boolean_option(GNCOptionDB *odb, char *section,
                                    char *name, gboolean default_value)
{
  GNCOption *option;
  SCM getter;
  SCM value;

  option = gnc_option_db_get_option_by_name(odb, section, name);

  if (option == NULL)
    return default_value;

  getter = gnc_option_getter(option);
  if (getter == SCM_UNDEFINED)
    return default_value;

  value = gh_call0(getter);

  if (gh_boolean_p(value))
    return gh_scm2bool(value);
  else
    return default_value;
}


/********************************************************************\
 * gnc_option_db_lookup_string_option                               *
 *   looks up a string option. If present, returns its malloc'ed    *
 *   value, otherwise returns the strdup'ed default, or NULL if     *
 *   default was NULL.                                              *
 *                                                                  *
 * Args: odb     - option database to search in                     *
 *       section - section name of option                           *
 *       name    - name of option                                   *
 *       default - default value if not found                       *
 * Return: char * option value                                      *
\********************************************************************/
char *
gnc_option_db_lookup_string_option(GNCOptionDB *odb, char *section,
                                   char *name, char *default_value)
{
  GNCOption *option;
  SCM getter;
  SCM value;

  option = gnc_option_db_get_option_by_name(odb, section, name);

  if (option != NULL)
  {
    getter = gnc_option_getter(option);
    if (getter != SCM_UNDEFINED)
    {
      value = gh_call0(getter);
      if (gh_string_p(value))
	return gh_scm2newstr(value, NULL);
    }
  }

  if (default_value == NULL)
    return NULL;

  return strdup(default_value);
}


/********************************************************************\
 * gnc_option_db_lookup_multichoice_option                          *
 *   looks up a multichoice option. If present, returns its         *
 *   name as a malloc'ed string                                     *
 *   value, otherwise returns the strdup'ed default, or NULL if     *
 *   default was NULL.                                              *
 *                                                                  *
 * Args: odb     - option database to search in                     *
 *       section - section name of option                           *
 *       name    - name of option                                   *
 *       default - default value if not found                       *
 * Return: char * option value                                      *
\********************************************************************/
char *
gnc_option_db_lookup_multichoice_option(GNCOptionDB *odb, char *section,
                                        char *name, char *default_value)
{
  GNCOption *option;
  SCM getter;
  SCM value;

  option = gnc_option_db_get_option_by_name(odb, section, name);

  if (option != NULL)
  {
    getter = gnc_option_getter(option);
    if (getter != SCM_UNDEFINED)
    {
      value = gh_call0(getter);
      if (gh_symbol_p(value))
	return gh_symbol2newstr(value, NULL);
    }
  }

  if (default_value == NULL)
    return NULL;

  return strdup(default_value);
}
