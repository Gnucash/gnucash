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
};


/****** Globals ****************************************************/

static Getters getters = {0, 0, 0, 0, 0, 0, 0, 0, 0};

static GSList *option_sections = NULL;
static gboolean options_dirty = FALSE;

static GSList *change_callbacks = NULL;

/* This static indicates the debugging module this .o belongs to.  */
static short module = MOD_GUI;

/*******************************************************************/


/********************************************************************\
 * gnc_options_init                                                 *
 *   initialize the options structures from the guile side          *
 *                                                                  *
 * Args: none                                                       *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_options_init()
{
  SCM func = gh_eval_str("gnc:send-ui-options");

  option_sections = NULL;
  options_dirty = FALSE;
  change_callbacks = NULL;

  if (gh_procedure_p(func))
    gh_call0(func);
  else
  {
    PERR("gnc_options_init: no guile options!");
  }
}


/********************************************************************\
 * gnc_options_shutdown                                             *
 *   unregister the scheme options and free the structure memory    *
 *                                                                  *
 * Args: none                                                       *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_options_shutdown()
{
  GSList *section_node;
  GSList *option_node;
  GNCOptionSection *section;
  GNCOption *option;

  section_node = option_sections;
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

  if (option_sections != NULL)
    g_slist_free(section->options);

  option_sections = NULL;
  options_dirty = FALSE;
}


/********************************************************************\
 * gnc_register_option_change_callback                              *
 *   register a callback to be called whenever an option changes    *
 *   this is rather heavy-weight, since all handlers will be called *
 *   whenever any option changes. We may need to refine it later.   *
 *                                                                  *
 * Args: callback - the callback function                           *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_register_option_change_callback(void (*callback) (void))
{
  change_callbacks = g_slist_prepend(change_callbacks, callback);
}

static void
gnc_call_option_change_callbacks()
{
  GSList *node = change_callbacks;
  void (*callback) (void);

  while (node != NULL)
  {
    callback = node->data;
    (*callback)();

    node = node->next;
  }
}

static void
initialize_getters()
{
  static gboolean getters_initialized = FALSE;

  if (getters_initialized)
    return;

  getters.section = gh_eval_str("gnc:configuration-option-section");
  getters.name = gh_eval_str("gnc:configuration-option-name");
  getters.type = gh_eval_str("gnc:configuration-option-type");
  getters.sort_tag = gh_eval_str("gnc:configuration-option-sort-tag");
  getters.documentation =
    gh_eval_str("gnc:configuration-option-documentation");
  getters.getter = gh_eval_str("gnc:configuration-option-getter");
  getters.setter = gh_eval_str("gnc:configuration-option-setter");
  getters.default_getter =
    gh_eval_str("gnc:configuration-option-default-getter");
  getters.value_validator =
    gh_eval_str("gnc:configuration-option-value-validator");
    
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
 * gnc_options_dirty                                                *
 *   returns true if guile has registered more options since the    *
 *   options dialog was created.                                    *
 *                                                                  *
 * Returns: dirty flag                                              *
\********************************************************************/
gboolean
gnc_options_dirty()
{
  return options_dirty;
}


/********************************************************************\
 * gnc_options_clean                                                *
 *   resets the dirty flag                                          *
 *                                                                  *
\********************************************************************/
void
gnc_options_clean()
{
  options_dirty = FALSE;
}


/********************************************************************\
 * gnc_register_option_ui                                           *
 *   registers an option with the GUI. Intended to be called from   *
 *   guile.                                                         *
 *                                                                  *
 * Args: option - the guile option                                  *
 * Returns: nothing                                                 *
\********************************************************************/
void
_gnc_register_option_ui(SCM guile_option)
{
  GNCOption *option;
  GNCOptionSection *section;

  options_dirty = TRUE;

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
    PERR("_gnc_register_option_ui: couldn't register\n");
    return;
  }

  /* Make the section structure */
  section = g_new0(GNCOptionSection, 1);
  section->section_name = gnc_option_section(option);
  section->options = NULL;

  /* See if the section is already there */
  {
    GSList *old;

    old = g_slist_find_custom(option_sections, section, compare_sections);

    if (old != NULL)
    {
      if (section->section_name != NULL)
	free(section->section_name);
      g_free(section);
      section = old->data;
    }
    else
      option_sections = g_slist_insert_sorted(option_sections, section,
					      compare_sections);
  }

  section->options = g_slist_insert_sorted(section->options, option,
					   compare_option_tags);
}


/********************************************************************\
 * gnc_num_options_sections                                         *
 *   returns the number of option sections registered so far        *
 *                                                                  *
 * Args: none                                                       *
 * Returns: number of option sections                               *
\********************************************************************/
guint
gnc_num_option_sections()
{
  return g_slist_length(option_sections);
}


/********************************************************************\
 * gnc_get_option_section                                           *
 *   returns the ith option section, or NULL                        *
 *                                                                  *
 * Args: i - index of section                                       *
 * Returns: ith option sectioin                                     *
\********************************************************************/
GNCOptionSection *
gnc_get_option_section(gint i)
{
  return g_slist_nth_data(option_sections, i);
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
 * gnc_get_option_by_name                                           *
 *   returns an option given section name and name                  *
 *                                                                  *
 * Args: section_name - name of section to search for               *
 *       name         - name to search for                          *
 * Returns: given option, or NULL if none                           *
\********************************************************************/
GNCOption *
gnc_get_option_by_name(char *section_name, char *name)
{
  GSList *section_node;
  GSList *option_node;
  GNCOptionSection section_key;
  GNCOptionSection *section;
  GNCOption *option;
  gint result;
  char *node_name;

  section_key.section_name = section_name;

  section_node = g_slist_find_custom(option_sections, &section_key,
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
 * gnc_get_option_by_SCM                                            *
 *   returns an option given SCM handle. Uses section and name.     *
 *                                                                  *
 * Args: guile_option - SCM handle of option                        *
 * Returns: given option, or NULL if none                           *
\********************************************************************/
GNCOption *
gnc_get_option_by_SCM(SCM guile_option)
{
  GNCOption option_key;
  GNCOption *option;
  char *section_name;
  char *name;

  option_key.guile_option = guile_option;

  section_name = gnc_option_section(&option_key);
  name = gnc_option_name(&option_key);

  option = gnc_get_option_by_name(section_name, name);

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
 * gnc_options_commit                                               *
 *   commits the options which have changed, and which are valid    *
 *   for those which are not valid, error dialogs are shown.        *
 *                                                                  *
 * Args: none                                                       *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_options_commit()
{
  GSList *section_node;
  GSList *option_node;
  GNCOptionSection *section;
  GNCOption *option;
  gboolean changed_something = FALSE;

  section_node = option_sections;
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
    gnc_call_option_change_callbacks();
}


/********************************************************************\
 * gnc_lookup_boolean_option                                        *
 *   looks up a boolean option. If present, returns its value,      *
 *   otherwise returns the default.                                 *
 *                                                                  *
 * Args: section - section name of option                           *
 *       name    - name of option                                   *
 *       default - default value if not found                       *
 * Return: gboolean option value                                    *
\********************************************************************/
gboolean
gnc_lookup_boolean_option(char *section, char *name, gboolean default_value)
{
  GNCOption *option;
  SCM getter;
  SCM value;

  option = gnc_get_option_by_name(section, name);

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
 * gnc_lookup_string_option                                         *
 *   looks up a string option. If present, returns its malloc'ed    *
 *   value, otherwise returns the strdup'ed default, or NULL if     *
 *   default was NULL.                                              *
 *                                                                  *
 * Args: section - section name of option                           *
 *       name    - name of option                                   *
 *       default - default value if not found                       *
 * Return: char * option value                                      *
\********************************************************************/
char *
gnc_lookup_string_option(char *section, char *name, char *default_value)
{
  GNCOption *option;
  SCM getter;
  SCM value;

  option = gnc_get_option_by_name(section, name);

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
