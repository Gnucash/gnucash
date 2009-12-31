/********************************************************************\
 * option-util.c -- GNOME<->guile option interface                  *
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <time.h>
#include <string.h>

#include "option-util.h"
#include "engine-helpers.h"
#include "glib-helpers.h"
#include "guile-util.h"
#include "qof.h"
#include "guile-mappings.h"

#include "swig-runtime.h"

/* TODO:

  - for make-date-option, there seems to be only support for getting,
    not for setting.
*/


/****** Structures *************************************************/

struct gnc_option
{
    /* Handle to the scheme-side option */
    SCM guile_option;

    /* Flag to indicate change by the UI */
    gboolean changed;

    /* The widget which is holding this option */
    gncUIWidget widget;

    /* The option db which holds this option */
    GNCOptionDB *odb;
};

struct gnc_option_section
{
    char * section_name;

    GSList * options;
};

struct gnc_option_db
{
    SCM guile_options;

    GSList *option_sections;

    gboolean options_dirty;

    GNCOptionDBHandle handle;

    GNCOptionGetUIValue get_ui_value;
    GNCOptionSetUIValue set_ui_value;
    GNCOptionSetSelectable set_selectable;
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
    SCM option_data;
    SCM index_to_name;
    SCM index_to_description;
    SCM index_to_value;
    SCM value_to_index;
    SCM number_of_indices;
    SCM option_widget_changed_cb;
    SCM date_option_subtype;
    SCM date_option_show_time;
    SCM date_option_value_type;
    SCM date_option_value_absolute;
    SCM date_option_value_relative;
};


/****** Globals ****************************************************/

static Getters getters = {0, 0, 0, 0, 0, 0, 0, 0, 0,
                          0, 0, 0, 0, 0, 0, 0, 0, 0
                         };

/* This static indicates the debugging module this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static GHashTable *option_dbs = NULL;
static int last_db_handle = 0;


/*******************************************************************/

gboolean
gnc_option_get_changed (GNCOption *option)
{
    if (!option) return FALSE;
    return option->changed;
}

void
gnc_option_set_changed (GNCOption *option, gboolean changed)
{
    g_return_if_fail (option != NULL);
    option->changed = changed;
}

gncUIWidget
gnc_option_get_widget (GNCOption *option)
{
    if (!option) return NULL;
    return option->widget;
}

void
gnc_option_set_widget (GNCOption *option, gncUIWidget widget)
{
    g_return_if_fail (option != NULL);
    option->widget = widget;
}

SCM
gnc_option_get_ui_value (GNCOption *option)
{
    g_return_val_if_fail (option != NULL, SCM_UNDEFINED);
    g_return_val_if_fail (option->odb != NULL, SCM_UNDEFINED);
    g_return_val_if_fail (option->odb->get_ui_value != NULL, SCM_UNDEFINED);

    return option->odb->get_ui_value (option);
}

void
gnc_option_set_ui_value (GNCOption *option, gboolean use_default)
{
    g_return_if_fail (option != NULL);
    g_return_if_fail (option->odb != NULL);

    if (!option->odb->set_ui_value)
        return;

    option->odb->set_ui_value (option, use_default);
}

void
gnc_option_set_selectable (GNCOption *option, gboolean selectable)
{
    g_return_if_fail (option != NULL);
    g_return_if_fail (option->odb != NULL);
    g_return_if_fail (option->odb->set_selectable != NULL);

    option->odb->set_selectable (option, selectable);
}

/********************************************************************\
 * gnc_option_db_init                                               *
 *   initialize the options structures from the guile side          *
 *                                                                  *
 * Args: odb     - the option database to initialize                *
 * Returns: nothing                                                 *
\********************************************************************/
static void
gnc_option_db_init(GNCOptionDB *odb)
{
    SCM func = scm_c_eval_string("gnc:send-options");

    scm_call_2(func, scm_int2num(odb->handle), odb->guile_options);
}


/********************************************************************\
 * gnc_option_db_new                                                *
 *   allocate a new option database and initialize its values       *
 *                                                                  *
 * Args: guile_options - SCM handle to options                      *
 * Returns: a new option database                                   *
\********************************************************************/
GNCOptionDB *
gnc_option_db_new(SCM guile_options)
{
    GNCOptionDB *odb;
    GNCOptionDB *lookup;

    odb = g_new0(GNCOptionDB, 1);

    odb->guile_options = guile_options;
    scm_gc_protect_object(guile_options);

    odb->option_sections = NULL;
    odb->options_dirty = FALSE;

    if (option_dbs == NULL)
        option_dbs = g_hash_table_new(g_int_hash, g_int_equal);

    do
    {
        odb->handle = last_db_handle++;
        lookup = g_hash_table_lookup(option_dbs, &odb->handle);
    }
    while (lookup != NULL);

    g_hash_table_insert(option_dbs, &odb->handle, odb);

    gnc_option_db_init(odb);

    return odb;
}

typedef struct
{
    GNCOptionDB *odb;
    SCM guile_options;
} ODBFindInfo;

static void
option_db_finder (gpointer key, gpointer value, gpointer data)
{
    ODBFindInfo *find_info = data;
    GNCOptionDB *odb = value;

    if (odb && (odb->guile_options == find_info->guile_options))
        find_info->odb = odb;
}

static GNCOptionDB *
gnc_option_db_find (SCM guile_options)
{
    ODBFindInfo find_info;

    find_info.odb = NULL;
    find_info.guile_options = guile_options;

    g_hash_table_foreach (option_dbs, option_db_finder, &find_info);

    return find_info.odb;
}

/* Create an option DB for a particular data type */
GNCOptionDB *
gnc_option_db_new_for_type(QofIdType id_type)
{
    SCM options;

    if (!id_type) return NULL;
    options = gnc_make_kvp_options(id_type);
    return gnc_option_db_new (options);
}

void
gnc_option_db_load_from_kvp(GNCOptionDB* odb, kvp_frame *slots)
{
    static SCM kvp_to_scm = SCM_UNDEFINED;
    static SCM kvp_option_path = SCM_UNDEFINED;
    SCM scm_slots;

    if (!odb || !slots) return;

    if (kvp_to_scm == SCM_UNDEFINED)
    {
        kvp_to_scm = scm_c_eval_string("gnc:options-kvp->scm");
        if (!scm_is_procedure (kvp_to_scm))
        {
            PERR ("not a procedure\n");
            kvp_to_scm = SCM_UNDEFINED;
            return;
        }
    }

    if (kvp_option_path == SCM_UNDEFINED)
    {
        kvp_option_path = scm_c_eval_string("gnc:*kvp-option-path*");
        if (kvp_option_path == SCM_UNDEFINED)
        {
            PERR ("can't find the option path");
            return;
        }
    }
    scm_slots = SWIG_NewPointerObj(slots, SWIG_TypeQuery("_p_KvpFrame"), 0);

    scm_call_3 (kvp_to_scm, odb->guile_options, scm_slots, kvp_option_path);
}

void
gnc_option_db_save_to_kvp(GNCOptionDB* odb, kvp_frame *slots)
{
    static SCM scm_to_kvp = SCM_UNDEFINED;
    static SCM kvp_option_path = SCM_UNDEFINED;
    SCM scm_slots;

    if (!odb || !slots) return;

    if (scm_to_kvp == SCM_UNDEFINED)
    {
        scm_to_kvp = scm_c_eval_string("gnc:options-scm->kvp");
        if (!scm_is_procedure (scm_to_kvp))
        {
            PERR ("not a procedure\n");
            scm_to_kvp = SCM_UNDEFINED;
            return;
        }
    }

    if (kvp_option_path == SCM_UNDEFINED)
    {
        kvp_option_path = scm_c_eval_string("gnc:*kvp-option-path*");
        if (kvp_option_path == SCM_UNDEFINED)
        {
            PERR ("can't find the option path");
            return;
        }
    }

    scm_slots = SWIG_NewPointerObj(slots, SWIG_TypeQuery("p_KvpFrame"), 0);

    scm_call_3 (scm_to_kvp, odb->guile_options, scm_slots, kvp_option_path);
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
    GSList *snode;

    if (odb == NULL)
        return;

    for (snode = odb->option_sections; snode; snode = snode->next)
    {
        GNCOptionSection *section = snode->data;
        GSList *onode;

        for (onode = section->options; onode; onode = onode->next)
        {
            GNCOption *option = onode->data;

            scm_gc_unprotect_object(option->guile_option);
            g_free (option);
        }

        /* Free the option list */
        g_slist_free(section->options);
        section->options = NULL;

        if (section->section_name != NULL)
            free(section->section_name);
        section->section_name = NULL;

        g_free (section);
    }

    g_slist_free(odb->option_sections);

    odb->option_sections = NULL;
    odb->options_dirty = FALSE;

    g_hash_table_remove(option_dbs, &odb->handle);

    if (g_hash_table_size(option_dbs) == 0)
    {
        g_hash_table_destroy(option_dbs);
        option_dbs = NULL;
    }

    scm_gc_unprotect_object(odb->guile_options);
    odb->guile_options = SCM_UNDEFINED;

    g_free(odb);
}

void
gnc_option_db_set_ui_callbacks (GNCOptionDB *odb,
                                GNCOptionGetUIValue get_ui_value,
                                GNCOptionSetUIValue set_ui_value,
                                GNCOptionSetSelectable set_selectable)
{
    g_return_if_fail (odb != NULL);

    odb->get_ui_value = get_ui_value;
    odb->set_ui_value = set_ui_value;
    odb->set_selectable = set_selectable;
}

/********************************************************************\
 * gnc_option_db_register_change_callback                           *
 *   register a callback to be called whenever an option changes    *
 *                                                                  *
 * Args: odb       - the option database to register with           *
 *       callback  - the callback function to register              *
 *       user_data - the user data for the callback                 *
 *       section   - the section to get callbacks for.              *
 *                   If NULL, get callbacks for any section changes.*
 *       name      - the option name to get callbacks for.          *
 *                   If NULL, get callbacks for any option in the   *
 *                   section. Only used if section is non-NULL.     *
 * Returns: SCM handle for unregistering                            *
\********************************************************************/
SCM
gnc_option_db_register_change_callback(GNCOptionDB *odb,
                                       GNCOptionChangeCallback callback,
                                       gpointer data,
                                       const char *section,
                                       const char *name)
{
    SCM register_proc;
    SCM arg;
    SCM args;

    if (!odb || !callback)
        return SCM_UNDEFINED;

    /* Get the register procedure */
    register_proc = scm_c_eval_string("gnc:options-register-c-callback");
    if (!scm_is_procedure(register_proc))
    {
        PERR("not a procedure\n");
        return SCM_UNDEFINED;
    }

    /* Now build the args list for apply */
    args = SCM_EOL;

    /* first the guile options database */
    args = scm_cons(odb->guile_options, args);

    /* next the data */
    arg = SWIG_NewPointerObj(data, SWIG_TypeQuery("_p_void"), 0);
    args = scm_cons(arg, args);

    /* next the callback */
    arg = SWIG_NewPointerObj(
              callback, SWIG_TypeQuery("GNCOptionChangeCallback"), 0);
    args = scm_cons(arg, args);

    /* next the name */
    if (name == NULL)
    {
        arg = SCM_BOOL_F;
    }
    else
    {
        arg = scm_makfrom0str(name);
    }
    args = scm_cons(arg, args);

    /* next the section */
    if (section == NULL)
    {
        arg = SCM_BOOL_F;
    }
    else
    {
        arg = scm_makfrom0str(section);
    }
    args = scm_cons(arg, args);

    /* now apply the procedure */
    return scm_apply(register_proc, args, SCM_EOL);
}


/********************************************************************\
 * gnc_option_db_unregister_change_callback_id                      *
 *   unregister the change callback associated with the given id    *
 *                                                                  *
 * Args: odb      - the option database to register with            *
 *       callback - the callback function to register               *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_option_db_unregister_change_callback_id(GNCOptionDB *odb, SCM callback_id)
{
    SCM proc;

    if (callback_id == SCM_UNDEFINED)
        return;

    proc = scm_c_eval_string("gnc:options-unregister-callback-id");
    if (!scm_is_procedure(proc))
    {
        PERR("not a procedure\n");
        return;
    }

    scm_call_2(proc, callback_id, odb->guile_options);
}

void
gncp_option_invoke_callback (GNCOptionChangeCallback callback, void *data)
{
    callback (data);
}

static void
gnc_call_option_change_callbacks(GNCOptionDB *odb)
{
    SCM proc;

    proc = scm_c_eval_string("gnc:options-run-callbacks");
    if (!scm_is_procedure(proc))
    {
        PERR("not a procedure\n");
        return;
    }

    scm_call_1(proc, odb->guile_options);
}


static void
initialize_getters(void)
{
    static gboolean getters_initialized = FALSE;

    if (getters_initialized)
        return;

    getters.section = scm_c_eval_string("gnc:option-section");
    getters.name = scm_c_eval_string("gnc:option-name");
    getters.type = scm_c_eval_string("gnc:option-type");
    getters.sort_tag = scm_c_eval_string("gnc:option-sort-tag");
    getters.documentation =
        scm_c_eval_string("gnc:option-documentation");
    getters.getter = scm_c_eval_string("gnc:option-getter");
    getters.setter = scm_c_eval_string("gnc:option-setter");
    getters.default_getter =
        scm_c_eval_string("gnc:option-default-getter");
    getters.value_validator =
        scm_c_eval_string("gnc:option-value-validator");
    getters.option_data = scm_c_eval_string("gnc:option-data");
    getters.index_to_name = scm_c_eval_string("gnc:option-index-get-name");
    getters.index_to_description =
        scm_c_eval_string("gnc:option-index-get-description");
    getters.number_of_indices = scm_c_eval_string("gnc:option-number-of-indices");
    getters.index_to_value = scm_c_eval_string("gnc:option-index-get-value");
    getters.value_to_index = scm_c_eval_string("gnc:option-value-get-index");
    getters.option_widget_changed_cb =
        scm_c_eval_string("gnc:option-widget-changed-proc");
    getters.date_option_subtype = scm_c_eval_string("gnc:date-option-get-subtype");
    getters.date_option_show_time = scm_c_eval_string("gnc:date-option-show-time?");
    getters.date_option_value_type = scm_c_eval_string ("gnc:date-option-value-type");
    getters.date_option_value_absolute =
        scm_c_eval_string("gnc:date-option-absolute-time");
    getters.date_option_value_relative =
        scm_c_eval_string("gnc:date-option-relative-time");

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
 *   This value should be tested with scm_procedure_p before use.   *
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
 *   This value should be tested with scm_procedure_p before use.   *
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
 *   This value should be tested with scm_procedure_p before use.   *
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
 *   This value should be tested with scm_procedure_p before use.   *
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
 * gnc_option_widget_changed_proc_getter                            *
 *   returns the SCM handle for the function to be called if the    *
 *   GUI widget representing the option is changed.                 *
 *   This value should be tested with scm_procedure_p before use.   *
 *   If no such function exists, returns SCM_UNDEFINED.             *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: SCM handle to function                                  *
 *          If no such function exists, returns SCM_UNDEFINED.      *
\********************************************************************/
SCM
gnc_option_widget_changed_proc_getter(GNCOption *option)
{
    SCM cb;

    initialize_getters();

    if ( scm_is_procedure( getters.option_widget_changed_cb ) )
    {
        /* call the callback function getter to get the actual callback function */
        cb = scm_call_1(getters.option_widget_changed_cb, option->guile_option);

        if ( scm_is_procedure( cb ) )  /* a callback exists */
        {
            return( cb );
        }
        /* else no callback exists -  this is a legal situation */
    }
    else  /* getters not set up correctly? */
    {
        PERR("getters.option_widget_changed_cb is not a valid procedure\n");
    }

    return( SCM_UNDEFINED );
}


/********************************************************************\
 * gnc_option_call_option_widget_changed_proc                       *
 *   If there is an option_widget_changed_cb for this option, call  *
 *   it with the SCM value of the option that is passed in.  If     *
 *   there is no such callback function or value, do nothing.       *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: void                                                    *
\********************************************************************/
void
gnc_option_call_option_widget_changed_proc(GNCOption *option)
{
    SCM cb, value;

    cb = gnc_option_widget_changed_proc_getter(option);

    if ( cb != SCM_UNDEFINED )
    {
        value = gnc_option_get_ui_value(option);

        if ( value != SCM_UNDEFINED )
        {
            scm_call_1(cb, value);
        }
    }
}


/********************************************************************\
 * gnc_option_num_permissible_values                                *
 *   returns the number of permissible values in the option, or     *
 *   -1 if there are no values available.                           *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: number of permissible options or -1                     *
\********************************************************************/
int
gnc_option_num_permissible_values(GNCOption *option)
{
    SCM value;

    initialize_getters();

    value = scm_call_1(getters.number_of_indices, option->guile_option);

    if (scm_is_exact(value))
    {
        return scm_num2int(value, SCM_ARG1, G_STRFUNC);
    }
    else
    {
        return -1;
    }
}


/********************************************************************\
 * gnc_option_permissible_value_index                               *
 *   returns the index of the permissible value matching the        *
 *   provided value, or -1 if it couldn't be found                  *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 *       value  - the SCM handle of the value                       *
 * Returns: index of permissible value, or -1                       *
\********************************************************************/
int
gnc_option_permissible_value_index(GNCOption *option, SCM search_value)
{
    SCM value;
    value = scm_call_2(getters.value_to_index, option->guile_option, search_value);
    if (value == SCM_BOOL_F)
    {
        return -1;
    }
    else
    {
        return scm_num2int(value, SCM_ARG1, G_STRFUNC);
    }
}


/********************************************************************\
 * gnc_option_permissible_value                                     *
 *   returns the SCM handle to the indexth permissible value in the *
 *   option, or SCM_UNDEFINED if the index was out of range or      *
 *   there was some other problem.                                  *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 *       index  - the index of the permissible value                *
 * Returns: SCM handle to option value or SCM_UNDEFINED             *
\********************************************************************/
SCM
gnc_option_permissible_value(GNCOption *option, int index)
{
    SCM value;

    if (index < 0)
        return SCM_UNDEFINED;

    initialize_getters();

    value = scm_call_2(getters.index_to_value, option->guile_option,
                       scm_int2num(index));

    return value;
}


/********************************************************************\
 * gnc_option_permissible_value_name                                *
 *   returns the malloc'd name of the indexth permissible value in  *
 *   the option, or NULL if the index was out of range or there are *
 *   no values available.                                           *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 *       index  - the index of the permissible value                *
 * Returns: malloc'd name of permissible value or NULL              *
\********************************************************************/
char *
gnc_option_permissible_value_name(GNCOption *option, int index)
{
    SCM name;

    if (index < 0)
        return NULL;

    initialize_getters();

    name = scm_call_2(getters.index_to_name, option->guile_option,
                      scm_int2num(index));
    if (name == SCM_UNDEFINED)
        return NULL;
    if (!scm_is_string(name))
        return NULL;

    return g_strdup(scm_to_locale_string(name));
}


/********************************************************************\
 * gnc_option_permissible_value_description                         *
 *   returns the malloc'd description of the indexth permissible    *
 *   value in the option, or NULL if the index was out of range or  *
 *   there are no values available.                                 *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 *       index  - the index of the permissible value                *
 * Returns: malloc'd description of permissible value or NULL       *
\********************************************************************/
char *
gnc_option_permissible_value_description(GNCOption *option, int index)
{
    SCM help;

    if (index < 0)
        return NULL;

    initialize_getters();

    help = scm_call_2(getters.index_to_description, option->guile_option,
                      scm_int2num(index));
    if (help == SCM_UNDEFINED)
        return NULL;
    if (!scm_is_string(help))
        return NULL;

    return g_strdup(scm_to_locale_string(help));
}


/********************************************************************\
 * gnc_option_show_time                                             *
 *   returns true if the gui should display the time as well as     *
 *   the date for this option. Only use this for date options.      *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: true if time should be shown                            *
\********************************************************************/
gboolean
gnc_option_show_time(GNCOption *option)
{
    SCM value;

    initialize_getters();

    value = scm_call_1(getters.date_option_show_time, option->guile_option);

    return scm_is_true(value);
}

/********************************************************************\
 * gnc_option_get_option_data                                       *
 *   returns the option data of this option                         *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: the option data                                         *
\********************************************************************/
SCM
gnc_option_get_option_data(GNCOption *option)
{
    initialize_getters();

    return scm_call_1(getters.option_data, option->guile_option);
}


/********************************************************************\
 * gnc_option_multiple_selection                                    *
 *   returns true if the gui should allow multiple selection of     *
 *   accounts. Only use this for account options.                   *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: true if multiple selection allowed                      *
\********************************************************************/
gboolean
gnc_option_multiple_selection(GNCOption *option)
{
    SCM pair;

    initialize_getters();

    pair = scm_call_1(getters.option_data, option->guile_option);

    return !scm_is_true(scm_not(SCM_CAR(pair)));
}

/********************************************************************\
 * gnc_option_get_account_type_list                                 *
 *   returns the list of account_types in the option (or NULL if    *
 *   no special list is provided).  Only use this for account       *
 *   options.                                                       *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: GList of account types (must be freed by caller)        *
\********************************************************************/
GList *
gnc_option_get_account_type_list(GNCOption *option)
{
    SCM pair;
    SCM lst;
    GList *type_list = NULL;

    initialize_getters();

    pair = scm_call_1(getters.option_data, option->guile_option);
    lst = SCM_CDR(pair);

    while (!scm_is_null (lst))
    {
        GNCAccountType type;
        SCM item;

        /* Compute this item and the rest of the list */
        item = SCM_CAR (lst);
        lst = SCM_CDR (lst);

        if (scm_is_false (scm_integer_p (item)))
        {
            PERR ("Invalid type");
        }
        else
        {
            type = scm_num2long (item, SCM_ARG1, G_STRFUNC);
            type_list = g_list_prepend (type_list, GINT_TO_POINTER (type));
        }
    }

    return g_list_reverse (type_list);
}


/********************************************************************\
 * gnc_option_get_range_info                                        *
 *   returns the range info for a number range option in the pointer*
 *   arguments. NULL arguments are ignored. Use only for number     *
 *   range options.                                                 *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: true if everything went ok :)                           *
\********************************************************************/
gboolean gnc_option_get_range_info(GNCOption *option,
                                   double *lower_bound,
                                   double *upper_bound,
                                   int    *num_decimals,
                                   double *step_size)
{
    SCM list;
    SCM value;

    initialize_getters();

    list = scm_call_1(getters.option_data, option->guile_option);

    if (!scm_is_list(list) || scm_is_null(list))
        return FALSE;

    /* lower bound */
    value = SCM_CAR(list);
    list = SCM_CDR(list);

    if (!scm_is_number(value))
        return FALSE;

    if (lower_bound != NULL)
        *lower_bound = scm_num2dbl(value, G_STRFUNC);

    if (!scm_is_list(list) || scm_is_null(list))
        return FALSE;

    /* upper bound */
    value = SCM_CAR(list);
    list = SCM_CDR(list);

    if (!scm_is_number(value))
        return FALSE;

    if (upper_bound != NULL)
        *upper_bound = scm_num2dbl(value, G_STRFUNC);

    if (!scm_is_list(list) || scm_is_null(list))
        return FALSE;

    /* number of decimals */
    value = SCM_CAR(list);
    list = SCM_CDR(list);

    if (!scm_is_number(value))
        return FALSE;

    /* Guile-1.6 returns this as a double, so let's use that in all cases.
     * This is still safe for earlier guiles, too -- tested with 1.3.4.
     */
    if (num_decimals != NULL)
    {
        double decimals = scm_num2dbl(value, G_STRFUNC);
        *num_decimals = (int)decimals;
    }

    if (!scm_is_list(list) || scm_is_null(list))
        return FALSE;

    /* step size */
    value = SCM_CAR(list);
    list = SCM_CDR(list);

    if (!scm_is_number(value))
        return FALSE;

    if (step_size != NULL)
        *step_size = scm_num2dbl(value, G_STRFUNC);

    return TRUE;
}


/********************************************************************\
 * gnc_option_color_range                                           *
 *   returns the color range for rgba values.                       *
 *   Only use this for color options.                               *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: color range for the option                              *
\********************************************************************/
gdouble
gnc_option_color_range(GNCOption *option)
{
    SCM list;
    SCM value;

    initialize_getters();

    list = scm_call_1(getters.option_data, option->guile_option);
    if (!scm_is_list(list) || scm_is_null(list))
        return 0.0;

    value = SCM_CAR(list);
    if (!scm_is_number(value))
        return 0.0;

    return scm_num2dbl(value, G_STRFUNC);
}


/********************************************************************\
 * gnc_option_use_alpha                                             *
 *   returns true if the color option should use alpha transparency *
 *   Only use this for color options.                               *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: true if alpha transparency should be used               *
\********************************************************************/
gdouble
gnc_option_use_alpha(GNCOption *option)
{
    SCM list;
    SCM value;

    initialize_getters();

    list = scm_call_1(getters.option_data, option->guile_option);
    if (!scm_is_list(list) || scm_is_null(list))
        return FALSE;

    list = SCM_CDR(list);
    if (!scm_is_list(list) || scm_is_null(list))
        return FALSE;

    value = SCM_CAR(list);
    if (!scm_is_bool(value))
        return FALSE;

    return scm_is_true(value);
}


/********************************************************************\
 * gnc_option_get_color_argb                                        *
 *   returns the argb value of a color option                       *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: argb value of option                                    *
\********************************************************************/
guint32
gnc_option_get_color_argb(GNCOption *option)
{
    gdouble red, green, blue, alpha;
    guint32 color = 0;

    if (!gnc_option_get_color_info(option, FALSE, &red, &green, &blue, &alpha))
        return 0;

    color |= (guint32) (alpha * 255.0);
    color <<= 8;

    color |= (guint32) (red * 255.0);
    color <<= 8;

    color |= (guint32) (green * 255.0);
    color <<= 8;

    color |= (guint32) (blue * 255.0);

    return color;
}


/********************************************************************\
 * gnc_option_get_color_info                                        *
 *   gets the color information from a color option. rgba values    *
 *   returned are between 0.0 and 1.0.                              *
 *                                                                  *
 * Args: option      - option to get info from                      *
 *       use_default - use the default or current value             *
 *       red         - where to store the red value                 *
 *       blue        - where to store the blue value                *
 *       green       - where to store the green value               *
 *       alpha       - where to store the alpha value               *
 * Return: true if everything went ok                               *
\********************************************************************/
gboolean
gnc_option_get_color_info(GNCOption *option,
                          gboolean use_default,
                          gdouble *red,
                          gdouble *green,
                          gdouble *blue,
                          gdouble *alpha)
{
    gdouble scale;
    gdouble rgba;
    SCM getter;
    SCM value;

    if (option == NULL)
        return FALSE;

    if (use_default)
        getter = gnc_option_default_getter(option);
    else
        getter = gnc_option_getter(option);
    if (getter == SCM_UNDEFINED)
        return FALSE;

    value = scm_call_0(getter);
    if (!scm_is_list(value) || scm_is_null(value) || !scm_is_number(SCM_CAR(value)))
        return FALSE;

    scale = gnc_option_color_range(option);
    if (scale <= 0.0)
        return FALSE;

    scale = 1.0 / scale;

    rgba = scm_num2dbl(SCM_CAR(value), G_STRFUNC);
    if (red != NULL)
        *red = MIN(1.0, rgba * scale);

    value = SCM_CDR(value);
    if (!scm_is_list(value) || scm_is_null(value) || !scm_is_number(SCM_CAR(value)))
        return FALSE;

    rgba = scm_num2dbl(SCM_CAR(value), G_STRFUNC);
    if (green != NULL)
        *green = MIN(1.0, rgba * scale);

    value = SCM_CDR(value);
    if (!scm_is_list(value) || scm_is_null(value) || !scm_is_number(SCM_CAR(value)))
        return FALSE;

    rgba = scm_num2dbl(SCM_CAR(value), G_STRFUNC);
    if (blue != NULL)
        *blue = MIN(1.0, rgba * scale);

    value = SCM_CDR(value);
    if (!scm_is_list(value) || scm_is_null(value) || !scm_is_number(SCM_CAR(value)))
        return FALSE;

    rgba = scm_num2dbl(SCM_CAR(value), G_STRFUNC);
    if (alpha != NULL)
        *alpha = MIN(1.0, rgba * scale);

    return TRUE;
}


/********************************************************************\
 * gnc_option_set_default                                           *
 *   set the option to its default value                            *
 *                                                                  *
 * Args: option - the GNCOption                                     *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_option_set_default(GNCOption *option)
{
    SCM default_getter;
    SCM setter;
    SCM value;

    if (option == NULL)
        return;

    default_getter = gnc_option_default_getter(option);
    if (default_getter == SCM_UNDEFINED)
        return;

    value = scm_call_0(default_getter);

    setter = gnc_option_setter(option);
    if (setter == SCM_UNDEFINED)
        return;

    scm_call_1(setter, value);
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
    g_return_val_if_fail (odb, FALSE);

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
    g_return_if_fail (odb);

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
gnc_option_db_register_option(GNCOptionDBHandle handle, SCM guile_option)
{
    GNCOptionDB *odb;
    GNCOption *option;
    GNCOptionSection *section;

    odb = g_hash_table_lookup(option_dbs, &handle);

    g_return_if_fail (odb != NULL);

    odb->options_dirty = TRUE;

    /* Make the option structure */
    option = g_new0(GNCOption, 1);
    option->guile_option = guile_option;
    option->changed = FALSE;
    option->widget = NULL;
    option->odb = odb;

    /* Prevent guile from garbage collecting the option */
    scm_gc_protect_object(guile_option);

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
const char *
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
gnc_option_db_get_option_by_name(GNCOptionDB *odb, const char *section_name,
                                 const char *name)
{
    GSList *section_node;
    GSList *option_node;
    GNCOptionSection section_key;
    GNCOptionSection *section;
    GNCOption *option;
    gint result;
    char *node_name;

    if (odb == NULL)
        return NULL;

    section_key.section_name = (char *) section_name;

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


static SCM
gnc_option_valid_value(GNCOption *option, SCM value)
{
    SCM validator;
    SCM result, ok;

    validator = gnc_option_value_validator(option);

    result = scm_call_1(validator, value);
    if (!scm_is_list(result) || scm_is_null(result))
        return SCM_UNDEFINED;

    ok = SCM_CAR(result);
    if (!scm_is_bool(ok))
        return SCM_UNDEFINED;

    if (!scm_is_true(ok))
        return SCM_UNDEFINED;

    result = SCM_CDR(result);
    if (!scm_is_list(result) || scm_is_null(result))
        return SCM_UNDEFINED;

    return SCM_CAR(result);
}


static void
gnc_commit_option(GNCOption *option)
{
    SCM validator, setter, value;
    SCM result, ok;

    /* Validate the ui's value */
    value = gnc_option_get_ui_value(option);
    if (value == SCM_UNDEFINED)
        return;

    validator = gnc_option_value_validator(option);

    result = scm_call_1(validator, value);
    if (!scm_is_list(result) || scm_is_null(result))
    {
        PERR("bad validation result\n");
        return;
    }

    /* First element determines validity */
    ok = SCM_CAR(result);
    if (!scm_is_bool(ok))
    {
        PERR("bad validation result\n");
        return;
    }

    if (scm_is_true(ok))
    {
        /* Second element is value to use */
        value = SCM_CADR(result);
        setter = gnc_option_setter(option);

        scm_call_1(setter, value);

        gnc_option_set_ui_value (option, FALSE);
    }
    else
    {
        SCM oops;
        char *section, *name;
        GtkWidget *dialog;
        const gchar *message;
        const gchar *format = _("There is a problem with option %s:%s.\n%s");

        /* Second element is error message */
        oops = SCM_CADR(result);
        if (!scm_is_string(oops))
        {
            PERR("bad validation result\n");
            return;
        }

        message = scm_to_locale_string(oops);
        name = gnc_option_name(option);
        section = gnc_option_section(option);

        dialog = gtk_message_dialog_new(NULL,
                                        0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_OK,
                                        format,
                                        section ? section : "(null)",
                                        name ? name : "(null)",
                                        message ? message : "(null)");
        gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);

        if (name != NULL)
            free(name);
        if (section != NULL)
            free(section);
    }
}


/********************************************************************\
 * gnc_option_db_get_changed                                        *
 *   returns a boolean value, TRUE if any option has changed,       *
 *   FALSE is none of the options have changed                      *
 *                                                                  *
 * Args: odb - option database to check                             *
 * Return: boolean                                                  *
\********************************************************************/
gboolean
gnc_option_db_get_changed(GNCOptionDB *odb)
{
    GSList *section_node;
    GSList *option_node;
    GNCOptionSection *section;
    GNCOption *option;

    g_return_val_if_fail (odb, FALSE);

    for (section_node = odb->option_sections; section_node;
            section_node = section_node->next)
    {

        section = section_node->data;

        for (option_node = section->options; option_node;
                option_node = option_node->next)
        {

            option = option_node->data;

            if (option->changed)
                return TRUE;
        }
    }
    return FALSE;
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

    g_return_if_fail (odb);

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
 * gnc_option_db_section_reset_widgets                              *
 *   reset all option widgets in one section to their default.      *
 *   values                                                         *
 *                                                                  *
 * Args: odb - option database to reset                             *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_option_db_section_reset_widgets (GNCOptionSection *section)
{
    GSList *option_node;
    GNCOption *option;

    g_return_if_fail (section);

    /* Don't reset "invisible" options.
     * If the section name begins "__" we should not reset
     */
    if (section->section_name == NULL ||
            strncmp (section->section_name, "__", 2) == 0)
        return;

    for (option_node = section->options;
            option_node != NULL;
            option_node = option_node->next)
    {
        option = option_node->data;

        gnc_option_set_ui_value (option, TRUE);
        gnc_option_set_changed (option, TRUE);
    }
}


/********************************************************************\
 * gnc_option_db_reset_widgets                                      *
 *   reset all option widgets to their default values.              *
 *                                                                  *
 * Args: odb - option database to reset                             *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_option_db_reset_widgets (GNCOptionDB *odb)
{
    GSList *section_node;
    GNCOptionSection *section;

    g_return_if_fail (odb);

    for (section_node = odb->option_sections;
            section_node != NULL;
            section_node = section_node->next)
    {
        section = section_node->data;
        gnc_option_db_section_reset_widgets (section);
    }
}


/********************************************************************\
 * gnc_option_db_get_default_section                                *
 *   returns the malloc'd section name of the default section,      *
 *   or NULL if there is none.                                      *
 *                                                                  *
 * Args: odb - option database to get default page for              *
 * Return: g_malloc'd default section name                          *
\********************************************************************/
char *
gnc_option_db_get_default_section(GNCOptionDB *odb)
{
    SCM getter;
    SCM value;

    if (odb == NULL)
        return NULL;

    getter = scm_c_eval_string("gnc:options-get-default-section");
    if (!scm_is_procedure(getter))
        return NULL;

    value = scm_call_1(getter, odb->guile_options);
    if (!scm_is_string(value))
        return NULL;

    return g_strdup(scm_to_locale_string(value));
}


/********************************************************************\
 * gnc_option_db_lookup_option                                      *
 *   looks up an option. If present, returns its SCM value,         *
 *   otherwise returns the default.                                 *
 *                                                                  *
 * Args: odb     - option database to search in                     *
 *       section - section name of option                           *
 *       name    - name of option                                   *
 *       default - default value if not found                       *
 * Return: option value                                             *
\********************************************************************/
SCM
gnc_option_db_lookup_option(GNCOptionDB *odb,
                            const char *section,
                            const char *name,
                            SCM default_value)
{
    GNCOption *option;
    SCM getter;

    option = gnc_option_db_get_option_by_name(odb, section, name);

    if (option == NULL)
        return default_value;

    getter = gnc_option_getter(option);
    if (getter == SCM_UNDEFINED)
        return default_value;

    return scm_call_0(getter);
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
gnc_option_db_lookup_boolean_option(GNCOptionDB *odb,
                                    const char *section,
                                    const char *name,
                                    gboolean default_value)
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

    value = scm_call_0(getter);

    if (scm_is_bool(value))
        return scm_is_true(value);
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
gnc_option_db_lookup_string_option(GNCOptionDB *odb,
                                   const char *section,
                                   const char *name,
                                   const char *default_value)
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
            value = scm_call_0(getter);
            if (scm_is_string(value))
                return g_strdup(scm_to_locale_string(value));
        }
    }

    if (default_value == NULL)
        return NULL;

    return strdup(default_value);
}


/********************************************************************\
 * gnc_option_db_lookup_font_option                                 *
 *   looks up a font option. If present, returns its malloc'ed      *
 *   string value, otherwise returns the strdup'ed default, or NULL *
 *   if default was NULL.                                           *
 *                                                                  *
 * Args: odb     - option database to search in                     *
 *       section - section name of option                           *
 *       name    - name of option                                   *
 *       default - default value if not found                       *
 * Return: char * option value                                      *
\********************************************************************/
char *
gnc_option_db_lookup_font_option(GNCOptionDB *odb,
                                 const char *section,
                                 const char *name,
                                 const char *default_value)
{
    return gnc_option_db_lookup_string_option(odb, section, name, default_value);
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
gnc_option_db_lookup_multichoice_option(GNCOptionDB *odb,
                                        const char *section,
                                        const char *name,
                                        const char *default_value)
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
            value = scm_call_0(getter);
            if (scm_is_symbol(value))
                return g_strdup(SCM_SYMBOL_CHARS(value));
        }
    }

    if (default_value == NULL)
        return NULL;

    return strdup(default_value);
}


/********************************************************************\
 * gnc_option_db_lookup_date_option                                 *
 *   looks up a date option. If present, returns the absolute date  *
 *   represented in the set_ab_value argument provided, otherwise   *
 *   copies the default_value argument (if non-NULL) to the         *
 *   set_ab_value argument. If the default_value argument is NULL,  *
 *   copies the current date to set_ab_value. Whatever value is     *
 *   stored in set_ab_value is returned as an approximate (no       *
 *   nanoseconds) time_t value.  set_ab_value may be NULL, in which *
 *   case only the return value can be used. If is_relative is      *
 *   non-NULL, it is set to whether the date option is currently    *
 *   storing a relative date.  If it is, and set_rel_value          *
 *   is non-NULL, it returns a newly allocated string               *
 *   representing the scheme symbol for that relative date          *
 *                                                                  *
 * Args: odb           - option database to search in               *
 *       section       - section name of option                     *
 *       name          - name of option                             *
 *       is_relative   - location to store boolean value            *
 *       set_ab_value  - location to store absolute option value    *
 *       set_rel_value - location to store relative option value    *
 *       default       - default value if not found                 *
 * Return: time_t approximation of set_value                        *
\********************************************************************/
time_t
gnc_option_db_lookup_date_option(GNCOptionDB *odb,
                                 const char *section,
                                 const char *name,
                                 gboolean *is_relative,
                                 Timespec *set_ab_value,
                                 char **set_rel_value,
                                 Timespec *default_value)
{
    GNCOption *option;
    Timespec temp = {0, 0};
    char *symbol;
    SCM getter;
    SCM value;

    initialize_getters();

    if (set_ab_value == NULL)
    {
        set_ab_value = &temp;
    }

    if (set_rel_value != NULL)
    {
        *set_rel_value = NULL;
    }

    if (is_relative != NULL)
    {
        *is_relative = FALSE;
    }

    option = gnc_option_db_get_option_by_name(odb, section, name);

    if (option != NULL)
    {
        getter = gnc_option_getter(option);
        if (getter != SCM_UNDEFINED)
        {
            value = scm_call_0(getter);

            if (scm_is_pair(value))
            {
                Timespec absolute;

                absolute = gnc_date_option_value_get_absolute (value);

                *set_ab_value = absolute;

                symbol = gnc_date_option_value_get_type (value);

                if (safe_strcmp(symbol, "relative") == 0)
                {
                    SCM relative = gnc_date_option_value_get_relative (value);

                    if (is_relative != NULL)
                        *is_relative = TRUE;

                    if (set_rel_value != NULL)
                        *set_rel_value = g_strdup(SCM_SYMBOL_CHARS (relative));
                }

                if (symbol)
                    free (symbol);
            }
        }
    }
    else
    {
        if (default_value == NULL)
        {
            set_ab_value->tv_sec = time (NULL);
            set_ab_value->tv_nsec = 0;
        }
        else
            *set_ab_value = *default_value;
    }

    return set_ab_value->tv_sec;
}


/********************************************************************\
 * gnc_option_db_lookup_number_option                               *
 *   looks up a number option. If present, returns its value        *
 *   as a gdouble, otherwise returns the default_value.             *
 *                                                                  *
 * Args: odb       - option database to search in                   *
 *       section   - section name of option                         *
 *       name      - name of option                                 *
 *       default   - default value if not found                     *
 * Return: gdouble representation of value                          *
\********************************************************************/
gdouble
gnc_option_db_lookup_number_option(GNCOptionDB *odb,
                                   const char *section,
                                   const char *name,
                                   gdouble default_value)
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
            value = scm_call_0(getter);
            if (scm_is_number(value))
                return scm_num2dbl(value, G_STRFUNC);
        }
    }

    return default_value;
}


/********************************************************************\
 * gnc_option_db_lookup_color_option                                *
 *   looks up a color option. If present, returns its value in the  *
 *   color variable, otherwise leaves the color variable alone.     *
 *                                                                  *
 * Args: odb       - option database to search in                   *
 *       section   - section name of option                         *
 *       name      - name of option                                 *
 *       red       - where to store the red value                   *
 *       blue      - where to store the blue value                  *
 *       green     - where to store the green value                 *
 *       alpha     - where to store the alpha value                 *
 * Return: true if option was found                                 *
\********************************************************************/
gboolean gnc_option_db_lookup_color_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        gdouble *red,
        gdouble *green,
        gdouble *blue,
        gdouble *alpha)
{
    GNCOption *option;

    option = gnc_option_db_get_option_by_name(odb, section, name);

    return gnc_option_get_color_info(option, FALSE, red, green, blue, alpha);
}


/********************************************************************\
 * gnc_option_db_lookup_color_option_argb                           *
 *   looks up a color option. If present, returns its argb value,   *
 *   otherwise returns the given default value.                     *
 *                                                                  *
 * Args: odb       - option database to search in                   *
 *       section   - section name of option                         *
 *       name      - name of option                                 *
 *       default_value - default value to return if problem         *
 * Return: argb value                                               *
\********************************************************************/
guint32 gnc_option_db_lookup_color_option_argb(GNCOptionDB *odb,
        const char *section,
        const char *name,
        guint32 default_value)
{
    GNCOption *option;

    option = gnc_option_db_get_option_by_name(odb, section, name);
    if (option == NULL)
        return default_value;

    return gnc_option_get_color_argb(option);
}


/********************************************************************\
 * gnc_option_db_lookup_list_option                                 *
 *   looks up a list option. If present, returns its value as a     *
 *   list of strings representing the symbols.                      *
 *                                                                  *
 * Args: odb       - option database to search in                   *
 *       section   - section name of option                         *
 *       name      - name of option                                 *
 *       default_value - default value to return if problem         *
 * Return: list of values                                           *
\********************************************************************/
GSList *
gnc_option_db_lookup_list_option(GNCOptionDB *odb,
                                 const char *section,
                                 const char *name,
                                 GSList *default_value)
{
    GNCOption *option;
    GSList *list = NULL;
    SCM getter;
    SCM value;
    SCM item;

    option = gnc_option_db_get_option_by_name(odb, section, name);
    if (option == NULL)
        return default_value;

    getter = gnc_option_getter(option);
    if (getter == SCM_UNDEFINED)
        return default_value;

    value = scm_call_0(getter);
    while (scm_is_list(value) && !scm_is_null(value))
    {
        item = SCM_CAR(value);
        value = SCM_CDR(value);

        if (!scm_is_symbol(item))
        {
            gnc_free_list_option_value(list);

            return default_value;
        }

        list = g_slist_prepend(list, g_strdup(SCM_SYMBOL_CHARS(item)));
    }

    if (!scm_is_list(value) || !scm_is_null(value))
    {
        gnc_free_list_option_value(list);

        return default_value;
    }

    return list;
}


/********************************************************************\
 * gnc_option_db_lookup_currency_option                             *
 *   looks up a currency option. If present, returns its value as a *
 *   gnc_commodity object.                                          *
 *                                                                  *
 * Args: odb       - option database to search in                   *
 *       section   - section name of option                         *
 *       name      - name of option                                 *
 *       default_value - default value to return if problem         *
 * Return: commodity or NULL if no commodity found                  *
\********************************************************************/
gnc_commodity *
gnc_option_db_lookup_currency_option(GNCOptionDB *odb,
                                     const char *section,
                                     const char *name,
                                     gnc_commodity *default_value)
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

    value = scm_call_0(getter);

    return gnc_scm_to_commodity (value);
}

static void
free_helper(gpointer string, gpointer not_used)
{
    if (string) free(string);
}

void
gnc_free_list_option_value(GSList *list)
{
    g_slist_foreach(list, free_helper, NULL);
    g_slist_free(list);
}


/********************************************************************\
 * gnc_option_db_set_option_default                                 *
 *   set the option to its default value                            *
 *                                                                  *
 * Args: odb     - option database to search in                     *
 *       section - section name of option                           *
 *       name    - name of option                                   *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_option_db_set_option_default(GNCOptionDB *odb,
                                 const char *section,
                                 const char *name)
{
    GNCOption *option;

    option = gnc_option_db_get_option_by_name(odb, section, name);

    gnc_option_set_default(option);
}


/********************************************************************\
 * gnc_option_db_set_option                                         *
 *   sets the option to the given value. If successful              *
 *   returns TRUE, otherwise FALSE.                                 *
 *                                                                  *
 * Args: odb       - option database to search in                   *
 *       section   - section name of option                         *
 *       name      - name of option                                 *
 *       value     - value to set to                                *
 * Return: success indicator                                        *
\********************************************************************/
gboolean
gnc_option_db_set_option(GNCOptionDB *odb,
                         const char *section,
                         const char *name,
                         SCM value)
{
    GNCOption *option;
    SCM setter;

    option = gnc_option_db_get_option_by_name(odb, section, name);
    if (option == NULL)
        return FALSE;

    value = gnc_option_valid_value(option, value);
    if (value == SCM_UNDEFINED)
        return FALSE;

    setter = gnc_option_setter(option);
    if (setter == SCM_UNDEFINED)
        return FALSE;

    scm_call_1(setter, value);

    return TRUE;
}


/********************************************************************\
 * gnc_option_db_set_number_option                                  *
 *   sets the number option to the given value. If successful       *
 *   returns TRUE, otherwise FALSE.                                 *
 *                                                                  *
 * Args: odb       - option database to search in                   *
 *       section   - section name of option                         *
 *       name      - name of option                                 *
 *       value     - value to set to                                *
 * Return: success indicator                                        *
\********************************************************************/
gboolean
gnc_option_db_set_number_option(GNCOptionDB *odb,
                                const char *section,
                                const char *name,
                                gdouble value)
{
    GNCOption *option;
    SCM scm_value;
    SCM setter;

    option = gnc_option_db_get_option_by_name(odb, section, name);
    if (option == NULL)
        return FALSE;

    scm_value = scm_make_real(value);

    scm_value = gnc_option_valid_value(option, scm_value);
    if (scm_value == SCM_UNDEFINED)
        return FALSE;

    setter = gnc_option_setter(option);
    if (setter == SCM_UNDEFINED)
        return FALSE;

    scm_call_1(setter, scm_value);

    return TRUE;
}

/********************************************************************\
 * gnc_option_db_set_boolean_option                                 *
 *   sets the boolean option to the given value. If successful      *
 *   returns TRUE, otherwise FALSE.                                 *
 *                                                                  *
 * Args: odb       - option database to search in                   *
 *       section   - section name of option                         *
 *       name      - name of option                                 *
 *       value     - value to set to                                *
 * Return: success indicator                                        *
\********************************************************************/
gboolean
gnc_option_db_set_boolean_option(GNCOptionDB *odb,
                                 const char *section,
                                 const char *name,
                                 gboolean value)
{
    GNCOption *option;
    SCM scm_value;
    SCM setter;

    option = gnc_option_db_get_option_by_name(odb, section, name);
    if (option == NULL)
        return FALSE;

    scm_value = SCM_BOOL(value);

    scm_value = gnc_option_valid_value(option, scm_value);
    if (scm_value == SCM_UNDEFINED)
        return FALSE;

    setter = gnc_option_setter(option);
    if (setter == SCM_UNDEFINED)
        return FALSE;

    scm_call_1(setter, scm_value);

    return TRUE;
}

/********************************************************************\
 * gnc_option_db_set_string_option                                  *
 *   sets the string option to the given value. If successful       *
 *   returns TRUE, otherwise FALSE.                                 *
 *                                                                  *
 * Args: odb       - option database to search in                   *
 *       section   - section name of option                         *
 *       name      - name of option                                 *
 *       value     - value to set to                                *
 * Return: success indicator                                        *
\********************************************************************/
gboolean
gnc_option_db_set_string_option(GNCOptionDB *odb,
                                const char *section,
                                const char *name,
                                const char *value)
{
    GNCOption *option;
    SCM scm_value;
    SCM setter;

    option = gnc_option_db_get_option_by_name(odb, section, name);
    if (option == NULL)
        return FALSE;

    if (value)
        scm_value = scm_mem2string(value, strlen(value));
    else
        scm_value = SCM_BOOL_F;

    scm_value = gnc_option_valid_value(option, scm_value);
    if (scm_value == SCM_UNDEFINED)
        return FALSE;

    setter = gnc_option_setter(option);
    if (setter == SCM_UNDEFINED)
        return FALSE;

    scm_call_1(setter, scm_value);

    return TRUE;
}

/*******************************************************************\
 * gnc_option_date_option_get_subtype                              *
 *   find out whether a date option is a relative or absolute date *
 *                                                                 *
 * Args: option - option to get date subtype for                   *
 * Return: newly allocated subtype string or NULL                  *
\*******************************************************************/
char *
gnc_option_date_option_get_subtype(GNCOption *option)
{
    SCM value;

    initialize_getters();

    value = scm_call_1(getters.date_option_subtype, option->guile_option);

    if (scm_is_symbol(value))
        return g_strdup(SCM_SYMBOL_CHARS(value));
    else
        return NULL;
}

/*******************************************************************\
 * gnc_date_option_value_get_type                                  *
 *   get the type of a date option value                           *
 *                                                                 *
 * Args: option_value - option value to get type of                *
 * Return: newly allocated type string or NULL                     *
\*******************************************************************/
char *
gnc_date_option_value_get_type (SCM option_value)
{
    SCM value;

    initialize_getters();

    value = scm_call_1 (getters.date_option_value_type, option_value);
    if (!scm_is_symbol (value))
        return NULL;

    return g_strdup(SCM_SYMBOL_CHARS (value));
}

/*******************************************************************\
 * gnc_date_option_value_get_absolute                              *
 *   get the absolute time of a date option value                  *
 *                                                                 *
 * Args: option_value - option value to get absolute time of       *
 * Return: Timespec value                                          *
\*******************************************************************/
Timespec
gnc_date_option_value_get_absolute (SCM option_value)
{
    SCM value;

    initialize_getters();

    value = scm_call_1 (getters.date_option_value_absolute, option_value);

    return gnc_timepair2timespec (value);
}

/*******************************************************************\
 * gnc_date_option_value_get_relative                              *
 *   get the relative time of a date option value                  *
 *                                                                 *
 * Args: option_value - option value to get relative time of       *
 * Return: SCM value                                               *
\*******************************************************************/
SCM
gnc_date_option_value_get_relative (SCM option_value)
{
    initialize_getters();

    return scm_call_1 (getters.date_option_value_relative, option_value);
}

/*******************************************************************\
 * gnc_option_db_set_option_selectable_by_name                     *
 *   set the sensitivity of the option widget                      *
 *                                                                 *
 * Args: guile_options - guile side option db                      *
 *       section       - section of option                         *
 *       name          - name of option                            *
 *       selectable    - selectable status                         *
 * Return: SCM value                                               *
\*******************************************************************/
void
gnc_option_db_set_option_selectable_by_name(SCM guile_option,
        const char *section,
        const char *name,
        gboolean selectable)
{
    GNCOptionDB *odb;
    GNCOption *option;

    odb = gnc_option_db_find (guile_option);
    if (!odb)
        return;

    option = gnc_option_db_get_option_by_name(odb, section, name);
    if (!option)
        return;

    gnc_option_set_selectable (option, selectable);
}

/* the value is a list of:
 * format(symbol), month(symbol), include-years(bool), custom-string(string)
 */

gboolean gnc_dateformat_option_value_parse(SCM value, QofDateFormat *format,
        GNCDateMonthFormat *months,
        gboolean *years, char **custom)
{
    SCM val;
    const char *str;

    if (!scm_is_list(value) || scm_is_null(value))
        return TRUE;

    do
    {

        /* Parse the format */
        val = SCM_CAR(value);
        value = SCM_CDR(value);
        if (!scm_is_symbol(val))
            break;
        str = SCM_SYMBOL_CHARS (val);
        if (!str)
            break;

        if (format)
        {
            if (gnc_date_string_to_dateformat(str, format))
            {
                break;
            }
        }

        /* parse the months */
        val = SCM_CAR(value);
        value = SCM_CDR(value);
        if (!scm_is_symbol(val))
            break;
        str = SCM_SYMBOL_CHARS (val);
        if (!str)
            break;

        if (months)
        {
            if (gnc_date_string_to_monthformat(str, months))
            {
                break;
            }
        }

        /* parse the years */
        val = SCM_CAR(value);
        value = SCM_CDR(value);
        if (!scm_is_bool(val))
            break;

        if (years)
            *years = scm_is_true(val);

        /* parse the custom */
        val = SCM_CAR(value);
        value = SCM_CDR(value);
        if (!scm_is_string(val))
            break;
        if (!scm_is_null(value))
            break;

        if (custom)
            *custom = g_strdup(scm_to_locale_string(val));

        return FALSE;

    }
    while (FALSE);

    return TRUE;
}

SCM gnc_dateformat_option_set_value(QofDateFormat format, GNCDateMonthFormat months,
                                    gboolean years, const char *custom)
{
    SCM value = SCM_EOL;
    SCM val;
    const char *str;

    /* build the list in reverse order */
    if (custom)
        val = scm_mem2string(custom, strlen(custom));
    else
        val = SCM_BOOL_F;
    value = scm_cons(val, value);

    val = SCM_BOOL(years);
    value = scm_cons(val, value);

    str = gnc_date_monthformat_to_string(months);
    if (str)
        val = scm_str2symbol(str);
    else
        val = SCM_BOOL_F;
    value = scm_cons(val, value);

    str = gnc_date_dateformat_to_string(format);
    if (str)
        val = scm_str2symbol(str);
    else
        val = SCM_BOOL_F;
    value = scm_cons(val, value);

    return value;
}

/* For now, this is global, just like when it was in guile.
   But, it should be make per-book. */
static GHashTable *kvp_registry = NULL;

static void
init_table(void)
{
    if (!kvp_registry)
        kvp_registry = g_hash_table_new(g_str_hash, g_str_equal);
}

/*
 * the generator should be a procedure that takes one argument,
 * an options object.  The procedure should fill in the options with
 * its defined kvp options.
 */
void
gnc_register_kvp_option_generator(QofIdType id_type, SCM generator)
{
    GList *list;
    init_table();
    list = g_hash_table_lookup(kvp_registry, id_type);
    list = g_list_prepend(list, generator);
    g_hash_table_insert(kvp_registry, (gpointer) id_type, list);
    scm_gc_protect_object(generator);
}


/*  create a new options object for the requested type */
SCM
gnc_make_kvp_options(QofIdType id_type)
{
    GList *list, *p;
    SCM gnc_new_options = SCM_UNDEFINED;
    SCM options = SCM_UNDEFINED;

    init_table();
    list = g_hash_table_lookup(kvp_registry, id_type);
    gnc_new_options = scm_c_eval_string("gnc:new-options");
    options = scm_call_0(gnc_new_options);

    for (p = list; p; p = p->next)
    {
        SCM generator = p->data;
        scm_call_1(generator, options);
    }
    return options;
}
