/********************************************************************\
 * gnc-menu-extensions.c -- functions to build dynamic menus        *
 * Copyright (C) 1999 Rob Browning         	                    *
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
#include <ctype.h>

#include "guile-util.h"
#include "gnc-engine.h"
#include "gnc-menu-extensions.h"
#include "gnc-ui.h"

typedef struct _Getters Getters;
struct _Getters
{
    SCM type;
    SCM name;
    SCM documentation;
    SCM path;
    SCM script;
};

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static GSList *extension_list = NULL;
static Getters getters = {0, 0, 0, 0, 0};

GSList *
gnc_extensions_get_menu_list (void)
{
    return g_slist_copy(extension_list);
}

static void
initialize_getters()
{
    static gboolean getters_initialized = FALSE;

    if (getters_initialized)
        return;

    getters.type = scm_c_eval_string("gnc:extension-type");
    getters.name = scm_c_eval_string("gnc:extension-name");
    getters.documentation = scm_c_eval_string("gnc:extension-documentation");
    getters.path = scm_c_eval_string("gnc:extension-path");
    getters.script = scm_c_eval_string("gnc:extension-script");

    getters_initialized = TRUE;
}


static gboolean
gnc_extension_type (SCM extension, GtkUIManagerItemType *type)
{
    char *string;

    initialize_getters();

    string = gnc_guile_call1_symbol_to_string(getters.type, extension);
    if (string == NULL)
    {
        PERR("bad type");
        return FALSE;
    }

    if (safe_strcmp(string, "menu-item") == 0)
    {
        *type = GTK_UI_MANAGER_MENUITEM;
    }
    else if (safe_strcmp(string, "menu") == 0)
    {
        *type = GTK_UI_MANAGER_MENU;
    }
    else if (safe_strcmp(string, "separator") == 0)
    {
        *type = GTK_UI_MANAGER_SEPARATOR;
    }
    else
    {
        PERR("bad type");
        return FALSE;
    }

    free(string);

    return TRUE;
}

/* returns malloc'd name */
static char *
gnc_extension_name (SCM extension)
{
    initialize_getters();

    return gnc_guile_call1_to_string(getters.name, extension);
}


/* returns malloc'd docs */
static char *
gnc_extension_documentation (SCM extension)
{
    initialize_getters();

    return gnc_guile_call1_to_string(getters.documentation, extension);
}

/* returns g_malloc'd path */
static void
gnc_extension_path (SCM extension, char **fullpath)
{
    SCM path;
    gchar **strings;
    gint i;
    gint num_strings;

    initialize_getters();

    path = gnc_guile_call1_to_list(getters.path, extension);
    if ((path == SCM_UNDEFINED) || scm_is_null(path))
    {
        *fullpath = g_strdup("");
        return;
    }

    num_strings = scm_ilength(path) + 2;
    strings = g_new0(gchar *, num_strings);
    strings[0] = "/menubar";

    i = 1;
    while (!scm_is_null(path))
    {
        SCM item;

        item = SCM_CAR(path);
        path = SCM_CDR(path);

        if (scm_is_string(item))
        {
            if (i == 1)
                strings[i] = g_strdup(scm_to_locale_string(item));
            else
                strings[i] = g_strdup(gettext(scm_to_locale_string(item)));
        }
        else
        {
            g_free(strings);

            PERR("not a string");

            *fullpath = g_strdup("");
            return;
        }

        i++;
    }

    *fullpath = g_strjoinv("/", strings);

    for (i = 1; i < num_strings; i++)
    {
        if (strings[i] != NULL)
        {
            g_free(strings[i]);
        }
    }

    g_free(strings);
}

/******************** New Menu Item ********************/

static gchar*
gnc_ext_gen_action_name (const gchar *name)
{

    const gchar *extChar;
    GString *actionName;

    actionName = g_string_sized_new( strlen( name ) + 7 );

    // 'Mum & ble' => 'Mumble'
    for ( extChar = name; *extChar != '\0'; extChar++ )
    {
        if ( ! isalnum( *extChar ) )
            continue;
        g_string_append_c( actionName, *extChar );
    }

    // 'Mumble + 'Action' => 'MumbleAction'
    g_string_append_printf( actionName, "Action" );

    return g_string_free(actionName, FALSE);
}

/******************** Callback ********************/

void
gnc_extension_invoke_cb (SCM extension, SCM window)
{
    SCM script;

    initialize_getters();

    script = gnc_guile_call1_to_procedure(getters.script, extension);
    if (script == SCM_UNDEFINED)
    {
        PERR("not a procedure.");
        return;
    }

    scm_call_1(script, window);
}

/******************** New Menu Item ********************/

static gboolean
gnc_create_extension_info (SCM extension)
{
    ExtensionInfo *ext_info;
    gchar *typeStr, *tmp;
    const gchar *name;

    ext_info = g_new0(ExtensionInfo, 1);
    ext_info->extension = extension;
    gnc_extension_path(extension, &ext_info->path);
    if (!gnc_extension_type( extension, &ext_info->type ))
    {
        /* Can't parse the type passed to us.  Bail now. */
        g_free(ext_info);
        return FALSE;
    }

    /* Get all the pieces */
    name = gnc_extension_name(extension);
    ext_info->ae.label = g_strdup(gettext(name));
    ext_info->ae.name = gnc_ext_gen_action_name(name);
    ext_info->ae.tooltip = gnc_extension_documentation(extension);
    ext_info->ae.stock_id = NULL;
    ext_info->ae.accelerator = NULL;
    ext_info->ae.callback = NULL;

    tmp = g_strdup_printf("%s/%s", ext_info->path, ext_info->ae.label);
    ext_info->sort_key = g_utf8_collate_key(tmp, -1);
    g_free(tmp);

    switch (ext_info->type)
    {
    case GTK_UI_MANAGER_MENU:
        typeStr = "menu";
        break;
    case GTK_UI_MANAGER_MENUITEM:
        typeStr = "menuitem";
        break;
    default:
        typeStr = "unk";
        break;
    }
    ext_info->typeStr = typeStr;

    DEBUG( "extension: %s/%s [%s] tip [%s] type %s\n",
           ext_info->path, ext_info->ae.label, ext_info->ae.name,
           ext_info->ae.tooltip, ext_info->typeStr );

    scm_gc_protect_object(extension);

    /* need to append so we can run them in order */
    extension_list = g_slist_append(extension_list, ext_info);

    return TRUE;
}

static void
cleanup_extension_info(gpointer extension_info, gpointer not_used)
{
    ExtensionInfo *ext_info = extension_info;

    if (ext_info->extension)
        scm_gc_unprotect_object(ext_info->extension);

    g_free(ext_info->sort_key);
    g_free((gchar *)ext_info->ae.name);
    g_free((gchar *)ext_info->ae.label);
    g_free((gchar *)ext_info->ae.tooltip);
    g_free(ext_info->path);
    g_free(ext_info);
}


void
gnc_add_scm_extension (SCM extension)
{
    if (!gnc_create_extension_info(extension))
    {
        PERR("bad extension");
        return;
    }
}

/******************** Shutdown ********************/

void
gnc_extensions_shutdown (void)
{
    g_slist_foreach(extension_list, cleanup_extension_info, NULL);

    g_slist_free(extension_list);

    extension_list = NULL;
}
