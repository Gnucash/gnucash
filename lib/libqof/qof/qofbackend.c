/********************************************************************\
 * qofbackend.c -- utility routines for dealing with the data backend  *
 * Copyright (C) 2000 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2004-5 Neil Williams <linux@codehelp.co.uk>        *
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
 *                                                                  *
\********************************************************************/

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <regex.h>
#include <glib.h>
#include <gmodule.h>
#include <errno.h>
#include "qof.h"
#include "qofbackend-p.h"

GQuark qof_backend_error_quark (void)
{
	static GQuark quark;
	if (!quark)
		quark = g_quark_from_static_string ("qof_backend_error");
	return quark;
}

static QofLogModule log_module = QOF_MOD_BACKEND;

#define QOF_CONFIG_DESC    "desc"
#define QOF_CONFIG_TIP     "tip"

/***********************************************************************/
/* Get a clean backend */
void
qof_backend_init(QofBackend *be)
{
    be->session_begin = NULL;
    be->session_end = NULL;
    be->destroy_backend = NULL;

    be->load = NULL;

    be->begin = NULL;
    be->commit = NULL;
    be->rollback = NULL;

    be->compile_query = NULL;
    be->free_query = NULL;
    be->run_query = NULL;

    be->sync = NULL;
    be->load_config = NULL;

    be->events_pending = NULL;
    be->process_events = NULL;

    be->percentage = NULL;
    be->backend_configuration = kvp_frame_new();

    /* to be removed */
    be->price_lookup = NULL;
    be->export = NULL;
}

void
qof_backend_run_begin(QofBackend *be, QofInstance *inst, GError **error)
{
	if(!be || !inst) { return; }
	if(!be->begin) { return; }
	(be->begin) (be, inst, error);
}

gboolean
qof_backend_begin_exists(QofBackend *be)
{
	if(be->begin) { return TRUE; }
	else { return FALSE; }
}

void
qof_backend_run_commit(QofBackend *be, QofInstance *inst, GError **error)
{
	if(!be || !inst) { return; }
	if(!be->commit) { return; }
	(be->commit) (be, inst, error);
}

/* =========== Backend Configuration ================ */

void qof_backend_prepare_frame(QofBackend *be)
{
	g_return_if_fail(be);
	if(!kvp_frame_is_empty(be->backend_configuration)) {
		kvp_frame_delete(be->backend_configuration);
		be->backend_configuration = kvp_frame_new();
	}
	be->config_count = 0;
}

void qof_backend_prepare_option(QofBackend *be, QofBackendOption *option)
{
	KvpValue *value;
	gchar *temp;
	gint count;

	g_return_if_fail(be || option);
	count = be->config_count;
	count++;
	value = NULL;
	switch (option->type)
	{
		case KVP_TYPE_GINT64   : {
			value = kvp_value_new_gint64(*(gint64*)option->value);
			break; 
		}
		case KVP_TYPE_DOUBLE   : { 
			value = kvp_value_new_double(*(double*)option->value);
			break; 
		}
		case KVP_TYPE_NUMERIC  : {
			value = kvp_value_new_numeric(*(gnc_numeric*)option->value);
			break; 
		}
		case KVP_TYPE_STRING   : {
			value = kvp_value_new_string((const char*)option->value);
			break;
		}
		case KVP_TYPE_GUID     : { break; } /* unsupported */
		case KVP_TYPE_TIMESPEC : {
			value = kvp_value_new_timespec(*(Timespec*)option->value);
			break;
		}
		case KVP_TYPE_BINARY   : { break; } /* unsupported */
		case KVP_TYPE_GLIST    : { break; } /* unsupported */
		case KVP_TYPE_FRAME    : { break; } /* unsupported */
	}
	if(value) {
		temp = g_strdup_printf("/%s", option->option_name);
		kvp_frame_set_value(be->backend_configuration, temp, value);
		g_free(temp);
		temp = g_strdup_printf("/%s/%s", QOF_CONFIG_DESC, option->option_name);
		kvp_frame_set_string(be->backend_configuration, temp, option->description);
		g_free(temp);
		temp = g_strdup_printf("/%s/%s", QOF_CONFIG_TIP, option->option_name);
		kvp_frame_set_string(be->backend_configuration, temp, option->tooltip);
		g_free(temp);
		/* only increment the counter if successful */
		be->config_count = count;
	}
}

KvpFrame* qof_backend_complete_frame(QofBackend *be)
{
	g_return_val_if_fail(be, NULL);
	be->config_count = 0;
	return be->backend_configuration;
}

struct config_iterate {
	QofBackendOptionCB fcn;
	gpointer           data;
	gint               count;
	KvpFrame          *recursive;
};

/* Set the option with the default KvpValue,
manipulate the option in the supplied callback routine
then set the value of the option into the KvpValue
in the configuration frame. */
static void
config_foreach_cb (const char *key, KvpValue *value, gpointer data)
{
	QofBackendOption option;
	gint64 int64;
	double db;
	gnc_numeric num;
	Timespec ts;
	gchar *parent;
	struct config_iterate *helper;

	g_return_if_fail(key || value || data);
	helper = (struct config_iterate*)data;
	if(!helper->recursive) { PERR (" no parent frame"); return;	}
	// skip the presets.
	if(0 == safe_strcmp(key, QOF_CONFIG_DESC)) { return; }
	if(0 == safe_strcmp(key, QOF_CONFIG_TIP)) { return; }
	ENTER (" key=%s", key);
	option.option_name = key;
	option.type = kvp_value_get_type(value);
	if(!option.type) { return; }
	switch (option.type)
	{ /* set the KvpFrame value into the option */
		case KVP_TYPE_GINT64   : {
			int64 = kvp_value_get_gint64(value);
			option.value = (gpointer)&int64;
			break;
		}
		case KVP_TYPE_DOUBLE   : {
			db = kvp_value_get_double(value);
			option.value = (gpointer)&db;
			break; 
		}
		case KVP_TYPE_NUMERIC  : {
			num = kvp_value_get_numeric(value);
			option.value = (gpointer)&num;
			break; 
		}
		case KVP_TYPE_STRING   : {
			option.value = (gpointer)kvp_value_get_string(value);
			break;
		}
		case KVP_TYPE_TIMESPEC : {
			ts = kvp_value_get_timespec(value);
			option.value = (gpointer)&ts;
			break;
		}
		case KVP_TYPE_GUID     : { break; } /* unsupported */
		case KVP_TYPE_BINARY   : { break; } /* unsupported */
		case KVP_TYPE_GLIST    : { break; } /* unsupported */
		case KVP_TYPE_FRAME    : { break; } /* unsupported */
	}
	parent = g_strdup_printf("/%s/%s", QOF_CONFIG_DESC, key);
	option.description = kvp_frame_get_string(helper->recursive, parent);
	g_free(parent);
	parent = g_strdup_printf("/%s/%s", QOF_CONFIG_TIP, key);
	option.tooltip = kvp_frame_get_string(helper->recursive, parent);
	g_free(parent);
	helper->count++;
    /* manipulate the option */
	helper->fcn (&option, helper->data);
	switch (option.type)
	{ /* set the option value into the KvpFrame */
		case KVP_TYPE_GINT64   : {
			kvp_frame_set_gint64(helper->recursive, key, 
				(*(gint64*)option.value));
			break;
		}
		case KVP_TYPE_DOUBLE   : {
			kvp_frame_set_double(helper->recursive, key, 
				(*(double*)option.value));
			break; 
		}
		case KVP_TYPE_NUMERIC  : {
			kvp_frame_set_numeric(helper->recursive, key, 
				(*(gnc_numeric*)option.value));
			break; 
		}
		case KVP_TYPE_STRING   : {
			kvp_frame_set_string(helper->recursive, key, 
				(gchar*)option.value);
			break;
		}
		case KVP_TYPE_TIMESPEC : {
			kvp_frame_set_timespec(helper->recursive, key, 
				(*(Timespec*)option.value));
			break;
		}
		case KVP_TYPE_GUID     : { break; } /* unsupported */
		case KVP_TYPE_BINARY   : { break; } /* unsupported */
		case KVP_TYPE_GLIST    : { break; } /* unsupported */
		case KVP_TYPE_FRAME    : { break; } /* unsupported */
	}
	LEAVE (" ");
}

void qof_backend_option_foreach(KvpFrame *config, QofBackendOptionCB cb, gpointer data)
{
	struct config_iterate helper;

	if(!config || !cb) { return; }
	ENTER (" ");
	helper.fcn = cb;
	helper.count = 1;
	helper.data = data;
	helper.recursive = config;
	kvp_frame_for_each_slot(config, config_foreach_cb, &helper);
	LEAVE (" ");
}

void
qof_backend_load_config(QofBackend *be, KvpFrame *config)
{
	if(!be || !config) { return; }
	if(!be->load_config) { return; }
	(be->load_config) (be, config);
}

KvpFrame*
qof_backend_get_config(QofBackend *be)
{
	if(!be) { return NULL; }
	if(!be->get_config) { return NULL; }
	return (be->get_config) (be);
}

gboolean
qof_backend_commit_exists(QofBackend *be)
{
	if(!be) { return FALSE; }
	if(be->commit) { return TRUE; }
	else { return FALSE; }
}

gboolean
qof_load_backend_library (const char *directory, const char* module_name)
{
	gchar *fullpath;
	GModule *backend;

	g_return_val_if_fail(g_module_supported(), FALSE);
	fullpath = g_module_build_path(directory, module_name);
	backend = g_module_open(fullpath, G_MODULE_BIND_LAZY);
	if (!backend) {
		g_message ("%s: %s\n", PACKAGE, g_module_error ());
		return FALSE;
	}

	/* the module should have done that already in g_module_check_init */
	g_module_make_resident(backend);
	return TRUE;
}

GError* 
qof_backend_get_error (QofBackend *be)
{
  if (be->error)
    return be->error;
  else
    return NULL;
}


QofBackendError 
qof_backend_get_error_code (QofBackend *be)
{
  if (be->error)
    return be->error->code;
  else
    return ERR_BACKEND_NO_ERR;
}


void 
qof_backend_error_free (QofBackend *be)
{
  if(be->error)
    g_error_free (be->error);
}

void
qof_backend_set_error (QofBackend *be, QofBackendError code)
{
  g_return_if_fail (code == ERR_BACKEND_NO_ERR);
  
  /*FIXME: Add conditionals to set error messages depending on error's code */  
  be->error = g_error_new (QOF_BACKEND_ERROR, code, "BackendError");
}

/************************* END OF FILE ********************************/
