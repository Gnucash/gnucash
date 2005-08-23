/*
 * command-context.c : Error dispatch utilities.
 *
 * Author:
 * 	Jody Goldberg <jody@gnome.org>
 *
 * (C) 1999-2001 Jody Goldberg
 */
#include <config.h>
#include <glib/gi18n.h>
#include "gnumeric.h"
#include "command-context-priv.h"
#include "ranges.h"

#include <gsf/gsf-impl-utils.h>

#define CC_CLASS(o) (G_TYPE_INSTANCE_GET_INTERFACE ((o), GNM_CMD_CONTEXT_TYPE, GnmCmdContextClass))

static GError *
format_message (GQuark id, char const *message)
{
	char const *msg = message ? message : "";
	return g_error_new_literal (id, 0, msg);
}

void
gnm_cmd_context_error (GnmCmdContext *context, GError *err)
{
	g_return_if_fail (IS_GNM_CMD_CONTEXT (context));
	CC_CLASS (context)->error.error (context, err);
}

void
gnm_cmd_context_error_info (GnmCmdContext *context, ErrorInfo *error)
{
	g_return_if_fail (IS_GNM_CMD_CONTEXT (context));
	CC_CLASS (context)->error.error_info (context, error);
}

void
gnm_cmd_context_error_system (GnmCmdContext *context, char const *message)
{
	GError *err = format_message (gnm_error_system (), message);
	gnm_cmd_context_error (context, err);
	g_error_free (err);
}

void
gnm_cmd_context_error_import (GnmCmdContext *context, char const *message)
{
	GError *err = format_message (gnm_error_import (), message);
	gnm_cmd_context_error (context, err);
	g_error_free (err);
}

void
gnm_cmd_context_error_export (GnmCmdContext *context, char const *message)
{
	GError *err = format_message (gnm_error_export (), message);
	gnm_cmd_context_error (context, err);
	g_error_free (err);
}

void
gnm_cmd_context_error_invalid (GnmCmdContext *context, char const *msg, char const *val)
{
	GError *err = g_error_new (gnm_error_invalid(), 0, "Invalid %s : '%s'", msg, val);
	gnm_cmd_context_error (context, err);
	g_error_free (err);
}

void
gnm_cmd_context_error_calc (GnmCmdContext *context, char const *msg)
{
	GError *err = format_message (gnm_error_calc (), msg);
	gnm_cmd_context_error (context, err);
	g_error_free (err);
}

char const *
range_name(GnmRange const *src)
{
  return "undefined";
}


void
gnm_cmd_context_error_splits_array (GnmCmdContext *context,
			     G_GNUC_UNUSED char const *cmd,
			     GnmRange const *array)
{
	GError *err;

	if (array != NULL)
		err = g_error_new (gnm_error_array(), 1,
			_("Would split array %s"), range_name (array));
	else
		err = g_error_new (gnm_error_array(), 0,
			_("Would split an array"));
	gnm_cmd_context_error (context, err);
}

GQuark
gnm_error_system (void)
{
	static GQuark quark;
	if (!quark)
		quark = g_quark_from_static_string ("gnm_error_system");
	return quark;
}
GQuark
gnm_error_import (void)
{
	static GQuark quark;
	if (!quark)
		quark = g_quark_from_static_string ("gnm_error_import");
	return quark;
}
GQuark
gnm_error_export (void)
{
	static GQuark quark;
	if (!quark)
		quark = g_quark_from_static_string ("gnm_error_export");
	return quark;
}
GQuark
gnm_error_array (void)
{
	static GQuark quark;
	if (!quark)
		quark = g_quark_from_static_string ("gnm_error_array");
	return quark;
}

GQuark
gnm_error_calc (void)
{
	static GQuark quark;
	if (!quark)
		quark = g_quark_from_static_string ("gnm_error_calc");
	return quark;
}

GQuark
gnm_error_invalid (void)
{
	static GQuark quark;
	if (!quark)
		quark = g_quark_from_static_string ("gnm_error_invalid");
	return quark;
}

void
cmd_context_progress_set (GnmCmdContext *context, gfloat f)
{
	g_return_if_fail (IS_GNM_CMD_CONTEXT (context));

	CC_CLASS (context)->progress_set (context, f);
}

void
cmd_context_progress_message_set (GnmCmdContext *context, gchar const *msg)
{
	g_return_if_fail (IS_GNM_CMD_CONTEXT (context));

	if (msg == NULL)
		msg = " ";
	CC_CLASS (context)->progress_message_set (context, msg);
}

char *
gnm_cmd_context_get_password (GnmCmdContext *cc, char const *filename)
{
	g_return_val_if_fail (IS_GNM_CMD_CONTEXT (cc), NULL);

	return CC_CLASS (cc)->get_password (cc, filename);
}

void
gnm_cmd_context_set_sensitive (GnmCmdContext *cc, gboolean sensitive)
{
	g_return_if_fail (IS_GNM_CMD_CONTEXT (cc));

	CC_CLASS (cc)->set_sensitive (cc, sensitive);
}

GType
gnm_cmd_context_get_type (void)
{
	static GType gnm_cmd_context_type = 0;

	if (!gnm_cmd_context_type) {
		static GTypeInfo const gnm_cmd_context_info = {
			sizeof (GnmCmdContextClass),	/* class_size */
			NULL,				/* base_init */
			NULL,				/* base_finalize */
		};

		gnm_cmd_context_type = g_type_register_static (G_TYPE_INTERFACE,
			"GnmCmdContext", &gnm_cmd_context_info, 0);
	}

	return gnm_cmd_context_type;
}

