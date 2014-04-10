/*
 * go-cmd-context.c : Error dispatch utilities.
 *
 * Author:
 * 	Jody Goldberg <jody@gnome.org>
 *
 * (C) 1999-2004 Jody Goldberg
 */
#include <goffice/goffice-config.h>
#include "go-cmd-context-impl.h"
#include <goffice/app/goffice-app.h>
#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>

#define GCC_CLASS(o) (G_TYPE_INSTANCE_GET_INTERFACE ((o), GO_CMD_CONTEXT_TYPE, GOCmdContextClass))

static GError *
format_message (GQuark id, char const *message)
{
	char const *msg = message ? message : "";
	return g_error_new_literal (id, 0, msg);
}

void
go_cmd_context_error (GOCmdContext *context, GError *err)
{
	g_return_if_fail (IS_GO_CMD_CONTEXT (context));
	GCC_CLASS (context)->error.error (context, err);
}

void
go_cmd_context_error_info (GOCmdContext *context, ErrorInfo *stack)
{
	g_return_if_fail (IS_GO_CMD_CONTEXT (context));
	GCC_CLASS (context)->error.error_info (context, stack);
}

void
go_cmd_context_error_system (GOCmdContext *context, char const *message)
{
	GError *err = format_message (go_error_system (), message);
	go_cmd_context_error (context, err);
	g_error_free (err);
}

void
go_cmd_context_error_import (GOCmdContext *context, char const *message)
{
	GError *err = format_message (go_error_import (), message);
	go_cmd_context_error (context, err);
	g_error_free (err);
}

void
go_cmd_context_error_export (GOCmdContext *context, char const *message)
{
	GError *err = format_message (go_error_export (), message);
	go_cmd_context_error (context, err);
	g_error_free (err);
}

void
go_cmd_context_error_invalid (GOCmdContext *context, char const *msg, char const *val)
{
	GError *err = g_error_new (go_error_invalid(), 0, "Invalid %s : '%s'", msg, val);
	go_cmd_context_error (context, err);
	g_error_free (err);
}

GQuark
go_error_system (void)
{
	static GQuark quark;
	if (!quark)
		quark = g_quark_from_static_string ("go_error_system");
	return quark;
}
GQuark
go_error_import (void)
{
	static GQuark quark;
	if (!quark)
		quark = g_quark_from_static_string ("go_error_import");
	return quark;
}
GQuark
go_error_export (void)
{
	static GQuark quark;
	if (!quark)
		quark = g_quark_from_static_string ("go_error_export");
	return quark;
}
GQuark
go_error_invalid (void)
{
	static GQuark quark;
	if (!quark)
		quark = g_quark_from_static_string ("go_error_invalid");
	return quark;
}

void
go_cmd_context_progress_set (GOCmdContext *context, gfloat f)
{
	g_return_if_fail (IS_GO_CMD_CONTEXT (context));

	GCC_CLASS (context)->progress_set (context, f);
}

void
go_cmd_context_progress_message_set (GOCmdContext *context, gchar const *msg)
{
	g_return_if_fail (IS_GO_CMD_CONTEXT (context));

	if (msg == NULL)
		msg = " ";
	GCC_CLASS (context)->progress_message_set (context, msg);
}

char *
go_cmd_context_get_password (GOCmdContext *cc, char const *filename)
{
	g_return_val_if_fail (IS_GO_CMD_CONTEXT (cc), NULL);

	return GCC_CLASS (cc)->get_password (cc, filename);
}

void
go_cmd_context_set_sensitive (GOCmdContext *cc, gboolean sensitive)
{
	g_return_if_fail (IS_GO_CMD_CONTEXT (cc));

	GCC_CLASS (cc)->set_sensitive (cc, sensitive);
}

GType
go_cmd_context_get_type (void)
{
	static GType go_cmd_context_type = 0;

	if (!go_cmd_context_type) {
		static GTypeInfo const go_cmd_context_info = {
			sizeof (GOCmdContextClass),	/* class_size */
			NULL,				/* base_init */
			NULL,				/* base_finalize */
		};

		go_cmd_context_type = g_type_register_static (G_TYPE_INTERFACE,
			"GOCmdContext", &go_cmd_context_info, 0);
	}

	return go_cmd_context_type;
}

