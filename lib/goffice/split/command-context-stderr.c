/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * command-context-stderr.c : Error dispatch for line oriented clients
 *
 * Author:
 * 	Jon K Hellan <hellan@acm.org>
 *
 * (C) 2002 Jon K Hellan
 */
#include <config.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include "gnumeric.h"
#include "command-context-stderr.h"
#include "command-context-priv.h"
#include <gsf/gsf-impl-utils.h>
#include "error-info.h"
#include "ranges.h"

struct _CmdContextStderr {
	GObject	 base;
	int	 status;
};
typedef GObjectClass CmdContextStderrClass;

#define COMMAND_CONTEXT_STDERR_CLASS(k) \
	(G_TYPE_CHECK_CLASS_CAST ((k), CMD_CONTEXT_STDERR_TYPE, CmdContextStderrClass))

GnmCmdContext *
cmd_context_stderr_new (void)
{
	return g_object_new (CMD_CONTEXT_STDERR_TYPE, NULL);
}

void
cmd_context_stderr_set_status (CmdContextStderr *ccs, int status)
{
	g_return_if_fail (ccs != NULL);
	g_return_if_fail (IS_COMMAND_CONTEXT_STDERR (ccs));

	ccs->status = status;
}

int
cmd_context_stderr_get_status (CmdContextStderr *ccs)
{
	g_return_val_if_fail (ccs != NULL, -1);
	g_return_val_if_fail (IS_COMMAND_CONTEXT_STDERR (ccs), -1);

	return ccs->status;
}

static void
ccs_error_error (GnmCmdContext *cc, GError *error)
{
	CmdContextStderr *ccs = COMMAND_CONTEXT_STDERR (cc);

	fprintf (stderr, "Error: %s\n", error->message);
	ccs->status = -1;
}
static void
ccs_error_info (GnmCmdContext *cc, ErrorInfo *error)
{
	CmdContextStderr *ccs = COMMAND_CONTEXT_STDERR (cc);

	error_info_print (error);
	ccs->status = -1;
}

static char *
ccs_get_password (G_GNUC_UNUSED GnmCmdContext *cc,
		  G_GNUC_UNUSED char const* filename)
{
	return NULL;
}
static void
ccs_set_sensitive (G_GNUC_UNUSED GnmCmdContext *cc,
		   G_GNUC_UNUSED gboolean sensitive)
{
}

static void
ccs_progress_set (GnmCmdContext *cc, gfloat val)
{
}

static void
ccs_progress_message_set (GnmCmdContext *cc, gchar const *msg)
{
}

static void
ccs_init (CmdContextStderr *ccs)
{
	ccs->status = 0;
}

static void
ccs_gnm_cmd_context_init (GnmCmdContextClass *cc_class)
{
	cc_class->get_password		= ccs_get_password;
	cc_class->set_sensitive	   	= ccs_set_sensitive;
	cc_class->progress_set		= ccs_progress_set;
	cc_class->progress_message_set	= ccs_progress_message_set;
	cc_class->error.error		= ccs_error_error;
	cc_class->error.error_info	= ccs_error_info;
}

GSF_CLASS_FULL (CmdContextStderr, cmd_context_stderr,
		NULL, ccs_init,
		G_TYPE_OBJECT, 0,
		GSF_INTERFACE (ccs_gnm_cmd_context_init, GNM_CMD_CONTEXT_TYPE))
