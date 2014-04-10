/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */

/*
 * io-context.c : Place holder for an io error context.
 *   It is intended to become a place to handle errors
 *   as well as storing non-fatal warnings.
 *
 * Authors:
 *	Jody Goldberg <jody@gnome.org>
 *	Zbigniew Chyla <cyba@gnome.pl>
 *
 * (C) 2000-2005 Jody Goldberg
 */
#include <goffice/goffice-config.h>
#include "io-context-priv.h"
#include "go-cmd-context.h"
#include <goffice/utils/go-file.h>
#include <gsf/gsf-impl-utils.h>
#include <gtk/gtkmain.h>

#define PROGRESS_UPDATE_STEP        0.01
#define PROGRESS_UPDATE_STEP_END    (1.0 / 400)
#define PROGRESS_UPDATE_PERIOD_SEC  0.20

#define IOC_CLASS(ioc) IO_CONTEXT_CLASS(G_OBJECT_GET_CLASS(ioc))

static void
io_context_init (IOContext *ioc)
{
	ioc->impl = NULL;
	ioc->info = NULL;
	ioc->error_occurred = FALSE;
	ioc->warning_occurred = FALSE;

	ioc->progress_ranges = NULL;
	ioc->progress_min = 0.0;
	ioc->progress_max = 1.0;
	ioc->last_progress = -1.0;
	ioc->last_time = 0.0;
	ioc->helper.helper_type = GO_PROGRESS_HELPER_NONE;
}

static void
ioc_finalize (GObject *obj)
{
	IOContext *ioc;

	g_return_if_fail (IS_IO_CONTEXT (obj));

	ioc = IO_CONTEXT (obj);
	error_info_free (ioc->info);
	if (ioc->impl) {
		go_cmd_context_progress_set (ioc->impl, 0.0);
		go_cmd_context_progress_message_set (ioc->impl, NULL);
		g_object_unref (G_OBJECT (ioc->impl));
	}

	G_OBJECT_CLASS (g_type_class_peek (G_TYPE_OBJECT))->finalize (obj);
}

static char *
ioc_get_password (GOCmdContext *cc, char const *filename)
{
	IOContext *ioc = (IOContext *)cc;
	return go_cmd_context_get_password (ioc->impl, filename);
}

static void
ioc_set_sensitive (GOCmdContext *cc, gboolean sensitive)
{
	(void)cc; (void)sensitive;
}

static void
ioc_error_error (GOCmdContext *cc, GError *err)
{
	gnumeric_io_error_string (IO_CONTEXT (cc), err->message);
}

static void
ioc_error_error_info (G_GNUC_UNUSED GOCmdContext *ctxt,
		      ErrorInfo *error)
{
	/* TODO what goes here */
	error_info_print (error);
}

void
gnumeric_io_error_string (IOContext *context, const gchar *str)
{
	ErrorInfo *error;

	g_return_if_fail (context != NULL);
	g_return_if_fail (str != NULL);

	error = error_info_new_str (str);
	gnumeric_io_error_info_set (context, error);
}

static void
io_context_gnm_cmd_context_init (GOCmdContextClass *cc_class)
{
	cc_class->get_password	   = ioc_get_password;
	cc_class->set_sensitive	   = ioc_set_sensitive;
	cc_class->error.error      = ioc_error_error;
	cc_class->error.error_info = ioc_error_error_info;
}
static void
io_context_class_init (GObjectClass *klass)
{
	klass->finalize = ioc_finalize;
}

GSF_CLASS_FULL (IOContext, io_context,
		NULL, NULL, io_context_class_init, NULL,
		io_context_init, G_TYPE_OBJECT, 0,
		GSF_INTERFACE (io_context_gnm_cmd_context_init, GO_CMD_CONTEXT_TYPE))

IOContext *
gnumeric_io_context_new (GOCmdContext *cc)
{
	IOContext *ioc;

	g_return_val_if_fail (IS_GO_CMD_CONTEXT (cc), NULL);

	ioc = g_object_new (TYPE_IO_CONTEXT, NULL);
	/* The cc is optional for subclasses, but mandatory in this class. */
	ioc->impl = cc;
	g_object_ref (G_OBJECT (ioc->impl));

	return ioc;
}

void
gnumeric_io_error_unknown (IOContext *context)
{
	g_return_if_fail (context != NULL);

	context->error_occurred = TRUE;
}

void
gnumeric_io_error_info_set (IOContext *context, ErrorInfo *error)
{
	g_return_if_fail (context != NULL);
	g_return_if_fail (error != NULL);

	g_return_if_fail (context->info == NULL);

	context->info = error;
	context->error_occurred = TRUE;
}

void
gnumeric_io_error_push (IOContext *context, ErrorInfo *error)
{
	g_return_if_fail (context != NULL);
	g_return_if_fail (error != NULL);

	error_info_add_details (error, context->info);
	context->info = error;
}

void
gnumeric_io_error_display (IOContext *context)
{
	GOCmdContext *cc;

	g_return_if_fail (context != NULL);

	if (context->info != NULL) {
		if (context->impl)
			cc = context->impl;
		else
			cc = GO_CMD_CONTEXT (context);
		go_cmd_context_error_info (cc, context->info);
	}
}

/* TODO: Rename to gnumeric_io_info_clear */
void
gnumeric_io_error_clear (IOContext *context)
{
	g_return_if_fail (context != NULL);

	context->error_occurred = FALSE;
	context->warning_occurred = FALSE;
	error_info_free (context->info);
	context->info = NULL;
}

gboolean
gnumeric_io_error_occurred (IOContext *context)
{
	return context->error_occurred;
}

gboolean
gnumeric_io_warning_occurred (IOContext *context)
{
	return context->warning_occurred;
}

void
io_progress_update (IOContext *ioc, gdouble f)
{
	gboolean at_end;

	g_return_if_fail (IS_IO_CONTEXT (ioc));

	if (ioc->progress_ranges != NULL) {
		f = f * (ioc->progress_max - ioc->progress_min)
		    + ioc->progress_min;
	}

	at_end = (f - ioc->last_progress > PROGRESS_UPDATE_STEP_END &&
		  f + PROGRESS_UPDATE_STEP > 1);
	if (at_end || f - ioc->last_progress >= PROGRESS_UPDATE_STEP) {
		GTimeVal tv;
		double t;

		(void) g_get_current_time (&tv);
		t = tv.tv_sec + tv.tv_usec / 1000000.0;
		if (at_end || t - ioc->last_time >= PROGRESS_UPDATE_PERIOD_SEC) {
			GOCmdContext *cc;

			if (ioc->impl)
				cc = ioc->impl;
			else
				cc = GO_CMD_CONTEXT (ioc);
			go_cmd_context_progress_set (cc, f);
			ioc->last_time = t;
			ioc->last_progress = f;
		}
	}

	/* FIXME : abstract this into the workbook control */
	while (gtk_events_pending ())
		gtk_main_iteration_do (FALSE);
}

void
io_progress_message (IOContext *ioc, const gchar *msg)
{
	GOCmdContext *cc;

	g_return_if_fail (IS_IO_CONTEXT (ioc));

	if (ioc->impl)
		cc = ioc->impl;
	else
		cc = GO_CMD_CONTEXT (ioc);
	go_cmd_context_progress_message_set (cc, msg);
}

void
io_progress_range_push (IOContext *ioc, gdouble min, gdouble max)
{
	ProgressRange *r;
	gdouble new_min, new_max;

	g_return_if_fail (IS_IO_CONTEXT (ioc));

	r = g_new (ProgressRange, 1);
	r->min = min;
	r->max = max;
	ioc->progress_ranges = g_list_append (ioc->progress_ranges, r);

	new_min = min / (ioc->progress_max - ioc->progress_min)
	          + ioc->progress_min;
	new_max = max / (ioc->progress_max - ioc->progress_min)
	          + ioc->progress_min;
	ioc->progress_min = new_min;
	ioc->progress_max = new_max;
}

void
io_progress_range_pop (IOContext *ioc)
{
	GList *l;

	g_return_if_fail (IS_IO_CONTEXT (ioc));
	g_return_if_fail (ioc->progress_ranges != NULL);

	l = g_list_last (ioc->progress_ranges);
	ioc->progress_ranges= g_list_remove_link (ioc->progress_ranges, l);
	g_free (l->data);
	g_list_free_1 (l);

	ioc->progress_min = 0.0;
	ioc->progress_max = 1.0;
	for (l = ioc->progress_ranges; l != NULL; l = l->next) {
		ProgressRange *r = l->data;
		gdouble new_min, new_max;

		new_min = r->min / (ioc->progress_max - ioc->progress_min)
		          + ioc->progress_min;
		new_max = r->max / (ioc->progress_max - ioc->progress_min)
		          + ioc->progress_min;
		ioc->progress_min = new_min;
		ioc->progress_max = new_max;
	}
}

void
value_io_progress_set (IOContext *ioc, gint total, gint step)
{
	g_return_if_fail (IS_IO_CONTEXT (ioc));
	g_return_if_fail (total >= 0);

	ioc->helper.helper_type = GO_PROGRESS_HELPER_VALUE;
	ioc->helper.v.value.total = MAX (total, 1);
	ioc->helper.v.value.last = -step;
	ioc->helper.v.value.step = step;
}

void
value_io_progress_update (IOContext *ioc, gint value)
{
	gdouble complete;
	gint step, total;

	g_return_if_fail (IS_IO_CONTEXT (ioc));
	g_return_if_fail (ioc->helper.helper_type == GO_PROGRESS_HELPER_VALUE);

	total = ioc->helper.v.value.total;
	step = ioc->helper.v.value.step;

	if (value - ioc->helper.v.value.last < step &&
	    value + step < total) {
		return;
	}
	ioc->helper.v.value.last = value;

	complete = (gdouble)value / total;
	io_progress_update (ioc, complete);
}

void
count_io_progress_set (IOContext *ioc, gint total, gint step)
{
	g_return_if_fail (IS_IO_CONTEXT (ioc));
	g_return_if_fail (total >= 0);

	ioc->helper.helper_type = GO_PROGRESS_HELPER_COUNT;
	ioc->helper.v.count.total = MAX (total, 1);
	ioc->helper.v.count.last = -step;
	ioc->helper.v.count.current = 0;
	ioc->helper.v.count.step = step;
}

void
count_io_progress_update (IOContext *ioc, gint inc)
{
	gdouble complete;
	gint current, step, total;

	g_return_if_fail (IS_IO_CONTEXT (ioc));
	g_return_if_fail (ioc->helper.helper_type == GO_PROGRESS_HELPER_COUNT);

	current = (ioc->helper.v.count.current += inc);
	step = ioc->helper.v.count.step;
	total = ioc->helper.v.count.total;

	if (current - ioc->helper.v.count.last < step && current + step < total) {
		return;
	}
	ioc->helper.v.count.last = current;

	complete = (gdouble)current / total;
	io_progress_update (ioc, complete);
}

void
io_progress_unset (IOContext *ioc)
{
	g_return_if_fail (IS_IO_CONTEXT (ioc));

	ioc->helper.helper_type = GO_PROGRESS_HELPER_NONE;
}

void
gnm_io_context_set_num_files (IOContext *ioc, guint count)
{
	IOContextClass *klass = IOC_CLASS(ioc);
	g_return_if_fail (klass != NULL);
	if (klass->set_num_files != NULL)
		klass->set_num_files (ioc, count);
}

/**
 * gnm_io_context_processing_file :
 * @ioc : #IOContext
 * @uri : An escaped uri (eg foo%20bar)
 **/
void
gnm_io_context_processing_file (IOContext *ioc, char const *uri)
{
	char *basename;
	IOContextClass *klass = IOC_CLASS(ioc);

	g_return_if_fail (klass != NULL);

	basename = go_basename_from_uri (uri); /* unescape the uri */
	if (basename != NULL && klass->processing_file != NULL)
		klass->processing_file (ioc, basename);
	g_free (basename);
}

void
gnm_io_warning (G_GNUC_UNUSED IOContext *context,
		char const *fmt, ...)
{
	va_list args;

	va_start (args, fmt);
	gnm_io_warning_varargs (context, fmt, args);
	va_end (args);
}

void
gnm_io_warning_varargs (IOContext *context, char const *fmt, va_list args)
{
	context->info = error_info_new_vprintf (GO_WARNING, fmt, args);
	context->warning_occurred = TRUE;
}

void
gnm_io_warning_unknown_font (IOContext *context,
			     G_GNUC_UNUSED char const *font_name)
{
	g_return_if_fail (IS_IO_CONTEXT (context));
}

void
gnm_io_warning_unknown_function	(IOContext *context,
				 G_GNUC_UNUSED char const *funct_name)
{
	g_return_if_fail (IS_IO_CONTEXT (context));
}

void
gnm_io_warning_unsupported_feature (IOContext *context, char const *feature)
{
	g_return_if_fail (IS_IO_CONTEXT (context));
	g_warning ("%s : are not supported yet", feature);
}
