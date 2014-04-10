/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * god-drawing-ms-client-handler.c: MS Office Graphic Object support
 *
 * Copyright (C) 2000-2002
 *	Jody Goldberg (jody@gnome.org)
 *	Michael Meeks (mmeeks@gnu.org)
 *      Christopher James Lahey <clahey@ximian.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <goffice/goffice-config.h>
#include <goffice/ms-compat/god-drawing-ms-client-handler.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-input.h>
#include <string.h>

static GObjectClass *parent_class;

struct GodDrawingMsClientHandlerPrivate_ {
	int dummy;
};

GodTextModel *
god_drawing_ms_client_handler_handle_client_text    (GodDrawingMsClientHandler *handler,
						     GsfInput                  *input,
						     gsf_off_t                  length,
						     GError                    **err)
{
	if (GOD_DRAWING_MS_CLIENT_HANDLER_GET_CLASS (handler)->handle_client_text) {
		const guint8 *data = NULL;
		if (GOD_DRAWING_MS_CLIENT_HANDLER_GET_CLASS (handler)->client_text_read_data)
			data = gsf_input_read (input, length, NULL);
		return GOD_DRAWING_MS_CLIENT_HANDLER_GET_CLASS (handler)->handle_client_text (handler,
											      data,
											      input,
											      length,
											      err);
	} else {
		return NULL;
	}
}

GodAnchor *
god_drawing_ms_client_handler_handle_client_anchor    (GodDrawingMsClientHandler *handler,
						       GsfInput                  *input,
						       gsf_off_t                  length,
						       GError                    **err)
{
	if (GOD_DRAWING_MS_CLIENT_HANDLER_GET_CLASS (handler)->handle_client_anchor) {
		const guint8 *data = NULL;
		if (GOD_DRAWING_MS_CLIENT_HANDLER_GET_CLASS (handler)->client_anchor_read_data)
			data = gsf_input_read (input, length, NULL);
		return GOD_DRAWING_MS_CLIENT_HANDLER_GET_CLASS (handler)->handle_client_anchor (handler,
												data,
												input,
												length,
												err);
	} else {
		return NULL;
	}
}

GObject *
god_drawing_ms_client_handler_handle_client_data    (GodDrawingMsClientHandler *handler,
						     GsfInput                  *input,
						     gsf_off_t                  length,
						     GError                    **err)
{
	if (GOD_DRAWING_MS_CLIENT_HANDLER_GET_CLASS (handler)->handle_client_data) {
		const guint8 *data = NULL;
		if (GOD_DRAWING_MS_CLIENT_HANDLER_GET_CLASS (handler)->client_data_read_data)
			data = gsf_input_read (input, length, NULL);
		return GOD_DRAWING_MS_CLIENT_HANDLER_GET_CLASS (handler)->handle_client_data (handler,
											      data,
											      input,
											      length,
											      err);
	} else {
		return NULL;
	}
}

static void
god_drawing_ms_client_handler_init (GObject *object)
{
	GodDrawingMsClientHandler *handler = GOD_DRAWING_MS_CLIENT_HANDLER (object);
	handler->priv = g_new0 (GodDrawingMsClientHandlerPrivate, 1);
}

static void
god_drawing_ms_client_handler_finalize (GObject *object)
{
	GodDrawingMsClientHandler *handler = GOD_DRAWING_MS_CLIENT_HANDLER (object);

	g_free (handler->priv);
	handler->priv = NULL;

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
god_drawing_ms_client_handler_class_init (GodDrawingMsClientHandlerClass *class)
{
	GObjectClass *object_class;

	object_class                   = (GObjectClass *) class;

	parent_class                   = g_type_class_peek_parent (class);

	object_class->finalize         = god_drawing_ms_client_handler_finalize;

	class->client_text_read_data   = TRUE;
	class->client_anchor_read_data = TRUE;
	class->client_data_read_data   = TRUE;
}

GSF_CLASS (GodDrawingMsClientHandler, god_drawing_ms_client_handler,
	   god_drawing_ms_client_handler_class_init, god_drawing_ms_client_handler_init,
	   G_TYPE_OBJECT)
