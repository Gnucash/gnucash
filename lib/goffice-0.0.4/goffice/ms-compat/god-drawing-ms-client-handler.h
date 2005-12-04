/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * god-drawing-ms-client-handler.h: MS Office Graphic Object support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2003 Michael Meeks, Jody Goldberg, Chris Lahey
 **/

#ifndef GOD_DRAWING_MS_CLIENT_HANDLER_H
#define GOD_DRAWING_MS_CLIENT_HANDLER_H

#include <glib-object.h>
#include <glib.h>
#include <goffice/drawing/god-text-model.h>
#include <goffice/drawing/god-anchor.h>
#include <gsf/gsf.h>

G_BEGIN_DECLS

#define GOD_DRAWING_MS_CLIENT_HANDLER_TYPE		(god_drawing_ms_client_handler_get_type ())
#define GOD_DRAWING_MS_CLIENT_HANDLER(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOD_DRAWING_MS_CLIENT_HANDLER_TYPE, GodDrawingMsClientHandler))
#define GOD_DRAWING_MS_CLIENT_HANDLER_CLASS(k)		(G_TYPE_CHECK_CLASS_CAST ((k), GOD_DRAWING_MS_CLIENT_HANDLER_TYPE, GodDrawingMsClientHandlerClass))
#define GOD_DRAWING_MS_CLIENT_HANDLER_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS((o), GOD_DRAWING_MS_CLIENT_HANDLER_TYPE, GodDrawingMsClientHandlerClass))
#define IS_GOD_DRAWING_MS_CLIENT_HANDLER(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOD_DRAWING_MS_CLIENT_HANDLER_TYPE))
#define IS_GOD_DRAWING_MS_CLIENT_HANDLER_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOD_DRAWING_MS_CLIENT_HANDLER_TYPE))

typedef struct GodDrawingMsClientHandlerPrivate_ GodDrawingMsClientHandlerPrivate;

typedef struct {
	GObject parent;
	GodDrawingMsClientHandlerPrivate *priv;
} GodDrawingMsClientHandler;

typedef struct {
	GObjectClass parent_class;

	GodTextModel *(*handle_client_text)   (GodDrawingMsClientHandler *handler, const guint8 *data, GsfInput *input, gsf_off_t length, GError **err);
	GodAnchor    *(*handle_client_anchor) (GodDrawingMsClientHandler *handler, const guint8 *data, GsfInput *input, gsf_off_t length, GError **err);
	GObject      *(*handle_client_data)   (GodDrawingMsClientHandler *handler, const guint8 *data, GsfInput *input, gsf_off_t length, GError **err);

	guint client_text_read_data : 1;
	guint client_anchor_read_data : 1;
	guint client_data_read_data : 1;
} GodDrawingMsClientHandlerClass;

GType         god_drawing_ms_client_handler_get_type              (void);

GodTextModel *god_drawing_ms_client_handler_handle_client_text    (GodDrawingMsClientHandler  *handler,
								   GsfInput                   *input,
								   gsf_off_t                   length,
								   GError                    **err);
GodAnchor    *god_drawing_ms_client_handler_handle_client_anchor  (GodDrawingMsClientHandler  *handler,
								   GsfInput                   *input,
								   gsf_off_t                   length,
								   GError                    **err);
GObject      *god_drawing_ms_client_handler_handle_client_data    (GodDrawingMsClientHandler  *handler,
								   GsfInput                   *input,
								   gsf_off_t                   length,
								   GError                    **err);



G_END_DECLS

#endif /* GOD_DRAWING_MS_CLIENT_HANDLER_H */
