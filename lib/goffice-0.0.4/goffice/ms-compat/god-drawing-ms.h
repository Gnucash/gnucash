/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * god-drawing-ms.h: MS Office Graphic Object I/O support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2004 Michael Meeks, Jody Goldberg, Chris Lahey
 **/

#ifndef GO_DRAWING_MS_H
#define GO_DRAWING_MS_H

#include <goffice/drawing/god-drawing.h>
#include <goffice/drawing/god-drawing-group.h>
#include <goffice/ms-compat/god-drawing-ms-client-handler.h>
#include <gsf/gsf.h>

G_BEGIN_DECLS

GodDrawing      *god_drawing_read_ms             (GsfInput                   *input,
						  gsf_off_t                   length,
						  GodDrawingMsClientHandler  *handler,
						  GError                    **err);
GodDrawingGroup *god_drawing_group_read_ms       (GsfInput                   *input,
						  gsf_off_t                   length,
						  GodDrawingMsClientHandler  *handler,
						  GError                    **err);
void             god_drawing_group_parse_images  (GodDrawingGroup            *drawing_group,
						  GsfInput                   *input,
						  gsf_off_t                   length,
						  GodDrawingMsClientHandler  *handler,
						  GError                    **err);
#if 0
int              god_drawing_write_ms            (GodDrawing                 *drawing,
						  GsfOutput                  *output);
#endif

G_END_DECLS

#endif /* GO_DRAWING_MS_H */
