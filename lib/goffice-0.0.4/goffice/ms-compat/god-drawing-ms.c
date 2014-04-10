/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* 
 * god-drawing-ms.h - 
 * Copyright (C) 2003-2004, Christopher James Lahey
 *
 * Authors:
 *   Christopher James Lahey <clahey@ximian.com>
 *
 * This file is free software; you can redistribute it and/or modify
 * it under the terms of version 2 of the GNU Library General Public
 * License as published by the Free Software Foundation.
 *
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this file; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA.
 **/

#include <goffice/goffice-config.h>
#include <goffice/ms-compat/god-drawing-ms.h>
#include <goffice/ms-compat/go-ms-parser.h>
#include <goffice/ms-compat/god-image-ms.h>
#include <goffice/drawing/god-property-table.h>
#include <goffice/drawing/god-shape.h>
#include <glib/gmacros.h>
#include <gsf/gsf-input.h>
#include <gsf/gsf-utils.h>
#include <string.h>

#define d(x) x

#define CVS_VERSION "$Id: god-drawing-ms.c,v 1.12 2005/08/08 08:57:01 jdassen Exp $"
#define ERROR_STRING(cond,str) G_STRLOC "\n<" CVS_VERSION ">\n" str " (" #cond ")"
#define ERROR(cond,str) { \
	if (!(cond)) { \
		if (err) \
			g_set_error (err, domain, code, ERROR_STRING(cond,str)); \
		else \
			g_warning (ERROR_STRING(cond,str)); \
		return; \
	} \
}

static GQuark domain;
static gint code;

static void
god_drawing_ms_init (void)
{
	static gboolean inited = FALSE;
	if (inited)
		return;
	domain = g_quark_from_static_string ("GodDrawingMs");
	code = 1;
	inited = TRUE;
}

typedef enum {
	EscherDggContainer		= 0xf000, /* Drawing Group Container */
	EscherDgg			= 0xf006,
	EscherCLSID			= 0xf016,
	EscherOPT			= 0xf00b,
	EscherBStoreContainer		= 0xf001,
	EscherBSE			= 0xf007,
	EscherBlip_START		= 0xf018, /* Blip types are between */
	EscherBlip_END			= 0xf117, /* these two values */
	EscherDgContainer		= 0xf002, /* Drawing Container */
	EscherDg			= 0xf008,
	EscherRegroupItems		= 0xf118,
	EscherColorScheme		= 0xf120, /* bug in docs */
	EscherSpgrContainer		= 0xf003,
	EscherSpContainer		= 0xf004,
	EscherSpgr			= 0xf009,
	EscherSp			= 0xf00a,
	EscherTextbox			= 0xf00c,
	EscherClientTextbox		= 0xf00d,
	EscherAnchor			= 0xf00e,
	EscherChildAnchor		= 0xf00f,
	EscherClientAnchor		= 0xf010,
	EscherClientData		= 0xf011,
	EscherSolverContainer		= 0xf005,
	EscherConnectorRule		= 0xf012, /* bug in docs */
	EscherAlignRule			= 0xf013,
	EscherArcRule			= 0xf014,
	EscherClientRule		= 0xf015,
	EscherCalloutRule		= 0xf017,
	EscherSelection			= 0xf119,
	EscherColorMRU			= 0xf11a,
	EscherDeletedPspl		= 0xf11d, /* bug in docs */
	EscherSplitMenuColors		= 0xf11e,
	EscherOleObject			= 0xf11f,
	EscherUserDefined		= 0xf122,
} EscherTypecode;

static const GOMSParserRecordType types[] =
{
	{	EscherDggContainer,		"EscherDggContainer",		TRUE,	FALSE,	-1,	-1	},
	{	EscherDgg,			"EscherDgg",			FALSE,	FALSE,	-1,	-1	},
	{	EscherCLSID,			"EscherCLSID",			FALSE,	FALSE,	-1,	-1	},
	{	EscherOPT,			"EscherOPT",			FALSE,	TRUE,	-1,	-1	},
	{	EscherBStoreContainer,		"EscherBStoreContainer",	TRUE,	FALSE,	-1,	-1	},
	{	EscherBSE,			"EscherBSE",			FALSE,	TRUE,	-1,	-1	},
	{	EscherBlip_START,		"EscherBlip_START",		FALSE,	FALSE,	-1,	-1	},
	{	EscherBlip_END,			"EscherBlip_END",		FALSE,	FALSE,	-1,	-1	},
	{	EscherDgContainer,		"EscherDgContainer",		TRUE,	FALSE,	-1,	-1	},
	{	EscherDg,			"EscherDg",			FALSE,	FALSE,	-1,	-1	},
	{	EscherRegroupItems,		"EscherRegroupItems",		FALSE,	FALSE,	-1,	-1	},
	{	EscherColorScheme,		"EscherColorScheme",		FALSE,	FALSE,	-1,	-1	},
	{	EscherSpgrContainer,		"EscherSpgrContainer",		TRUE,	FALSE,	-1,	-1	},
	{	EscherSpContainer,		"EscherSpContainer",		TRUE,	FALSE,	-1,	-1	},
	{	EscherSpgr,			"EscherSpgr",			FALSE,	FALSE,	-1,	-1	},
	{	EscherSp,			"EscherSp",			FALSE,	TRUE,	-1,	-1	},
	{	EscherTextbox,			"EscherTextbox",		FALSE,	FALSE,	-1,	-1	},
	{	EscherClientTextbox,		"EscherClientTextbox",		FALSE,	FALSE,	-1,	-1	},
	{	EscherAnchor,			"EscherAnchor",			FALSE,	FALSE,	-1,	-1	},
	{	EscherChildAnchor,		"EscherChildAnchor",		FALSE,	FALSE,	-1,	-1	},
	{	EscherClientAnchor,		"EscherClientAnchor",		FALSE,	FALSE,	-1,	-1	},
	{	EscherClientData,		"EscherClientData",		FALSE,	FALSE,	-1,	-1	},
	{	EscherSolverContainer,		"EscherSolverContainer",	TRUE,	FALSE,	-1,	-1	},
	{	EscherConnectorRule,		"EscherConnectorRule",		FALSE,	FALSE,	-1,	-1	},
	{	EscherAlignRule,		"EscherAlignRule",		FALSE,	FALSE,	-1,	-1	},
	{	EscherArcRule,			"EscherArcRule",		FALSE,	FALSE,	-1,	-1	},
	{	EscherClientRule,		"EscherClientRule",		FALSE,	FALSE,	-1,	-1	},
	{	EscherCalloutRule,		"EscherCalloutRule",		FALSE,	FALSE,	-1,	-1	},
	{	EscherSelection,		"EscherSelection",		FALSE,	FALSE,	-1,	-1	},
	{	EscherColorMRU,			"EscherColorMRU",		FALSE,	FALSE,	-1,	-1	},
	{	EscherDeletedPspl,		"EscherDeletedPspl",		FALSE,	FALSE,	-1,	-1	},
	{	EscherSplitMenuColors,		"EscherSplitMenuColors",	FALSE,	FALSE,	-1,	-1	},
	{	EscherOleObject,		"EscherOleObject",		FALSE,	FALSE,	-1,	-1	},
	{	EscherUserDefined,		"EscherUserDefined",		FALSE,	FALSE,	-1,	-1	},
};

typedef guint32 Spid;

typedef struct {
	Spid id;
	guint is_group : 1;      /* This shape is a group shape */
	guint is_child : 1;      /* Not a top-level shape */
	guint is_patriarch : 1;  /* This is the topmost group shape. */
	guint is_deleted : 1;    /* The shape has been deleted */
	guint is_ole_shape : 1;  /* The shape is an OLE object */
	guint have_master : 1;   /* Shape has a hspMaster property */
	guint is_flip_h : 1;     /* Shape is flipped horizontally */
	guint is_flip_v : 1;     /* Shape is flipped vertically */
	guint is_connector : 1;  /* Connector type of shape */
	guint have_anchor : 1;   /* Shape has an anchor of some kind */
	guint is_background : 1; /* Background shape */
	guint have_spt : 1;      /* Shape has a shape type property */
} ShapeDetails;

typedef struct {
	GodPropertyTable *prop_table;
	GodAnchor *anchor;
	GodTextModel *text_model;
	ShapeDetails sp;
} ShapeParseState;

typedef struct {
	GodShape *main_shape;
	GList *shapes; /* Of type GodShape */
	ShapeDetails sp;
} ShapeGroupParseState;

typedef struct {
	GodShape *root_shape;
	GodShape *background;
} DrawingParseState;

typedef struct {
	GodDrawing *drawing;
	GodDrawingGroup *drawing_group;
	GodDrawingMsClientHandler *handler;
} ParseCallbackData;

#define STACK_TOP GO_MS_PARSER_STACK_TOP(stack)
#define STACK_SECOND GO_MS_PARSER_STACK_SECOND(stack)

static void
handle_atom (GOMSParserRecord *record, GSList *stack, const guint8 *data, GsfInput *input, GError **err, gpointer user_data)
{
	ParseCallbackData *cb_data = user_data;
	switch (record->opcode) {
	case EscherClientAnchor:
		{
			ShapeParseState *parse_state;

			ERROR (STACK_TOP && STACK_TOP->opcode == EscherSpContainer, "Placement Error");

			parse_state = STACK_TOP->parse_state;

			ERROR (parse_state->anchor == NULL, "Placement Error");

			if (cb_data->handler) {
				parse_state->anchor = god_drawing_ms_client_handler_handle_client_anchor
					(cb_data->handler,
					 input,
					 record->length,
					 err);
			}
		}
		break;
	case EscherClientTextbox:
		{
			ShapeParseState *parse_state;

			ERROR (STACK_TOP && STACK_TOP->opcode == EscherSpContainer, "Placement Error");

			parse_state = STACK_TOP->parse_state;

			ERROR (parse_state->text_model == NULL, "Placement Error");

			if (cb_data->handler) {
				parse_state->text_model = god_drawing_ms_client_handler_handle_client_text
					(cb_data->handler,
					 input,
					 record->length,
					 err);
			}
		}
		break;
	case EscherOPT:
		{
			int i;
			guint complex_offset;
			ShapeParseState *parse_state;

			ERROR (STACK_TOP && (STACK_TOP->opcode == EscherSpContainer ||
					     STACK_TOP->opcode == EscherDggContainer), "Placement Error");

			if (STACK_TOP->opcode == EscherDggContainer)
				break;

			parse_state = STACK_TOP->parse_state;

			ERROR (parse_state->prop_table == NULL, "Placement Error");

			parse_state->prop_table = god_property_table_new ();
			complex_offset = 6 * record->inst;
			ERROR (record->length >= complex_offset, "Length Error");
			for (i = 0; i < record->inst; i++) {
				int id = GSF_LE_GET_GUINT16 (data + i * 6);
#if 0
				gboolean is_bid = id & 0x4000;
#endif
				gboolean is_complex = id & 0x8000;
				guint32 opt_data = GSF_LE_GET_GUINT32 (data + i * 6 + 2);
				guint32 color = (data[i * 6 + 3] << 16) | (data[i * 6 + 4] << 8) | data[i * 6 + 5];

				id &= 0x3fff;
				switch (id) {
				case 128:
					god_property_table_set_int
						(parse_state->prop_table,
						 GOD_PROPERTY_LTXID,
						 opt_data);
					break;
				case 129:
					god_property_table_set_length
						(parse_state->prop_table,
						 GOD_PROPERTY_DX_TEXT_LEFT,
						 GO_EMU_TO_UN(opt_data));
					break;
				case 130:
					god_property_table_set_length
						(parse_state->prop_table,
						 GOD_PROPERTY_DX_TEXT_TOP,
						 GO_EMU_TO_UN(opt_data));
					break;
				case 131:
					god_property_table_set_length
						(parse_state->prop_table,
						 GOD_PROPERTY_DX_TEXT_RIGHT,
						 GO_EMU_TO_UN(opt_data));
					break;
				case 132:
					god_property_table_set_length
						(parse_state->prop_table,
						 GOD_PROPERTY_DX_TEXT_BOTTOM,
						 GO_EMU_TO_UN(opt_data));
					break;
				case 133:
					/*
typedef enum
   {
   msowrapSquare,
   msowrapByPoints,
   msowrapNone,
   msowrapTopBottom,
   msowrapThrough,
   } MSOWRAPMODE;
					*/
					break;
				case 135:
					/*
typedef enum
   {
   msoanchorTop, 
   msoanchorMiddle, 
   msoanchorBottom, 
   msoanchorTopCentered, 
   msoanchorMiddleCentered, 
   msoanchorBottomCentered,
   msoanchorTopBaseline,
   msoanchorBottomBaseline,
   msoanchorTopCenteredBaseline,
   msoanchorBottomCenteredBaseline
   } MSOANCHOR;
					*/
					break;
 
				case 260:
					god_property_table_set_int
						(parse_state->prop_table,
						 GOD_PROPERTY_BLIP_ID,
						 opt_data - 1);
					break;
				case 384:
					god_property_table_set_int
						(parse_state->prop_table,
						 GOD_PROPERTY_FILL_TYPE,
						 opt_data);
					break;
				case 385:
					god_property_table_set_uint
						(parse_state->prop_table,
						 GOD_PROPERTY_FILL_COLOR,
						 color);
					break;
				case 386:
					god_property_table_set_int
						(parse_state->prop_table,
						 GOD_PROPERTY_FILL_ALPHA,
						 opt_data);
					break;
				case 387:
					god_property_table_set_uint
						(parse_state->prop_table,
						 GOD_PROPERTY_FILL_BACKGROUND,
						 color);
					break;
				case 388:
					god_property_table_set_int
						(parse_state->prop_table,
						 GOD_PROPERTY_FILL_BACKGROUND_ALPHA,
						 opt_data);
					break;
				case 401:
					god_property_table_set_int
						(parse_state->prop_table,
						 GOD_PROPERTY_FILL_RECT_LEFT,
						 GO_EMU_TO_UN(opt_data));
					break;
				case 402:
					god_property_table_set_int
						(parse_state->prop_table,
						 GOD_PROPERTY_FILL_RECT_TOP,
						 GO_EMU_TO_UN(opt_data));
					break;
				case 403:
					god_property_table_set_int
						(parse_state->prop_table,
						 GOD_PROPERTY_FILL_RECT_RIGHT,
						 GO_EMU_TO_UN(opt_data));
					break;
				case 404:
					god_property_table_set_int
						(parse_state->prop_table,
						 GOD_PROPERTY_FILL_RECT_BOTTOM,
						 GO_EMU_TO_UN(opt_data));
					break;
				case 447:
					god_property_table_set_flag
						(parse_state->prop_table,
						 GOD_PROPERTY_FILLED,
						 opt_data & 0x00000010);
					break;
				case 448:
					/* Line color */
					break;
				case 450:
					/* Line background color */
					break;
				case 769:
					/* Master shape */
					break;
				case 831:
					god_property_table_set_flag
						(parse_state->prop_table,
						 GOD_PROPERTY_BACKGROUND,
						 opt_data & 0x1);
					break;
				}
				if (is_complex) {
					complex_offset += opt_data;
					ERROR (record->length >= complex_offset, "Length Error");
				}
			}
		}
		break;
	case EscherSp:
		{
			ShapeParseState *parse_state;

			ERROR (STACK_TOP && STACK_TOP->opcode == EscherSpContainer, "Placement Error");

			parse_state = STACK_TOP->parse_state;

			parse_state->sp.id = GSF_LE_GET_GUINT32 (data);
			parse_state->sp.is_group = !!(data[4] & 0x01);
			parse_state->sp.is_child = !!(data[4] & 0x02);
			parse_state->sp.is_patriarch = !!(data[4] & 0x04);
			parse_state->sp.is_deleted = !!(data[4] & 0x08);
			parse_state->sp.is_ole_shape = !!(data[4] & 0x10);
			parse_state->sp.have_master = !!(data[4] & 0x20);
			parse_state->sp.is_flip_h = !!(data[4] & 0x40);
			parse_state->sp.is_flip_v = !!(data[4] & 0x80);
			parse_state->sp.is_connector = !!(data[5] & 0x01);
			parse_state->sp.have_anchor = !!(data[5] & 0x02);
			parse_state->sp.is_background = !!(data[5] & 0x04);
			parse_state->sp.have_spt = !!(data[5] & 0x08);
		}
		break;
	case EscherBSE:
		{
			GodImageStore *store;
			GodImage *image;
			ERROR (cb_data->drawing_group, "Placement Error");
			store = god_drawing_group_get_image_store (cb_data->drawing_group);
			image = god_image_ms_new ();
			god_image_ms_set_hash (GOD_IMAGE_MS(image), data + 2);
			god_image_store_append_image (store, image);
		}
		break;
	}

	if (record->opcode >= EscherBlip_START && record->opcode <= EscherBlip_END) {
		int i, image_count;
		GodImageStore *store;
		GodImage *image;

		ERROR (record->length >= 17, "Length Error");
		data = gsf_input_read (input, record->length, NULL);
		ERROR (data, "Length Error");

		ERROR (cb_data->drawing_group, "Placement Error");
		store = god_drawing_group_get_image_store (cb_data->drawing_group);
		image_count = god_image_store_get_image_count (store);
		for (i = 0; i < image_count; i++) {
			const guint8 *hash;
			image = god_image_store_get_image (store, i);
			hash = god_image_ms_get_hash (GOD_IMAGE_MS(image));
			if (!memcmp(hash, data, 16)) {
				god_image_set_image_data (image,
							  NULL,
							  data + 17,
							  record->length - 17);
			}
		}
		g_object_unref (store);
	}
}

static void
start_container (GSList *stack, GsfInput *input, GError **err, gpointer user_data)
{
	ParseCallbackData *cb_data = user_data;
	switch (STACK_TOP->opcode) {
	case EscherSpContainer:
		{
			ShapeParseState *parse_state = g_new0 (ShapeParseState, 1);
			STACK_TOP->parse_state = parse_state;
			ERROR (STACK_SECOND && (STACK_SECOND->opcode == EscherSpgrContainer ||
						STACK_SECOND->opcode == EscherDgContainer), "Placement Error");
		}
		break;
	case EscherSpgrContainer:
		{
			ShapeGroupParseState *parse_state = g_new0 (ShapeGroupParseState, 1);
			STACK_TOP->parse_state = parse_state;
			ERROR (STACK_SECOND && (STACK_SECOND->opcode == EscherSpgrContainer ||
						STACK_SECOND->opcode == EscherDgContainer), "Placement Error");
		}
		break;
	case EscherDgContainer:
		{
			DrawingParseState *parse_state = g_new0 (DrawingParseState, 1);
			STACK_TOP->parse_state = parse_state;
			ERROR (!STACK_SECOND, "Placement Error");
			ERROR (cb_data->drawing == NULL, "Multiple EscherDgContainers");
			cb_data->drawing = god_drawing_new();
		}
		break;
	case EscherDggContainer:
		{
			ERROR (!STACK_SECOND, "Placement Error");
			ERROR (cb_data->drawing_group == NULL, "Multiple EscherDggContainers");
			cb_data->drawing_group = god_drawing_group_new();
		}
		break;
	}
}

static void
append_shape_on_stack (GSList *stack, GError **err, GodShape *shape, ShapeDetails *sp)
{
	if (STACK_SECOND->opcode == EscherSpgrContainer) {
		ShapeGroupParseState *parent_state = STACK_SECOND->parse_state;
		ERROR (!sp->is_patriarch &&
		       !sp->is_background &&
		       !sp->is_deleted, "Placement Error");
		parent_state->shapes = g_list_prepend (parent_state->shapes,
						       shape);
		g_object_ref (shape);
	} else if (STACK_SECOND->opcode == EscherDgContainer) {
		DrawingParseState *parent_state = STACK_SECOND->parse_state;
		ERROR (sp->is_patriarch ||
		       sp->is_background ||
		       sp->is_deleted, "Placement Error");
		if (sp->is_patriarch) {
			ERROR (parent_state->root_shape == NULL, "Only one patriarch per drawing.");
			parent_state->root_shape = shape;
			g_object_ref (shape);
		} else if (sp->is_background) {
			ERROR (parent_state->background == NULL, "Only one background per drawing.");
			parent_state->background = shape;
			g_object_ref (shape);
		}
	}
}

static void
end_container (GSList *stack, GsfInput *input, GError **err, gpointer user_data)
{
	ParseCallbackData *cb_data = user_data;
	switch (STACK_TOP->opcode) {
	case EscherSpContainer:
		{
			ShapeParseState *parse_state = STACK_TOP->parse_state;
			GodShape *shape;
			shape = g_object_new (GOD_SHAPE_TYPE, NULL);
			if (parse_state->prop_table) {
				god_shape_set_prop_table (shape, parse_state->prop_table);
				g_object_unref (parse_state->prop_table);
			}
			if (parse_state->anchor) {
				god_shape_set_anchor (shape, parse_state->anchor);
				g_object_unref (parse_state->anchor);
			}
			if (parse_state->text_model) {
				god_shape_set_text_model (shape, parse_state->text_model);
				g_object_unref (parse_state->text_model);
			}
			if (parse_state->sp.is_group) {
				ShapeGroupParseState *parent_state = STACK_SECOND->parse_state;
				ERROR (parent_state->main_shape == NULL, "Placement Error");
				ERROR (STACK_SECOND->opcode == EscherSpgrContainer, "Placement Error");
				parent_state->main_shape = shape;
				parent_state->sp = parse_state->sp;
			} else {
				append_shape_on_stack (stack, err, shape, &parse_state->sp);
				g_object_unref (shape);
			}
		}
		break;
	case EscherSpgrContainer:
		{
			ShapeGroupParseState *parse_state = STACK_TOP->parse_state;
			GList *list;

			ERROR (parse_state->main_shape != NULL, "Children Error");
			parse_state->shapes = g_list_reverse (parse_state->shapes);
			for (list = parse_state->shapes; list; list = list->next) {
				god_shape_append_child (parse_state->main_shape, 
							       list->data);
				g_object_unref (list->data);
			}
			g_list_free (parse_state->shapes);
			append_shape_on_stack (stack, err, parse_state->main_shape, &parse_state->sp);
			g_object_unref (parse_state->main_shape);
			g_free (parse_state);
		}
		break;
	case EscherDgContainer:
		{
			DrawingParseState *parse_state = STACK_TOP->parse_state;

			god_drawing_set_root_shape (cb_data->drawing, parse_state->root_shape);
			god_drawing_set_background (cb_data->drawing, parse_state->background);
			g_object_unref (parse_state->root_shape);
			g_object_unref (parse_state->background);

			g_free (parse_state);
		}
		break;
	}
}

static GOMSParserCallbacks callbacks = { handle_atom,
					 start_container,
					 end_container };

GodDrawing *
god_drawing_read_ms (GsfInput   *input,
		     gsf_off_t   length,
		     GodDrawingMsClientHandler *handler,
		     GError    **err)
{
	ParseCallbackData cb_data;

	god_drawing_ms_init();

	cb_data.drawing = NULL;
	cb_data.drawing_group = NULL;
	cb_data.handler = handler;

	go_ms_parser_read (input,
			   length,
			   types,
			   (sizeof (types) / sizeof (types[0])),
			   &callbacks,
			   &cb_data,
			   err);

	if (cb_data.drawing_group)
		g_object_unref (cb_data.drawing_group);
	return cb_data.drawing;
}

GodDrawingGroup *
god_drawing_group_read_ms  (GsfInput   *input,
			    gsf_off_t   length,
			    GodDrawingMsClientHandler *handler,
			    GError    **err)
{
	ParseCallbackData cb_data;

	god_drawing_ms_init();

	cb_data.drawing = NULL;
	cb_data.drawing_group = NULL;
	cb_data.handler = handler;

	go_ms_parser_read (input,
			   length,
			   types,
			   (sizeof (types) / sizeof (types[0])),
			   &callbacks,
			   &cb_data,
			   err);

	if (cb_data.drawing)
		g_object_unref (cb_data.drawing);
	return cb_data.drawing_group;
}

void
god_drawing_group_parse_images  (GodDrawingGroup *drawing_group,
				 GsfInput   *input,
				 gsf_off_t   length,
				 GodDrawingMsClientHandler *handler,
				 GError    **err)
{
	ParseCallbackData cb_data;

	god_drawing_ms_init();

	cb_data.drawing = NULL;
	cb_data.drawing_group = drawing_group;
	cb_data.handler = handler;

	go_ms_parser_read (input,
			   length,
			   types,
			   (sizeof (types) / sizeof (types[0])),
			   &callbacks,
			   &cb_data,
			   err);
	if (cb_data.drawing)
		g_object_unref (cb_data.drawing);
}
