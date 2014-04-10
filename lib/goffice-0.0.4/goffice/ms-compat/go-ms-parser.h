/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * go-drawing-ms.h: MS Office Graphic Object I/O support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2004 Michael Meeks, Jody Goldberg, Chris Lahey
 **/

#ifndef GO_MS_PARSER_H
#define GO_MS_PARSER_H

#include <gsf/gsf.h>

G_BEGIN_DECLS

#define GO_MS_PARSER_STACK_TOP(stack) ((stack) ? (GOMSParserRecord *) (stack)->data : NULL)
#define GO_MS_PARSER_STACK_SECOND(stack) ((stack) && (stack)->next ? (GOMSParserRecord *) (stack)->next->data : NULL)

typedef struct
{
	guint  typecode;
	const char *name;
	gboolean    is_container;
	gboolean    do_read; /* If false, data will be NULL.  Ignored for containers. */
	int min_record_size;
	int max_record_size;
} GOMSParserRecordType;

typedef struct {
	guint16 opcode;
	guint8 vers;
	guint16 inst;
	guint32 length;
	guint32 length_remaining;
	gpointer parse_state;
	const GOMSParserRecordType *type;
} GOMSParserRecord;

/* stack is of type GOMSParserRecord */
typedef void (*GOMSParserHandleAtom) (GOMSParserRecord *record, GSList *stack, const guint8 *data, GsfInput *input, GError **err, gpointer user_data);
typedef void (*GOMSParserStartContainer) (GSList *stack, GsfInput *input, GError **err, gpointer user_data);
typedef void (*GOMSParserEndContainer) (GSList *stack, GsfInput *input, GError **err, gpointer user_data);

typedef struct {
	GOMSParserHandleAtom handle_atom;
	GOMSParserStartContainer start_container;
	GOMSParserEndContainer end_container;
} GOMSParserCallbacks;

void go_ms_parser_read        (GsfInput   *input,
			       gsf_off_t   length,
			       const GOMSParserRecordType *types,
			       guint type_count,
			       GOMSParserCallbacks *callbacks,
			       gpointer user_data,
			       GError    **err);
#if 0
int             go_ms_parser_write       (GODrawing  *drawing,
					  GsfOutput  *output);
#endif

G_END_DECLS

#endif /* GO_MS_PARSER_H */
