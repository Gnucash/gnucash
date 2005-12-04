/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-doc.h :  A GOffice document
 *
 * Copyright (C) 2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
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
#ifndef GO_DOC_H
#define GO_DOC_H

#include <goffice/app/goffice-app.h>
#include <glib-object.h>

G_BEGIN_DECLS

#define GO_DOC_TYPE	    (go_doc_get_type ())
#define GO_DOC(o)	    (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_DOC_TYPE, GODoc))
#define IS_GO_DOC(o)	    (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_DOC_TYPE))

GType go_doc_get_type (void);

#if 0
GODoc   *go_doc_new_from_input  (GsfInput *input,
				 GODocImporter const *fmt,
				 GOIOContext *context,
				 gchar const *encoding);
GODoc   *go_doc_new_from_uri	(char const *uri,
				 GnmFileOpener const *fmt,
				 GOIOContext *context, 
				 gchar const *encoding);
gboolean go_doc_save		(GODoc *doc, GOIOContext *context);
gboolean go_doc_save_as		(GODoc *doc, GODocExporter *fmt, char const *uri,
				 GOIOContext *cc);
gboolean go_doc_sendto		(GODoc *doc, GOIOContext *cc);
#endif

G_END_DECLS

#endif /* GO_DOC_H */
