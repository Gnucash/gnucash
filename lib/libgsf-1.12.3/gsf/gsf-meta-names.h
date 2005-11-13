/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-meta-names.h: a list of gsf-meta-names to "generically" represent 
 *                   all diviserly available implementation-specific
 *                   meta-names.
 *
 * Author:  Veerapuram Varadhan (vvaradhan@novell.com)
 *
 * Copyright (C) 2004 Novell, Inc
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */
#ifndef GSF_META_NAMES_H
#define GSF_META_NAMES_H

/* (String) A formal name given to the resource */
#define GSF_META_NAME_TITLE                 "dc:title"

/* (String) An account of the content of the resource */
#define GSF_META_NAME_DESCRIPTION           "dc:description"

/* (String) The topic of the content of the resource, *typically* including keywords */
#define GSF_META_NAME_SUBJECT               "dc:subject"

/* (Date as ISO String) The last time this document was saved */
#define GSF_META_NAME_DATE_MODIFIED         "dc:date-modified"

/* (Date as ISO String) A date associated with an event in the life cycle of the resource (creation/publication date) */
#define GSF_META_NAME_DATE_CREATED          "gsf:date-created"

/* (String) Searchable, indexable keywords. Similar to PDF keywords or HTML's meta block */
#define GSF_META_NAME_KEYWORDS              "dc:keywords"

/* (String) A language of the intellectual content of the resource (basically xx_YY form for us) */
#define GSF_META_NAME_LANGUAGE              "dc:language"

/* (Integer) Count of revision on the document, if appropriate */
#define GSF_META_NAME_REVISION_COUNT        "gsf:revision-count"

/* (Date as ISO String) The total-time taken till last-modified */
#define GSF_META_NAME_EDITING_DURATION      "gsf:editing-duration"

/* (Integer) Count of tables in the document, if appropriate */
#define GSF_META_NAME_TABLE_COUNT           "gsf:table-count"

/* (Integer) Count of images in the document, if appropriate */
#define GSF_META_NAME_IMAGE_COUNT           "gsf:image-count"

/* (Integer) Count of objects (OLE and other graphics) in the document, if appropriate */
#define GSF_META_NAME_OBJECT_COUNT          "gsf:object-count"

/* (Integer) Count of pages in the document, if appropriate */
#define GSF_META_NAME_PAGE_COUNT            "gsf:page-count"

/* (Integer) Count of paragraphs in the document, if appropriate */
#define GSF_META_NAME_PARAGRAPH_COUNT       "gsf:paragraph-count"

/* (Integer) Count of words in the document */
#define GSF_META_NAME_WORD_COUNT            "gsf:word-count"

/* (Integer) Count of characters in the document */
#define GSF_META_NAME_CHARACTER_COUNT       "gsf:character-count"

/* (Integer) Count of cells in the spread-sheet document, if appropriate */
#define GSF_META_NAME_CELL_COUNT            "gsf:cell-count"

/* (Integer) Count of pages in the document, if appropriate */
#define GSF_META_NAME_SPREADSHEET_COUNT     "gsf:spreadsheet-count"

/* (String) An entity primarily responsible for making the content of the resource
 * typically a person, organization, or service
 */
#define GSF_META_NAME_CREATOR               "gsf:creator"

/* (String) The template file that is been used to generate this document */
#define GSF_META_NAME_TEMPLATE              "gsf:template"

/* (String) The entity that made the last change to the document, 
 * typically a person, organization, or service
 */
#define GSF_META_NAME_LAST_SAVED_BY         "gsf:last-saved-by"

/* (Date as ISO String) The last time this document was printed */
#define GSF_META_NAME_LAST_PRINTED          "gsf:last-printed"

/* (Integer) Level of security.
 * Level                         Value
 * -----                         -----
 * None                            0
 * Password protected              1
 * Read-only recommended           2
 * Read-only enforced              3
 * Locked for annotations          4
 *
 */
#define GSF_META_NAME_SECURITY              "gsf:security"

/* (String) Category of the document (example???) */
#define GSF_META_NAME_CATEGORY              "gsf:category"

/* (String) Type of presentation, like "On-screen Show", "SlideView" etc */
#define GSF_META_NAME_PRESENTATION_FORMAT   "gsf:presentation-format"

/* (Clipboard Format (VT_CF)) Thumbnail data of the document, typically
 * a preview image of the document
 */
#define GSF_META_NAME_THUMBNAIL             "gsf:thumbnail"

/* (String) The creator (product) of this document. AbiWord, Gnumeric, etc...  */
#define GSF_META_NAME_GENERATOR             "gsf:generator"

/* (Integer) Count of liness in the document */
#define GSF_META_NAME_LINE_COUNT            "gsf:line-count"

/* (Integer) Count of slides in the presentation document */
#define GSF_META_NAME_SLIDE_COUNT           "gsf:slide-count"

/* (Integer) Count of "notes" in the document */
#define GSF_META_NAME_NOTE_COUNT            "gsf:note-count"

/* (Integer) Count of hidden-slides in the presentation document */
#define GSF_META_NAME_HIDDEN_SLIDE_COUNT    "gsf:hidden-slide-count"

/* (Integer) Count of "multi-media" clips in the document */
#define GSF_META_NAME_MM_CLIP_COUNT         "gsf:MM-clip-count"

/* (Integer) Count of bytes in the document */
#define GSF_META_NAME_BYTE_COUNT            "gsf:byte-count"

/* (Boolean) ????? */
#define GSF_META_NAME_SCALE                 "gsf:scale"

/* (VT_VECTOR|VT_VARIANT) ??????? */
#define GSF_META_NAME_HEADING_PAIRS         "gsf:heading-pairs"

/* (VT_VECTOR|VT_LPSTR) ??????? */
#define GSF_META_NAME_DOCUMENT_PARTS        "gsf:document-parts"

/* (String) Name of the manager of "CREATOR" entity */
#define GSF_META_NAME_MANAGER               "gsf:manager"

/* (String) Name of the company/organization that
 * the "CREATOR" entity is associated with.
 */
#define GSF_META_NAME_COMPANY               "gsf:company"

/* (Boolean) ??????? */
#define GSF_META_NAME_LINKS_DIRTY           "gsf:links-dirty"

/* (Unknown) User-defined names */
#define GSF_META_NAME_MSOLE_UNKNOWN_17		"msole:unknown-doc-17"
#define GSF_META_NAME_MSOLE_UNKNOWN_18		"msole:unknown-doc-18"
#define GSF_META_NAME_MSOLE_UNKNOWN_19		"msole:unknown-doc-19"	/* bool */
#define GSF_META_NAME_MSOLE_UNKNOWN_20		"msole:unknown-doc-20"
#define GSF_META_NAME_MSOLE_UNKNOWN_21		"msole:unknown-doc-21"
#define GSF_META_NAME_MSOLE_UNKNOWN_22		"msole:unknown-doc-22"	/* bool */
#define GSF_META_NAME_MSOLE_UNKNOWN_23		"msole:unknown-doc-23"	/* i4 */

/* (None) Reserved name (PID) for Dictionary */
#define GSF_META_NAME_DICTIONARY            "gsf:dictionary"

/* (Unsigned Integer) Identifier representing the default 
 * system locale.
 */
#define GSF_META_NAME_LOCALE_SYSTEM_DEFAULT	"gsf:default-locale"

/* (Unsigned Integer) Identifier representing the case-sensitiveness */
#define GSF_META_NAME_CASE_SENSITIVE        "gsf:case-sensitivity"

#endif /* GSF_META_NAMES_H */
