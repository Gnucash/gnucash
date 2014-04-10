/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-msole-utils.c: 
 *
 * Copyright (C) 2002-2005 Jody Goldberg (jody@gnome.org)
 * Copyright (C) 2002-2005 Dom Lachowicz (cinamod@hotmail.com)
 * excel_iconv* family of functions (C) 2001 by Vlad Harchev <hvv@hippo.ru>
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

#include <gsf-config.h>
#include <gsf/gsf-docprop-vector.h>
#include <gsf/gsf-msole-utils.h>
#include <gsf/gsf-input.h>
#include <gsf/gsf-output.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-timestamp.h>
#include <gsf/gsf-meta-names.h>
#include <gsf/gsf-doc-meta-data.h>
#include <gsf/gsf-clip-data.h>

#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <glib/gi18n-lib.h>

#define NO_DEBUG_OLE_PROPS
#ifndef NO_DEBUG_OLE_PROPS
#define d(code)	do { code } while (0)
#else
#define d(code)
#endif

/*
 * The Format Identifier for Summary Information
 * F29F85E0-4FF9-1068-AB91-08002B27B3D9
 */
static guint8 const component_guid [] = {
	0xe0, 0x85, 0x9f, 0xf2, 0xf9, 0x4f, 0x68, 0x10,
	0xab, 0x91, 0x08, 0x00, 0x2b, 0x27, 0xb3, 0xd9
};

/*
 * The Format Identifier for Document Summary Information
 * D5CDD502-2E9C-101B-9397-08002B2CF9AE
 */
static guint8 const document_guid [] = {
	0x02, 0xd5, 0xcd, 0xd5, 0x9c, 0x2e, 0x1b, 0x10,
	0x93, 0x97, 0x08, 0x00, 0x2b, 0x2c, 0xf9, 0xae
};

/*
 * The Format Identifier for User-Defined Properties
 * D5CDD505-2E9C-101B-9397-08002B2CF9AE
 */
static guint8 const user_guid [] = {
	0x05, 0xd5, 0xcd, 0xd5, 0x9c, 0x2e, 0x1b, 0x10,
	0x93, 0x97, 0x08, 0x00, 0x2b, 0x2c, 0xf9, 0xae
};

typedef enum {
	COMMON_PROP,	/* in either summary or docsummary */
	COMPONENT_PROP, /* SummaryInformation properties */
	DOC_PROP,	/* DocumentSummaryInformation properties */
	USER_PROP
} GsfMSOleMetaDataType;

typedef enum {
	VT_EMPTY	   = 0,
	VT_NULL		   = 1,
	VT_I2		   = 2,
	VT_I4		   = 3,
	VT_R4		   = 4,
	VT_R8		   = 5,
	VT_CY		   = 6,
	VT_DATE		   = 7,
	VT_BSTR		   = 8,
	VT_DISPATCH	   = 9,
	VT_ERROR	   = 10,
	VT_BOOL		   = 11,
	VT_VARIANT	   = 12,
	VT_UNKNOWN	   = 13,
	VT_DECIMAL	   = 14,

	VT_I1		   = 16,
	VT_UI1		   = 17,
	VT_UI2		   = 18,
	VT_UI4		   = 19,
	VT_I8		   = 20,
	VT_UI8		   = 21,
	VT_INT		   = 22,
	VT_UINT		   = 23,
	VT_VOID		   = 24,
	VT_HRESULT	   = 25,
	VT_PTR		   = 26,
	VT_SAFEARRAY	   = 27,
	VT_CARRAY	   = 28,
	VT_USERDEFINED	   = 29,
	VT_LPSTR	   = 30,
	VT_LPWSTR	   = 31,

	VT_FILETIME	   = 64,
	VT_BLOB		   = 65,
	VT_STREAM	   = 66,
	VT_STORAGE	   = 67,
	VT_STREAMED_OBJECT = 68,
	VT_STORED_OBJECT   = 69,
	VT_BLOB_OBJECT	   = 70,
	VT_CF		   = 71,
	VT_CLSID	   = 72,
	VT_VECTOR	   = 0x1000
} GsfMSOleVariantType;

typedef struct {
	char const		*ms_name;
	GsfMSOleMetaDataType	 section;
	char const		*gsf_name;
	guint32			 id;
	GsfMSOleVariantType	 prefered_type;
} GsfMSOleMetaDataPropMap;

typedef struct {
	guint32		id;
	gsf_off_t	offset;
} GsfMSOleMetaDataProp;

typedef struct {
	GsfMSOleMetaDataType type;
	gsf_off_t   offset;
	guint32	    size, num_props;
	GIConv	    iconv_handle;
	unsigned    char_size;
	GHashTable *dict;
} GsfMSOleMetaDataSection;

static GsfMSOleMetaDataPropMap const builtin_props [] = {
	{ "Dictionary",		  COMMON_PROP,	GSF_META_NAME_DICTIONARY,            0,	   0, /* magic */},
	{ "CodePage",		  COMMON_PROP,	GSF_META_NAME_LANGUAGE,              1,	   VT_I2 },
	{ "LOCALE_SYSTEM_DEFAULT",COMMON_PROP,	GSF_META_NAME_LOCALE_SYSTEM_DEFAULT, 0x80000000, VT_UI4},
	{ "CASE_SENSITIVE",	  COMMON_PROP,	GSF_META_NAME_CASE_SENSITIVE,        0x80000003, VT_UI4},
	{ "Category",		DOC_PROP,	GSF_META_NAME_CATEGORY,             2,	VT_LPSTR },
	{ "PresentationFormat",	DOC_PROP,	GSF_META_NAME_PRESENTATION_FORMAT,  3,	VT_LPSTR },
	{ "NumBytes",		DOC_PROP,	GSF_META_NAME_BYTE_COUNT,           4,	VT_I4 },
	{ "NumLines",		DOC_PROP,	GSF_META_NAME_LINE_COUNT,           5,	VT_I4 },
	{ "NumParagraphs",	DOC_PROP,	GSF_META_NAME_PARAGRAPH_COUNT,      6,	VT_I4 },
	{ "NumSlides",		DOC_PROP,	GSF_META_NAME_SLIDE_COUNT,          7,	VT_I4 },
	{ "NumNotes",		DOC_PROP,	GSF_META_NAME_NOTE_COUNT,           8,	VT_I4 },
	{ "NumHiddenSlides",	DOC_PROP,	GSF_META_NAME_HIDDEN_SLIDE_COUNT,   9,	VT_I4 },
	{ "NumMMClips",		DOC_PROP,	GSF_META_NAME_MM_CLIP_COUNT,       10,	VT_I4 },
	{ "Scale",		DOC_PROP,	GSF_META_NAME_SCALE,               11,	VT_BOOL },
	{ "HeadingPairs",	DOC_PROP,	GSF_META_NAME_HEADING_PAIRS,       12,	VT_VECTOR | VT_VARIANT },
	{ "DocumentParts",	DOC_PROP,	GSF_META_NAME_DOCUMENT_PARTS,      13,	VT_VECTOR | VT_LPSTR },
	{ "Manager",		DOC_PROP,	GSF_META_NAME_MANAGER,             14,	VT_LPSTR },
	{ "Company",		DOC_PROP,	GSF_META_NAME_COMPANY,             15,	VT_LPSTR },
	{ "LinksDirty",		DOC_PROP,	GSF_META_NAME_LINKS_DIRTY,         16,	VT_BOOL },
	{ "DocSumInfo_17",      DOC_PROP,	GSF_META_NAME_MSOLE_UNKNOWN_17,    17,	VT_UNKNOWN },
	{ "DocSumInfo_18",      DOC_PROP,	GSF_META_NAME_MSOLE_UNKNOWN_18,    18,	VT_UNKNOWN },
	{ "DocSumInfo_19",      DOC_PROP,	GSF_META_NAME_MSOLE_UNKNOWN_19,    19,	VT_BOOL },
	{ "DocSumInfo_20",      DOC_PROP,	GSF_META_NAME_MSOLE_UNKNOWN_20,    20,	VT_UNKNOWN },
	{ "DocSumInfo_21",      DOC_PROP,	GSF_META_NAME_MSOLE_UNKNOWN_21,    21,	VT_UNKNOWN },
	{ "DocSumInfo_22",      DOC_PROP,	GSF_META_NAME_MSOLE_UNKNOWN_22,    22,	VT_BOOL },
	{ "DocSumInfo_23",      DOC_PROP,	GSF_META_NAME_MSOLE_UNKNOWN_23,    23,	VT_I4 },
	{ "Title",		COMPONENT_PROP, GSF_META_NAME_TITLE,		    2,	VT_LPSTR },
	{ "Subject",		COMPONENT_PROP, GSF_META_NAME_SUBJECT,		    3,	VT_LPSTR },
	{ "Author",		COMPONENT_PROP, GSF_META_NAME_CREATOR,		    4,	VT_LPSTR },
	{ "Keywords",		COMPONENT_PROP, GSF_META_NAME_KEYWORDS,		    5,	VT_LPSTR },
	{ "Comments",		COMPONENT_PROP, GSF_META_NAME_DESCRIPTION,	    6,	VT_LPSTR },
	{ "Template",		COMPONENT_PROP, GSF_META_NAME_TEMPLATE,		    7,	VT_LPSTR },
	{ "LastSavedBy",	COMPONENT_PROP, GSF_META_NAME_LAST_SAVED_BY,	    8,	VT_LPSTR },
	{ "RevisionNumber",	COMPONENT_PROP, GSF_META_NAME_REVISION_COUNT,	    9,	VT_LPSTR },
	{ "TotalEditingTime",	COMPONENT_PROP, GSF_META_NAME_EDITING_DURATION,	   10,	VT_FILETIME },
	{ "LastPrinted",	COMPONENT_PROP, GSF_META_NAME_LAST_PRINTED,	   11,	VT_FILETIME },
	{ "CreateTime",		COMPONENT_PROP, GSF_META_NAME_DATE_CREATED,	   12,	VT_FILETIME },
	{ "LastSavedTime",	COMPONENT_PROP, GSF_META_NAME_DATE_MODIFIED,	   13,	VT_FILETIME },
	{ "NumPages",		COMPONENT_PROP, GSF_META_NAME_PAGE_COUNT,	   14,	VT_I4 },
	{ "NumWords",		COMPONENT_PROP, GSF_META_NAME_WORD_COUNT,	   15,	VT_I4 },
	{ "NumCharacters",	COMPONENT_PROP, GSF_META_NAME_CHARACTER_COUNT,	   16,	VT_I4 },
	{ "Thumbnail",		COMPONENT_PROP, GSF_META_NAME_THUMBNAIL,	   17,	VT_CF },
	{ "AppName",		COMPONENT_PROP, GSF_META_NAME_GENERATOR,	   18,	VT_LPSTR },
	{ "Security",		COMPONENT_PROP, GSF_META_NAME_SECURITY,		   19,	VT_I4 }
};

static GHashTable *name_to_prop_hash = NULL;

static char const *
msole_vt_name (GsfMSOleVariantType type)
{
	static char const *names[] = {
		"VT_EMPTY",	"VT_NULL",	"VT_I2",	"VT_I4",	"VT_R4",
		"VT_R8",	"VT_CY",	"VT_DATE",	"VT_BSTR",	"VT_DISPATCH",
		"VT_ERROR",	"VT_BOOL",	"VT_VARIANT",	"VT_UNKNOWN",	"VT_DECIMAL",
		NULL,		"VT_I1",	"VT_UI1",	"VT_UI2",	"VT_UI4",
		"VT_I8",	"VT_UI8",	"VT_INT",	"VT_UINT",	"VT_VOID",
		"VT_HRESULT",	"VT_PTR",	"VT_SAFEARRAY",	"VT_CARRAY",	"VT_USERDEFINED",
		"VT_LPSTR",	"VT_LPWSTR",
	};	
	static char const *names2[] = {
		"VT_FILETIME",
		"VT_BLOB",	"VT_STREAM",	"VT_STORAGE",	"VT_STREAMED_OBJECT",
		"VT_STORED_OBJECT", "VT_BLOB_OBJECT", "VT_CF",	"VT_CLSID"
	};

	type &= ~VT_VECTOR;
	if (type <= VT_LPWSTR)
		return names[type];
	g_return_val_if_fail (type >= VT_FILETIME, "_UNKNOWN_");
	g_return_val_if_fail (type <= VT_CLSID, "_UNKNOWN_");
	return names2[type-VT_FILETIME];
}

static char const *
msole_prop_id_to_gsf (GsfMSOleMetaDataSection *section, guint32 id, gboolean *linked)
{
	char const *res = NULL;
	GsfMSOleMetaDataPropMap const *map = NULL;
	unsigned i = 0;

	*linked = FALSE;
	if (section->dict != NULL) {
		if (id & 0x1000000) {
			*linked = TRUE;
			id &= ~0x1000000;
			d (g_print ("LINKED "););
		}

		res = g_hash_table_lookup (section->dict, GINT_TO_POINTER (id));

		if (res != NULL) {
			d (g_print (res););
			return res;
		}
	}

	map = builtin_props ;
	i = G_N_ELEMENTS (builtin_props);
	while (i-- > 0)
		if (map[i].id == id &&
		    (map[i].section == COMMON_PROP || map[i].section == section->type)) {
			d (g_print (map[i].gsf_name););
			return map[i].gsf_name;
		}

	d (g_print ("_UNKNOWN_(0x%x %d)", id, id););

	return NULL;
}

static GsfMSOleMetaDataPropMap const *
msole_gsf_name_to_prop (char const *name)
{
	if (NULL == name_to_prop_hash) {
		int i;
		name_to_prop_hash = g_hash_table_new (g_str_hash, g_str_equal);
		for (i = G_N_ELEMENTS (builtin_props); i-- > 0; )
			g_hash_table_replace (name_to_prop_hash,
				(gpointer) builtin_props[i].gsf_name,
				(gpointer) (builtin_props+i));
	}

	return g_hash_table_lookup (name_to_prop_hash, (gpointer)name);
}

static void
set_error_missing_data (GError **error, const char *property_name, gsize size_needed, gsize size_gotten)
{
	g_set_error (error,
		     GSF_ERROR,
		     GSF_ERROR_INVALID_DATA,
		     _("Missing data when reading the %s property; got %" G_GSIZE_FORMAT "bytes, "
		       "but %" G_GSIZE_FORMAT " bytes at least are needed."),
		     property_name,
		     size_needed,
		     size_gotten);
}

/* Can return errors from gsf_blob_new() and GSF_ERROR_INVALID_DATA */
static gboolean
parse_vt_cf (GValue *res, guint8 const **data, guint8 const *data_end, GError **error)
{
	/* clipboard size		uint32		sizeof (clipboard format tag) + sizeof (clipboard data)
	 * clipboard format tag		int32		see below
	 * clipboard data		byte[]		see below
	 *
	 * Clipboard format tag:
	 * -1 - Windows clipboard format
	 * -2 - Macintosh clipboard format
	 * -3 - GUID that contains a format identifier (FMTID)
	 * >0 - custom clipboard format name plus data (see msdn site below)
	 *  0 - No data
	 *
	 * References:
	 * http://msdn.microsoft.com/library/default.asp?url=/library/en-us/stg/stg/propvariant.asp
	 * http://jakarta.apache.org/poi/hpsf/thumbnails.html
	 * http://linux.com.hk/docs/poi/org/apache/poi/hpsf/Thumbnail.html
	 * http://sparks.discreet.com/knowledgebase/public/solutions/ExtractThumbnailImg.htm
	 */
	guint32 clip_size, clip_data_size;
	gint32 clip_format;
	GsfBlob *blob;
	GsfClipData *clip_data;

	/* Clipboard size field */

	if (data_end < *data + 4) {
		set_error_missing_data (error, "VT_CF", 4, data_end - *data);
		return FALSE;
	}

	clip_size = GSF_LE_GET_GUINT32 (*data);

	if (clip_size < 4) {	/* must emcompass int32 format plus data size */
		g_set_error (error,
			     GSF_ERROR,
			     GSF_ERROR_INVALID_DATA,
			     _("Corrupt data in the VT_CF property; clipboard data length must be at least 4 bytes, "
			       "but the data says it only has %" G_GSIZE_FORMAT " bytes available."),
			     (gsize) clip_size);
		return FALSE;
	}

	*data += 4;

	/* Check clipboard format plus data size */

	if (data_end < *data + clip_size) {
		set_error_missing_data (error, "VT_CF", clip_size, data_end - *data);
		return FALSE;
	}

	clip_format = GSF_LE_GET_GINT32 (*data);
	*data += 4;

	switch (clip_format) {
	case GSF_CLIP_FORMAT_WINDOWS_CLIPBOARD:
	case GSF_CLIP_FORMAT_MACINTOSH_CLIPBOARD:
	case GSF_CLIP_FORMAT_GUID:
	case GSF_CLIP_FORMAT_NO_DATA:
		/* everything is ok */
		break;

	default:
		if (clip_format > 0)
			clip_format = GSF_CLIP_FORMAT_CLIPBOARD_FORMAT_NAME;
		else
			clip_format = GSF_CLIP_FORMAT_UNKNOWN;

		break;
	}

	clip_data_size = clip_size - 4;

	blob = gsf_blob_new (clip_data_size, *data, error);

	*data += clip_data_size;

	if (!blob)
		return FALSE;

	clip_data = gsf_clip_data_new (clip_format, blob);
	g_object_unref (blob);

	g_value_init (res, GSF_TYPE_CLIP_DATA);
	g_value_set_object (res, clip_data);
	g_object_unref (clip_data);

	return TRUE;
}

static GValue *
msole_prop_parse (GsfMSOleMetaDataSection *section,
		  guint32 type, guint8 const **data, guint8 const *data_end)
{
	GValue *res;
	char *str;
	guint32 len;
	gboolean const is_vector = type & VT_VECTOR;
	GError *error;

	g_return_val_if_fail (!(type & (unsigned)(~0x1fff)), NULL); /* not valid in a prop set */

	type &= 0xfff;

	if (is_vector) {
		/*
		 *  A vector is basically an array.  If the type associated with
		 *  it is a variant, then each element can have a different
		 *  variant type.  Otherwise, each element has the same variant
		 *  type associated with the vector.
		 */
		unsigned i, n;
		GsfDocPropVector *vector;

		g_return_val_if_fail (*data + 4 <= data_end, NULL);

		n = GSF_LE_GET_GUINT32 (*data);
		*data += 4;

		d (g_print (" array with %d elem\n", n);
		   gsf_mem_dump (*data, (unsigned)(data_end - *data)););
		
		vector = gsf_docprop_vector_new ();

		for (i = 0 ; i < n ; i++) {
			GValue *v;
			d (g_print ("\t[%d] ", i););
			v = msole_prop_parse (section, type, data, data_end);
			if (v) {
				if (G_IS_VALUE (v)) {
					gsf_docprop_vector_append (vector, v);
					g_value_unset (v);
				}
				g_free (v);
			}
		}

		res = g_new0 (GValue, 1);
		g_value_init (res, GSF_DOCPROP_VECTOR_TYPE);
		g_value_set_object (res, vector);
		g_object_unref (vector);
		return res;
	}

	res = g_new0 (GValue, 1);
	d (g_print ("%s\n", msole_vt_name (type)););
	switch (type) {
	case VT_EMPTY :
		/*
		 * A property with a type indicator of VT_EMPTY has no data
		 * associated with it; that is, the size of the value is zero.
		 */
		/* value::unset == empty */
		break;

	case VT_NULL :
		/* This is like a pointer to NULL */
		/* value::unset == null too :-) do we need to distinguish ? */
		break;

	case VT_I2 :
		/* 2-byte signed integer */
		g_return_val_if_fail (*data + 2 <= data_end, NULL);
		g_value_init (res, G_TYPE_INT);
		g_value_set_int	(res, GSF_LE_GET_GINT16 (*data));
		*data += 2;
		break;

	case VT_I4 :
		/* 4-byte signed integer */
		g_return_val_if_fail (*data + 4 <= data_end, NULL);
		g_value_init (res, G_TYPE_INT);
		g_value_set_int	(res, GSF_LE_GET_GINT32 (*data));
		*data += 4;
		break;

	case VT_R4 :
		/* 32-bit IEEE floating-point value */
		g_return_val_if_fail (*data + 4 <= data_end, NULL);
		g_value_init (res, G_TYPE_FLOAT);
		g_value_set_float (res, GSF_LE_GET_FLOAT (*data));
		*data += 4;
		break;

	case VT_R8 :
		/* 64-bit IEEE floating-point value */
		g_return_val_if_fail (*data + 8 <= data_end, NULL);
		g_value_init (res, G_TYPE_DOUBLE);
		g_value_set_double (res, GSF_LE_GET_DOUBLE (*data));
		*data += 8;
		break;

	case VT_CY :
		/* 8-byte two's complement integer (scaled by 10,000) */
		/* CHEAT : just store as an int64 for now */
		g_return_val_if_fail (*data + 8 <= data_end, NULL);
		g_value_init (res, G_TYPE_INT64);
		g_value_set_int64 (res, GSF_LE_GET_GINT64 (*data));
		break;

	case VT_DATE :
		/* 
		 * 64-bit floating-point number representing the number of days
		 * (not seconds) since December 31, 1899.
		 */
/* FIXME FIXME FIXME  TODO */
		break;

	case VT_BSTR :
		/*
		 * Pointer to null-terminated Unicode string; the string is pre-
		 * ceeded by a DWORD representing the byte count of the number
		 * of bytes in the string (including the  terminating null).
		 */
/* FIXME FIXME FIXME  TODO */
		break;

	case VT_DISPATCH :
/* FIXME FIXME FIXME  TODO */
		break;

	case VT_BOOL :
		/* A boolean (WORD) value containg 0 (false) or -1 (true). */
		g_return_val_if_fail (*data + 1 <= data_end, NULL);
		g_value_init (res, G_TYPE_BOOLEAN);
		g_value_set_boolean (res, **data ? TRUE : FALSE);
		*data += 1;
		break;

	case VT_VARIANT :	 d (g_print ("\tcontaining a "););
		/*
		 * A type indicator (a DWORD) followed by the corresponding
		 *  value.  VT_VARIANT is only used in conjunction with
		 *  VT_VECTOR.
		 */
		g_free (res);
		type = GSF_LE_GET_GUINT32 (*data);
		*data += 4;
		return msole_prop_parse (section, type, data, data_end);

	case VT_UI1 :
		/* 1-byte unsigned integer */
		g_return_val_if_fail (*data + 1 <= data_end, NULL);
		g_value_init (res, G_TYPE_UCHAR);
		g_value_set_uchar (res, (guchar)(**data));
		*data += 1;
		break;

	case VT_UI2 :
		/* 2-byte unsigned integer */
		g_return_val_if_fail (*data + 2 <= data_end, NULL);
		g_value_init (res, G_TYPE_UINT);
		g_value_set_uint (res, GSF_LE_GET_GUINT16 (*data));
		*data += 2;
		break;

	case VT_UI4 :
		/* 4-type unsigned integer */
		g_return_val_if_fail (*data + 4 <= data_end, NULL);
		g_value_init (res, G_TYPE_UINT);
		g_value_set_uint (res, GSF_LE_GET_GUINT32 (*data));
		*data += 4;
		break;

	case VT_I8 :		 d (g_print ("VT_I8\n"););
		/* 8-byte signed integer */
		g_return_val_if_fail (*data + 8 <= data_end, NULL);
		g_value_init (res, G_TYPE_INT64);
		g_value_set_int64 (res, GSF_LE_GET_GINT64 (*data));
		*data += 8;
		break;

	case VT_UI8 :
		/* 8-byte unsigned integer */
		g_return_val_if_fail (*data + 8 <= data_end, NULL);
		g_value_init (res, G_TYPE_UINT64);
		g_value_set_uint64 (res, GSF_LE_GET_GUINT64 (*data));
		*data += 8;
		break;

	case VT_LPSTR :
		/* 
		 * This is the representation of many strings.  It is stored in
		 * the same representation as VT_BSTR.  Note that the serialized
		 * representation of VP_LPSTR has a preceding byte count, whereas
		 * the in-memory representation does not.
		 */
		/* be anal and safe */
		g_return_val_if_fail (*data + 4 <= data_end, NULL);

		len = GSF_LE_GET_GUINT32 (*data);

		g_return_val_if_fail (len < 0x10000, NULL);
		g_return_val_if_fail (*data + 4 + len*section->char_size <= data_end, NULL);

		error = NULL;
		d (gsf_mem_dump (*data + 4, len * section->char_size););
		str = g_convert_with_iconv (*data + 4,
			len * section->char_size,
			section->iconv_handle, NULL, NULL, &error);

		g_value_init (res, G_TYPE_STRING);
		if (NULL != str) {
			g_value_set_string (res, str);
			g_free (str);
		} else if (NULL != error) {
			g_warning ("error: %s", error->message);
			g_error_free (error);
		} else {
			g_warning ("unknown error converting string property, using blank");
		}
		*data += 4 + len * section->char_size;
		break;

	case VT_LPWSTR :
		/*
		 * A counted and null-terminated Unicode string; a DWORD character
		 * count (where the count includes the terminating null) followed
		 * by that many Unicode (16-bit) characters.  Note that the count
		 * is character count, not byte count.
		 */
		/* be anal and safe */
		g_return_val_if_fail (*data + 4 <= data_end, NULL);

		len = GSF_LE_GET_GUINT32 (*data);

		g_return_val_if_fail (len < 0x10000, NULL);
		g_return_val_if_fail (*data + 4 + len <= data_end, NULL);

		error = NULL;
		d (gsf_mem_dump (*data + 4, len*2););
		str = g_convert (*data + 4, len*2,
				 "UTF-8", "UTF-16LE", NULL, NULL, &error);

		g_value_init (res, G_TYPE_STRING);
		if (NULL != str) {
			g_value_set_string (res, str);
			g_free (str);
		} else if (NULL != error) {
			g_warning ("error: %s", error->message);
			g_error_free (error);
		} else {
			g_warning ("unknown error converting string property, using blank");
		}
		*data += 4 + len*2;
		break;

	case VT_FILETIME :
		/* 64-bit FILETIME structure, as defined by Win32. */
		g_return_val_if_fail (*data + 8 <= data_end, NULL);
	{
		/* ft * 100ns since Jan 1 1601 */
		guint64 ft = GSF_LE_GET_GUINT64 (*data);
		GsfTimestamp ts;

		ft /= 10000000; /* convert to seconds */
		ft -= G_GINT64_CONSTANT (11644473600); /* move to Jan 1 1970 */
		ts.timet = (time_t)ft;
		g_value_init (res, GSF_TIMESTAMP_TYPE);
		gsf_value_set_timestamp (res, &ts);
		*data += 8;
		break;
	}
	case VT_BLOB :
		/*
		 * A DWORD count of bytes, followed by that many bytes of data.
		 * The byte count does not include the four bytes for the length
		 * of the count itself:  An empty blob would have a count of
		 * zero, followed by zero bytes.  Thus the serialized represen-
		 * tation of a VT_BLOB is similar to that of a VT_BSTR but does
		 * not guarantee a null byte at the end of the data.
		 */
/* FIXME FIXME FIXME  TODO */
		g_free (res);
		res = NULL;
		break;

	case VT_STREAM :
		/*
		 * Indicates the value is stored in a stream that is sibling to
		 * the CONTENTS stream.  Following this type indicator is data
		 * in the format of a serialized VT_LPSTR, which names the stream
		 * containing the data.
		 */
/* FIXME FIXME FIXME  TODO */
		g_free (res);
		res = NULL;
		break;

	case VT_STORAGE :
		/*
		 * Indicates the value is stored in an IStorage that is sibling
		 * to the CONTENTS stream.  Following this type indicator is data
		 * in the format of a serialized VT_LPSTR, which names the
		 * IStorage containing the data.
		 */
/* FIXME FIXME FIXME  TODO */
		g_free (res);
		res = NULL;
		break;

	case VT_STREAMED_OBJECT:
		/*
		 * Same as VT_STREAM, but indicates that the stream contains a
		 * serialized object, which is a class ID followed by initiali-
		 * zation data for the class.
		 */
/* FIXME FIXME FIXME  TODO */
		g_free (res);
		res = NULL;
		break;

	case VT_STORED_OBJECT :
		/*
		 * Same as VT_STORAGE, but indicates that the designated IStorage
		 * contains a loadable object.
		 */
/* FIXME FIXME FIXME  TODO */
		g_free (res);
		res = NULL;
		break;

	case VT_BLOB_OBJECT :
		/*
		 * Contains a serialized object in the same representation as
		 * would appear in a VT_STREAMED_OBJECT.  That is, following the
		 * VT_BLOB_OBJECT tag is a DWORD byte count of the remaining data
		 * (where the byte count does not include the size of itself)
		 * which is in the format of a class ID followed by initialization
		 * data for that class
		 */
/* FIXME FIXME FIXME  TODO */

		g_free (res);
		res = NULL;
		break;

	case VT_CF :
		error = NULL;
		if (!parse_vt_cf (res, data, data_end, &error)) {
			/* suck, we can't propagate the error upwards */
			g_warning ("error: %s", error->message);
			g_error_free (error);
			g_free (res);
			res = NULL;
		}
		break;

	case VT_CLSID :
		/* A class ID (or other GUID) */
		*data += 16;
		g_free (res);
		res = NULL;
		break;

	case VT_ERROR :
		/* A DWORD containing a status code. */
	case VT_UNKNOWN :
	case VT_DECIMAL :
	case VT_I1 :
		/* 1-byte signed integer */
	case VT_INT :
	case VT_UINT :
	case VT_VOID :
	case VT_HRESULT :
	case VT_PTR :
	case VT_SAFEARRAY :
	case VT_CARRAY :
	case VT_USERDEFINED :
		g_warning ("type %s (0x%x) is not permitted in property sets",
			   msole_vt_name (type), type);
		g_free (res);
		res = NULL;
		break;

	default :
		g_warning ("Unknown property type %d (0x%x)", type, type);
		g_free (res);
		res = NULL;
	}

	if (res != NULL && G_IS_VALUE (res)) {
		d ( {
			char *val = g_strdup_value_contents (res);
			g_print ("%s\n", val);
			g_free (val);
		});
	} else {
		d ({
			char const *type_name = msole_vt_name (type);
			if (type_name) {
				g_print ("A '%s' property could not be parsed\n", type_name);
			} else {
				g_print ("A %d property could not be parsed\n", type);
			}
		});
	}
	return res;
}

static gboolean
msole_prop_read (GsfInput *in,
		 GsfMSOleMetaDataSection *section,
		 GsfMSOleMetaDataProp    *props,
		 unsigned		  i,
		 GsfDocMetaData		 *accum)
{
	guint32 type;
	guint8 const *data;
	/* FIXME : why size-4 ? I must be missing something */
	gsf_off_t size = ((i+1) >= section->num_props)
		? section->size-4 : props[i+1].offset;
	char   *name;
	GValue *val;

	g_return_val_if_fail (i < section->num_props, FALSE);
	g_return_val_if_fail (size >= props[i].offset + 4, FALSE);

	size -= props[i].offset; /* includes the type id */
	if (gsf_input_seek (in, section->offset+props[i].offset, G_SEEK_SET) ||
	    NULL == (data = gsf_input_read (in, size, NULL))) {
		g_warning ("failed to read prop #%d", i);
		return FALSE;
	}

	type = GSF_LE_GET_GUINT32 (data);
	data += 4;

	/* dictionary is magic */
	if (props[i].id == 0) {
		guint32 len, id, i, n;
		gsize gslen;
		char *name;
		guint8 const *start = data;

		g_return_val_if_fail (section->dict == NULL, FALSE);

		section->dict = g_hash_table_new_full (
			g_direct_hash, g_direct_equal,
			NULL, g_free);

		d ({ g_print ("Dictionary = \n"); gsf_mem_dump (data-4, size); });
		n = type;
		for (i = 0 ; i < n ; i++) {
			id = GSF_LE_GET_GUINT32 (data);
			len = GSF_LE_GET_GUINT32 (data + 4);

			g_return_val_if_fail (len < 0x10000, FALSE);

			gslen = 0;
			name = g_convert_with_iconv (data + 8,
				len * section->char_size,
				section->iconv_handle, &gslen, NULL, NULL);
			len = (guint32)gslen;
			data += 8 + len;

			d (g_print ("\t%u == %s\n", id, name););
			g_hash_table_replace (section->dict,
				GINT_TO_POINTER (id), name);

			/* MS documentation blows goats !
			 * The docs claim there are padding bytes in the dictionary.
			 * Their examples show padding bytes.
			 * In reality non-unicode strings do not see to have padding.
			 */
			if (section->char_size != 1 && (data - start) % 4)
				data += 4 - ((data - start) % 4);
		}
	} else {
		gboolean linked;
		d (g_print ("===> %u) ", i);
		   gsf_mem_dump (data-4, size););

		name = g_strdup (msole_prop_id_to_gsf (section, props[i].id, &linked));
		d (g_print (" @ %x %x = ", (unsigned)props[i].offset, (unsigned)size););
		val = msole_prop_parse (section, type, &data, data + size);

		if (NULL != name && NULL != val) {
			if (linked) {
				GsfDocProp *prop = gsf_doc_meta_data_lookup (accum, name);
				if (NULL == prop) {
					g_warning ("linking property '%s' before it\'s value is specified",
						   (name ? name : "<null>"));
				} else if (!G_VALUE_HOLDS_STRING (val)) {
					g_warning ("linking property '%s' before it\'s value is specified",
						   (name ? name : "<null>"));
				} else
					gsf_doc_prop_set_link (prop,
						g_value_dup_string (val));
			} else {
				gsf_doc_meta_data_insert (accum, name, val);
				val = NULL;
				name = NULL;
			}
		}

		if (NULL != val) {
			if (G_IS_VALUE (val))
				g_value_unset (val);
			g_free (val);
		}
		g_free (name);
	}

	return TRUE;
}

static int
msole_prop_cmp (gconstpointer a, gconstpointer b)
{
	GsfMSOleMetaDataProp const *prop_a = a;
	GsfMSOleMetaDataProp const *prop_b = b;

	if (prop_a->offset < prop_b->offset)
		return -1;
	else if (prop_a->offset > prop_b->offset)
		return +1;
	else
		return 0;
}

/**
 * gsf_msole_metadata_read :
 * @in    : #GsfInput
 * @accum : #GsfDocMetaData
 *
 * Read a stream formated as a set of MS OLE properties from @in and store the
 * results in @accum.
 *
 * Returns GError which the caller must free on error.
 **/
GError *
gsf_msole_metadata_read	(GsfInput *in, GsfDocMetaData *accum)
{
	guint8 const *data = gsf_input_read (in, 28, NULL);
	guint16 version;
	guint32 os, num_sections;
	unsigned i, j;
	GsfMSOleMetaDataSection *sections;
	GsfMSOleMetaDataProp	*props;
	GsfDocProp		*prop;
	
	if (NULL == data)
		return g_error_new (gsf_input_error_id (), 0,
			"Unable to read MS property stream header");

	d ({g_print ("===================================\n"
		   "header class id ==\n");
	   gsf_mem_dump (data, 28);});
	/*
	 * Validate the Property Set Header.
	 * Format (bytes) :
	 *   00 - 01	Byte order		0xfffe
	 *   02 - 03	Format			0
	 *   04 - 05	OS Version		high word is the OS
	 *   06 - 07				low  word is the OS version
	 *					  0 = win16
	 *					  1 = mac
	 *					  2 = win32
	 *   08 - 23	Class Identifier	Usually Format ID
	 *   24 - 27	Section count		Should be at least 1
	 */
	os	     = GSF_LE_GET_GUINT16 (data + 6);
	version	     = GSF_LE_GET_GUINT16 (data + 2);
	num_sections = GSF_LE_GET_GUINT32 (data + 24);
	if (GSF_LE_GET_GUINT16 (data + 0) != 0xfffe
	    || (version != 0 && version != 1)
	    || os > 2
	    || num_sections > 100) /* arbitrary sanity check */
		return g_error_new (gsf_input_error_id (), 0,
			"Invalid MS property stream header");

	/* extract the section info */
	/*
	 * The Format ID/Offset list follows.
	 * Format:
	 *   00 - 16	Section Name		Format ID
	 *   16 - 19	Section Offset		The offset is the number of
	 *					bytes from the start of the
	 *					whole stream to where the
	 *					section begins.
	 */
	sections = (GsfMSOleMetaDataSection *)g_alloca (sizeof (GsfMSOleMetaDataSection)* num_sections);
	for (i = 0 ; i < num_sections ; i++) {
		data = gsf_input_read (in, 20, NULL);
		if (NULL == data)
			return g_error_new (gsf_input_error_id (), 0,
				"Unable to read MS property stream header");
		if (!memcmp (data, component_guid, sizeof (component_guid)))
			sections [i].type = COMPONENT_PROP;
		else if (!memcmp (data, document_guid, sizeof (document_guid)))
			sections [i].type = DOC_PROP;
		else if (!memcmp (data, user_guid, sizeof (user_guid)))
			sections [i].type = USER_PROP;
		else {
			sections [i].type = USER_PROP;
			g_warning ("Unknown property section type, treating it as USER");
			gsf_mem_dump (data, 16);
		}

		sections [i].offset = GSF_LE_GET_GUINT32 (data + 16);
	}

	/*
	 * A section is the third part of the property set stream.
	 * Format (bytes) :
	 *   00 - 03	Section size	A byte count for the section (which is inclusive
	 *				of the byte count itself and should always be a
	 *				multiple of 4);
	 *   04 - 07	Property count	A count of the number of properties
	 *   08 - xx   			An array of 32-bit Property ID/Offset pairs
	 *   yy - zz			An array of Property Type indicators/Value pairs
	 */
	for (i = 0 ; i < num_sections ; i++) {
		if (gsf_input_seek (in, sections[i].offset, G_SEEK_SET) ||
		    NULL == (data = gsf_input_read (in, 8, NULL)))
			return g_error_new (gsf_input_error_id (), 0,
				"Invalid MS property section");

		d (g_print ("=============================================\n"
			   "===> section #%d : type %d at offset 0x%x\n",
			   i, (int)sections [i].type,
			   (guint32)sections [i].offset););

		sections[i].iconv_handle = (GIConv)-1;
		sections[i].char_size    = 1;
		sections[i].dict      = NULL;
		sections[i].size      = GSF_LE_GET_GUINT32 (data); /* includes header */
		sections[i].num_props = GSF_LE_GET_GUINT32 (data + 4);
		if (sections[i].num_props <= 0)
			continue;

		/*
		 * Get and save all the Property ID/Offset pairs.
		 * Format (bytes) :
		 *   00 - 03	id	Property ID
		 *   04 - 07	offset	The distance from the start of the section to the
		 *			start of the Property Type/Value pair.
		 */
		d (g_print ("Offsets\n"););
		props = g_new (GsfMSOleMetaDataProp, sections[i].num_props);
		for (j = 0; j < sections[i].num_props; j++) {
			if (NULL == (data = gsf_input_read (in, 8, NULL))) {
				g_free (props);
				return g_error_new (gsf_input_error_id (), 0,
					"Invalid MS property section");
			}

			props [j].id = GSF_LE_GET_GUINT32 (data);
			props [j].offset  = GSF_LE_GET_GUINT32 (data + 4);
			d (g_print ("%d) ID=%d, offset=0x%x\n", j,
				    props [j].id, (unsigned)props [j].offset););
		}

		/* order prop info by offset to facilitate bounds checking */
		qsort (props, sections[i].num_props,
		       sizeof (GsfMSOleMetaDataProp),
		       msole_prop_cmp);

		/*
		 * Find and process the code page.
		 * Property ID 1 is reserved as an indicator of the code page.
		 */
		sections[i].iconv_handle = (GIConv)-1;
		sections[i].char_size = 1;
		for (j = 0; j < sections[i].num_props; j++) /* first codepage */
			if (props[j].id == 1) {
				msole_prop_read (in, sections+i, props, j, accum);
				if (NULL != (prop = gsf_doc_meta_data_lookup (accum, GSF_META_NAME_LANGUAGE))) {
					GValue const *val = gsf_doc_prop_get_val (prop);
					if (NULL != val && G_VALUE_HOLDS_INT (val)) {
						int codepage = g_value_get_int (val);
						sections[i].iconv_handle =
							gsf_msole_iconv_open_for_import (codepage);
						if (codepage == 1200 || codepage == 1201)
							sections[i].char_size = 2;
					}
				}
			}

		if (sections[i].iconv_handle == (GIConv)-1)
			sections[i].iconv_handle = gsf_msole_iconv_open_for_import (1252);

		/*
		 * Find and process the Property Set Dictionary
		 * Property ID 0 is reserved as an indicator of the dictionary.
		 * For User Defined Sections, Property ID 0 is NOT a dictionary.
		 */
		for (j = 0; j < sections[i].num_props; j++) /* then dictionary */
			if (props[j].id == 0)
				msole_prop_read (in, sections+i, props, j, accum);

		/* Process all the properties */
		for (j = 0; j < sections[i].num_props; j++) /* the rest */
			if (props[j].id > 1)
				msole_prop_read (in, sections+i, props, j, accum);

		gsf_iconv_close (sections[i].iconv_handle);
		g_free (props);
		if (sections[i].dict != NULL)
			g_hash_table_destroy (sections[i].dict);
	}
	return NULL;
}

/****************************************************************************/

typedef struct {
	GsfOutput  *out;
	gboolean    doc_not_component;

	GHashTable *dict;
	struct {
		unsigned count;	 /* includes 2nd prop for links */
		GSList  *props;
	} builtin, user;

	unsigned codepage;
} WritePropState;

static GsfMSOleVariantType
gvalue_to_msole_vt (GValue const *value, GsfMSOleMetaDataPropMap const *map)
{
	g_return_val_if_fail (value != NULL, VT_EMPTY);

	switch (G_TYPE_FUNDAMENTAL (G_VALUE_TYPE (value))) {
	case G_TYPE_BOOLEAN:	return VT_BOOL;
	case G_TYPE_UCHAR:	return VT_UI1;
	case G_TYPE_FLOAT:	return VT_R4;
	case G_TYPE_DOUBLE:	return VT_R8;
	case G_TYPE_STRING: 	return VT_LPSTR;
	case G_TYPE_INT:
		return (NULL != map && map->prefered_type == VT_I2)
			? VT_I2 : VT_I4;
	case G_TYPE_UINT:
		return (NULL != map && map->prefered_type == VT_UI2)
			? VT_UI2 : VT_UI4;
	case G_TYPE_BOXED:
		if (VAL_IS_GSF_TIMESTAMP (value))
			return VT_FILETIME;
		return VT_UNKNOWN;
	case G_TYPE_OBJECT:
		if (VAL_IS_GSF_DOCPROP_VECTOR (value)) {
			GValueArray *vector = gsf_value_get_docprop_varray (value);
			unsigned i, n;
			GsfMSOleVariantType type, tmp;

			if (vector == NULL)
				return VT_UNKNOWN;

			if (map != NULL) {
				type = map->prefered_type & (~VT_VECTOR);
				if (type == VT_VARIANT)
					return VT_VECTOR | VT_VARIANT;
			} else
				type = VT_UNKNOWN;
			n = vector->n_values;
			for (i = 0; i < n; i++) {
				tmp = gvalue_to_msole_vt (
					g_value_array_get_nth (vector, i), NULL);
				if (type == VT_UNKNOWN)
					type = tmp;
				else if (type != tmp)
					return VT_VECTOR | VT_VARIANT;
			}
			return VT_VECTOR | type;
		}
		break;
	}
	return VT_UNKNOWN;
}

/* Returns TRUE on success */
static gboolean
msole_metadata_write_prop (WritePropState *state,
			   char const *name,
			   GValue const *value,
			   gboolean suppress_type)
{
	static guint8 const zero[1] = { '\0' };
	GsfMSOleMetaDataPropMap const *map =
		(name != NULL) ? msole_gsf_name_to_prop (name) : NULL;
	GsfMSOleVariantType type;
	guint8 buf[8];

	g_return_val_if_fail (value != NULL, FALSE);

	type = gvalue_to_msole_vt (value, map);
	if (!suppress_type) {
		GSF_LE_SET_GUINT32 (buf, type);
		gsf_output_write (state->out, 4, buf);
	}
	if (NULL != map && map->prefered_type != type) {
		d(g_print ("Exporting property '%s' with type 0x%x rather than the usual 0x%x\n",
			   map->gsf_name, type, map->prefered_type););
	}

	if (type & VT_VECTOR) {
		GValueArray *vector = gsf_value_get_docprop_varray (value);
		unsigned i, n = vector->n_values;
		gboolean res;

		GSF_LE_SET_GINT32 (buf, n);
		res = gsf_output_write (state->out, 4, buf);
		for (i = 0; i < n; i++)
			res &= msole_metadata_write_prop (state, NULL,
				g_value_array_get_nth (vector, i),
				type != (VT_VECTOR | VT_VARIANT));
		return res;
	}

	switch (type) {
	case VT_BOOL:
		if (g_value_get_boolean (value))
			GSF_LE_SET_GINT32 (buf, 0xffffffff);
		else
			GSF_LE_SET_GINT32 (buf, 0);
		return gsf_output_write (state->out, 4, buf);
	case VT_UI1:
		GSF_LE_SET_GUINT32 (buf, g_value_get_uchar (value));
		return gsf_output_write (state->out, 4, buf);
	case VT_I2:
		GSF_LE_SET_GINT16 (buf, g_value_get_int (value));
		GSF_LE_SET_GUINT16 (buf+2, 0);
		return gsf_output_write (state->out, 4, buf);
	case VT_I4:
		GSF_LE_SET_GINT32 (buf, g_value_get_int (value));
		return gsf_output_write (state->out, 4, buf);
	case VT_UI2:
	case VT_UI4:
		GSF_LE_SET_GUINT32 (buf, g_value_get_uint (value));
		return gsf_output_write (state->out, 4, buf);
	case VT_R4:
		GSF_LE_SET_FLOAT (buf, g_value_get_float (value));
		return gsf_output_write (state->out, 4, buf);
	case VT_R8:
		GSF_LE_SET_FLOAT (buf, g_value_get_double (value));
		return gsf_output_write (state->out, 8, buf);

	case VT_LPSTR : {
/* FIXME FIXME FIXME  TODO : use iconv from codepage */
		char const *txt = g_value_get_string (value);
		unsigned len = (NULL != txt) ? strlen (txt) : 0;
		GSF_LE_SET_GUINT32 (buf, len+1);
		return  gsf_output_write (state->out, 4, buf) &&
			gsf_output_write (state->out, len, txt) &&
			gsf_output_write (state->out, 1, zero);
	}

	case VT_FILETIME : {
		GsfTimestamp const *ts = g_value_get_boxed (value);
		gint32  timet_signed = (gint32) ts->timet;
		guint64 ft;

		ft = timet_signed + G_GINT64_CONSTANT (11644473600);
		ft *= 10000000;

		GSF_LE_SET_GUINT64 (buf, ft);

		return gsf_output_write (state->out, 8, buf);
	}

	default:
		break;
	}

	g_warning ("Ignoring property '%s', how do we export a property of type '%s'",
		name ? name : "<unnamed>",
		g_type_name (G_TYPE_FUNDAMENTAL (G_VALUE_TYPE (value))));
	return FALSE;
}

static void
cb_write_dict (char const *name, gpointer id, WritePropState *state)
{
	static guint8 const zero[1] = { '\0' };
	guint8	  buf [4];
	unsigned  len = strlen (name) + 1;
	GSF_LE_SET_GUINT32 (buf, GPOINTER_TO_UINT (id));
	GSF_LE_SET_GUINT32 (buf+4, len+1);
	gsf_output_write (state->out, 8, buf);
	gsf_output_write (state->out, len, name);
	gsf_output_write (state->out, 1, zero);
}

static gboolean
msole_metadata_write_section (WritePropState *state, gboolean user)
{
	char const *name;
	guint8	  buf [8];
	GSList   *ptr   = user ? state->user.props : state->builtin.props;
	unsigned  count = user ? state->user.count : state->builtin.count;
	gsf_off_t len, base  = gsf_output_tell (state->out);
	GsfMSOleMetaDataProp *offsets;
	GsfMSOleMetaDataPropMap const *map;
	GsfDocProp const *prop;
	gpointer tmp;
	unsigned i;
	GValue	 scratch;

	if (user && state->dict == NULL)
		return TRUE;

	/* Skip past the size and id/offset pairs */
	if (!gsf_output_seek (state->out,
			     4 /* length */ +
			     4 /* count */ +
			     8 * count /* id/offset pairs */,
			     G_SEEK_END))
		return FALSE;

	memset (&scratch,  0, sizeof (GValue));
	g_value_init (&scratch, G_TYPE_STRING);

	offsets = g_alloca (sizeof (GsfMSOleMetaDataProp) * count);

	/* 0) codepage */
	offsets[0].id = 1;
	offsets[0].offset = gsf_output_tell (state->out);
	GSF_LE_SET_GUINT32 (buf, VT_I2);
	GSF_LE_SET_GUINT32 (buf+4, state->codepage);
	gsf_output_write (state->out, 8, buf);

	/* 1) dictionary */
	if (user) {
		offsets[1].id = 0;
		offsets[1].offset = gsf_output_tell (state->out);
		GSF_LE_SET_GUINT32 (buf, g_hash_table_size (state->dict));
		gsf_output_write (state->out, 4, buf);
		g_hash_table_foreach (state->dict,
			(GHFunc) cb_write_dict, state);
		i = 2;
	} else
		i = 1;

	/* 2) props */
	for (; ptr != NULL && i < count ; ptr = ptr->next, i++) {
		prop = ptr->data;
		name = gsf_doc_prop_get_name (prop);
		if (user) {
			tmp = g_hash_table_lookup (state->dict, name);
			offsets[i].id = GPOINTER_TO_INT (tmp);
			if (offsets[i].id < 2) {
				g_warning ("Invalid ID (%d) for custom name '%s'", offsets[i].id, name);
				continue;
			}
		} else {
			map = msole_gsf_name_to_prop (name);
			if (map == NULL) {
				g_warning ("Missing map for builting property '%s'", name);
				continue;
			}
			offsets[i].id = map->id;
		}

		offsets[i].offset = gsf_output_tell (state->out);
		msole_metadata_write_prop (state, name,
			gsf_doc_prop_get_val  (prop), FALSE);
		if (gsf_doc_prop_get_link (prop)) {
			i++;
			offsets[i].id     = offsets[i-1].id | 0x1000000;
			offsets[i].offset = gsf_output_tell (state->out);
			g_value_set_static_string (&scratch, 
				gsf_doc_prop_get_link (prop));
			msole_metadata_write_prop (state, NULL, &scratch, FALSE);
		}
	}

	len = gsf_output_tell (state->out) - base;
	gsf_output_seek (state->out, base, G_SEEK_SET);
	GSF_LE_SET_GUINT32 (buf, len);
	GSF_LE_SET_GUINT32 (buf+4, count);
	gsf_output_write (state->out, 8, buf);
	for (i = 0 ; i < count ; i++) {
		GSF_LE_SET_GUINT32 (buf, offsets[i].id);
		GSF_LE_SET_GUINT32 (buf+4, offsets[i].offset - base);
		gsf_output_write (state->out, 8, buf);
	}

	return gsf_output_seek (state->out, 0, G_SEEK_END);
}

static void
cb_count_props (char const *name, GsfDocProp *prop, WritePropState *state)
{
	GsfMSOleMetaDataPropMap const *map = msole_gsf_name_to_prop (name);

	/* allocate predefined ids or add it to the dictionary */
	if (map != NULL) {
		if (map->id == 0) return; /* dictionary is handled elsewhere */
		if (map->section == (state->doc_not_component ? COMPONENT_PROP : DOC_PROP))
			return;
		if (map->id == 1) { /*codepage */
			GValue const *val = gsf_doc_prop_get_val (prop);
			if (NULL != val && G_VALUE_HOLDS_INT (val))
				state->codepage = g_value_get_int (val);
			return;
		}

		d (g_print ("%d) Adding builtin %s'\n",
			    state->builtin.count, map->gsf_name););
		state->builtin.count += gsf_doc_prop_get_link (prop) ? 2 : 1;
		state->builtin.props = g_slist_prepend (state->builtin.props, prop);
	} else if (state->doc_not_component) { /* keep user props in the document */
		d (g_print("user defined named '%s' assigned id = %d\n",
			   name, state->user.count););
		if (NULL == state->dict)
			state->dict = g_hash_table_new (g_str_hash, g_str_equal);
		g_hash_table_insert (state->dict,
			(gpointer) name, GINT_TO_POINTER (state->user.count));
		state->user.count += gsf_doc_prop_get_link (prop) ? 2 : 1;
		state->user.props = g_slist_prepend (state->user.props, prop);
	}
}

/**
 * gsf_msole_metadata_write :
 * @out : #GsfOutput
 * @meta_data : #GsfDocMetaData
 * @doc_not_component : a kludge to differentiate DocumentSummary from Summary
 *
 * Returns TRUE on success;
 **/
gboolean
gsf_msole_metadata_write (GsfOutput *out,
			  GsfDocMetaData const *meta_data,
			  gboolean doc_not_component)
{
	static guint8 const header[] = {
		0xfe, 0xff,	/* byte order */
		   0,    0,	/* Format */
		0x04, 0x0a,	/* OS : XP == 0xA04 */
		0x02, 0x00,	/* win32 == 2 */
		0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, /* clasid = 0 */
	};

	gboolean	success = FALSE;
	guint8		buf [4];
	WritePropState	state;

	state.codepage		= 1252;
	state.out		= out;
	state.dict		= NULL;
	state.builtin.count     = 1; /* codepage */
	state.user.count	= 2; /* codepage and dictionary */
	state.builtin.props     = state.user.props = NULL;
	state.doc_not_component = doc_not_component;
	d (g_print ("================================\nFinding props\n"););
	gsf_doc_meta_data_foreach (meta_data,
		(GHFunc) cb_count_props, &state);
	d (g_print ("Done\n"
		    "================================\n"););

	/* Write stream header */
	GSF_LE_SET_GUINT32 (buf, (state.dict != NULL) ? 2 : 1);
	if (!gsf_output_write (out, sizeof (header), header) ||
	    !gsf_output_write (out, 4, buf))
		goto err;

	/* Write section header(s) */
	GSF_LE_SET_GUINT32 (buf, (state.dict != NULL) ? 0x44 : 0x30);
	if (!gsf_output_write (out, 16,
		doc_not_component ? document_guid : component_guid) ||
	    !gsf_output_write (out, 4, buf))
		goto err;
	if (state.dict != NULL) {
		GSF_LE_SET_GUINT32 (buf, 0);
		if (!gsf_output_write (out, sizeof (user_guid), user_guid) ||
		    !gsf_output_write (out, 4, buf)) /* bogus position, fix it later */
			goto err;
	}

	/* Write section(s) */
	if (!msole_metadata_write_section (&state, FALSE))
		goto err;
	if (state.dict != NULL) {
		gsf_off_t base  = gsf_output_tell (state.out);
		GSF_LE_SET_GUINT32 (buf, base);
		if (!gsf_output_seek (state.out, 0x40, G_SEEK_SET) ||
		    !gsf_output_write (out, 4, buf) ||
		    !gsf_output_seek (state.out, 0, G_SEEK_END) ||
		    !msole_metadata_write_section (&state, TRUE))
			goto err;
	}

	success = TRUE;
err :
	g_slist_free (state.builtin.props);
	g_slist_free (state.user.props);
	if (state.dict != NULL)
		g_hash_table_destroy (state.dict);
	return success;
}

static struct {
	char const *tag;
	guint	lid;
} const gsf_msole_language_ids[] = {
	{ "-none-", 0x0000 }, /* none (language neutral) */
	{ "-none-", 0x0400 }, /* none */
	{ "af_ZA",  0x0436 }, /* Afrikaans */
	{ "am",     0x045e }, /* Amharic */
	{ "sq_AL",  0x041c }, /* Albanian */
	{ "ar_SA",  0x0401 }, /* Arabic (Saudi) */
	{ "ar_IQ",  0x0801 }, /* Arabic (Iraq) */
	{ "ar_EG",  0x0c01 }, /* Arabic (Egypt) */		
	{ "ar_LY",  0x1001 }, /* Arabic (Libya) */
	{ "ar_DZ",  0x1401 }, /* Arabic (Algeria) */
	{ "ar_MA",  0x1801 }, /* Arabic (Morocco) */
	{ "ar_TN",  0x1c01 }, /* Arabic (Tunisia) */
	{ "ar_OM",  0x2001 }, /* Arabic (Oman) */
	{ "ar_YE",  0x2401 }, /* Arabic (Yemen) */		
	{ "ar_SY",  0x2801 }, /* Arabic (Syria) */
	{ "ar_JO",  0x2c01 }, /* Arabic (Jordan) */
	{ "ar_LB",  0x3001 }, /* Arabic (Lebanon) */
	{ "ar_KW",  0x3401 }, /* Arabic (Kuwait) */
	{ "ar_AE",  0x3801 }, /* Arabic (United Arab Emirates) */
	{ "ar_BH",  0x3c01 }, /* Arabic (Bahrain) */		
	{ "ar_QA",  0x4001 }, /* Arabic (Qatar) */
	{ "as",     0x044d }, /* Assamese */
	{ "az",     0x042c }, /* Azerbaijani */
	{ "hy_AM",  0x042b }, /* Armenian */
	{ "az",     0x044c }, /* Azeri (Latin) az_ */
	{ "az",     0x082c }, /* Azeri (Cyrillic) az_ */
	{ "eu_ES",  0x042d }, /* Basque */
	{ "be_BY",  0x0423 }, /* Belarussian */		
	{ "bn",     0x0445 }, /* Bengali bn_ */
	{ "bg_BG",  0x0402 }, /* Bulgarian */
	{ "ca_ES",  0x0403 }, /* Catalan */
	{ "zh_TW",  0x0404 }, /* Chinese (Taiwan) */
	{ "zh_CN",  0x0804 }, /* Chinese (PRC) */
	{ "zh_HK",  0x0c04 }, /* Chinese (Hong Kong) */		
	{ "zh_SG",  0x1004 }, /* Chinese (Singapore) */
	{ "ch_MO",  0x1404 }, /* Chinese (Macau SAR) */
	{ "hr_HR",  0x041a }, /* Croatian */
	{ "cs_CZ",  0x0405 }, /* Czech */
	{ "da_DK",  0x0406 }, /* Danish */
	{ "div",    0x465 }, /* Divehi div_*/
	{ "nl_NL",  0x0413 }, /* Dutch (Netherlands) */		
	{ "nl_BE",  0x0813 }, /* Dutch (Belgium) */
	{ "en_US",  0x0409 }, /* English (USA) */
	{ "en_GB",  0x0809 }, /* English (UK) */
	{ "en_AU",  0x0c09 }, /* English (Australia) */
	{ "en_CA",  0x1009 }, /* English (Canada) */
	{ "en_NZ",  0x1409 }, /* English (New Zealand) */
	{ "en_IE",  0x1809 }, /* English (Ireland) */
	{ "en_ZA",  0x1c09 }, /* English (South Africa) */
	{ "en_JM",  0x2009 }, /* English (Jamaica) */
	{ "en",     0x2409 }, /* English (Caribbean) */
	{ "en_BZ",  0x2809 }, /* English (Belize) */
	{ "en_TT",  0x2c09 }, /* English (Trinidad) */		
	{ "en_ZW",  0x3009 }, /* English (Zimbabwe) */
	{ "en_PH",  0x3409 }, /* English (Phillipines) */
	{ "et_EE",  0x0425 }, /* Estonian */
	{ "fo",     0x0438 }, /* Faeroese fo_ */
	{ "fa_IR",  0x0429 }, /* Farsi */
	{ "fi_FI",  0x040b }, /* Finnish */		
	{ "fr_FR",  0x040c }, /* French (France) */
	{ "fr_BE",  0x080c }, /* French (Belgium) */
	{ "fr_CA",  0x0c0c }, /* French (Canada) */
	{ "fr_CH",  0x100c }, /* French (Switzerland) */
	{ "fr_LU",  0x140c }, /* French (Luxembourg) */
	{ "fr_MC",  0x180c }, /* French (Monaco) */		
	{ "gl",     0x0456 }, /* Galician gl_ */
	{ "ga_IE",  0x083c }, /* Irish Gaelic */
	{ "gd_GB",  0x100c }, /* Scottish Gaelic */
	{ "ka_GE",  0x0437 }, /* Georgian */
	{ "de_DE",  0x0407 }, /* German (Germany) */
	{ "de_CH",  0x0807 }, /* German (Switzerland) */
	{ "de_AT",  0x0c07 }, /* German (Austria) */
	{ "de_LU",  0x1007 }, /* German (Luxembourg) */
	{ "de_LI",  0x1407 }, /* German (Liechtenstein) */
	{ "el_GR",  0x0408 }, /* Greek */
	{ "gu",     0x0447 }, /* Gujarati gu_ */
	{ "ha",     0x0468 }, /* Hausa */
	{ "he_IL",  0x040d }, /* Hebrew */
	{ "hi_IN",  0x0439 }, /* Hindi */
	{ "hu_HU",  0x040e }, /* Hungarian */
	{ "is_IS",  0x040f }, /* Icelandic */		
	{ "id_ID",  0x0421 }, /* Indonesian */
	{ "iu",     0x045d }, /* Inkutitut */
	{ "it_IT",  0x0410 }, /* Italian (Italy) */
	{ "it_CH",  0x0810 }, /* Italian (Switzerland) */
	{ "ja_JP",  0x0411}, /* Japanese */
	{ "kn",     0x044b }, /* Kannada kn_ */
	{ "ks",     0x0860 }, /* Kashmiri (India) ks_ */
	{ "kk",     0x043f }, /* Kazakh kk_ */
	{ "kok",    0x0457 }, /* Konkani kok_ */
	{ "ko_KR",  0x0412 }, /* Korean */
	{ "ko",     0x0812 }, /* Korean (Johab) ko_ */
	{ "kir",    0x0440 }, /* Kyrgyz */
	{ "la",     0x0476 }, /* Latin */
	{ "lo",     0x0454 }, /* Laothian */
	{ "lv_LV",  0x0426 }, /* Latvian */
	{ "lt_LT",  0x0427 }, /* Lithuanian */		
	{ "lt_LT",  0x0827 }, /* Lithuanian (Classic) */
	{ "mk",     0x042f }, /* FYRO Macedonian */
	{ "my_MY",  0x043e }, /* Malaysian */
	{ "my_BN",  0x083e }, /* Malay Brunei Darussalam */
	{ "ml",     0x044c }, /* Malayalam ml_ */
	{ "mr",     0x044e }, /* Marathi mr_ */
	{ "mt",     0x043a }, /* Maltese */
	{ "mo",     0x0450 }, /* Mongolian */
	{ "ne_NP",  0x0461 }, /* Napali (Nepal) */
	{ "ne_IN",  0x0861 }, /* Nepali (India) */
	{ "nb_NO",  0x0414 }, /* Norwegian (Bokmaal) */
	{ "nn_NO",  0x0814 }, /* Norwegian (Nynorsk) */
	{ "or",     0x0448 }, /* Oriya or_ */
	{ "om",     0x0472 }, /* Oromo (Afan, Galla) */
	{ "pl_PL",  0x0415 }, /* Polish */		
	{ "pt_BR",  0x0416 }, /* Portuguese (Brazil) */
	{ "pt_PT",  0x0816 }, /* Portuguese (Portugal) */
	{ "pa",     0x0446 }, /* Punjabi pa_ */
	{ "ps",     0x0463 }, /* Pashto (Pushto) */
	{ "rm",     0x0417 }, /* Rhaeto_Romanic rm_ */
	{ "ro_RO",  0x0418 }, /* Romanian */
	{ "ro_MD",  0x0818 }, /* Romanian (Moldova) */		
	{ "ru_RU",  0x0419 }, /* Russian */
	{ "ru_MD",  0x0819 }, /* Russian (Moldova) */
	{ "se",     0x043b }, /* Sami (Lappish) se_ */
	{ "sa",     0x044f }, /* Sanskrit sa_ */
	{ "sr",     0x0c1a }, /* Serbian (Cyrillic) sr_ */
	{ "sr",     0x081a }, /* Serbian (Latin) sr_ */		
	{ "sd",     0x0459 }, /* Sindhi sd_ */
	{ "sk_SK",  0x041b }, /* Slovak */
	{ "sl_SI",  0x0424 }, /* Slovenian */
	{ "wen",    0x042e }, /* Sorbian wen_ */
	{ "so",     0x0477 }, /* Somali */
	{ "es_ES",  0x040a }, /* Spanish (Spain, Traditional) */
	{ "es_MX",  0x080a }, /* Spanish (Mexico) */		
	{ "es_ES",  0x0c0a }, /* Spanish (Modern) */
	{ "es_GT",  0x100a }, /* Spanish (Guatemala) */
	{ "es_CR",  0x140a }, /* Spanish (Costa Rica) */
	{ "es_PA",  0x180a }, /* Spanish (Panama) */
	{ "es_DO",  0x1c0a }, /* Spanish (Dominican Republic) */
	{ "es_VE",  0x200a }, /* Spanish (Venezuela) */		
	{ "es_CO",  0x240a }, /* Spanish (Colombia) */
	{ "es_PE",  0x280a }, /* Spanish (Peru) */
	{ "es_AR",  0x2c0a }, /* Spanish (Argentina) */
	{ "es_EC",  0x300a }, /* Spanish (Ecuador) */
	{ "es_CL",  0x340a }, /* Spanish (Chile) */
	{ "es_UY",  0x380a }, /* Spanish (Uruguay) */		
	{ "es_PY",  0x3c0a }, /* Spanish (Paraguay) */
	{ "es_BO",  0x400a }, /* Spanish (Bolivia) */
	{ "es_SV",  0x440a }, /* Spanish (El Salvador) */
	{ "es_HN",  0x480a }, /* Spanish (Honduras) */
	{ "es_NI",  0x4c0a }, /* Spanish (Nicaragua) */
	{ "es_PR",  0x500a }, /* Spanish (Puerto Rico) */
	{ "sx",     0x0430 }, /* Sutu */
	{ "sw",     0x0441 }, /* Swahili (Kiswahili/Kenya) */
	{ "sv_SE",  0x041d }, /* Swedish */
	{ "sv_FI",  0x081d }, /* Swedish (Finland) */
	{ "ta",     0x0449 }, /* Tamil ta_ */
	{ "tt",     0x0444 }, /* Tatar (Tatarstan) tt_ */
	{ "te",     0x044a }, /* Telugu te_ */
	{ "th_TH",  0x041e }, /* Thai */
	{ "ts",     0x0431 }, /* Tsonga ts_ */
	{ "tn",     0x0432 }, /* Tswana tn_ */
	{ "tr_TR",  0x041f }, /* Turkish */
	{ "tl",     0x0464 }, /* Tagalog */
	{ "tg",     0x0428 }, /* Tajik */
	{ "bo",     0x0451 }, /* Tibetan */
	{ "ti",     0x0473 }, /* Tigrinya */
	{ "uk_UA",  0x0422 }, /* Ukrainian */		
	{ "ur_PK",  0x0420 }, /* Urdu (Pakistan) */
	{ "ur_IN",  0x0820 }, /* Urdu (India) */
	{ "uz",     0x0443 }, /* Uzbek (Latin) uz_ */
	{ "uz",     0x0843 }, /* Uzbek (Cyrillic) uz_ */
	{ "ven",    0x0433 }, /* Venda ven_ */
	{ "vi_VN",  0x042a }, /* Vietnamese */
	{ "cy_GB",  0x0452 }, /* Welsh */
	{ "xh",     0x0434 }, /* Xhosa xh */
	{ "yi",     0x043d }, /* Yiddish yi_ */
	{ "yo",     0x046a }, /* Yoruba */
	{ "zu",     0x0435 }, /* Zulu zu_ */
	{ "en_US",  0x0800 } /* Default */
};

/**
 * gsf_msole_lid_for_language
 * @lang :
 *
 * Returns the LID (Language Identifier) for the input language.
 * If lang is %null, return 0x0400 ("-none-"), and not 0x0000 ("no proofing")
 **/
guint
gsf_msole_lid_for_language (char const *lang)
{
	guint i = 0 ;
	size_t len;

	if (lang == NULL)
		return 0x0400;   /* return -none- */

	/* Allow lang to match as a prefix (eg fr == fr_FR@euro) */
	len = strlen (lang);
	for (i = 0 ; i < G_N_ELEMENTS(gsf_msole_language_ids); i++)
		if (!strncmp (lang, gsf_msole_language_ids[i].tag, len))
			return gsf_msole_language_ids[i].lid;
	
	return 0x0400 ;   /* return -none- */
}

/**
 * gsf_msole_language_for_lid :
 * @lid :
 *
 * Returns the xx_YY style string (can be just xx or xxx) for the given LID.
 * Return value must not be freed. If the LID is not found, is set to 0x0400,
 * or is set to 0x0000, will return "-none-"
 **/
char const *
gsf_msole_language_for_lid (guint lid)
{
	guint i = 0 ;
	
	for (i = 0 ; i < G_N_ELEMENTS(gsf_msole_language_ids); i++)
		if (gsf_msole_language_ids[i].lid == lid)
			return gsf_msole_language_ids[i].tag;
	
	return "-none-"; /* default */
}

/**
 * gsf_msole_locale_to_lid :
 *
 * Covert the the codepage into an applicable LID
 **/
guint
gsf_msole_codepage_to_lid (int codepage)
{
	switch (codepage) {
	case 77:		/* MAC_CHARSET */
		return 0xFFF;	/* This number is a hack */
	case 128:		/* SHIFTJIS_CHARSET */
		return 0x411;	/* Japanese */
	case 129:		/* HANGEUL_CHARSET */
		return 0x412;	/* Korean */
	case 130:		/* JOHAB_CHARSET */
		return 0x812;	/* Korean (Johab) */
	case 134:		/* GB2312_CHARSET - Chinese Simplified */
		return 0x804;	/* China PRC - And others!! */
	case 136:		/* CHINESEBIG5_CHARSET - Chinese Traditional */
		return 0x404;	/* Taiwan - And others!! */
	case 161:		/* GREEK_CHARSET */
		return 0x408;	/* Greek */
	case 162:		/* TURKISH_CHARSET */
		return 0x41f;	/* Turkish */
	case 163:		/* VIETNAMESE_CHARSET */
		return 0x42a;	/* Vietnamese */
	case 177:		/* HEBREW_CHARSET */
		return 0x40d;	/* Hebrew */
	case 178:		/* ARABIC_CHARSET */
		return 0x01;	/* Arabic */
	case 186:		/* BALTIC_CHARSET */
		return 0x425;	/* Estonian - And others!! */
	case 204:		/* RUSSIAN_CHARSET */
		return 0x419;	/* Russian - And others!! */
	case 222:		/* THAI_CHARSET */
		return 0x41e;	/* Thai */
	case 238:		/* EASTEUROPE_CHARSET */
		return 0x405;	/* Czech - And many others!! */
	}

	/* default */
	return 0x0;
}

/**
 * gsf_msole_lid_to_codepage
 * @lid :
 *
 * Returns our best guess at the codepage for the given language id
 **/
int
gsf_msole_lid_to_codepage (guint lid)
{
	if (lid == 0x0FFF) /* Macintosh Hack */
		return 0x0FFF;

	switch (lid & 0xff) {
	case 0x01:		/* Arabic */
		return 1256;
	case 0x02:		/* Bulgarian */
		return 1251;
	case 0x03:		/* Catalan */
		return 1252;
	case 0x04:		/* Chinese */
		switch (lid) {
		case 0x1004:		/* Chinese (Singapore) */
		case 0x0404:		/* Chinese (Taiwan) */
		case 0x1404:		/* Chinese (Macau SAR) */
		case 0x0c04:		/* Chinese (Hong Kong SAR, PRC) */
			return 950;
			
		case 0x0804:		/* Chinese (PRC) */
			return 936;
		default :
			break;
		}
		break;
	case 0x05:		/* Czech */
		return 1250;
	case 0x06:		/* Danish */
		return 1252;
	case 0x07:		/* German */
		return 1252;
	case 0x08:		/* Greek */
		return 1253;
	case 0x09:		/* English */
		return 1252;
	case 0x0a:		/* Spanish */
		return 1252;
	case 0x0b:		/* Finnish */
		return 1252;
	case 0x0c:		/* French */
		return 1252;
	case 0x0d:		/* Hebrew */
		return 1255;
	case 0x0e:		/* Hungarian */
		return 1250;
	case 0x0f:		/* Icelandic */
		return 1252;
	case 0x10:		/* Italian */
		return 1252;
	case 0x11:		/* Japanese */
		return 932;
	case 0x12:		/* Korean */
		switch (lid) {
		case 0x0812:		/* Korean (Johab) */
			return 1361;
		case 0x0412:		/* Korean */
			return 949;
		default :
			break;
		}
		break;
	case 0x13:		/* Dutch */
		return 1252;
	case 0x14:		/* Norwegian */
		return 1252;
	case 0x15:		/* Polish */
		return 1250;
	case 0x16:		/* Portuguese */
		return 1252;
	case 0x17:		/* Rhaeto-Romanic */
		return 1252;
	case 0x18:		/* Romanian */
		return 1250;
	case 0x19:		/* Russian */
		return 1251;
	case 0x1a:		/* Serbian, Croatian, (Bosnian?) */
		switch (lid) {
		case 0x041a:		/* Croatian */
			return 1252;
		case 0x0c1a:		/* Serbian (Cyrillic) */
			return 1251;
		case 0x081a:		/* Serbian (Latin) */
			return 1252;
		default :
			break;
		}
		break;
	case 0x1b:		/* Slovak */
		return 1250;
	case 0x1c:		/* Albanian */
		return 1251;
	case 0x1d:		/* Swedish */
		return 1252;
	case 0x1e:		/* Thai */
		return 874;
	case 0x1f:		/* Turkish */
		return 1254;
	case 0x20:		/* Urdu. This is Unicode only. */
		return 0;
	case 0x21:		/* Bahasa Indonesian */
		return 1252;
	case 0x22:		/* Ukrainian */
		return 1251;
	case 0x23:		/* Byelorussian / Belarusian */
		return 1251;
	case 0x24:		/* Slovenian */
		return 1250;
	case 0x25:		/* Estonian */
		return 1257;
	case 0x26:		/* Latvian */
		return 1257;
	case 0x27:		/* Lithuanian */
		return 1257;
	case 0x29:		/* Farsi / Persian. This is Unicode only. */
		return 0;
	case 0x2a:		/* Vietnamese */
		return 1258;
	case 0x2b:		/* Windows 2000: Armenian. This is Unicode only. */
		return 0;
	case 0x2c:		/* Azeri */
		switch (lid) {
		case 0x082c:		/* Azeri (Cyrillic) */
			return 1251;
		default :
			break;
		}
		break;
	case 0x2d:		/* Basque */
		return 1252;
	case 0x2f:		/* Macedonian */
		return 1251;
	case 0x36:		/* Afrikaans */
		return 1252;
	case 0x37:		/* Windows 2000: Georgian. This is Unicode only. */
		return 0;
	case 0x38:		/* Faeroese */
		return 1252;
	case 0x39:		/* Windows 2000: Hindi. This is Unicode only. */
		return 0;
	case 0x3E:		/* Malaysian / Malay */
		return 1252;
	case 0x41:		/* Swahili */
		return 1252;
	case 0x43:		/* Uzbek */
		switch (lid) {
		case 0x0843:		/* Uzbek (Cyrillic) */
			return 1251;
		default :
			break;
		}
		break;
	case 0x45:		/* Windows 2000: Bengali. This is Unicode only. */
	case 0x46:		/* Windows 2000: Punjabi. This is Unicode only. */
	case 0x47:		/* Windows 2000: Gujarati. This is Unicode only. */
	case 0x48:		/* Windows 2000: Oriya. This is Unicode only. */
	case 0x49:		/* Windows 2000: Tamil. This is Unicode only. */
	case 0x4a:		/* Windows 2000: Telugu. This is Unicode only. */
	case 0x4b:		/* Windows 2000: Kannada. This is Unicode only. */
	case 0x4c:		/* Windows 2000: Malayalam. This is Unicode only. */
	case 0x4d:		/* Windows 2000: Assamese. This is Unicode only. */
	case 0x4e:		/* Windows 2000: Marathi. This is Unicode only. */
	case 0x4f:		/* Windows 2000: Sanskrit. This is Unicode only. */
	case 0x55:		/* Myanmar / Burmese. This is Unicode only. */
	case 0x57:		/* Windows 2000: Konkani. This is Unicode only. */
	case 0x61:		/* Windows 2000: Nepali (India). This is Unicode only. */
		return 0;

#if 0
		/****************************************************************** 
		 * Below this line is untested, unproven, and are just guesses.   *
		 * Insert above and use at your own risk                          *
		 ******************************************************************/

	case 0x042c:		/* Azeri (Latin) */
	case 0x0443:		/* Uzbek (Latin) */
	case 0x30:		/* Sutu */
		return 1252; /* UNKNOWN, believed to be CP1252 */

	case 0x3f:		/* Kazakh */
		return 1251; /* JUST UNKNOWN, probably CP1251 */

	case 0x44:		/* Tatar */
	case 0x58:		/* Manipuri */
	case 0x59:		/* Sindhi */
	case 0x60:		/* Kashmiri (India) */
		return 0; /* UNKNOWN, believed to be Unicode only */
#endif
	};
	
	/* This is just a guess, but it will be a frequent guess */
	return 1252;
}

/**
 * gsf_msole_lid_to_codepage_str
 * @lid :
 * 
 * Returns the Iconv codepage string for the given LID.
 * Return value must be g_free ()'d
 **/
gchar *
gsf_msole_lid_to_codepage_str (guint lid)
{
	guint cp = 0;

	if (lid == 0x0FFF)	/* Macintosh Hack */
		return g_strdup ("MACINTOSH");

	cp = gsf_msole_lid_to_codepage (lid);
	return g_strdup_printf ("CP%d", cp);
}

/**
 * gsf_msole_iconv_win_codepage :
 *
 * Returns our best guess at the applicable windows code page based on an
 * 	environment variable or the current locale.
 **/
int
gsf_msole_iconv_win_codepage (void)
{
	char *lang;

	if ((lang = getenv("WINDOWS_LANGUAGE")) == NULL) {
		char const *locale = setlocale (LC_CTYPE, NULL);
		if (locale != NULL) {
			char const *lang_sep = strchr (locale, '.');
			if (lang_sep)
				lang = g_strndup (locale, (unsigned)(lang_sep - locale));
			else
				lang = g_strdup (locale); /* simplifies exit */
		}
	}

	if (lang != NULL) {
		guint lid = gsf_msole_lid_for_language (lang);
		g_free (lang);
		return gsf_msole_lid_to_codepage (lid);
	}
	return 1252; /* default ansi */
}

static GSList *
gsf_msole_iconv_get_codepage_string_list (int codepage)
{
	GSList *cp_list = NULL;

	switch (codepage)
	{
		case 1200:
			cp_list = g_slist_prepend (cp_list, g_strdup ("UTF-16LE"));
			break;
		case 1201:
			cp_list = g_slist_prepend (cp_list, g_strdup ("UTF-16BE"));
			break;
		case 0x8000:
		case 10000:
			cp_list = g_slist_prepend (cp_list, g_strdup ("MACROMAN"));
			cp_list = g_slist_prepend (cp_list, g_strdup ("MACINTOSH"));
			break;
		case -535:
		case 65001:
			cp_list = g_slist_prepend (cp_list, g_strdup ("UTF-8"));
			break;
		case 0x8001:
			/* according to OOo docs 8001 is a synonym CP1252 */
			codepage = 1252;
		default:
			cp_list = g_slist_prepend (cp_list, g_strdup_printf ("CP%u", codepage));
	}
	
	return cp_list;
}

/**
 * gsf_msole_iconv_open_codepage_for_import :
 * @to:
 * @codepage :
 *
 * Returns an iconv converter for @codepage -> utf8.
 **/
GIConv
gsf_msole_iconv_open_codepage_for_import (char const *to, int codepage)
{
	GIConv iconv_handle = (GIConv)(-1);
	gchar *codepage_str;
	GSList *codepage_list, *cp;
	g_return_val_if_fail (to != NULL, (GIConv)(-1));

	cp = codepage_list = gsf_msole_iconv_get_codepage_string_list (codepage);
	while (cp) {
		codepage_str = cp->data;
		if (iconv_handle == (GIConv)(-1))
			iconv_handle = g_iconv_open (to, codepage_str);
		g_free (codepage_str);
		cp = cp->next;
	}
	g_slist_free (codepage_list);

	if (iconv_handle == (GIConv)(-1))
		g_warning ("Unable to open an iconv handle from codepage %d -> %s",
			   codepage, to);
	return iconv_handle;
}

/**
 * gsf_msole_iconv_open_for_import :
 * @codepage :
 *
 * Returns an iconv converter for single byte encodings @codepage -> utf8.
 * 	Attempt to handle the semantics of a specification for multibyte encodings
 * 	since this is only supposed to be used for single bytes.
 **/
GIConv
gsf_msole_iconv_open_for_import (int codepage)
{
	return gsf_msole_iconv_open_codepage_for_import ("UTF-8", codepage);
}

/**
 * gsf_msole_iconv_open_codepages_for_export :
 * @codepage_to :
 * @from :
 *
 * Returns an iconv converter to go from utf8 -> to our best guess at a useful
 * 	windows codepage.
 **/
GIConv
gsf_msole_iconv_open_codepages_for_export (int codepage_to, char const *from)
{
	GIConv iconv_handle = (GIConv)(-1);
	gchar *codepage_str;
	GSList *codepage_list, *cp;
	g_return_val_if_fail (from != NULL, (GIConv)(-1));

	cp = codepage_list = gsf_msole_iconv_get_codepage_string_list (codepage_to);
	while (cp) {
		codepage_str = cp->data;
		if (iconv_handle == (GIConv)(-1))
			iconv_handle = g_iconv_open (codepage_str, from);
		g_free (codepage_str);
		cp = cp->next;
	}
	g_slist_free (codepage_list);

	if (iconv_handle == (GIConv)(-1))
		g_warning ("Unable to open an iconv handle from %s -> codepage %u",
			   from, codepage_to);
	return iconv_handle;
}

/**
 * gsf_msole_iconv_open_codepage_for_export :
 * @codepage_to:
 *
 * Returns an iconv converter to go from utf8 -> to our best guess at a useful
 * 	windows codepage.
 **/
GIConv
gsf_msole_iconv_open_codepage_for_export (int codepage_to)
{
	return gsf_msole_iconv_open_codepages_for_export (codepage_to, "UTF-8");
}

/**
 * gsf_msole_iconv_open_for_export :
 *
 * Returns an iconv convert to go from utf8 -> to our best guess at a useful
 * 	windows codepage.
 **/
GIConv
gsf_msole_iconv_open_for_export (void)
{
	return gsf_msole_iconv_open_codepage_for_export (gsf_msole_iconv_win_codepage ());
}

#define VBA_COMPRESSION_WINDOW 4096

/**
 * gsf_msole_inflate:
 * @input: stream to read from
 * @offset: offset into it for start byte of compresse stream
 * 
 * Decompresses an LZ compressed stream.
 * 
 * Return value: A GByteArray that the caller is responsible for freeing
 **/
GByteArray *
gsf_msole_inflate (GsfInput *input, gsf_off_t offset)
{
	GByteArray *res;
	unsigned	i, win_pos, pos = 0;
	unsigned	mask, shift, distance;
	guint8		flag, buffer [VBA_COMPRESSION_WINDOW];
	guint8 const   *tmp;
	guint16		token, len;
	gboolean	clean = TRUE;

	if (gsf_input_seek (input, offset, G_SEEK_SET))
		return NULL;

	res = g_byte_array_new ();

	/* explaination from libole2/ms-ole-vba.c */
	/* The first byte is a flag byte.  Each bit in this byte
	 * determines what the next byte is.  If the bit is zero,
	 * the next byte is a character.  Otherwise the  next two
	 * bytes contain the number of characters to copy from the
	 * umcompresed buffer and where to copy them from (offset,
	 * length).
	 */
	while (NULL != gsf_input_read (input, 1, &flag))
		for (mask = 1; mask < 0x100 ; mask <<= 1)
			if (flag & mask) {
				if (NULL == (tmp = gsf_input_read (input, 2, NULL)))
					break;
				win_pos = pos % VBA_COMPRESSION_WINDOW;
				if (win_pos <= 0x80) {
					if (win_pos <= 0x20)
						shift = (win_pos <= 0x10) ? 12 : 11;
					else
						shift = (win_pos <= 0x40) ? 10 : 9;
				} else {
					if (win_pos <= 0x200)
						shift = (win_pos <= 0x100) ? 8 : 7;
					else if (win_pos <= 0x800)
						shift = (win_pos <= 0x400) ? 6 : 5;
					else
						shift = 4;
				}

				token = GSF_LE_GET_GUINT16 (tmp);
				len = (token & ((1 << shift) - 1)) + 3;
				distance = token >> shift;
				clean = TRUE;
/*				fprintf (stderr, "Shift %d, token len %d, distance %d bytes %.2x %.2x\n",
				shift, len, distance, (token & 0xff), (token >> 8)); */

				for (i = 0; i < len; i++) {
					unsigned srcpos = (pos - distance - 1) % VBA_COMPRESSION_WINDOW;
					guint8 c = buffer [srcpos];
					buffer [pos++ % VBA_COMPRESSION_WINDOW] = c;
				}
			} else {
				if ((pos != 0) && ((pos % VBA_COMPRESSION_WINDOW) == 0) && clean) {
					(void) gsf_input_read (input, 2, NULL);
					clean = FALSE;
					g_byte_array_append (res, buffer, VBA_COMPRESSION_WINDOW);
					break;
				}
				if (NULL != gsf_input_read (input, 1, buffer + (pos % VBA_COMPRESSION_WINDOW)))
					pos++;
				clean = TRUE;
			}

	if (pos % VBA_COMPRESSION_WINDOW)
		g_byte_array_append (res, buffer, pos % VBA_COMPRESSION_WINDOW);
	return res;
}
