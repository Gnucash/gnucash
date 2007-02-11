/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-utils.c: 
 *
 * Copyright (C) 2002-2004 Jody Goldberg (jody@gnome.org)
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
#include <gsf/gsf-utils.h>
#include <gsf/gsf-input.h>
#include <gobject/gvaluecollector.h>
#include <glib/gi18n-lib.h>

#include <ctype.h>
#include <stdio.h>
#include <string.h>

/*
 * Glib gets this wrong, really.  ARM's floating point format is a weird
 * mixture.
 */
#define G_ARMFLOAT_ENDIAN 56781234
#if defined(__arm__) && !defined(__vfp__) && (G_BYTE_ORDER == G_LITTLE_ENDIAN)
#define G_FLOAT_BYTE_ORDER G_ARMFLOAT_ENDIAN
#else
#define G_FLOAT_BYTE_ORDER G_BYTE_ORDER
#endif


static void base64_init (void);

/**
 * gsf_init :
 *
 * Initializes the GSF library
 */
void
gsf_init (void)
{
#ifdef ENABLE_NLS
	bindtextdomain(GETTEXT_PACKAGE, GNOMELOCALEDIR);
	bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
#endif

	g_type_init ();
	base64_init ();
}

/**
 * gsf_shutdown:
 * 
 * De-intializes the GSF library
 */
void
gsf_shutdown (void)
{
}

void
gsf_init_dynamic (GTypeModule *module)
{
	(void)module;
}

void
gsf_shutdown_dynamic (GTypeModule *module)
{
	(void)module;
}

static void
gsf_mem_dump_full (guint8 const *ptr, size_t len, gsf_off_t offset)
{
	gsf_off_t i, j, off;

	for (i = 0 ; i < (len+15)/16 ; i++) {
		g_print ("%8lx | ", (long)(i*16 + offset));
		for (j = 0;j < 16; j++) {
			off = j + (i << 4);
			off<len ? g_print("%2x ", ptr[off]) : g_print("XX ");
		}
		g_print ("| ");
		for (j = 0 ; j < 16 ; j++) {
			off = j + (i<<4);
			g_print ("%c", off < len ? (ptr[off] >= '!' && ptr[off] < 127 ? ptr[off] : '.') : '*');
		}
		g_print ("\n");
	}
}

/**
 * gsf_mem_dump :
 * @ptr: memory area to be dumped.
 * @len: how many bytes will be dumped.
 *
 * Dump @len bytes from the memory location given by @ptr.
 **/
void
gsf_mem_dump (guint8 const *ptr, size_t len)
{
	gsf_mem_dump_full (ptr, len, 0);
}

/**
 * gsf_input_dump :
 * @input: a #GsfInput
 * @dump_as_hex: If TRUE, dump in hexidecmal format
 *
 * Dumps @input's contents to STDOUT, optionally in hex format.
 */
void
gsf_input_dump (GsfInput *input, gboolean dump_as_hex)
{
	gsf_off_t offset = 0;
	size_t size, count, count2, written;
	guint8 const *data;

	/* read in small blocks to excercise things */
	size = gsf_input_size (GSF_INPUT (input));
	while (size > 0) {
		count = size;
		if (count > 0x100)
			count = 0x100;
		data = gsf_input_read (GSF_INPUT (input), count, NULL);
		g_return_if_fail (data != NULL);
		if (dump_as_hex)
			gsf_mem_dump_full (data, count, offset);
		else {
			count2 = count;
			do {
				written = fwrite (data, 1, count2, stdout);
				count2 -= written;
			} while (count2 > 0);
		}
		size -= count;
		offset += count;
	}
	if (!dump_as_hex)
		fflush (stdout);
}

guint64
gsf_le_get_guint64 (void const *p)
{
#if G_BYTE_ORDER == G_BIG_ENDIAN
	if (sizeof (guint64) == 8) {
		guint64 li;
		int     i;
		guint8 *t  = (guint8 *)&li;
		guint8 *p2 = (guint8 *)p;
		int     sd = sizeof (li);

		for (i = 0; i < sd; i++)
			t[i] = p2[sd - 1 - i];

		return li;
	} else {
		g_error ("Big endian machine, but weird size of guint64");
	}
#elif G_BYTE_ORDER == G_LITTLE_ENDIAN
	if (sizeof (guint64) == 8) {
		/*
		 * On i86, we could access directly, but Alphas require
		 * aligned access.
		 */
		guint64 data;
		memcpy (&data, p, sizeof (data));
		return data;
	} else {
		g_error ("Little endian machine, but weird size of guint64");
	}
#else
#error "Byte order not recognised -- out of luck"
#endif
}

float
gsf_le_get_float (void const *p)
{
#if G_FLOAT_BYTE_ORDER == G_BIG_ENDIAN
	if (sizeof (float) == 4) {
		float   f;
		int     i;
		guint8 *t  = (guint8 *)&f;
		guint8 *p2 = (guint8 *)p;
		int     sd = sizeof (f);

		for (i = 0; i < sd; i++)
			t[i] = p2[sd - 1 - i];

		return f;
	} else {
		g_error ("Big endian machine, but weird size of floats");
	}
#elif (G_FLOAT_BYTE_ORDER == G_LITTLE_ENDIAN) || (G_FLOAT_BYTE_ORDER == G_ARMFLOAT_ENDIAN)
	if (sizeof (float) == 4) {
		/*
		 * On i86, we could access directly, but Alphas require
		 * aligned access.
		 */
		float data;
		memcpy (&data, p, sizeof (data));
		return data;
	} else {
		g_error ("Little endian machine, but weird size of floats");
	}
#else
#error "Floating-point byte order not recognised -- out of luck"
#endif
}

void
gsf_le_set_float (void *p, float d)
{
#if G_FLOAT_BYTE_ORDER == G_BIG_ENDIAN
	if (sizeof (float) == 4) {
		int     i;
		guint8 *t  = (guint8 *)&d;
		guint8 *p2 = (guint8 *)p;
		int     sd = sizeof (d);

		for (i = 0; i < sd; i++)
			p2[sd - 1 - i] = t[i];
	} else {
		g_error ("Big endian machine, but weird size of floats");
	}
#elif (G_FLOAT_BYTE_ORDER == G_LITTLE_ENDIAN) || (G_FLOAT_BYTE_ORDER == G_ARMFLOAT_ENDIAN)
	if (sizeof (float) == 4) {
		/*
		 * On i86, we could access directly, but Alphas require
		 * aligned access.
		 */
		memcpy (p, &d, sizeof (d));
	} else {
		g_error ("Little endian machine, but weird size of floats");
	}
#else
#error "Floating-point byte order not recognised -- out of luck"
#endif
}

double
gsf_le_get_double (void const *p)
{
#if G_FLOAT_BYTE_ORDER == G_ARMFLOAT_ENDIAN
	double data;
	memcpy ((char *)&data + 4, p, 4);
	memcpy ((char *)&data, (char const *)p + 4, 4);
	return data;
#elif G_FLOAT_BYTE_ORDER == G_BIG_ENDIAN
	if (sizeof (double) == 8) {
		double  d;
		int     i;
		guint8 *t  = (guint8 *)&d;
		guint8 *p2 = (guint8 *)p;
		int     sd = sizeof (d);

		for (i = 0; i < sd; i++)
			t[i] = p2[sd - 1 - i];

		return d;
	} else {
		g_error ("Big endian machine, but weird size of doubles");
	}
#elif G_FLOAT_BYTE_ORDER == G_LITTLE_ENDIAN
	if (sizeof (double) == 8) {
		/*
		 * On i86, we could access directly, but Alphas require
		 * aligned access.
		 */
		double data;
		memcpy (&data, p, sizeof (data));
		return data;
	} else {
		g_error ("Little endian machine, but weird size of doubles");
	}
#else
#error "Floating-point byte order not recognised -- out of luck"
#endif
}

void
gsf_le_set_double (void *p, double d)
{
#if G_FLOAT_BYTE_ORDER == G_ARMFLOAT_ENDIAN
	memcpy (p, (char const *)&d + 4, 4);
	memcpy ((char *)p + 4, &d, 4);
#elif G_FLOAT_BYTE_ORDER == G_BIG_ENDIAN
	if (sizeof (double) == 8) {
		int     i;
		guint8 *t  = (guint8 *)&d;
		guint8 *p2 = (guint8 *)p;
		int     sd = sizeof (d);

		for (i = 0; i < sd; i++)
			p2[sd - 1 - i] = t[i];
	} else {
		g_error ("Big endian machine, but weird size of doubles");
	}
#elif G_FLOAT_BYTE_ORDER == G_LITTLE_ENDIAN
	if (sizeof (double) == 8) {
		/*
		 * On i86, we could access directly, but Alphas require
		 * aligned access.
		 */
		memcpy (p, &d, sizeof (d));
	} else {
		g_error ("Little endian machine, but weird size of doubles");
	}
#else
#error "Floating-point byte order not recognised -- out of luck"
#endif
}

/**
 * gsf_extension_pointer:
 * @path: A filename or file path.
 *
 * Extracts the extension from the end of a filename (the part after the final
 * '.' in the filename).
 *
 * Returns: A pointer to the extension part of the filename, or a
 * pointer to the end of the string if the filename does not
 * have an extension.
 */
char const *
gsf_extension_pointer (char const *path)
{
	char const *s, *end;
	
	g_return_val_if_fail (path != NULL, NULL);

	end = path + strlen (path);
	for (s = end; s > path; ) {
		s--;
		if (G_IS_DIR_SEPARATOR (*s))
			break;
		if (*s == '.')
			return s + 1;
	}

	return end;
}

/**
 * gsf_iconv_close : A utility wrapper to safely close an iconv handle
 * @handle :
 **/
void
gsf_iconv_close (GIConv handle)
{
	if (handle != NULL && handle != ((GIConv)-1))
		g_iconv_close (handle);
}

/**
 * gsf_filename_to_utf8:
 * @filename: file name suitable for open(2).
 * @quoted: if TRUE, the resulting utf8 file name will be quoted
 *    (unless it is invalid).
 *
 * A utility wrapper to make sure filenames are valid utf8.
 * Caller must g_free the result.
 *
 * Returns @filename using utf-8 encoding for display
 **/
char *
gsf_filename_to_utf8 (char const *filename, gboolean quoted)
{
	char *dname = g_filename_display_name (filename);
	char *result;

	if (quoted) {
		result = g_strconcat ("\"", dname, "\"", NULL);
		g_free (dname);
	} else
		result = dname;

	return result;
}

/***************************************************************************/
/* some code taken from evolution/camel/camel-mime-utils.c */

/*
 *  Copyright (C) 2000 Ximian Inc.
 *
 *  Authors: Michael Zucchi <notzed@ximian.com>
 *           Jeffrey Stedfast <fejj@ximian.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
 * Boston, MA  02110-1301  USA
 */

/* dont touch this file without my permission - Michael */
static guint8 camel_mime_base64_rank[256];
static char const *base64_alphabet =
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

#define d(x)

/* Line length for base64 encoding.  Must be a multiple of 4. */
enum { BASE64_LINE_LEN = 76 };

static void
base64_init(void)
{
	int i;

	memset(camel_mime_base64_rank, 0xff, sizeof(camel_mime_base64_rank));
	for (i=0;i<64;i++) {
		camel_mime_base64_rank[(unsigned int)base64_alphabet[i]] = i;
	}
	camel_mime_base64_rank['='] = 0;
}

/* call this when finished encoding everything, to
   flush off the last little bit */
size_t
gsf_base64_encode_close (guint8 const *in, size_t inlen,
			 gboolean break_lines, guint8 *out, int *state, unsigned int *save)
{
	int c1, c2;
	guint8 *outptr = out;

	if (inlen>0)
		outptr += gsf_base64_encode_step(in, inlen, break_lines, outptr, state, save);

	c1 = ((guint8 *)save)[1];
	c2 = ((guint8 *)save)[2];
	
	d(printf("mode = %d\nc1 = %c\nc2 = %c\n",
		 (int)((char *)save)[0],
		 (int)((char *)save)[1],
		 (int)((char *)save)[2]));

	switch (((char *)save)[0]) {
	case 2:
		outptr[2] = base64_alphabet[ ( (c2 &0x0f) << 2 ) ];
		g_assert(outptr[2] != 0);
		goto skip;
	case 1:
		outptr[2] = '=';
	skip:
		outptr[0] = base64_alphabet[ c1 >> 2 ];
		outptr[1] = base64_alphabet[ c2 >> 4 | ( (c1&0x3) << 4 )];
		outptr[3] = '=';
		outptr += 4;
		++*state;
		break;
	}
	if (break_lines && *state > 0)
		*outptr++ = '\n';

	*save = 0;
	*state = 0;

	return outptr-out;
}

/*
  performs an 'encode step', only encodes blocks of 3 characters to the
  output at a time, saves left-over state in state and save (initialise to
  0 on first invocation).
*/
size_t
gsf_base64_encode_step (guint8 const *in, size_t len,
			gboolean break_lines, guint8 *out, int *state, unsigned int *save)
{
	register guint8 const *inptr;
	register guint8 *outptr;

	if (len<=0)
		return 0;

	inptr = in;
	outptr = out;

	d(printf("we have %d chars, and %d saved chars\n", len, ((char *)save)[0]));

	if (len + ((char *)save)[0] > 2) {
		guint8 const *inend = in+len-2;
		register int c1, c2, c3;
		register int already;

		already = *state;

		switch (((char *)save)[0]) {
		case 1:	c1 = ((guint8 *)save)[1]; goto skip1;
		case 2:	c1 = ((guint8 *)save)[1];
			c2 = ((guint8 *)save)[2]; goto skip2;
		}
		
		/* yes, we jump into the loop, no i'm not going to change it, it's beautiful! */
		while (inptr < inend) {
			c1 = *inptr++;
		skip1:
			c2 = *inptr++;
		skip2:
			c3 = *inptr++;
			*outptr++ = base64_alphabet[ c1 >> 2 ];
			*outptr++ = base64_alphabet[ c2 >> 4 | ( (c1&0x3) << 4 ) ];
			*outptr++ = base64_alphabet[ ( (c2 &0x0f) << 2 ) | (c3 >> 6) ];
			*outptr++ = base64_alphabet[ c3 & 0x3f ];
			/* this is a bit ugly ... */
			if (break_lines && (++already) * 4 >= BASE64_LINE_LEN) {
				*outptr++='\n';
				already = 0;
			}
		}

		((char *)save)[0] = 0;
		len = 2-(inptr-inend);
		*state = already;
	}

	d(printf("state = %d, len = %d\n",
		 (int)((char *)save)[0],
		 len));

	if (len>0) {
		register char *saveout;

		/* points to the slot for the next char to save */
		saveout = & (((char *)save)[1]) + ((char *)save)[0];

		/* len can only be 0 1 or 2 */
		switch(len) {
		case 2:	*saveout++ = *inptr++;
		case 1:	*saveout++ = *inptr++;
		}
		((char *)save)[0]+=len;
	}

	d(printf("mode = %d\nc1 = %c\nc2 = %c\n",
		 (int)((char *)save)[0],
		 (int)((char *)save)[1],
		 (int)((char *)save)[2]));

	return outptr-out;
}


/**
 * gsf_base64_decode_step: decode a chunk of base64 encoded data
 * @in: input stream
 * @len: max length of data to decode
 * @out: output stream
 * @state: holds the number of bits that are stored in @save
 * @save: leftover bits that have not yet been decoded
 *
 * Decodes a chunk of base64 encoded data
 *
 * Returns the number of bytes converted
 **/
size_t
gsf_base64_decode_step (guint8 const *in, size_t len, guint8 *out,
			int *state, guint *save)
{
	register guint8 const *inptr;
	register guint8 *outptr, c;
	register unsigned int v;
	guint8 const *inend;
	int i;

	inend = in+len;
	outptr = out;

	/* convert 4 base64 bytes to 3 normal bytes */
	v=*save;
	i=*state;
	inptr = in;
	while (inptr<inend) {
		c = camel_mime_base64_rank[*inptr++];
		if (c != 0xff) {
			v = (v<<6) | c;
			i++;
			if (i==4) {
				*outptr++ = v>>16;
				*outptr++ = v>>8;
				*outptr++ = v;
				i=0;
			}
		}
	}

	*save = v;
	*state = i;

	/* quick scan back for '=' on the end somewhere */
	/* fortunately we can drop 1 output char for each trailing = (upto 2) */
	i=2;
	while (inptr>in && i) {
		inptr--;
		if (camel_mime_base64_rank[*inptr] != 0xff) {
			if (*inptr == '=' && outptr>out)
				outptr--;
			i--;
		}
	}

	/* if i!= 0 then there is a truncation error! */
	return outptr-out;
}

guint8 *
gsf_base64_encode_simple (guint8 const *data, size_t len)
{
	guint8 *out;
	int state = 0, outlen;
	unsigned int save = 0;
	gboolean break_lines = TRUE;

	outlen = len * 4 / 3 + 5;
	if (break_lines)
		outlen += outlen / BASE64_LINE_LEN + 1;
	out = g_new (guint8, outlen);
	outlen = gsf_base64_encode_close (data, len, break_lines,
					  out, &state, &save);
	out [outlen] = '\0';
	return out;
}

size_t
gsf_base64_decode_simple (guint8 *data, size_t len)
{
	int state = 0;
	unsigned int save = 0;
	return gsf_base64_decode_step (data, len, data, &state, &save);
}


/* Largely a copy of g_object_new_valist.  */
/**
 * gsf_property_settings_collect_valist: collect property setting from a va_list.
 * @object_type: the GType for which the properties are being set.
 * @p_n_params: a pointer to the number of properties collected.  (Used for
 *   both input and output.)
 * @p_params: a pointer to the GParameter array that holds the properties.
 *   (Used for both input and output.  This may point to a NULL pointer if
 *   there are no properties collected yet.)
 * @first_property_name: the name of the first property being set, or NULL.
 * @var_args: a va_list holding the remainder of the property names and
 *   values, terminated by a NULL.
 *
 * This function builds a GParameter array suitable for g_object_newv.
 **/
void
gsf_property_settings_collect_valist (GType object_type,
				      GParameter **p_params,
				      size_t *p_n_params,
				      const gchar *first_property_name,
				      va_list var_args)
{
  GObjectClass *class;
  GParameter *params = *p_params;
  const gchar *name;
  size_t n_params = *p_n_params;
  size_t n_alloced_params = n_params;  /* We might have more.  */

  g_return_if_fail (G_TYPE_IS_OBJECT (object_type));

  class = g_type_class_ref (object_type);

  name = first_property_name;
  while (name)
    {
      gchar *error = NULL;
      GParamSpec *pspec = g_object_class_find_property (class, name);
      if (!pspec)
	{
	  g_warning ("%s: object class `%s' has no property named `%s'",
		     G_STRFUNC,
		     g_type_name (object_type),
		     name);
	  break;
	}

      if (n_params >= n_alloced_params)
	{
	  n_alloced_params += 16;
	  params = g_renew (GParameter, params, n_alloced_params);
	}
      params[n_params].name = name;
      params[n_params].value.g_type = 0;
      g_value_init (&params[n_params].value, G_PARAM_SPEC_VALUE_TYPE (pspec));
      G_VALUE_COLLECT (&params[n_params].value, var_args, 0, &error);
      if (error)
	{
	  g_warning ("%s: %s", G_STRFUNC, error);
	  g_free (error);
          g_value_unset (&params[n_params].value);
	  break;
	}
      n_params++;
      name = va_arg (var_args, gchar*);
    }

  g_type_class_unref (class);

  *p_params = params;
  *p_n_params = n_params;
}

/* This is a vararg version of gsf_property_settings_collect_valist.  */
void
gsf_property_settings_collect (GType object_type,
			       GParameter **p_params,
			       size_t *p_n_params,
			       const gchar *first_property_name,
			       ...)
{
  va_list var_args;
  va_start (var_args, first_property_name);
  gsf_property_settings_collect_valist (object_type, p_params, p_n_params, first_property_name, var_args);
  va_end (var_args);
}

void
gsf_property_settings_free (GParameter *params,
			    size_t n_params)
{
	while (n_params--)
		g_value_unset (&params[n_params].value);
	g_free (params);
}



/* Errors */

/**
 * gsf_error_quark:
 *
 * Returns the #GQuark used to identify libgsf errors in #GError structures.
 * Specific error codes come from the #GsfError enumeration.
 * 
 * Return value: A #GQuark.
 **/
GQuark
gsf_error_quark (void)
{
	static GQuark quark;

	if (quark == 0)
		quark = g_quark_from_static_string ("gsf-error-quark");

	return quark;
}
