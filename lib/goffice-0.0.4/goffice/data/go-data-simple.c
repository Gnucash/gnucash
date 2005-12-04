/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-data-simple.c : 
 *
 * Copyright (C) 2003-2005 Jody Goldberg (jody@gnome.org)
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
#include <goffice/goffice-config.h>
#include "go-data-simple.h"
#include "go-data-impl.h"
#include <goffice/utils/go-format.h>
#include <goffice/utils/go-math.h>

#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>

#include <string.h>
#include <errno.h>
#include <stdlib.h>

struct _GODataScalarVal {
	GODataScalar	 base;
	double		 val;
	char		*str;
};
typedef GODataScalarClass GODataScalarValClass;

static GObjectClass *scalar_val_parent_klass;

static void
go_data_scalar_val_finalize (GObject *obj)
{
	GODataScalarVal *val = (GODataScalarVal *)obj;

	if (val->str != NULL) {
		g_free (val->str);
		val->str = NULL;
	}

	(*scalar_val_parent_klass->finalize) (obj);
}

static GOData *
go_data_scalar_val_dup (GOData const *src)
{
	GODataScalarVal *dst = g_object_new (G_OBJECT_TYPE (src), NULL);
	GODataScalarVal const *src_val = (GODataScalarVal const *)src;
	dst->val = src_val->val;
	return GO_DATA (dst);
}

static gboolean
go_data_scalar_val_eq (GOData const *a, GOData const *b)
{
	GODataScalarVal const *sval_a = (GODataScalarVal const *)a;
	GODataScalarVal const *sval_b = (GODataScalarVal const *)b;

	/* GOData::eq is used for identity, not arithmetic */
	return sval_a->val == sval_b->val;
}

static char *
go_data_scalar_val_as_str (GOData const *dat)
{
	return g_strdup (go_data_scalar_get_str (GO_DATA_SCALAR (dat)));
}

static gboolean
go_data_scalar_val_from_str (GOData *dat, char const *str)
{
	GODataScalarVal *sval = (GODataScalarVal *)dat;
	double tmp;
	char *end;
	errno = 0; /* strto(ld) sets errno, but does not clear it.  */
	tmp = strtod (str, &end);

	if (end == str || *end != '\0' || errno == ERANGE)
		return FALSE;

	g_free (sval->str);
	sval->str = NULL;
	sval->val = tmp;
	return TRUE;
}

static double
go_data_scalar_val_get_value (GODataScalar *dat)
{
	GODataScalarVal const *sval = (GODataScalarVal const *)dat;
	return sval->val;
}

static char const *
go_data_scalar_val_get_str (GODataScalar *dat)
{
	GODataScalarVal *sval = (GODataScalarVal *)dat;

	if (sval->str == NULL)
		sval->str = g_strdup_printf ("%g", sval->val);
	return sval->str;
}

static void
go_data_scalar_val_class_init (GObjectClass *gobject_klass)
{
	GODataClass *godata_klass = (GODataClass *) gobject_klass;
	GODataScalarClass *scalar_klass = (GODataScalarClass *) gobject_klass;

	scalar_val_parent_klass = g_type_class_peek_parent (gobject_klass);
	gobject_klass->finalize = go_data_scalar_val_finalize;
	godata_klass->dup	= go_data_scalar_val_dup;
	godata_klass->eq	= go_data_scalar_val_eq;
	godata_klass->as_str	= go_data_scalar_val_as_str;
	godata_klass->from_str	= go_data_scalar_val_from_str;
	scalar_klass->get_value	= go_data_scalar_val_get_value;
	scalar_klass->get_str	= go_data_scalar_val_get_str;
}

GSF_CLASS (GODataScalarVal, go_data_scalar_val,
	   go_data_scalar_val_class_init, NULL,
	   GO_DATA_SCALAR_TYPE)

GOData *
go_data_scalar_val_new (double val)
{
	GODataScalarVal *res = g_object_new (GO_DATA_SCALAR_VAL_TYPE, NULL);
	res->val = val;
	return GO_DATA (res);
}

/*****************************************************************************/

struct _GODataScalarStr {
	GODataScalar	 base;
	char const *str;
	gboolean    needs_free;
};
typedef GODataScalarClass GODataScalarStrClass;

static GObjectClass *scalar_str_parent_klass;

static void
go_data_scalar_str_finalize (GObject *obj)
{
	GODataScalarStr *str = (GODataScalarStr *)obj;

	if (str->needs_free && str->str != NULL) {
		g_free ((char *)str->str);
		str->str = NULL;
	}
	(*scalar_str_parent_klass->finalize) (obj);
}

static GOData *
go_data_scalar_str_dup (GOData const *src)
{
	GODataScalarStr *dst = g_object_new (G_OBJECT_TYPE (src), NULL);
	GODataScalarStr const *src_val = (GODataScalarStr const *)src;
	dst->needs_free = TRUE;
	dst->str = g_strdup (src_val->str);
	return GO_DATA (dst);
}

static gboolean
go_data_scalar_str_eq (GOData const *a, GOData const *b)
{
	GODataScalarStr const *str_a = (GODataScalarStr const *)a;
	GODataScalarStr const *str_b = (GODataScalarStr const *)b;
	return 0 == strcmp (str_a->str, str_b->str);
}

static char *
go_data_scalar_str_as_str (GOData const *dat)
{
	GODataScalarStr const *str = (GODataScalarStr const *)dat;
	return g_strdup (str->str);
}

static gboolean
go_data_scalar_str_from_str (GOData *dat, char const *string)
{
	GODataScalarStr *str = (GODataScalarStr *)dat;

	if (str->str == string)
		return TRUE;
	if (str->needs_free)
		g_free ((char *)str->str);
	str->str = g_strdup (string);
	str->needs_free = TRUE;
	return TRUE;
}

static double
go_data_scalar_str_get_value (GODataScalar *dat)
{
	return go_nan;
}

static char const *
go_data_scalar_str_get_str (GODataScalar *dat)
{
	GODataScalarStr const *str = (GODataScalarStr const *)dat;
	return str->str;
}

static void
go_data_scalar_str_class_init (GObjectClass *gobject_klass)
{
	GODataClass *godata_klass = (GODataClass *) gobject_klass;
	GODataScalarClass *scalar_klass = (GODataScalarClass *) gobject_klass;

	scalar_str_parent_klass = g_type_class_peek_parent (gobject_klass);
	gobject_klass->finalize	= go_data_scalar_str_finalize;
	godata_klass->dup	= go_data_scalar_str_dup;
	godata_klass->eq	= go_data_scalar_str_eq;
	godata_klass->as_str	= go_data_scalar_str_as_str;
	godata_klass->from_str	= go_data_scalar_str_from_str;
	scalar_klass->get_value	= go_data_scalar_str_get_value;
	scalar_klass->get_str	= go_data_scalar_str_get_str;
}

static void
go_data_scalar_str_init (GObject *obj)
{
	GODataScalarStr *str = (GODataScalarStr *)obj;
	str->str = "";
	str->needs_free = FALSE;
}

GSF_CLASS (GODataScalarStr, go_data_scalar_str,
	   go_data_scalar_str_class_init, go_data_scalar_str_init,
	   GO_DATA_SCALAR_TYPE)

GOData *
go_data_scalar_str_new (char const *str, gboolean needs_free)
{
	GODataScalarStr *res = g_object_new (GO_DATA_SCALAR_STR_TYPE, NULL);
	res->str	= str;
	res->needs_free = needs_free;
	return GO_DATA (res);
}
void
go_data_scalar_str_set_str (GODataScalarStr *str,
			    char const *text, gboolean needs_free)
{
	if (str->str == text)
		return;
	if (str->needs_free)
		g_free ((char *)str->str);
	str->str = text;
	str->needs_free = needs_free;
	go_data_emit_changed (GO_DATA (str));
}

/*****************************************************************************/

struct _GODataVectorVal {
	GODataVector	 base;
	unsigned	 n;
	double *val;
	GDestroyNotify notify;
};
typedef GODataVectorClass GODataVectorValClass;

static GObjectClass *vector_val_parent_klass;

static void
go_data_vector_val_finalize (GObject *obj)
{
	GODataVectorVal *vec = (GODataVectorVal *)obj;
	if (vec->notify && vec->val)
		(*vec->notify) (vec->val);

	(*vector_val_parent_klass->finalize) (obj);
}

static GOData *
go_data_vector_val_dup (GOData const *src)
{
	GODataVectorVal *dst = g_object_new (G_OBJECT_TYPE (src), NULL);
	GODataVectorVal const *src_val = (GODataVectorVal const *)src;
	if (src_val->notify) {
		dst->val = g_new (double, src_val->n);
		memcpy (dst->val, src_val->val, src_val->n * sizeof (double));
		dst->notify = g_free;
	} else
		dst->val = src_val->val;
	dst->n = src_val->n;
	return GO_DATA (dst);
}

static gboolean
go_data_vector_val_eq (GOData const *a, GOData const *b)
{
	GODataVectorVal const *val_a = (GODataVectorVal const *)a;
	GODataVectorVal const *val_b = (GODataVectorVal const *)b;

	/* GOData::eq is used for identity, not arithmetic */
	return val_a->val == val_b->val && val_a->n == val_b->n;
}

static void
go_data_vector_val_load_len (GODataVector *vec)
{
	vec->base.flags |= GO_DATA_VECTOR_LEN_CACHED;
	vec->len = ((GODataVectorVal *)vec)->n;
}

static void
go_data_vector_val_load_values (GODataVector *vec)
{
	GODataVectorVal const *val = (GODataVectorVal const *)vec;
	double minimum = DBL_MAX, maximum = -DBL_MAX;
	int i = val->n;

	vec->values = (double *)val->val;

	while (i-- > 0) {
		if (minimum > val->val[i])
			minimum = val->val[i];
		if (maximum < val->val[i])
			maximum = val->val[i];
	}
	vec->minimum = minimum;
	vec->maximum = maximum;
	vec->base.flags |= GO_DATA_CACHE_IS_VALID;
}

static double
go_data_vector_val_get_value (GODataVector *vec, unsigned i)
{
	GODataVectorVal const *val = (GODataVectorVal const *)vec;
	g_return_val_if_fail (val != NULL && val->val != NULL && i < val->n, go_nan);
	return val->val[i];
}

static char *
go_data_vector_val_get_str (GODataVector *vec, unsigned i)
{
	GODataVectorVal const *val = (GODataVectorVal const *)vec;
	g_return_val_if_fail (val != NULL && val->val != NULL && i < val->n, NULL);
	return g_strdup_printf ("%g", val->val[i]);
}

static char *
go_data_vector_val_as_str (GOData const *dat)
{
	GODataVectorVal *vec = GO_DATA_VECTOR_VAL (dat);
	GString *str;
	char sep, sz[G_ASCII_DTOSTR_BUF_SIZE];
	unsigned i;

	if (vec->n ==0)
		return g_strdup ("");

	sep = format_get_col_sep ();
	g_snprintf (sz, sizeof (sz), "%g", vec->val[0]);
	str = g_string_new (sz);
	for (i = 1; i < vec->n; i++) {
		g_string_append_c (str, sep);
		g_snprintf (sz, sizeof (sz), "%g", vec->val[i]);
		g_string_append (str, sz);
	}
	return g_string_free (str, FALSE);
}

static gboolean
go_data_vector_val_from_str (GOData *dat, char const *str)
{
	GODataVectorVal *vec = GO_DATA_VECTOR_VAL (dat);
	char sep, *end = (char*) str;
	double val;
	GArray *values;

	g_return_val_if_fail (str != NULL, TRUE);

	if (vec->notify && vec->val)
		(*vec->notify) (vec->val);

	values = g_array_sized_new (FALSE, FALSE, sizeof(double), 16);
	sep = 0;
	vec->val = NULL;
	vec->n = 0;
	vec->notify = (GDestroyNotify) g_free;
	while (1) {
		val = g_strtod (end, &end);
		g_array_append_val (values, val);
		if (*end) {
			if (!sep) {
				/* allow the use of all possible seps */
				if ((sep = format_get_arg_sep ()) != *end)
					if ((sep = format_get_col_sep ()) != *end)
						sep = format_get_row_sep ();
			}
			if (*end != sep) {
				g_array_free (values, TRUE);
				return FALSE;
			}
			end++;
		} else
			break;
	}
	if (values->len == 0) {
		g_array_free (values, TRUE);
		return TRUE;
	}
	vec->n = values->len;
	vec->val = (double*) values->data;
	g_array_free (values, FALSE);
	go_data_emit_changed (GO_DATA (vec));
	return TRUE;
}

static void
go_data_vector_val_class_init (GObjectClass *gobject_klass)
{
	GODataClass *godata_klass = (GODataClass *) gobject_klass;
	GODataVectorClass *vector_klass = (GODataVectorClass *) gobject_klass;

	vector_val_parent_klass = g_type_class_peek_parent (gobject_klass);
	gobject_klass->finalize = go_data_vector_val_finalize;
	godata_klass->dup	= go_data_vector_val_dup;
	godata_klass->eq	= go_data_vector_val_eq;
	godata_klass->as_str	= go_data_vector_val_as_str;
	godata_klass->from_str	= go_data_vector_val_from_str;
	vector_klass->load_len    = go_data_vector_val_load_len;
	vector_klass->load_values = go_data_vector_val_load_values;
	vector_klass->get_value   = go_data_vector_val_get_value;
	vector_klass->get_str     = go_data_vector_val_get_str;
}

GSF_CLASS (GODataVectorVal, go_data_vector_val,
	   go_data_vector_val_class_init, NULL,
	   GO_DATA_VECTOR_TYPE)

GOData *
go_data_vector_val_new (double *val, unsigned n, GDestroyNotify notify)
{
	GODataVectorVal *res = g_object_new (GO_DATA_VECTOR_VAL_TYPE, NULL);
	res->val = val;
	res->n = n;
	res->notify = notify;
	return GO_DATA (res);
}

/*****************************************************************************/

struct _GODataVectorStr {
	GODataVector	 base;
	char const * const *str;
	int n;
	GDestroyNotify notify;   

	GOTranslateFunc translate_func;
	gpointer        translate_data;
	GDestroyNotify  translate_notify;   
};
typedef GODataVectorClass GODataVectorStrClass;

static GObjectClass *vector_str_parent_klass;

static void
cb_strings_destroy_notify (gpointer data)
{
	char **str = (char **) data;
	unsigned i = 0;
	while (str[i] != NULL)
		g_free (str[i++]);
	g_free (str);
}

static void
go_data_vector_str_finalize (GObject *obj)
{
	GODataVectorStr *str = (GODataVectorStr *)obj;
	if (str->notify && str->str != NULL)
		(*str->notify) ((gpointer)str->str);

	if (str->translate_notify != NULL)
		(*str->translate_notify) (str->translate_data);

	if (str->base.values != NULL)
		g_free (str->base.values);
	str->base.values = NULL;

	(*vector_str_parent_klass->finalize) (obj);
}

static GOData *
go_data_vector_str_dup (GOData const *src)
{
	GODataVectorStr *dst = g_object_new (G_OBJECT_TYPE (src), NULL);
	GODataVectorStr const *src_val = (GODataVectorStr const *)src;
	dst->n = src_val->n;
	if (src_val->notify) {
		int i;
		char const * *str = g_new (char const *, src_val->n + 1);
		for (i = 0; i < src_val->n; i++)
			str[i] = g_strdup (src_val->str[i]);
		str[src_val->n] = NULL;
		dst->str = str;
		dst->notify = cb_strings_destroy_notify;
	} else
		dst->str = src_val->str;
	return GO_DATA (dst);
}

static gboolean
go_data_vector_str_eq (GOData const *a, GOData const *b)
{
	GODataVectorStr const *str_a = (GODataVectorStr const *)a;
	GODataVectorStr const *str_b = (GODataVectorStr const *)b;
	return str_a->str == str_b->str && str_a->n == str_b->n;
}

static char *
go_data_vector_str_as_str (GOData const *dat)
{
	GODataVectorStr *vec = GO_DATA_VECTOR_STR (dat);
	GString *str;
	char sep;
	int i;

	sep = format_get_col_sep ();
	if (vec->n ==0)
		return g_strdup ("");

	str = g_string_new ("");
	g_string_append_c (str, '\"');
	g_string_append (str, vec->str[0]);
	g_string_append_c (str, '\"');
	for (i = 1; i < vec->n; i++) {
		g_string_append_c (str, sep);
		g_string_append_c (str, '\"');
		g_string_append (str, vec->str[i]);
		g_string_append_c (str, '\"');
	}
	return g_string_free (str, FALSE);
}

static gboolean
go_data_vector_str_from_str (GOData *dat, char const *str)
{
	GODataVectorStr *vec = GO_DATA_VECTOR_STR (dat);
	char sep, *cur = (char*) str, *end, *val;
	GArray *values;

	g_return_val_if_fail (str != NULL, TRUE);

	if (vec->notify && vec->str)
		(*vec->notify) ((gpointer)vec->str);

	values = g_array_sized_new (FALSE, FALSE, sizeof(char*), 16);
	/* search which separator has been used */
	sep = format_get_col_sep ();
	end = strchr (cur, sep);
	if (end == NULL) {
		sep = format_get_arg_sep ();
		end = strchr (cur, sep);
		if (end ==NULL)
			sep = format_get_row_sep ();
	}
	vec->str = NULL;
	vec->n = 0;
	vec->notify = cb_strings_destroy_notify;
	while (*cur) {
		if (*cur == '\"') {
			cur++;
			end = strchr (cur, '\"');
			if (end == NULL) {
				g_array_free (values, TRUE);
				return FALSE;
			}
			val = g_strndup (cur, end - cur);
			g_array_append_val (values, val);
			if (end[1] == 0)
				break;
			if (end[1] != sep) {
				g_array_free (values, TRUE);
				return FALSE;
			}
			cur = end + 2;
		} else {
			/* the string is not quotes delimited */
			end = strchr (cur, sep);
			if (end == NULL) {
				if (strchr (cur, '\"')) {
					/* string containg quotes are not allowed */
					g_array_free (values, TRUE);
					return FALSE;
				}
				val = g_strdup (cur);
				g_array_append_val (values, val);
				break;
			}
			val = g_strndup (cur, end - cur);
			g_array_append_val (values, val);
			if (strchr (val, '\"')) {
				/* string containg quotes are not allowed */
				g_array_free (values, TRUE);
				return FALSE;
			}					
			cur = end + 1;
		}
	}
	if (values->len == 0) {
		g_array_free (values, TRUE);
		return TRUE;
	}
	vec->n = values->len;
	val = NULL;
	g_array_append_val (values, val);
	vec->str = (char const*const*) values->data;
	g_array_free (values, FALSE);
	go_data_emit_changed (GO_DATA (vec));
	return TRUE;
}

static void
go_data_vector_str_load_len (GODataVector *vec)
{
	vec->base.flags |= GO_DATA_VECTOR_LEN_CACHED;
	if (vec->values && vec->len != ((GODataVectorStr *)vec)->n) {
		g_free (vec->values);
		vec->values = NULL;
	}
	vec->len = ((GODataVectorStr *)vec)->n;
}

static void
go_data_vector_str_load_values (GODataVector *vec)
{
	char *end;
	GODataVectorStr const *strs = (GODataVectorStr const *)vec;
	double minimum = DBL_MAX, maximum = -DBL_MAX;
	int i = vec->len = strs->n;

	if (vec->values == NULL)
		vec->values = g_new (double, strs->n);
	while (i-- > 0) {
		vec->values[i] = g_strtod (strs->str[i], &end);
		if (*end) {
			vec->values[i] = go_nan;
			continue;
		}
		if (minimum > vec->values[i])
			minimum = vec->values[i];
		if (maximum < vec->values[i])
			maximum = vec->values[i];
	}
	vec->minimum = minimum;
	vec->maximum = maximum;
	vec->base.flags |= GO_DATA_CACHE_IS_VALID;
}

static double
go_data_vector_str_get_value (GODataVector *vec, unsigned i)
{
	char *end;
	GODataVectorStr *strs = (GODataVectorStr *)vec;
	double d = g_strtod (strs->str[i], &end);
	return (*end)? go_nan: d;
}

static char *
go_data_vector_str_get_str (GODataVector *vec, unsigned i)
{
	GODataVectorStr *strs = (GODataVectorStr *)vec;
	if (strs->translate_func == NULL)
		return g_strdup (strs->str[i]);
	return g_strdup ((strs->translate_func) (strs->str[i],
						 strs->translate_data));
}

static void
go_data_vector_str_class_init (GObjectClass *gobject_klass)
{
	GODataClass *godata_klass = (GODataClass *) gobject_klass;
	GODataVectorClass *vector_klass = (GODataVectorClass *) gobject_klass;

	vector_str_parent_klass = g_type_class_peek_parent (gobject_klass);
	gobject_klass->finalize	= go_data_vector_str_finalize;
	godata_klass->dup	= go_data_vector_str_dup;
	godata_klass->eq	= go_data_vector_str_eq;
	godata_klass->as_str	= go_data_vector_str_as_str;
	godata_klass->from_str	= go_data_vector_str_from_str;
	vector_klass->load_len    = go_data_vector_str_load_len;
	vector_klass->load_values = go_data_vector_str_load_values;
	vector_klass->get_value   = go_data_vector_str_get_value;
	vector_klass->get_str     = go_data_vector_str_get_str;
}

static void
go_data_vector_str_init (GObject *obj)
{
	GODataVectorStr *str = (GODataVectorStr *)obj;
	str->str = NULL;
	str->n = 0;
	str->notify = NULL;
	str->translate_func = NULL;
	str->translate_data = NULL;
	str->translate_notify = NULL;
}

GSF_CLASS (GODataVectorStr, go_data_vector_str,
	   go_data_vector_str_class_init, go_data_vector_str_init,
	   GO_DATA_VECTOR_TYPE)

GOData *
go_data_vector_str_new (char const * const *str, unsigned n, GDestroyNotify notify)
{
	GODataVectorStr *res = g_object_new (GO_DATA_VECTOR_STR_TYPE, NULL);
	res->str = str;
	res->n	 = n;
	res->notify = notify;
	return GO_DATA (res);
}

/**
 * go_data_vector_str_set_translate_func:
 * @vec: a #GODataVectorStr
 * @func: a #GOTranslateFunc
 * @data: data to be passed to @func and @notify
 * @notify: a #GODestroyNotify function to be called when @vec is 
 *   destroyed or when the translation function is changed
 * 
 * Sets a function to be used for translating elements of @vec
 **/
void
go_data_vector_str_set_translate_func (GODataVectorStr	*vec,
				       GOTranslateFunc	 func,
				       gpointer		 data,
				       GDestroyNotify	 notify)
{
	g_return_if_fail (GO_DATA_VECTOR_STR (vec) != NULL);

	if (vec->translate_notify != NULL)
		(*vec->translate_notify) (vec->translate_data);

	vec->translate_func = func;
	vec->translate_data = data;
	vec->translate_notify = notify;
}

static char const *
dgettext_swapped (char const *msgid, 
		  char const *domainname)
{
	return dgettext (domainname, msgid);
}

/**
 * go_data_vector_str_set_translation_domain:
 * @action_group: a #GtkActionGroup
 * @domain: the translation domain to use for dgettext() calls
 * 
 * Sets the translation domain and uses dgettext() for translating the 
 * elements of @vec.
 * Note that libgoffice expects all strings to be encoded in UTF-8, therefore
 * the translation domain must have its codeset set to UTF-8, see
 * bind_textdomain_codeset() in the gettext() documentation. 
 *
 * If you're not using gettext() for localization, see 
 * go_data_vector_str_set_translate_func().
 **/
void 
go_data_vector_str_set_translation_domain (GODataVectorStr *vec,
					   char const      *domain)
{
	g_return_if_fail (GO_DATA_VECTOR_STR (vec) != NULL);

	go_data_vector_str_set_translate_func (vec, 
		(GOTranslateFunc)dgettext_swapped, g_strdup (domain), g_free);
}
/*****************************************************************************/

struct _GODataMatrixVal {
	GODataMatrix	 base;
	GODataMatrixSize size;
	double *val;
	GDestroyNotify notify;
};

typedef GODataMatrixClass GODataMatrixValClass;

static GObjectClass *matrix_val_parent_klass;

static void
go_data_matrix_val_finalize (GObject *obj)
{
	GODataMatrixVal *mat = (GODataMatrixVal *)obj;
	if (mat->notify && mat->val)
		(*mat->notify) (mat->val);

	(*matrix_val_parent_klass->finalize) (obj);
}

static GOData *
go_data_matrix_val_dup (GOData const *src)
{
	GODataMatrixVal *dst = g_object_new (G_OBJECT_TYPE (src), NULL);
	GODataMatrixVal const *src_val = (GODataMatrixVal const *)src;
	if (src_val->notify) {
		dst->val = g_new (double, src_val->size.rows * src_val->size.columns);
		memcpy (dst->val, src_val->val, src_val->size.rows * src_val->size.columns * sizeof (double));
		dst->notify = g_free;
	} else
		dst->val = src_val->val;
	dst->size = src_val->size;
	return GO_DATA (dst);
}

static gboolean
go_data_matrix_val_eq (GOData const *a, GOData const *b)
{
	GODataMatrixVal const *val_a = (GODataMatrixVal const *)a;
	GODataMatrixVal const *val_b = (GODataMatrixVal const *)b;

	/* GOData::eq is used for identity, not arithmetic */
	return val_a->val == val_b->val &&
			val_a->size.rows == val_b->size.rows &&
			val_a->size.columns == val_b->size.columns;
}

static void
go_data_matrix_val_load_size (GODataMatrix *mat)
{
	mat->base.flags |= GO_DATA_MATRIX_SIZE_CACHED;
	mat->size = ((GODataMatrixVal *)mat)->size;
}

static void
go_data_matrix_val_load_values (GODataMatrix *mat)
{
	GODataMatrixVal const *val = (GODataMatrixVal const *)mat;
	double minimum = DBL_MAX, maximum = -DBL_MAX;
	int i = val->size.rows * val->size.columns;

	mat->values = (double *)val->val;

	while (i-- > 0) {
		if (minimum > val->val[i])
			minimum = val->val[i];
		if (maximum < val->val[i])
			maximum = val->val[i];
	}
	mat->minimum = minimum;
	mat->maximum = maximum;
	mat->base.flags |= GO_DATA_CACHE_IS_VALID;
}

static double
go_data_matrix_val_get_value (GODataMatrix *mat, unsigned i, unsigned j)
{
	return mat->values[i * mat->size.columns + j];
}

static char *
go_data_matrix_val_get_str (GODataMatrix *mat, unsigned i, unsigned j)
{
	return g_strdup_printf ("%g", mat->values[i * mat->size.columns + j]);
}

static char *
go_data_matrix_val_as_str (GOData const *dat)
{
	GODataMatrixVal *mat = GO_DATA_MATRIX_VAL (dat);
	GString *str;
	char row_sep, col_sep, sz[G_ASCII_DTOSTR_BUF_SIZE];
	int i, j;

	if (mat->size.rows == 0 || mat->size.columns == 0)
		return g_strdup ("");

	col_sep = format_get_col_sep ();
	row_sep = format_get_row_sep ();
	g_snprintf (sz, sizeof (sz), "%g", mat->val[0]);
	str = g_string_new (sz);
	for (j = 1; j < mat->size.columns; j++) {
		g_string_append_c (str, col_sep);
		g_snprintf (sz, sizeof (sz), "%g", mat->val[j]);
		g_string_append (str, sz);
	}
	for (i = 1; i < mat->size.rows; i++) {
		g_string_append_c (str, row_sep);
		g_snprintf (sz, sizeof (sz), "%g", mat->val[i * mat->size.columns]);
		g_string_append (str, sz);
		for (j = 1; j < mat->size.columns; j++) {
			g_string_append_c (str, col_sep);
			g_snprintf (sz, sizeof (sz), "%g", mat->val[i * mat->size.columns + j]);
			g_string_append (str, sz);
		}
	}
	return g_string_free (str, FALSE);
}

static gboolean
go_data_matrix_val_from_str (GOData *dat, char const *str)
{
	GODataMatrixVal *mat = GO_DATA_MATRIX_VAL (dat);
	char row_sep, col_sep, *end = (char*) str;
	int i, j, columns;
	double val;
	GArray *values;

	g_return_val_if_fail (str != NULL, TRUE);

	values = g_array_sized_new (FALSE, FALSE, sizeof(double), 16);
	col_sep = format_get_col_sep ();
	row_sep = format_get_row_sep ();
	i = j = columns = 0;
	if (mat->notify && mat->val)
		(*mat->notify) (mat->val);
	mat->val = NULL;
	mat->size.rows = 0;
	mat->size.columns = 0;
	mat->notify = g_free;
	while (1) {
		val = g_strtod (end, &end);
		g_array_append_val (values, val);
		if (*end) {
			if (*end == col_sep)
				j++;
			else if (*end == row_sep) {
				if (columns > 0) {
					if (j == columns - 1) {
						i++;
						j = 0;
					} else {
						g_array_free (values, TRUE);
						return FALSE;
					}
				} else {
					columns = j + 1;
					i++;
					j = 0;
				}
			} else {
				g_array_free (values, TRUE);
				return FALSE;
			}
			end++;
		} else
			break;
	}
	if (j != columns - 1) {
		g_array_free (values, TRUE);
		return FALSE;
	}
	if (columns == 0) {
		g_array_free (values, TRUE);
		return TRUE;
	}
	mat->size.columns = columns;
	mat->size.rows = i + 1;
	mat->val = (double*) values->data;
	g_array_free (values, FALSE);
	go_data_emit_changed (GO_DATA (mat));
	return TRUE;
}

static void
go_data_matrix_val_class_init (GObjectClass *gobject_klass)
{
	GODataClass *godata_klass = (GODataClass *) gobject_klass;
	GODataMatrixClass *matrix_klass = (GODataMatrixClass *) gobject_klass;

	matrix_val_parent_klass = g_type_class_peek_parent (gobject_klass);
	gobject_klass->finalize = go_data_matrix_val_finalize;
	godata_klass->dup	= go_data_matrix_val_dup;
	godata_klass->eq	= go_data_matrix_val_eq;
	godata_klass->as_str	= go_data_matrix_val_as_str;
	godata_klass->from_str	= go_data_matrix_val_from_str;
	matrix_klass->load_size    = go_data_matrix_val_load_size;
	matrix_klass->load_values = go_data_matrix_val_load_values;
	matrix_klass->get_value   = go_data_matrix_val_get_value;
	matrix_klass->get_str     = go_data_matrix_val_get_str;
}

GSF_CLASS (GODataMatrixVal, go_data_matrix_val,
	   go_data_matrix_val_class_init, NULL,
	   GO_DATA_MATRIX_TYPE)

GOData *
go_data_matrix_val_new (double *val, unsigned rows, unsigned columns, GDestroyNotify   notify)
{
	GODataMatrixVal *res = g_object_new (GO_DATA_MATRIX_VAL_TYPE, NULL);
	res->val = val;
	res->size.rows = rows;
	res->size.columns = columns;
	res->notify = notify;
	return GO_DATA (res);
}
