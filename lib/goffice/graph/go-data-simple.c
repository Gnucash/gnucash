/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-data-simple.c : 
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */
#include <goffice/goffice-config.h>
#include <goffice/graph/go-data-simple.h>
#include <goffice/graph/go-data-impl.h>
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
	double const	*val;
};
typedef GODataVectorClass GODataVectorValClass;

static GObjectClass *vector_val_parent_klass;

static void
go_data_vector_val_finalize (GObject *obj)
{
	/* GODataVectorVal *val = (GODataVectorVal *)obj; */

	(*vector_val_parent_klass->finalize) (obj);
}

static GOData *
go_data_vector_val_dup (GOData const *src)
{
	GODataVectorVal *dst = g_object_new (G_OBJECT_TYPE (src), NULL);
	GODataVectorVal const *src_val = (GODataVectorVal const *)src;
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
	return vec->values[i];
}
static char *
go_data_vector_val_get_str (GODataVector *vec, unsigned i)
{
	return g_strdup_printf ("%g", vec->values[i]);
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
	godata_klass->as_str	= NULL;
	godata_klass->from_str	= NULL;
	vector_klass->load_len    = go_data_vector_val_load_len;
	vector_klass->load_values = go_data_vector_val_load_values;
	vector_klass->get_value   = go_data_vector_val_get_value;
	vector_klass->get_str     = go_data_vector_val_get_str;
}

GSF_CLASS (GODataVectorVal, go_data_vector_val,
	   go_data_vector_val_class_init, NULL,
	   GO_DATA_VECTOR_TYPE)

GOData *
go_data_vector_val_new (double const *val, unsigned n)
{
	GODataVectorVal *res = g_object_new (GO_DATA_VECTOR_VAL_TYPE, NULL);
	res->val = val;
	res->n = n;
	return GO_DATA (res);
}

/*****************************************************************************/

struct _GODataVectorStr {
	GODataVector	 base;
	char const * const *str;
	unsigned n;

	GOTranslateFunc translate_func;
	gpointer        translate_data;
	GDestroyNotify  translate_notify;   
};
typedef GODataVectorClass GODataVectorStrClass;

static GObjectClass *vector_str_parent_klass;

static void
go_data_vector_str_finalize (GObject *obj)
{
	/* GODataVectorStr *str = (GODataVectorStr *)obj; */

	(*vector_str_parent_klass->finalize) (obj);
}

static GOData *
go_data_vector_str_dup (GOData const *src)
{
	GODataVectorStr *dst = g_object_new (G_OBJECT_TYPE (src), NULL);
	GODataVectorStr const *src_val = (GODataVectorStr const *)src;
	dst->n = src_val->n;
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

static void
go_data_vector_str_load_len (GODataVector *vec)
{
	vec->base.flags |= GO_DATA_VECTOR_LEN_CACHED;
	vec->len = ((GODataVectorStr *)vec)->n;
}
static void
go_data_vector_str_load_values (GODataVector *vec)
{
}
static double
go_data_vector_str_get_value (GODataVector *vec, unsigned i)
{
	return go_nan;
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
	godata_klass->as_str	= NULL;
	godata_klass->from_str	= NULL;
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
	str->translate_func = NULL;
	str->translate_data = NULL;
	str->translate_notify = NULL;
}

GSF_CLASS (GODataVectorStr, go_data_vector_str,
	   go_data_vector_str_class_init, go_data_vector_str_init,
	   GO_DATA_VECTOR_TYPE)

GOData *
go_data_vector_str_new (char const * const *str, unsigned n)
{
	GODataVectorStr *res = g_object_new (GO_DATA_VECTOR_STR_TYPE, NULL);
	res->str = str;
	res->n	 = n;
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
