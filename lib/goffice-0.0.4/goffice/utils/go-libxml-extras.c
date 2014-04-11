/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-libxml-extras.c: stuff that should have been in libxml2.
 *
 * Authors:
 *   Daniel Veillard <Daniel.Veillard@w3.org>
 *   Miguel de Icaza <miguel@gnu.org>
 *   Jody Goldberg <jody@gnome.org>
 *   Jukka-Pekka Iivonen <jiivonen@hutcs.cs.hut.fi>
 */
#include <goffice/goffice-config.h>
#include "go-libxml-extras.h"
#include "go-color.h"

#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <math.h>

#define CC2XML(s) ((xmlChar const *)(s))
#define CXML2C(s) ((char const *)(s))


/**
 * Like xmlParseFile, but faster.  Does not accept compressed files.
 * See http://bugzilla.gnome.org/show_bug.cgi?id=168414
 *
 * Note: this reads the entire file into memory and should therefore
 * not be used for user-supplied files.
 **/
xmlDocPtr
go_xml_parse_file (char const *filename)
{
	xmlDocPtr result = NULL;
	gchar *contents;
	gsize length;

	if (g_file_get_contents (filename, &contents, &length, NULL)) {
		result = xmlParseMemory (contents, length);
		g_free (contents);
	}

	return result;
}

/* Get an xmlChar * value for a node carried as an attibute
 * result must be xmlFree
 */
xmlChar *
xml_node_get_cstr (xmlNodePtr node, char const *name)
{
	if (name != NULL)
		return xmlGetProp (node, CC2XML (name));
	/* in libxml1 <foo/> would return NULL
	 * in libxml2 <foo/> would return ""
	 */
	if (node->xmlChildrenNode != NULL)
		return xmlNodeGetContent (node);
	return NULL;
}
void
xml_node_set_cstr (xmlNodePtr node, char const *name, char const *val)
{
	if (name)
		xmlSetProp (node, CC2XML (name), CC2XML (val));
	else
		xmlNodeSetContent (node, CC2XML (val));
}

gboolean
xml_node_get_bool (xmlNodePtr node, char const *name, gboolean *val)
{
	xmlChar *buf = xml_node_get_cstr (node, name);
	if (buf == NULL)
		return FALSE;

	*val = (!strcmp (CXML2C (buf), "1")
		|| 0 == g_ascii_strcasecmp (CXML2C (buf), "true"));
	g_free (buf);
	return TRUE;
}

void
xml_node_set_bool (xmlNodePtr node, char const *name, gboolean val)
{
	xml_node_set_cstr (node, name, val ? "true" : "false");
}

gboolean
xml_node_get_int (xmlNodePtr node, char const *name, int *val)
{
	xmlChar *buf;
	char *end;
	gboolean ok;
	long l;

	buf = xml_node_get_cstr (node, name);
	if (buf == NULL)
		return FALSE;

	errno = 0; /* strto(ld) sets errno, but does not clear it.  */
	*val = l = strtol (CXML2C (buf), &end, 10);
	ok = (CXML2C (buf) != end) && *end == 0 && errno != ERANGE && (*val == l);
	xmlFree (buf);

	return ok;
}

void
xml_node_set_int (xmlNodePtr node, char const *name, int val)
{
	char str[4 * sizeof (int)];
	sprintf (str, "%d", val);
	xml_node_set_cstr (node, name, str);
}

gboolean
xml_node_get_double (xmlNodePtr node, char const *name, double *val)
{
	xmlChar *buf;
	char *end;
	gboolean ok;

	buf = xml_node_get_cstr (node, name);
	if (buf == NULL)
		return FALSE;

	errno = 0; /* strto(ld) sets errno, but does not clear it.  */
	*val = strtod (CXML2C (buf), &end);
	ok = (CXML2C (buf) != end) && *end == 0 && errno != ERANGE;
	xmlFree (buf);

	return ok;
}

void
xml_node_set_double (xmlNodePtr node, char const *name, double val,
		     int precision)
{
	char str[101 + DBL_DIG];

	if (precision < 0 || precision > DBL_DIG)
		precision = DBL_DIG;

	if (fabs (val) < 1e9 && fabs (val) > 1e-5)
		g_snprintf (str, 100 + DBL_DIG, "%.*g", precision, val);
	else
		g_snprintf (str, 100 + DBL_DIG, "%f", val);

	xml_node_set_cstr (node, name, str);
}


gboolean
xml_node_get_gocolor (xmlNodePtr node, char const *name, GOColor *res)
{
	xmlChar *color;
	int r, g, b;

	color = xmlGetProp (node, CC2XML (name));
	if (color == NULL)
		return FALSE;
	if (sscanf (CXML2C (color), "%X:%X:%X", &r, &g, &b) == 3) {
		r >>= 8;
		g >>= 8;
		b >>= 8;
		*res = RGBA_TO_UINT (r,g,b,0xff);
		xmlFree (color);
		return TRUE;
	}
	xmlFree (color);
	return FALSE;
}

void
xml_node_set_gocolor (xmlNodePtr node, char const *name, GOColor val)
{
	unsigned r, g, b;
	char str[4 * sizeof (val)];

	UINT_TO_RGB (val, &r, &g, &b);
	sprintf (str, "%X:%X:%X", r, g, b);
	xml_node_set_cstr (node, name, str);
}

gboolean
xml_node_get_enum (xmlNodePtr node, char const *name, GType etype, gint *val)
{
	GEnumClass *eclass = G_ENUM_CLASS (g_type_class_peek (etype));
	GEnumValue *ev;
	xmlChar *s;
	int i;

	s = xmlGetProp (node, CC2XML (name));
	if (s == NULL)
		return FALSE;

	ev = g_enum_get_value_by_name (eclass, CXML2C (s));
	if (!ev) ev = g_enum_get_value_by_nick (eclass, CXML2C (s));
	if (!ev && xml_node_get_int (node, name, &i))
		/* Check that the value is valid.  */
		ev = g_enum_get_value (eclass, i);
	xmlFree (s);
	if (!ev) return FALSE;

	*val = ev->value;
	return TRUE;
}

void
xml_node_set_enum (xmlNodePtr node, char const *name, GType etype, gint val)
{
	GEnumClass *eclass = G_ENUM_CLASS (g_type_class_peek (etype));
	GEnumValue *ev = g_enum_get_value (eclass, val);

	if (ev)
		xml_node_set_cstr (node, name, ev->value_name);
	else
		g_warning ("Invalid value %d for type %s",
			   val, g_type_name (etype));
}

/*************************************************************************/

xmlNode *
e_xml_get_child_by_name (xmlNode const *parent, char const *child_name)
{
	xmlNode *child;

	g_return_val_if_fail (parent != NULL, NULL);
	g_return_val_if_fail (child_name != NULL, NULL);

	for (child = parent->xmlChildrenNode; child != NULL; child = child->next) {
		if (xmlStrcmp (child->name, CC2XML (child_name)) == 0) {
			return child;
		}
	}
	return NULL;
}

xmlNode *
e_xml_get_child_by_name_no_lang (xmlNode const *parent, char const *name)
{
	xmlNodePtr node;

	g_return_val_if_fail (parent != NULL, NULL);
	g_return_val_if_fail (name != NULL, NULL);

	for (node = parent->xmlChildrenNode; node != NULL; node = node->next) {
		xmlChar *lang;

		if (node->name == NULL || strcmp (CXML2C (node->name), name) != 0) {
			continue;
		}
		lang = xmlGetProp (node, CC2XML ("xml:lang"));
		if (lang == NULL) {
			return node;
		}
		xmlFree (lang);
	}

	return NULL;
}


xmlNode *
e_xml_get_child_by_name_by_lang (xmlNode const *parent, gchar const *name)
{
	xmlNodePtr   best_node = NULL, node;
	gint         best_lang_score = INT_MAX;
	char const * const *langs = g_get_language_names ();

	g_return_val_if_fail (parent != NULL, NULL);
	g_return_val_if_fail (name != NULL, NULL);

	for (node = parent->xmlChildrenNode; node != NULL; node = node->next) {
		xmlChar *lang;

		if (node->name == NULL || strcmp (CXML2C (node->name), name) != 0)
			continue;

		lang = xmlGetProp (node, CC2XML ("xml:lang"));
		if (lang != NULL) {
			gint i;

			for (i = 0; langs[i] != NULL && i < best_lang_score; i++) {
				if (strcmp (langs[i], CXML2C (lang)) == 0) {
					best_node = node;
					best_lang_score = i;
				}
			}
		} else if (best_node == NULL)
			best_node = node;

		xmlFree (lang);
		if (best_lang_score == 0) 
			return best_node;
	}

	return best_node;
}
