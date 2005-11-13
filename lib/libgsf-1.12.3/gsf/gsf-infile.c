/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-infile.c :
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
#include <gsf/gsf-infile-impl.h>
#include <gsf/gsf-impl-utils.h>

#include <stdarg.h>

#define GET_CLASS(instance) G_TYPE_INSTANCE_GET_CLASS (instance, GSF_INFILE_TYPE, GsfInfileClass)

/**
 * gsf_infile_num_children :
 * @infile : the structured storage
 *
 * Returns the number of children the storage has, or -1 if the storage can not
 * 	have children.
 **/
int
gsf_infile_num_children (GsfInfile *infile)
{
	g_return_val_if_fail (infile != NULL, -1);

	return GET_CLASS (infile)->num_children (infile);
}

/**
 * gsf_infile_name_by_index :
 * @infile :
 * @i      :
 *
 * Returns the utf8 encoded name of the @i-th child
 * NOTE : DO NOT FREE THE STRING
 **/
char const *
gsf_infile_name_by_index (GsfInfile *infile, int i)
{
	g_return_val_if_fail (infile != NULL, NULL);

	return GET_CLASS (infile)->name_by_index (infile, i);
}

/**
 * gsf_infile_child_by_index :
 * @infile :
 * @i :
 *
 * TODO : For 2.0 api will change to include a GError.
 * Returns a newly created child which must be unrefed.
 **/
GsfInput *
gsf_infile_child_by_index (GsfInfile *infile, int i)
{
	GError *err = NULL;
	GsfInput *res;

	g_return_val_if_fail (GSF_INFILE (infile) != NULL, NULL);

	res = GET_CLASS (infile)->child_by_index (infile, i, &err);

	if (err != NULL) {
		char const *iname = gsf_input_name (GSF_INPUT (infile));
		g_warning ("Unable to get child[%d] for infile '%s' because : %s",
			   i, iname ? iname : "?", err->message);
		g_error_free (err);
		g_return_val_if_fail (res == NULL, NULL); /* be anal */
	}

	return res;
}

/**
 * gsf_infile_child_by_name :
 * @infile :
 * @name :
 *
 * TODO : For 2.0 api will change to include a GError.
 * Returns a newly created child which must be unrefed.
 **/
GsfInput *
gsf_infile_child_by_name (GsfInfile *infile, char const *name)
{
	GError *err = NULL;
	GsfInput *res;

	g_return_val_if_fail (GSF_INFILE (infile) != NULL, NULL);
	g_return_val_if_fail (name != NULL, NULL);

	res = GET_CLASS (infile)->child_by_name (infile, name, &err);

	if (err != NULL) {
		char const *iname = gsf_input_name (GSF_INPUT (infile));
		g_warning ("Unable to get child['%s'] for infile '%s' because : %s",
			   name, iname ? iname : "?", err->message);
		g_error_free (err);
		g_return_val_if_fail (res == NULL, NULL); /* be anal */
	}

	return res;
}

/**
 * gsf_infile_child_by_vname :
 * @infile :
 * @name : A %null terminated list of names
 * @Varargs : the rest of the names
 *
 * Returns a newly created child which must be unrefed.
 **/
GsfInput *
gsf_infile_child_by_vname (GsfInfile *infile, char const *name, ...)
{
	va_list   ap;
	GsfInput  *child = GSF_INPUT (infile);
	GsfInfile *tmp = NULL;

	g_return_val_if_fail (GSF_IS_INFILE (infile), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	va_start (ap, name);
	while (1) {
		child = gsf_infile_child_by_name (infile, name);

		name = va_arg (ap, char *);
		if (tmp != NULL)
			g_object_unref (G_OBJECT (tmp));
		if (name == NULL)
			break;
		if (child == NULL)
			break;

		g_return_val_if_fail (GSF_IS_INFILE (child), NULL);

		infile = tmp = GSF_INFILE (child);
	}
	va_end (ap);

	return child;
}

GSF_CLASS_ABSTRACT (GsfInfile, gsf_infile, NULL, NULL, GSF_INPUT_TYPE)
