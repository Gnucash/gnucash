/* strsub.c -- Do literal line-by line string replacement.
 *
 * Copyright (c) 2003 John H. Pierce <john@killterm.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * See http://www.gnu.org/ for details.
 *
 * This is a very stupid program for the distdep target because I
 * can't figure out an easy way to get sed to not interpret RE chars
 * in the pattern space without complicated and error-prone escaping.
 *
 * Reads stdin line by line and replaces the first occurance of
 * <str1> with <str2> and prints the resulting line to stdout.
 *
 * Lines which do not contain <str1> are printed unchanged.
 */

#define _GNU_SOURCE 1

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>

void do_subst(gchar *, gchar *);
gchar * getwholeline(FILE *);
void useage(void);

void
useage(void)
{
	fprintf(stderr,
		"Useage: strsub <str1> <str2>\n"
		"  Replaces the first occurance of str1 with str2 in each line.\n");
	exit(EXIT_FAILURE);
}

int
main(int argc, char **argv)
{
	gchar *str1 = NULL;
	gchar *str2 = NULL;

	if (argc != 3)
		useage();

	if (argv + 1)
		str1 = (gchar *)*(argv + 1);

	if (argv + 2)
		str2 = (gchar *)*(argv + 2);

	if (!str1 || !str2)
		useage();

	do_subst(str1, str2);

	exit(EXIT_SUCCESS);
}

void
do_subst(gchar *s1, gchar *s2)
{
	gchar *inln;
	gchar **split;
	gchar *jstr;

	while ((inln = getwholeline(stdin)))
	{
		split = g_strsplit(inln, s1, 1);
		g_assert(split != NULL);
		jstr = g_strjoinv(s2, split);
		printf(jstr);
		g_free(jstr);
		g_free(inln);
		g_strfreev(split);
	}
}

/* read a whole line that may be really big.  free return w/g_free() */
gchar *
getwholeline(FILE *infile)
{
	GStringChunk *gs;
	size_t retlen;
	const gint strsize = 100;
	gchar tmp[100];
	gchar *ostr = NULL;
	gchar *ret;

	if ((gs = g_string_chunk_new(strsize)))
	{
		do
		{
			if(!fgets((char*)tmp, strsize, infile))
				break;

			ostr = g_string_chunk_insert(gs, tmp);

			retlen = strlen(ostr);
		} while (retlen - 1 >= 0 && *(ostr + retlen - 1) != '\n');
	}
	
	
	ret = g_strdup(ostr);
	g_string_chunk_free(gs);
	return ret;
}

