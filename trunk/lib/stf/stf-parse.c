/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * stf-parse.c : Structured Text Format parser. (STF)
 *               A general purpose engine for parsing data
 *               in CSV and Fixed width format.
 *
 *
 * Copyright (C) Almer. S. Tigelaar.
 * EMail: almer1@dds.nl or almer-t@bigfoot.com
 *
 * Copyright (C) 2003 Andreas J. Guelzow <aguelzow@taliesin.ca>
 * Copyright (C) 2003 Morten Welinder <terra@gnome.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#define GETTEXT_PACKAGE gnumeric

#include <glib/gi18n-lib.h>
/* #include "gnumeric.h" */
#include "stf-parse.h"

/* #include "workbook.h" */
/* #include "cell.h" */
/* #include "sheet.h" */
/* #include "clipboard.h" */
/* #include "sheet-style.h" */
/* #include "value.h" */
/* #include "mstyle.h" */
/* #include "number-match.h" */
/* #include "gutils.h" */
/* #include "parse-util.h" */
#include <goffice/utils/go-glib-extras.h>
#include <goffice/utils/go-format.h>

#include <stdlib.h>
#include <stdio.h>
#include <locale.h>
#include <string.h>
#include <math.h>

#define SETUP_LOCALE_SWITCH char *oldlocale = NULL

#define START_LOCALE_SWITCH if (parseoptions->locale) {\
oldlocale = g_strdup(go_setlocale (LC_ALL, NULL)); \
go_setlocale(LC_ALL, parseoptions->locale);}

#define END_LOCALE_SWITCH if (oldlocale) {\
go_setlocale(LC_ALL, oldlocale);\
g_free (oldlocale);}

/* Source_t struct, used for interchanging parsing information between the low level parse functions */
typedef struct {
	GStringChunk *chunk;
	char const *position;  /* Indicates the current position within data */

	/* Used internally for fixed width parsing */
	int splitpos;          /* Indicates current position in splitpositions array */
	int linepos;           /* Position on the current line */
} Source_t;

/* Struct used for autodiscovery */
typedef struct {
	int start;
	int stop;
} AutoDiscovery_t;

/*
 * Some silly dude make the length field an unsigned int.  C just does
 * not deal very well with that.
 */
static inline int
my_garray_len (GArray const *a)
{
	return (int)a->len;
}

static inline int
my_gptrarray_len (GPtrArray const *a)
{
	return (int)a->len;
}

static int
compare_terminator (char const *s, StfParseOptions_t *parseoptions)
{
	guchar const *us = (guchar const *)s;
	GSList *l;

	if (*us > parseoptions->compiled_terminator.max ||
	    *us < parseoptions->compiled_terminator.min)
		return 0;

	for (l = parseoptions->terminator; l; l = l->next) {
		char const *term = l->data;
		char const *d = s;

		while (*term) {
			if (*d != *term)
				goto next;
			term++;
			d++;
		}
		return d - s;

	next:
		;
	}
	return 0;
}


/*******************************************************************************************************
 * STF PARSE OPTIONS : StfParseOptions related
 *******************************************************************************************************/

/**
 * stf_parse_options_new:
 *
 * This will return a new StfParseOptions_t struct.
 * The struct should, after being used, freed with stf_parse_options_free.
 **/
StfParseOptions_t *
stf_parse_options_new (void)
{
	StfParseOptions_t* parseoptions = g_new0 (StfParseOptions_t, 1);

	parseoptions->parsetype   = PARSE_TYPE_NOTSET;

	parseoptions->terminator  = NULL;
	stf_parse_options_add_line_terminator (parseoptions, "\r\n");
	stf_parse_options_add_line_terminator (parseoptions, "\n");
	stf_parse_options_add_line_terminator (parseoptions, "\r");

	parseoptions->trim_spaces = (TRIM_TYPE_RIGHT | TRIM_TYPE_LEFT);
	parseoptions->locale = NULL;

	parseoptions->splitpositions = NULL;
	stf_parse_options_fixed_splitpositions_clear (parseoptions);

	parseoptions->stringindicator = '"';
	parseoptions->indicator_2x_is_single = TRUE;
	parseoptions->duplicates = FALSE;
	parseoptions->trim_seps = FALSE;

	parseoptions->sep.str = NULL;
	parseoptions->sep.chr = NULL;

	parseoptions->col_import_array = NULL;
	parseoptions->col_import_array_len = 0;
	parseoptions->formats = NULL;

	parseoptions->cols_exceeded = FALSE;

	return parseoptions;
}

/**
 * stf_parse_options_free:
 *
 * will free @parseoptions, note that this will not free the splitpositions
 * member (GArray) of the struct, the caller is responsible for that.
 **/
void
stf_parse_options_free (StfParseOptions_t *parseoptions)
{
	g_return_if_fail (parseoptions != NULL);

	g_free (parseoptions->col_import_array);
	g_free (parseoptions->locale);
	g_free (parseoptions->sep.chr);

	if (parseoptions->sep.str) {
		GSList *l;

		for (l = parseoptions->sep.str; l != NULL; l = l->next)
			g_free ((char *) l->data);
		g_slist_free (parseoptions->sep.str);
	}

	g_array_free (parseoptions->splitpositions, TRUE);

	stf_parse_options_clear_line_terminator (parseoptions);

	if (parseoptions->formats) {
		unsigned int ui;
		GPtrArray *formats = parseoptions->formats;

		for (ui = 0; ui < formats->len; ui++)
			go_format_unref (g_ptr_array_index (formats, ui));
		g_ptr_array_free (formats, TRUE);
		parseoptions->formats = NULL;
	}

	g_free (parseoptions);
}

void
stf_parse_options_set_type (StfParseOptions_t *parseoptions, StfParseType_t const parsetype)
{
	g_return_if_fail (parseoptions != NULL);
	g_return_if_fail (parsetype == PARSE_TYPE_CSV || parsetype == PARSE_TYPE_FIXED);

	parseoptions->parsetype = parsetype;
}

static gint
long_string_first (gchar const *a, gchar const *b)
{
	/* This actually is UTF-8 safe.  */
	return strlen (b) - strlen (a);
}

static void
compile_terminators (StfParseOptions_t *parseoptions)
{
	GSList *l;
	GO_SLIST_SORT (parseoptions->terminator, (GCompareFunc)long_string_first);

	parseoptions->compiled_terminator.min = 255;
	parseoptions->compiled_terminator.max = 0;
	for (l = parseoptions->terminator; l; l = l->next) {
		const guchar *term = l->data;
		parseoptions->compiled_terminator.min =
			MIN (parseoptions->compiled_terminator.min, *term);
		parseoptions->compiled_terminator.max =
			MAX (parseoptions->compiled_terminator.max, *term);
	}
}

/**
 * stf_parse_options_add_line_terminator:
 *
 * This will add to the line terminators, in both the Fixed width and CSV delimited importers
 * this indicates the end of a row.
 *
 **/
void
stf_parse_options_add_line_terminator (StfParseOptions_t *parseoptions, char const *terminator)
{
	g_return_if_fail (parseoptions != NULL);
	g_return_if_fail (terminator != NULL && *terminator != 0);

	GO_SLIST_PREPEND (parseoptions->terminator, g_strdup (terminator));
	compile_terminators (parseoptions);
}

/**
 * stf_parse_options_clear_line_terminator:
 *
 * This will clear the line terminator, in both the Fixed width and CSV delimited importers
 * this indicates the end of a row.
 *
 **/
void
stf_parse_options_clear_line_terminator (StfParseOptions_t *parseoptions)
{
	g_return_if_fail (parseoptions != NULL);

	go_slist_free_custom (parseoptions->terminator, g_free);
	parseoptions->terminator = NULL;
	compile_terminators (parseoptions);
}

/**
 * stf_parse_options_set_trim_spaces:
 *
 * If enabled will trim spaces in every parsed field on left and/or right
 * sides.
 **/
void
stf_parse_options_set_trim_spaces (StfParseOptions_t *parseoptions, StfTrimType_t const trim_spaces)
{
	g_return_if_fail (parseoptions != NULL);

	parseoptions->trim_spaces = trim_spaces;
}

/**
 * stf_parse_options_csv_set_separators:
 *
 * A copy is made of the parameters.
 **/
void
stf_parse_options_csv_set_separators (StfParseOptions_t *parseoptions, char const *character,
				      GSList const *string)
{
	g_return_if_fail (parseoptions != NULL);

	g_free (parseoptions->sep.chr);
	parseoptions->sep.chr = g_strdup (character);

	go_slist_free_custom (parseoptions->sep.str, g_free);
	parseoptions->sep.str = go_slist_map (string, (GOMapFunc)g_strdup);
}

void
stf_parse_options_csv_set_stringindicator (StfParseOptions_t *parseoptions, gunichar const stringindicator)
{
	g_return_if_fail (parseoptions != NULL);
	g_return_if_fail (stringindicator != '\0');

	parseoptions->stringindicator = stringindicator;
}

/**
 * stf_parse_options_csv_set_indicator_2x_is_single:
 * @indic_2x : a boolean value indicating whether we want to see two
 * 		adjacent string indicators as a single string indicator
 * 		that is part of the cell, rather than a terminator.
 **/
void
stf_parse_options_csv_set_indicator_2x_is_single (StfParseOptions_t *parseoptions,
						  gboolean const indic_2x)
{
	g_return_if_fail (parseoptions != NULL);

	parseoptions->indicator_2x_is_single = indic_2x;
}

/**
 * stf_parse_options_csv_set_duplicates:
 * @duplicates : a boolean value indicating whether we want to see two
 *               separators right behind each other as one
 **/
void
stf_parse_options_csv_set_duplicates (StfParseOptions_t *parseoptions, gboolean const duplicates)
{
	g_return_if_fail (parseoptions != NULL);

	parseoptions->duplicates = duplicates;
}

/**
 * stf_parse_options_csv_set_trim_seps:
 * @trim_seps : a boolean value indicating whether we want to ignore
 *               separators at the beginning of lines
 **/
void
stf_parse_options_csv_set_trim_seps (StfParseOptions_t *parseoptions, gboolean const trim_seps)
{
	g_return_if_fail (parseoptions != NULL);

	parseoptions->trim_seps = trim_seps;
}

/**
 * stf_parse_options_fixed_splitpositions_clear:
 *
 * This will clear the splitpositions (== points on which a line is split)
 **/
void
stf_parse_options_fixed_splitpositions_clear (StfParseOptions_t *parseoptions)
{
	int minus_one = -1;
	g_return_if_fail (parseoptions != NULL);

	if (parseoptions->splitpositions)
		g_array_free (parseoptions->splitpositions, TRUE);
	parseoptions->splitpositions = g_array_new (FALSE, FALSE, sizeof (int));

	g_array_append_val (parseoptions->splitpositions, minus_one);
}

/**
 * stf_parse_options_fixed_splitpositions_add:
 *
 * @position will be added to the splitpositions.
 **/
void
stf_parse_options_fixed_splitpositions_add (StfParseOptions_t *parseoptions, int position)
{
	unsigned int ui;

	g_return_if_fail (parseoptions != NULL);
	g_return_if_fail (position >= 0);

	for (ui = 0; ui < parseoptions->splitpositions->len - 1; ui++) {
		int here = g_array_index (parseoptions->splitpositions, int, ui);
		if (position == here)
			return;
		if (position < here)
			break;
	}

	g_array_insert_val (parseoptions->splitpositions, ui, position);
}

void
stf_parse_options_fixed_splitpositions_remove (StfParseOptions_t *parseoptions, int position)
{
	unsigned int ui;

	g_return_if_fail (parseoptions != NULL);
	g_return_if_fail (position >= 0);

	for (ui = 0; ui < parseoptions->splitpositions->len - 1; ui++) {
		int here = g_array_index (parseoptions->splitpositions, int, ui);
		if (position == here)
			g_array_remove_index (parseoptions->splitpositions, ui);
		if (position <= here)
			return;
	}
}

int
stf_parse_options_fixed_splitpositions_count (StfParseOptions_t *parseoptions)
{
	return parseoptions->splitpositions->len;
}

int
stf_parse_options_fixed_splitpositions_nth (StfParseOptions_t *parseoptions, int n)
{
	return g_array_index (parseoptions->splitpositions, int, n);
}


/**
 * stf_parse_options_valid:
 * @parseoptions : an import options struct
 *
 * Checks if @parseoptions is correctly filled
 *
 * returns : TRUE if it is correctly filled, FALSE otherwise.
 **/
static gboolean
stf_parse_options_valid (StfParseOptions_t *parseoptions)
{
	g_return_val_if_fail (parseoptions != NULL, FALSE);

	if (parseoptions->parsetype == PARSE_TYPE_CSV) {
		if (parseoptions->stringindicator == '\0') {
			g_warning ("STF: Cannot have \\0 as string indicator");
			return FALSE;
		}

	} else if (parseoptions->parsetype == PARSE_TYPE_FIXED) {
		if (!parseoptions->splitpositions) {
			g_warning ("STF: No splitpositions in struct");
			return FALSE;
		}
	}

	return TRUE;
}

/*******************************************************************************************************
 * STF PARSE : The actual routines that do the 'trick'
 *******************************************************************************************************/

static void
trim_spaces_inplace (char *field, StfParseOptions_t const *parseoptions)
{
	if (!field) return;

	if (parseoptions->trim_spaces & TRIM_TYPE_LEFT) {
		char *s = field;

		while (g_unichar_isspace (g_utf8_get_char (s)))
			s = g_utf8_next_char (s);

		if (s != field)
			strcpy (field, s);
	}

	if (parseoptions->trim_spaces & TRIM_TYPE_RIGHT) {
		char *s = field + strlen (field);

		while (field != s) {
			s = g_utf8_prev_char (s);
			if (!g_unichar_isspace (g_utf8_get_char (s)))
				break;
			*s = 0;
		}
	}
}

/**
 * stf_parse_csv_is_separator:
 *
 * returns NULL if @character is not a separator, a pointer to the character
 * after the separator otherwise.
 **/
static char const *
stf_parse_csv_is_separator (char const *character, char const *chr, GSList const *str)
{
	g_return_val_if_fail (character != NULL, NULL);

	if (*character == 0)
		return NULL;

	if (str) {
		GSList const *l;

		for (l = str; l != NULL; l = l->next) {
			char const *s = l->data;
			char const *r;
			glong cnt;
			glong const len = g_utf8_strlen (s, -1);

			/* Don't compare past the end of the buffer! */
			for (r = character, cnt = 0; cnt < len; cnt++, r = g_utf8_next_char (r))
				if (*r == '\0')
					break;

			if ((cnt == len) && (memcmp (character, s, len) == 0))
				return g_utf8_offset_to_pointer (character, len);
		}
	}

	if (chr && g_utf8_strchr (chr, -1,
				  g_utf8_get_char (character)))
		return g_utf8_next_char(character);

	return NULL;
}

/*
 * stf_parse_eat_separators:
 *
 * skip over leading separators
 *
 */

static void
stf_parse_eat_separators (Source_t *src, StfParseOptions_t *parseoptions)
{
	char const *cur, *next;

	g_return_if_fail (src != NULL);
        g_return_if_fail (parseoptions != NULL);

	cur = src->position;

	if (*cur == '\0' || compare_terminator (cur, parseoptions))
		return;
	while ((next = stf_parse_csv_is_separator (cur, parseoptions->sep.chr, parseoptions->sep.str)))
		cur = next;
	src->position = cur;
	return;
}


typedef enum {
	STF_CELL_ERROR,
	STF_CELL_EOF,
	STF_CELL_EOL,
	STF_CELL_FIELD_NO_SEP,
	STF_CELL_FIELD_SEP,
} StfParseCellRes;

static StfParseCellRes
stf_parse_csv_cell (GString *text, Source_t *src, StfParseOptions_t *parseoptions)
{
	char const *cur;
	gboolean saw_sep = FALSE;

	g_return_val_if_fail (src != NULL, STF_CELL_ERROR);
	g_return_val_if_fail (parseoptions != NULL, STF_CELL_ERROR);

	cur = src->position;
	g_return_val_if_fail (cur != NULL, STF_CELL_ERROR);

	/* Skip whitespace, but stop at line terminators.  */
	while (1) {
		int term_len;

		if (*cur == 0) {
			src->position = cur;
			return STF_CELL_EOF;
		}

		term_len = compare_terminator (cur, parseoptions);
		if (term_len) {
			src->position = cur + term_len;
			return STF_CELL_EOL;
		}

		if ((parseoptions->trim_spaces & TRIM_TYPE_LEFT) == 0)
			break;

		if (stf_parse_csv_is_separator (cur, parseoptions->sep.chr,
						parseoptions->sep.str))
			break;

		if (!g_unichar_isspace (g_utf8_get_char (cur)))
			break;
		cur = g_utf8_next_char (cur);
	}

	if (g_utf8_get_char (cur) == parseoptions->stringindicator) {
		cur = g_utf8_next_char (cur);
		while (*cur) {
			gunichar uc = g_utf8_get_char (cur);
			cur = g_utf8_next_char (cur);

			if (uc == parseoptions->stringindicator) {
				if (parseoptions->indicator_2x_is_single &&
				    g_utf8_get_char (cur) == parseoptions->stringindicator)
					cur = g_utf8_next_char (cur);
				else {
					/* "field content"dropped-garbage,  */
					while (*cur && !compare_terminator (cur, parseoptions)) {
						char const *post = stf_parse_csv_is_separator
							(cur, parseoptions->sep.chr, parseoptions->sep.str);
						if (post) {
							cur = post;
							saw_sep = TRUE;
							break;
						}
						cur = g_utf8_next_char (cur);
					}
					break;
				}
			}

			g_string_append_unichar (text, uc);
		}

		/* We silently allow a missing terminating quote.  */
	} else {
		/* Unquoted field.  */

		while (*cur && !compare_terminator (cur, parseoptions)) {

			char const *post = stf_parse_csv_is_separator
				(cur, parseoptions->sep.chr, parseoptions->sep.str);
			if (post) {
				cur = post;
				saw_sep = TRUE;
				break;
			}

			g_string_append_unichar (text, g_utf8_get_char (cur));
			cur = g_utf8_next_char (cur);
		}

		if (parseoptions->trim_spaces & TRIM_TYPE_RIGHT) {
			while (text->len) {
				const char *last = g_utf8_prev_char (text->str + text->len);
				if (!g_unichar_isspace (g_utf8_get_char (last)))
					break;
				g_string_truncate (text, last - text->str);
			}
		}
	}

	src->position = cur;

	if (saw_sep && parseoptions->duplicates)
		stf_parse_eat_separators (src, parseoptions);

	return saw_sep ? STF_CELL_FIELD_SEP : STF_CELL_FIELD_NO_SEP;
}

/**
 * stf_parse_csv_line:
 *
 * This will parse one line from the current @src->position.
 * NOTE: The calling routine is responsible for freeing the result.
 *
 * returns : a GPtrArray of char*'s
 **/
static GPtrArray *
stf_parse_csv_line (Source_t *src, StfParseOptions_t *parseoptions)
{
	GPtrArray *line;
	gboolean cont = FALSE;

	g_return_val_if_fail (src != NULL, NULL);
	g_return_val_if_fail (parseoptions != NULL, NULL);

	line = g_ptr_array_new ();
	if (parseoptions->trim_seps)
		stf_parse_eat_separators (src, parseoptions);

	while (1) {
		GString *text = g_string_sized_new (30);
		StfParseCellRes res =
			stf_parse_csv_cell (text, src, parseoptions);
		trim_spaces_inplace (text->str, parseoptions);
		switch (res) {
		case STF_CELL_FIELD_NO_SEP:
			g_ptr_array_add (line, g_string_free (text, FALSE));
			cont = FALSE;
			break;

		case STF_CELL_FIELD_SEP:
			g_ptr_array_add (line, g_string_free (text, FALSE));
			cont = TRUE;  /* Make sure we see one more field.  */
			break;

		default:
			if (cont)
				g_ptr_array_add (line, g_string_free (text, FALSE));
			else
				g_string_free (text, TRUE);
			return line;
		}
	}
}

/**
 * stf_parse_fixed_cell:
 *
 * returns a pointer to the parsed cell contents.
 **/
static char *
stf_parse_fixed_cell (Source_t *src, StfParseOptions_t *parseoptions)
{
	char *res;
	char const *cur;
	int splitval;

	g_return_val_if_fail (src != NULL, NULL);
	g_return_val_if_fail (parseoptions != NULL, NULL);

	cur = src->position;

	if (src->splitpos < my_garray_len (parseoptions->splitpositions))
		splitval = (int) g_array_index (parseoptions->splitpositions, int, src->splitpos);
	else
		splitval = -1;

	while (*cur != 0 && !compare_terminator (cur, parseoptions) && splitval != src->linepos) {
		src->linepos++;
	        cur = g_utf8_next_char (cur);
	}

	res = g_string_chunk_insert_len (src->chunk,
					 src->position,
					 cur - src->position);

	src->position = cur;

	return res;
}

/**
 * stf_parse_fixed_line:
 *
 * This will parse one line from the current @src->position.
 * It will return a GPtrArray with the cell contents as strings.

 * NOTE: The calling routine is responsible for freeing result.
 **/
static GPtrArray *
stf_parse_fixed_line (Source_t *src, StfParseOptions_t *parseoptions)
{
	GPtrArray *line;

	g_return_val_if_fail (src != NULL, NULL);
	g_return_val_if_fail (parseoptions != NULL, NULL);

	src->linepos = 0;
	src->splitpos = 0;

	line = g_ptr_array_new ();
	while (*src->position != '\0' && !compare_terminator (src->position, parseoptions)) {
		char *field = stf_parse_fixed_cell (src, parseoptions);

		trim_spaces_inplace (field, parseoptions);
		g_ptr_array_add (line, field);

		src->splitpos++;
	}

	return line;
}

void
stf_parse_general_free (GPtrArray *lines)
{
	unsigned lineno;
	for (lineno = 0; lineno < lines->len; lineno++) {
		GPtrArray *line = g_ptr_array_index (lines, lineno);
		/* Fields are not free here.  */
		g_ptr_array_free (line, TRUE);
	}
	g_ptr_array_free (lines, TRUE);
}


/**
 * stf_parse_general:
 *
 * Returns a GPtrArray of lines, where each line is itself a
 * GPtrArray of strings.
 *
 * The caller must free this entire structure, for example by calling
 * stf_parse_general_free.
 **/
GPtrArray *
stf_parse_general (StfParseOptions_t *parseoptions,
		   GStringChunk *lines_chunk,
		   char const *data, char const *data_end)
{
	GPtrArray *lines;
	Source_t src;
	int row;

	g_return_val_if_fail (parseoptions != NULL, NULL);
	g_return_val_if_fail (data != NULL, NULL);
	g_return_val_if_fail (data_end != NULL, NULL);
	g_return_val_if_fail (stf_parse_options_valid (parseoptions), NULL);
	g_return_val_if_fail (g_utf8_validate (data, -1, NULL), NULL);

	src.chunk = lines_chunk;
	src.position = data;
	row = 0;

	lines = g_ptr_array_new ();
	while (*src.position != '\0' && src.position < data_end) {
		GPtrArray *line;

		line = parseoptions->parsetype == PARSE_TYPE_CSV
			? stf_parse_csv_line (&src, parseoptions)
			: stf_parse_fixed_line (&src, parseoptions);

		g_ptr_array_add (lines, line);
		if (parseoptions->parsetype != PARSE_TYPE_CSV)
			src.position += compare_terminator (src.position, parseoptions);

		if (++row == SHEET_MAX_ROWS)
			break;
	}

	return lines;
}

GPtrArray *
stf_parse_lines (StfParseOptions_t *parseoptions,
		 GStringChunk *lines_chunk,
		 char const *data,
		 int maxlines, gboolean with_lineno)
{
	GPtrArray *lines;
	int lineno = 1;

	g_return_val_if_fail (data != NULL, NULL);

	lines = g_ptr_array_new ();
	while (*data) {
		char const *data0 = data;
		GPtrArray *line = g_ptr_array_new ();

		if (with_lineno) {
			char buf[4 * sizeof (int)];
			sprintf (buf, "%d", lineno);
			g_ptr_array_add (line,
					 g_string_chunk_insert (lines_chunk, buf));
		}

		while (1) {
			int termlen = compare_terminator (data, parseoptions);
			if (termlen > 0 || *data == 0) {
				g_ptr_array_add (line,
						 g_string_chunk_insert_len (lines_chunk,
									    data0,
									    data - data0));
				data += termlen;
				break;
			} else
				data = g_utf8_next_char (data);
		}

		g_ptr_array_add (lines, line);

		lineno++;
		if (lineno >= maxlines)
			break;
	}
	return lines;
}

char const *
stf_parse_find_line (StfParseOptions_t *parseoptions,
		     char const *data,
		     int line)
{
	while (line > 0) {
		int termlen = compare_terminator (data, parseoptions);
		if (termlen > 0) {
			data += termlen;
			line--;
		} else if (*data == 0) {
			return data;
		} else {
			data = g_utf8_next_char (data);
		}
	}
	return data;
}


/**
 * stf_parse_options_fixed_autodiscover:
 * @parseoptions: a Parse options struct.
 * @data_lines : The number of lines to look at in @data.
 * @data : The actual data.
 *
 * Automatically try to discover columns in the text to be parsed.
 * We ignore empty lines (only containing parseoptions->terminator)
 *
 * FIXME: This is so extremely ugly that I am too tired to rewrite it right now.
 *        Think hard of a better more flexible solution...
 **/
void
stf_parse_options_fixed_autodiscover (StfParseOptions_t *parseoptions,
				      char const *data, char const *data_end)
{
	char const *iterator = data;
	GSList *list = NULL;
	GSList *list_start = NULL;
	int lines = 0;
	int effective_lines = 0;
	int max_line_length = 0;
	int *line_begin_hits = NULL;
	int *line_end_hits = NULL;
	int i;

	stf_parse_options_fixed_splitpositions_clear (parseoptions);

	/*
	 * First take a look at all possible white space combinations
	 */
	while (*iterator && iterator < data_end) {
		gboolean begin_recorded = FALSE;
		AutoDiscovery_t *disc = NULL;
		int position = 0;
		int termlen = 0;

		while (*iterator && (termlen = compare_terminator (iterator, parseoptions)) == 0) {
			if (!begin_recorded && *iterator == ' ') {
				disc = g_new0 (AutoDiscovery_t, 1);

				disc->start = position;

				begin_recorded = TRUE;
			} else if (begin_recorded && *iterator != ' ') {
				disc->stop = position;
				list = g_slist_prepend (list, disc);

				begin_recorded = FALSE;
				disc = NULL;
			}

			position++;
			iterator++;
		}

		if (position > max_line_length)
			max_line_length = position;

		/*
		 * If there are excess spaces at the end of
		 * the line : ignore them
		 */
		g_free (disc);

		/*
		 * Hop over the terminator
		 */
		iterator += termlen;

		if (position != 0)
			effective_lines++;

		lines++;
	}

	list       = g_slist_reverse (list);
	list_start = list;

	/*
	 * Kewl stuff :
	 * Look at the number of hits at each line position
	 * if the number of hits equals the number of lines
	 * we can be pretty sure this is the start or end
	 * of a column, we filter out empty columns
	 * later
	 */
	line_begin_hits = g_new0 (int, max_line_length + 1);
	line_end_hits   = g_new0 (int, max_line_length + 1);

	while (list) {
		AutoDiscovery_t *disc = list->data;

		line_begin_hits[disc->start]++;
		line_end_hits[disc->stop]++;

		g_free (disc);

		list = g_slist_next (list);
	}
	g_slist_free (list_start);

	for (i = 0; i < max_line_length + 1; i++)
		if (line_begin_hits[i] == effective_lines || line_end_hits[i] == effective_lines)
			stf_parse_options_fixed_splitpositions_add (parseoptions, i);

	/*
	 * Do some corrections to the initial columns
	 * detected here, we obviously don't need to
	 * do this if there are no columns at all.
	 */
	if (my_garray_len (parseoptions->splitpositions) > 0) {
		/*
		 * Try to find columns that look like :
		 *
		 * Example     100
		 * Example2      9
		 *
		 * (In other words : Columns with left & right justification with
		 *  a minimum of 2 spaces in the middle)
		 * Split these columns in 2
		 */

		for (i = 0; i < my_garray_len (parseoptions->splitpositions) - 1; i++) {
			int begin = g_array_index (parseoptions->splitpositions, int, i);
			int end   = g_array_index (parseoptions->splitpositions, int, i + 1);
			int num_spaces   = -1;
			int spaces_start = 0;
			gboolean right_aligned = TRUE;
			gboolean left_aligned  = TRUE;
			gboolean has_2_spaces  = TRUE;

			iterator = data;
			lines = 0;
			while (*iterator && iterator < data_end) {
				gboolean trigger = FALSE;
				gboolean space_trigger = FALSE;
				int pos = 0;

				num_spaces   = -1;
				spaces_start = 0;
				while (*iterator && !compare_terminator (iterator, parseoptions)) {
					if (pos == begin) {
						if (*iterator == ' ')
							left_aligned = FALSE;

						trigger = TRUE;
					} else if (pos == end - 1) {
						if (*iterator == ' ')
							right_aligned = FALSE;

						trigger = FALSE;
					}

					if (trigger || pos == end - 1) {
						if (!space_trigger && *iterator == ' ') {
							space_trigger = TRUE;
							spaces_start = pos;
						} else if (space_trigger && *iterator != ' ') {
							space_trigger = FALSE;
							num_spaces = pos - spaces_start;
						}
					}

					iterator++;
					pos++;
				}

				if (num_spaces < 2)
					has_2_spaces = FALSE;

				if (*iterator)
					iterator++;

				lines++;
			}

			/*
			 * If this column meets all the criteria
			 * split it into two at the last measured
			 * spaces_start + num_spaces
			 */
			if (has_2_spaces && right_aligned && left_aligned) {
				int val = (((spaces_start + num_spaces) - spaces_start) / 2) + spaces_start;

				g_array_insert_val (parseoptions->splitpositions, i + 1, val);

				/*
				 * Skip over the inserted column
				 */
				i++;
			}
		}

		/*
		 * Remove empty columns here if needed
		 */
		for (i = 0; i < my_garray_len (parseoptions->splitpositions) - 1; i++) {
			int begin = g_array_index (parseoptions->splitpositions, int, i);
			int end = g_array_index (parseoptions->splitpositions, int, i + 1);
			gboolean only_spaces = TRUE;

			iterator = data;
			lines = 0;
			while (*iterator && iterator < data_end) {
				gboolean trigger = FALSE;
				int pos = 0;

				while (*iterator && !compare_terminator (iterator, parseoptions)) {
					if (pos == begin)
						trigger = TRUE;
					else if (pos == end)
						trigger = FALSE;

					if (trigger) {
						if (*iterator != ' ')
							only_spaces = FALSE;
					}

					iterator++;
					pos++;
				}

				if (*iterator)
					iterator++;

				lines++;
			}

			/*
			 * The column only contains spaces
			 * remove it
			 */
			if (only_spaces) {
				g_array_remove_index (parseoptions->splitpositions, i);

				/*
				 * We HAVE to make sure that the next column (end) also
				 * gets checked out. If we don't decrease "i" here, we
				 * will skip over it as the indexes shift down after
				 * the removal
				 */
				i--;
			}
		}
	}

	g_free (line_begin_hits);
	g_free (line_end_hits);
}

/*******************************************************************************************************
 * STF PARSE HL: high-level functions that dump the raw data returned by the low-level parsing
 *               functions into something meaningful (== application specific)
 *******************************************************************************************************/

/* gboolean */
/* stf_parse_sheet (StfParseOptions_t *parseoptions, */
/* 		 char const *data, char const *data_end, */
/* 		 Sheet *sheet, int start_col, int start_row) */
/* { */
/* 	int row, col; */
/* 	unsigned int lrow, lcol; */
/* 	GODateConventions const *date_conv; */
/* 	GStringChunk *lines_chunk; */
/* 	GPtrArray *lines, *line; */

/* 	SETUP_LOCALE_SWITCH; */

/* 	g_return_val_if_fail (parseoptions != NULL, FALSE); */
/* 	g_return_val_if_fail (data != NULL, FALSE); */
/* 	g_return_val_if_fail (IS_SHEET (sheet), FALSE); */

/* 	START_LOCALE_SWITCH; */

/* 	date_conv = workbook_date_conv (sheet->workbook); */

/* 	if (!data_end) */
/* 		data_end = data + strlen (data); */
/* 	lines_chunk = g_string_chunk_new (100 * 1024); */
/* 	lines = stf_parse_general (parseoptions, lines_chunk, data, data_end); */
/* 	if (lines == NULL) */
/* 		return FALSE; */
/* 	for (row = start_row, lrow = 0; lrow < lines->len ; row++, lrow++) { */
/* 		col = start_col; */
/* 		line = g_ptr_array_index (lines, lrow); */

/* 		for (lcol = 0; lcol < line->len; lcol++) */
/* 			if (parseoptions->col_import_array == NULL || */
/* 			    parseoptions->col_import_array_len <= lcol || */
/* 			    parseoptions->col_import_array[lcol]) { */
/* 				if (col >= SHEET_MAX_COLS) { */
/* 					if (!parseoptions->cols_exceeded) { */
/* 						g_warning (_("There are more columns of data than " */
/* 							     "there is room for in the sheet.  Extra " */
/* 							     "columns will be ignored.")); */
/* 						parseoptions->cols_exceeded = TRUE; */
/* 					} */
/* 				} else { */
/* 					char const *text = g_ptr_array_index (line, lcol); */
/* 					if (text && *text) */
/* 						gnm_cell_set_text ( */
/* 							sheet_cell_fetch (sheet, col, row), */
/* 							text); */
/* 				} */
/* 				col++; */
/* 			} */
/* 	} */

/* 	stf_parse_general_free (lines); */
/* 	g_string_chunk_free (lines_chunk); */
/* 	END_LOCALE_SWITCH; */
/* 	return TRUE; */
/* } */

/* GnmCellRegion * */
/* stf_parse_region (StfParseOptions_t *parseoptions, char const *data, char const *data_end, */
/* 		  Workbook const *wb) */
/* { */
/* 	static GODateConventions const default_conv = {FALSE}; */
/* 	GODateConventions const *date_conv = wb ? workbook_date_conv (wb) : &default_conv; */

/* 	GnmCellRegion *cr; */
/* 	unsigned int row, colhigh = 0; */
/* 	char *text; */
/* 	GStringChunk *lines_chunk; */
/* 	GPtrArray *lines; */
/* 	GnmCellCopy	*cc; */
/* 	GOFormat	*fmt; */
/* 	GnmValue	*v; */

/* 	SETUP_LOCALE_SWITCH; */

/* 	g_return_val_if_fail (parseoptions != NULL, NULL); */
/* 	g_return_val_if_fail (data != NULL, NULL); */

/* 	START_LOCALE_SWITCH; */

/* 	cr = cellregion_new (NULL); */

/* 	if (!data_end) */
/* 		data_end = data + strlen (data); */
/* 	lines_chunk = g_string_chunk_new (100 * 1024); */
/* 	lines = stf_parse_general (parseoptions, lines_chunk, data, data_end); */
/* 	for (row = 0; row < lines->len; row++) { */
/* 		GPtrArray *line = g_ptr_array_index (lines, row); */
/* 		unsigned int col, targetcol = 0; */
/* 		for (col = 0; col < line->len; col++) { */
/* 			if (parseoptions->col_import_array == NULL || */
/* 			    parseoptions->col_import_array_len <= col || */
/* 			    parseoptions->col_import_array[col]) { */
/* 				if (NULL != (text = g_ptr_array_index (line, col))) { */
/* 					fmt = g_ptr_array_index ( */
/* 						parseoptions->formats, col); */
/* 					if (NULL == (v = format_match (text, fmt, date_conv))) */
/* 						v = value_new_string (text); */

/* 					cc = gnm_cell_copy_new (cr, targetcol, row); */
/* 					cc->val  = v; */
/* 					cc->texpr = NULL; */
/* 					targetcol++; */
/* 					if (targetcol > colhigh) */
/* 						colhigh = targetcol; */
/* 				} */
/* 			} */
/* 		} */
/* 	} */
/* 	stf_parse_general_free (lines); */
/* 	g_string_chunk_free (lines_chunk); */

/* 	END_LOCALE_SWITCH; */

/* 	cr->cols    = (colhigh > 0) ? colhigh : 1; */
/* 	cr->rows    = row; */

/* 	return cr; */
/* } */

static int
int_sort (void const *a, void const *b)
{
	return *(int const *)a - *(int const *)b;
}

static int
count_character (GPtrArray *lines, gunichar c, double quantile)
{
	int *counts, res;
	unsigned int lno, cno;

	if (lines->len == 0)
		return 0;

	counts = g_new (int, lines->len);
	for (lno = cno = 0; lno < lines->len; lno++) {
		int count = 0;
		GPtrArray *boxline = g_ptr_array_index (lines, lno);
		char const *line = g_ptr_array_index (boxline, 0);

		/* Ignore empty lines.  */
		if (*line == 0)
			continue;

		while (*line) {
			if (g_utf8_get_char (line) == c)
				count++;
			line = g_utf8_next_char (line);
		}

		counts[cno++] = count;
	}

	if (cno == 0)
		res = 0;
	else {
		unsigned int qi = (unsigned int)ceil (quantile * cno);
		qsort (counts, cno, sizeof (counts[0]), int_sort);
		if (qi == cno)
			qi--;
		res = counts[qi];
	}

	g_free (counts);

	return res;
}


StfParseOptions_t *
stf_parse_options_guess (char const *data)
{
	StfParseOptions_t *res;
	GStringChunk *lines_chunk;
	GPtrArray *lines;
	int tabcount;
	int sepcount;
	/* TODO In the future, use the goffice 0.3. */
	/* gunichar sepchar = go_locale_get_arg_sep (); */
	gunichar sepchar = ',';

	g_return_val_if_fail (data != NULL, NULL);

	res = stf_parse_options_new ();
	lines_chunk = g_string_chunk_new (100 * 1024);
	lines = stf_parse_lines (res, lines_chunk, data, SHEET_MAX_ROWS, FALSE);

	tabcount = count_character (lines, '\t', 0.2);
	sepcount = count_character (lines, sepchar, 0.2);

	/* At least one tab per line and enough to separate every
	   would-be sepchars.  */
	if (tabcount >= 1 && tabcount >= sepcount - 1)
		stf_parse_options_csv_set_separators (res, "\t", NULL);
	else {
		gunichar c;

		/*
		 * Try a few more or less likely characters and pick the first
		 * one that occurs on at least half the lines.
		 *
		 * The order is mostly random, although ' ' and '!' which
		 * could very easily occur in text are put last.
		 */
		/* TODO Replace with the 0.3 goffice call in the future. */
		if (count_character (lines, (c = sepchar), 0.5) > 0 ||
		    /* count_character (lines, (c = go_locale_get_col_sep ()), 0.5) > 0 || */
		    count_character (lines, (c = ','), 0.5) > 0 ||
		    count_character (lines, (c = ':'), 0.5) > 0 ||
		    count_character (lines, (c = ','), 0.5) > 0 ||
		    count_character (lines, (c = ';'), 0.5) > 0 ||
		    count_character (lines, (c = '|'), 0.5) > 0 ||
		    count_character (lines, (c = '!'), 0.5) > 0 ||
		    count_character (lines, (c = ' '), 0.5) > 0) {
			char sep[7];
			sep[g_unichar_to_utf8 (c, sep)] = 0;
			if (c == ' ')
				strcat (sep, "\t");
			stf_parse_options_csv_set_separators (res, sep, NULL);
		}
	}

	if (1) {
		/* Separated */
		gboolean dups =
			res->sep.chr &&
			strchr (res->sep.chr, ' ') != NULL;
		gboolean trim =
			res->sep.chr &&
			strchr (res->sep.chr, ' ') != NULL;

		stf_parse_options_set_type (res, PARSE_TYPE_CSV);
		stf_parse_options_set_trim_spaces (res, TRIM_TYPE_LEFT | TRIM_TYPE_RIGHT);
		stf_parse_options_csv_set_indicator_2x_is_single (res, TRUE);
		stf_parse_options_csv_set_duplicates (res, dups);
		stf_parse_options_csv_set_trim_seps (res, trim);

		stf_parse_options_csv_set_stringindicator (res, '"');
	} else {
		/* Fixed-width */
	}

	stf_parse_general_free (lines);
	g_string_chunk_free (lines_chunk);

	return res;
}
