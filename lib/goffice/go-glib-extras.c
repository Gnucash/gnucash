/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * utils.c:  Various utility routines that should have been in glib.
 *
 * Authors:
 *    Miguel de Icaza (miguel@gnu.org)
 *    Jukka-Pekka Iivonen (iivonen@iki.fi)
 *    Zbigniew Chyla (cyba@gnome.pl)
 *    Morten Welinder (terra@gnome.org)
 */
#include <goffice/goffice-config.h>
#include "go-glib-extras.h"
#include "go-locale.h"
#include <goffice/app/go-cmd-context.h>

#include <glib/gi18n-lib.h>
#include <gsf/gsf-impl-utils.h>
#include <libxml/encoding.h>

#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#include <errno.h>

static void
cb_hash_collect_keys (gpointer key, gpointer value, GSList **accum)
{
	*accum = g_slist_prepend (*accum, key);
}

/**
 * go_hash_keys :
 * @hash : #GHashTable
 *
 * Collects an unordered list of the keys in @hash.
 *
 * Returns: a list which the caller needs to free.
 * 	The content has not additional references added
 **/
GSList *
go_hash_keys (GHashTable *hash)
{
	GSList *accum = NULL;
	g_hash_table_foreach (hash,
		(GHFunc )cb_hash_collect_keys, &accum);
	return accum;
}

static void
cb_hash_collect_values (gpointer key, gpointer value, GSList **accum)
{
	*accum = g_slist_prepend (*accum, value);
}

/**
 * go_hash_values :
 * @hash : #GHashTable
 *
 * Collects an unordered list of the values in @hash.
 *
 * Returns: a list which the caller needs to free.
 * 	The content has not additional references added
 **/
GSList *
go_hash_values (GHashTable *hash)
{
	GSList *accum = NULL;
	g_hash_table_foreach (hash,
		(GHFunc )cb_hash_collect_values, &accum);
	return accum;
}

/***************************************************************************/
void
go_ptr_array_insert (GPtrArray *array, gpointer value, int index)
{
	if (index < (int)array->len) {
		int i = array->len - 1;
		gpointer last = g_ptr_array_index (array, i);
		g_ptr_array_add (array, last);

		while (i-- > index) {
			gpointer tmp = g_ptr_array_index (array, i);
			g_ptr_array_index (array, i + 1) = tmp;
		}
		g_ptr_array_index (array, index) = value;
	} else
		g_ptr_array_add (array, value);
}

/**
 * go_slist_create:
 * @item1: itionally %NULL
 * @Varargs : %NULL terminated list of additional items
 *
 * Creates a GList from NULL-terminated list of arguments.
 *
 * Returns: created list.
 **/
GSList *
go_slist_create (gpointer item1, ...)
{
	va_list args;
	GSList *list = NULL;
	gpointer item;

	va_start (args, item1);
	for (item = item1; item != NULL; item = va_arg (args, gpointer)) {
		list = g_slist_prepend (list, item);
	}
	va_end (args);

	return g_slist_reverse (list);
}

/**
 * go_slist_map:
 * @list        : list of some items
 * @map_func    : mapping function
 *
 **/
GSList *
go_slist_map (GSList const *list, GOMapFunc map_func)
{
	GSList *list_copy = NULL;

	GO_SLIST_FOREACH (list, void, value,
		GO_SLIST_PREPEND (list_copy, map_func (value))
	);

	return g_slist_reverse (list_copy);
}

/**
 * go_slist_free_custom:
 * @list: list of some items
 * @free_func: function freeing list item
 *
 * Clears a list, calling @free_func for each list item.
 **/
void
go_slist_free_custom (GSList *list, GFreeFunc free_func)
{
	GSList *l;

	for (l = list; l != NULL; l = l->next) {
		free_func (l->data);
	}
	g_slist_free (list);
}

gint
go_list_index_custom (GList *list, gpointer data, GCompareFunc cmp_func)
{
	GList *l;
	gint i;

	for (l = list, i = 0; l != NULL; l = l->next, i++) {
		if (cmp_func (l->data, data) == 0) {
			return i;
		}
	}

	return -1;
}

/**
 * go_list_free_custom:
 * @list: list of some items
 * @free_func: function freeing list item
 *
 * Clears a list, calling @free_func for each list item.
 *
 */
void
go_list_free_custom (GList *list, GFreeFunc free_func)
{
	GList *l;

	for (l = list; l != NULL; l = l->next) {
		free_func (l->data);
	}
	g_list_free (list);
}

/**
 * go_strsplit_to_slist:
 * @str: String to split
 * @delimiter: Token delimiter
 *
 * Splits up string into tokens at delim and returns a string list.
 *
 * Returns: string list which you should free after use using function
 * e_free_string_list().
 **/
GSList *
go_strsplit_to_slist (gchar const *string, gchar delimiter)
{
	gchar **token_v;
	GSList *string_list = NULL;
	char buf[2] = { '\0', '\0' };
	gint i;

	buf[0] = delimiter;
	token_v = g_strsplit (string, buf, 0);
	if (token_v != NULL) {
		for (i = 0; token_v[i] != NULL; i++) {
			string_list = g_slist_prepend (string_list, token_v[i]);
		}
		string_list = g_slist_reverse (string_list);
		g_free (token_v);
	}

	return string_list;
}

gint
go_utf8_collate_casefold (const char *a, const char *b)
{
	char *a2 = g_utf8_casefold (a, -1);
	char *b2 = g_utf8_casefold (b, -1);
	int res = g_utf8_collate (a2, b2);
	g_free (a2);
	g_free (b2);
	return res;
}

gint
go_ascii_strcase_equal (gconstpointer v1, gconstpointer v2)
{
	return g_ascii_strcasecmp ((char const *) v1, (char const *)v2) == 0;
}

/* a char* hash function from ASU */
guint
go_ascii_strcase_hash (gconstpointer v)
{
	unsigned const char *s = (unsigned const char *)v;
	unsigned const char *p;
	guint h = 0, g;

	for(p = s; *p != '\0'; p += 1) {
		h = ( h << 4 ) + g_ascii_tolower (*p);
		if ( ( g = h & 0xf0000000 ) ) {
			h = h ^ (g >> 24);
			h = h ^ g;
		}
	}

	return h /* % M */;
}

/* ------------------------------------------------------------------------- */

/*
 * Escapes all backslashes and quotes in a string. It is based on glib's
 * g_strescape.
 *
 * Also adds quotes around the result.
 */
void
go_strescape (GString *target, char const *string)
{
	g_string_append_c (target, '"');
	/* This loop should be UTF-8 safe.  */
	for (; *string; string++) {
		switch (*string) {
		case '"':
		case '\\':
			g_string_append_c (target, '\\');
		default:
			g_string_append_c (target, *string);
		}
	}
	g_string_append_c (target, '"');
}

/*
 * The reverse operation of go_strescape.  Returns a pointer to the
 * first char after the string on success or NULL on failure.
 *
 * First character of the string should be an ASCII character used
 * for quoting.
 */
const char *
go_strunescape (GString *target, const char *string)
{
	char quote = *string++;
	size_t oldlen = target->len;

	/* This should be UTF-8 safe as long as quote is ASCII.  */
	while (*string != quote) {
		if (*string == 0)
			goto error;
		else if (*string == '\\') {
			string++;
			if (*string == 0)
				goto error;
		}

		g_string_append_c (target, *string);
		string++;
	}

	return ++string;

 error:
	g_string_truncate (target, oldlen);
	return NULL;
}

void
go_string_append_gstring (GString *target, const GString *source)
{
	g_string_append_len (target, source->str, source->len);
}

void
go_string_append_c_n (GString *target, char c, gsize n)
{
	gsize len = target->len;
	g_string_set_size (target, len + n);
	memset (target->str + len, c, n);
}

void
go_string_replace (GString *target,
		   gsize pos, gssize oldlen,
		   const char *txt, gssize newlen)
{
	gsize cplen;

	g_return_if_fail (target != NULL);
	g_return_if_fail (pos >= 0);
	g_return_if_fail (pos <= target->len);

	if (oldlen < 0)
		oldlen = target->len - pos;
	if (newlen < 0)
		newlen = strlen (txt);

	cplen = MIN (oldlen, newlen);
	memcpy (target->str + pos, txt, cplen);

	pos += cplen;
	oldlen -= cplen;
	txt += cplen;
	newlen -= cplen;

	/*
	 * At least one of oldlen and newlen is zero now.  We could call
	 * both erase and insert unconditionally, but erase does not appear
	 * to handle zero length efficiently.
	 */

	if (oldlen > 0)
		g_string_erase (target, pos, oldlen);
	else if (newlen > 0)
		g_string_insert_len (target, pos, txt, newlen);
}

/* ------------------------------------------------------------------------- */

/**
 * go_utf8_strcapital:
 * @p: pointer to UTF-8 string
 * @len: length in bytes, or -1.
 *
 * Similar to g_utf8_strup and g_utf8_strup, except that this function
 * creates a string "Very Much Like: This, One".
 *
 * Return value: newly allocated string.
 **/
char *
go_utf8_strcapital (const char *p, gssize len)
{
	const char *pend = (len < 0 ? NULL : p + len);
	GString *res = g_string_sized_new (len < 0 ? 1 : len + 1);
	gboolean up = TRUE;

	/*
	 * This does a simple character-by-character mapping and probably
	 * is not linguistically correct.
	 */

	for (; (len < 0 || p < pend) && *p; p = g_utf8_next_char (p)) {
		gunichar c = g_utf8_get_char (p);

		if (g_unichar_isalpha (c)) {
			if (up ? g_unichar_isupper (c) : g_unichar_islower (c))
				/* Correct case -- keep the char.  */
				g_string_append_unichar (res, c);
			else {
				char *tmp = up
					? g_utf8_strup (p, 1)
					: g_utf8_strdown (p, 1);
				g_string_append (res, tmp);
				g_free (tmp);
			}
			up = FALSE;
		} else {
			g_string_append_unichar (res, c);
			up = TRUE;
		}
	}

	return g_string_free (res, FALSE);
}

/* ------------------------------------------------------------------------- */

#undef DEBUG_CHUNK_ALLOCATOR

typedef struct _go_mem_chunk_freeblock go_mem_chunk_freeblock;
typedef struct _go_mem_chunk_block go_mem_chunk_block;

struct _go_mem_chunk_freeblock {
	go_mem_chunk_freeblock *next;
};

struct _go_mem_chunk_block {
	gpointer data;
	int freecount, nonalloccount;
	go_mem_chunk_freeblock *freelist;
#ifdef DEBUG_CHUNK_ALLOCATOR
	int id;
#endif
};

struct _GOMemChunk {
	char *name;
	size_t atom_size, user_atom_size, chunk_size, alignment;
	int atoms_per_block;

	/* List of all blocks.  */
	GSList *blocklist;

	/* List of blocks that are not full.  */
	GList *freeblocks;

#ifdef DEBUG_CHUNK_ALLOCATOR
	int blockid;
#endif
};


GOMemChunk *
go_mem_chunk_new (char const *name, size_t user_atom_size, size_t chunk_size)
{
	int atoms_per_block;
	GOMemChunk *res;
	size_t user_alignment, alignment, atom_size;
	size_t maxalign = 1 + ((sizeof (void *) - 1) |
			       (sizeof (long) - 1) |
			       (sizeof (double) - 1));

	/*
	 * The alignment that the caller can expect is 2^(lowest_bit_in_size).
	 * The fancy bit math computes this.  Think it over.
	 *
	 * We don't go lower than pointer-size, so this always comes out as
	 * 4 or 8.  Sometimes, when user_atom_size is a multiple of 8, this
	 * alignment is bigger than really needed, but we don't know if the
	 * structure has elements with 8-byte alignment.  In those cases we
	 * waste memory.
	 */
	user_alignment = ((user_atom_size ^ (user_atom_size - 1)) + 1) / 2;
	alignment = MIN (MAX (user_alignment, sizeof (go_mem_chunk_block *)), maxalign);
	atom_size = alignment + MAX (user_atom_size, sizeof (go_mem_chunk_freeblock));
	atoms_per_block = MAX (1, chunk_size / atom_size);
	chunk_size = atoms_per_block * atom_size;

#ifdef DEBUG_CHUNK_ALLOCATOR
	g_print ("Created %s with alignment=%d, atom_size=%d (%d), chunk_size=%d.\n",
		 name, alignment, atom_size, user_atom_size,
		 chunk_size);
#endif

	res = g_new (GOMemChunk, 1);
	res->alignment = alignment;
	res->name = g_strdup (name);
	res->user_atom_size = user_atom_size;
	res->atom_size = atom_size;
	res->chunk_size = chunk_size;
	res->atoms_per_block = atoms_per_block;
	res->blocklist = NULL;
	res->freeblocks = NULL;
#ifdef DEBUG_CHUNK_ALLOCATOR
	res->blockid = 0;
#endif

	return res;
}

void
go_mem_chunk_destroy (GOMemChunk *chunk, gboolean expect_leaks)
{
	GSList *l;

	g_return_if_fail (chunk != NULL);

#ifdef DEBUG_CHUNK_ALLOCATOR
	g_print ("Destroying %s.\n", chunk->name);
#endif
	/*
	 * Since this routine frees all memory allocated for the pool,
	 * it is sometimes convenient not to free at all.  For such
	 * cases, don't report leaks.
	 */
	if (!expect_leaks) {
		GSList *l;
		int leaked = 0;

		for (l = chunk->blocklist; l; l = l->next) {
			go_mem_chunk_block *block = l->data;
			leaked += chunk->atoms_per_block - (block->freecount + block->nonalloccount);
		}
		if (leaked) {
			g_warning ("Leaked %d nodes from %s.",
				   leaked, chunk->name);
		}
	}

	for (l = chunk->blocklist; l; l = l->next) {
		go_mem_chunk_block *block = l->data;
		g_free (block->data);
		g_free (block);
	}
	g_slist_free (chunk->blocklist);
	g_list_free (chunk->freeblocks);
	g_free (chunk->name);
	g_free (chunk);
}

gpointer
go_mem_chunk_alloc (GOMemChunk *chunk)
{
	go_mem_chunk_block *block;
	char *res;

	/* First try the freelist.  */
	if (chunk->freeblocks) {
		go_mem_chunk_freeblock *res;

		block = chunk->freeblocks->data;
		res = block->freelist;
		if (res) {
			block->freelist = res->next;

			block->freecount--;
			if (block->freecount == 0 && block->nonalloccount == 0) {
				/* Block turned full -- remove it from freeblocks.  */
				chunk->freeblocks = g_list_delete_link (chunk->freeblocks,
									chunk->freeblocks);
			}
			return res;
		}
		/*
		 * If we get here, the block has free space that was never
		 * allocated.
		 */
	} else {
		block = g_new (go_mem_chunk_block, 1);
#ifdef DEBUG_CHUNK_ALLOCATOR
		block->id = chunk->blockid++;
		g_print ("Allocating new chunk %d for %s.\n", block->id, chunk->name);
#endif
		block->nonalloccount = chunk->atoms_per_block;
		block->freecount = 0;
		block->data = g_malloc (chunk->chunk_size);
		block->freelist = NULL;

		chunk->blocklist = g_slist_prepend (chunk->blocklist, block);
		chunk->freeblocks = g_list_prepend (chunk->freeblocks, block);
	}

	res = (char *)block->data +
		(chunk->atoms_per_block - block->nonalloccount--) * chunk->atom_size;
	*((go_mem_chunk_block **)res) = block;

	if (block->nonalloccount == 0 && block->freecount == 0) {
		/* Block turned full -- remove it from freeblocks.  */
		chunk->freeblocks = g_list_delete_link (chunk->freeblocks, chunk->freeblocks);
	}

	return res + chunk->alignment;
}

gpointer
go_mem_chunk_alloc0 (GOMemChunk *chunk)
{
	gpointer res = go_mem_chunk_alloc (chunk);
	memset (res, 0, chunk->user_atom_size);
	return res;
}

void
go_mem_chunk_free (GOMemChunk *chunk, gpointer mem)
{
	go_mem_chunk_freeblock *fb = mem;
	go_mem_chunk_block *block =
		*((go_mem_chunk_block **)((char *)mem - chunk->alignment));

#if 0
	/*
	 * This is useful when the exact location of a leak needs to be
	 * pin-pointed.
	 */
	memset (mem, 0, chunk->user_atom_size);
#endif

	fb->next = block->freelist;
	block->freelist = fb;
	block->freecount++;

	if (block->freecount == 1 && block->nonalloccount == 0) {
		/* Block turned non-full.  */
		chunk->freeblocks = g_list_prepend (chunk->freeblocks, block);
	} else if (block->freecount == chunk->atoms_per_block) {
		/* Block turned all-free.  */

#ifdef DEBUG_CHUNK_ALLOCATOR
		g_print ("Releasing chunk %d for %s.\n", block->id, chunk->name);
#endif
		/*
		 * FIXME -- this could be faster if we rolled our own lists.
		 * Hopefully, however, (a) the number of blocks is small,
		 * and (b) the freed block might be near the beginning ("top")
		 * of the stacks.
		 */
		chunk->blocklist = g_slist_remove (chunk->blocklist, block);
		chunk->freeblocks = g_list_remove (chunk->freeblocks, block);

		g_free (block->data);
		g_free (block);
	}
}

/*
 * Loop over all non-freed memory in the chunk.  It's safe to allocate or free
 * from the chunk in the callback.
 */
void
go_mem_chunk_foreach_leak (GOMemChunk *chunk, GFunc cb, gpointer user)
{
	GSList *l, *leaks = NULL;

	for (l = chunk->blocklist; l; l = l->next) {
		go_mem_chunk_block *block = l->data;
		if (chunk->atoms_per_block - (block->freecount + block->nonalloccount) > 0) {
			char *freed = g_new0 (char, chunk->atoms_per_block);
			go_mem_chunk_freeblock *fb = block->freelist;
			int i;

			while (fb) {
				char *atom = (char *)fb - chunk->alignment;
				int no = (atom - (char *)block->data) / chunk->atom_size;
				freed[no] = 1;
				fb = fb->next;
			}

			for (i = chunk->atoms_per_block - block->nonalloccount - 1; i >= 0; i--) {
				if (!freed[i]) {
					char *atom = (char *)block->data + i * chunk->atom_size;
					leaks = g_slist_prepend (leaks, atom + chunk->alignment);
				}
			}
			g_free (freed);
		}
	}

	g_slist_foreach (leaks, cb, user);
	g_slist_free (leaks);
}

int
go_str_compare (void const *x, void const *y)
{
	if (x == y)
		return 0;

	if (x == NULL || y == NULL)
		return x ? -1 : 1;

	return strcmp (x, y);
}


const char *
go_guess_encoding (const char *raw, size_t len, const char *user_guess,
		   char **utf8_str)
{
	int try;
	gboolean debug = FALSE;

	g_return_val_if_fail (raw != NULL, NULL);

	for (try = 1; 1; try++) {
		char const *guess = NULL;
		GError *error = NULL;
		char *utf8_data;

		switch (try) {
		case 1: guess = user_guess; break;
		case 2: g_get_charset (&guess); break;
		case 3: {
			xmlCharEncoding enc =
				xmlDetectCharEncoding ((const unsigned char*)raw, len);
			switch (enc) {
			case XML_CHAR_ENCODING_ERROR:
			case XML_CHAR_ENCODING_NONE:
				break;
			case XML_CHAR_ENCODING_UTF16LE:
				/* Default would give "UTF-16".  */
				guess = "UTF-16LE";
				break;
			case XML_CHAR_ENCODING_UTF16BE:
				/* Default would give "UTF-16".  */
				guess = "UTF-16BE";
				break;
			default:
				guess = xmlGetCharEncodingName (enc);
			}
			break;
		}
		case 4: guess = "ASCII"; break;
		case 5: guess = "ISO-8859-1"; break;
		case 6: guess = "UTF-8"; break;
		default: return NULL;
		}

		if (!guess)
			continue;

		if (debug)
			g_print ("Trying %s as encoding.\n", guess);

		utf8_data = g_convert (raw, len, "UTF-8", guess,
				       NULL, NULL, &error);
		if (!error) {
			/*
			 * We can actually fail this test when gues is UTF-8,
			 * see #401588.
			 */
			if (!g_utf8_validate (utf8_data, -1, NULL))
				continue;
			if (debug)
				g_print ("Guessed %s as encoding.\n", guess);
			if (utf8_str)
				*utf8_str = utf8_data;
			else
				g_free (utf8_data);
			return guess;
		}

		g_error_free (error);
	}
}

/**
 * go_get_real_name :
 *
 * Returns: a utf8 encoded string with the current user name.
 * 	Caller should _NOT_ free the result.
 **/
char const *
go_get_real_name (void)
{
	/* We will leak this.  */
	static char *go_real_name = NULL;

	if (go_real_name == NULL) {
		char const *name = getenv ("NAME");
		if (name == NULL)
			name = g_get_real_name ();
		if (name == NULL)
			name = g_get_user_name ();
		if (name != NULL)
			(void) go_guess_encoding (name, strlen (name),
				NULL, &go_real_name);
		else
			go_real_name = (char *)"unknown";
	}
	return go_real_name;
}

/**
 * go_destroy_password :
 * @passwd : The buffer to clear
 *
 * Overwrite a string holding a password.  This is a separate routine to
 * ensure that the compiler does not try to outsmart us.
 *
 * Note: this does not free the memory.
 **/
void
go_destroy_password (char *passwd)
{
	memset (passwd, 0, strlen (passwd));
}


/**
 * go_object_toggle:
 * @object : #GObject
 * @property_name : name
 *
 * Toggle a boolean object property.
 **/
void
go_object_toggle (gpointer object, const gchar *property_name)
{
	gboolean value = FALSE;
	GParamSpec *pspec;

	g_return_if_fail (G_IS_OBJECT (object));
	g_return_if_fail (property_name != NULL);

	pspec = g_object_class_find_property (G_OBJECT_GET_CLASS (object), property_name);
	if (!pspec ||
	    !G_IS_PARAM_SPEC_BOOLEAN (pspec) ||
	    ((pspec->flags & (G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY)) !=
	     G_PARAM_READWRITE)) {
		g_warning ("%s: object class `%s' has no boolean property named `%s' that can be both read and written.",
			   G_STRFUNC,
			   G_OBJECT_TYPE_NAME (object),
			   property_name);
		return;
	}

	/* And now, the actual action.  */
	g_object_get (object, property_name, &value, NULL);
	g_object_set (object, property_name, !value, NULL);
}


gboolean
go_object_set_property (GObject *obj, const char *property_name,
			const char *user_prop_name, const char *value,
			GError **err,
			const char *error_template)
{
	GParamSpec *pspec;

	if (err) *err = NULL;

	g_return_val_if_fail (G_IS_OBJECT (obj), TRUE);
	g_return_val_if_fail (property_name != NULL, TRUE);
	g_return_val_if_fail (user_prop_name != NULL, TRUE);
	g_return_val_if_fail (value != NULL, TRUE);

	pspec = g_object_class_find_property (G_OBJECT_GET_CLASS (obj),
					      property_name);
	g_return_val_if_fail (pspec != NULL, TRUE);

	if (G_IS_PARAM_SPEC_STRING (pspec)) {
		g_object_set (obj, property_name, value, NULL);
		return FALSE;
	}

	if (G_IS_PARAM_SPEC_BOOLEAN (pspec)) {
		gboolean b;

		if (go_utf8_collate_casefold (value, go_locale_boolean_name (TRUE)) == 0 ||
		    go_utf8_collate_casefold (value, _("yes")) == 0 ||
		    g_ascii_strcasecmp (value, "TRUE") == 0 ||
		    g_ascii_strcasecmp (value, "yes") == 0 ||
		    strcmp (value, "1") == 0)
			b = TRUE;
		else if (go_utf8_collate_casefold (value, go_locale_boolean_name (FALSE)) == 0 ||
		    go_utf8_collate_casefold (value, _("no")) == 0 ||
		    g_ascii_strcasecmp (value, "FALSE") == 0 ||
		    g_ascii_strcasecmp (value, "no") == 0 ||
		    strcmp (value, "0") == 0)
			b = FALSE;
		else
			goto error;

		g_object_set (obj, property_name, b, NULL);
		return FALSE;
	}

	if (G_IS_PARAM_SPEC_ENUM (pspec)) {
		GEnumClass *eclass = ((GParamSpecEnum *)pspec)->enum_class;
		GEnumValue *ev;

		ev = g_enum_get_value_by_name (eclass, value);
		if (!ev) ev = g_enum_get_value_by_nick (eclass, value);

		if (!ev)
			goto error;

		g_object_set (obj, property_name, ev->value, NULL);
		return FALSE;
	}

	error:
		if (err)
			*err = g_error_new (go_error_invalid (), 0,
					    error_template,
					    user_prop_name,
					    value);
		return TRUE;
}




/*
 * Collect all rw properties and their values.
 */
GSList *
go_object_properties_collect (GObject *obj)
{
	GSList *res = NULL;
	guint n;
	GParamSpec **pspecs =
		g_object_class_list_properties (G_OBJECT_GET_CLASS (obj),
						&n);

	while (n--) {
		GParamSpec *pspec = pspecs[n];
		if ((pspec->flags & (G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY)) ==
		    G_PARAM_READWRITE) {
			GValue *value = g_new0 (GValue, 1);
			g_value_init (value, G_PARAM_SPEC_VALUE_TYPE (pspec));
			g_object_get_property (obj, pspec->name, value);
			res = g_slist_prepend (res, value);
			res = g_slist_prepend (res, pspec);
		}
	}

	g_free (pspecs);
	return res;
}

void
go_object_properties_apply (GObject *obj, GSList *props, gboolean changed_only)
{
	GValue current = { 0, };

	for (; props; props = props->next->next) {
		GParamSpec *pspec = props->data;
		const GValue *value = props->next->data;
		gboolean doit;

		if (changed_only) {
			g_value_init (&current,
				      G_PARAM_SPEC_VALUE_TYPE (pspec));
			g_object_get_property (obj, pspec->name, &current);
			doit = g_param_values_cmp (pspec, &current, value);
#if 0
			g_print ("%2d:  old: [%s]   new: [%s]\n",
				 g_param_values_cmp (pspec, &current, value),
				 g_strdup_value_contents (value),
				 g_strdup_value_contents (&current));
#endif
			g_value_unset (&current);
		} else
			doit = TRUE;

		if (doit)
			g_object_set_property (obj, pspec->name, value);
	}
}

void
go_object_properties_free (GSList *props)
{
	GSList *l;

	for (l = props; l; l = l->next->next) {
		GValue *value = l->next->data;
		g_value_unset (value);
		g_free (value);
	}

	g_slist_free (props);
}


/**
 * go_parse_key_value:
 * @options: Options string.
 * @err: Reference to store GError if parsing fails.
 * @handler: Handler to call for each key-value pair.
 * @user: user pointer.
 */
gboolean
go_parse_key_value (const char *options,
		    GError **err,
		    gboolean (*handler) (const char *name,
					 const char *value,
					 GError **err,
					 gpointer user),
		    gpointer user)
{
	GString *sname = g_string_new (NULL);
	GString *svalue = g_string_new (NULL);
	gboolean res = FALSE;

	if (err) *err = NULL;

	while (1) {
		const char *p;

		g_string_truncate (sname, 0);
		g_string_truncate (svalue, 0);

		while (g_unichar_isspace (g_utf8_get_char (options)))
			options = g_utf8_next_char (options);

		if (*options == 0)
			break;

		if (*options == '"' || *options == '\'') {
			options = go_strunescape (sname, options);
			if (!options)
				goto open_string;
		} else {
			p = options;
			while (strchr ("-!_.,:;|/$%#@~", *options) ||
			       g_unichar_isalnum (g_utf8_get_char (options)))
				options = g_utf8_next_char (options);
			g_string_append_len (sname, p, options - p);
			if (p == options)
				goto syntax;
		}

		while (g_unichar_isspace (g_utf8_get_char (options)))
			options = g_utf8_next_char (options);
		if (*options != '=')
			goto syntax;
		options++;
		while (g_unichar_isspace (g_utf8_get_char (options)))
			options = g_utf8_next_char (options);

		if (*options == '"' || *options == '\'') {
			options = go_strunescape (svalue, options);
			if (!options)
				goto open_string;
		} else {
			p = options;
			while (*options && !
			       g_unichar_isspace (g_utf8_get_char (options)))
				options = g_utf8_next_char (options);
			g_string_append_len (svalue, p, options - p);
		}

		if (handler (sname->str, svalue->str, err, user)) {
			res = TRUE;
			break;
		}
	}

done:
	g_string_free (sname, TRUE);
	g_string_free (svalue, TRUE);

	return res;

open_string:
	if (err)
		*err = g_error_new (go_error_invalid (), 0,
				    _("Quoted string not terminated"));
	res = TRUE;
	goto done;

syntax:
	if (err)
		*err = g_error_new (go_error_invalid (), 0,
				    _("Syntax error"));
	res = TRUE;
	goto done;
}
