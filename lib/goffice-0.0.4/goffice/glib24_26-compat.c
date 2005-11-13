#include "glib24_26-compat.h"

/**
 * g_date_get_iso8601_week_of_year:
 * @date: a valid #GDate
 *
 * Returns the week of the year, where weeks are interpreted according
 * to ISO 8601. 
 * 
 * Returns: ISO 8601 week number of the year.
 *
 * Since: 2.6
 **/
guint
g_date_get_iso8601_week_of_year (const GDate *d)
{
  guint j, d4, L, d1, w;

  g_return_val_if_fail (g_date_valid (d), 0);
  
  if (!d->julian)
    g_date_update_julian (d);
  g_return_val_if_fail (d->julian, 0);

  /* Formula taken from the Calendar FAQ; the formula was for the
   * Julian Period which starts on 1 January 4713 BC, so we add
   * 1,721,425 to the number of days before doing the formula. 
   */
  j  = d->julian_days + 1721425;
  d4 = (j + 31741 - (j % 7)) % 146097 % 36524 % 1461;
  L  = d4 / 1460;
  d1 = ((d4 - L) % 365) + L;
  w  = d1 / 7 + 1;

  return w;
}

/** ------------------------------------------------------------ **/

/* The following is (partly) taken from the gettext package.
   Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.  */

static const gchar *
guess_category_value (const gchar *category_name)
{
  const gchar *retval;

  /* The highest priority value is the `LANGUAGE' environment
     variable.  This is a GNU extension.  */
  retval = g_getenv ("LANGUAGE");
  if ((retval != NULL) && (retval[0] != '\0'))
    return retval;

  /* `LANGUAGE' is not set.  So we have to proceed with the POSIX
     methods of looking to `LC_ALL', `LC_xxx', and `LANG'.  On some
     systems this can be done by the `setlocale' function itself.  */

  /* Setting of LC_ALL overwrites all other.  */
  retval = g_getenv ("LC_ALL");  
  if ((retval != NULL) && (retval[0] != '\0'))
    return retval;

  /* Next comes the name of the desired category.  */
  retval = g_getenv (category_name);
  if ((retval != NULL) && (retval[0] != '\0'))
    return retval;

  /* Last possibility is the LANG environment variable.  */
  retval = g_getenv ("LANG");
  if ((retval != NULL) && (retval[0] != '\0'))
    return retval;

#ifdef G_PLATFORM_WIN32
  /* g_win32_getlocale() first checks for LC_ALL, LC_MESSAGES and
   * LANG, which we already did above. Oh well. The main point of
   * calling g_win32_getlocale() is to get the thread's locale as used
   * by Windows and the Microsoft C runtime (in the "English_United
   * States" format) translated into the Unixish format.
   */
  retval = g_win32_getlocale ();
  if ((retval != NULL) && (retval[0] != '\0'))
    return retval;
#endif  

  return NULL;
}

typedef struct _GLanguageNamesCache GLanguageNamesCache;

struct _GLanguageNamesCache {
  gchar *languages;
  gchar **language_names;
};

static void
language_names_cache_free (gpointer data)
{
  GLanguageNamesCache *cache = data;
  g_free (cache->languages);
  g_strfreev (cache->language_names);
  g_free (cache);
}

/**
 * g_get_language_names:
 * 
 * Computes a list of applicable locale names, which can be used to 
 * e.g. construct locale-dependent filenames or search paths. The returned 
 * list is sorted from most desirable to least desirable and always contains 
 * the default locale "C".
 *
 * For example, if LANGUAGE=de:en_US, then the returned list is
 * "de", "en_US", "en", "C".
 *
 * This function consults the environment variables <envar>LANGUAGE</envar>, 
 * <envar>LC_ALL</envar>, <envar>LC_MESSAGES</envar> and <envar>LANG</envar> 
 * to find the list of locales specified by the user.
 * 
 * Return value: a %NULL-terminated array of strings owned by GLib 
 *    that must not be modified or freed.
 *
 * Since: 2.6
 **/
G_CONST_RETURN gchar * G_CONST_RETURN * 
g_get_language_names (void)
{
  static GStaticPrivate cache_private = G_STATIC_PRIVATE_INIT;
  GLanguageNamesCache *cache = g_static_private_get (&cache_private);
  const gchar *value;

  if (!cache)
    {
      cache = g_new0 (GLanguageNamesCache, 1);
      g_static_private_set (&cache_private, cache, language_names_cache_free);
    }

  value = guess_category_value ("LC_MESSAGES");
  if (!value)
    value = "C";

  if (!(cache->languages && strcmp (cache->languages, value) == 0))
    {
      gchar **languages;
      gchar **alist, **a;
      GSList *list, *l;
      gint i;

      g_free (cache->languages);
      g_strfreev (cache->language_names);
      cache->languages = g_strdup (value);

      alist = g_strsplit (value, ":", 0);
      list = NULL;
      for (a = alist; *a; a++)
	{
	  gchar *b = unalias_lang (*a);
	  list = g_slist_concat (list, _g_compute_locale_variants (b));
	}
      g_strfreev (alist);
      list = g_slist_append (list, g_strdup ("C"));

      cache->language_names = languages = g_new (gchar *, g_slist_length (list) + 1);
      for (l = list, i = 0; l; l = l->next, i++)
	languages[i] = l->data;
      languages[i] = NULL;

      g_slist_free (list);
    }

  return (G_CONST_RETURN gchar * G_CONST_RETURN *) cache->language_names;
}

/**
 * g_strv_length:
 * @str_array: a %NULL-terminated array of strings.
 * 
 * Returns the length of the given %NULL-terminated 
 * string array @str_array.
 * 
 * Return value: length of @str_array.
 *
 * Since: 2.6
 **/
guint
g_strv_length (gchar **str_array)
{
  guint i = 0;

  g_return_val_if_fail (str_array != NULL, 0);

  while (str_array[i])
    ++i;

  return i;
}
