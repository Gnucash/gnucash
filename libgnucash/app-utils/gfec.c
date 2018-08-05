/*  Authors: Eric M. Ludlam <zappo@ultranet.com>
 *           Russ McManus <russell.mcmanus@gs.com>
 *           Dave Peticolas <dave@krondo.com>
 *
 *  gfec stands for 'guile fancy error catching'.
 *  This code is in the public domain.
 */

#include <assert.h>
#include <string.h>

#include <config.h>
#include "gfec.h"
#include "gnc-guile-utils.h"
#include "platform.h"
#include <glib.h>
#if COMPILER(MSVC)
# define strdup _strdup
#endif

static SCM
gfec_string_from_utf8(void *data)
{
    char *str = (char*)data;
    return scm_from_utf8_string(str);
}

static SCM
gfec_string_from_locale(void *data)
{
    char *str = (char*)data;
    return scm_from_locale_string(str);
}

static SCM
gfec_string_outer_handler(void *data, SCM key, SCM args)
{
    return SCM_UNDEFINED;
}

static SCM
gfec_string_inner_handler(void *data, SCM key, SCM args)
{
    char *str = (char*)data;
    SCM scm_string =  scm_internal_catch(SCM_BOOL_T,
					 gfec_string_from_locale, (void*)str,
					 gfec_string_inner_handler,
					 (void*)str);
    return scm_string;
}

SCM
gfec_eval_string(const char *str, gfec_error_handler error_handler)
{
    SCM result = SCM_UNDEFINED;
    SCM func = scm_c_eval_string("gnc:eval-string-with-error-handling");
    if (scm_is_procedure(func))
    {
        char *err_msg = NULL;
        SCM call_result, error = SCM_UNDEFINED;
	/* Deal with the possibility that scm_from_utf8_string will
	 * throw, falling back to scm_from_locale_string. If that fails, log a
	 * warning and punt.
	 */
	SCM scm_string = scm_internal_catch(SCM_BOOL_T,
					    gfec_string_from_utf8, (void*)str,
					    gfec_string_inner_handler,
					    (void*)str);
	if (!scm_string)
	{
	    error_handler("Contents could not be interpreted as UTF-8 or the current locale/codepage.");
	    return result;
	}
        call_result = scm_call_1 (func, scm_string);

        error = scm_list_ref (call_result, scm_from_uint (1));
        if (scm_is_true (error))
            err_msg = gnc_scm_to_utf8_string (error);
        else
            result = scm_list_ref (call_result, scm_from_uint (0));

        if (err_msg != NULL)
        {
            if (error_handler)
                error_handler (err_msg);

            free(err_msg);
        }
    }

    return result;
}

SCM
gfec_eval_file(const char *file, gfec_error_handler error_handler)
{
    gchar *contents = NULL;
    GError *save_error = NULL;
    SCM result;

    if (!g_file_get_contents (file, &contents, NULL, &save_error))
    {
        gchar *full_msg = g_strdup_printf ("Couldn't read contents of %s.\nReason: %s", file, save_error->message);
        error_handler(full_msg);

        g_error_free (save_error);
        g_free(full_msg);

        return SCM_UNDEFINED;
    }

    result = gfec_eval_string (contents, error_handler);
    g_free (contents);

    if (!result)
    {
        gchar *full_msg = g_strdup_printf ("Couldn't read contents of %s", file);
        error_handler(full_msg);

        g_free(full_msg);
    }

    return result;
}

SCM
gfec_apply(SCM proc, SCM arglist, gfec_error_handler error_handler)
{
    SCM result = SCM_UNDEFINED;
    SCM func = scm_c_eval_string("gnc:apply-with-error-handling");
    if (scm_is_procedure(func))
    {
        char *err_msg = NULL;
        SCM call_result, error;
        call_result = scm_call_2 (func, proc, arglist);

        error = scm_list_ref (call_result, scm_from_uint (1));
        if (scm_is_true (error))
            err_msg = gnc_scm_to_utf8_string (error);
        else
            result = scm_list_ref (call_result, scm_from_uint (0));

        if (err_msg != NULL)
        {
            if (error_handler)
                error_handler (err_msg);

            free(err_msg);
        }
    }

    return result;
}

static int error_in_scm_eval = FALSE;

static void
error_handler(const char *msg)
{
    g_warning("%s", msg);
    error_in_scm_eval = TRUE;
}

gboolean
gfec_try_load(const gchar *fn)
{
    g_debug("looking for %s", fn);
    if (g_file_test(fn, G_FILE_TEST_EXISTS))
    {
        g_debug("trying to load %s", fn);
        error_in_scm_eval = FALSE;
        gfec_eval_file(fn, error_handler);
        return !error_in_scm_eval;
    }
    return FALSE;
}
