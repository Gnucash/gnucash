/*  Authors: Eric M. Ludlam <zappo@ultranet.com>
 *           Russ McManus <russell.mcmanus@gs.com>
 *           Dave Peticolas <dave@krondo.com>
 *
 *  gfec stands for 'guile fancy error catching'.
 *  This code is in the public domain.
 */

#include <assert.h>
#include <string.h>

#include "config.h"
#include "gfec.h"
#include "gnc-guile-utils.h"
#include "platform.h"
#include <glib.h>
#if COMPILER(MSVC)
# define strdup _strdup
#endif

typedef struct {
    char **msg;
    SCM *scm_string;
} helper_data_t;

static SCM helper_scm_to_string(void *ptr_void)
{
    helper_data_t* ptr = ptr_void;
    g_assert(ptr);
    *(ptr->msg) = gnc_scm_to_utf8_string(*ptr->scm_string);
    return SCM_UNDEFINED;
}


static int gfec_catcher_recursion_level = 0;

/* We assume that data is actually a char**. The way we return results
 * from this function is to malloc a fresh string, and store it in
 * this pointer. It is the caller's responsibility to do something
 * smart with this freshly allocated storage. the caller can determine
 * whether there was an error by initializing the char* passed in to
 * NULL. If there is an error, the char string will not be NULL on
 * return.
 *
 * This function might call itself recursively: The conversion of the error
 * object to a string might itself throw an exception, hence the scm_to_string
 * function must be wrapped into a stack_catch block as well. To avoid infinite
 * recursion, we check the recursion level by gfec_catcher_recursion_level.
 */
static SCM
gfec_catcher(void *data, SCM tag, SCM throw_args)
{
    SCM func;
    SCM result;
    char *msg = NULL;

    // To much recursion? Better jump out of here quickly.
    if (gfec_catcher_recursion_level > 2)
    {
        *(char**)data = strdup("Guile error: Too many recursions in error catch handler.");
        return SCM_UNDEFINED;
    }

    gfec_catcher_recursion_level++;

    func = scm_c_eval_string("gnc:error->string");
    if (scm_is_procedure(func))
    {
        result = scm_call_2(func, tag, throw_args);
        if (scm_is_string(result))
        {
            char *internal_err_msg = NULL;
            helper_data_t helper_data;

            helper_data.msg = &msg;
            helper_data.scm_string = &result;

            // The conversion to string can itself throw as well
            scm_internal_stack_catch(SCM_BOOL_T,
                                     helper_scm_to_string,
                                     (void *) &helper_data,
                                     gfec_catcher,
                                     &internal_err_msg);
            // Previously: msg = gnc_scm_to_utf8_string (result);

            // Did we run into an exception? Then the output argument msg is
            // not set (due to the exception), but err_msg is set and contains
            // that error message. We thus pass the err_msg instead of msg to
            // our caller.
            if (internal_err_msg)
            {
                msg = internal_err_msg;
            }
        }
    }

    if (msg == NULL)
    {
        *(char**)data = strdup("Error running guile function.");
    }
    else
    {
        *(char**)data = strdup(msg);
        g_free(msg);
    }

    --gfec_catcher_recursion_level;
    return SCM_UNDEFINED;
}


/* The arguments to scm_internal_stack_catch:
   ------------------------------------------
   SCM tag                     : this should be SCM_BOOL_T to catch all errors.
   scm_catch_body_t body       : the function to run.
   void *body_data             : a pointer to pass to body
   scm_catch_handler_t handler : the hander function
   void *handler_data          : a pointer to pass to the handler
*/

static SCM
gfec_string_helper(void *data)
{
    char *string = data;

    return scm_c_eval_string(string);
}

SCM
gfec_eval_string(const char *str, gfec_error_handler error_handler)
{
    char *err_msg = NULL;
    SCM result;

    result = scm_internal_stack_catch(SCM_BOOL_T,
                                      gfec_string_helper,
                                      (void *) str,
                                      gfec_catcher,
                                      &err_msg);

    if (err_msg != NULL)
    {
        if (error_handler)
            error_handler(err_msg);

        free(err_msg);

        return SCM_UNDEFINED;
    }

    return result;
}

SCM
gfec_eval_file(const char *file, gfec_error_handler error_handler)
{
    char *err_msg = NULL;
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

    return result;
}

struct gfec_apply_rec
{
    SCM proc;
    SCM arglist;
};

static SCM
gfec_apply_helper(void *data)
{
    struct gfec_apply_rec *apply_rec = (struct gfec_apply_rec *)data;

    return scm_apply(apply_rec->proc, apply_rec->arglist, SCM_EOL);
}

SCM
gfec_apply(SCM proc, SCM arglist, gfec_error_handler error_handler)
{
    char *err_msg = NULL;
    struct gfec_apply_rec apply_rec;
    SCM result;

    apply_rec.proc = proc;
    apply_rec.arglist = arglist;

    result = scm_internal_stack_catch(SCM_BOOL_T,
                                      gfec_apply_helper,
                                      &apply_rec,
                                      gfec_catcher,
                                      &err_msg);

    if (err_msg != NULL)
    {
        if (error_handler)
            error_handler(err_msg);

        free(err_msg);

        return SCM_UNDEFINED;
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
gfec_try_load(gchar *fn)
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

