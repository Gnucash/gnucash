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


/* We assume that data is actually a char**. The way we return results
 * from this function is to malloc a fresh string, and store it in
 * this pointer. It is the caller's responsibility to do something
 * smart with this freshly allocated storage. the caller can determine
 * whether there was an error by initializing the char* passed in to
 * NULL. If there is an error, the char string will not be NULL on
 * return. */
static SCM
gfec_catcher(void *data, SCM tag, SCM throw_args)
{
    SCM func;
    SCM result;
    char *msg = NULL;

    func = scm_c_eval_string("gnc:error->string");
    if (SCM_PROCEDUREP(func))
    {
        result = scm_call_2(func, tag, throw_args);
        if (SCM_STRINGP(result))
            msg = SCM_STRING_CHARS(result);
    }

    if (msg == NULL)
    {
        msg = "Error running guile function.";
    }

    *(char**)data = strdup(msg);

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
gfec_file_helper(void *data)
{
    char *file = data;

    return scm_c_primitive_load(file);
}

SCM
gfec_eval_file(const char *file, gfec_error_handler error_handler)
{
    char *err_msg = NULL;
    SCM result;

    result = scm_internal_stack_catch(SCM_BOOL_T,
                                      gfec_file_helper,
                                      (void *) file,
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

