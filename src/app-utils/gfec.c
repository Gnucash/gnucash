/*  Authors: Eric M. Ludlam <zappo@ultranet.com>
 *           Russ McManus <russell.mcmanus@gs.com>
 *           Dave Peticolas <dave@krondo.com>
 *
 *  gfec stands for 'guile fancy error catching'.
 *  This code is in the public domain.
 */

#include <assert.h>
#include <string.h>

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

  func = gh_eval_str("gnc:error->string");
  if (gh_procedure_p(func))
  {
    result = gh_call2(func, tag, throw_args);
    if (gh_string_p(result))
      msg = gh_scm2newstr(result, NULL);
  }

  if (msg == NULL)
  {
    msg = strdup("Error running guile function.");
    assert(msg != NULL);
  }

  *(char**)data = msg;

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

  return gh_eval_file(file);
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

  return gh_eval_str(string);
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

  return gh_apply(apply_rec->proc, apply_rec->arglist);
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
