/*
 * gnc-be-utils.h -- GnuCash Backend Utilities
 *	common code used by objects to define begin_edit() and
 *	commit_edit() functions.
 *
 * Written by:	Derek Atkins <derek@ihtfp.com>
 *
 */

#ifndef GNC_BE_UTILS_H
#define GNC_BE_UTILS_H

#include "BackendP.h"
#include "gnc-book.h"
#include "gnc-engine-util.h"

/* begin_edit helper
 *
 * assumes:
 *	obj->editlevel	(int)
 *	obj->book	(GNCBook*)
 *
 * @args:
 *	obj: the object to begin editing
 *	type: the object type
 *
 * The caller should use this macro first and then perform any other operations.
 */

#define GNC_BEGIN_EDIT(obj,type) { \
  Backend * be; \
  if (!(obj)) return; \
  \
  (obj)->editlevel++; \
  if (1 < (obj)->editlevel) return; \
  \
  if (0 >= (obj)->editlevel) \
  { \
    PERR ("unbalanced call - resetting (was %d)", (obj)->editlevel); \
    (obj)->editlevel = 1; \
  } \
  \
  /* See if there's a backend.  If there is, invoke it. */ \
  be = gnc_book_get_backend ((obj)->book); \
  if (be && be->begin) { \
     (be->begin) (be, (type), (obj)); \
  } \
}


/*
 * commit_edit helpers
 *
 * The caller should call PART1 as the first thing, then 
 * perform any local operations prior to calling the backend.
 * Then call PART2.  You cannot do anything after PART2.
 *
 * assumes:
 *	obj->editlevel	(int)
 *	obj->book	(GNCBook*)
 *	obj->do_free	(gboolean)
 */

/*
 * part1 -- deal with the editlevel
 * 
 * assumes:
 *	obj->editlevel	(int)
 *
 * @args:
 *	obj: the object being committed
 */

#define GNC_COMMIT_EDIT_PART1(obj) { \
  if (!(obj)) return; \
  \
  (obj)->editlevel--; \
  if (0 < (obj)->editlevel) return; \
  \
  if (0 > (obj)->editlevel) \
  { \
    PERR ("unbalanced call - resetting (was %d)", (obj)->editlevel); \
    (obj)->editlevel = 0; \
  } \
}

/*
 * part2 -- deal with the backend
 * 
 * assumes:
 *	obj->book	(GNCBook*)
 *	obj->do_free	(gboolean)
 *
 * @args:
 *	obj: the object being committed
 *	type: the type of the object
 *	on_error: a function called if there is a backend error.
 *		void (*on_error)(obj, GNCBackendError)
 *	on_done: a function called after the commit is complete but before
 *		the object is freed.  This is where you clear the "dirty"
 *		flag, and perform any other operations after the commit.
 *		void (*on_done)(obj)
 *	on_free: a function called if obj->do_free is TRUE. 
 *		void (*on_free)(obj)
 */
#define GNC_COMMIT_EDIT_PART2(obj,type,on_error,on_done,on_free) { \
  Backend * be; \
  \
  /* See if there's a backend.  If there is, invoke it. */ \
  be = gnc_book_get_backend ((obj)->book); \
  if (be && be->commit) \
  { \
    GNCBackendError errcode; \
    \
    /* clear errors */ \
    do { \
      errcode = xaccBackendGetError (be); \
    } while (ERR_BACKEND_NO_ERR != errcode); \
    \
    (be->commit) (be, (type), (obj)); \
    errcode = xaccBackendGetError (be); \
    if (ERR_BACKEND_NO_ERR != errcode) \
    { \
      (obj)->do_free = FALSE; \
      (on_error)((obj), errcode); \
      xaccBackendSetError (be, errcode); \
    } \
  } \
  (on_done)(obj);\
  \
  if ((obj)->do_free) (on_free)(obj); \
}


#endif /* GNC_BE_UTILS_H */
