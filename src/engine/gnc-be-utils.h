/********************************************************************\
 * gnc-be-utils.h: api for data storage backend                       *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/
/*
 * gnc-be-utils.h -- QOF Backend Utilities
 *        common code used by objects to define begin_edit() and
 *        commit_edit() functions.
 *
 * Written by:        Derek Atkins <derek@ihtfp.com>
 *
 */

#ifndef GNC_BE_UTILS_H
#define GNC_BE_UTILS_H

#include "gnc-engine-util.h"
#include "qofbackend-p.h"
#include "qofbook.h"

/* begin_edit helper
 *
 * @args:
 *        inst: an instance of QofInstance
 *
 * The caller should use this macro first and then perform any other operations.
 */

#define GNC_BEGIN_EDIT(inst)                                        \
  QofBackend * be;                                                  \
  if (!(inst)) return;                                              \
                                                                    \
  (inst)->editlevel++;                                              \
  if (1 < (inst)->editlevel) return;                                \
                                                                    \
  if (0 >= (inst)->editlevel)                                       \
  {                                                                 \
    PERR ("unbalanced call - resetting (was %d)", (inst)->editlevel); \
    (inst)->editlevel = 1;                                          \
  }                                                                 \
  ENTER ("inst=%p", (inst));                                        \
                                                                    \
  /* See if there's a backend.  If there is, invoke it. */          \
  be = qof_book_get_backend ((inst)->book);                         \
  if (be && be->begin) {                                            \
     (be->begin) (be, (inst)->entity.e_type, (inst));               \
  } else {                                                          \
     /* We tried and failed to start transaction! */                \
     (inst)->dirty = TRUE;                                          \
  }


/*
 * commit_edit helpers
 *
 * The caller should call PART1 as the first thing, then 
 * perform any local operations prior to calling the backend.
 * Then call PART2.  
 */

/*
 * part1 -- deal with the editlevel
 * 
 * @args:
 *        inst: an instance of QofInstance
 */

#define GNC_COMMIT_EDIT_PART1(inst) {                            \
  if (!(inst)) return;                                           \
                                                                 \
  (inst)->editlevel--;                                           \
  if (0 < (inst)->editlevel) return;                             \
                                                                 \
  /* The pricedb sufffers from delayed update...     */          \
  /* This may be setting a bad precedent for other types, I fear. */ \
  /* Other types probably really should handle begin like this. */ \
  if ((-1 == (inst)->editlevel) && (inst)->dirty)                \
  {                                                              \
    QofBackend * be;                                             \
    be = qof_book_get_backend ((inst)->book);                    \
    if (be && be->begin) {                                       \
     (be->begin) (be, (inst)->entity.e_type, (inst));            \
    }                                                            \
    (inst)->editlevel = 0;                                       \
  }                                                              \
  if (0 > (inst)->editlevel)                                     \
  {                                                              \
    PERR ("unbalanced call - resetting (was %d)", (inst)->editlevel); \
    (inst)->editlevel = 0;                                       \
  }                                                              \
  ENTER ("inst=%p, dirty=%d do-free=%d",                         \
            (inst), (inst)->dirty, (inst)->do_free);             \
}

/*
 * part2 -- deal with the backend
 * 
 * @args:
 *        inst: an instance of QofInstance
 *        on_error: a function called if there is a backend error.
 *                void (*on_error)(inst, QofBackendError)
 *        on_done: a function called after the commit is complete 
 *                but before the instect is freed. Perform any other 
 *                operations after the commit.
 *                void (*on_done)(inst)
 *        on_free: a function called if inst->do_free is TRUE. 
 *                void (*on_free)(inst)
 */
#define GNC_COMMIT_EDIT_PART2(inst,on_error,on_done,on_free) {   \
  QofBackend * be;                                               \
                                                                 \
  /* See if there's a backend.  If there is, invoke it. */       \
  be = qof_book_get_backend ((inst)->book);                      \
  if (be && be->commit)                                          \
  {                                                              \
    QofBackendError errcode;                                     \
                                                                 \
    /* clear errors */                                           \
    do {                                                         \
      errcode = qof_backend_get_error (be);                      \
    } while (ERR_BACKEND_NO_ERR != errcode);                     \
                                                                 \
    (be->commit) (be, (inst)->entity.e_type, (inst));            \
    errcode = qof_backend_get_error (be);                        \
    if (ERR_BACKEND_NO_ERR != errcode)                           \
    {                                                            \
      /* XXX Should perform a rollback here */                   \
      (inst)->do_free = FALSE;                                   \
                                                                 \
      /* Push error back onto the stack */                       \
      qof_backend_set_error (be, errcode);                       \
      (on_error)((inst), errcode);                               \
    }                                                            \
    /* XXX the backend commit code should clear dirty!! */       \
    (inst)->dirty = FALSE;                                       \
  }                                                              \
  (on_done)(inst);                                               \
                                                                 \
  LEAVE ("inst=%p, dirty=%d do-free=%d",                         \
            (inst), (inst)->dirty, (inst)->do_free);             \
  if ((inst)->do_free) {                                         \
     (on_free)(inst);                                            \
     return;                                                     \
  }                                                              \
}


#endif /* GNC_BE_UTILS_H */
