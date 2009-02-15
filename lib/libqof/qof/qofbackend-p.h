/********************************************************************\
 * qofbackend-p.h -- private api for data storage backend           *
 *                                                                  *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/
/** @addtogroup Object
    @{ */
/** @addtogroup Object_Private
    Private interfaces, not meant to be used by applications.
    @{ */
/** @name  Backend_Private
   Pseudo-object defining how the engine can interact with different
   back-ends (which may be SQL databases, or network interfaces to 
   remote QOF servers. File-io is just one type of backend).
   
   The callbacks will be called at the appropriate times during 
   a book session to allow the backend to store the data as needed.

   @file qofbackend-p.h
   @brief private api for data storage backend
   @author Copyright (c) 2000,2001,2004 Linas Vepstas <linas@linas.org> 
   @author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
@{ */

#ifndef QOF_BACKEND_P_H
#define QOF_BACKEND_P_H

#include "qofbackend.h"
#include "qofbook.h"
#include "qofinstance-p.h"
#include "qofquery.h"
#include "qofsession.h"

/**
 * The backend_new routine sets the functions that will be used 
 *    by the backend to perform the actions required by QOF. A
 *    basic minimum is session_begin, session_end, load and
 *    sync. Any unused functions should be set to NULL. If the
 *    backend uses configuration options, backend_new must ensure
 *    that these are set to usable defaults before returning. To use
 *    configuration options, load_config and get_config must also
 *    be defined.
 * 
 * The session_begin() routine gives the backend a second initialization
 *    opportunity.  It is suggested that the backend check that 
 *    the URL is syntactically correct, and that it is actually
 *    reachable.  This is probably(?) a good time to initialize
 *    the actual network connection.
 *
 *    The 'ignore_lock' argument indicates whether the single-user
 *    lock on the backend should be cleared.  The typical GUI sequence
 *    leading to this is: (1) GUI attempts to open the backend
 *    by calling this routine with FALSE==ignore_lock.  (2) If backend  
 *    error'ed BACKEND_LOCK, then GUI asks user what to do. (3) if user 
 *    answers 'break & enter' then this routine is called again with
 *    TRUE==ignore_lock.
 *
 *    The 'create_if_nonexistent' argument indicates whether this
 *    routine should create a new 'database', if it doesn't already
 *    exist. For example, for a file-backend, this would create the
 *    file, if it didn't already exist.  For an SQL backend, this
 *    would create the database (the schema) if it didn't already 
 *    exist.  This flag is used to implement the 'SaveAs' GUI, where
 *    the user requests to save data to a new backend.
 *
 * The load() routine should load the minimal set of application data
 *    needed for the application to be operable at initial startup.
 *    It is assumed that the application will perform a 'run_query()'
 *    to obtain any additional data that it needs.  For file-based
 *    backends, it is acceptable for the backend to return all data
 *    at load time; for SQL-based backends, it is acceptable for the
 *    backend to return no data.
 *
 *    Thus, for example, the GnuCash postgres backend returned
 *    the account tree, all currencies, and the pricedb, as these
 *    were needed at startup.  It did not have to return any
 *    transactions whatsoever, as these were obtained at a later stage
 *    when a user opened a register, resulting in a query being sent to
 *    the backend.
 *
 *    (Its OK to send over entities at this point, but one should 
 *    be careful of the network load; also, its possible that whatever 
 *    is sent is not what the user wanted anyway, which is why its 
 *    better to wait for the query).
 *
 * The begin() routine is called when the engine is about to
 *    make a change to a data structure. It can provide an advisory
 *    lock on data.
 *
 * The commit() routine commits the changes from the engine to the
 *    backend data storage.
 *
 * The rollback() routine is used to revert changes in the engine
 *    and unlock the backend. 
 *
 *    If the second user tries to modify an entity that
 *    the first user deleted, then the backend should set the error
 *    to ERR_BACKEND_MOD_DESTROY from this routine, so that the 
 *    engine can properly clean up.
 *
 * The compile_query() method compiles a QOF query object into
 *    a backend-specific data structure and returns the compiled
 *    query. For an SQL backend, the contents of the query object
 *    need to be turned into a corresponding SQL query statement, and
 *    sent to the database for evaluation.
 *
 * The free_query() method frees the data structure returned from 
 *    compile_query()
 *
 * The run_query() callback takes a compiled query (generated by
 *    compile_query) and runs the query in across the backend,
 *    inserting the responses into the engine. The database will
 *    return a set of splits and transactions and this callback needs
 *    to poke these into the account-group hierarchy held by the query
 *    object.
 *
 *    For a network-communications backend, essentially the same is 
 *    done, except that this routine would convert the query to wire 
 *    protocol, get an answer from the remote server, and push that
 *    into the account-group object.
 *
 *    The returned list of entities can be used to build a local
 *    cache of the matching data.  This will allow the QOF client to 
 *    continue functioning even when disconnected from the server:
 *    this is because it will have its local cache of data from which to work.
 *
 * The sync() routine synchronizes the engine contents to the backend.
 *    This should done by using version numbers (hack alert -- the engine
 *    does not currently contain version numbers).
 *    If the engine contents are newer than what is in the backend, the 
 *    data is stored to the backend. If the engine contents are older,
 *    then the engine contents are updated.  
 *
 *    Note that this sync operation is only meant to apply to the 
 *    current contents of the engine. This routine is not intended
 *    to be used to fetch entity data from the backend.
 *
 *    File based backends tend to use sync as if it was called dump.
 *    Data is written out into the backend, overwriting the previous
 *    data. Database backends should implement a more intelligent 
 *    solution.
 *
 * The counter() routine increments the named counter and returns the
 *    post-incremented value.  Returns -1 if there is a problem.
 *
 * The events_pending() routines should return true if there are
 *    external events which need to be processed to bring the
 *    engine up to date with the backend.
 *
 * The process_events() routine should process any events indicated
 *    by the events_pending() routine. It should return TRUE if
 *    the engine was changed while engine events were suspended.
 *
 * The last_err member indicates the last error that occurred.
 *    It should probably be implemented as an array (actually,
 *    a stack) of all the errors that have occurred.
 *
 * For support of book partitioning, use special "Book"  begin_edit()
 *    and commit_edit() QOF_ID types.
 *
 *    Call the book begin() at the begining of a book partitioning.  A
 *    'partitioning' is the splitting off of a chunk of the current
 *    book into a second book by means of a query.  Every transaction
 *    in that query is to be moved ('transfered') to the second book
 *    from the existing book.  The argument of this routine is a
 *    pointer to the second book, where the results of the query
 *    should go.
 *
 *    Call the book commit() to complete the book partitioning.
 *
 *    After the begin(), there will be a call to run_query(), followed
 *    probably by a string of object calls, and completed by commit().  
 *    It should be explicitly understood that the results of that 
 *    run_query() precisely constitute the set of objects that are to 
 *    be moved between the initial and the new book. This specification 
 *    can be used by a clever backend to avoid excess data movement 
 *    between the server and the QOF client, as explained below.
 *
 *    There are several possible ways in which a backend may choose to
 *    implement the book splitting process.  A 'file-type' backend may
 *    choose to ignore this call, and the subsequent query, and simply
 *    write out the new book to a file when the commit() call is made.
 *    By that point, the engine will have performed all of the
 *    nitty-gritty of moving transactions from one book to the other.
 * 
 *    A 'database-type' backend has several interesting choices.  One
 *    simple choice is to simply perform the run_query() as it
 *    normally would, and likewise treat the object edits as usual.  
 *    In this scenario, the commit() is more or less a no-op.  
 *    This implementation has a drawback, however: the run_query() may 
 *    cause the transfer of a <b>huge</b> amount of data between the backend 
 *    and the engine.  For a large dataset, this is quite undesirable. 
 *    In addition, there are risks associated with the loss of network 
 *    connectivity during the transfer; thus a partition might terminate 
 *    half-finished, in some indeterminate state, due to network errors.
 *    It might be difficult to recover from such errors: the engine does 
 *    not take any special safety measures during the transfer.
 *
 *    Thus, for a large database, an alternate implementation 
 *    might be to use the run_query() call as an opportunity to 
 *    transfer entities between the two books in the database,
 *    and not actually return any new data to the engine.  In
 *    this scenario, the engine will attempt to transfer those 
 *    entities that it does know about.  It does not, however,
 *    need to know about all the other entities that also would  
 *    be transfered over.  In this way, a backend could perform
 *    a mass transfer of entities between books without having
 *    to actually move much (or any) data to the engine.
 *
 * To support configuration options from the frontend, the backend
 *    can be passed a KvpFrame - according to the allowed options
 *    for that backend, using load_config(). Configuration can be
 *    updated at any point - it is up to the frontend to load the
 *    data in time for whatever the backend needs to do. e.g. an
 *    option to save a new book in a compressed format need not be
 *    loaded until the backend is about to save. If the configuration
 *    is updated by the user, the frontend should call load_config
 *    again to update the backend.
 *
 *    Backends are responsible for ensuring that any supported
 *    configuration options are initialised to usable values.
 *    This should be done in the function called from backend_new.
 */

struct QofBackendProvider_s
{
  /** Some arbitrary name given for this particular backend provider */
  const char * provider_name;

  /** The access method that this provider provides, for example,
   *  file:// http:// postgres:// or sqlite://, but without the :// at the end
   */
  const char * access_method;

  /** \brief Partial QofBook handler
	
	TRUE if the backend handles external references
	to entities outside this book and can save a QofBook that
	does not contain any specific QOF objects.
	*/
  gboolean partial_book_supported;
	
  /** Return a new, fully initialized backend. 
   *
   * If the backend supports configuration, all configuration options
   * should be initialised to usable values here.
   * */
  QofBackend * (*backend_new) (void);

/** \brief Distinguish two providers with same access method.
  
  More than 1 backend can be registered under the same access_method,
  so each one is passed the path to the data (e.g. a file) and
  should return TRUE only:
-# if the backend recognises the type as one that it can load and write or 
-# if the path contains no data but can be used (e.g. a new session).
  
  \note If the backend can cope with more than one type, the backend
  should not try to store or cache the sub-type for this data.
  It is sufficient only to return TRUE if any ONE of the supported
  types match the incoming data. The backend should not assume that
  returning TRUE will mean that the data will naturally follow.
  */
  gboolean (*check_data_type) (const char*);
  
  /** Free this structure, unregister this backend handler. */
  void (*provider_free) (QofBackendProvider *);
};

typedef enum {
  LOAD_TYPE_INITIAL_LOAD,
  LOAD_TYPE_LOAD_ALL
} QofBackendLoadType;

struct QofBackend_s
{
  void (*session_begin) (QofBackend *be,
                         QofSession *session,
                         const char *book_id, 
                         gboolean ignore_lock,
                         gboolean create_if_nonexistent);
  void (*session_end) (QofBackend *);
  void (*destroy_backend) (QofBackend *);

  void (*load) (QofBackend *, QofBook *, QofBackendLoadType);

  void (*begin) (QofBackend *, QofInstance *);
  void (*commit) (QofBackend *, QofInstance *);
  void (*rollback) (QofBackend *, QofInstance *);

  gpointer (*compile_query) (QofBackend *, QofQuery *);
  void (*free_query) (QofBackend *, gpointer);
  void (*run_query) (QofBackend *, gpointer);

  void (*sync) (QofBackend *, QofBook *);
  void (*load_config) (QofBackend *, KvpFrame *);
  KvpFrame* (*get_config) (QofBackend *);
  gint64 (*counter) (QofBackend *, const char *counter_name);

  gboolean (*events_pending) (QofBackend *);
  gboolean (*process_events) (QofBackend *);

  QofBePercentageFunc percentage;
  
  QofBackendProvider *provider;

  /** Detect if the sync operation will overwrite data
   *
   * File based backends tend to consider the original file
   * as 'stale' immediately the data finishes loading. New data
   * only exists in memory and the data in the file is completely
   * replaced when qof_session_save is called. e.g. this routine can be
   * used to detect if a Save As... operation would overwrite a
   * possibly unrelated file. Not all file backends use this function.
   *
   * @return TRUE if the user may need to be warned about possible
   * data loss, otherwise FALSE.
   */
  gboolean (*save_may_clobber_data) (QofBackend *);

  QofBackendError last_err;
  char * error_msg;

  KvpFrame* backend_configuration;
  gint config_count;
  /** Each backend resolves a fully-qualified file path.
   * This holds the filepath and communicates it to the frontends.
   */
  char * fullpath;

  /** \deprecated price_lookup should be removed during the redesign
   * of the SQL backend... prices can now be queried using
   * the generic query mechanism.
   *
   * Note the correct signature for this call is 
   * void (*price_lookup) (QofBackend *, GNCPriceLookup *);
   * we use gpointer to avoid an unwanted include file dependency. 
   */
  void (*price_lookup) (QofBackend *, gpointer);

  /** \deprecated Export should really _NOT_ be here, but is left here for now.
   * I'm not sure where this should be going to. It should be
   * removed ASAP.   This is a temporary hack-around until period-closing
   * is fully implemented.
   */
  void (*export) (QofBackend *, QofBook *);

};

/** Let the sytem know about a new provider of backends.  This function
 *  is typically called by the provider library at library load time.
 *  This function allows the backend library to tell the QOF infrastructure
 *  that it can handle URL's of a certain type.  Note that a single
 *  backend library may register more than one provider, if it is
 *  capable of handling more than one URL access method.
 */
void qof_backend_register_provider (QofBackendProvider *);

/** The qof_backend_set_message() assigns a string to the backend error message.
 */
void qof_backend_set_message(QofBackend *be, const char *format, ...);

/** The qof_backend_get_message() pops the error message string from
 *  the Backend.  This string should be freed with g_free().
 */
char * qof_backend_get_message(QofBackend *be);

void qof_backend_init(QofBackend *be);
void qof_backend_destroy(QofBackend *be);

/** Allow backends to see if the book is open 

@return 'y' if book is open, otherwise 'n'.
*/
gchar qof_book_get_open_marker(const QofBook *book);

/** get the book version

used for tracking multiuser updates in backends.

@return -1 if no book exists, 0 if the book is
new, otherwise the book version number.
*/
gint32 qof_book_get_version (const QofBook *book);

void qof_book_set_version (QofBook *book, gint32 version);

/* @} */
/* @} */
/* @} */
#endif /* QOF_BACKEND_P_H */
