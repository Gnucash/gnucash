changecom(`/*', `*/')

/* data dictionary for the gnucash tables */
/* sql table description and manipulation macros */


define(`account', `gncAccount, Account,
       accountName,    , char *, xaccAccountGetName(ptr),
       accountCode,    , char *, xaccAccountGetCode(ptr),
       description,    , char *, xaccAccountGetDescription(ptr),
       notes,          , char *, xaccAccountGetNotes(ptr),
       accountGUID, KEY, GUID *, xaccAccountGetGUID(ptr),
       ')


define(`split', `gncEntry, Split,
       accountGUID,     , GUID *,   xaccAccountGetGUID(xaccSplitGetAccount(ptr)),
       transGUID,       , GUID *,   xaccTransGetGUID(xaccSplitGetParent(ptr)),
       memo,            , char *,   xaccSplitGetMemo(ptr),
       action,          , char *,   xaccSplitGetAction(ptr),
       reconciled,      , char  ,   xaccSplitGetReconcile(ptr),
       date_reconciled, , Timespec, xaccSplitRetDateReconciledTS(ptr),
       entryGUID,    KEY, GUID *,   xaccSplitGetGUID(ptr),
       ')

define(`transaction', `gncTransaction, Transaction,
       num,            , char *,   xaccTransGetNum(ptr),
       description,    , char *,   xaccTransGetDescription(ptr),
       date_entered,   , Timespec, xaccTransRetDateEnteredTS(ptr),
       date_posted,    , Timespec, xaccTransRetDatePostedTS(ptr),
       transGUID,   KEY, GUID *,   xaccTransGetGUID(ptr),
       ')

/* ------------------------------------------------------- */
/* symbolic names for the table accessors */
define(`tablename', $1)
define(`xacc_type', $2)

define(`firstrec', `shift(shift($@))')
define(`nextrec', `shift(shift(shift(shift($@))))')

/* -------- */
/* macros that use teh sql builder to build a query */

define(`sql_setter', `ifelse($2, `KEY',
                     `ifelse($1, `char *',   sqlBuild_Where_Str,
                             $1, `GUID *',   sqlBuild_Where_GUID)',

                             $2,     ,
                     `ifelse($1, `char *',   sqlBuild_Set_Str,
                             $1, `GUID *',   sqlBuild_Set_GUID,
                             $1, `Timespec', sqlBuild_Set_Date,
                             $1, `char  ',   sqlBuild_Set_Char)')')


/* recursively walk the table, build the builders */

define(`set_fields_r', `ifelse($#, 1, , 
`   sql_setter($3,$2) (be->builder, "$1", $4);
set_fields_r(nextrec($@))')')

define(`set_fields', `set_fields_r(firstrec($@))')

/* -------- */
/* macros to compare a query result */

define(`cmp_value', `ifelse($1, `char *',   COMP_STR,
                            $1, `GUID *',   COMP_GUID,
                            $1, `Timespec', COMP_DATE,
                            $1, `char  ',   COMP_CHAR)')

/* recursively walk the table, build compare functions,
 * but only for non-primary-keys */

define(`cmp_fields_r', `ifelse($#, 1, , 
`ifelse($2, `KEY',  ,
`    cmp_value($3,$2) ("$1", $4, ndiffs);
cmp_fields_r(nextrec($@))')')')

define(`cmp_fields', `cmp_fields_r(firstrec($@))')

/* -------- */

define(`store_one_only', 
`
/* ------------------------------------------------------ */
/* This routine stores/updates one record in the database.
 * It does not do any traversals, it does not lock.  
 * It just pokes the data in 
 */

static void 
pgendStoreOne`'xacc_type($@)`'Only (PGBackend *be,
                     xacc_type($@) *ptr,
                     sqlBuild_QType update)
{
   const char *buf;
   ENTER ("be=%p, xacc_type($@)=%p", be, ptr);
   if (!be || !ptr) return;

   /* build the sql query */
   sqlBuild_Table (be->builder, "tablename($@)", update);
   set_fields($@)

   buf = sqlBuild_Query (be->builder);
   SEND_QUERY (be,buf, );

   /* complete/commit the transaction, check the status */
   FINISH_QUERY(be->connection);
   LEAVE (" ");
}

')

define(`compare_one_only', 
`
/* ------------------------------------------------------ */
/* This routine returns a positive int if the indicated transaction
 * differs from that in the SQL database.  It returns negative
 * number if theres an error.
 * It does not do any traversals, it does not lock.  
 */

static int
pgendCompareOne`'xacc_type($@)`'Only (PGBackend *be,
                     xacc_type($@) *ptr)
{
   const char *buf;
   PGresult *result;
   int i=0, nrows=0, ndiffs=0;

   ENTER ("be=%p, xacc_type($@)=%p", be, ptr);
   if (!be || !ptr) return -1;

   /* build the sql query */
   sqlBuild_Table (be->builder, "tablename($@)", SQL_SELECT);
   set_fields($@)

   buf = sqlBuild_Query (be->builder);
   SEND_QUERY (be,buf, -1);

   i=0; nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      IF_ONE_ROW (result, nrows, i) {

         /* compared queried values to input values */
         cmp_fields($@)
      }

      PQclear (result);
      i++;
   } while (result);

   if (0 == nrows) ndiffs = -1;
   LEAVE ("ndiffs=%d", ndiffs);
   return ndiffs;
}

')

/* ------------------------------------------------------- */

store_one_only(account)
store_one_only(transaction)
store_one_only(split)

compare_one_only(account)
compare_one_only(transaction)
compare_one_only(split)
