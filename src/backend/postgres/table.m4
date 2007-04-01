
divert(-1)
changecom(`/*', `*/')

/* data dictionary for the gnucash tables */
/* sql table description and manipulation macros */

define(`account', `gncAccount, Account, Account, a,
       accountName,    , char *, xaccAccountGetName(ptr),
       accountCode,    , char *, xaccAccountGetCode(ptr),
       description,    , char *, xaccAccountGetDescription(ptr),
       type,           , char *, xaccAccountTypeEnumAsString(xaccAccountGetType(ptr)),
       commodity,      , char *, gnc_commodity_get_unique_name(xaccAccountGetCommodity(ptr)),
       version,        , int32,  xaccAccountGetVersion(ptr),
       iguid,          , int32,  ptr->idata,
       bookGUID,       , GUID *, qof_instance_get_guid((QofInstance*)gnc_account_get_book(ptr)),
       parentGUID,     , GUID *, xaccAccountGetGUID(gnc_account_get_parent(ptr)),
       accountGUID, KEY, GUID *, xaccAccountGetGUID(ptr),
       ')

define(`book', `gncBook, Book, QofBook, b,
       book_open,      , char,   qof_book_get_open_marker(ptr),
       version,        , int32,  qof_book_get_version(ptr),
       iguid,          , int32,  qof_book_get_idata(ptr),
       bookGUID,    KEY, GUID *, qof_book_get_guid(ptr),
       ')

define(`split', `gncSplit, Split, Split, e,
       accountGUID,     , GUID *,   xaccAccountGetGUID(xaccSplitGetAccount(ptr)),
       transGUID,       , GUID *,   xaccTransGetGUID(xaccSplitGetParent(ptr)),
       memo,            , char *,   xaccSplitGetMemo(ptr),
       action,          , char *,   xaccSplitGetAction(ptr),
       reconciled,      , char,     xaccSplitGetReconcile(ptr),
       date_reconciled, , Timespec, xaccSplitRetDateReconciledTS(ptr),
       amount,          , int64,    gnc_numeric_num(xaccSplitGetAmount(ptr)),
       value,           , int64,    gnc_numeric_num(xaccSplitGetValue(ptr)),
       iguid,           , int32,    ptr->idata,
       splitGuid,    KEY, GUID *,   xaccSplitGetGUID(ptr),
       ')

/* note that for the last_modified, we use the sql database     */
/* notion of 'current time'. This should help prevent clock     */
/* skew problems between different simultaneous users           */
/* Note also this means that the table entry is not             */
/* last_modified,    , Timespec, xaccTransRetDateModifiedTS(ptr), */
/* as one might have guessed                                    */

define(`transaction', `gncTransaction, Transaction, Transaction, t,
       num,            , char *,   xaccTransGetNum(ptr),
       description,    , char *,   xaccTransGetDescription(ptr),
       currency,       , commod,   gnc_commodity_get_unique_name(xaccTransGetCurrency(ptr)),
       last_modified,  , now,      "NOW",
       date_entered,   , Timespec, xaccTransRetDateEnteredTS(ptr),
       date_posted,    , Timespec, xaccTransRetDatePostedTS(ptr),
       version,        , int32,    xaccTransGetVersion(ptr),
       iguid,          , int32,    ptr->idata,
       transGUID,   KEY, GUID *,   xaccTransGetGUID(ptr),
       ')


define(`modity', `gncCommodity, Commodity, gnc_commodity, c,
       namespace,    , char *, gnc_commodity_get_namespace(ptr),
       fullname,     , char *, gnc_commodity_get_fullname(ptr),
       mnemonic,     , char *, gnc_commodity_get_mnemonic(ptr),
       code,         , char *, gnc_commodity_get_cusip(ptr),
       fraction,     , int32,  gnc_commodity_get_fraction(ptr),
       commodity, KEY, char *, gnc_commodity_get_unique_name(ptr),
       ')
       

define(`price', `gncPrice, Price, GNCPrice, p,
       commodity,    , commod,   gnc_commodity_get_unique_name(gnc_price_get_commodity(ptr)),
       currency,     , commod,   gnc_commodity_get_unique_name(gnc_price_get_currency(ptr)),
       time,         , Timespec, gnc_price_get_time(ptr),
       source,       , char *,   gnc_price_get_source(ptr),
       type,         , char *,   gnc_price_get_type(ptr),
       valueNum,     , int64,    gnc_numeric_num(gnc_price_get_value(ptr)),
       valueDenom,   , int64,    gnc_numeric_denom(gnc_price_get_value(ptr)),
       version,      , int32,    gnc_price_get_version(ptr),
       bookGUID,     , GUID *,   qof_book_get_guid(gnc_price_get_book(ptr)),
       priceGUID, KEY, GUID *,   gnc_price_get_guid(ptr),
       ')
       

define(`checkpoint', `gncCheckpoint, Checkpoint, Checkpoint, x,
       balance,             , int64,    ptr->balance,
       cleared_balance,     , int64,    ptr->cleared_balance,
       reconciled_balance,  , int64,    ptr->reconciled_balance,
       date_start,          , Timespec, ptr->date_start,
       date_end,            , Timespec, ptr->date_end,
       commodity,           , char *,   ptr->commodity,
       accountGuid,         , GUID *,   ptr->account_guid,
       ')


define(`session', `gncSession, Session, void, x,
       session_mode,        , char *, pgendSessionGetMode(be),
       hostname,            , char *, pgendGetHostname(be),
       login_name,          , char *, pgendGetUsername(be),
       gecos,               , char *, pgendGetUserGecos(be),
       time_on,             , now,    "NOW",
       time_off,            , now,    "INFINITY",
       sessionGUID,      KEY, GUID *, be->sessionGuid,
       ')

define(`kvp_gint64', `gncKVPvalue_int64, KVPint64, store_data_t, k,
       type,                , char *, ptr->stype,
       data,                , int64,  ptr->u.ival,
       iguid,            KEY, int32,  ptr->iguid,
       ipath,            KEY, int32,  ptr->ipath,
       ')

define(`kvp_double', `gncKVPvalue_dbl, KVPdouble, store_data_t, k,
       type,                , char *, ptr->stype,
       data,                , double, ptr->u.dbl,
       iguid,            KEY, int32,  ptr->iguid,
       ipath,            KEY, int32,  ptr->ipath,
       ')

define(`kvp_numeric', `gncKVPvalue_numeric, KVPnumeric, store_data_t, k,
       type,                , char *, ptr->stype,
       num,                 , int64,  ptr->u.numeric.num,
       denom,               , int64,  ptr->u.numeric.denom,
       iguid,            KEY, int32,  ptr->iguid,
       ipath,            KEY, int32,  ptr->ipath,
       ')

define(`kvp_string', `gncKVPvalue_str, KVPstring, store_data_t, k,
       type,                , char *, ptr->stype,
       data,                , char *, ptr->u.str,
       iguid,            KEY, int32,  ptr->iguid,
       ipath,            KEY, int32,  ptr->ipath,
       ')

define(`kvp_guid', `gncKVPvalue_guid, KVPguid, store_data_t, k,
       type,                , char *, ptr->stype,
       data,                , char *, ptr->u.str,
       iguid,            KEY, int32,  ptr->iguid,
       ipath,            KEY, int32,  ptr->ipath,
       ')

define(`kvp_timespec', `gncKVPvalue_timespec, KVPtimespec, store_data_t, k,
       type,                , char *,   ptr->stype,
       data,                , Timespec, ptr->u.ts,
       iguid,            KEY, int32,    ptr->iguid,
       ipath,            KEY, int32,    ptr->ipath,
       ')

/* ------------------------------------------------------- */
/* symbolic names for the table accessors */
define(`tablename', $1)
define(`func_name', $2)
define(`xacc_type', $3)
define(`obj_type',  $4)

define(`firstrec', `shift(shift(shift(shift($@))))')
define(`nextrec', `shift(shift(shift(shift($@))))')

/* -------- */
/* macros that use the sql builder to build a query */

define(`sql_setter', `ifelse($2, `KEY',
                     `ifelse($1, `char *',   sqlBuild_Where_Str,
                             $1, `int32',    sqlBuild_Where_Int32,
                             $1, `GUID *',   sqlBuild_Where_GUID)',

                             $2,     ,
                     `ifelse($1, `char *',   sqlBuild_Set_Str,
                             $1, `now',      sqlBuild_Set_Str,
                             $1, `commod',   sqlBuild_Set_Str,
                             $1, `double',   sqlBuild_Set_Double,
                             $1, `int32',    sqlBuild_Set_Int32,
                             $1, `int64',    sqlBuild_Set_Int64,
                             $1, `GUID *',   sqlBuild_Set_GUID,
                             $1, `Timespec', sqlBuild_Set_Date,
                             $1, `char',     sqlBuild_Set_Char)')')


/* recursively walk the table, build the builders */

define(`set_fields_r', `ifelse($#, 1, , 
`   sql_setter($3,$2) (be->builder, "$1", $4);
set_fields_r(nextrec($@))')')

define(`set_fields', `set_fields_r(firstrec($@))')

/* -------- */
/* macros to compare a query result */
/* the commod type behaves just like a string, except it 
 * has its one compre function.  */

define(`cmp_value', `ifelse($1, `char *',   COMP_STR,
                            $1, `now',      COMP_NOW,
                            $1, `int32',    COMP_INT32,
                            $1, `int64',    COMP_INT64,
                            $1, `double',   COMP_DOUBLE,
                            $1, `GUID *',   COMP_GUID,
                            $1, `commod',   COMP_COMMODITY,
                            $1, `Timespec', COMP_DATE,
                            $1, `char',     COMP_CHAR)')

/* recursively walk the table, build compare functions,
 * but only for non-primary-keys */

define(`cmp_fields_r', `ifelse($#, 1, , 
`ifelse($2, `KEY',  ,
`    cmp_value($3,$2) ("$1", $4, ndiffs);
cmp_fields_r(nextrec($@))')')')

define(`cmp_fields', `cmp_fields_r(firstrec($@))')

/* -------- */
/* return the name of the sql field associcate with the primary key */

define(`key_fieldname_r', `ifelse($#, 1, , 
`ifelse($2, `KEY', $1,
`key_fieldname_r(nextrec($@))')')')

define(`key_fieldname', `key_fieldname_r(firstrec($@))')

/* -------- */
/* return the getter function that deals with the version number */
define(`version_function_r', `ifelse($#, 1, , 
`ifelse($1, `version', $4,
`version_function_r(nextrec($@))')')')

define(`version_function', `version_function_r(firstrec($@))')

/* -------- */

define(`store_one_only_header',
`
/* ------------------------------------------------------ */
/* This routine stores/updates one record in the database.
 * It does not do any traversals, it does not lock.  
 * It just pokes the data in.
 */
void 
pgendStoreOne`'func_name($@)`'Only (PGBackend *be,
                                    xacc_type($@) *ptr,
                                    sqlBuild_QType update)')

define(`store_one_only',
`store_one_only_header($@)
{
   const char *buf;
   ENTER ("be=%p, xacc_type($@)=%p", be, ptr);
   if (!be || !ptr) return;

   /* build the sql query */
   sqlBuild_Table (be->builder, "tablename($@)", update);
   set_fields($@)

   buf = sqlBuild_Query (be->builder);
   SEND_QUERY (be,buf, );

   /* flush the buffers, check the status */
   FINISH_QUERY(be->connection);
   LEAVE (" ");
}

')

define(`compare_one_only_header',
`
/* ------------------------------------------------------ */
/* This routine returns a positive int if the indicated object
 * differs from that in the SQL database.  It returns negative
 * number if theres an error.
 * It does not do any traversals, it does not lock.  
 */
int
pgendCompareOne`'func_name($@)`'Only (PGBackend *be, xacc_type($@) *ptr)')

define(`compare_one_only', 
`compare_one_only_header($@)
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

         /* Compare queried values to input values. */
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

define(`put_one_only_header',
`
/* ------------------------------------------------------ */
/* This routine inserts or updates, as appropriate
 * It does not do any traversals, it does not lock.  
 * It just updates.
 */
void 
pgendPutOne`'func_name($@)`'Only (PGBackend *be, xacc_type($@) *ptr)')

define(`put_one_only', 
`put_one_only_header($@)
{
   int ndiffs;
   ndiffs = pgendCompareOne`'func_name($@)`'Only (be, ptr);

   /* update the record if there are differences ... */
   if (0<ndiffs) 
   {
      pgendStoreOne`'func_name($@)`'Only (be, ptr, SQL_UPDATE);
      pgendStoreAudit`'func_name($@)`' (be, ptr, SQL_UPDATE);
   }
   /* insert the record if it doesnt exist */
   if (0>ndiffs)
   {
      pgendStoreOne`'func_name($@)`'Only (be, ptr, SQL_INSERT);
      pgendStoreAudit`'func_name($@)`' (be, ptr, SQL_INSERT);
   }
}

')

define(`compare_version_header',
`
/* ------------------------------------------------------ */
/* This routine compares the version number of the object in 
 * the engine and the sql database. It returns a negative 
 * number if the sql version is older (or the item is not 
 * present in the sql db). It returns a positive number
 * if the sql version is newer.  It returns zero if the
 * two are equal.
 */
int 
pgend`'func_name($@)`'CompareVersion (PGBackend *be, xacc_type($@) *ptr)')

define(`compare_version', 
`compare_version_header($@)
{
   char *p;
   int sql_version = 0;

   p = be->buff; *p = 0;
   p = stpcpy (p, "SELECT version FROM tablename($@) WHERE key_fieldname($@) = ''`");
   p = guid_to_string_buff (qof_instance_get_guid(QOF_INSTANCE(ptr)), p);
   p = stpcpy (p, "''`;");
   SEND_QUERY (be,be->buff, -1);
   sql_version = GPOINTER_TO_INT(pgendGetResults (be, get_version_cb, (gpointer) -1));

   if (-1 == sql_version) return -1;
   return (sql_version - version_function($@));
}

')

define(`is_deleted_header',
`
/* ------------------------------------------------------ */
/* This routine looks at the audit trail to see if the
 * indicated object has been deleted. If it has been,
 * it returns the version number of the deleted object;
 * otherwise it returns -1.
 */ 
int
pgend`'func_name($@)`'GetDeletedVersion (PGBackend *be, xacc_type($@) *ptr)')

define(`is_deleted',
`is_deleted_header($@)
{
   char *p;
   int sql_version = -1;

   p = be->buff; *p = 0;
   p = stpcpy (p, "SELECT version FROM tablename($@)" "Trail WHERE key_fieldname($@) = ''`");
   p = guid_to_string_buff (qof_instance_get_guid(QOF_INSTANCE(ptr)), p);
   p = stpcpy (p, "''` AND change = ''`d''`;");
   SEND_QUERY (be,be->buff, -1);
   sql_version = GPOINTER_TO_INT(pgendGetResults (be, get_version_cb, (gpointer) -1));

   return sql_version;
}

')

define(`store_audit_header',
`
/* ------------------------------------------------------ */
/* This routine stores one autdit record in the database.
 * It does not do any traversals, it does not lock.  
 * It just pokes the data in. 
 */
void 
pgendStoreAudit`'func_name($@)`' (PGBackend *be,
                                  xacc_type($@) *ptr,
                                  sqlBuild_QType update)')

define(`store_audit', 
`store_audit_header($@)
{
   const char *buf;
   ENTER ("be=%p, xacc_type($@)=%p", be, ptr);
   if (!be || !ptr) return;

   /* build the sql query */
   sqlBuild_Table (be->builder, "tablename($@)" "Trail", SQL_INSERT);
#define sqlBuild_Where_Str sqlBuild_Set_Str
#define sqlBuild_Where_GUID sqlBuild_Set_GUID
#define sqlBuild_Where_Int32 sqlBuild_Set_Int32
   set_fields($@)
#undef sqlBuild_Where_Str
#undef sqlBuild_Where_GUID
#undef sqlBuild_Where_Int32
   sqlBuild_Set_Str (be->builder, "date_changed", "NOW");
   /* sqlBuild_Set_GUID (be->builder, "sessionGUID", be->sessionGuid); */
   sqlBuild_Set_Str (be->builder, "sessionGUID", be->session_guid_str);
   sqlBuild_Set_Char (be->builder, "change", update);
   sqlBuild_Set_Char (be->builder, "objtype", ''`obj_type($@)''`);

   buf = sqlBuild_Query (be->builder);
   SEND_QUERY (be,buf, );

   /* flush the buffers, check the status */
   FINISH_QUERY(be->connection);
   LEAVE (" ");
}

')

divert
/* DO NOT EDIT THIS FILE -- it is autogenerated -- edit table.m4 instead */
