--
-- FILE:
-- functions.sql
--
-- FUNCTION:
-- Define assorted utility functions.
--
-- HISTORY:
-- Copyright (C) 2001 Linas Vepstas
--


-- utility functions to compute checkpoint balance subtotals

CREATE FUNCTION gncSubtotalBalance (CHAR(32), DATETIME, DATETIME)
    RETURNS INT8
    AS 'SELECT INT8(sum(gncEntry.amount))
        FROM gncEntry, gncTransaction
        WHERE
        gncEntry.accountGuid = $1 AND
        gncEntry.transGuid = gncTransaction.transGuid AND
        gncTransaction.date_posted BETWEEN $2 AND $3'
    LANGUAGE 'sql';

CREATE FUNCTION gncSubtotalClearedBalance (CHAR(32), DATETIME, DATETIME)
    RETURNS INT8
    AS 'SELECT INT8(sum(gncEntry.amount))
        FROM gncEntry, gncTransaction
        WHERE
        gncEntry.accountGuid = $1 AND
        gncEntry.transGuid = gncTransaction.transGuid AND
        gncTransaction.date_posted BETWEEN $2 AND $3 AND
        gncEntry.reconciled <> \\'n\\''
    LANGUAGE 'sql';

CREATE FUNCTION gncSubtotalReconedBalance (CHAR(32), DATETIME, DATETIME)
    RETURNS INT8
    AS 'SELECT INT8(sum(gncEntry.amount))
        FROM gncEntry, gncTransaction
        WHERE
        gncEntry.accountGuid = $1 AND
        gncEntry.transGuid = gncTransaction.transGuid AND
        gncTransaction.date_posted BETWEEN $2 AND $3 AND
        gncEntry.reconciled = \\'y\\''
    LANGUAGE 'sql';

-- helper functions.  These intentionally use the 'wrong' fraction. 
-- This is because value_frac * amount * price = value * amount_frac

CREATE FUNCTION gncHelperPrVal (gncEntry)
   RETURNS INT8
   AS 'SELECT abs($1 . value * gncCommodity.fraction)
       FROM gncEntry, gncAccount, gncCommodity
       WHERE
       $1 . accountGuid = gncAccount.accountGuid AND
       gncAccount.commodity = gncCommodity.commodity'
    LANGUAGE 'sql';
       
CREATE FUNCTION gncHelperPrAmt (gncEntry)
   RETURNS INT8
   AS 'SELECT abs($1 . amount * gncCommodity.fraction)
       FROM gncEntry, gncTransaction, gncCommodity
       WHERE
       $1 . transGuid = gncTransaction.transGuid AND
       gncTransaction.currency = gncCommodity.commodity'
    LANGUAGE 'sql';
       
-- end of file
