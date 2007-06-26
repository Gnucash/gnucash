
--
-- PostgreSQL database dump
--

SET client_encoding = 'UNICODE';
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA public IS 'Standard public schema';


SET search_path = public, pg_catalog;

--
-- Name: plpgsql_call_handler(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION plpgsql_call_handler() RETURNS language_handler
    AS '$libdir/plpgsql', 'plpgsql_call_handler'
    LANGUAGE c;


ALTER FUNCTION public.plpgsql_call_handler() OWNER TO postgres;

--
-- Name: plpgsql; Type: PROCEDURAL LANGUAGE; Schema: public; Owner: 
--

CREATE TRUSTED PROCEDURAL LANGUAGE plpgsql HANDLER plpgsql_call_handler;


SET default_tablespace = '';

SET default_with_oids = true;

--
-- Name: gncsplit; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gncsplit (
    splitguid character(32) NOT NULL,
    accountguid character(32) NOT NULL,
    transguid character(32) NOT NULL,
    memo text,
    "action" text,
    reconciled character(1) DEFAULT 'n'::bpchar,
    date_reconciled timestamp with time zone,
    amount bigint DEFAULT 0::bigint,
    value bigint DEFAULT 0::bigint,
    iguid integer DEFAULT 0
);


ALTER TABLE public.gncsplit OWNER TO klaus;

--
-- Name: gnchelperpramt(gncsplit); Type: FUNCTION; Schema: public; Owner: klaus
--

CREATE FUNCTION gnchelperpramt(gncsplit) RETURNS bigint
    AS $_$SELECT abs($1 . amount * gncCommodity.fraction) 
       FROM gncSplit, gncTransaction, gncCommodity 
       WHERE 
       $1 . transGuid = gncTransaction.transGuid AND 
       gncTransaction.currency = gncCommodity.commodity$_$
    LANGUAGE sql;


ALTER FUNCTION public.gnchelperpramt(gncsplit) OWNER TO klaus;

--
-- Name: gnchelperprval(gncsplit); Type: FUNCTION; Schema: public; Owner: klaus
--

CREATE FUNCTION gnchelperprval(gncsplit) RETURNS bigint
    AS $_$SELECT abs($1 . value * gncCommodity.fraction) 
       FROM gncSplit, gncAccount, gncCommodity 
       WHERE 
       $1 . accountGuid = gncAccount.accountGuid AND 
       gncAccount.commodity = gncCommodity.commodity$_$
    LANGUAGE sql;


ALTER FUNCTION public.gnchelperprval(gncsplit) OWNER TO klaus;

--
-- Name: gncsubtotalbalance(character, timestamp with time zone, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: klaus
--

CREATE FUNCTION gncsubtotalbalance(character, timestamp with time zone, timestamp with time zone) RETURNS bigint
    AS $_$SELECT INT8(sum(gncSplit.amount)) 
        FROM gncSplit, gncTransaction 
        WHERE 
        gncSplit.accountGuid = $1 AND 
        gncSplit.transGuid = gncTransaction.transGuid AND 
        gncTransaction.date_posted BETWEEN $2 AND $3$_$
    LANGUAGE sql;


ALTER FUNCTION public.gncsubtotalbalance(character, timestamp with time zone, timestamp with time zone) OWNER TO klaus;

--
-- Name: gncsubtotalclearedbalance(character, timestamp with time zone, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: klaus
--

CREATE FUNCTION gncsubtotalclearedbalance(character, timestamp with time zone, timestamp with time zone) RETURNS bigint
    AS $_$SELECT INT8(sum(gncSplit.amount)) 
        FROM gncSplit, gncTransaction 
        WHERE 
        gncSplit.accountGuid = $1 AND 
        gncSplit.transGuid = gncTransaction.transGuid AND 
        gncTransaction.date_posted BETWEEN $2 AND $3 AND 
        gncSplit.reconciled <> 'n'$_$
    LANGUAGE sql;


ALTER FUNCTION public.gncsubtotalclearedbalance(character, timestamp with time zone, timestamp with time zone) OWNER TO klaus;

--
-- Name: gncsubtotalreconedbalance(character, timestamp with time zone, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: klaus
--

CREATE FUNCTION gncsubtotalreconedbalance(character, timestamp with time zone, timestamp with time zone) RETURNS bigint
    AS $_$SELECT INT8(sum(gncSplit.amount)) 
        FROM gncSplit, gncTransaction 
        WHERE 
        gncSplit.accountGuid = $1 AND 
        gncSplit.transGuid = gncTransaction.transGuid AND 
        gncTransaction.date_posted BETWEEN $2 AND $3 AND 
        (gncSplit.reconciled = 'y' OR 
         gncSplit.reconciled = 'f')$_$
    LANGUAGE sql;


ALTER FUNCTION public.gncsubtotalreconedbalance(character, timestamp with time zone, timestamp with time zone) OWNER TO klaus;

--
-- Name: gnc_iguid_seq; Type: SEQUENCE; Schema: public; Owner: klaus
--

CREATE SEQUENCE gnc_iguid_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.gnc_iguid_seq OWNER TO klaus;

--
-- Name: gncaccount; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gncaccount (
    accountguid character(32) NOT NULL,
    parentguid character(32) NOT NULL,
    bookguid character(32) NOT NULL,
    accountname text NOT NULL,
    accountcode text,
    description text,
    "type" text NOT NULL,
    commodity text NOT NULL,
    version integer NOT NULL,
    iguid integer DEFAULT 0,
    CONSTRAINT gncaccount_accountname_check CHECK ((accountname <> ''::text)),
    CONSTRAINT gncaccount_commodity_check CHECK ((commodity <> ''::text))
);


ALTER TABLE public.gncaccount OWNER TO klaus;

--
-- Name: gncaudittrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gncaudittrail (
    sessionguid character(32) NOT NULL,
    date_changed timestamp with time zone,
    change character(1) NOT NULL,
    objtype character(1) NOT NULL
);


ALTER TABLE public.gncaudittrail OWNER TO klaus;

--
-- Name: gncaccounttrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gncaccounttrail (
    accountguid character(32) NOT NULL,
    parentguid character(32) NOT NULL,
    bookguid character(32) NOT NULL,
    accountname text NOT NULL,
    accountcode text,
    description text,
    "type" text NOT NULL,
    commodity text NOT NULL,
    version integer NOT NULL,
    iguid integer DEFAULT 0,
    CONSTRAINT gncaccounttrail_accountname_check CHECK ((accountname <> ''::text)),
    CONSTRAINT gncaccounttrail_commodity_check CHECK ((commodity <> ''::text))
)
INHERITS (gncaudittrail);


ALTER TABLE public.gncaccounttrail OWNER TO klaus;

--
-- Name: gncbook; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gncbook (
    bookguid character(32) NOT NULL,
    book_open character(1) DEFAULT 'n'::bpchar,
    version integer NOT NULL,
    iguid integer DEFAULT 0
);


ALTER TABLE public.gncbook OWNER TO klaus;

--
-- Name: gncbooktrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gncbooktrail (
    bookguid character(32) NOT NULL,
    book_open character(1) DEFAULT 'n'::bpchar,
    version integer NOT NULL,
    iguid integer DEFAULT 0
)
INHERITS (gncaudittrail);


ALTER TABLE public.gncbooktrail OWNER TO klaus;

--
-- Name: gnccheckpoint; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnccheckpoint (
    accountguid character(32) NOT NULL,
    date_start timestamp with time zone NOT NULL,
    date_end timestamp with time zone NOT NULL,
    commodity text NOT NULL,
    "type" text DEFAULT 'simple'::text,
    balance bigint DEFAULT 0::bigint,
    cleared_balance bigint DEFAULT 0::bigint,
    reconciled_balance bigint DEFAULT 0::bigint,
    CONSTRAINT gnccheckpoint_commodity_check CHECK ((commodity <> ''::text))
);


ALTER TABLE public.gnccheckpoint OWNER TO klaus;

--
-- Name: gnccommodity; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnccommodity (
    commodity text NOT NULL,
    fullname text,
    namespace text NOT NULL,
    mnemonic text NOT NULL,
    code text,
    fraction integer DEFAULT 100
);


ALTER TABLE public.gnccommodity OWNER TO klaus;

--
-- Name: gnccommoditytrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnccommoditytrail (
    commodity text NOT NULL,
    fullname text,
    namespace text NOT NULL,
    mnemonic text NOT NULL,
    code text,
    fraction integer DEFAULT 100
)
INHERITS (gncaudittrail);


ALTER TABLE public.gnccommoditytrail OWNER TO klaus;

--
-- Name: gnckvpvalue; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue (
    iguid integer NOT NULL,
    ipath integer NOT NULL,
    "type" character(4)
);


ALTER TABLE public.gnckvpvalue OWNER TO klaus;

--
-- Name: gnckvpvalue_dbl; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_dbl (
    data double precision
)
INHERITS (gnckvpvalue);


ALTER TABLE public.gnckvpvalue_dbl OWNER TO klaus;

--
-- Name: gnckvpvalue_dbltrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_dbltrail (
    iguid integer,
    ipath integer,
    "type" character(4),
    data double precision
)
INHERITS (gncaudittrail);


ALTER TABLE public.gnckvpvalue_dbltrail OWNER TO klaus;

--
-- Name: gnckvpvalue_guid; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_guid (
    data character(32)
)
INHERITS (gnckvpvalue);


ALTER TABLE public.gnckvpvalue_guid OWNER TO klaus;

--
-- Name: gnckvpvalue_guidtrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_guidtrail (
    iguid integer,
    ipath integer,
    "type" character(4),
    data character(32)
)
INHERITS (gncaudittrail);


ALTER TABLE public.gnckvpvalue_guidtrail OWNER TO klaus;

--
-- Name: gnckvpvalue_int64; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_int64 (
    data bigint
)
INHERITS (gnckvpvalue);


ALTER TABLE public.gnckvpvalue_int64 OWNER TO klaus;

--
-- Name: gnckvpvalue_int64trail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_int64trail (
    iguid integer,
    ipath integer,
    "type" character(4),
    data bigint
)
INHERITS (gncaudittrail);


ALTER TABLE public.gnckvpvalue_int64trail OWNER TO klaus;

--
-- Name: gnckvpvalue_list; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_list (
    data text[]
)
INHERITS (gnckvpvalue);


ALTER TABLE public.gnckvpvalue_list OWNER TO klaus;

--
-- Name: gnckvpvalue_listtrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_listtrail (
    iguid integer,
    ipath integer,
    "type" character(4),
    data text[]
)
INHERITS (gncaudittrail);


ALTER TABLE public.gnckvpvalue_listtrail OWNER TO klaus;

--
-- Name: gnckvpvalue_numeric; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_numeric (
    num bigint,
    denom bigint
)
INHERITS (gnckvpvalue);


ALTER TABLE public.gnckvpvalue_numeric OWNER TO klaus;

--
-- Name: gnckvpvalue_numerictrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_numerictrail (
    iguid integer,
    ipath integer,
    "type" character(4),
    num bigint,
    denom bigint
)
INHERITS (gncaudittrail);


ALTER TABLE public.gnckvpvalue_numerictrail OWNER TO klaus;

--
-- Name: gnckvpvalue_str; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_str (
    data text
)
INHERITS (gnckvpvalue);


ALTER TABLE public.gnckvpvalue_str OWNER TO klaus;

--
-- Name: gnckvpvalue_strtrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_strtrail (
    iguid integer,
    ipath integer,
    "type" character(4),
    data text
)
INHERITS (gncaudittrail);


ALTER TABLE public.gnckvpvalue_strtrail OWNER TO klaus;

--
-- Name: gnckvpvalue_timespec; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_timespec (
    data timestamp with time zone
)
INHERITS (gnckvpvalue);


ALTER TABLE public.gnckvpvalue_timespec OWNER TO klaus;

--
-- Name: gnckvpvalue_timespectrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvalue_timespectrail (
    iguid integer,
    ipath integer,
    "type" character(4),
    data timestamp with time zone
)
INHERITS (gncaudittrail);


ALTER TABLE public.gnckvpvalue_timespectrail OWNER TO klaus;

--
-- Name: gnckvpvaluetrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnckvpvaluetrail (
    iguid integer,
    ipath integer,
    "type" character(4)
)
INHERITS (gncaudittrail);


ALTER TABLE public.gnckvpvaluetrail OWNER TO klaus;

--
-- Name: gncpathcache; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gncpathcache (
    ipath serial NOT NULL,
    path text
);


ALTER TABLE public.gncpathcache OWNER TO klaus;

--
-- Name: gncprice; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gncprice (
    priceguid character(32) NOT NULL,
    bookguid character(32) NOT NULL,
    commodity text NOT NULL,
    currency text NOT NULL,
    "time" timestamp with time zone,
    source text,
    "type" text,
    valuenum bigint DEFAULT 0::bigint,
    valuedenom integer DEFAULT 100,
    version integer NOT NULL,
    CONSTRAINT gncprice_commodity_check CHECK ((commodity <> ''::text)),
    CONSTRAINT gncprice_commodity_check1 CHECK ((commodity <> ''::text))
);


ALTER TABLE public.gncprice OWNER TO klaus;

--
-- Name: gncpricetrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gncpricetrail (
    priceguid character(32) NOT NULL,
    bookguid character(32) NOT NULL,
    commodity text NOT NULL,
    currency text NOT NULL,
    "time" timestamp with time zone,
    source text,
    "type" text,
    valuenum bigint DEFAULT 0::bigint,
    valuedenom integer DEFAULT 100,
    version integer NOT NULL,
    CONSTRAINT gncpricetrail_commodity_check CHECK ((commodity <> ''::text)),
    CONSTRAINT gncpricetrail_commodity_check1 CHECK ((commodity <> ''::text))
)
INHERITS (gncaudittrail);


ALTER TABLE public.gncpricetrail OWNER TO klaus;

--
-- Name: gncsession; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gncsession (
    sessionguid character(32) NOT NULL,
    session_mode character(16) NOT NULL,
    hostname text,
    login_name text,
    gecos text,
    time_on timestamp with time zone NOT NULL,
    time_off timestamp with time zone DEFAULT 'infinity'::timestamp with time zone NOT NULL
);


ALTER TABLE public.gncsession OWNER TO klaus;

--
-- Name: gncsplittrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gncsplittrail (
    splitguid character(32) NOT NULL,
    accountguid character(32) NOT NULL,
    transguid character(32) NOT NULL,
    memo text,
    "action" text,
    reconciled character(1) DEFAULT 'n'::bpchar,
    date_reconciled timestamp with time zone,
    amount bigint DEFAULT 0::bigint,
    value bigint DEFAULT 0::bigint,
    iguid integer DEFAULT 0
)
INHERITS (gncaudittrail);


ALTER TABLE public.gncsplittrail OWNER TO klaus;

--
-- Name: gnctransaction; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnctransaction (
    transguid character(32) NOT NULL,
    last_modified timestamp with time zone DEFAULT '2005-12-28 23:53:57.607072+01'::timestamp with time zone,
    date_entered timestamp with time zone,
    date_posted timestamp with time zone,
    num text,
    description text,
    currency text NOT NULL,
    version integer NOT NULL,
    iguid integer DEFAULT 0,
    CONSTRAINT gnctransaction_currency_check CHECK ((currency <> ''::text))
);


ALTER TABLE public.gnctransaction OWNER TO klaus;

--
-- Name: gnctransactiontrail; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gnctransactiontrail (
    transguid character(32) NOT NULL,
    last_modified timestamp with time zone DEFAULT '2005-12-28 23:53:57.700436+01'::timestamp with time zone,
    date_entered timestamp with time zone,
    date_posted timestamp with time zone,
    num text,
    description text,
    currency text NOT NULL,
    version integer NOT NULL,
    iguid integer DEFAULT 0,
    CONSTRAINT gnctransactiontrail_currency_check CHECK ((currency <> ''::text))
)
INHERITS (gncaudittrail);


ALTER TABLE public.gnctransactiontrail OWNER TO klaus;

--
-- Name: gncversion; Type: TABLE; Schema: public; Owner: klaus; Tablespace: 
--

CREATE TABLE gncversion (
    major integer NOT NULL,
    minor integer NOT NULL,
    rev integer DEFAULT 0,
    name text NOT NULL,
    date timestamp with time zone DEFAULT '2005-12-28 23:53:57.607072+01'::timestamp with time zone,
    CONSTRAINT gncversion_name_check CHECK ((name <> ''::text))
);


ALTER TABLE public.gncversion OWNER TO klaus;

--
-- Name: gncaccount_pkey; Type: CONSTRAINT; Schema: public; Owner: klaus; Tablespace: 
--

ALTER TABLE ONLY gncaccount
    ADD CONSTRAINT gncaccount_pkey PRIMARY KEY (accountguid);


ALTER INDEX public.gncaccount_pkey OWNER TO klaus;

--
-- Name: gncbook_pkey; Type: CONSTRAINT; Schema: public; Owner: klaus; Tablespace: 
--

ALTER TABLE ONLY gncbook
    ADD CONSTRAINT gncbook_pkey PRIMARY KEY (bookguid);


ALTER INDEX public.gncbook_pkey OWNER TO klaus;

--
-- Name: gnccheckpoint_pkey; Type: CONSTRAINT; Schema: public; Owner: klaus; Tablespace: 
--

ALTER TABLE ONLY gnccheckpoint
    ADD CONSTRAINT gnccheckpoint_pkey PRIMARY KEY (accountguid, date_start, commodity);


ALTER INDEX public.gnccheckpoint_pkey OWNER TO klaus;

--
-- Name: gnccommodity_pkey; Type: CONSTRAINT; Schema: public; Owner: klaus; Tablespace: 
--

ALTER TABLE ONLY gnccommodity
    ADD CONSTRAINT gnccommodity_pkey PRIMARY KEY (commodity);


ALTER INDEX public.gnccommodity_pkey OWNER TO klaus;

--
-- Name: gnckvpvalue_pkey; Type: CONSTRAINT; Schema: public; Owner: klaus; Tablespace: 
--

ALTER TABLE ONLY gnckvpvalue
    ADD CONSTRAINT gnckvpvalue_pkey PRIMARY KEY (iguid, ipath);


ALTER INDEX public.gnckvpvalue_pkey OWNER TO klaus;

--
-- Name: gncpathcache_pkey; Type: CONSTRAINT; Schema: public; Owner: klaus; Tablespace: 
--

ALTER TABLE ONLY gncpathcache
    ADD CONSTRAINT gncpathcache_pkey PRIMARY KEY (ipath);


ALTER INDEX public.gncpathcache_pkey OWNER TO klaus;

--
-- Name: gncprice_pkey; Type: CONSTRAINT; Schema: public; Owner: klaus; Tablespace: 
--

ALTER TABLE ONLY gncprice
    ADD CONSTRAINT gncprice_pkey PRIMARY KEY (priceguid);


ALTER INDEX public.gncprice_pkey OWNER TO klaus;

--
-- Name: gncsession_pkey; Type: CONSTRAINT; Schema: public; Owner: klaus; Tablespace: 
--

ALTER TABLE ONLY gncsession
    ADD CONSTRAINT gncsession_pkey PRIMARY KEY (sessionguid);


ALTER INDEX public.gncsession_pkey OWNER TO klaus;

--
-- Name: gncsplit_pkey; Type: CONSTRAINT; Schema: public; Owner: klaus; Tablespace: 
--

ALTER TABLE ONLY gncsplit
    ADD CONSTRAINT gncsplit_pkey PRIMARY KEY (splitguid);


ALTER INDEX public.gncsplit_pkey OWNER TO klaus;

--
-- Name: gnctransaction_pkey; Type: CONSTRAINT; Schema: public; Owner: klaus; Tablespace: 
--

ALTER TABLE ONLY gnctransaction
    ADD CONSTRAINT gnctransaction_pkey PRIMARY KEY (transguid);


ALTER INDEX public.gnctransaction_pkey OWNER TO klaus;

--
-- Name: gncversion_name_key; Type: CONSTRAINT; Schema: public; Owner: klaus; Tablespace: 
--

ALTER TABLE ONLY gncversion
    ADD CONSTRAINT gncversion_name_key UNIQUE (name);


ALTER INDEX public.gncversion_name_key OWNER TO klaus;

--
-- Name: gncaccounttrail_account_idx; Type: INDEX; Schema: public; Owner: klaus; Tablespace: 
--

CREATE INDEX gncaccounttrail_account_idx ON gncaccounttrail USING btree (accountguid);


ALTER INDEX public.gncaccounttrail_account_idx OWNER TO klaus;

--
-- Name: gncbooktrail_book_idx; Type: INDEX; Schema: public; Owner: klaus; Tablespace: 
--

CREATE INDEX gncbooktrail_book_idx ON gncbooktrail USING btree (bookguid);


ALTER INDEX public.gncbooktrail_book_idx OWNER TO klaus;

--
-- Name: gnccommoditytrail_commodity_idx; Type: INDEX; Schema: public; Owner: klaus; Tablespace: 
--

CREATE INDEX gnccommoditytrail_commodity_idx ON gnccommoditytrail USING btree (commodity);


ALTER INDEX public.gnccommoditytrail_commodity_idx OWNER TO klaus;

--
-- Name: gncpricetrail_price_idx; Type: INDEX; Schema: public; Owner: klaus; Tablespace: 
--

CREATE INDEX gncpricetrail_price_idx ON gncpricetrail USING btree (priceguid);


ALTER INDEX public.gncpricetrail_price_idx OWNER TO klaus;

--
-- Name: gncsplit_acc_idx; Type: INDEX; Schema: public; Owner: klaus; Tablespace: 
--

CREATE INDEX gncsplit_acc_idx ON gncsplit USING btree (accountguid);


ALTER INDEX public.gncsplit_acc_idx OWNER TO klaus;

--
-- Name: gncsplit_trn_idx; Type: INDEX; Schema: public; Owner: klaus; Tablespace: 
--

CREATE INDEX gncsplit_trn_idx ON gncsplit USING btree (transguid);


ALTER INDEX public.gncsplit_trn_idx OWNER TO klaus;

--
-- Name: gncsplittrail_split_idx; Type: INDEX; Schema: public; Owner: klaus; Tablespace: 
--

CREATE INDEX gncsplittrail_split_idx ON gncsplittrail USING btree (splitguid);


ALTER INDEX public.gncsplittrail_split_idx OWNER TO klaus;

--
-- Name: gnctransaction_posted_idx; Type: INDEX; Schema: public; Owner: klaus; Tablespace: 
--

CREATE INDEX gnctransaction_posted_idx ON gnctransaction USING btree (date_posted);


ALTER INDEX public.gnctransaction_posted_idx OWNER TO klaus;

--
-- Name: gnctransactiontrail_trans_idx; Type: INDEX; Schema: public; Owner: klaus; Tablespace: 
--

CREATE INDEX gnctransactiontrail_trans_idx ON gnctransactiontrail USING btree (transguid);


ALTER INDEX public.gnctransactiontrail_trans_idx OWNER TO klaus;

--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--
