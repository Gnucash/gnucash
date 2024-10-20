/********************************************************************
 * gnc-commodity.c -- api for tradable commodities (incl. currency) *
 * Copyright (C) 2000 Bill Gribble                                  *
 * Copyright (C) 2001,2003 Linas Vepstas <linas@linas.org>          *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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
 *******************************************************************/

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <ctype.h>
#include <limits.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <qofinstance-p.h>

#include "gnc-commodity.hpp"
#include "gnc-commodity.h"
#include "gnc-locale-utils.h"
#include "gnc-prefs.h"
#include "guid.h"
#include "qofinstance.h"

#include <list>
#include <unordered_map>
#include <numeric>

static QofLogModule log_module = GNC_MOD_COMMODITY;

/* Parts per unit is nominal, i.e. number of 'partname' units in
 * a 'unitname' unit.  fraction is transactional, i.e. how many
 * of the smallest-transactional-units of the currency are there
 * in a 'unitname' unit. */

enum
{
    PROP_0,
    PROP_NAMESPACE,	/* Table */
    PROP_FULL_NAME,	/* Table */
    PROP_MNEMONIC,	/* Table */
    PROP_PRINTNAME,	/* Constructed */
    PROP_CUSIP,		/* Table */
    PROP_FRACTION,	/* Table */
    PROP_UNIQUE_NAME,	/* Constructed */
    PROP_QUOTE_FLAG,	/* Table */
    PROP_QUOTE_SOURCE,	/* Table */
    PROP_QUOTE_TZ,	/* Table */
};

struct gnc_commodity_s
{
    QofInstance inst;
};

typedef struct gnc_commodityPrivate
{
    gnc_commodity_namespace *name_space;

    const char *fullname;
    const char *mnemonic;
    char       *printname;
    const char *cusip;                /* CUSIP or other identifying code */
    int         fraction;
    char       *unique_name;
    char       *user_symbol;

    gboolean    quote_flag;	      /* user wants price quotes */
    gnc_quote_source *quote_source;   /* current/old source of quotes */
    const char *quote_tz;

    /* the number of accounts using this commodity - this field is not
     * persisted */
    int         usage_count;

    /* the default display_symbol, set in iso-4217-currencies at start-up */
    const char *default_symbol;
} gnc_commodityPrivate;

#define GET_PRIVATE(o) \
    ((gnc_commodityPrivate*)gnc_commodity_get_instance_private((gnc_commodity*)o))

struct _GncCommodityClass
{
    QofInstanceClass parent_class;
};

static void commodity_free(gnc_commodity * cm);
static void gnc_commodity_set_default_symbol(gnc_commodity *, const char *);

using StringCommodityMap = std::unordered_map<std::string, gnc_commodity*>;
using CommodityVec = std::vector<gnc_commodity*>;
using StringCommNSMap = std::unordered_map<std::string, gnc_commodity_namespace*>;
using CommNSVec = std::vector<gnc_commodity_namespace*>;

struct gnc_commodity_namespace_s
{
    QofInstance inst;

    const gchar *name;
    gboolean     iso4217;
    StringCommodityMap cm_table;
    CommodityVec cm_vec;
};

struct _GncCommodityNamespaceClass
{
    QofInstanceClass parent_class;
};

struct gnc_commodity_table_s
{
    StringCommNSMap ns_table;
    CommNSVec ns_vec;
};

static const std::unordered_map<std::string,std::string> gnc_new_iso_codes =
{
    {"RUR", "RUB"}, /* Russian Ruble: RUR through 1997-12, RUB from 1998-01 onwards; see bug #393185 */
    {"PLZ", "PLN"}, /* Polish Zloty */
    {"UAG", "UAH"}, /* Ukraine Hryvnia */
    {"NIS", "ILS"}, /* New Israeli Shekel: The informal abbreviation may be "NIS", but
		     its iso-4217 is clearly ILS and only this! Incorrectly changed
		     due to bug#152755 (Nov 2004) and changed back again by bug#492417
		     (Oct 2008). */
    {"MXP", "MXN"}, /* Mexican (Nuevo) Peso */
    {"TRL", "TRY"}, /* New Turkish Lira: changed 2005 */

    /* Only add currencies to this table when the old currency no longer
     * exists in the file iso-4217-currencies.xml */
};

static std::string fq_version;

struct gnc_quote_source_s
{
private:
    gboolean m_supported;
    QuoteSourceType m_type;
    std::string m_user_name;		/* User friendly name incl. region code*/
    std::string m_internal_name;	/* Name used internally and by finance::quote. */
public:
    bool get_supported () const { return m_supported; }
    void set_supported (bool supported) { m_supported = supported; }
    QuoteSourceType get_type () const { return m_type; }
    const char* get_user_name () const { return m_user_name.c_str(); }
    const char* get_internal_name () const { return m_internal_name.c_str(); }
    gnc_quote_source_s (gboolean supported, QuoteSourceType type,
                        const char* username, const char* int_name)
        : m_supported{supported}
        , m_type{type}
        , m_user_name{username ? username : ""}
        , m_internal_name{int_name ? int_name: ""} { };
};

using QuoteSourceList = std::list<gnc_quote_source>;

/* To update the following lists scan
 * from github.com/finance-quote/finance-quote
 * in lib/Finance/Quote/ all *.pm for "methods"
 * because many of them have more than one -
 * ideally after each release of them.
 *
 * Apply changes here also to the FQ appendix of help.
 */
static QuoteSourceList currency_quote_sources =
{
    { true, SOURCE_CURRENCY, "Currency", "currency" }
};

/* The single quote method is usually the module name, but
 * sometimes it gets the suffix "_direct"
 * and the failover method is without suffix.
 */
static QuoteSourceList single_quote_sources =
{
    { false, SOURCE_SINGLE, "Alphavantage, US", "alphavantage" },
    { false, SOURCE_SINGLE, "Amsterdam Euronext eXchange, NL", "aex" },
    { false, SOURCE_SINGLE, "Association of Mutual Funds in India", "amfiindia" },
    { false, SOURCE_SINGLE, "Australian Stock Exchange, AU", "asx" },
    { false, SOURCE_SINGLE, "Canada Mutual", "canadamutual" },
    { false, SOURCE_SINGLE, "Deka Investments, DE", "deka" },
    { false, SOURCE_SINGLE, "Dutch", "dutch" },
    { false, SOURCE_SINGLE, "DWS, DE", "dwsfunds" },
    { false, SOURCE_SINGLE, "Financial Times Funds service, GB", "ftfunds" },
    { false, SOURCE_SINGLE, "Finanzpartner, DE", "finanzpartner" },
    { false, SOURCE_SINGLE, "GoldMoney spot rates, JE", "goldmoney" },
    { false, SOURCE_SINGLE, "Google Web, US Stocks", "googleweb" },
    { false, SOURCE_SINGLE, "India Mutual", "indiamutual" },
    { false, SOURCE_SINGLE, "Morningstar, GB", "morningstaruk" },
    { false, SOURCE_SINGLE, "Morningstar, JP", "morningstarjp" },
    { false, SOURCE_SINGLE, "New Zealand stock eXchange, NZ", "nzx" },
    { false, SOURCE_SINGLE, "Paris Stock Exchange/Boursorama, FR", "bourso" },
    { false, SOURCE_SINGLE, "Romania", "romania" },
    { false, SOURCE_SINGLE, "SIX Swiss Exchange shares, CH", "six" },
    { false, SOURCE_SINGLE, "Skandinaviska Enskilda Banken, SE", "seb_funds" },
    { false, SOURCE_SINGLE, "Sharenet, ZA", "za" },
    { false, SOURCE_SINGLE, "TIAA-CREF, US", "tiaacref" },
    { false, SOURCE_SINGLE, "Toronto Stock eXchange, CA", "tsx" },
    { false, SOURCE_SINGLE, "T. Rowe Price", "troweprice" },
    { false, SOURCE_SINGLE, "T. Rowe Price, US", "troweprice_direct" },
    { false, SOURCE_SINGLE, "Union Investment, DE", "unionfunds" },
    { false, SOURCE_SINGLE, "US Govt. Thrift Savings Plan", "tsp" },
    { false, SOURCE_SINGLE, "Yahoo as JSON", "yahoo_json" },
    { false, SOURCE_SINGLE, "Yahoo Web", "yahooweb" },
    { false, SOURCE_SINGLE, "YH Finance (FinanceAPI)", "financeapi" },
};

static QuoteSourceList multiple_quote_sources =
{
    { false, SOURCE_MULTI, "Australia (ASX, ...)", "australia" },
    { false, SOURCE_MULTI, "Canada (Alphavantage, TSX, ...)", "canada" },
    { false, SOURCE_MULTI, "Canada Mutual (Fund Library, StockHouse, ...)", "canadamutual" },
    { false, SOURCE_MULTI, "Dutch (AEX, ...)", "dutch" },
    { false, SOURCE_MULTI, "Europe (asegr,.bsero, hex ...)", "europe" },
    { false, SOURCE_MULTI, "India Mutual (AMFI, ...)", "indiamutual" },
    { false, SOURCE_MULTI, "France (bourso, ĺerevenu, ...)", "france" },
    { false, SOURCE_MULTI, "Nasdaq (alphavantage, yahoo_json, ...)", "nasdaq" },
    { false, SOURCE_MULTI, "NYSE (alphavantage, yahoo_json, ...)", "nyse" },
    { false, SOURCE_MULTI, "South Africa (Sharenet, ...)", "za" },
    { false, SOURCE_MULTI, "Romania (BSE-RO, ...)", "romania" },
    { false, SOURCE_MULTI, "T. Rowe Price", "troweprice" },
    { false, SOURCE_MULTI, "U.K. Funds (citywire, FTfunds, MStar, tnetuk, ...)", "ukfunds" },
    { false, SOURCE_MULTI, "USA (alphavantage, yahoo_json, ...)", "usa" },
};

static QuoteSourceList new_quote_sources;

// cannot use map or unordered_map because order must be preserved
static const std::vector<std::pair<QuoteSourceType,QuoteSourceList&>> quote_sources_map =
    {
        { SOURCE_CURRENCY, currency_quote_sources },
        { SOURCE_SINGLE, single_quote_sources },
        { SOURCE_MULTI, multiple_quote_sources },
        { SOURCE_UNKNOWN, new_quote_sources }
    };

/********************************************************************
 * gnc_quote_source_fq_installed
 *
 * This function indicates whether or not the Finance::Quote module
 * is installed on a users computer.
 ********************************************************************/
gboolean
gnc_quote_source_fq_installed (void)
{
    return (!fq_version.empty());
}


/********************************************************************
 * gnc_quote_source_fq_version
 *
 * This function the version of the Finance::Quote module installed
 * on a user's computer or nullptr if no installation is found.
 ********************************************************************/
const char*
gnc_quote_source_fq_version (void)
{
    return fq_version.c_str();
}

static QuoteSourceList&
get_quote_source_from_type (QuoteSourceType type)
{
    auto quote_sources_it = std::find_if (quote_sources_map.begin(), quote_sources_map.end(),
                                          [type] (const auto& qs) { return type == qs.first; });

    if (quote_sources_it != quote_sources_map.end())
        return quote_sources_it->second;

    PWARN ("Invalid Quote Source %d, returning new_quote_sources", type);
    return new_quote_sources;
}

/********************************************************************
 * gnc_quote_source_num_entries
 *
 * Return the number of entries for a given type of price source.
 ********************************************************************/
gint gnc_quote_source_num_entries(QuoteSourceType type)
{
    auto source{get_quote_source_from_type(type)};
    return std::distance(source.begin(), source.end());
}



/********************************************************************
 * gnc_quote_source_add_new
 *
 * Add a new price source. Called when unknown source names are found
 * either in the F::Q installation (a newly available source) or in
 * the user's data file (a source that has vanished but needs to be
 * tracked.)
 ********************************************************************/
gnc_quote_source *
gnc_quote_source_add_new (const char *source_name, gboolean supported)
{
    DEBUG("Creating new source %s", (!source_name ? "(null)" : source_name));
    /* This name can be changed if/when support for this price source is
     * integrated into gnucash. */
    /* This name is permanent and must be kept the same if/when support
     * for this price source is integrated into gnucash (i.e. for a
     * nice user name). */
    return &new_quote_sources.emplace_back (supported, SOURCE_UNKNOWN, source_name, source_name);
}

/********************************************************************
 * gnc_quote_source_lookup_by_xxx
 *
 * Lookup a price source data structure based upon various criteria.
 ********************************************************************/
gnc_quote_source *
gnc_quote_source_lookup_by_ti (QuoteSourceType type, gint index)
{
    ENTER("type/index is %d/%d", type, index);
    auto& sources = get_quote_source_from_type (type);
    if ((size_t) index < sources.size())
    {
        auto it = std::next(sources.begin(), index);
        LEAVE("found %s", it->get_user_name());
        return &*it;
    }

    LEAVE("not found");
    return nullptr;
}

gnc_quote_source *
gnc_quote_source_lookup_by_internal(const char * name)
{
    if (!name || !*name)
        return nullptr;

    for (const auto& [_, sources] : quote_sources_map)
    {
        auto source_it = std::find_if (sources.begin(), sources.end(),
                                       [name] (const auto& qs)
                                       { return (g_strcmp0(name, qs.get_internal_name()) == 0); });
        if (source_it != sources.end())
            return &(*source_it);
    }

    DEBUG("gnc_quote_source_lookup_by_internal: Unknown source %s", name);
    return nullptr;
}

/********************************************************************
 * gnc_quote_source_get_xxx
 *
 * Accessor functions - get functions only. There are no set functions.
 ********************************************************************/
QuoteSourceType
gnc_quote_source_get_type (const gnc_quote_source *source)
{
    ENTER("%p", source);
    if (!source)
    {
        LEAVE("bad source");
        return SOURCE_SINGLE;
    }

    LEAVE("type is %d", source->get_type());
    return source->get_type();
}

gint
gnc_quote_source_get_index (const gnc_quote_source *source)
{
    if (!source)
    {
        PWARN ("bad source");
        return 0;
    }

    auto& sources = get_quote_source_from_type (source->get_type());
    auto is_source = [&source](const auto& findif_source)
    { return &findif_source == source; };

    auto iter = std::find_if (sources.begin(), sources.end(), is_source);
    if (iter != sources.end())
        return std::distance (sources.begin(), iter);

    PWARN ("couldn't locate source");
    return 0;
}

gboolean
gnc_quote_source_get_supported (const gnc_quote_source *source)
{
    ENTER("%p", source);
    if (!source)
    {
        LEAVE("bad source");
        return FALSE;
    }

    LEAVE("%s supported", source && source->get_supported() ? "" : "not ");
    return source->get_supported();
}

const char *
gnc_quote_source_get_user_name (const gnc_quote_source *source)
{
    ENTER("%p", source);
    if (!source)
    {
        LEAVE("bad source");
        return nullptr;
    }
    LEAVE("user name %s", source->get_user_name());
    return source->get_user_name();
}

const char *
gnc_quote_source_get_internal_name (const gnc_quote_source *source)
{
    ENTER("%p", source);
    if (!source)
    {
        LEAVE("bad source");
        return nullptr;
    }
    LEAVE("internal name %s", source->get_internal_name());
    return source->get_internal_name();
}


/********************************************************************
 * gnc_quote_source_set_fq_installed
 *
 * Update gnucash internal tables on what Finance::Quote sources are
 * installed.
 ********************************************************************/
void
gnc_quote_source_set_fq_installed (const char* version_string,
                                   const std::vector<std::string>& sources_list)
{
    ENTER(" ");

    if (sources_list.empty())
        return;

    if (version_string)
        fq_version = version_string;
    else
        fq_version.clear();

    for (const auto& source_name_str : sources_list)
    {
        auto source_name = source_name_str.c_str();
        auto source = gnc_quote_source_lookup_by_internal(source_name);

        if (source)
        {
            DEBUG("Found source %s: %s", source_name, source->get_user_name());
            source->set_supported (true);
            continue;
        }

        gnc_quote_source_add_new(source_name, TRUE);
    }
    LEAVE(" ");
}

/********************************************************************
 * QoF Helpers
 ********************************************************************/

void
gnc_commodity_begin_edit (gnc_commodity *cm)
{
    qof_begin_edit(&cm->inst);
}

static void commit_err (QofInstance *inst, QofBackendError errcode)
{
    PERR ("Failed to commit: %d", errcode);
    gnc_engine_signal_commit_error( errcode );
}

static void noop (QofInstance *inst) {}

static void
comm_free(QofInstance* inst)
{
    commodity_free( GNC_COMMODITY(inst) );
}

void
gnc_commodity_commit_edit (gnc_commodity *cm)
{
    if (!qof_commit_edit (QOF_INSTANCE(cm))) return;
    qof_commit_edit_part2 (&cm->inst, commit_err, noop, comm_free);
}

/********************************************************************
 * gnc_commodity_new
 ********************************************************************/

static void
mark_commodity_dirty (gnc_commodity *cm)
{
    qof_instance_set_dirty(&cm->inst);
    qof_event_gen (&cm->inst, QOF_EVENT_MODIFY, nullptr);
}

static void
reset_printname(gnc_commodityPrivate *priv)
{
    g_free(priv->printname);
    priv->printname = g_strdup_printf("%s (%s)",
                                      priv->mnemonic ? priv->mnemonic : "",
                                      priv->fullname ? priv->fullname : "");
}

static void
reset_unique_name(gnc_commodityPrivate *priv)
{
    gnc_commodity_namespace *ns;

    g_free(priv->unique_name);
    ns = priv->name_space;
    priv->unique_name = g_strdup_printf("%s::%s",
                                        ns ? ns->name : "",
                                        priv->mnemonic ? priv->mnemonic : "");
}

/* GObject Initialization */
G_DEFINE_TYPE_WITH_PRIVATE(gnc_commodity, gnc_commodity, QOF_TYPE_INSTANCE)

static void
gnc_commodity_init(gnc_commodity* com)
{
    gnc_commodityPrivate* priv;

    priv = GET_PRIVATE(com);

    priv->name_space = nullptr;
    priv->fullname = CACHE_INSERT("");
    priv->mnemonic = CACHE_INSERT("");
    priv->cusip = CACHE_INSERT("");
    priv->fraction = 10000;
    priv->quote_flag = 0;
    priv->quote_source = nullptr;
    priv->quote_tz = CACHE_INSERT("");

    reset_printname(priv);
    reset_unique_name(priv);
}

static void
gnc_commodity_dispose(GObject *comp)
{
    G_OBJECT_CLASS(gnc_commodity_parent_class)->dispose(comp);
}

static void
gnc_commodity_finalize(GObject* comp)
{
    G_OBJECT_CLASS(gnc_commodity_parent_class)->finalize(comp);
}
/* Note that g_value_set_object() refs the object, as does
 * g_object_get(). But g_object_get() only unrefs once when it disgorges
 * the object, leaving an unbalanced ref, which leaks. So instead of
 * using g_value_set_object(), use g_value_take_object() which doesn't
 * ref the object when used in get_property().
 */
static void
gnc_commodity_get_property (GObject         *object,
                            guint            prop_id,
                            GValue          *value,
                            GParamSpec      *pspec)
{
    gnc_commodity *commodity;
    gnc_commodityPrivate* priv;

    g_return_if_fail(GNC_IS_COMMODITY(object));

    commodity = GNC_COMMODITY(object);
    priv = GET_PRIVATE(commodity);
    switch (prop_id)
    {
    case PROP_NAMESPACE:
        g_value_take_object(value, priv->name_space);
        break;
    case PROP_FULL_NAME:
        g_value_set_string(value, priv->fullname);
        break;
    case PROP_MNEMONIC:
        g_value_set_string(value, priv->mnemonic);
        break;
    case PROP_PRINTNAME:
        g_value_set_string(value, priv->printname);
        break;
    case PROP_CUSIP:
        g_value_set_string(value, priv->cusip);
        break;
    case PROP_FRACTION:
        g_value_set_int(value, priv->fraction);
        break;
    case PROP_UNIQUE_NAME:
        g_value_set_string(value, priv->unique_name);
        break;
    case PROP_QUOTE_FLAG:
        g_value_set_boolean(value, priv->quote_flag);
        break;
    case PROP_QUOTE_SOURCE:
        g_value_set_pointer(value, priv->quote_source);
        break;
    case PROP_QUOTE_TZ:
        g_value_set_string(value, priv->quote_tz);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_commodity_set_property (GObject         *object,
                            guint            prop_id,
                            const GValue    *value,
                            GParamSpec      *pspec)
{
    gnc_commodity *commodity;

    g_return_if_fail(GNC_IS_COMMODITY(object));

    commodity = GNC_COMMODITY(object);
    g_assert (qof_instance_get_editlevel(commodity));

    switch (prop_id)
    {
    case PROP_NAMESPACE:
        gnc_commodity_set_namespace(commodity, static_cast<const char*>(g_value_get_object(value)));
        break;
    case PROP_FULL_NAME:
        gnc_commodity_set_fullname(commodity, g_value_get_string(value));
        break;
    case PROP_MNEMONIC:
        gnc_commodity_set_mnemonic(commodity, g_value_get_string(value));
        break;
    case PROP_CUSIP:
        gnc_commodity_set_cusip(commodity, g_value_get_string(value));
        break;
    case PROP_FRACTION:
        gnc_commodity_set_fraction(commodity, g_value_get_int(value));
        break;
    case PROP_QUOTE_FLAG:
        gnc_commodity_set_quote_flag(commodity, g_value_get_boolean(value));
        break;
    case PROP_QUOTE_SOURCE:
        gnc_commodity_set_quote_source(commodity, static_cast<gnc_quote_source*>(g_value_get_pointer(value)));
        break;
    case PROP_QUOTE_TZ:
        gnc_commodity_set_quote_tz(commodity, g_value_get_string(value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}
static void
gnc_commodity_class_init(struct _GncCommodityClass* klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS(klass);

    gobject_class->dispose = gnc_commodity_dispose;
    gobject_class->finalize = gnc_commodity_finalize;
    gobject_class->set_property = gnc_commodity_set_property;
    gobject_class->get_property = gnc_commodity_get_property;

    g_object_class_install_property(gobject_class,
                                    PROP_NAMESPACE,
                                    g_param_spec_object ("namespace",
                                            "Namespace",
                                            "The namespace field denotes the "
                                            "namespace for this commodity, either "
                                            "a currency or symbol from a quote source.",
                                            GNC_TYPE_COMMODITY_NAMESPACE,
                                            G_PARAM_READWRITE));
    g_object_class_install_property(gobject_class,
                                    PROP_FULL_NAME,
                                    g_param_spec_string ("fullname",
                                            "Full Commodity Name",
                                            "The fullname is the official full name of"
                                            "the currency.",
                                            nullptr,
                                            G_PARAM_READWRITE));
    g_object_class_install_property(gobject_class,
                                    PROP_MNEMONIC,
                                    g_param_spec_string ("mnemonic",
                                            "Commodity Mnemonic",
                                            "The mnemonic is the official abbreviated"
                                            "designation for the currency.",
                                            nullptr,
                                            G_PARAM_READWRITE));
    g_object_class_install_property(gobject_class,
                                    PROP_PRINTNAME,
                                    g_param_spec_string ("printname",
                                            "Commodity Print Name",
                                            "Printable form of the commodity name.",
                                            nullptr,
                                            G_PARAM_READABLE));
    g_object_class_install_property(gobject_class,
                                    PROP_CUSIP,
                                    g_param_spec_string ("cusip",
                                            "Commodity CUSIP Code",
                                            "?????",
                                            nullptr,
                                            G_PARAM_READWRITE));
    g_object_class_install_property(gobject_class,
                                    PROP_FRACTION,
                                    g_param_spec_int ("fraction",
                                            "Fraction",
                                            "The fraction is the number of sub-units that "
                                            "the basic commodity can be divided into.",
                                            1,
                                            GNC_COMMODITY_MAX_FRACTION,
                                            1,
                                            G_PARAM_READWRITE));
    g_object_class_install_property(gobject_class,
                                    PROP_UNIQUE_NAME,
                                    g_param_spec_string ("unique-name",
                                            "Commodity Unique Name",
                                            "Unique form of the commodity name which combines "
                                            "the namespace name and the commodity name.",
                                            nullptr,
                                            G_PARAM_READABLE));
    g_object_class_install_property(gobject_class,
                                    PROP_QUOTE_FLAG,
                                    g_param_spec_boolean ("quote_flag",
                                            "Quote Flag",
                                            "TRUE if prices are to be downloaded for this "
                                            "commodity from a quote source.",
                                            FALSE,
                                            G_PARAM_READWRITE));
    g_object_class_install_property(gobject_class,
                                    PROP_QUOTE_SOURCE,
                                    g_param_spec_pointer("quote-source",
                                            "Quote Source",
                                            "The quote source from which prices are downloaded.",
                                            G_PARAM_READWRITE));
    g_object_class_install_property(gobject_class,
                                    PROP_QUOTE_TZ,
                                    g_param_spec_string ("quote-tz",
                                            "Commodity Quote Timezone",
                                            "?????",
                                            nullptr,
                                            G_PARAM_READWRITE));
}

gnc_commodity *
gnc_commodity_new(QofBook *book, const char * fullname,
                  const char * name_space, const char * mnemonic,
                  const char * cusip, int fraction)
{
    auto retval = GNC_COMMODITY(g_object_new(GNC_TYPE_COMMODITY, nullptr));

    qof_instance_init_data (&retval->inst, GNC_ID_COMMODITY, book);
    gnc_commodity_begin_edit(retval);

    if ( name_space != nullptr )
    {
	/* Prevent setting anything except template in namespace template. */
        if (g_strcmp0 (name_space, GNC_COMMODITY_NS_TEMPLATE) == 0 &&
	    g_strcmp0 (mnemonic, "template") != 0)
	{
	    PWARN("Converting commodity %s from namespace template to "
		  "namespace User", mnemonic);
	    name_space = "User";
	}
        gnc_commodity_set_namespace(retval, name_space);
        if (gnc_commodity_namespace_is_iso(name_space))
        {
            gnc_commodity_set_quote_source(retval,
                                           gnc_quote_source_lookup_by_internal("currency") );
        }
    }
    gnc_commodity_set_fullname(retval, fullname);
    gnc_commodity_set_mnemonic(retval, mnemonic);
    gnc_commodity_set_cusip(retval, cusip);
    gnc_commodity_set_fraction(retval, fraction);
    mark_commodity_dirty (retval);
    gnc_commodity_commit_edit(retval);

    qof_event_gen (&retval->inst, QOF_EVENT_CREATE, nullptr);

    return retval;
}


/********************************************************************
 * gnc_commodity_destroy
 ********************************************************************/

static void
commodity_free(gnc_commodity * cm)
{
    QofBook *book;
    gnc_commodity_table *table;
    gnc_commodityPrivate* priv;

    if (!cm) return;

    book = qof_instance_get_book(&cm->inst);
    table = gnc_commodity_table_get_table(book);
    gnc_commodity_table_remove(table, cm);
    priv = GET_PRIVATE(cm);

    qof_event_gen (&cm->inst, QOF_EVENT_DESTROY, nullptr);

    /* Set at creation */
    CACHE_REMOVE (priv->fullname);
    CACHE_REMOVE (priv->cusip);
    CACHE_REMOVE (priv->mnemonic);
    CACHE_REMOVE (priv->quote_tz);
    priv->name_space = nullptr;

    /* Set through accessor functions */
    priv->quote_source = nullptr;

    /* Automatically generated */
    g_free(priv->printname);
    priv->printname = nullptr;

    g_free(priv->unique_name);
    priv->unique_name = nullptr;

#ifdef ACCOUNTS_CLEANED_UP
    /* Account objects are not actually cleaned up when a book is closed (in fact
     * a memory leak), but commodities are, so in currently this warning gets hit
     * quite frequently.  Disable the check until cleaning up of accounts objects
     * on close is implemented.  */
    if (priv->usage_count != 0)
    {
        PWARN("Destroying commodity (%p) with non-zero usage_count (%d).", cm,
              priv->usage_count);
    }
#endif

    /* qof_instance_release (&cm->inst); */
    g_object_unref(cm);
}

void
gnc_commodity_destroy(gnc_commodity * cm)
{
    gnc_commodity_begin_edit(cm);
    qof_instance_set_destroying(cm, TRUE);
    gnc_commodity_commit_edit(cm);
}

void
gnc_commodity_copy(gnc_commodity * dest, const gnc_commodity *src)
{
    gnc_commodityPrivate* src_priv = GET_PRIVATE(src);
    gnc_commodityPrivate* dest_priv = GET_PRIVATE(dest);

    gnc_commodity_set_fullname (dest, src_priv->fullname);
    gnc_commodity_set_mnemonic (dest, src_priv->mnemonic);
    dest_priv->name_space = src_priv->name_space;
    gnc_commodity_set_fraction (dest, src_priv->fraction);
    gnc_commodity_set_cusip (dest, src_priv->cusip);
    gnc_commodity_set_quote_flag (dest, src_priv->quote_flag);
    gnc_commodity_set_quote_source (dest, gnc_commodity_get_quote_source (src));
    gnc_commodity_set_quote_tz (dest, src_priv->quote_tz);
    qof_instance_copy_kvp (QOF_INSTANCE (dest), QOF_INSTANCE (src));
}

gnc_commodity *
gnc_commodity_clone(const gnc_commodity *src, QofBook *dest_book)
{
    gnc_commodityPrivate* src_priv;
    gnc_commodityPrivate* dest_priv;

    auto dest = GNC_COMMODITY (g_object_new(GNC_TYPE_COMMODITY, nullptr));
    qof_instance_init_data (&dest->inst, GNC_ID_COMMODITY, dest_book);
    src_priv = GET_PRIVATE(src);
    dest_priv = GET_PRIVATE(dest);

    dest_priv->fullname = CACHE_INSERT(src_priv->fullname);
    dest_priv->mnemonic = CACHE_INSERT(src_priv->mnemonic);
    dest_priv->cusip = CACHE_INSERT(src_priv->cusip);
    dest_priv->quote_tz = CACHE_INSERT(src_priv->quote_tz);

    dest_priv->name_space = src_priv->name_space;

    dest_priv->fraction = src_priv->fraction;
    dest_priv->quote_flag = src_priv->quote_flag;

    gnc_commodity_set_quote_source (dest, gnc_commodity_get_quote_source (src));

    qof_instance_copy_kvp (QOF_INSTANCE (dest), QOF_INSTANCE (src));

    reset_printname(dest_priv);
    reset_unique_name(dest_priv);

    return dest;
}

/********************************************************************
 * gnc_commodity_get_mnemonic
 ********************************************************************/

const char *
gnc_commodity_get_mnemonic(const gnc_commodity * cm)
{
    if (!cm) return nullptr;
    return GET_PRIVATE(cm)->mnemonic;
}

/********************************************************************
 * gnc_commodity_get_printname
 ********************************************************************/

const char *
gnc_commodity_get_printname(const gnc_commodity * cm)
{
    if (!cm) return nullptr;
    return GET_PRIVATE(cm)->printname;
}


/********************************************************************
 * gnc_commodity_get_namespace
 ********************************************************************/

const char *
gnc_commodity_get_namespace(const gnc_commodity * cm)
{
    if (!cm) return nullptr;
    return gnc_commodity_namespace_get_name(GET_PRIVATE(cm)->name_space);
}

gnc_commodity_namespace *
gnc_commodity_get_namespace_ds(const gnc_commodity * cm)
{
    if (!cm) return nullptr;
    return GET_PRIVATE(cm)->name_space;
}

/********************************************************************
 * gnc_commodity_get_fullname
 ********************************************************************/

const char *
gnc_commodity_get_fullname(const gnc_commodity * cm)
{
    if (!cm) return nullptr;
    return GET_PRIVATE(cm)->fullname;
}


/********************************************************************
 * gnc_commodity_get_unique_name
 ********************************************************************/

const char *
gnc_commodity_get_unique_name(const gnc_commodity * cm)
{
    if (!cm) return nullptr;
    return GET_PRIVATE(cm)->unique_name;
}


/********************************************************************
 * gnc_commodity_get_cusip
 ********************************************************************/

const char *
gnc_commodity_get_cusip(const gnc_commodity * cm)
{
    if (!cm) return nullptr;
    return GET_PRIVATE(cm)->cusip;
}

/********************************************************************
 * gnc_commodity_get_fraction
 ********************************************************************/

int
gnc_commodity_get_fraction(const gnc_commodity * cm)
{
    if (!cm) return 0;
    return GET_PRIVATE(cm)->fraction;
}

/********************************************************************
 * gnc_commodity_get_auto_quote_control_flag
 ********************************************************************/

static gboolean
gnc_commodity_get_auto_quote_control_flag(const gnc_commodity *cm)
{
    GValue v = G_VALUE_INIT;
    gboolean retval = TRUE;

    if (!cm) return FALSE;
    qof_instance_get_kvp (QOF_INSTANCE (cm), &v, 1, "auto_quote_control");
    if (G_VALUE_HOLDS_STRING (&v) &&
        strcmp(g_value_get_string (&v), "false") == 0)
        retval = FALSE;
    g_value_unset (&v);
    return retval;
}

/********************************************************************
 * gnc_commodity_get_quote_flag
 ********************************************************************/

gboolean
gnc_commodity_get_quote_flag(const gnc_commodity *cm)
{
    if (!cm) return FALSE;
    return (GET_PRIVATE(cm)->quote_flag);
}

/********************************************************************
 * gnc_commodity_get_quote_source
 ********************************************************************/

gnc_quote_source*
gnc_commodity_get_quote_source(const gnc_commodity *cm)
{
    gnc_commodityPrivate* priv;

    if (!cm) return nullptr;
    priv = GET_PRIVATE(cm);
    if (!priv->quote_source && gnc_commodity_is_iso(cm))
        return &currency_quote_sources.front();
    return priv->quote_source;
}

gnc_quote_source*
gnc_commodity_get_default_quote_source(const gnc_commodity *cm)
{
    if (cm && gnc_commodity_is_iso(cm))
        return &currency_quote_sources.front();
    /* Should make this a user option at some point. */
    return gnc_quote_source_lookup_by_internal("alphavantage");
}

/********************************************************************
 * gnc_commodity_get_quote_tz
 ********************************************************************/

const char*
gnc_commodity_get_quote_tz(const gnc_commodity *cm)
{
    if (!cm) return nullptr;
    return GET_PRIVATE(cm)->quote_tz;
}

/********************************************************************
 * gnc_commodity_get_user_symbol
 ********************************************************************/
const char*
gnc_commodity_get_user_symbol(const gnc_commodity *cm)
{
    g_return_val_if_fail (GNC_IS_COMMODITY (cm), nullptr);

    GValue v = G_VALUE_INIT;
    qof_instance_get_kvp (QOF_INSTANCE(cm), &v, 1, "user_symbol");
    const char *rv = G_VALUE_HOLDS_STRING (&v) ? g_value_get_string (&v) : nullptr;
    g_value_unset (&v);
    return rv;
}

/********************************************************************
 * gnc_commodity_get_default_symbol
 *******************************************************************/
const char*
gnc_commodity_get_default_symbol(const gnc_commodity *cm)
{
    if (!cm) return nullptr;
    return GET_PRIVATE(cm)->default_symbol;
}

/********************************************************************
 * gnc_commodity_get_nice_symbol
 *******************************************************************/
const char*
gnc_commodity_get_nice_symbol (const gnc_commodity *cm)
{
    const char *nice_symbol;
    struct lconv *lc;
    if (!cm) return nullptr;

    nice_symbol = gnc_commodity_get_user_symbol(cm);
    if (nice_symbol && *nice_symbol)
        return nice_symbol;

    lc = gnc_localeconv();
    nice_symbol = lc->currency_symbol;
    if (!g_strcmp0(gnc_commodity_get_mnemonic(cm), lc->int_curr_symbol))
        return nice_symbol;

    nice_symbol = gnc_commodity_get_default_symbol(cm);
    if (nice_symbol && *nice_symbol)
        return nice_symbol;

    return gnc_commodity_get_mnemonic(cm);
}

/********************************************************************
 * gnc_commodity_set_mnemonic
 ********************************************************************/

void
gnc_commodity_set_mnemonic(gnc_commodity * cm, const char * mnemonic)
{
    gnc_commodityPrivate* priv;

    if (!cm) return;
    priv = GET_PRIVATE(cm);
    if (priv->mnemonic == mnemonic) return;

    gnc_commodity_begin_edit(cm);
    CACHE_REMOVE (priv->mnemonic);
    priv->mnemonic = CACHE_INSERT(mnemonic);

    mark_commodity_dirty (cm);
    reset_printname(priv);
    reset_unique_name(priv);
    gnc_commodity_commit_edit(cm);
}

/********************************************************************
 * gnc_commodity_set_namespace
 ********************************************************************/

void
gnc_commodity_set_namespace(gnc_commodity * cm, const char * name_space)
{
    QofBook *book;
    gnc_commodity_table *table;
    gnc_commodity_namespace *nsp;
    gnc_commodityPrivate* priv;

    if (!cm) return;
    priv = GET_PRIVATE(cm);
    book = qof_instance_get_book (&cm->inst);
    table = gnc_commodity_table_get_table(book);
    nsp = gnc_commodity_table_add_namespace(table, name_space, book);
    if (priv->name_space == nsp)
        return;

    gnc_commodity_begin_edit(cm);
    priv->name_space = nsp;
    if (nsp->iso4217)
        priv->quote_source = gnc_quote_source_lookup_by_internal("currency");
    mark_commodity_dirty(cm);
    reset_printname(priv);
    reset_unique_name(priv);
    gnc_commodity_commit_edit(cm);
}

/********************************************************************
 * gnc_commodity_set_fullname
 ********************************************************************/

void
gnc_commodity_set_fullname(gnc_commodity * cm, const char * fullname)
{
    gnc_commodityPrivate* priv;

    if (!cm) return;
    priv = GET_PRIVATE(cm);
    if (priv->fullname == fullname) return;

    CACHE_REMOVE (priv->fullname);
    priv->fullname = CACHE_INSERT (fullname);

    gnc_commodity_begin_edit(cm);
    mark_commodity_dirty(cm);
    reset_printname(priv);
    gnc_commodity_commit_edit(cm);
}

/********************************************************************
 * gnc_commodity_set_cusip
 ********************************************************************/

void
gnc_commodity_set_cusip(gnc_commodity * cm,
                        const char * cusip)
{
    gnc_commodityPrivate* priv;

    if (!cm) return;

    priv = GET_PRIVATE(cm);
    if (priv->cusip == cusip) return;

    gnc_commodity_begin_edit(cm);
    CACHE_REMOVE (priv->cusip);
    priv->cusip = CACHE_INSERT (cusip);
    mark_commodity_dirty(cm);
    gnc_commodity_commit_edit(cm);
}

/********************************************************************
 * gnc_commodity_set_fraction
 ********************************************************************/

void
gnc_commodity_set_fraction(gnc_commodity * cm, int fraction)
{
    if (!cm) return;
    gnc_commodity_begin_edit(cm);
    GET_PRIVATE(cm)->fraction = fraction;
    mark_commodity_dirty(cm);
    gnc_commodity_commit_edit(cm);
}

/********************************************************************
 * gnc_commodity_set_auto_quote_control_flag
 ********************************************************************/

static void
gnc_commodity_set_auto_quote_control_flag(gnc_commodity *cm,
        const gboolean flag)
{
    GValue v = G_VALUE_INIT;
    ENTER ("(cm=%p, flag=%d)", cm, flag);

    if (!cm)
    {
        LEAVE("");
        return;
    }
    gnc_commodity_begin_edit(cm);
    if (flag)
        qof_instance_set_kvp (QOF_INSTANCE (cm), nullptr, 1, "auto_quote_control");
    else
    {
        g_value_init (&v, G_TYPE_STRING);
        g_value_set_string (&v, "false");
        qof_instance_set_kvp (QOF_INSTANCE (cm), &v, 1, "auto_quote_control");
    }
    g_value_unset (&v);
    mark_commodity_dirty(cm);
    gnc_commodity_commit_edit(cm);
    LEAVE("");
}

/********************************************************************
 * gnc_commodity_user_set_quote_flag
 ********************************************************************/

void
gnc_commodity_user_set_quote_flag(gnc_commodity *cm, const gboolean flag)
{
    gnc_commodityPrivate* priv;

    ENTER ("(cm=%p, flag=%d)", cm, flag);

    if (!cm)
    {
        LEAVE("");
        return;
    }

    priv = GET_PRIVATE(cm);
    gnc_commodity_begin_edit(cm);
    gnc_commodity_set_quote_flag(cm, flag);
    if (gnc_commodity_is_iso(cm))
    {
        /* For currencies, disable auto quote control if the quote flag is being
         * changed from its default value and enable it if the quote flag is being
         * reset to its default value.  The defaults for the quote flag are
         * disabled if no accounts are using the currency, and true otherwise.
         * Thus enable auto quote control if flag is FALSE and there are not any
         * accounts using this currency OR flag is TRUE and there are accounts
         * using this currency; otherwise disable auto quote control */
        gnc_commodity_set_auto_quote_control_flag(cm,
                (!flag && (priv->usage_count == 0)) || (flag && (priv->usage_count != 0)));
    }
    gnc_commodity_commit_edit(cm);
    LEAVE("");
}

/********************************************************************
 * gnc_commodity_set_quote_flag
 ********************************************************************/

void
gnc_commodity_set_quote_flag(gnc_commodity *cm, const gboolean flag)
{
    ENTER ("(cm=%p, flag=%d)", cm, flag);

    if (!cm) return;
    gnc_commodity_begin_edit(cm);
    GET_PRIVATE(cm)->quote_flag = flag;
    mark_commodity_dirty(cm);
    gnc_commodity_commit_edit(cm);
    LEAVE(" ");
}

/********************************************************************
 * gnc_commodity_set_quote_source
 ********************************************************************/

void
gnc_commodity_set_quote_source(gnc_commodity *cm, gnc_quote_source *src)
{
    ENTER ("(cm=%p, src=%p(%s))", cm, src, src ? src->get_internal_name() : "unknown");

    if (!cm) return;
    gnc_commodity_begin_edit(cm);
    GET_PRIVATE(cm)->quote_source = src;
    mark_commodity_dirty(cm);
    gnc_commodity_commit_edit(cm);
    LEAVE(" ");
}

/********************************************************************
 * gnc_commodity_set_quote_tz
 ********************************************************************/

void
gnc_commodity_set_quote_tz(gnc_commodity *cm, const char *tz)
{
    gnc_commodityPrivate* priv;

    if (!cm) return;

    ENTER ("(cm=%p, tz=%s)", cm, tz ? tz : "(null)");

    priv = GET_PRIVATE(cm);

    if (tz == priv->quote_tz)
    {
        LEAVE("Already correct TZ");
        return;
    }

    gnc_commodity_begin_edit(cm);
    CACHE_REMOVE (priv->quote_tz);
    priv->quote_tz = CACHE_INSERT (tz);
    mark_commodity_dirty(cm);
    gnc_commodity_commit_edit(cm);
    LEAVE(" ");
}

/********************************************************************
 * gnc_commodity_set_user_symbol
 ********************************************************************/

void
gnc_commodity_set_user_symbol(gnc_commodity * cm, const char * user_symbol)
{
    struct lconv *lc;

    if (!cm) return;

    ENTER ("(cm=%p, symbol=%s)", cm, user_symbol ? user_symbol : "(null)");

    lc = gnc_localeconv();
    if (!user_symbol || !*user_symbol)
	user_symbol = nullptr;
    else if (!g_strcmp0(lc->int_curr_symbol, gnc_commodity_get_mnemonic(cm)) &&
	     !g_strcmp0(lc->currency_symbol, user_symbol))
	/* if the user gives the ISO symbol for the locale currency or the
	 * default symbol, actually remove the user symbol */
	user_symbol = nullptr;
    else if (!g_strcmp0(user_symbol, gnc_commodity_get_default_symbol(cm)))
	user_symbol = nullptr;

    gnc_commodity_begin_edit (cm);

    if (user_symbol)
    {
        GValue v = G_VALUE_INIT;
        g_value_init (&v, G_TYPE_STRING);
        g_value_set_static_string (&v, user_symbol);
        qof_instance_set_kvp (QOF_INSTANCE(cm), &v, 1, "user_symbol");
        g_value_unset (&v);
    }
    else
    {
        qof_instance_set_kvp (QOF_INSTANCE(cm), nullptr, 1, "user_symbol");
    }

    mark_commodity_dirty(cm);
    gnc_commodity_commit_edit(cm);

    LEAVE(" ");
}

/********************************************************************
 * gnc_commodity_set_default_symbol
 * Not made visible in gnc-commodity.h, it is only called from
 * iso-4217-currencies.c at startup.
 ********************************************************************/
void
gnc_commodity_set_default_symbol(gnc_commodity * cm,
				 const char * default_symbol)
{
    GET_PRIVATE(cm)->default_symbol = default_symbol;
}

/********************************************************************
 * gnc_commodity_increment_usage_count
 ********************************************************************/

void
gnc_commodity_increment_usage_count(gnc_commodity *cm)
{
    gnc_commodityPrivate* priv;

    ENTER("(cm=%p)", cm);

    if (!cm)
    {
        LEAVE("");
        return;
    }

    priv = GET_PRIVATE(cm);

    if ((priv->usage_count == 0) && !priv->quote_flag
            && gnc_commodity_get_auto_quote_control_flag(cm)
            && gnc_commodity_is_iso(cm))
    {
        /* compatibility hack - Gnucash 1.8 gets currency quotes when a
           non-default currency is assigned to an account.  */
        gnc_commodity_begin_edit(cm);
        gnc_commodity_set_quote_flag(cm, TRUE);
        gnc_commodity_set_quote_source(cm,
                                       gnc_commodity_get_default_quote_source(cm));
        gnc_commodity_commit_edit(cm);
    }
    priv->usage_count++;
    LEAVE("(usage_count=%d)", priv->usage_count);
}

/********************************************************************
 * gnc_commodity_decrement_usage_count
 ********************************************************************/

void
gnc_commodity_decrement_usage_count(gnc_commodity *cm)
{
    gnc_commodityPrivate* priv;

    ENTER("(cm=%p)", cm);

    if (!cm)
    {
        LEAVE("");
        return;
    }

    priv = GET_PRIVATE(cm);

    if (priv->usage_count == 0)
    {
        PWARN("usage_count already zero");
        LEAVE("");
        return;
    }

    priv->usage_count--;
    if ((priv->usage_count == 0) && priv->quote_flag
            && gnc_commodity_get_auto_quote_control_flag(cm)
            && gnc_commodity_is_iso(cm))
    {
        /* if this is a currency with auto quote control enabled and no more
         * accounts reference this currency, disable quote retrieval */
        gnc_commodity_set_quote_flag(cm, FALSE);
    }
    LEAVE("(usage_count=%d)", priv->usage_count);
}

/********************************************************************\
\********************************************************************/


/********************************************************************
 * gnc_commodity_equiv
 * are two commodities the same?
 ********************************************************************/

gboolean
gnc_commodity_equiv(const gnc_commodity * a, const gnc_commodity * b)
{
    gnc_commodityPrivate* priv_a;
    gnc_commodityPrivate* priv_b;

    if (a == b) return TRUE;
    if (!a || !b) return FALSE;

    priv_a = GET_PRIVATE(a);
    priv_b = GET_PRIVATE(b);
    if (priv_a->name_space != priv_b->name_space) return FALSE;
    if (g_strcmp0(priv_a->mnemonic, priv_b->mnemonic) != 0) return FALSE;

    return TRUE;
}

gboolean
gnc_commodity_equal(const gnc_commodity * a, const gnc_commodity * b)
{
    return gnc_commodity_compare(a, b) == 0;
}

// Used as a sorting callback for deleting old prices, so it needs to be
// stable but doesn't need to be in any particular order sensible to humans.
int gnc_commodity_compare(const gnc_commodity * a, const gnc_commodity * b)
{
    if (a == b) return 0;
    if (a && !b) return 1;
    if (b && !a) return -1;
    return qof_instance_guid_compare(a, b);
}

// Used as a callback to g_list_find_custom, it should return 0
// when the commodities match.
int gnc_commodity_compare_void(const void * a, const void * b)
{
    return gnc_commodity_compare(GNC_COMMODITY (a), GNC_COMMODITY (b));
}

/************************************************************
 *                   Namespace functions                    *
 ************************************************************/
const char *
gnc_commodity_namespace_get_name (const gnc_commodity_namespace *ns)
{
    if (ns == nullptr)
        return nullptr;
    return ns->name;
}

const char *
gnc_commodity_namespace_get_gui_name (const gnc_commodity_namespace *ns)
{
    if (ns == nullptr)
        return nullptr;
    if (g_strcmp0 (ns->name, GNC_COMMODITY_NS_CURRENCY) == 0)
        return GNC_COMMODITY_NS_ISO_GUI;
    return ns->name;
}

GList *
gnc_commodity_namespace_get_commodity_list(const gnc_commodity_namespace *name_space)
{
    if (!name_space)
        return nullptr;

    return std::accumulate (name_space->cm_vec.rbegin(), name_space->cm_vec.rend(),
                            static_cast<GList*>(nullptr), g_list_prepend);
}

gboolean
gnc_commodity_namespace_is_iso(const char *name_space)
{
    return ((g_strcmp0(name_space, GNC_COMMODITY_NS_ISO) == 0) ||
            (g_strcmp0(name_space, GNC_COMMODITY_NS_CURRENCY) == 0));
}

static const gchar *
gnc_commodity_table_map_namespace(const char * name_space)
{
    if (g_strcmp0(name_space, GNC_COMMODITY_NS_ISO) == 0)
        return GNC_COMMODITY_NS_CURRENCY;
    return name_space;
}

/********************************************************************
 * gnc_commodity_table_new
 * make a new commodity table
 ********************************************************************/

gnc_commodity_table *
gnc_commodity_table_new(void)
{
    gnc_commodity_table * retval = g_new0(gnc_commodity_table, 1);
    new (&retval->ns_table) StringCommNSMap ();
    new (&retval->ns_vec) CommNSVec ();
    return retval;
}

/********************************************************************
 * book anchor functions
 ********************************************************************/

gnc_commodity_table *
gnc_commodity_table_get_table(QofBook *book)
{
    if (!book) return nullptr;
    return static_cast<gnc_commodity_table*>(qof_book_get_data (book, GNC_COMMODITY_TABLE));
}

gnc_commodity *
gnc_commodity_obtain_twin (const gnc_commodity *from, QofBook *book)
{
    gnc_commodity *twin;
    const char * ucom;
    gnc_commodity_table * comtbl;

    if (!from) return nullptr;
    comtbl = gnc_commodity_table_get_table (book);
    if (!comtbl) return nullptr;

    ucom = gnc_commodity_get_unique_name (from);
    twin = gnc_commodity_table_lookup_unique (comtbl, ucom);
    if (!twin)
    {
        twin = gnc_commodity_clone (from, book);
        twin = gnc_commodity_table_insert (comtbl, twin);
    }
    return twin;
}

/********************************************************************
 * gnc_commodity_table_get_size
 * get the size of the commodity table
 ********************************************************************/

guint
gnc_commodity_table_get_size(const gnc_commodity_table* tbl)
{
    g_return_val_if_fail(tbl, 0);

    return std::accumulate (tbl->ns_table.begin(), tbl->ns_table.end(), 0,
                            [](guint acc, const auto& str_ns)
                            { return str_ns.first == GNC_COMMODITY_NS_CURRENCY ? acc
                                    : acc + str_ns.second->cm_table.size(); });
}

/********************************************************************
 * gnc_commodity_table_lookup
 * locate a commodity by namespace and mnemonic.
 ********************************************************************/

gnc_commodity *
gnc_commodity_table_lookup(const gnc_commodity_table * table,
                           const char * name_space, const char * mnemonic)
{
    gnc_commodity_namespace * nsp = nullptr;

    if (!table || !name_space || !mnemonic) return nullptr;

    nsp = gnc_commodity_table_find_namespace(table, name_space);

    if (nsp)
    {
        /*
         * Backward compatibility support for currencies that have
         * recently changed.
         */
        if (nsp->iso4217)
        {
            auto it = gnc_new_iso_codes.find (mnemonic);
            if (it != gnc_new_iso_codes.end())
                mnemonic = it->second.c_str();
        }
        return nsp->cm_table[mnemonic];
    }
    else
    {
        return nullptr;
    }
}

/********************************************************************
 * gnc_commodity_table_lookup
 * locate a commodity by unique name.
 ********************************************************************/

gnc_commodity *
gnc_commodity_table_lookup_unique(const gnc_commodity_table *table,
                                  const char * unique_name)
{
    char *name_space;
    char *mnemonic;
    gnc_commodity *commodity;

    if (!table || !unique_name) return nullptr;

    name_space = g_strdup (unique_name);
    mnemonic = strstr (name_space, "::");
    if (!mnemonic)
    {
        g_free (name_space);
        return nullptr;
    }

    *mnemonic = '\0';
    mnemonic += 2;

    commodity = gnc_commodity_table_lookup (table, name_space, mnemonic);

    g_free (name_space);

    return commodity;
}

/********************************************************************
 * gnc_commodity_table_find_full
 * locate a commodity by namespace and printable name
 ********************************************************************/

gnc_commodity *
gnc_commodity_table_find_full(const gnc_commodity_table * table,
                              const char * name_space,
                              const char * fullname)
{
    gnc_commodity * retval = nullptr;
    GList         * all;
    GList         * iterator;

    if (!fullname || (fullname[0] == '\0'))
        return nullptr;

    all = gnc_commodity_table_get_commodities(table, name_space);

    for (iterator = all; iterator; iterator = iterator->next)
    {
        auto commodity = GNC_COMMODITY (iterator->data);
        if (!strcmp(fullname,
                    gnc_commodity_get_printname(commodity)))
        {
            retval = commodity;
            break;
        }
    }

    g_list_free (all);

    return retval;
}


/********************************************************************
 * gnc_commodity_table_insert
 * add a commodity to the table.
 ********************************************************************/

gnc_commodity *
gnc_commodity_table_insert(gnc_commodity_table * table,
                           gnc_commodity * comm)
{
    gnc_commodity_namespace * nsp = nullptr;
    gnc_commodity *c;
    const char *ns_name;
    gnc_commodityPrivate* priv;
    QofBook *book;

    if (!table) return nullptr;
    if (!comm) return nullptr;

    priv = GET_PRIVATE(comm);

    ENTER ("(table=%p, comm=%p) %s %s", table, comm,
           (priv->mnemonic == nullptr ? "(null)" : priv->mnemonic),
           (priv->fullname == nullptr ? "(null)" : priv->fullname));
    ns_name = gnc_commodity_namespace_get_name(priv->name_space);
    c = gnc_commodity_table_lookup (table, ns_name, priv->mnemonic);

    if (c)
    {
        if (c == comm)
        {
            LEAVE("already in table");
            return c;
        }

        /* Backward compatibility support for currencies that have
         * recently changed. */
        if (priv->name_space->iso4217)
        {
            auto it = gnc_new_iso_codes.find (priv->mnemonic);
            if (it != gnc_new_iso_codes.end())
                gnc_commodity_set_mnemonic(comm, it->second.c_str());
        }
        gnc_commodity_copy (c, comm);
        gnc_commodity_destroy (comm);
        LEAVE("found at %p", c);
        return c;
    }

    /* Prevent setting anything except template in namespace template. */
    if (g_strcmp0 (ns_name, GNC_COMMODITY_NS_TEMPLATE) == 0 &&
	g_strcmp0 (priv->mnemonic, "template") != 0)
    {
	PWARN("Converting commodity %s from namespace template to "
	      "namespace User", priv->mnemonic);
	gnc_commodity_set_namespace (comm, "User");
	ns_name = "User";
	mark_commodity_dirty (comm);
    }

    book = qof_instance_get_book (&comm->inst);
    nsp = gnc_commodity_table_add_namespace(table, ns_name, book);

    PINFO ("insert %p %s into nsp=%p %s", priv->mnemonic, priv->mnemonic,
           &nsp->cm_table, nsp->name);
    nsp->cm_table[priv->mnemonic] = comm;
    nsp->cm_vec.push_back (comm);

    qof_event_gen (&comm->inst, QOF_EVENT_ADD, nullptr);
    LEAVE ("(table=%p, comm=%p)", table, comm);
    return comm;
}

/********************************************************************
 * gnc_commodity_table_remove
 * remove a commodity from the table.
 ********************************************************************/

void
gnc_commodity_table_remove(gnc_commodity_table * table,
                           gnc_commodity * comm)
{
    gnc_commodity_namespace * nsp;
    gnc_commodity *c;
    gnc_commodityPrivate* priv;
    const char *ns_name;

    if (!table) return;
    if (!comm) return;

    priv = GET_PRIVATE(comm);
    ns_name = gnc_commodity_namespace_get_name(priv->name_space);
    c = gnc_commodity_table_lookup (table, ns_name, priv->mnemonic);
    if (c != comm) return;

    qof_event_gen (&comm->inst, QOF_EVENT_REMOVE, nullptr);

    nsp = gnc_commodity_table_find_namespace(table, ns_name);
    if (!nsp) return;

    nsp->cm_vec.erase (std::remove (nsp->cm_vec.begin(), nsp->cm_vec.end(),  comm));
    nsp->cm_table.erase (priv->mnemonic);
}

/********************************************************************
 * gnc_commodity_table_has_namespace
 * see if the commodities namespace exists. May have zero commodities.
 ********************************************************************/

int
gnc_commodity_table_has_namespace(const gnc_commodity_table * table,
                                  const char * name_space)
{
    gnc_commodity_namespace * nsp = nullptr;

    if (!table || !name_space)
    {
        return 0;
    }

    nsp = gnc_commodity_table_find_namespace(table, name_space);
    if (nsp)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

/********************************************************************
 * gnc_commodity_table_get_namespaces
 * see if any commodities in the namespace exist
 ********************************************************************/

GList *
gnc_commodity_table_get_namespaces(const gnc_commodity_table * table)
{
    if (!table)
        return nullptr;

    return std::accumulate (table->ns_table.begin(), table->ns_table.end(),
                            static_cast<GList*>(nullptr),
                            [](GList *acc, auto& str_ns)
                            { return g_list_prepend (acc, (gpointer)str_ns.first.c_str()); });
}

GList *
gnc_commodity_table_get_namespaces_list(const gnc_commodity_table * table)
{
    if (!table)
        return nullptr;

    return std::accumulate (table->ns_vec.rbegin(), table->ns_vec.rend(),
                            static_cast<GList*>(nullptr), g_list_prepend);
}

/* Because gnc_commodity_table_add_namespace maps GNC_COMMODITY_NS_ISO to
   GNC_COMMODITY_NS_CURRENCY and then sets iso4217 if the namespace is
   either of these, the net result is that the iso4217 bit is set only
   for GNC_COMMODITY_NS_CURRENCY.  This means that gnc_commodity_is_iso is
   a subset of gnc_commodity_is_currency.  Most callers seem to use
   gnc_commodity_is_iso. */
gboolean
gnc_commodity_is_iso(const gnc_commodity * cm)
{
    gnc_commodityPrivate* priv;

    if (!cm) return FALSE;

    priv = GET_PRIVATE(cm);
    if ( !priv->name_space) return FALSE;
    return priv->name_space->iso4217;
}

gboolean
gnc_commodity_is_currency(const gnc_commodity *cm)
{
    const char *ns_name;
    if (!cm) return FALSE;

    ns_name = gnc_commodity_namespace_get_name(GET_PRIVATE(cm)->name_space);
    return (!g_strcmp0(ns_name, GNC_COMMODITY_NS_LEGACY) ||
            !g_strcmp0(ns_name, GNC_COMMODITY_NS_CURRENCY));
}

/********************************************************************
 * gnc_commodity_table_get_commodities
 * list commodities in a given namespace
 ********************************************************************/

static CommodityList*
commodity_table_get_all_noncurrency_commodities(const gnc_commodity_table* table)
{
    GList *node = nullptr, *nslist = gnc_commodity_table_get_namespaces(table);
    CommodityList *retval = nullptr;
    for (node = nslist; node; node=g_list_next(node))
    {
        gnc_commodity_namespace *ns = nullptr;
        if (g_strcmp0((char*)(node->data), GNC_COMMODITY_NS_CURRENCY) == 0
            || g_strcmp0((char*)(node->data), GNC_COMMODITY_NS_TEMPLATE) == 0)
            continue;
        ns = gnc_commodity_table_find_namespace(table, (char*)(node->data));
        if (!ns)
            continue;
        for (auto n : ns->cm_table)
            retval = g_list_prepend (retval, n.second);
    }
    g_list_free(nslist);
    return retval;
}

CommodityList *
gnc_commodity_table_get_commodities(const gnc_commodity_table * table,
                                    const char * name_space)
{
    gnc_commodity_namespace * ns = nullptr;

    if (!table)
        return nullptr;
    if (g_strcmp0(name_space, GNC_COMMODITY_NS_NONISO_GUI) == 0)
        return commodity_table_get_all_noncurrency_commodities(table);
    ns = gnc_commodity_table_find_namespace(table, name_space);
    if (!ns)
        return nullptr;

    return std::accumulate (ns->cm_table.begin(), ns->cm_table.end(),
                            static_cast<GList*>(nullptr),
                            [](auto acc, auto ns){ return g_list_prepend (acc, ns.second); });
}

/********************************************************************
 * gnc_commodity_table_get_quotable_commodities
 * list commodities in a given namespace that get price quotes
 ********************************************************************/

static void
get_quotables_helper1 (const std::string& key, gnc_commodity* comm, GList **l)
{
    gnc_commodityPrivate* priv = GET_PRIVATE(comm);

    if (!priv->quote_flag || !priv->quote_source || !priv->quote_source->get_supported())
        return;
    *l = g_list_prepend(*l, comm);
}

static gboolean
get_quotables_helper2 (gnc_commodity *comm, gpointer data)
{
    auto l = static_cast<GList**>(data);
    gnc_commodityPrivate* priv = GET_PRIVATE(comm);

    if (!priv->quote_flag || priv->quote_source || !priv->quote_source->get_supported())
        return TRUE;
    *l = g_list_prepend(*l, comm);
    return TRUE;
}

CommodityList *
gnc_commodity_table_get_quotable_commodities(const gnc_commodity_table * table)
{
    gnc_commodity_namespace * ns = nullptr;
    const char *name_space;
    GList * nslist, * tmp;
    GList * l = nullptr;
    regex_t pattern;
    const char *expression = gnc_prefs_get_namespace_regexp();

    ENTER("table=%p, expression=%s", table, expression);
    if (!table)
        return nullptr;

    if (expression && *expression)
    {
        if (regcomp(&pattern, expression, REG_EXTENDED | REG_ICASE) != 0)
        {
            LEAVE("Cannot compile regex");
            return nullptr;
        }

        nslist = gnc_commodity_table_get_namespaces(table);
        for (tmp = nslist; tmp; tmp = tmp->next)
        {
            name_space = static_cast<const char*>(tmp->data);
            if (regexec(&pattern, name_space, 0, nullptr, 0) == 0)
            {
                DEBUG("Running list of %s commodities", name_space);
                ns = gnc_commodity_table_find_namespace(table, name_space);
                if (ns)
                {
                    std::for_each (ns->cm_table.begin(), ns->cm_table.end(),
                                   [&l](auto cm){ get_quotables_helper1 (cm.first, cm.second, &l); });
                }
            }
        }
        g_list_free(nslist);
        regfree(&pattern);
    }
    else
    {
        gnc_commodity_table_foreach_commodity(table, get_quotables_helper2,
                                              (gpointer) &l);
    }
    LEAVE("list head %p", l);
    return l;
}

/********************************************************************
 * gnc_commodity_table_add_namespace
 * add an empty namespace if it does not exist
 ********************************************************************/

/* GObject Initialization */
QOF_GOBJECT_IMPL(gnc_commodity_namespace, gnc_commodity_namespace, QOF_TYPE_INSTANCE)

static void
gnc_commodity_namespace_init(gnc_commodity_namespace* ns)
{
}

static void
gnc_commodity_namespace_dispose_real (GObject *nsp)
{
}

static void
gnc_commodity_namespace_finalize_real(GObject* nsp)
{
}

gnc_commodity_namespace *
gnc_commodity_table_add_namespace(gnc_commodity_table * table,
                                  const char * name_space,
                                  QofBook *book)
{
    gnc_commodity_namespace * ns = nullptr;

    if (!table) return nullptr;

    name_space = gnc_commodity_table_map_namespace(name_space);
    ns = gnc_commodity_table_find_namespace(table, name_space);
    if (!ns)
    {
        ns = static_cast<gnc_commodity_namespace*>(g_object_new(GNC_TYPE_COMMODITY_NAMESPACE, nullptr));
        new (&ns->cm_table) StringCommodityMap ();
        ns->name = CACHE_INSERT(static_cast<const char*>(name_space));
        ns->iso4217 = gnc_commodity_namespace_is_iso(name_space);
        qof_instance_init_data (&ns->inst, GNC_ID_COMMODITY_NAMESPACE, book);
        qof_event_gen (&ns->inst, QOF_EVENT_CREATE, nullptr);
        table->ns_table[ns->name] = ns;
        table->ns_vec.push_back (ns);
        qof_event_gen (&ns->inst, QOF_EVENT_ADD, nullptr);
    }
    return ns;
}


gnc_commodity_namespace *
gnc_commodity_table_find_namespace(const gnc_commodity_table * table,
                                   const char * name_space)
{
    if (!table || !name_space)
        return nullptr;

    name_space = gnc_commodity_table_map_namespace(name_space);

    auto it = table->ns_table.find (name_space);
    return (it == table->ns_table.end()) ? nullptr : it->second;
}


gnc_commodity *
gnc_commodity_find_commodity_by_guid(const GncGUID *guid, QofBook *book)
{
    QofCollection *col;
    if (!guid || !book) return nullptr;
    col = qof_book_get_collection (book, GNC_ID_COMMODITY);
    return (gnc_commodity *) qof_collection_lookup_entity (col, guid);
}

/********************************************************************
 * gnc_commodity_table_delete_namespace
 * delete a namespace
 ********************************************************************/

void
gnc_commodity_table_delete_namespace(gnc_commodity_table * table,
                                     const char * name_space)
{
    gnc_commodity_namespace * ns;

    if (!table) return;

    ns = gnc_commodity_table_find_namespace(table, name_space);
    if (!ns)
        return;

    qof_event_gen (&ns->inst, QOF_EVENT_REMOVE, nullptr);
    table->ns_table.erase (name_space);
    table->ns_vec.erase (std::remove (table->ns_vec.begin(), table->ns_vec.end(), ns));

    ns->cm_vec.~CommodityVec ();
    ns->cm_table.~StringCommodityMap ();

    CACHE_REMOVE(ns->name);

    qof_event_gen (&ns->inst, QOF_EVENT_DESTROY, nullptr);
    /* qof_instance_release(&ns->inst); */
    g_object_unref(ns);
}

/********************************************************************
 * gnc_commodity_table_foreach_commodity
 * call user-defined function once for every commodity in every
 * namespace
 ********************************************************************/

gboolean
gnc_commodity_table_foreach_commodity (const gnc_commodity_table * tbl,
                                       gboolean (*f)(gnc_commodity *, gpointer),
                                       gpointer user_data)
{
    if (!tbl || !f) return FALSE;

    return std::all_of (tbl->ns_table.begin(), tbl->ns_table.end(),
                        [&](const auto& str_ns)
                        { return std::all_of(str_ns.second->cm_table.begin(), str_ns.second->cm_table.end(),
                                             [&](const auto& n){ return f(n.second, user_data); });
                        });
}

/********************************************************************
 * gnc_commodity_table_destroy
 * cleanup and free.
 ********************************************************************/

void
gnc_commodity_table_destroy(gnc_commodity_table * t)
{
    if (!t) return;
    ENTER ("table=%p", t);

    for (auto it = t->ns_vec.rbegin(); it != t->ns_vec.rend(); ++it)
    {
        auto ns{*it};
        gnc_commodity_table_delete_namespace(t, ns->name);
    }

    t->ns_vec.~CommNSVec ();
    t->ns_table.~StringCommNSMap ();

    LEAVE ("table=%p", t);
    g_free(t);
}

/* =========================================================== */

/********************************************************************
 * gnc_commodity_table_add_default_data
 ********************************************************************/

#define CUR_I18N(String) dgettext ("iso_4217", String)

gboolean
gnc_commodity_table_add_default_data(gnc_commodity_table *table, QofBook *book)
{
    QofCollection *col;
    gnc_commodity* c;

    ENTER ("table=%p", table);
    gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_TEMPLATE, book);
    c = gnc_commodity_new(book, "template", GNC_COMMODITY_NS_TEMPLATE, "template", "template", 1);
    gnc_commodity_table_insert(table, c);

#include "iso-4217-currencies.c"

    /* We've just created the default namespaces and currencies.  Mark
     * these collections as clean because there is no USER entered data
     * in these collections as of yet. */
    col = qof_book_get_collection(book, GNC_ID_COMMODITY);
    qof_collection_mark_clean(col);
    col = qof_book_get_collection(book, GNC_ID_COMMODITY_NAMESPACE);
    qof_collection_mark_clean(col);

    LEAVE ("table=%p", table);
    return TRUE;
}

/********************************************************************
 ********************************************************************/
/* QofObject function implementation and registration */

#ifdef _MSC_VER
/* MSVC compiler doesn't have C99 "designated initializers"
 * so we wrap them in a macro that is empty on MSVC. */
# define DI(x) /* */
#else
# define DI(x) x
#endif
static QofObject commodity_object_def =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_ID_COMMODITY,
    DI(.type_label        = ) "Commodity",
    DI(.create            = ) nullptr,
    DI(.book_begin        = ) nullptr,
    DI(.book_end          = ) nullptr,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) qof_collection_foreach,
    DI(.printable         = ) (const char * (*)(gpointer)) gnc_commodity_get_fullname,
};

static QofObject namespace_object_def =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_ID_COMMODITY_NAMESPACE,
    DI(.type_label        = ) "Namespace",
    DI(.create            = ) nullptr,
    DI(.book_begin        = ) nullptr,
    DI(.book_end          = ) nullptr,
    DI(.is_dirty          = ) nullptr,
    DI(.mark_clean        = ) nullptr,
    DI(.foreach           = ) nullptr,
    DI(.printable         = ) nullptr,
};

static void
commodity_table_book_begin (QofBook *book)
{
    gnc_commodity_table *ct;
    ENTER ("book=%p", book);

    if (gnc_commodity_table_get_table(book))
        return;

    ct = gnc_commodity_table_new ();
    qof_book_set_data (book, GNC_COMMODITY_TABLE, ct);

    if (!gnc_commodity_table_add_default_data(ct, book))
    {
        PWARN("unable to initialize book's commodity_table");
    }

    LEAVE ("book=%p", book);
}

static void
commodity_table_book_end (QofBook *book)
{
    gnc_commodity_table *ct;

    ct = gnc_commodity_table_get_table (book);
    qof_book_set_data (book, GNC_COMMODITY_TABLE, nullptr);
    gnc_commodity_table_destroy (ct);
}

static QofObject commodity_table_object_def =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_ID_COMMODITY_TABLE,
    DI(.type_label        = ) "CommodityTable",
    DI(.create            = ) nullptr,
    DI(.book_begin        = ) commodity_table_book_begin,
    DI(.book_end          = ) commodity_table_book_end,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) nullptr,
    DI(.printable         = ) nullptr,
    DI(.version_cmp       = ) nullptr,
};

gboolean
gnc_commodity_table_register (void)
{
    if (!qof_object_register (&commodity_object_def))
        return FALSE;
    if (!qof_object_register (&namespace_object_def))
        return FALSE;
    return qof_object_register (&commodity_table_object_def);
}

/* *******************************************************************
*  gnc_monetary methods
********************************************************************/

/** Add a gnc_monetary to the list */
MonetaryList *
gnc_monetary_list_add_monetary(MonetaryList *list, gnc_monetary add_mon)
{
    MonetaryList *l = list, *tmp;
    for (tmp = list; tmp; tmp = tmp->next)
    {
        auto list_mon = static_cast<gnc_monetary*>(tmp->data);
        if (gnc_commodity_equiv(list_mon->commodity, add_mon.commodity))
        {
            list_mon->value = gnc_numeric_add(list_mon->value, add_mon.value,
                                              GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
            break;
        }
    }

    /* See if we found an entry, and add one if not */
    if (tmp == nullptr)
    {
        auto new_mon = static_cast<gnc_monetary*>(g_new0(gnc_monetary, 1));
        *new_mon = add_mon;
        l = g_list_prepend(l, new_mon);
    }

    return l;
}

/** Delete all entries in the list that have zero value.  Return list
    pointer will be a null pointer if there are no non-zero entries **/
MonetaryList *
gnc_monetary_list_delete_zeros(MonetaryList *list)
{
    MonetaryList *node, *next;
    for (node = list; node; node = next)
    {
        auto mon = static_cast<gnc_monetary*>(node->data);
        next = node->next;
        if (gnc_numeric_zero_p(mon->value))
        {
            g_free(mon);
            list = g_list_delete_link(list, node);
        }
    }
    return list;
}

/** Free a MonetaryList and all the monetaries it points to */
void
gnc_monetary_list_free(MonetaryList *list)
{
    MonetaryList *tmp;
    for (tmp = list; tmp; tmp = tmp->next)
    {
        g_free(tmp->data);
    }

    g_list_free(list);
}

/* ========================= END OF FILE ============================== */
