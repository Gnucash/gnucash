/*
 * qif-parse.c -- parse QIF
 *
 * Written by:        Derek Atkins <derek@ihtfp.com>
 * Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <glib.h>
#include <glib/gi18n.h>
#include <string.h>

/* For regex */
#include <sys/types.h>
#include <regex.h>

#include <stdarg.h>

#include "gnc-engine.h"
#include "gnc-ui-util.h"

#include "qif-import-p.h"
#include "qif-objects-p.h"

#include "import-parse.h"

static QofLogModule log_module = GNC_MOD_IMPORT;

/* An array of handlers for the various bang-types */
static QifHandler qif_handlers[QIF_TYPE_MAX+1] = { NULL };

/* Parser Regular Expressions */
static gboolean qifp_regex_compiled = FALSE;
static regex_t category_regex;

/* A Hash Table of bang-types */
static GHashTable *qif_bangtype_map = NULL;

/* A Hash Table of action strings */
static GHashTable *qif_action_map = NULL;

/* A Hash Table of account types */
static GHashTable *qif_atype_map = NULL;

/************************************************************************/

/* Register a handler */
void
qif_register_handler(QifType type, QifHandler handler)
{
  if (type <= 0 || type > QIF_TYPE_MAX) {
    PERR("Invalid type: %d", type);
    return;
  }
  qif_handlers[type] = handler;
}

static void
compile_regex()
{
  regcomp(&category_regex,
      "^ *(\\[)?([^]/\\|]*)(]?)(/([^\\|]*))?(\\|(\\[)?([^]/]*)(]?)(/(.*))?)? *$",
          REG_EXTENDED);

  qifp_regex_compiled = TRUE;
}

#define QIF_ADD_TYPE(ts,t) \
        g_hash_table_insert(qif_bangtype_map, ts, GINT_TO_POINTER(t)); \
        g_hash_table_insert(qif_bangtype_map, _(ts), GINT_TO_POINTER(t));

static void
build_bangtype_map()
{
  g_return_if_fail(!qif_bangtype_map);

  qif_bangtype_map = g_hash_table_new(g_str_hash, g_str_equal);
  g_assert(qif_bangtype_map);

  /* Translators FIXME: It is unclear whether these strings should
     really be translated, and if yes, into which translation. */
  QIF_ADD_TYPE(N_("type:bank"), QIF_TYPE_BANK);
  QIF_ADD_TYPE(N_("type:cash"), QIF_TYPE_CASH);
  QIF_ADD_TYPE(N_("type:ccard"), QIF_TYPE_CCARD);
  QIF_ADD_TYPE(N_("type:invst"), QIF_TYPE_INVST);
  QIF_ADD_TYPE(N_("type:port"), QIF_TYPE_PORT);
  QIF_ADD_TYPE(N_("type:oth a"), QIF_TYPE_OTH_A);
  QIF_ADD_TYPE(N_("type:oth l"), QIF_TYPE_OTH_L);
  QIF_ADD_TYPE(N_("type:class"), QIF_TYPE_CLASS);
  QIF_ADD_TYPE(N_("type:cat"), QIF_TYPE_CAT);
  QIF_ADD_TYPE(N_("type:security"), QIF_TYPE_SECURITY);
  QIF_ADD_TYPE(N_("account"), QIF_ACCOUNT);
  QIF_ADD_TYPE(N_("option:autoswitch"), QIF_AUTOSWITCH);
  QIF_ADD_TYPE(N_("clear:autoswitch"), QIF_CLEAR_AUTOSWITCH);
}
#undef QIF_ADD_TYPE

#define QIF_ADD_ACT(ts,t) \
        g_hash_table_insert(qif_action_map, ts, GINT_TO_POINTER(t));

static void
build_action_map()
{
  g_return_if_fail(!qif_action_map);

  qif_action_map = g_hash_table_new(g_str_hash, g_str_equal);
  g_assert(qif_action_map);

  QIF_ADD_ACT("buy", QIF_A_BUY);
  QIF_ADD_ACT("cvrshrt", QIF_A_BUY);
  QIF_ADD_ACT("kauf", QIF_A_BUY);
  QIF_ADD_ACT("buyx", QIF_A_BUYX);
  QIF_ADD_ACT("cvrshrtx", QIF_A_BUYX);
  QIF_ADD_ACT("kaufx", QIF_A_BUYX);
  QIF_ADD_ACT("cglong", QIF_A_CGLONG);
  QIF_ADD_ACT("kapgew", QIF_A_CGLONG); /* Kapitalgewinnsteuer */
  QIF_ADD_ACT("cglongx", QIF_A_CGLONG);
  QIF_ADD_ACT("kapgewx", QIF_A_CGLONG);
  QIF_ADD_ACT("cgmid", QIF_A_CGMID);
  QIF_ADD_ACT("cgmidx", QIF_A_CGMIDX);
  QIF_ADD_ACT("cgshort", QIF_A_CGSHORT);
  QIF_ADD_ACT("k.gewsp", QIF_A_CGSHORT);
  QIF_ADD_ACT("cgshortx", QIF_A_CGSHORTX);
  QIF_ADD_ACT("k.gewspx", QIF_A_CGSHORTX);
  QIF_ADD_ACT("div", QIF_A_DIV); /* dividende */
  QIF_ADD_ACT("divx", QIF_A_DIVX);
  //QIF_ADD_ACT("exercise", QIF_A_EXERCISE);
  //QIF_ADD_ACT("exercisex", QIF_A_EXERCISEX);
  //QIF_ADD_ACT("expire", QIF_A_EXPIRE);
  //QIF_ADD_ACT("grant", QIF_A_GRANT);
  QIF_ADD_ACT("int", QIF_A_INTINC);
  QIF_ADD_ACT("intinc", QIF_A_INTINC);
  QIF_ADD_ACT("aktzu", QIF_A_INTINC); /* zinsen */
  QIF_ADD_ACT("intx", QIF_A_INTINCX);
  QIF_ADD_ACT("intincx", QIF_A_INTINCX);
  QIF_ADD_ACT("margint", QIF_A_MARGINT);
  QIF_ADD_ACT("margintx", QIF_A_MARGINTX);
  QIF_ADD_ACT("miscexp", QIF_A_MISCEXP);
  QIF_ADD_ACT("miscexpx", QIF_A_MISCEXPX);
  QIF_ADD_ACT("miscinc", QIF_A_MISCINC);
  QIF_ADD_ACT("cash", QIF_A_MISCINC);
  QIF_ADD_ACT("miscincx", QIF_A_MISCINCX);
  QIF_ADD_ACT("reinvdiv", QIF_A_REINVDIV);
  QIF_ADD_ACT("reinvint", QIF_A_REINVINT);
  QIF_ADD_ACT("reinvzin", QIF_A_REINVINT);
  QIF_ADD_ACT("reinvlg", QIF_A_REINVLG);
  QIF_ADD_ACT("reinvkur", QIF_A_REINVLG);
  QIF_ADD_ACT("reinvmd", QIF_A_REINVMD);
  QIF_ADD_ACT("reinvsg", QIF_A_REINVSG);
  QIF_ADD_ACT("reinvksp", QIF_A_REINVSG);
  QIF_ADD_ACT("reinvsh", QIF_A_REINVSH);
  QIF_ADD_ACT("reminder", QIF_A_REMINDER);
  QIF_ADD_ACT("erinnerg", QIF_A_REMINDER);
  QIF_ADD_ACT("rtrncap", QIF_A_RTRNCAP);
  QIF_ADD_ACT("rtrncapx", QIF_A_RTRNCAPX);
  QIF_ADD_ACT("sell", QIF_A_SELL);
  QIF_ADD_ACT("shtsell", QIF_A_SELL);
  QIF_ADD_ACT("verkauf", QIF_A_SELL); /* verkaufen */
  QIF_ADD_ACT("sellx", QIF_A_SELLX);
  QIF_ADD_ACT("shtsellx", QIF_A_SELLX);
  QIF_ADD_ACT("verkaufx", QIF_A_SELLX); /* verkaufen */
  QIF_ADD_ACT("shrsin", QIF_A_SHRSIN);
  QIF_ADD_ACT("aktzu", QIF_A_SHRSIN);
  QIF_ADD_ACT("shrsout", QIF_A_SHRSOUT);
  QIF_ADD_ACT("aktab", QIF_A_SHRSOUT);
  QIF_ADD_ACT("stksplit", QIF_A_STKSPLIT);
  QIF_ADD_ACT("aktsplit", QIF_A_STKSPLIT);
  //QIF_ADD_ACT("vest", QIF_A_VEST);
  QIF_ADD_ACT("xin", QIF_A_XIN);
  QIF_ADD_ACT("contribx", QIF_A_XIN);
  QIF_ADD_ACT("xout", QIF_A_XOUT);
  QIF_ADD_ACT("withdrwx", QIF_A_XOUT);
}
#undef QIF_ADD_ACT

static GList *
make_list(int count, ...)
{
  GList *result = NULL;
  GNCAccountType type;
  va_list ap;

  va_start (ap, count);
  while (count--) {
    type = va_arg (ap, GNCAccountType);
    result = g_list_prepend (result, GINT_TO_POINTER(type));
  }
  va_end (ap);


  return g_list_reverse(result);
}

#define QIF_ADD_ATYPE(a,t) g_hash_table_insert(qif_atype_map, a, t);
static void
build_atype_map()
{
  g_return_if_fail(!qif_atype_map);

  qif_atype_map = g_hash_table_new(g_str_hash, g_str_equal);
  g_assert(qif_atype_map);

  QIF_ADD_ATYPE("bank", make_list(1, ACCT_TYPE_BANK));
  QIF_ADD_ATYPE("port", make_list(1, ACCT_TYPE_BANK));
  QIF_ADD_ATYPE("cash", make_list(1, ACCT_TYPE_CASH));
  QIF_ADD_ATYPE("ccard", make_list(1, ACCT_TYPE_CREDIT));
  QIF_ADD_ATYPE("invst", make_list(3, ACCT_TYPE_BANK, ACCT_TYPE_STOCK,
				   ACCT_TYPE_MUTUAL));
  QIF_ADD_ATYPE("oth a", make_list(3, ACCT_TYPE_ASSET, ACCT_TYPE_BANK,
				   ACCT_TYPE_CASH));
  QIF_ADD_ATYPE("oth l", make_list(2, ACCT_TYPE_LIABILITY, ACCT_TYPE_CREDIT));
  QIF_ADD_ATYPE("mutual", make_list(3, ACCT_TYPE_BANK, ACCT_TYPE_MUTUAL,
				    ACCT_TYPE_STOCK));

  /* Internal types */
  QIF_ADD_ATYPE("__any_bank__", make_list(5, ACCT_TYPE_BANK, ACCT_TYPE_CREDIT,
					  ACCT_TYPE_CASH, ACCT_TYPE_ASSET,
                                          ACCT_TYPE_LIABILITY));
  QIF_ADD_ATYPE("__all__", make_list(7, ACCT_TYPE_BANK, ACCT_TYPE_CREDIT,
				     ACCT_TYPE_CASH, ACCT_TYPE_ASSET,
				     ACCT_TYPE_LIABILITY, ACCT_TYPE_STOCK,
				     ACCT_TYPE_MUTUAL));
  QIF_ADD_ATYPE("__stock__", make_list(2, ACCT_TYPE_STOCK, ACCT_TYPE_MUTUAL));
  QIF_ADD_ATYPE("__income__", make_list(1, ACCT_TYPE_INCOME));
  QIF_ADD_ATYPE("__expense__", make_list(1, ACCT_TYPE_EXPENSE));
  QIF_ADD_ATYPE("__equity__", make_list(1, ACCT_TYPE_EQUITY));
}
#undef QIF_ADD_ATYPE

/************************************************************************/

/*
 * We've got a !Type line.  Parse the line into the appropriate
 * type and then initialize the handler.
 */
void
qif_parse_bangtype(QifContext ctx, const char *line)
{
  QifType type;
  char *bangtype;
  gpointer result;

  g_return_if_fail(line && *line == '!');

  if (!qif_bangtype_map)
    build_bangtype_map();

  /* Make a local copy so we can manipulate it.
   * - strip off leading/trailing whitespace
   * - make it all lower case
   */
  bangtype = g_utf8_strdown(line+1, -1);
  g_strstrip(bangtype);

  /* In some cases we get "!Type Bank" -- change the space to a colon */
  if (!strncmp(bangtype, "type ", 5))
    bangtype[5] = ':';

  /* Lookup the bangtype in the map and then destroy the local copy */
  result = g_hash_table_lookup(qif_bangtype_map, bangtype);
  g_free(bangtype);

  if (!result) {
    PWARN("Unknown bang-type at line %d: %s.  Ignored", ctx->lineno, line);
    return;
  }
  type = GPOINTER_TO_INT(result);

  /* Set the current context parse type and handler */
  ctx->parse_type = type;
  ctx->handler = qif_handlers[type];

  /* now initialize this new parse type (if there's an init function) */
  if (ctx->handler && ctx->handler->init)
    ctx->handler->init(ctx);
}

/* returns TRUE if successful, FALSE if there is a problem */
gboolean
qif_parse_split_category(const char* str,
                         char** cat, gboolean *cat_is_acct, char** cat_class,
                         char** miscx_cat, gboolean *miscx_cat_is_acct,
                         char **miscx_class)
{
  /* This is a pretty f**ked up string.  Basically it looks like:
   *  ([)cat-or-acct(])(/(class))(|([)cat-of-acct(])(/ext))
   *
   * where data in parens is "optional" (depending on the context).
   *
   * examples from reality:
   *
   * category
   * category:subcategory
   * category/class
   * category:subcat/class
   * [account]
   * [account]/class
   *
   * cat/cat-class|miscx-cat/miscx-class
   */

  regmatch_t pmatch[12];

  g_return_val_if_fail(cat && cat_is_acct && cat_class &&
                       miscx_cat && miscx_cat_is_acct && miscx_class, FALSE);


  if (!qifp_regex_compiled)
    compile_regex();

  if (regexec(&category_regex, str, 12, pmatch, 0) != 0) {
    PERR("category match failed");
    return FALSE;
  }

  /*
   * what the substrings mean:
   * 1 the opening [ for a transfer
   * 2 the category
   * 3 the closing ]
   * 4 the class /
   * 5 the class 
   * 6 the miscx expression (whole thing)
   * 7 the opening [ 
   * 8 the miscx category
   * 9 the closing ]
   * 10 the class /
   * 11 the class 
   */

  if (pmatch[2].rm_so == -1) {
    PERR("no category match found!");
    return FALSE;
  }

  /* catgory name */
  *cat = g_strndup(str+pmatch[2].rm_so, pmatch[2].rm_eo - pmatch[2].rm_so);
  /* category is account? */
  *cat_is_acct = (pmatch[1].rm_so != -1 && pmatch[3].rm_so != -1);
  /* category class */
  *cat_class = (pmatch[4].rm_so != -1 ?
                g_strndup(str+pmatch[5].rm_so, pmatch[5].rm_eo - pmatch[5].rm_so) :
                NULL);

  /* miscx category name */
  *miscx_cat = (pmatch[6].rm_so != -1 ?
                g_strndup(str+pmatch[8].rm_so, pmatch[8].rm_eo - pmatch[8].rm_so) :
                NULL);
  /* miscx cat is acct */
  *miscx_cat_is_acct  = (pmatch[7].rm_so != -1 && pmatch[9].rm_so != -1);
  /* miscx class */
  *miscx_class = (pmatch[10].rm_so != -1 ?
                  g_strndup(str+pmatch[11].rm_so,
                            pmatch[11].rm_eo - pmatch[11].rm_so) : NULL);

  return TRUE;
}

/*
 * qif_parse_cleared -- parse the 'C'leared field of a QIF Transaction.
 * returns the QIF reconciled flag.
 *
 * * means cleared, x or X means reconciled, and ! or ? mean some 
 * budget related stuff I don't understand. 
 */
QifRecnFlag
qif_parse_cleared(QifLine line)
{
  g_return_val_if_fail(line, QIF_R_NO);
  g_return_val_if_fail(line->line, QIF_R_NO);

  switch (*line->line) {
  case '*':
    return QIF_R_CLEARED;
  case 'x':
  case 'X':
    return QIF_R_RECONCILED;
  case '?':
  case '!':
    return QIF_R_BUDGETED;
  default:
    PERR("Unknown QIF Cleared flag at line %d: %s", line->lineno, line->line);
    return QIF_R_NO;
  }
}

QifAction qif_parse_action(QifLine line)
{
  QifAction qaction;
  gpointer result;
  char *action;

  g_return_val_if_fail(line, QIF_A_NONE);
  g_return_val_if_fail(line->line, QIF_A_NONE);

  if (!qif_action_map)
    build_action_map();

  /* Duplicate the action and force it to lower case and strip any spaces */
  action = g_utf8_strdown(line->line, -1);
  g_strstrip(action);

  result = g_hash_table_lookup(qif_action_map, action);
  g_free(action);

  if (!result) {
    /* XXX: pop up a dialog? */
    PWARN("Unknown Action at line %d: %s.  Some transactions may be discarded",
          line->lineno, line->line);
    return QIF_A_NONE;
  }

  qaction = GPOINTER_TO_INT(result);
  return qaction;
}

GList * qif_parse_acct_type(const char *str, gint lineno)
{
  GList *result;
  char *type;

  if (!qif_atype_map)
    build_atype_map();

  /* Duplicate the type and force it to lower case and strip any spaces */
  type = g_utf8_strdown(str, -1);
  g_strstrip(type);

  result = g_hash_table_lookup(qif_atype_map, type);
  g_free(type);

  if (!result) {
    PWARN("Unknown account type at line %d: %s. ", lineno, str);
    result = g_hash_table_lookup(qif_atype_map, "bank");
    g_return_val_if_fail(result, NULL);
  }

  return result;
}

GList * qif_parse_acct_type_guess(QifType type)
{
  const char *atype = NULL;

  switch (type) {
  case QIF_TYPE_BANK: atype = "bank"; break;
  case QIF_TYPE_CASH: atype = "cash"; break;
  case QIF_TYPE_CCARD: atype = "ccard"; break;
  case QIF_TYPE_INVST: atype = "invst"; break;
  case QIF_TYPE_PORT: atype = "port"; break;
  case QIF_TYPE_OTH_A: atype = "oth a"; break;
  case QIF_TYPE_OTH_L: atype = "oth l"; break;
  default: return NULL;
  }

  return qif_parse_acct_type(atype, -1);
}

/***********************************************************************
 * Parsing numbers and dates...
 */

typedef struct _parse_helper {
  QifContext                ctx;

  GncImportFormat        budget;
  GncImportFormat        limit;
  GncImportFormat        amount;
  GncImportFormat        d_amount;
  GncImportFormat        price;
  GncImportFormat        shares;
  GncImportFormat        commission;
  GncImportFormat        date;
} *parse_helper_t;

#define QIF_PARSE_CHECK_NUMBER(str,help) { \
        if (str) (help) = gnc_import_test_numeric((str), (help)); \
}
#define QIF_PARSE_PARSE_NUMBER(str,fmt,val) { \
        if (str) gnc_import_parse_numeric((str), (fmt), (val)); \
}

static void
qif_parse_check_account(gpointer key, gpointer val, gpointer data)
{
  parse_helper_t helper = data;
  QifAccount acct = val;

  QIF_PARSE_CHECK_NUMBER(acct->limitstr, helper->limit);
  QIF_PARSE_CHECK_NUMBER(acct->budgetstr, helper->budget);
}

static void
qif_parse_parse_account(gpointer key, gpointer val, gpointer data)
{
  parse_helper_t helper = data;
  QifAccount acct = val;

  QIF_PARSE_PARSE_NUMBER(acct->limitstr, helper->limit, &acct->limit);
  QIF_PARSE_PARSE_NUMBER(acct->budgetstr, helper->budget, &acct->budget);
}

static void
qif_parse_check_category(gpointer key, gpointer val, gpointer data)
{
  parse_helper_t helper = data;
  QifCategory cat = val;

  QIF_PARSE_CHECK_NUMBER(cat->budgetstr, helper->budget);
}

static void
qif_parse_parse_category(gpointer key, gpointer val, gpointer data)
{
  parse_helper_t helper = data;
  QifCategory cat = val;

  QIF_PARSE_PARSE_NUMBER(cat->budgetstr, helper->budget, &cat->budget);
}

static void
qif_parse_check_txn(gpointer val, gpointer data)
{
  parse_helper_t helper = data;
  QifTxn txn = val;
  QifSplit split;
  QifInvstTxn itxn;
  GList *node;

  /* Check the date */
  helper->date = gnc_import_test_date(txn->datestr, helper->date);

  /* If this is an investment transaction, then all the info is in
   * the invst_info.  Otherwise it's all in the splits.
   */
  itxn = txn->invst_info;
  if (itxn) {
    QIF_PARSE_CHECK_NUMBER(itxn->amountstr, helper->amount);
    QIF_PARSE_CHECK_NUMBER(itxn->d_amountstr, helper->d_amount);
    QIF_PARSE_CHECK_NUMBER(itxn->pricestr, helper->price);
    QIF_PARSE_CHECK_NUMBER(itxn->sharesstr, helper->shares);
    QIF_PARSE_CHECK_NUMBER(itxn->commissionstr, helper->commission);

  } else {
    split = txn->default_split;
    node = txn->splits;
    do {
      QIF_PARSE_CHECK_NUMBER(split->amountstr, helper->amount);

      if (node) {
        split = node->data;
        node = node->next;
      } else
        split = NULL;
    } while (split);
  }
}

static void
qif_parse_parse_txn(gpointer val, gpointer data)
{
  parse_helper_t helper = data;
  QifTxn txn = val;
  QifSplit split;
  QifInvstTxn itxn;
  GList *node;

  /* Parse the date */
  gnc_import_parse_date(txn->datestr, helper->date, &txn->date);

  /* If this is an investment transaction, then all the info is in
   * the invst_info.  Otherwise it's all in the splits.
   */
  itxn = txn->invst_info;
  if (itxn) {
    QIF_PARSE_PARSE_NUMBER(itxn->amountstr, helper->amount, &itxn->amount);
    QIF_PARSE_PARSE_NUMBER(itxn->d_amountstr, helper->d_amount, &itxn->d_amount);
    QIF_PARSE_PARSE_NUMBER(itxn->pricestr, helper->price, &itxn->price);
    QIF_PARSE_PARSE_NUMBER(itxn->sharesstr, helper->shares, &itxn->shares);
    QIF_PARSE_PARSE_NUMBER(itxn->commissionstr, helper->commission,
                           &itxn->commission);

    qif_invst_txn_setup_splits(helper->ctx, txn);

  } else {
    split = txn->default_split;
    node = txn->splits;
    do {
      QIF_PARSE_PARSE_NUMBER(split->amountstr, helper->amount, &split->amount);

      if (node) {
        split = node->data;
        node = node->next;
      } else
        split = NULL;
    } while (split);

    qif_txn_setup_splits(txn);
  }
}

void
qif_parse_all(QifContext ctx, gpointer arg)
{
  struct _parse_helper helper;

  helper.ctx = ctx;

  /* PARSE ACCOUNTS */

  /* First, figure out the formats */
  helper.limit = GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA;
  helper.budget = GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA;
  qif_object_map_foreach(ctx, QIF_O_ACCOUNT, qif_parse_check_account, &helper);

  /* Make sure it's not ambiguous */
  if (helper.limit & (helper.limit - 1)) helper.limit = GNCIF_NUM_PERIOD;
  if (helper.budget & (helper.budget - 1)) helper.budget = GNCIF_NUM_PERIOD;

  /* Now convert the numbers */
  qif_object_map_foreach(ctx, QIF_O_ACCOUNT, qif_parse_parse_account, &helper);

  /* PARSE CATEGORIES */

  helper.budget = GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA;
  qif_object_map_foreach(ctx, QIF_O_CATEGORY, qif_parse_check_category, &helper);

  /* make sure it's not ambiguous */
  if (helper.budget & (helper.budget - 1)) helper.budget = GNCIF_NUM_PERIOD;

  /* Now convert the numbers */
  qif_object_map_foreach(ctx, QIF_O_CATEGORY, qif_parse_parse_category, &helper);

  /* PARSE TRANSACTIONS */
  helper.amount = GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA;
  helper.d_amount = GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA;
  helper.price = GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA;
  helper.shares = GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA;
  helper.commission = GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA;
  helper.date = GNCIF_DATE_MDY | GNCIF_DATE_DMY | GNCIF_DATE_YMD | GNCIF_DATE_YDM;

  qif_object_list_foreach(ctx, QIF_O_TXN, qif_parse_check_txn, &helper);

  /* check/fix ambiguities */
  if (helper.amount & (helper.amount - 1)) helper.amount = GNCIF_NUM_PERIOD;
  if (helper.d_amount & (helper.d_amount - 1)) helper.d_amount = GNCIF_NUM_PERIOD;
  if (helper.price & (helper.price - 1)) helper.price = GNCIF_NUM_PERIOD;
  if (helper.shares & (helper.shares - 1)) helper.shares = GNCIF_NUM_PERIOD;
  if (helper.commission & (helper.commission - 1))
    helper.commission = GNCIF_NUM_PERIOD;

  if (helper.date & (helper.date - 1)) {
    helper.date = gnc_import_choose_fmt(_("The Date format is ambiguous.  "
                                          "Please choose the correct format."),
                                        helper.date, arg);
  }

  /* now parse it.. */
  qif_object_list_foreach(ctx, QIF_O_TXN, qif_parse_parse_txn, &helper);
}

typedef struct {
  QifContext        ctx;
  GList *        list;
  const char*        type;
} qif_merge_t;

static void
qif_merge_accts(gpointer key, gpointer value, gpointer data)
{
  qif_merge_t *merge = data;
  QifAccount acct = value;

  /* Merge into the context.  Remember items moved into the parent */
  if (qif_account_merge(merge->ctx, acct) == acct)
    merge->list = g_list_prepend(merge->list, acct->name);
}

static void
qif_merge_cats(gpointer key, gpointer value, gpointer data)
{
  qif_merge_t *merge = data;
  QifCategory cat = value;

  /* Merge into the context.  Remember items moved into the parent */
  if (qif_cat_merge(merge->ctx, cat) == cat)
    merge->list = g_list_prepend(merge->list, cat->name);
}

static void
qif_merge_classes(gpointer key, gpointer value, gpointer data)
{
  qif_merge_t *merge = data;
  QifClass qclass = value;

  /* Merge into the context.  Remember items moved into the parent */
  if (qif_class_merge(merge->ctx, qclass) == qclass)
    merge->list = g_list_prepend(merge->list, qclass->name);
}

static void
qif_merge_secs(gpointer key, gpointer value, gpointer data)
{
  qif_merge_t *merge = data;
  QifSecurity sec = value;

  /* Merge into the context.  Remember items moved into the parent */
  if (qif_security_merge(merge->ctx, sec) == sec)
    merge->list = g_list_prepend(merge->list, sec->name);
}

static void
qif_merge_del(gpointer obj, gpointer data)
{
  qif_merge_t *merge = data;
  const char *name = obj;

  qif_object_map_remove(merge->ctx, merge->type, name);
}

static void
qif_massage_split(QifSplit split, QifContext ctx)
{
  const char *type = QIF_O_CATEGORY;
  char *name;

  if (split->cat.obj) {
    if (split->cat_is_acct) {
      type = QIF_O_ACCOUNT;
      name = split->cat.acct->name;
    } else
      name = split->cat.cat->name;

    split->cat.obj = qif_object_map_lookup(ctx, type, name);
  }

  if (split->cat_class) {
    split->cat_class = (QifClass) qif_object_map_lookup(ctx, QIF_O_CLASS,
                                                        split->cat_class->name);
  }
}

static void
qif_massage_itxn(QifInvstTxn itxn, QifContext ctx)
{
  const char *type = QIF_O_CATEGORY;
  char *name;

  if (itxn->far_cat.obj) {
    if (itxn->far_cat_is_acct) {
      type = QIF_O_ACCOUNT;
      name = itxn->far_cat.acct->name;
    } else
      name = itxn->far_cat.cat->name;

    itxn->far_cat.obj = qif_object_map_lookup(ctx, type, name);
  }
}

static void
qif_massage_txn(gpointer obj, gpointer data)
{
  QifTxn txn = obj;
  QifContext ctx = data;
  QifSplit split;
  GList *node;

  if (txn->from_acct)
    txn->from_acct = (QifAccount) qif_object_map_lookup(ctx, QIF_O_ACCOUNT,
                                                        txn->from_acct->name);

  if (txn->invst_info)
    qif_massage_itxn(txn->invst_info, ctx);

  if (txn->default_split)
    qif_massage_split(txn->default_split, ctx);

  for (node = txn->splits; node; node = node->next) {
    split = node->data;
    qif_massage_split(split, ctx);
  }
}

void
qif_parse_merge_files(QifContext ctx)
{
  GList *node;
  GList *accts = NULL;
  GList *cats = NULL;
  GList *classes = NULL;
  GList *securities = NULL;
  QifContext fctx;

  qif_merge_t merge;

  g_return_if_fail(ctx);

  /* Make sure each of the "file" contexts have been parsed.
   * note that we don't care about OUR context -- we can run this
   * process multiple times safely.
   */
  for (node = ctx->files; node; node = node->next) {
    fctx = node->data;
    g_return_if_fail(fctx->parsed);
  }


  /* Iterate over each file.  Merge the Accounts, Categories, Classes,
   * Securities, and Transactions into the top-level context.  Be sure
   * to re-point all Transaction/Split category/class/account pointers
   * to the new top-level item.  Then be sure to remove the
   * "duplicated" items so we don't double-free (as we don't refcount,
   * either).
   */
  for (node = ctx->files; node; node = node->next) {
    fctx = node->data;

    /* Merge accts, categories, classes, and securities */

    merge.ctx = ctx;
    merge.list = NULL;
    qif_object_map_foreach(fctx, QIF_O_ACCOUNT, qif_merge_accts, &merge);
    accts = merge.list;

    merge.list = NULL;
    qif_object_map_foreach(fctx, QIF_O_CATEGORY, qif_merge_cats, &merge);
    cats = merge.list;

    merge.list = NULL;
    qif_object_map_foreach(fctx, QIF_O_CLASS, qif_merge_classes, &merge);
    classes = merge.list;

    merge.list = NULL;
    qif_object_map_foreach(fctx, QIF_O_SECURITY, qif_merge_secs, &merge);
    securities = merge.list;


    /* repoint the transactions to the merged context data */
    qif_object_list_foreach(fctx, QIF_O_TXN, qif_massage_txn, ctx);


    /* then remove from the file context objects referenced in the top context */
    merge.ctx = fctx;
    merge.type = QIF_O_ACCOUNT;
    g_list_foreach(accts, qif_merge_del, &merge);
    g_list_free(accts);

    merge.type = QIF_O_CATEGORY;
    g_list_foreach(cats, qif_merge_del, &merge);
    g_list_free(cats);

    merge.type = QIF_O_CLASS;
    g_list_foreach(classes, qif_merge_del, &merge);
    g_list_free(classes);

    merge.type = QIF_O_SECURITY;
    g_list_foreach(securities, qif_merge_del, &merge);
    g_list_free(securities);

  }

  /* We've been parsed */
  ctx->parsed = TRUE;
}
