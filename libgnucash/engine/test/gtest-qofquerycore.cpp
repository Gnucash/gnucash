/********************************************************************\
 * gtest-qofquerycore.cpp -- Unit tests for qofquerycore            *
 *                                                                  *
 * Copyright 2018 Geert Janssens <geert@kobaltwit.be>               *
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
 \ *********************************************************************/

#include <config.h>
#include <glib.h>
#include "../test-core/test-engine-stuff.h"
#include "../qofquerycore.h"
#include "../qofquerycore-p.h"
#include <gtest/gtest.h>

TEST(qof_query_construct_predicate, string)
{
    query_string_def *pdata;
    pdata = (query_string_def*)qof_query_string_predicate(
        QOF_COMPARE_EQUAL,
        "Test",
        QOF_STRING_MATCH_NORMAL,
        FALSE
    );
    EXPECT_STREQ (QOF_TYPE_STRING,      pdata->pd.type_name);
    EXPECT_EQ (QOF_COMPARE_EQUAL,       pdata->pd.how);
    EXPECT_EQ (QOF_STRING_MATCH_NORMAL, pdata->options);
    EXPECT_STREQ ("Test",               pdata->matchstring);
    EXPECT_EQ (FALSE,                   pdata->is_regex);
}

TEST(qof_query_construct_predicate, date)
{
    query_date_def *pdata;
    pdata = (query_date_def*)qof_query_date_predicate(
        QOF_COMPARE_LT,
        QOF_DATE_MATCH_DAY,
        1524772012
    );
    EXPECT_STREQ (QOF_TYPE_DATE,   pdata->pd.type_name);
    EXPECT_EQ (QOF_COMPARE_LT,     pdata->pd.how);
    EXPECT_EQ (QOF_DATE_MATCH_DAY, pdata->options);
    EXPECT_EQ (1524772012,         pdata->date);
}

TEST(qof_query_construct_predicate, numeric)
{
    query_numeric_def *pdata;
    pdata = (query_numeric_def*)qof_query_numeric_predicate(
        QOF_COMPARE_LTE,
        QOF_NUMERIC_MATCH_CREDIT,
        { 500, 100 }
    );
    EXPECT_STREQ (QOF_TYPE_NUMERIC,      pdata->pd.type_name);
    EXPECT_EQ (QOF_COMPARE_LTE,          pdata->pd.how);
    EXPECT_EQ (QOF_NUMERIC_MATCH_CREDIT, pdata->options);
    EXPECT_TRUE (gnc_numeric_eq({ 500, 100 }, pdata->amount));
}

TEST(qof_query_construct_predicate, guid)
{
    GncGUID *guid = guid_new();
    GList *guidlist = g_list_prepend (NULL, guid);
    query_guid_def *pdata;
    pdata = (query_guid_def*)qof_query_guid_predicate(
        QOF_GUID_MATCH_ANY,
        guidlist
    );
    EXPECT_STREQ (QOF_TYPE_GUID,   pdata->pd.type_name);
    EXPECT_EQ (QOF_GUID_MATCH_ANY, pdata->options);
    EXPECT_TRUE (guid_equal (guid, (const GncGUID*)pdata->guids->data));
    EXPECT_EQ (NULL,               pdata->guids->next);
}

TEST(qof_query_construct_predicate, int32)
{
    query_int32_def *pdata;
    pdata = (query_int32_def*)qof_query_int32_predicate(
        QOF_COMPARE_EQUAL,
        -613
    );
    EXPECT_STREQ (QOF_TYPE_INT32, pdata->pd.type_name);
    EXPECT_EQ (QOF_COMPARE_EQUAL, pdata->pd.how);
    EXPECT_EQ (-613,              pdata->val);
}

TEST(qof_query_construct_predicate, int64)
{
    query_int64_def *pdata;
    pdata = (query_int64_def*)qof_query_int64_predicate(
        QOF_COMPARE_GT,
        1000000
    );
    EXPECT_STREQ (QOF_TYPE_INT64, pdata->pd.type_name);
    EXPECT_EQ (QOF_COMPARE_GT,    pdata->pd.how);
    EXPECT_EQ (1000000,           pdata->val);
}

TEST(qof_query_construct_predicate, double)
{
    query_double_def *pdata;
    pdata = (query_double_def*)qof_query_double_predicate(
        QOF_COMPARE_GTE,
        10.05
    );
    EXPECT_STREQ (QOF_TYPE_DOUBLE, pdata->pd.type_name);
    EXPECT_EQ (QOF_COMPARE_GTE,    pdata->pd.how);
    EXPECT_EQ (10.05,              pdata->val);
}

TEST(qof_query_construct_predicate, boolean)
{
    query_boolean_def *pdata;
    pdata = (query_boolean_def*)qof_query_boolean_predicate(
        QOF_COMPARE_NEQ,
        TRUE
    );
    EXPECT_STREQ (QOF_TYPE_BOOLEAN, pdata->pd.type_name);
    EXPECT_EQ (QOF_COMPARE_NEQ,     pdata->pd.how);
    EXPECT_EQ (TRUE,                pdata->val);
}

TEST(qof_query_construct_predicate, char)
{
    query_char_def *pdata;
    pdata = (query_char_def*)qof_query_char_predicate(
        QOF_CHAR_MATCH_ANY,
        "Foo"
    );
    EXPECT_STREQ (QOF_TYPE_CHAR,   pdata->pd.type_name);
    EXPECT_EQ (QOF_COMPARE_EQUAL,  pdata->pd.how);
    EXPECT_EQ (QOF_CHAR_MATCH_ANY, pdata->options);
    EXPECT_STREQ ("Foo",           pdata->char_list);
}

TEST(qof_query_core_predicate_copy, date)
{
    qof_query_core_init();
    query_date_def *pdata, *pdata2;
    pdata = (query_date_def*)qof_query_date_predicate(
        QOF_COMPARE_LT,
        QOF_DATE_MATCH_DAY,
        1524772012
    );
    pdata2 = (query_date_def*) qof_query_core_predicate_copy ((QofQueryPredData*)pdata);

    EXPECT_STREQ (pdata2->pd.type_name, pdata->pd.type_name);
    EXPECT_EQ (pdata2->pd.how,          pdata->pd.how);
    EXPECT_EQ (pdata2->options,         pdata->options);
    EXPECT_EQ (pdata2->date,            pdata->date);
}

TEST(qof_query_core_predicate_get_date, date)
{
    qof_query_core_init();
    time64 date;
    QofQueryPredData *pdata;
    pdata = qof_query_date_predicate(
        QOF_COMPARE_LT,
        QOF_DATE_MATCH_DAY,
        1524772012
    );

    EXPECT_TRUE (qof_query_date_predicate_get_date(pdata, &date));
    EXPECT_EQ (1524772012, date);
}

TEST(qof_query_core_predicate_get_date, numeric)
{
    qof_query_core_init();
    time64 date;
    QofQueryPredData *pdata;
    pdata = qof_query_numeric_predicate(
        QOF_COMPARE_LT,
        QOF_NUMERIC_MATCH_CREDIT,
        { 1000, 100 }
    );

    EXPECT_FALSE (qof_query_date_predicate_get_date(pdata, &date));
}
