/********************************************************************
 * test-tx-import.cpp: test suite for the GncTxImport class.        *
 * Copyright 2016 Geert Janssens <geert.gnucash@kobaltwit.be>       *
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
 * along with this program; if not, you can retrieve it from        *
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html            *
 * or contact:                                                      *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/

#include <guid.hpp>
#include "../gnc-tokenizer.hpp"
#include "../gnc-tokenizer-csv.hpp"
#include "../gnc-tokenizer-fw.hpp"
#include <gtest/gtest.h>
#include <iostream>
#include <fstream>      // fstream

#include <string>
#include <stdlib.h>     /* getenv */

/* Add specific headers for this class */
#include "../gnc-import-tx.hpp"

//typedef struct
//{
//    GncTxImport* parse_data;
//} Fixture;

typedef struct
{
    int          date_fmt;
    const gchar *date_str;
    int          exp_year;
    int          exp_month;
    int          exp_day;
} parse_date_data;

class GncTxImportTest : public ::testing::Test
{
public:
    GncTxImportTest()
    {
        tx_importer = std::unique_ptr<GncTxImport>(new GncTxImport);
    }


protected:
    std::unique_ptr<GncTxImport> tx_importer;
};
