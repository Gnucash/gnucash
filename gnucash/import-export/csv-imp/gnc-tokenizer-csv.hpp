/********************************************************************\
 * gnc-tokenizer-csv.hpp - takes a csv file and converts it into a  *
 *                         two-dimensional vector of strings (table)*
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
\********************************************************************/

/** @file
     @brief Class to convert a csv file into vector of string vectors.
     One can define the separator characters to use to split each line
     into multiple fields. Quote characters will be removed.
     However, no gnucash specific interpretation is done yet, that's up
     to the code using this class.
     *
     gnc-tokenizer-csv.hpp
     @author Copyright (c) 2015 Geert Janssens <geert@kobaltwit.be>
 */

#ifndef GNC_CSV_TOKENIZER_HPP
#define GNC_CSV_TOKENIZER_HPP

extern "C" {
#include <config.h>
}

#include <iostream>
#include <fstream>      // fstream
#include <vector>
#include <string>
#include "gnc-tokenizer.hpp"

class GncCsvTokenizer : public GncTokenizer
{
public:
    GncCsvTokenizer() = default;                                  // default constructor
    GncCsvTokenizer(const GncCsvTokenizer&) = default;            // copy constructor
    GncCsvTokenizer& operator=(const GncCsvTokenizer&) = default; // copy assignment
    GncCsvTokenizer(GncCsvTokenizer&&) = default;                 // move constructor
    GncCsvTokenizer& operator=(GncCsvTokenizer&&) = default;      // move assignment
    ~GncCsvTokenizer() = default;                                 // destructor

    void set_separators(const std::string& separators);
    int  tokenize() override;

private:
    std::string m_sep_str = ",";
};

#endif
