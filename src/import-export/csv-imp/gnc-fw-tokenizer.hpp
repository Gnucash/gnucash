/********************************************************************\
 * gnc-fw-tokenizer.hpp - takes a file and converts it into a       *
 *                        two-dimensional vector of strings (table) *
 *                        splitting the contents on fixed width     *
 *                        positions                                 *
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
     @brief Class convert a file with fixed with delimited contents
     into vector of string vectors.
     One can define the widths of each column to use to split each line
     into multiple fields.
     However, no gnucash specific interpretation is done yet, that's up
     to the code using this class.
     *
     gnc-fw-tokenizer.hpp
     @author Copyright (c) 2015 Geert Janssens <geert@kobaltwit.be>
 */

#ifndef GNC_FW_TOKENIZER_HPP
#define GNC_FW_TOKENIZER_HPP

extern "C" {
#include "config.h"
}

#include <iostream>
#include <fstream>      // fstream
#include <vector>
#include <string>
#include "gnc-tokenizer.hpp"

class GncFwTokenizer : public GncTokenizer
{
public:
    GncFwTokenizer() = default;                                 // default constructor
    GncFwTokenizer(const GncFwTokenizer&) = default;            // copy constructor
    GncFwTokenizer& operator=(const GncFwTokenizer&) = default; // copy assignment
    GncFwTokenizer(GncFwTokenizer&&) = default;                 // move constructor
    GncFwTokenizer& operator=(GncFwTokenizer&&) = default;      // move assignment
    ~GncFwTokenizer() = default;                                // destructor

    void columns(const std::vector<uint>& cols = std::vector<uint>());
    std::vector<uint> get_columns();
    uint get_column (uint num);

    // Column manipulators
    bool col_can_delete (uint col_num);
    void col_delete (uint col_num);
    bool col_can_narrow (uint col_num);
    void col_narrow (uint col_num);
    bool col_can_widen (uint col_num);
    void col_widen (uint col_num);
    bool col_can_split (uint col_num, uint position);
    void col_split (uint col_num, uint position);

    void load_file (const std::string& path) override;
    int  tokenize() override;


private:
    std::vector<uint> m_col_vec;
    uint m_longest_line;
};

#endif
