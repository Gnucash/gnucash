/********************************************************************\
 * gnc-tokenizer-dummy.cpp - takes a file and converts it into a    *
 *                        two-dimensional vector of strings (table) *
 *                        each row will only have one single column *
 *                                                                  *
 * Copyright (C) 2016 Geert Janssens <geert@kobaltwit.be>           *
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

#include "gnc-tokenizer-dummy.hpp"

#include <iostream>
#include <fstream>      // fstream
#include <vector>
#include <string>
#include <algorithm>    // copy
#include <iterator>     // ostream_operator

#include <boost/locale.hpp>


int GncDummyTokenizer::tokenize()
{
    StrVec vec;
    std::string line;

    m_tokenized_contents.clear();
    std::istringstream in_stream(m_utf8_contents);

    while (std::getline (in_stream, line))
    {
        vec.push_back (line);
        m_tokenized_contents.push_back(vec);

        line.clear();
        vec.clear();
    }

    return 0;
}
