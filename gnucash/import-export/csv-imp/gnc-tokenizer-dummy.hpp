/********************************************************************\
 * gnc-tokenizer-dummy.hpp - takes a file and converts it into a    *
 *                        two-dimensional vector of strings (table) *
 *                        each row will only have one single column *
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
     @brief Dummy converter class to convert a file
     into vector of string vectors. Each string vector has only one element,
     the contents of one line of the file.
     This is just a dummy that can be used as long as the file format isn't
     specified yet by the user.
     *
     gnc-tokenizer-dummy.hpp
     @author Copyright (c) 2016 Geert Janssens <geert@kobaltwit.be>
 */

#ifndef GNC_DUMMY_TOKENIZER_HPP
#define GNC_DUMMY_TOKENIZER_HPP

extern "C" {
#include <config.h>
}

#include <iostream>
#include <fstream>      // fstream
#include <vector>
#include <string>
#include "gnc-tokenizer.hpp"

class GncDummyTokenizer : public GncTokenizer
{
public:
    GncDummyTokenizer() = default;                                 // default constructor
    GncDummyTokenizer(const GncDummyTokenizer&) = default;            // copy constructor
    GncDummyTokenizer& operator=(const GncDummyTokenizer&) = default; // copy assignment
    GncDummyTokenizer(GncDummyTokenizer&&) = default;                 // move constructor
    GncDummyTokenizer& operator=(GncDummyTokenizer&&) = default;      // move assignment
    ~GncDummyTokenizer() = default;                                // destructor

    int  tokenize() override;
};

#endif
