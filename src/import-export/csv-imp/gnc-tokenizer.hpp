/********************************************************************\
 * gnc-tokenizer.hpp - base class for converting a text file into a *
 *                     two-dimensional vector of strings (table)    *
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
     @brief Class convert a file into vector of string vectors.
     This is a generic base class that holds the functionality common
     to different specializations (eg a csv file parser, a fixed-width
     file parser,...)
     The child classes have to override the tokenize function to
     create a full tokenizer class.
     *
     gnc-tokenizer.hpp
     @author Copyright (c) 2015 Geert Janssens <geert@kobaltwit.be>
 */

#ifndef GNC_TOKENIZER_HPP
#define GNC_TOKENIZER_HPP

extern "C" {
#include "config.h"
}

#include <iostream>
#include <fstream>      // fstream
#include <vector>
#include <string>
#include <memory>

using str_vec = std::vector<std::string>;

/** Enumeration for file formats supported by this importer. */
enum class GncImpFileFormat {
    UNKNOWN,
};


class GncTokenizer
{
public:
    GncTokenizer() = default;                               // default constructor
    GncTokenizer(const GncTokenizer&) = default;            // copy constructor
    GncTokenizer& operator=(const GncTokenizer&) = default; // copy assignment
    GncTokenizer(GncTokenizer&&) = default;                 // move constructor
    GncTokenizer& operator=(GncTokenizer&&) = default;      // move assignment
    virtual ~GncTokenizer() = default;                      // destructor

    void load_file(const std::string& path);
    std::string current_file();
    void encoding(const std::string& encoding);
    std::string encoding();
    virtual int  tokenize();
    std::vector<str_vec> get_tokens();
    
protected:
    std::string utf8_contents;
    std::vector<str_vec> tokenized_contents;

private:
    std::string imp_file_str;
    std::string raw_contents;
    std::string enc_str;
};


// Function to instantiate specializations of the GncTokenizer
std::unique_ptr<GncTokenizer> GncTokenizerFactory(GncImpFileFormat fmt);

#endif
