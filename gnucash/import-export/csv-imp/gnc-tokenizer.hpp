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
#include <config.h>
}

#include <iostream>
#include <fstream>      // fstream
#include <vector>
#include <string>
#include <memory>

using StrVec = std::vector<std::string>;

/** Enumeration for file formats supported by this importer. */
enum class GncImpFileFormat {
    UNKNOWN,
    CSV,
    FIXED_WIDTH
};

class GncTokenizerTest;

class GncTokenizer
{
friend GncTokenizerTest;
public:
    GncTokenizer() = default;                               // default constructor
    GncTokenizer(const GncTokenizer&) = default;            // copy constructor
    GncTokenizer& operator=(const GncTokenizer&) = default; // copy assignment
    GncTokenizer(GncTokenizer&&) = default;                 // move constructor
    GncTokenizer& operator=(GncTokenizer&&) = default;      // move assignment
    virtual ~GncTokenizer() = default;                      // destructor

    virtual void load_file(const std::string& path);
    const std::string& current_file();
    void encoding(const std::string& encoding);
    const std::string& encoding();
    virtual int  tokenize() = 0;
    const std::vector<StrVec>& get_tokens();

protected:
    std::string m_utf8_contents;
    std::vector<StrVec> m_tokenized_contents;

private:
    std::string m_imp_file_str;
    std::string m_raw_contents;
    std::string m_enc_str;
};


// Function to instantiate specializations of the GncTokenizer
std::unique_ptr<GncTokenizer> gnc_tokenizer_factory(GncImpFileFormat fmt);

#endif
