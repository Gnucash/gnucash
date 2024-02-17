/********************************************************************\
 * gnc-tokenizer.cpp - base class for converting a text file into a *
 *                     two-dimensional vector of strings (table)    *
 *                                                                  *
 * Copyright (C) 2015 Geert Janssens <geert@kobaltwit.be>           *
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

#include "gnc-tokenizer.hpp"
#include "gnc-tokenizer-csv.hpp"
#include "gnc-tokenizer-dummy.hpp"
#include "gnc-tokenizer-fw.hpp"

#include <iostream>
#include <fstream>      // fstream
#include <vector>
#include <string>
#include <algorithm>    // copy
#include <iterator>     // ostream_operator
#include <memory>

#include <boost/locale.hpp>
#include <boost/algorithm/string.hpp>

#include <go-glib-extras.h>
#include <glib.h>
#include <glib/gstdio.h>

std::unique_ptr<GncTokenizer> gnc_tokenizer_factory(GncImpFileFormat fmt)
{
    std::unique_ptr<GncTokenizer> tok(nullptr);
    switch (fmt)
    {
    case GncImpFileFormat::CSV:
        tok.reset(new GncCsvTokenizer());
        break;
    case GncImpFileFormat::FIXED_WIDTH:
        tok.reset(new GncFwTokenizer());
        break;
    default:
        tok.reset(new GncDummyTokenizer());
        break;
    }

    return tok;
}

void
GncTokenizer::load_file(const std::string& path)
{
    if (path.empty())
        return;

    m_imp_file_str = path;
    char *raw_contents;
    size_t raw_length;
    GError *error = nullptr;

    if (!g_file_get_contents(path.c_str(), &raw_contents, &raw_length, &error))
    {
        std::string msg {error->message};
        g_error_free (error);
        throw std::ifstream::failure {msg};
    }

    m_raw_contents = raw_contents;
    g_free(raw_contents);
    
    // Guess encoding, user can override if needed later on.
    const char *guessed_enc = NULL;
    guessed_enc = go_guess_encoding (m_raw_contents.c_str(),
                                     m_raw_contents.length(),
                                     m_enc_str.empty() ? "UTF-8" : m_enc_str.c_str(),
                                     NULL);
    if (guessed_enc)
        this->encoding(guessed_enc);
    else
        m_enc_str.clear();

}

const std::string&
GncTokenizer::current_file()
{
    return m_imp_file_str;
}

void
GncTokenizer::encoding(const std::string& encoding)
{
    m_enc_str = encoding;
    m_utf8_contents = boost::locale::conv::to_utf<char>(m_raw_contents, m_enc_str);

    // While we are converting here, let's also normalize line-endings to "\n"
    // That's what STL expects by default
    boost::replace_all (m_utf8_contents, "\r\n", "\n");
    boost::replace_all (m_utf8_contents, "\r", "\n");
}

const std::string&
GncTokenizer::encoding()
{
    return m_enc_str;
}


const std::vector<StrVec>&
GncTokenizer::get_tokens()
{
    return m_tokenized_contents;
}
