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

extern "C" {
#include <go-glib-extras.h>
#include <glib.h>
#include <glib/gstdio.h>
}

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
      throw std::ifstream::failure(error->message);

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
