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

extern "C" {
#include <go-glib-extras.h>
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

    std::ifstream in;
    in.exceptions ( std::ifstream::failbit | std::ifstream::badbit );
    in.open (m_imp_file_str.c_str(), std::ios::in | std::ios::binary);

    m_raw_contents.clear();
    in.seekg(0, std::ios::end);
    m_raw_contents.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&m_raw_contents[0], m_raw_contents.size());
    in.close();

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
