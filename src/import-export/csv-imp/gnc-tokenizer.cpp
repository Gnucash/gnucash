#include "gnc-tokenizer.hpp"
#include "gnc-csv-tokenizer.hpp"
#include "gnc-fw-tokenizer.hpp"

#include <iostream>
#include <fstream>      // fstream
#include <vector>
#include <string>
#include <algorithm>    // copy
#include <iterator>     // ostream_operator
#include <memory>

#include <boost/locale.hpp>

extern "C" {
#include <goffice/go-glib-extras.h>
}

std::unique_ptr<GncTokenizer> GncTokenizerFactory(GncImpFileFormat fmt)
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
        break;
    }

    return tok;
}

void
GncTokenizer::load_file(const std::string& path)
{
    if (path.empty())
        return;

    imp_file_str = path;

    std::ifstream in;
    in.exceptions ( std::ifstream::failbit | std::ifstream::badbit );
    in.open (imp_file_str.c_str(), std::ios::in | std::ios::binary);

    raw_contents.clear();
    in.seekg(0, std::ios::end);
    raw_contents.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&raw_contents[0], raw_contents.size());
    in.close();

    // Guess encoding, user can override if needed later on.
    const char *guessed_enc = NULL;
    guessed_enc = go_guess_encoding (raw_contents.c_str(),
                                     raw_contents.length(),
                                     enc_str.empty() ? "UTF-8" : enc_str.c_str(),
                                     NULL);
    if (guessed_enc)
        this->encoding(guessed_enc);
    else
        enc_str.clear();

}

std::string
GncTokenizer::current_file()
{
    return imp_file_str;
}

void
GncTokenizer::encoding(const std::string& encoding)
{
    enc_str = encoding;
    utf8_contents = boost::locale::conv::to_utf<char>(raw_contents, enc_str);
}

std::string
GncTokenizer::encoding()
{
    return enc_str;
}


int GncTokenizer::tokenize()
{
	return 0;
}


std::vector<str_vec> GncTokenizer::get_tokens()
{
    return tokenized_contents;
}
