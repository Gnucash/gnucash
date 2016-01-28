#include "gnc-fw-tokenizer.hpp"

#include <iostream>
#include <fstream>      // fstream
#include <vector>
#include <string>
#include <algorithm>    // copy
#include <iterator>     // ostream_operator

#include <boost/tokenizer.hpp>
#include <boost/locale.hpp>

void
GncFwTokenizer::columns(const std::vector<uint>& cols)
{
    col_vec = cols;
}


int GncFwTokenizer::tokenize()
{
    typedef boost::tokenizer< boost::offset_separator > Tokenizer;

    boost::offset_separator sep(col_vec.begin(), col_vec.end(), false);

    std::vector<std::string> vec;
    std::string line;
    std::string buffer;

    tokenized_contents.clear();
    std::istringstream in_stream(utf8_contents);

    while (std::getline (in_stream, line))
    {
        Tokenizer tok(line, sep);
        vec.assign(tok.begin(),tok.end());

        line.clear(); // clear here, next check could fail

        // example checking
        // for correctly parsed 3 fields per record
        if (vec.size() < 3) continue;

        tokenized_contents.push_back(vec);
    }

    return 0;
}
