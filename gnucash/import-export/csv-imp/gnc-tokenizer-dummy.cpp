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
