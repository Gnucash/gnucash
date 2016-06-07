#include "gnc-dummy-tokenizer.hpp"

#include <iostream>
#include <fstream>      // fstream
#include <vector>
#include <string>
#include <algorithm>    // copy
#include <iterator>     // ostream_operator

#include <boost/locale.hpp>


int GncDummyTokenizer::tokenize()
{
    std::vector<std::string> vec;
    std::string line;

    tokenized_contents.clear();
    std::istringstream in_stream(utf8_contents);

    while (std::getline (in_stream, line))
    {
        vec.push_back (line);
        tokenized_contents.push_back(vec);

        line.clear();
        vec.clear();
    }

    return 0;
}
