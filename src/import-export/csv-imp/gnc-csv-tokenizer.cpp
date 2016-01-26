#include "gnc-csv-tokenizer.hpp"

#include <iostream>
#include <fstream>      // fstream
#include <vector>
#include <string>
#include <algorithm>    // copy
#include <iterator>     // ostream_operator

#include <boost/tokenizer.hpp>
#include <boost/locale.hpp>

void
GncCsvTokenizer::set_separators(const std::string& separators)
{
    sep_str = separators;
}


int GncCsvTokenizer::tokenize()
{
    typedef boost::tokenizer< boost::escaped_list_separator<char> > Tokenizer;

    boost::escaped_list_separator<char> sep("\\", sep_str, "\"");

    std::vector<std::string> vec;
    std::string line;
    std::string buffer;

    bool inside_quotes(false);
    size_t last_quote(0);

    tokenized_contents.clear();
    std::istringstream in_stream(utf8_contents);

    while (std::getline (in_stream, buffer))
    {
        // --- deal with line breaks in quoted strings
        last_quote = buffer.find_first_of('"');
        while (last_quote != std::string::npos)
        {
            if (last_quote == 0) // Test separately because last_quote - 1 would be out of range
                inside_quotes = !inside_quotes;
            else if (buffer[ last_quote - 1 ] != '\\')
                inside_quotes = !inside_quotes;

            last_quote = buffer.find_first_of('"',last_quote+1);
        }

        line.append(buffer);
        if (inside_quotes)
        {
            line.append("\n");
            continue;
        }
        // ---

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
