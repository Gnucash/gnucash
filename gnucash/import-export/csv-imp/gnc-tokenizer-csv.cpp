#include "gnc-tokenizer-csv.hpp"

#include <iostream>
#include <fstream>      // fstream
#include <vector>
#include <string>
#include <algorithm>    // copy
#include <iterator>     // ostream_operator

#include <boost/tokenizer.hpp>
#include <boost/locale.hpp>
#include <boost/algorithm/string.hpp>

extern "C" {
    #include <glib/gi18n.h>
}

void
GncCsvTokenizer::set_separators(const std::string& separators)
{
    m_sep_str = separators;
}


int GncCsvTokenizer::tokenize()
{
    using Tokenizer = boost::tokenizer< boost::escaped_list_separator<char>>;

    boost::escaped_list_separator<char> sep("\\", m_sep_str, "\"");

    StrVec vec;
    std::string line;
    std::string buffer;

    bool inside_quotes(false);
    size_t last_quote(0);

    m_tokenized_contents.clear();
    std::istringstream in_stream(m_utf8_contents);

    try
    {
        while (std::getline (in_stream, buffer))
        {
            // --- deal with line breaks in quoted strings
            buffer = boost::trim_copy (buffer); // Removes trailing newline and spaces
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
                line.append(" ");
                continue;
            }
            // ---

            // Deal with backslashes that are not meant to be escapes
            // The boost::tokenizer with escaped_list_separator as we use
            // it would choke on this.
            auto bs_pos = line.find ('\\');
            while (bs_pos != std::string::npos)
            {
                if ((bs_pos == line.size()) ||                                 // got trailing single backslash
                    (line.find_first_of ("\"\\n", bs_pos + 1) != bs_pos + 1))  // backslash is not part of known escapes \\, \" or \n
                    line = line.substr(0, bs_pos) + "\\\\" + line.substr(bs_pos + 1);
                bs_pos += 2;
                bs_pos = line.find ('\\', bs_pos);
            }

            // Deal with repeated " ("") in strings.
            // This is commonly used as escape mechanism for double quotes in csv files.
            // However boost just eats them.
            bs_pos = line.find ("\"\"");
            while (bs_pos != std::string::npos)
            {
                line.replace (bs_pos, 2, "\\\"");
                bs_pos = line.find ("\"\"");
            }

            Tokenizer tok(line, sep);
            vec.assign(tok.begin(),tok.end());
            m_tokenized_contents.push_back(vec);
            line.clear();
        }
    }
    catch (boost::escaped_list_error &e)
    {
        throw (std::range_error N_("There was an error parsing the file."));
    }

    return 0;
}
