#include "gnc-fw-tokenizer.hpp"

#include <iostream>
#include <fstream>      // fstream
#include <vector>
#include <string>
#include <algorithm>    // copy
#include <iterator>     // ostream_operator

#include <boost/tokenizer.hpp>
#include <boost/locale.hpp>
#include <boost/algorithm/string.hpp>

void
GncFwTokenizer::columns(const std::vector<uint>& cols)
{
    m_col_vec = cols;
}


bool GncFwTokenizer::col_can_delete (uint col_num)
{
    auto last_col = m_col_vec.size() - 1;
    if (col_num < 0 || col_num >= last_col)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_delete (uint col_num)
{
    if (!col_can_delete (col_num))
        return;

    m_col_vec[col_num + 1] += m_col_vec[col_num];
    m_col_vec.erase (m_col_vec.begin() + col_num);
}

bool GncFwTokenizer::col_can_narrow (uint col_num)
{
    // Can't narrow the last column, it always sticks to the end of the parseable data
    auto last_col = m_col_vec.size() - 1;
    if (col_num >= last_col)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_narrow (uint col_num)
{
    if (!col_can_narrow (col_num))
        return;

    m_col_vec[col_num]--;
    m_col_vec[col_num + 1]++;

    // Drop column if it has become 0-width now
    if (m_col_vec[col_num] == 0)
        m_col_vec.erase (m_col_vec.begin() + col_num);
}

bool GncFwTokenizer::col_can_widen (uint col_num)
{
    // Can't widen the last column, it always sticks to the end of the parseable data
    auto last_col = m_col_vec.size() - 1;
    if (col_num >= last_col)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_widen (uint col_num)
{
    if (!col_can_widen (col_num))
        return;

    m_col_vec[col_num]++;
    m_col_vec[col_num + 1]--;

    // Drop next column if it has become 0-width now
    if (m_col_vec[col_num + 1] == 0)
        m_col_vec.erase (m_col_vec.begin() + col_num + 1);
}

bool GncFwTokenizer::col_can_split (uint col_num, uint position)
{
    auto last_col = m_col_vec.size() - 1;
    if (col_num > last_col)
        return false;

    uint col_end = m_col_vec[col_num];
    if (position < 1 || position >= col_end)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_split (uint col_num, uint position)
{
    if (col_can_split (col_num, position))
    {
        m_col_vec.insert (m_col_vec.begin() + col_num, position);
        m_col_vec[col_num + 1] -= position;
    }
}


std::string GncFwTokenizer::cols_to_string()
{
    std::ostringstream colstream;
    for (auto col_end : m_col_vec)
        colstream<<col_end<<",";
    std::string colstr = colstream.str();
    if (!colstr.empty())
        colstr.pop_back(); // Drop last ","
    return colstr;
}

void GncFwTokenizer::cols_from_string(const std::string& col_str)
{
    // Clear existing columns first
    columns();

    /* Set an initial column as expected by the code below
     * If no data is read yet, take a rather large value
     * Otherwise take the size of the widest line in the data
     */
    if (m_longest_line != 0)
        m_col_vec.push_back (m_longest_line);
    else
        m_col_vec.push_back (99999);

    uint col = 0;
    std::istringstream in_stream(col_str);
    std::string col_end_str;
    while (std::getline (in_stream, col_end_str, ','))
    {
        if (col_end_str.empty())
            continue;  // Skip empty column positions

        uint col_width = std::stoi (col_end_str);
        col_split (col, col_width);
        col++;
    }

    // If no data is loaded yet, remove last, unreasonably wide column
    if (m_longest_line == 0)
        m_col_vec.pop_back();
}


void GncFwTokenizer::load_file(const std::string& path)
{
    GncTokenizer::load_file(path);

    std::string line;
    m_longest_line = 0;
    std::istringstream in_stream(m_utf8_contents);
    while (std::getline (in_stream, line))
    {
        if (line.size() > m_longest_line)
            m_longest_line = line.size();

        line.clear();
    }

    if (m_col_vec.empty())
        /* Set a sane default for the offsets
         * That is, assume one column with all the data */
        m_col_vec.push_back(m_longest_line);
    else
    {
        /* Adjust existing last column(s) so the total column width
         * equals the width of the longest line
         * This may mean
         * - widening the last column to widen to the longest line or
         * - deleting columns/narrowing the last one to reduce to the longest line
         */
        uint total_width = 0;
        for (auto col_width : m_col_vec)
            total_width += col_width;

        if (m_longest_line > total_width)
            m_col_vec.back() += m_longest_line - total_width;
        else if (m_longest_line < total_width)
        {
            while (total_width - m_col_vec.back() > m_longest_line)
                col_delete (m_col_vec[m_col_vec.size() - 2]);
            m_col_vec.back() -= total_width - m_longest_line;
        }
    }
}


int GncFwTokenizer::tokenize()
{
    typedef boost::tokenizer< boost::offset_separator > Tokenizer;

    boost::offset_separator sep(m_col_vec.begin(), m_col_vec.end(), false);

    std::vector<std::string> vec;
    std::string line;
    std::string buffer;

    m_tokenized_contents.clear();
    std::istringstream in_stream(m_utf8_contents);

    while (std::getline (in_stream, line))
    {
        Tokenizer tok(line, sep);
        vec.clear();
        for (auto token : tok)
            vec.push_back (boost::trim_copy(token));
        //vec.assign(tok.begin(),tok.end());

        // Trim all leading and trailing whitespace
        //for (auto token : vec)
        //    boost::trim(token);

        line.clear(); // clear here, next check could fail

        // example checking
        // for correctly parsed 3 fields per record
        // if (vec.size() < 3) continue;

        m_tokenized_contents.push_back(vec);
    }

    return 0;
}
