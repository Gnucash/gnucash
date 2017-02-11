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

std::vector<uint>
GncFwTokenizer::get_columns()
{
    return m_col_vec;
}


bool GncFwTokenizer::col_can_delete (uint col_num)
{
    auto last_col = m_col_vec.size() - 1;
    if (col_num >= last_col)
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
    using Tokenizer = boost::tokenizer< boost::offset_separator > ;

    boost::offset_separator sep(m_col_vec.begin(), m_col_vec.end(), false);

    StrVec vec;
    std::string line;
    std::string buffer;

    m_tokenized_contents.clear();
    std::istringstream in_stream(m_utf8_contents);

    while (std::getline (in_stream, line))
    {
        Tokenizer tok(line, sep);
        vec.clear();
        for (auto token : tok)
            vec.push_back (boost::trim_copy(token)); // strips newlines as well as whitespace
        m_tokenized_contents.push_back(vec);
        line.clear(); // clear here, next check could fail
    }

    return 0;
}
