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


bool GncFwTokenizer::col_can_add (uint col_end)
{
    if (col_end < 0 || col_end > m_longest_line)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_add (uint col_end)
{
    if (col_can_add (col_end))
    {
        for (auto col_it = m_col_vec.begin(); col_it != m_col_vec.end(); col_it++)
        {
            if (*col_it == col_end)
                return; // don't add same column end twice in the column list
            if (*col_it > col_end)
                m_col_vec.insert (col_it, col_end);
        }

        // If we got here that means the requested col_end is beyond the currently
        // inserted columns, so append it
        m_col_vec.push_back (col_end);
    }
}

bool GncFwTokenizer::col_can_delete (uint col_num)
{
    auto last_col = m_col_vec.size() - 1;
    if (col_num < 0 || col_num > last_col)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_delete (uint col_num)
{
    if (col_can_delete (col_num))
        m_col_vec.erase (m_col_vec.begin() + col_num);
}

bool GncFwTokenizer::col_can_narrow (uint col_num)
{
    auto last_col = m_col_vec.size() - 1;
    int col_start, next_col_start;

    if (col_num > last_col)
        return false;

    col_start = (col_num == 0) ? 0 : m_col_vec[col_num - 1];
    next_col_start = m_col_vec[col_num];

    if (next_col_start - 1 <= col_start)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_narrow (uint col_num)
{
    if (col_can_narrow (col_num))
        m_col_vec[col_num]--;
}

bool GncFwTokenizer::col_can_widen (uint col_num)
{
    auto last_col = m_col_vec.size() - 1;
    int col_end, next_col_end;

    if (col_num > last_col)
        return false;

    col_end = m_col_vec[col_num];
    next_col_end = (col_num == last_col - 1)
                    ? m_longest_line
                    : m_col_vec[col_num + 1];

    if (col_end + 1 >= next_col_end)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_widen (uint col_num)
{
    if (col_can_widen (col_num))
        m_col_vec[col_num]++;
}

bool GncFwTokenizer::col_can_split (uint col_num, uint position)
{
    auto last_col = m_col_vec.size() - 1;
    if (col_num > last_col)
        return false;

    uint col_start = (col_num == 0) ? 0 : m_col_vec[col_num - 1];
    uint col_end = m_col_vec[col_num];
    if (position <= col_start || position >= col_end)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_split (uint col_num, uint position)
{
    if (col_can_split (col_num, position))
    {
        uint col_start = (col_num == 0) ? 0 : m_col_vec[col_num - 1];;
        m_col_vec.insert (m_col_vec.begin() + col_num, col_start + position);
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

    std::istringstream in_stream(col_str);
    std::string col_end_str;
    while (std::getline (in_stream, col_end_str, ','))
    {
        if (col_end_str.empty())
            continue;  // Skip empty column positions

        uint charindex = std::stoi (col_end_str);
        col_add (charindex);
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

    /* Set a sane default for the offsets
     * That is, assume one column with all the data */
    if (m_col_vec.empty())
        m_col_vec.push_back(m_longest_line);
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
