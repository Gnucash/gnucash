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
    col_vec = cols;
}


bool GncFwTokenizer::col_can_add (uint col_end)
{
    if (col_end < 0 || col_end > longest_line)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_add (uint col_end)
{
    if (col_can_add (col_end))
    {
        for (auto col_it = col_vec.begin(); col_it != col_vec.end(); col_it++)
        {
            if (*col_it == col_end)
                return; // don't add same column end twice in the column list
            if (*col_it > col_end)
                col_vec.insert (col_it, col_end);
        }

        // If we got here that means the requested col_end is beyond the currently
        // inserted columns, so append it
        col_vec.push_back (col_end);
    }
}

bool GncFwTokenizer::col_can_delete (uint col_num)
{
    auto last_col = col_vec.size() - 1;
    if (col_num < 0 || col_num > last_col)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_delete (uint col_num)
{
    if (col_can_delete (col_num))
        col_vec.erase (col_vec.begin() + col_num);
}

bool GncFwTokenizer::col_can_narrow (uint col_num)
{
    auto last_col = col_vec.size() - 1;
    int col_start, next_col_start;

    if (col_num > last_col)
        return false;

    col_start = (col_num == 0) ? 0 : col_vec[col_num - 1];
    next_col_start = col_vec[col_num];

    if (next_col_start - 1 <= col_start)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_narrow (uint col_num)
{
    if (col_can_narrow (col_num))
        col_vec[col_num]--;
}

bool GncFwTokenizer::col_can_widen (uint col_num)
{
    auto last_col = col_vec.size() - 1;
    int col_end, next_col_end;

    if (col_num > last_col)
        return false;

    col_end = col_vec[col_num];
    next_col_end = (col_num == last_col - 1)
                    ? longest_line
                    : col_vec[col_num + 1];

    if (col_end + 1 >= next_col_end)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_widen (uint col_num)
{
    if (col_can_widen (col_num))
        col_vec[col_num]++;
}

bool GncFwTokenizer::col_can_split (uint col_num, uint position)
{
    auto last_col = col_vec.size() - 1;
    if (col_num > last_col)
        return false;

    uint col_start = (col_num == 0) ? 0 : col_vec[col_num - 1];
    uint col_end = col_vec[col_num];
    if (position <= col_start || position >= col_end)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_split (uint col_num, uint position)
{
    if (col_can_split (col_num, position))
    {
        uint col_start = (col_num == 0) ? 0 : col_vec[col_num - 1];;
        col_vec.insert (col_vec.begin() + col_num, col_start + position);
    }
}


std::string GncFwTokenizer::cols_to_string()
{
    std::ostringstream colstream;
    for (auto col_end : col_vec)
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
    longest_line = 0;
    std::istringstream in_stream(utf8_contents);
    while (std::getline (in_stream, line))
    {
        if (line.size() > longest_line)
            longest_line = line.size();

        line.clear();
    }
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

        tokenized_contents.push_back(vec);
    }

    return 0;
}
