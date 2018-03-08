#include "gnc-tokenizer-fw.hpp"

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
GncFwTokenizer::columns(const std::vector<uint32_t>& cols)
{
    m_col_vec = cols;
}

std::vector<uint32_t>
GncFwTokenizer::get_columns()
{
    return m_col_vec;
}


bool GncFwTokenizer::col_can_delete (uint32_t col_num)
{
    auto last_col = m_col_vec.size() - 1;
    if (col_num >= last_col)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_delete (uint32_t col_num)
{
    if (!col_can_delete (col_num))
        return;

    m_col_vec[col_num + 1] += m_col_vec[col_num];
    m_col_vec.erase (m_col_vec.begin() + col_num);
}

bool GncFwTokenizer::col_can_narrow (uint32_t col_num)
{
    // Can't narrow the last column, it always sticks to the end of the parseable data
    auto last_col = m_col_vec.size() - 1;
    if (col_num >= last_col)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_narrow (uint32_t col_num)
{
    if (!col_can_narrow (col_num))
        return;

    m_col_vec[col_num]--;
    m_col_vec[col_num + 1]++;

    // Drop column if it has become 0-width now
    if (m_col_vec[col_num] == 0)
        m_col_vec.erase (m_col_vec.begin() + col_num);
}

bool GncFwTokenizer::col_can_widen (uint32_t col_num)
{
    // Can't widen the last column, it always sticks to the end of the parseable data
    auto last_col = m_col_vec.size() - 1;
    if (col_num >= last_col)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_widen (uint32_t col_num)
{
    if (!col_can_widen (col_num))
        return;

    m_col_vec[col_num]++;
    m_col_vec[col_num + 1]--;

    // Drop next column if it has become 0-width now
    if (m_col_vec[col_num + 1] == 0)
        m_col_vec.erase (m_col_vec.begin() + col_num + 1);
}

bool GncFwTokenizer::col_can_split (uint32_t col_num, uint32_t position)
{
    auto last_col = m_col_vec.size() - 1;
    if (col_num > last_col)
        return false;

    uint32_t col_end = m_col_vec[col_num];
    if (position < 1 || position >= col_end)
        return false;
    else
        return true;
}

void GncFwTokenizer::col_split (uint32_t col_num, uint32_t position)
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
        uint32_t total_width = 0;
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

/* Fixed width tokenizer uses wide characters internally instead of
 * narrow (possibly multi-byte) characters. With multi-byte characters
 * the character offsets are incorrectly interpreted as byte offsets and
 * multi-byte characters (like the € sign in utf-8) could be inadvertently
 * split. This doesn't happen with wide characters.
 */
int GncFwTokenizer::tokenize()
{
    using boost::locale::conv::utf_to_utf;
    using Tokenizer = boost::tokenizer< boost::offset_separator,
            std::wstring::const_iterator, std::wstring > ;

    boost::offset_separator sep(m_col_vec.begin(), m_col_vec.end(), false);

    std::wstring wchar_contents = utf_to_utf<wchar_t>(m_utf8_contents.c_str(),
        m_utf8_contents.c_str() + m_utf8_contents.size());

    StrVec vec;
    std::wstring line;

    m_tokenized_contents.clear();
    std::wistringstream in_stream(wchar_contents);

    while (std::getline (in_stream, line))
    {
        Tokenizer tok(line, sep);
        vec.clear();
        for (auto token : tok)
        {
            auto stripped = boost::trim_copy(token); // strips newlines as well as whitespace
            auto narrow = utf_to_utf<char>(stripped.c_str(), stripped.c_str()
                + stripped.size());
            vec.push_back (narrow);
        }
        m_tokenized_contents.push_back(vec);
        line.clear(); // clear here, next check could fail
    }

    return 0;
}
