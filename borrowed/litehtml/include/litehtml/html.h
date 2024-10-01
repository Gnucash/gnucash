#ifndef LH_HTML_H
#define LH_HTML_H

#include <stdlib.h>
#include <string>
#include <ctype.h>
#include <vector>
#include <map>
#include <cstring>
#include <algorithm>
#include <functional>
#include "os_types.h"
#include "string_id.h"
#include "types.h"
#include "utf8_strings.h"
#include "background.h"
#include "borders.h"
#include "web_color.h"
#include "media_query.h"
#include "html_tag.h"
#include "document_container.h"
#include "document.h"

namespace litehtml
{
	void trim(string &s, const string& chars_to_trim = " \n\r\t");
	void lcase(string &s);
	int	 value_index(const string& val, const string& strings, int defValue = -1, char delim = ';');
    string index_value(int index, const string& strings, char delim = ';');
	bool value_in_list(const string& val, const string& strings, char delim = ';');
	string::size_type find_close_bracket(const string &s, string::size_type off, char open_b = '(', char close_b = ')');
	void split_string(const string& str, string_vector& tokens, const string& delims, const string& delims_preserve = "", const string& quote = "\"");
	void join_string(string& str, const string_vector& tokens, const string& delims);
    double t_strtod(const char* string, char** endPtr = nullptr);
    string get_escaped_string(const string& in_str);

	int t_strcasecmp(const char *s1, const char *s2);
	int t_strncasecmp(const char *s1, const char *s2, size_t n);

	bool is_number(const string& string, const bool allow_dot = 1);

	inline int t_isdigit(int c)
	{
		return (c >= '0' && c <= '9');
	}

	inline int t_isalpha(int c)
	{
		return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
	}

	inline int t_tolower(int c)
	{
		return (c >= 'A' && c <= 'Z' ? c + 'a' - 'A' : c);
	}
	
	inline int round_f(float val)
	{
		int int_val = (int) val;
		if(val - int_val >= 0.5)
		{
			int_val++;
		}
		return int_val;
	}

	inline int round_d(double val)
	{
		int int_val = (int) val;
		if(val - int_val >= 0.5)
		{
			int_val++;
		}
		return int_val;
	}

	inline float t_strtof(const string& str, char** endPtr = nullptr)
	{
		return (float)t_strtod(str.c_str(), endPtr);
	}

	inline int baseline_align(int line_height, int line_base_line, int height, int baseline)
	{
		return (line_height - line_base_line) - (height - baseline);
	}
}

#endif  // LH_HTML_H
