#include "html.h"
#include "document_container.h"

void litehtml::document_container::split_text(const char* text, const std::function<void(const char*)>& on_word, const std::function<void(const char*)>& on_space)
{
	std::wstring str;
	std::wstring str_in = (const wchar_t*)utf8_to_wchar(text);
	ucode_t c;
	for (size_t i = 0; i < str_in.length(); i++)
	{
		c = (ucode_t)str_in[i];
		if (c <= ' ' && (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'))
		{
			if (!str.empty())
			{
				on_word(wchar_to_utf8(str.c_str()));
				str.clear();
			}
			str += c;
			on_space(wchar_to_utf8(str.c_str()));
			str.clear();
		}
		// CJK character range
		else if (c >= 0x4E00 && c <= 0x9FCC)
		{
			if (!str.empty())
			{
				on_word(wchar_to_utf8(str.c_str()));
				str.clear();
			}
			str += c;
			on_word(wchar_to_utf8(str.c_str()));
			str.clear();
		}
		else
		{
			str += c;
		}
	}
	if (!str.empty())
	{
		on_word(wchar_to_utf8(str.c_str()));
	}
}
