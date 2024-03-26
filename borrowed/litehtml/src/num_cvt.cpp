#include "num_cvt.h"
#include "utf8_strings.h"
#include <vector>

static std::vector<char> latin_lower = { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' };
static std::vector<char> latin_upper = { 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' };
static std::vector<std::wstring> greek_lower = { L"α", L"β", L"γ", L"δ", L"ε", L"ζ", L"η", L"θ", L"ι", L"κ", L"λ", L"μ", L"ν", L"ξ", L"ο", L"π", L"ρ", L"σ", L"τ", L"υ", L"φ", L"χ", L"ψ", L"ω" };

static litehtml::string to_mapped_alpha(int num, const std::vector<char>& map)
{
	int dividend = num;
	litehtml::string out;
	int modulo;

	while (dividend > 0)
	{
		modulo = (dividend - 1) % map.size();
		out = map[modulo] + out;
		dividend = (int)((dividend - modulo) / map.size());
	}

	return out;
}

static litehtml::string to_mapped_alpha(int num, const std::vector<std::wstring>& map)
{
	int dividend = num;
	litehtml::string out;
	int modulo;

	while (dividend > 0)
	{
		modulo = (dividend - 1) % map.size();
		out = litehtml_from_wchar(map[modulo]).c_str() + out;
		dividend = (int)((dividend - modulo) / map.size());
	}

	return out;
}

litehtml::string litehtml::num_cvt::to_latin_lower(int val)
{
	return to_mapped_alpha(val, latin_lower);
}

litehtml::string litehtml::num_cvt::to_latin_upper(int val)
{
	return to_mapped_alpha(val, latin_upper);
}

litehtml::string litehtml::num_cvt::to_greek_lower(int val)
{
	return to_mapped_alpha(val, greek_lower);
}

litehtml::string litehtml::num_cvt::to_roman_lower(int value)
{
	struct romandata_t { int value; const char* numeral; };
	const struct romandata_t romandata[] =
	{
		{ 1000, "m" }, { 900, "cm" },
		{ 500, "d" }, { 400, "cd" },
		{ 100, "c" }, { 90, "xc" },
		{ 50, "l" }, { 40, "xl" },
		{ 10, "x" }, { 9, "ix" },
		{ 5, "v" }, { 4, "iv" },
		{ 1, "i" },
		{ 0, nullptr } // end marker
	};

	litehtml::string result;
	for (const romandata_t* current = romandata; current->value > 0; ++current)
	{
		while (value >= current->value)
		{
			result += current->numeral;
			value -= current->value;
		}
	}
	return result;
}

litehtml::string litehtml::num_cvt::to_roman_upper(int value)
{
	struct romandata_t { int value; const char* numeral; };
	const struct romandata_t romandata[] =
	{
		{ 1000, "M" }, { 900, "CM" },
		{ 500, "D" }, { 400, "CD" },
		{ 100, "C" }, { 90, "XC" },
		{ 50, "L" }, { 40, "XL" },
		{ 10, "X" }, { 9, "IX" },
		{ 5, "V" }, { 4, "IV" },
		{ 1, "I" },
		{ 0, nullptr } // end marker
	};

	litehtml::string result;
	for (const romandata_t* current = romandata; current->value > 0; ++current)
	{
		while (value >= current->value)
		{
			result += current->numeral;
			value -= current->value;
		}
	}
	return result;
}
