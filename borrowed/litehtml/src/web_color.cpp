#include "html.h"
#include "web_color.h"
#include <cstring>

const litehtml::web_color litehtml::web_color::transparent = web_color(0, 0, 0, 0);
const litehtml::web_color litehtml::web_color::black       = web_color(0, 0, 0, 255);
const litehtml::web_color litehtml::web_color::white       = web_color(255, 255, 255, 255);

litehtml::def_color litehtml::g_def_colors[] = 
{
	{"transparent","rgba(0, 0, 0, 0)"},
	{"AliceBlue","#F0F8FF"},
	{"AntiqueWhite","#FAEBD7"},
	{"Aqua","#00FFFF"},
	{"Aquamarine","#7FFFD4"},
	{"Azure","#F0FFFF"},
	{"Beige","#F5F5DC"},
	{"Bisque","#FFE4C4"},
	{"Black","#000000"},
	{"BlanchedAlmond","#FFEBCD"},
	{"Blue","#0000FF"},
	{"BlueViolet","#8A2BE2"},
	{"Brown","#A52A2A"},
	{"BurlyWood","#DEB887"},
	{"CadetBlue","#5F9EA0"},
	{"Chartreuse","#7FFF00"},
	{"Chocolate","#D2691E"},
	{"Coral","#FF7F50"},
	{"CornflowerBlue","#6495ED"},
	{"Cornsilk","#FFF8DC"},
	{"Crimson","#DC143C"},
	{"Cyan","#00FFFF"},
	{"DarkBlue","#00008B"},
	{"DarkCyan","#008B8B"},
	{"DarkGoldenRod","#B8860B"},
	{"DarkGray","#A9A9A9"},
	{"DarkGrey","#A9A9A9"},
	{"DarkGreen","#006400"},
	{"DarkKhaki","#BDB76B"},
	{"DarkMagenta","#8B008B"},
	{"DarkOliveGreen","#556B2F"},
	{"Darkorange","#FF8C00"},
	{"DarkOrchid","#9932CC"},
	{"DarkRed","#8B0000"},
	{"DarkSalmon","#E9967A"},
	{"DarkSeaGreen","#8FBC8F"},
	{"DarkSlateBlue","#483D8B"},
	{"DarkSlateGray","#2F4F4F"},
	{"DarkSlateGrey","#2F4F4F"},
	{"DarkTurquoise","#00CED1"},
	{"DarkViolet","#9400D3"},
	{"DeepPink","#FF1493"},
	{"DeepSkyBlue","#00BFFF"},
	{"DimGray","#696969"},
	{"DimGrey","#696969"},
	{"DodgerBlue","#1E90FF"},
	{"FireBrick","#B22222"},
	{"FloralWhite","#FFFAF0"},
	{"ForestGreen","#228B22"},
	{"Fuchsia","#FF00FF"},
	{"Gainsboro","#DCDCDC"},
	{"GhostWhite","#F8F8FF"},
	{"Gold","#FFD700"},
	{"GoldenRod","#DAA520"},
	{"Gray","#808080"},
	{"Grey","#808080"},
	{"Green","#008000"},
	{"GreenYellow","#ADFF2F"},
	{"HoneyDew","#F0FFF0"},
	{"HotPink","#FF69B4"},
	{"Ivory","#FFFFF0"},
	{"Khaki","#F0E68C"},
	{"Lavender","#E6E6FA"},
	{"LavenderBlush","#FFF0F5"},
	{"LawnGreen","#7CFC00"},
	{"LemonChiffon","#FFFACD"},
	{"LightBlue","#ADD8E6"},
	{"LightCoral","#F08080"},
	{"LightCyan","#E0FFFF"},
	{"LightGoldenRodYellow","#FAFAD2"},
	{"LightGray","#D3D3D3"},
	{"LightGrey","#D3D3D3"},
	{"LightGreen","#90EE90"},
	{"LightPink","#FFB6C1"},
	{"LightSalmon","#FFA07A"},
	{"LightSeaGreen","#20B2AA"},
	{"LightSkyBlue","#87CEFA"},
	{"LightSlateGray","#778899"},
	{"LightSlateGrey","#778899"},
	{"LightSteelBlue","#B0C4DE"},
	{"LightYellow","#FFFFE0"},
	{"Lime","#00FF00"},
	{"LimeGreen","#32CD32"},
	{"Linen","#FAF0E6"},
	{"Magenta","#FF00FF"},
	{"Maroon","#800000"},
	{"MediumAquaMarine","#66CDAA"},
	{"MediumBlue","#0000CD"},
	{"MediumOrchid","#BA55D3"},
	{"MediumPurple","#9370D8"},
	{"MediumSeaGreen","#3CB371"},
	{"MediumSlateBlue","#7B68EE"},
	{"MediumSpringGreen","#00FA9A"},
	{"MediumTurquoise","#48D1CC"},
	{"MediumVioletRed","#C71585"},
	{"MidnightBlue","#191970"},
	{"MintCream","#F5FFFA"},
	{"MistyRose","#FFE4E1"},
	{"Moccasin","#FFE4B5"},
	{"NavajoWhite","#FFDEAD"},
	{"Navy","#000080"},
	{"OldLace","#FDF5E6"},
	{"Olive","#808000"},
	{"OliveDrab","#6B8E23"},
	{"Orange","#FFA500"},
	{"OrangeRed","#FF4500"},
	{"Orchid","#DA70D6"},
	{"PaleGoldenRod","#EEE8AA"},
	{"PaleGreen","#98FB98"},
	{"PaleTurquoise","#AFEEEE"},
	{"PaleVioletRed","#D87093"},
	{"PapayaWhip","#FFEFD5"},
	{"PeachPuff","#FFDAB9"},
	{"Peru","#CD853F"},
	{"Pink","#FFC0CB"},
	{"Plum","#DDA0DD"},
	{"PowderBlue","#B0E0E6"},
	{"Purple","#800080"},
	{"Red","#FF0000"},
	{"RosyBrown","#BC8F8F"},
	{"RoyalBlue","#4169E1"},
	{"SaddleBrown","#8B4513"},
	{"Salmon","#FA8072"},
	{"SandyBrown","#F4A460"},
	{"SeaGreen","#2E8B57"},
	{"SeaShell","#FFF5EE"},
	{"Sienna","#A0522D"},
	{"Silver","#C0C0C0"},
	{"SkyBlue","#87CEEB"},
	{"SlateBlue","#6A5ACD"},
	{"SlateGray","#708090"},
	{"SlateGrey","#708090"},
	{"Snow","#FFFAFA"},
	{"SpringGreen","#00FF7F"},
	{"SteelBlue","#4682B4"},
	{"Tan","#D2B48C"},
	{"Teal","#008080"},
	{"Thistle","#D8BFD8"},
	{"Tomato","#FF6347"},
	{"Turquoise","#40E0D0"},
	{"Violet","#EE82EE"},
	{"Wheat","#F5DEB3"},
	{"White","#FFFFFF"},
	{"WhiteSmoke","#F5F5F5"},
	{"Yellow","#FFFF00"},
	{"YellowGreen","#9ACD32"},
	{nullptr,nullptr}
};


litehtml::web_color litehtml::web_color::from_string(const string& _str, document_container* callback)
{
	auto str = _str.c_str();
	if(!str[0])
	{
		return web_color(0, 0, 0);
	}
	if(str[0] == '#')
	{
		string red;
		string green;
		string blue;
		if(strlen(str + 1) == 3)
		{
			red		+= str[1];
			red		+= str[1];
			green	+= str[2];
			green	+= str[2];
			blue	+= str[3];
			blue	+= str[3];
		} else if(strlen(str + 1) == 6)
		{
			red		+= str[1];
			red		+= str[2];
			green	+= str[3];
			green	+= str[4];
			blue	+= str[5];
			blue	+= str[6];
		}
		char* sss = nullptr;
		web_color clr;
		clr.red		= (byte) strtol(red.c_str(),	&sss, 16);
		clr.green	= (byte) strtol(green.c_str(),	&sss, 16);
		clr.blue	= (byte) strtol(blue.c_str(),	&sss, 16);
		return clr;
	} else if(!strncmp(str, "rgb", 3))
	{
		string s = str;

		string::size_type pos = s.find_first_of('(');
		if(pos != string::npos)
		{
			s.erase(s.begin(), s.begin() + pos + 1);
		}
		pos = s.find_last_of(')');
		if(pos != string::npos)
		{
			s.erase(s.begin() + pos, s.end());
		}

		std::vector<string> tokens;
		split_string(s, tokens, ", \t");

		web_color clr;

		if(tokens.size() >= 1)	clr.red		= (byte) atoi(tokens[0].c_str());
		if(tokens.size() >= 2)	clr.green	= (byte) atoi(tokens[1].c_str());
		if(tokens.size() >= 3)	clr.blue	= (byte) atoi(tokens[2].c_str());
		if(tokens.size() >= 4)	clr.alpha	= (byte) (t_strtod(tokens[3].c_str(), nullptr) * 255.0);

		return clr;
	} else
	{
		string rgb = resolve_name(str, callback);
		if(!rgb.empty())
		{
			return from_string(rgb.c_str(), callback);
		}
	}
	return web_color(0, 0, 0);
}

litehtml::string litehtml::web_color::resolve_name(const string& name, document_container* callback)
{
	for(int i=0; g_def_colors[i].name; i++)
	{
		if(!t_strcasecmp(name.c_str(), g_def_colors[i].name))
		{
            return g_def_colors[i].rgb;
		}
	}
    if (callback)
    {
        string clr = callback->resolve_color(name);
        return clr;
    }
	return "";
}

bool litehtml::web_color::is_color(const string& str, document_container* callback)
{
	if (!t_strncasecmp(str.c_str(), "rgb", 3) || str[0] == '#')
	{
		return true;
	}
    if (t_isalpha(str[0]) && resolve_name(str, callback) != "")
	{
		return true;
	}
	return false;
}

litehtml::string litehtml::web_color::to_string() const
{
    char str[9];
    if(alpha)
    {
		t_snprintf(str, 9, "%02X%02X%02X%02X", red, green, blue, alpha);
    } else
    {
		t_snprintf(str, 9, "%02X%02X%02X", red, green, blue);
    }
    return str;
}
