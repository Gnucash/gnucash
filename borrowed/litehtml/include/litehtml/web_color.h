#ifndef LH_WEB_COLOR_H
#define LH_WEB_COLOR_H

namespace litehtml
{
	struct def_color
	{
		const char*	name;
		const char*	rgb;
	};

	extern def_color g_def_colors[];

	class document_container;

	struct web_color
	{
		byte	red;
		byte	green;
		byte	blue;
		byte	alpha;

		static const web_color transparent;
		static const web_color black;
		static const web_color white;

		web_color(byte r, byte g, byte b, byte a = 255)
		{
			red		= r;
			green	= g;
			blue	= b;
			alpha	= a;
		}

		web_color()
		{
			red		= 0;
			green	= 0;
			blue	= 0;
			alpha	= 0xFF;
		}

		bool operator==(web_color color) const { return red == color.red && green == color.green && blue == color.blue && alpha == color.alpha; }
		bool operator!=(web_color color) const { return !(*this == color); }

		string to_string() const;
		static web_color	from_string(const string& str, document_container* callback);
		static string		resolve_name(const string& name, document_container* callback);
		static bool			is_color(const string& str, document_container* callback);
		web_color darken(double fraction) const
		{
			int v_red = (int) red;
			int v_blue = (int) blue;
			int v_green = (int) green;
			v_red = (int) std::max(v_red - (v_red * fraction), 0.0);
			v_blue = (int) std::max(v_blue - (v_blue * fraction), 0.0);
			v_green = (int) std::max(v_green - (v_green * fraction), 0.0);
			return {(byte) v_red, (byte) v_green, (byte) v_blue, alpha};
		}
	};
}

#endif  // LH_WEB_COLOR_H
