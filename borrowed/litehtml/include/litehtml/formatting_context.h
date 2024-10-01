#ifndef LITEHTML_FLOATS_HOLDER_H
#define LITEHTML_FLOATS_HOLDER_H

#include <list>
#include "types.h"

namespace litehtml
{
	class formatting_context
	{
	private:
		std::list<floated_box> m_floats_left;
		std::list<floated_box> m_floats_right;
		int_int_cache m_cache_line_left;
		int_int_cache m_cache_line_right;
		int m_current_top;
		int m_current_left;

	public:
		formatting_context() : m_current_top(0), m_current_left(0)	{}

		void push_position(int x, int y)
		{
			m_current_left += x;
			m_current_top += y;
		}
		void pop_position(int x, int y)
		{
			m_current_left -= x;
			m_current_top -= y;
		}

		void add_float(const std::shared_ptr<render_item> &el, int min_width, int context);
		void clear_floats(int context);
		int find_next_line_top( int top, int width, int def_right );
		int get_floats_height(element_float el_float = float_none) const;
		int get_left_floats_height() const;
		int get_right_floats_height() const;
		int get_line_left( int y );
		void get_line_left_right( int y, int def_right, int& ln_left, int& ln_right )
		{
			ln_left		= get_line_left(y);
			ln_right	= get_line_right(y, def_right);
		}
		int get_line_right( int y, int def_right );
		int get_cleared_top(const std::shared_ptr<render_item> &el, int line_top) const;
		void update_floats(int dy, const std::shared_ptr<render_item> &parent);
		void apply_relative_shift(const containing_block_context &containing_block_size);
		int find_min_left(int y, int context_idx);
		int find_min_right(int y, int right, int context_idx);
	};
}

#endif //LITEHTML_FLOATS_HOLDER_H
