#ifndef LITEHTML_FLEX_LINE_H
#define LITEHTML_FLEX_LINE_H

#include "formatting_context.h"

namespace litehtml
{
	class flex_item;

	class flex_line
	{
		public:
		std::list<std::shared_ptr<flex_item>> items;
		int cross_start;	// for row direction: top. for column direction: left
		int main_size;		// sum of all items main size
		int cross_size;		// sum of all items cross size
		int base_size;
		int total_grow;
		int total_shrink;
		int num_auto_margin_main_start;		// number of items with auto margin left/top
		int num_auto_margin_main_end;		// number of items with auto margin right/bottom
		baseline first_baseline;
		baseline last_baseline;
		bool reverse_main;
		bool reverse_cross;

		flex_line(bool _reverse_main, bool _reverse_cross) :
				cross_start(0),
				main_size(0),
				cross_size(0),
				base_size(0),
				total_grow(0),
				total_shrink(0),
				num_auto_margin_main_start(0),
				num_auto_margin_main_end(0),
				first_baseline(),
				last_baseline(),
				reverse_main(_reverse_main),
				reverse_cross(_reverse_cross)
		{}

		void init(int container_main_size, bool fit_container, bool is_row_direction,
				  const litehtml::containing_block_context &self_size,
				  litehtml::formatting_context *fmt_ctx);
		bool distribute_main_auto_margins(int free_main_size);
		int  calculate_items_position(int container_main_size,
									  flex_justify_content justify_content,
									  bool is_row_direction,
									  const containing_block_context &self_size,
									  formatting_context *fmt_ctx);
	protected:
		void distribute_free_space(int container_main_size);
	};
}

#endif //LITEHTML_FLEX_LINE_H
