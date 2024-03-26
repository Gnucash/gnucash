#ifndef LITEHTML_FLEX_ITEM_H
#define LITEHTML_FLEX_ITEM_H

#include <functional>
#include "formatting_context.h"

namespace litehtml
{
	class flex_line;

	/**
	 * Base class for flex item
	 */
	class flex_item
	{
	public:
		std::shared_ptr<render_item> el;
		int base_size;
		int min_size;
		def_value<int> max_size;
		int main_size;
		int grow;
		int shrink;
		int scaled_flex_shrink_factor;
		bool frozen;
		int order;
		int src_order;
		def_value<int> auto_margin_main_start;
		def_value<int> auto_margin_main_end;
		bool auto_margin_cross_start;
		bool auto_margin_cross_end;
		flex_align_items align;

		explicit flex_item(std::shared_ptr<render_item> &_el) :
				el(_el),
				base_size(0),
				min_size(0),
				max_size(0),
				main_size(0),
				grow(0),
				shrink(0),
				scaled_flex_shrink_factor(0),
				frozen(false),
				order(0),
				src_order(0),
				auto_margin_main_start(0),
				auto_margin_main_end(0),
				auto_margin_cross_start(false),
				auto_margin_cross_end(false),
				align(flex_align_items_auto)
		{}

		virtual ~flex_item() = default;

		bool operator<(const flex_item& b) const
		{
			if(order < b.order) return true;
			if(order == b.order) return src_order < b.src_order;
			return false;
		}
		void init(const litehtml::containing_block_context &self_size,
				  litehtml::formatting_context *fmt_ctx, flex_align_items align_items);
		virtual void apply_main_auto_margins() = 0;
		virtual bool apply_cross_auto_margins(int cross_size) = 0;
		virtual void set_main_position(int pos) = 0;
		virtual void set_cross_position(int pos) = 0;
		virtual int get_el_main_size() = 0;
		virtual int get_el_cross_size() = 0;

		void place(flex_line &ln, int main_pos,
				   const containing_block_context &self_size,
				   formatting_context *fmt_ctx);
		int get_last_baseline(baseline::_baseline_type type) const;
		int get_first_baseline(baseline::_baseline_type type) const;

	protected:
		virtual void direction_specific_init(const litehtml::containing_block_context &self_size,
											 litehtml::formatting_context *fmt_ctx) = 0;
		virtual void align_stretch(flex_line &ln, const containing_block_context &self_size,
								   formatting_context *fmt_ctx) = 0;
		virtual void align_baseline(flex_line &ln,
									const containing_block_context &self_size,
									formatting_context *fmt_ctx) = 0;
	};

	/**
	 * Flex item with "flex-direction: row" or " flex-direction: row-reverse"
	 */
	class flex_item_row_direction : public flex_item
	{
	public:
		explicit flex_item_row_direction(std::shared_ptr<render_item> &_el) : flex_item(_el) {}

		void apply_main_auto_margins() override;
		bool apply_cross_auto_margins(int cross_size) override;
		void set_main_position(int pos) override;
		void set_cross_position(int pos) override;
		int get_el_main_size() override;
		int get_el_cross_size() override;

	protected:
		void direction_specific_init(const litehtml::containing_block_context &self_size,
									 litehtml::formatting_context *fmt_ctx) override;
		void align_stretch(flex_line &ln, const containing_block_context &self_size,
						   formatting_context *fmt_ctx) override;
		void align_baseline(flex_line &ln,
							const containing_block_context &self_size,
							formatting_context *fmt_ctx) override;
	};

	/**
	 * Flex item with "flex-direction: column" or " flex-direction: column-reverse"
	 */
	class flex_item_column_direction : public flex_item
	{
	public:
		explicit flex_item_column_direction(std::shared_ptr<render_item> &_el) : flex_item(_el) {}

		void apply_main_auto_margins() override;
		bool apply_cross_auto_margins(int cross_size) override;
		void set_main_position(int pos) override;
		void set_cross_position(int pos) override;
		int get_el_main_size() override;
		int get_el_cross_size() override;

	protected:
		void direction_specific_init(const litehtml::containing_block_context &self_size,
									 litehtml::formatting_context *fmt_ctx) override;
		void align_stretch(flex_line &ln, const containing_block_context &self_size,
						   formatting_context *fmt_ctx) override;
		void align_baseline(flex_line &ln,
							const containing_block_context &self_size,
							formatting_context *fmt_ctx) override;
	};
}

#endif //LITEHTML_FLEX_ITEM_H
