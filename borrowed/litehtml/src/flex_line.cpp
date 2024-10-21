#include "html.h"
#include "flex_line.h"
#include "flex_item.h"
#include "render_item.h"

void litehtml::flex_line::distribute_free_space(int container_main_size)
{
	// Determine the used flex factor. Sum the outer hypothetical main sizes of all items on the line.
	// If the sum is less than the flex container’s inner main size, use the flex grow factor for the
	// rest of this algorithm; otherwise, use the flex shrink factor.
	int initial_free_space = container_main_size - base_size;
	bool grow;
	int total_flex_factor;
	if(initial_free_space < 0)
	{
		grow = false;
		total_flex_factor = total_shrink;
		// Flex values between 0 and 1 have a somewhat special behavior: when the sum of the flex values on the line
		// is less than 1, they will take up less than 100% of the free space.
		// https://www.w3.org/TR/css-flexbox-1/#valdef-flex-flex-grow
		if(total_flex_factor < 1000)
		{
			for(auto &item : items)
			{
				item->main_size += initial_free_space * item->shrink / 1000;
			}
			return;
		}
	} else
	{
		grow = true;
		total_flex_factor = total_grow;
		// Flex values between 0 and 1 have a somewhat special behavior: when the sum of the flex values on the line
		// is less than 1, they will take up less than 100% of the free space.
		// https://www.w3.org/TR/css-flexbox-1/#valdef-flex-flex-grow
		if(total_flex_factor < 1000)
		{
			for(auto &item : items)
			{
				item->main_size += initial_free_space * item->grow / 1000;
			}
			return;
		}
	}

	if(total_flex_factor > 0)
	{
		bool processed = true;
		while (processed)
		{
			int sum_scaled_flex_shrink_factor = 0;
			int sum_flex_factors = 0;
			int remaining_free_space = container_main_size;
			int total_not_frozen = 0;
			for (auto &item: items)
			{
				if (!item->frozen)
				{
					sum_scaled_flex_shrink_factor += item->scaled_flex_shrink_factor;
					if(grow)
					{
						sum_flex_factors += item->grow;
					} else
					{
						sum_flex_factors += item->shrink;
					}
					remaining_free_space -= item->base_size;
					total_not_frozen++;
				} else
				{
					remaining_free_space -= item->main_size;
				}
			}
			// Check for flexible items. If all the flex items on the line are frozen, free space has
			// been distributed; exit this loop.
			if (!total_not_frozen) break;

			remaining_free_space = abs(remaining_free_space);
			// c. Distribute free space proportional to the flex factors.
			// If the remaining free space is zero
			//    Do nothing.
			if (!remaining_free_space)
			{
				processed = false;
			} else
			{
				int total_clamped = 0;
				for (auto &item: items)
				{
					if (!item->frozen)
					{
						if(!grow)
						{
							// If using the flex shrink factor
							//    For every unfrozen item on the line, multiply its flex shrink factor by its
							//    inner flex base size, and note this as its scaled flex shrink factor. Find
							//    the ratio of the item’s scaled flex shrink factor to the sum of the scaled
							//    flex shrink factors of all unfrozen items on the line. Set the item’s target
							//    main size to its flex base size minus a fraction of the absolute value of the
							//    remaining free space proportional to the ratio.
							int scaled_flex_shrink_factor = item->base_size * item->shrink;
							item->main_size = (int) ((float) item->base_size - (float) remaining_free_space *
																			 (float) scaled_flex_shrink_factor /
																			 (float) sum_scaled_flex_shrink_factor);

							// d. Fix min/max violations. Clamp each non-frozen item’s target main size by its used
							// min and max main sizes and floor its content-box size at zero. If the item’s target
							// main size was made smaller by this, it’s a max violation. If the item’s target main
							// size was made larger by this, it’s a min violation.
							if (item->main_size <= item->min_size)
							{
								total_clamped++;
								item->main_size = item->min_size;
								item->frozen = true;
							}
							if(!item->max_size.is_default() && item->main_size >= item->max_size)
							{
								total_clamped++;
								item->main_size = item->max_size;
								item->frozen = true;
							}
						} else
						{
							// If using the flex grow factor
							//    Find the ratio of the item’s flex grow factor to the sum of the flex grow
							//    factors of all unfrozen items on the line. Set the item’s target main size to
							//    its flex base size plus a fraction of the remaining free space proportional
							//    to the ratio.
							item->main_size = (int) ((float) item->base_size +
													(float) remaining_free_space * (float) item->grow /
													(float) total_flex_factor);
							// d. Fix min/max violations. Clamp each non-frozen item’s target main size by its used
							// min and max main sizes and floor its content-box size at zero. If the item’s target
							// main size was made smaller by this, it’s a max violation. If the item’s target main
							// size was made larger by this, it’s a min violation.
							if (item->main_size >= container_main_size)
							{
								total_clamped++;
								item->main_size = container_main_size;
								item->frozen = true;
							}
							if(!item->max_size.is_default() && item->main_size >= item->max_size)
							{
								total_clamped++;
								item->main_size = item->max_size;
								item->frozen = true;
							}
						}
					}
				}
				if (total_clamped == 0) processed = false;
			}
		}
		// Distribute remaining after algorithm space
		int sum_main_size = 0;
		for(auto &item : items)
		{
			sum_main_size += item->main_size;
		}
		int free_space = container_main_size - sum_main_size;
		if(free_space > 0)
		{
			for(auto &item : items)
			{
				if(free_space == 0) break;
				item->main_size++;
				free_space--;
			}
		}
	}
}

bool litehtml::flex_line::distribute_main_auto_margins(int free_main_size)
{
	if(free_main_size > 0 && (num_auto_margin_main_start || num_auto_margin_main_end))
	{
		int add = (int) (free_main_size / (items.size() * 2));
		for (auto &item: items)
		{
			if(!item->auto_margin_main_start.is_default())
			{
				item->auto_margin_main_start = add;
				item->main_size += add;
				main_size += add;
				free_main_size -= add;
			}
			if(!item->auto_margin_main_end.is_default())
			{
				item->auto_margin_main_end = add;
				item->main_size += add;
				main_size += add;
				free_main_size -= add;
			}
		}
		while (free_main_size > 0)
		{
			for (auto &item: items)
			{
				if(!item->auto_margin_main_start.is_default())
				{
					item->auto_margin_main_start = item->auto_margin_main_start + 1;
					free_main_size--;
					if(!free_main_size) break;
				}
				if(!item->auto_margin_main_end.is_default())
				{
					item->auto_margin_main_end = item->auto_margin_main_end + 1;
					free_main_size--;
					if(!free_main_size) break;
				}
			}
		}
		return true;
	}
	return false;
}

void litehtml::flex_line::init(int container_main_size, bool fit_container, bool is_row_direction,
							   const litehtml::containing_block_context &self_size,
							   litehtml::formatting_context *fmt_ctx)
{
	cross_size = 0;
	main_size = 0;
	first_baseline.set(0, baseline::baseline_type_none);
	last_baseline.set(0, baseline::baseline_type_none);

	if(!fit_container)
	{
		distribute_free_space(container_main_size);
	}

	if(is_row_direction)
	{
		def_value<int> first_baseline_top = 0;
		def_value<int> first_baseline_bottom = 0;
		def_value<int> last_baseline_top = 0;
		def_value<int> last_baseline_bottom = 0;
		int non_baseline_height = 0;

		// Calculate maximum cross size
		def_value<int> max_cross_size(0);
		if(self_size.height.type != containing_block_context::cbc_value_type_auto)
		{
			max_cross_size = self_size.height;
		}
		if(self_size.max_height.type != containing_block_context::cbc_value_type_none)
		{
			if(max_cross_size.is_default())
			{
				max_cross_size = self_size.max_height;
			} else
			{
				max_cross_size = std::max((int) max_cross_size, (int) self_size.max_height);
			}
		}

		/// Render items into new size
		/// Find line cross_size
		/// Find line first/last baseline
		for (auto &item: items)
		{
			item->el->render(0,
							 0,
							 self_size.new_width(item->main_size - item->el->content_offset_width(), containing_block_context::size_mode_exact_width), fmt_ctx, false);

			if((item->align & 0xFF) == flex_align_items_baseline)
			{
				if(item->align & flex_align_items_last)
				{
					last_baseline.type(reverse_cross ? baseline::baseline_type_top : baseline::baseline_type_bottom);

					int top = -item->el->get_last_baseline();
					int bottom = top + item->el->height();

					if(last_baseline_top.is_default()) last_baseline_top = top;
					else last_baseline_top = std::min((int) last_baseline_top, top);

					if(last_baseline_bottom.is_default()) last_baseline_bottom = bottom;
					else last_baseline_bottom = std::max((int)last_baseline_bottom, bottom);
				} else
				{
					first_baseline.type(reverse_cross ? baseline::baseline_type_bottom : baseline::baseline_type_top);
					int top = -item->el->get_first_baseline();
					int bottom = top + item->el->height();

					if(first_baseline_top.is_default()) first_baseline_top = top;
					else first_baseline_top = std::min((int) first_baseline_top, top);

					if(first_baseline_bottom.is_default()) first_baseline_bottom = bottom;
					else first_baseline_bottom = std::max((int) first_baseline_bottom, bottom);
				}
			} else
			{
				non_baseline_height = std::max(non_baseline_height, item->el->height());
			}
			main_size += item->el->width();
		}

		cross_size = std::max(first_baseline_bottom - first_baseline_top,last_baseline_bottom - last_baseline_top);
		cross_size = std::max(cross_size, non_baseline_height);
		if(!max_cross_size.is_default() && cross_size > max_cross_size)
		{
			cross_size = max_cross_size;
		}

		first_baseline.calc(first_baseline_top, first_baseline_bottom);
		last_baseline.calc(last_baseline_top, last_baseline_bottom);
	} else
	{
		// Calculate maximum cross size
		def_value<int> max_cross_size(0);
		if(self_size.width.type != containing_block_context::cbc_value_type_auto)
		{
			max_cross_size = self_size.width;
		}
		if(self_size.max_width.type != containing_block_context::cbc_value_type_none)
		{
			if(max_cross_size.is_default())
			{
				max_cross_size = self_size.max_width;
			} else
			{
				max_cross_size = std::max((int) max_cross_size, (int) self_size.max_width);
			}
		}

		for (auto &item: items)
		{
			int el_ret_width = item->el->render(0,
												0,
												self_size, fmt_ctx, false);
			item->el->render(0,
							 0,
							 self_size.new_width_height(el_ret_width - item->el->content_offset_width(),
														item->main_size - item->el->content_offset_height(),
														containing_block_context::size_mode_exact_width |
														containing_block_context::size_mode_exact_height),
							 fmt_ctx, false);
			main_size += item->el->height();
			cross_size = std::max(cross_size, item->el->width());
		}
		if(!max_cross_size.is_default() && cross_size > max_cross_size)
		{
			cross_size = max_cross_size;
		}
	}
}

int litehtml::flex_line::calculate_items_position(int container_main_size,
												  flex_justify_content justify_content,
												  bool is_row_direction,
												  const containing_block_context &self_size,
												  formatting_context *fmt_ctx)
{
	/// Distribute main axis free space for auto-margins
	int free_main_size = container_main_size - main_size;
	distribute_main_auto_margins(free_main_size);
	free_main_size = container_main_size - main_size;

	/// Fix justify-content property
	switch (justify_content)
	{
		case flex_justify_content_left:
		case flex_justify_content_right:
			if(!is_row_direction)
			{
				justify_content = flex_justify_content_start;
			}
			break;
		case flex_justify_content_space_between:
			// If the leftover free-space is negative or there is only a single flex item on the line, this
			// value is identical to flex-start.
			if(items.size() == 1 || free_main_size < 0)  justify_content = flex_justify_content_flex_start;
			break;
		case flex_justify_content_space_around:
		case flex_justify_content_space_evenly:
			// If the leftover free-space is negative or there is only a single flex item on the line, this
			// value is identical to center
			if(items.size() == 1 || free_main_size < 0)  justify_content = flex_justify_content_center;
			break;
		default:
			break;
	}

	/// Distribute free main size using justify-content property
	int main_pos = 0;
	int add_before_item = 0;
	int add_after_item = 0;
	int item_remainder = 0;

	/// find initial main position and spaces between items
	switch (justify_content)
	{

		case flex_justify_content_right:
			main_pos = free_main_size;
			break;
		case flex_justify_content_left:
		case flex_justify_content_start:
			main_pos = 0;
			break;
		case flex_justify_content_end:
			main_pos = free_main_size;
			break;
		case flex_justify_content_flex_end:
			if(!reverse_main)
			{
				main_pos = free_main_size;
			}
			break;
		case flex_justify_content_center:
			main_pos = free_main_size / 2;
			break;
		case flex_justify_content_space_between:
			add_after_item = free_main_size / ((int) items.size() - 1);
			item_remainder = free_main_size - (add_after_item * ((int) items.size() - 1));
			break;
		case flex_justify_content_space_around:
			add_after_item = add_before_item = free_main_size / ((int) items.size() * 2);
			item_remainder = free_main_size - (add_after_item * (int) items.size() * 2);
			break;
		case flex_justify_content_space_evenly:
			add_before_item = free_main_size / ((int) items.size() + 1);
			item_remainder = free_main_size - add_before_item * ((int) items.size() + 1);
			break;
		default:
			if(reverse_main)
			{
				main_pos = free_main_size;
			}
			break;
	}

	/// Place all items in main and cross positions
	int height =  0;
	for(auto &item : items)
	{
		main_pos += add_before_item;
		if(add_before_item > 0 && item_remainder > 0)
		{
			main_pos++;
			item_remainder--;
		}
		item->place(*this, main_pos, self_size, fmt_ctx);
		main_pos += item->get_el_main_size() + add_after_item;
		if(add_after_item > 0 && item_remainder > 0)
		{
			main_pos++;
			item_remainder--;
		}
		height = std::max(height, item->el->bottom());
	}
	return height;
}
