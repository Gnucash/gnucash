#include <litehtml.h>
using namespace litehtml;

class test_container : public document_container
{
public:
	int width;
	int height;
	string basedir;

	test_container(int width, int height, string basedir) : width(width), height(height), basedir(basedir) {}

	uint_ptr		create_font(const char* faceName, int size, int weight, font_style italic, unsigned int decoration, font_metrics* fm) override;
	void			delete_font(uint_ptr /*hFont*/) override {}
	int				text_width(const char* text, uint_ptr hFont) override;
	void			draw_text(uint_ptr hdc, const char* text, uint_ptr hFont, web_color color, const position& pos) override;
	int				pt_to_px(int pt) const override;
	int				get_default_font_size() const override;
	const char*		get_default_font_name() const override;
	void 			load_image(const char* /*src*/, const char* /*baseurl*/, bool /*redraw_on_ready*/) override {}
	void			get_image_size(const char* /*src*/, const char* /*baseurl*/, size& /*sz*/) override {}
	void			draw_background(uint_ptr hdc, const std::vector<background_paint>& bg) override;
	void			draw_borders(uint_ptr hdc, const borders& borders, const position& draw_pos, bool root) override;
	void 			draw_list_marker(uint_ptr hdc, const list_marker& marker) override;
	element::ptr	create_element(const char* /*tag_name*/,
								   const string_map& /*attributes*/,
								   const document::ptr& /*doc*/) override { return nullptr; }
	void			get_media_features(media_features& /*media*/) const override {}
	void			get_language(string& /*language*/, string& /*culture*/) const override {}
	void 			link(const document::ptr& /*doc*/, const element::ptr& /*el*/) override {}

	void			transform_text(string& /*text*/, text_transform /*tt*/) override {}
	void			set_clip(const position& /*pos*/, const border_radiuses& /*bdr_radius*/) override {}
	void			del_clip() override {}

	void 			set_caption(const char* /*caption*/) override {}
	void 			set_base_url(const char* /*base_url*/) override {}
	void			on_anchor_click(const char* /*url*/, const element::ptr& /*el*/) override {}
	void			set_cursor(const char* /*cursor*/) override {}
	void			import_css(string& text, const string& url, string& baseurl) override;
	void			get_client_rect(position& client) const override;
};