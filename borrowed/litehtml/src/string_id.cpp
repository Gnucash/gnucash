#include "html.h"
#include "string_id.h"
#include <assert.h>

#ifndef LITEHTML_NO_THREADS
	#include <mutex>
	static std::mutex mutex;
	#define lock_guard std::lock_guard<std::mutex> lock(mutex)
#else
	#define lock_guard
#endif

namespace litehtml
{

static std::map<string, string_id> map;
static std::vector<string> array;

static int init()
{
	string_vector names;
	split_string(initial_string_ids, names, ",");
	for (auto& name : names)
	{
		trim(name);
		assert(name[0] == '_' && name.back() == '_');
		name = name.substr(1, name.size() - 2);				// _border_color_ -> border_color
		std::replace(name.begin(), name.end(), '_', '-');	// border_color   -> border-color
		_id(name);  // this will create association _border_color_ <-> "border-color"
	}
	return 0;
}
static int dummy = init();

const string_id empty_id = _id("");
const string_id star_id = _id("*");

string_id _id(const string& str)
{
	lock_guard;
	auto it = map.find(str);
	if (it != map.end()) return it->second;
	// else: str not found, add it to the array and the map
	array.push_back(str);
	return map[str] = (string_id)(array.size() - 1);
}

const string& _s(string_id id)
{
	lock_guard;
	return array[id];
}

} // namespace litehtml