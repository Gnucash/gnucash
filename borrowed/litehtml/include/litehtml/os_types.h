#ifndef LH_OS_TYPES_H
#define LH_OS_TYPES_H

#include <string>
#include <cstdint>

namespace litehtml
{
	using std::string;
	typedef std::uintptr_t	uint_ptr;

#if defined( WIN32 ) || defined( _WIN32 ) || defined( WINCE )

// noexcept appeared since Visual Studio 2015
#if defined(_MSC_VER) && _MSC_VER < 1900
#define noexcept
#endif

	#define t_itoa(value, buffer, size, radix)	_itoa_s(value, buffer, size, radix)
	#define t_snprintf(s, n, format, ...) _snprintf_s(s, _TRUNCATE, n, format, __VA_ARGS__)

#else

	#define t_itoa(value, buffer, size, radix)	snprintf(buffer, size, "%d", value)
	#define t_snprintf(s, n, format, ...) snprintf(s, n, format, __VA_ARGS__)

#endif
}

#endif  // LH_OS_TYPES_H
