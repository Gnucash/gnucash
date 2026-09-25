/********************************************************************
 * gnc-icu-locale.cpp -- Localization with ICU.                        *
 *                                                                  *
 * Copyright (C) 2025 John Ralls <jralls@ceridwen.us                *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/

#include "gnc-unicode.h"

#include <memory>
#include <vector>
#include <unicode/stsearch.h>
#include <unicode/tblcoll.h>
#include <unicode/coll.h>
#include "gnc-locale-utils.h"
#include <glib-2.0/glib.h>

constexpr const char *logdomain{"gnc.locale"};

enum class CompareStrength {
  PRIMARY,
  SECONDARY,
  TERTIARY,
  QUATERNARY,
  IDENTICAL
};

static void
collator_set_strength(icu::Collator* collator, CompareStrength strength)
{
    switch (strength)
    {
        case CompareStrength::PRIMARY:
            collator->setStrength(icu::Collator::PRIMARY);
            break;
        case CompareStrength::SECONDARY:
            collator->setStrength(icu::Collator::SECONDARY);
            break;
        case CompareStrength::TERTIARY:
            collator->setStrength(icu::Collator::TERTIARY);
            break;
        case CompareStrength::QUATERNARY:
            collator->setStrength(icu::Collator::QUATERNARY);
            break;
        case CompareStrength::IDENTICAL:
            collator->setStrength(icu::Collator::IDENTICAL);
            break;
    }
}

static bool
unicode_has_substring_internal(const char* needle, const char* haystack,
                       int* position, int* length,
                       CompareStrength strength)
{
    UErrorCode status{U_ZERO_ERROR};
    auto locale{gnc_locale_name()};
    auto u_needle{icu::UnicodeString::fromUTF8(needle)};
    auto u_haystack{icu::UnicodeString::fromUTF8(haystack)};
    icu::StringSearch search(u_needle, u_haystack, locale, nullptr, status);
    g_free(locale);

    if (U_SUCCESS(status))
    {
        auto collator = search.getCollator();
        collator_set_strength(collator, strength);
        search.reset();
    }

    if (U_FAILURE(status))
    {
        g_log(logdomain, G_LOG_LEVEL_ERROR,
              "StringSearch creation failed for %s", haystack);
        return false;
    }

    auto pos{search.first(status)};
    if (U_FAILURE(status))
    {
        g_log(logdomain, G_LOG_LEVEL_ERROR,
              "StringSearch encountered an error finding %s in %s",
              needle, haystack);
        return false;
    }
    if (pos == USEARCH_DONE)
    {
        g_log(logdomain, G_LOG_LEVEL_DEBUG, "%s not found in %s",
              needle, haystack);
        return false;
    }

    if (position && length)
    {
        *position = pos;
        *length = search.getMatchedLength();
    }

    g_log(logdomain, G_LOG_LEVEL_DEBUG, "%s found in %s at index %d",
          needle, haystack, pos);
    return true;
}

bool
gnc_unicode_has_substring_base_chars(const char* needle,
                                     const char* haystack,
                                     int* position,
                                     int* length)
{
    return unicode_has_substring_internal(needle, haystack, position, length,
                                          CompareStrength::PRIMARY);
}

bool
gnc_unicode_has_substring_accented_chars(const char* needle,
                                         const char* haystack,
                                         int* position,
                                         int* length)
{
    return unicode_has_substring_internal(needle, haystack, position, length,
                                          CompareStrength::SECONDARY);
}

bool
gnc_unicode_has_substring_accented_case_sensitive(const char* needle,
                                                  const char* haystack,
                                                  int* position,
                                                  int* length)
{
    return unicode_has_substring_internal(needle, haystack, position, length,
                                          CompareStrength::TERTIARY);
}

bool
gnc_unicode_has_substring_identical(const char* needle,
                                    const char*haystack,
                                    int* position,
                                    int* length)
{
    auto location = strstr(haystack, needle);
    if (location && location != haystack)
    {
        *position = static_cast<int>(location - haystack);
        *length = strlen(needle);
        return true;
    }
    return false;
}

static icu::Collator*
get_collator(CompareStrength strength)
{
    thread_local std::vector<std::pair<CompareStrength, std::unique_ptr<icu::Collator>>> cache;

    for (const auto& kvp : cache)
        if (kvp.first == strength)
            return kvp.second.get();

    UErrorCode status{U_ZERO_ERROR};
    auto locale{gnc_locale_name()};
    std::unique_ptr<icu::Collator> coll(
        icu::Collator::createInstance(icu::Locale(locale), status));

    if (U_SUCCESS(status))
        collator_set_strength(coll.get(), strength);

    if (U_FAILURE(status))
    {
        g_log(logdomain, G_LOG_LEVEL_ERROR,
              "Failed to create collator for locale %s: %s",
              locale, u_errorName(status));
        g_free(locale);
        cache.push_back ({strength, nullptr});
        return nullptr;
    }

    auto* ptr = coll.get();
    cache.push_back ({strength, std::move(coll)});

    g_free(locale);
    return ptr;
}


static int
unicode_compare_internal(const char* one, const char* two,
                         CompareStrength strength)
{
    auto coll = get_collator (strength);
    if (!coll)
        return -99;

    UErrorCode status{U_ZERO_ERROR};
    auto result = coll->compare(one, two, status);

    if (U_FAILURE(status))
    {
        g_log(logdomain, G_LOG_LEVEL_ERROR,
              "Comparison of %s and %s failed: %s",
              one, two, u_errorName(status));
        return -99;
    }

    return result == UCOL_LESS ? -1 : result == UCOL_EQUAL ? 0 : 1;
}

int
gnc_unicode_compare_base_chars(const char* one, const char* two)
{
    return unicode_compare_internal(one, two, CompareStrength::PRIMARY);
}

int
gnc_unicode_compare_accented_chars(const char* one, const char* two)
{
    return unicode_compare_internal(one, two, CompareStrength::SECONDARY);
}

int
gnc_unicode_compare_accented_case_sensitive(const char* one, const char* two)
{
    return unicode_compare_internal(one, two, CompareStrength::TERTIARY);
}

int
gnc_unicode_compare_identical(const char* one, const char* two)
{
    return unicode_compare_internal(one, two, CompareStrength::IDENTICAL);

}
