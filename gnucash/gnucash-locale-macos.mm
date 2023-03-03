/*
 * gnucash-mac-locale.mm -- Macos specific locale handling
 *
 * Copyright (C) 2020 John Ralls <jralls@ceridwen.us>
 * Copyright (C) 2021 Geert Janssens <geert@kobaltwit.be>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */
#include <config.h>

#include <Foundation/Foundation.h>
#include <glib.h>
#include <qoflog.h>
#include <gnc-engine.h>

#include <gnc-locale-utils.h>
#include "gnucash-locale-platform.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static void
mac_set_currency_locale(NSLocale *locale, NSString *locale_str)
{
    /* If the currency doesn't match the base locale, we need to find a locale that does match, because setlocale won't know what to do with just a currency identifier. */
    NSLocale *cur_locale = [ [NSLocale alloc] initWithLocaleIdentifier: locale_str];
    if (![ [locale objectForKey: NSLocaleCurrencyCode] isEqualToString:
          [cur_locale objectForKey: NSLocaleCurrencyCode] ])
    {
        NSArray *all_locales = [NSLocale availableLocaleIdentifiers];
        NSEnumerator *locale_iter = [all_locales objectEnumerator];
        NSString *this_locale;
        NSString *currency = [locale objectForKey: NSLocaleCurrencyCode];
        NSString *money_locale = nil;
        while ((this_locale = (NSString*)[locale_iter nextObject]))
        {
            NSLocale *templocale = [ [NSLocale alloc]
                                    initWithLocaleIdentifier: this_locale];
            if ([ [templocale objectForKey: NSLocaleCurrencyCode]
                 isEqualToString: currency])
            {
                money_locale = this_locale;
                [templocale release];
                break;
            }
            [templocale release];
        }
        if (money_locale)
            setlocale(LC_MONETARY, [money_locale UTF8String]);
    }
    [cur_locale release];
}
/* The locale that we got from AppKit isn't a supported POSIX one, so we need to
 * find something close. First see if we can find another locale for the
 * country; failing that, try the language. Ultimately fall back on en_US.
 */
static NSString*
mac_find_close_country(NSString *locale_str, NSString *country_str,
                       NSString *lang_str)
{
    NSArray *all_locales = [NSLocale availableLocaleIdentifiers];
    NSEnumerator *locale_iter = [all_locales objectEnumerator];
    NSString *this_locale, *new_locale = nil;
    PWARN("Apple Locale is set to a value %s not supported"
          " by the C runtime", [locale_str UTF8String]);
    while ((this_locale = [(NSString*)[locale_iter nextObject] stringByAppendingString: @".UTF-8"]))
        if ([ [ [NSLocale componentsFromLocaleIdentifier: this_locale]
              objectForKey: NSLocaleCountryCode]
             isEqualToString: country_str] &&
            setlocale (LC_ALL, [this_locale UTF8String]))
        {
            new_locale = this_locale;
            break;
        }
    if (!new_locale)
        while ((this_locale = (NSString*)[locale_iter nextObject]))
            if ([ [ [NSLocale componentsFromLocaleIdentifier: this_locale]
                  objectForKey: NSLocaleLanguageCode]
                 isEqualToString: lang_str] &&
                setlocale (LC_ALL, [this_locale UTF8String]))
            {
                new_locale = this_locale;
                break;
            }
    if (new_locale)
        locale_str = new_locale;
    else
    {
        locale_str = @"en_US.UTF-8";
        setlocale(LC_ALL, [locale_str UTF8String]);
    }
    PWARN("Using %s instead.", [locale_str UTF8String]);
    return locale_str;
}

/* Language subgroups (e.g., US English) are reported in the form "ll-SS"
 * (e.g. again, "en-US"), not what gettext wants. We convert those to
 * old-style locales, which is easy for most cases. There are two where it
 * isn't, though: Simplified Chinese (zh-Hans) and traditional Chinese
 * (zh-Hant), which are normally assigned the locales zh_CN and zh_TW,
 * respectively. Those are handled specially.
 */
static NSString*
mac_convert_complex_language(NSString* this_lang)
{
    NSArray *elements = [this_lang componentsSeparatedByString: @"-"];
    if ([elements count] == 1)
        return this_lang;
    if ([ [elements objectAtIndex: 0] isEqualToString: @"zh"]) {
        if ([ [elements objectAtIndex: 1] isEqualToString: @"Hans"])
            this_lang = @"zh_CN";
        else
            this_lang = @"zh_TW";
    }
    else
        this_lang = [elements componentsJoinedByString: @"_"];
    return this_lang;
}

static void
mac_set_languages(NSArray* languages, NSString *lang_str)
{
    /* Process the language list. */

    const char *langs = NULL;
    NSEnumerator *lang_iter = [languages objectEnumerator];
    NSArray *new_languages = [NSArray array];
    NSString *this_lang = NULL;
    NSRange not_found = {NSNotFound, 0};
    while ((this_lang = [lang_iter nextObject])) {
        this_lang = [this_lang stringByTrimmingCharactersInSet:
                     [NSCharacterSet characterSetWithCharactersInString: @"\""] ];
        this_lang = mac_convert_complex_language(this_lang);
        new_languages = [new_languages arrayByAddingObject: this_lang];
/* If it's an English language, add the "C" locale after it so that
 * any messages can default to it */
        if (!NSEqualRanges([this_lang rangeOfString: @"en"], not_found))
            new_languages = [new_languages arrayByAddingObject: @"C"];
        if (![new_languages containsObject: lang_str]) {
            NSArray *temp_array = [NSArray arrayWithObject: lang_str];
            new_languages = [temp_array arrayByAddingObjectsFromArray: new_languages];
        }
        langs = [ [new_languages componentsJoinedByString:@":"] UTF8String];
    }
    if (langs && strlen(langs) > 0)
    {
        PWARN("Language list: %s", langs);
        g_setenv("LANGUAGE", langs, TRUE);
    }
}

char *
set_platform_locale()
{
    NSAutoreleasePool *pool = [ [NSAutoreleasePool alloc] init];
    NSUserDefaults *defs = [NSUserDefaults standardUserDefaults];
    NSLocale *locale = [NSLocale currentLocale];
    NSString *lang_str, *country_str, *locale_str;
    NSArray *languages = [ [defs arrayForKey: @"AppleLanguages"] retain];
    char *gnc_locale = NULL;
    @try
    {
        lang_str = [locale objectForKey: NSLocaleLanguageCode];
        country_str = [locale objectForKey: NSLocaleCountryCode];
        locale_str = [ [ [lang_str stringByAppendingString: @"_"]
                      stringByAppendingString: country_str]
                      stringByAppendingString: @".UTF-8"];
    }
    @catch (NSException *err)
    {
        PWARN("Locale detection raised error %s: %s. "
              "Check that your locale settings in "
              "System Preferences>Languages & Text are set correctly.",
              [ [err name] UTF8String], [ [err reason] UTF8String]);
        locale_str = @"_";
    }
/* If we didn't get a valid current locale, the string will be just "_" */
    if ([locale_str isEqualToString: @"_"])
        locale_str = @"en_US.UTF-8";

    lang_str = mac_convert_complex_language(lang_str);
    if (!setlocale(LC_ALL, [locale_str UTF8String]))
        locale_str =  mac_find_close_country(locale_str, country_str, lang_str);
/* Cache the final locale string to be returned to the calling program */
    gnc_locale = g_strdup ([locale_str UTF8String]);

    if (g_getenv("LANG") == NULL)
        g_setenv("LANG", [locale_str UTF8String], TRUE);
    mac_set_currency_locale(locale, locale_str);

/* Now call gnc_localeconv() to force creation of the app locale
 * before another call to setlocale messes it up. */
    gnc_localeconv ();
    /* Process the languages, including the one from the Apple locale. */
    if (g_getenv("LANGUAGE") == NULL)
    {
        if ([languages count] > 0)
            mac_set_languages(languages, lang_str);
        else
            g_setenv("LANGUAGE", [lang_str UTF8String], TRUE);
    }
    [languages release];
    [pool drain];
    return gnc_locale;
}
