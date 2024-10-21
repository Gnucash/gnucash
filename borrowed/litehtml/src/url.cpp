// Copyright (C) 2020-2021 Primate Labs Inc.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//    * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//    * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//    * Neither the names of the copyright holders nor the names of their
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "url.h"

#include <iostream>
#include <sstream>
#include <algorithm>

#include "codepoint.h"
#include "url_path.h"

namespace litehtml {

url::url(const string& str)
: str_(str)
{
    // TODO: Rewrite using tstring_view to avoid unnecessary allocations.
    string tmp = str_;

    // Does the URL include a scheme?
    size_t offset = tmp.find(':');
    if (offset != string::npos) {
        bool valid_scheme = true;
        for (size_t i = 0; i < offset; i++) {
            if (!is_url_scheme_codepoint(tmp[i])) {
                valid_scheme = false;
                break;
            }
        }
        if (valid_scheme) {
            scheme_ = tmp.substr(0, offset);
            tmp = tmp.substr(offset + 1);
        }
    }

    // Does the URL include an authority?  An authority component is preceded
    // by a double slash ("//") and is terminated by the next slash ("/"),
    // question mark ("?"), number sign ("#"), or the end of the URL.

    if (tmp.size() >= 2 && tmp[0] == '/' && tmp[1] == '/') {
        tmp = tmp.substr(2);
        offset = tmp.size();
        offset = std::min(offset, tmp.find('/'));
        offset = std::min(offset, tmp.find('?'));
        offset = std::min(offset, tmp.find('#'));
        authority_ = tmp.substr(0, offset);
        tmp = tmp.substr(offset);

        // TODO: Parse the network location into host and port?
    }

    // Does the URL include a fragment?
    offset = tmp.find('#');
    if (offset != string::npos) {
        fragment_ = tmp.substr(offset + 1);
        tmp = tmp.substr(0, offset);
    }

    // Does the URL include a query?
    offset = tmp.find('?');
    if (offset != string::npos) {
        query_ = tmp.substr(offset + 1);
        tmp = tmp.substr(0, offset);
    }

    // Whatever remains of the URL after removing the scheme, the network
    // location, the query, and the fragment is the path.
    path_ = tmp;
}

url::url(const string& scheme,
    const string& authority,
    const string& path,
    const string& query,
    const string& fragment)
: scheme_(scheme)
, authority_(authority)
, path_(path)
, query_(query)
, fragment_(fragment)
{
    std::stringstream tss;

    if (!scheme_.empty()) {
        tss << scheme_ << ":";
    }
    if (!authority_.empty()) {
        tss << "//" << authority_;
    }
    if (!path_.empty()) {
        tss << path_;
    }
    if (!query_.empty()) {
        tss << "?" << query_;
    }
    if (!fragment_.empty()) {
        tss << "#" << fragment_;
    }
    str_ = tss.str();
}

url resolve(const url& b, const url& r)
{
    // The resolution algorithm roughly follows the resolution algorithm
    // outlined in Section 5.2 (in particular Section 5.2.2) of RFC 3986.  The
    // major difference between the resolution algorithm and resolve() is that
    // resolve() does not attempt to normalize the path components.

    if (r.has_scheme()) {
        return r;
    } else if (r.has_authority()) {
        return url(b.scheme(), r.authority(), r.path(), r.query(), r.fragment());
    } else if (r.has_path()) {

        // The relative URL path is either an absolute path or a relative
        // path. If it is an absolute path, build the URL using only the
        // relative path.  If it is a relative path, resolve the relative path
        // against the base path and build the URL using the resolved path.

        if (is_url_path_absolute(r.path())) {
            return url(b.scheme(), b.authority(), r.path(), r.query(), r.fragment());
        } else {
            string path = url_path_resolve(b.path(), r.path());
            return url(b.scheme(), b.authority(), path, r.query(), r.fragment());
        }

    } else if (r.has_query()) {
        return url(b.scheme(), b.authority(), b.path(), r.query(), r.fragment());
    } else {
        // The resolved URL never includes the base URL fragment (i.e., it
        // always includes the reference URL fragment).
        return url(b.scheme(), b.authority(), b.path(), b.query(), r.fragment());
    }
}

} // namespace litehtml
