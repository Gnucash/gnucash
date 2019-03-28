#!/bin/sh
#
# Copyright (C) 2018 Aleix Conchillo Flaque <aconchillo@gmail.com>
#
# This file is part of guile-json.
#
# guile-json is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# guile-json is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with guile-json. If not, see https://www.gnu.org/licenses/.
#

if test -z "$GUILE_LOAD_PATH"; then
  GUILE_LOAD_PATH="@abs_top_srcdir@"
else
  GUILE_LOAD_PATH="@abs_top_srcdir@":$GUILE_LOAD_PATH
fi

if test -z "$GUILE_LOAD_COMPILED_PATH"; then
  GUILE_LOAD_COMPILED_PATH="@abs_top_builddir@"
else
  GUILE_LOAD_COMPILED_PATH="@abs_top_builddir@":$GUILE_LOAD_COMPILED_PATH
fi

export GUILE_LOAD_PATH GUILE_LOAD_COMPILED_PATH

exec "$@"
