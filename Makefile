# Generated automatically from Makefile.in by configure.
#
######################################################################
#********************************************************************
#* Makefile -- makefile for xacc                                    *
#* Copyright (C) 1997 Robin Clark                                   *
#*                                                                  *
#* This program is free software; you can redistribute it and/or    *
#* modify it under the terms of the GNU General Public License as   *
#* published by the Free Software Foundation; either version 2 of   *
#* the License, or (at your option) any later version.              *
#*                                                                  *
#* This program is distributed in the hope that it will be useful,  *
#* but WITHOUT ANY WARRANTY; without even the implied warranty of   *
#* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
#* GNU General Public License for more details.                     *
#*                                                                  *
#* You should have received a copy of the GNU General Public License*
#* along with this program; if not, write to the Free Software      *
#* Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
#*                                                                  *
#*   Author: Robin Clark                                            *
#* Internet: rclark@rush.aero.org                                   *
#*  Address: 609 8th Street                                         *
#*           Huntington Beach, CA 92648-4632                        *
#********************************************************************

srcdir  = .

PREFIX  = /usr/local
INSTALL = /usr/bin/ginstall -c
INSTALL_DATA = ${INSTALL} -m 644
TARGET  = xacc
DOCDIR	= share/xacc/Docs
CPU     = @target_cpu@

######################################################################
#
# Description of targets:
#
#   default      -- make the application
#   depend       -- generate the dependencies
#   clean        -- remove *.a, *.o, *.bak, and *~
#   distclean    -- get rid of config files too...
#   install      -- installs everything
#


default:
	@cd lib;    $(MAKE)
	@cd src/engine;      $(MAKE)
	@cd src/register;    $(MAKE)
	@cd src;    $(MAKE)

# link in motif libs statically
static:
	@cd lib;    $(MAKE)
	@cd src;    $(MAKE) static

depend:
	@cd lib;    $(MAKE) depend
	@cd src;    $(MAKE) depend

clean:
	rm -f *~ *.o *.bak
	@cd lib;    $(MAKE) clean
	@cd src;    $(MAKE) clean

distclean: clean
	rm -f *~ *.o *.bak Makefile xacc
	rm -f config.cache config.log config.status config.h
	@cd lib;    $(MAKE) distclean
	@cd src;    $(MAKE) distclean

install: $(TARGET)
	@mkdir -p $(PREFIX)/bin
	$(INSTALL) $(TARGET) $(PREFIX)/bin/$(TARGET)
	$(INSTALL) $(TARGET).bin $(PREFIX)/bin/$(TARGET).bin
	@mkdir -p $(PREFIX)/$(DOCDIR)
	$(INSTALL_DATA) Docs/* $(PREFIX)/$(DOCDIR)
