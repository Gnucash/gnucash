# Generated automatically from Makefile.in by configure.
# Makefile -- makefile for xacc/src                                
# Copyright (C) 1997 Robin Clark                                   
# Copyright (C) 1998 Rob Browning <rlb@cs.utexas.edu>              
#                                                                  
# This program is free software; you can redistribute it and/or    
# modify it under the terms of the GNU General Public License as   
# published by the Free Software Foundation; either version 2 of   
# the License, or (at your option) any later version.              
#                                                                  
# This program is distributed in the hope that it will be useful,  
# but WITHOUT ANY WARRANTY; without even the implied warranty of   
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
# GNU General Public License for more details.                     
#                                                                  
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software      
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        
#                                                                  
#   Author: Robin Clark                                            
# Internet: rclark@rush.aero.org                                   
#  Address: 609 8th Street                                         
#           Huntington Beach, CA 92648-4632                        

# These three lines are suggested defs for autoconf (see the info pages).

srcdir   = .

CC       = gcc
INCLPATH = -I.. -I./engine -I./register -I./../include -I/usr/local/include
CFLAGS   =  -O2 -Wall -g -Wall  -I/usr/X11R6/include ${INCLPATH}


######################################################################
# See Makefile.common for information about these variables.
COMMON_SRCS   := Ledger.c SplitLedger.c
CLEAN_SUBDIRS := engine gnome motif register
######################################################################

all: default

# This inclusion must come after the first target, and after the
# definitions of *_SRCS, etc., but before the usage of *_OBJS.
include ../Makefile.common

default: $(OBJS)

motif: ${MOTIF_OBJS}
	@cd engine;            $(MAKE) default
	@cd register;          $(MAKE) motif
	@cd motif;             $(MAKE) motif

gnome: ${GNOME_OBJS}
	@cd engine;            $(MAKE) default
	@cd register;          $(MAKE) gnome
	@cd gnome;               $(MAKE) gnome

.PHONY: default gnome motif all

# Local Variables:
# tab-width: 2
# End:
