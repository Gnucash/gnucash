# Generated automatically from Makefile.in by configure.
# Makefile -- makefile for xacc/src/register                       
# Copyright (C) 1997 Robin Clark                                   
# Copyright (C) 1998 Linas Vepstas                                 
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

srcdir   = .
CC       = gcc
RANLIB   = ranlib
INCLPATH = -I./../.. \
           -I./../../include \
           -I./../engine \
           -I./../../lib/ComboBox-1.33 \
           -I./../../lib/Xbae-4.6.2-linas 

CFLAGS :=  -O2 -Wall -g -Wall  -DCELL_WIDGETS=1 ${INCLPATH}

######################################################################
# See Makefile.common for information about these variables.
COMMON_SRCS := basiccell.c cellblock.c \
	       datecell.c pricecell.c QuickFill.c quickfillcell.c \
	       recncell.c register.c table-allgui.c textcell.c
MOTIF_SRCS := table-motif.c combocell-motif.c
GNOME_SRCS := table-gtk.c combocell-gtk.c
######################################################################

all:
	@echo " "
	@echo "Please choose one of the following targets:"
	@echo "motif           dynamically linked motif version"
	@echo "gnome           gnome/gtk version"
	@echo " "

# This inclusion must come after the first target, and after the
# definitions of *_SRCS, etc., but before the usage of *_OBJS.
include ../../Makefile.common

motif: ${MOTIF_OBJS}

gnome: ${GNOME_OBJS}

.PHONY: all motif gnome

# Local Variables:
# tab-width: 2
# End:
