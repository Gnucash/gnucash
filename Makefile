######################################################################
#********************************************************************
#* Makefile -- makefile for xacc (X-Accountant)                     *
#* Copyright (C) 1997 Robin D. Clark                                *
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
#*   Author: Rob Clark                                              *
#* Internet: rclark@cs.hmc.edu                                      *
#*  Address: 609 8th Street                                         *
#*           Huntington Beach, CA 92648-4632                        *
#********************************************************************


######################################################################
# CONFIGURABLE STUFF:                                                #
CC = cc
AR = ar r
RANLIB = ranlib

# USE_NO_COLOR - don't use red/black colors to denote neg/positive
#                balances, but instead display a signed number in
#                the balance field
# USEQUICKFILL - comment out if you get a compile error about
#                XbaeMatrixSetCursorPosition
# HYPER_HELP   - include hyper-text help system
# DEBUGMEMORY  - does some accounting whenever malloc/free
#                is called.
# USEDEBUG     - causes debugging info to be displayed
CFLAGS = $(LFLAGS) -I../include -I../lib/libhtmlw -I../lib/Xbae-4.6.2-linas \
         -I ../lib/ComboBox-1.33 -I/usr/local/include  -DMOTIF1_2 \


	 # -DDEBUGMEMORY -DUSEDEBUG
	 # -DUSEQUICKFILL # -DUSE_NO_COLOR -DDEBUGMEMORY -DUSEDEBUG
LFLAGS = -g -L/usr/local/lib -L/usr/X11/lib -L../lib
LIBS   = -lXm -lXmu -lXt -lXpm -lXext -lSM -lICE -lX11 
# LIBS   = -lXm -lXmu -lXt -lXpm -lXext -lSM -lICE -lX11 -lefence

######################################################################

######################################################################
# DO NOT EDIT THE STUFF BELOW THIS LINE!                             #

OPTIONS = "CC = $(CC)"           "LFLAGS = $(LFLAGS)" \
          "CFLAGS = $(CFLAGS)"   "LIBS = $(LIBS)"     \
          "RANLIB = $(RANLIB)"   "AR = $(AR)"

default :
	@cd lib/ComboBox-1.33 ; $(MAKE) 
	@cd lib/Xbae-4.6.2-linas ; $(MAKE) 
	@cd lib/libhtmlw ; $(MAKE) $(OPTIONS)
	@cd src ; $(MAKE) $(OPTIONS)

clean :
	rm -f core junk tmp *~ *.bak
	@cd include ; rm -f *~
	@cd help    ; rm -f *~
	@cd libhtmlw ; $(MAKE) clean
	@cd src ; $(MAKE) clean
	@cd ComboBox-1.33 ; $(MAKE) clean
	@cd Xbae-4.6.2-linas ; $(MAKE) clean

really_clean : clean
	@cd src ; $(MAKE) really_clean
	@cd libhtmlw ; $(MAKE) really_clean

realclean: really_clean

depend :
	@cd src ; $(MAKE) depend $(OPTIONS)

