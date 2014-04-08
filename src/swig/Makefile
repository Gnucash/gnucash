

# FIXME: This Makefile is crap.  Switch to Makefile.in, and clean this
# up...

SWIG_FILTER    := %P.h %/util.h
SWIG_HDRS      := $(filter-out ${SWIG_FILTER},$(wildcard ../engine/*.h)) \
                  $(filter-out ${SWIG_FILTER},$(wildcard ../guile/gnucash.h))

# Because Swig's include mechanism is not too smart.
SWIG_BASENAMES := $(notdir ${SWIG_HDRS})

CC := gcc

CFLAGS := -I.. \
          -I../.. \
          -I../engine \
          -I../register \
          -I../guile \
          -I./../../include \
          -I/usr/include/readline \
          -I/usr/lib/perl5/i386-linux/5.004/CORE

CFLAGS  += -O2 -Wall -Wno-unused

LIBENG  := ../engine/obj/*.o ../gnome/obj/gnome/*.o

all: guile perl5

# This is .PHONY because it has to be re-generated *every* time.  Who
# knows when headers are added to the engine dir?
gnucash.i:
	@echo "%module gnucash" > gnucash.i
	@echo "%{" >> gnucash.i
	@($(foreach hdr,${SWIG_BASENAMES},echo "#include <${hdr}>"; )) >> gnucash.i
	@echo "%}" >> gnucash.i
	@($(foreach hdr,${SWIG_BASENAMES},echo %include ${hdr}; )) >> gnucash.i
.PHONY: gnucash.i
TRASH += gnucash.i

%_wrap.c: gnucash.i

##### Guile

guile: guile/gnucash-guile_wrap.o
.PHONY: guile

##### Guile

gnucash-guile: gnucash-guile.o gnucash-guile_wrap.o $(LIBENG) 

guile/gnucash-guile_wrap.c: gnucash.i ${SWIG_HDRS}
	swig -I../engine -I../guile -guile -o $@ $< 
	perl -pi -e 's/^void gnucash\(/void gnucash_swig_init\(/' \
       guile/gnucash-guile_wrap.c

guile/gnucash-guile: guile/gnucash-guile.o guile/gnucash-guile_wrap.o $(LIBENG) 
	${CC} $^  -lguile -lqthreads -lreadline -lm -o $@
TRASH += guile/gnucash-guile

##### Perl5

perl5: perl5/gnucash.so

perl5/gnucash.so: perl5/gnucash-perl5_wrap.c $(LIBENG) 
	${CC} ${CFLAGS} -shared $^ -lm -o $@ -Dbool=char -fpic
TRASH += perl5/gnucash.so

perl5/gnucash-perl5_wrap.c: gnucash.i ${SWIG_HDRS}
	swig -I../engine -perl5 -o $@ $< 

clean:
	rm -f *.o *~ *.bak *.i ${TRASH}
	(cd guile && rm -f *.o *~ *.bak *_wrap.c *.doc)
	(cd perl5 && rm -f *.o *~ *.bak *_wrap.c *.doc *.pm)

# Local Variables:
# tab-width: 2
# End:

# DO NOT DELETE THIS LINE -- make depend depends on it.
