#! /bin/sh
#
# Simple stupid utility to count lines of code
# The output may deceive you, remember to subtract 1 from file count

#
# FIXME This script is very out of date. It doesn't count a number
#       of source directories (such as gnome, ledger, engine,...)
#

SRC_DIR=$(dirname $0)/../../src

cd $SRC_DIR

echo 
echo
echo "app utils:"
wc $(find app-utils core-utils calculation gnc-module tax \
            \( -regex '.*test[^/]*' -prune \) -o \
            \( -path '*.svn' -prune \)        -o \
            \( \( -name '*.c'         -o \
                  -name '*.h'         -o \
                  -name '*.scm' \)        -a \
               -print \) | sort )
wc $(find app-utils core-utils calculation gnc-module tax \
            \( -regex '.*test[^/]*' -prune \) -o \
            \( -path '*.svn' -prune \)        -o \
            \( \( -name '*.c'         -o \
                  -name '*.h'         -o \
                  -name '*.scm' \)        -a \
               -print \) | sort ) | wc

echo
echo
echo "import export:"
wc $(find import-export \
            \( -regex '.*test[^/]*' -prune \) -o \
            \( -path '*.svn' -prune \)        -o \
            \( \( -name '*.c'         -o \
                  -name '*.h'         -o \
                  -name '*.scm' \)        -a \
               -print \) | sort )
wc $(find import-export \
            \( -regex '.*test[^/]*' -prune \) -o \
            \( -path '*.svn' -prune \)        -o \
            \( \( -name '*.c'         -o \
                  -name '*.h'         -o \
                  -name '*.scm' \)        -a \
               -print \) | sort ) | wc

echo
echo
echo "reports:"
wc $(find report \
            \( -regex '.*test[^/]*' -prune \) -o \
            \( -path '*.svn' -prune \)        -o \
            \( \( -name '*.c'         -o \
                  -name '*.h'         -o \
                  -name '*.scm' \)        -a \
               -print \) | sort )
wc $(find report \
            \( -regex '.*test[^/]*' -prune \) -o \
            \( -path '*.svn' -prune \)        -o \
            \( \( -name '*.c'         -o \
                  -name '*.h'         -o \
                  -name '*.scm' \)        -a \
               -print \) | sort ) | wc

echo
echo
echo "scheme misc:"
wc $(find scm \
            \( -regex '.*test[^/]*' -prune \) -o \
            \( -path '*.svn' -prune \)        -o \
            \( \( -name '*.c'         -o \
                  -name '*.h'         -o \
                  -name '*.scm' \)        -a \
               -print \) | sort )
wc $(find scm \
            \( -regex '.*test[^/]*' -prune \) -o \
            \( -path '*.svn' -prune \)        -o \
            \( \( -name '*.c'         -o \
                  -name '*.h'         -o \
                  -name '*.scm' \)        -a \
               -print \) | sort ) |wc

echo
echo
echo "Business:"
wc $(find business \
            \( -regex '.*test[^/]*' -prune \) -o \
            \( -path '*.svn' -prune \)        -o \
            \( \( -name '*.c'         -o \
                  -name '*.h'         -o \
                  -name '*.scm' \)        -a \
               -print \) | sort )
wc $(find business \
            \( -regex '.*test[^/]*' -prune \) -o \
            \( -path '*.svn' -prune \)        -o \
            \( \( -name '*.c'         -o \
                  -name '*.h'         -o \
                  -name '*.scm' \)        -a \
               -print \) | sort ) | wc

echo
echo
echo "test:"
wc $(find . \( -path '*.svn' -prune \)        -o \
            \( -regex '.*/test.*/.*'      -a \
               \( -name '*.c'         -o \
                  -name '*.h'         -o \
                  -name '*.scm' \)        -a \
               -print \) | sort )
wc $(find . \( -path '*.svn' -prune \)        -o \
            \( -regex '.*/test.*/.*'      -a \
               \( -name '*.c'         -o \
                  -name '*.h'         -o \
                  -name '*.scm' \)        -a \
               -print \) | sort ) | wc

echo
echo
echo "internal docs"
wc $(find .. \( -path '*.svn' -prune \) -o \
            \( -name 'README*'             -o \
               -name '*.txt'               -o \
               -name '*.html'              -o \
               -name '*.texinfo'           -o \
               -name '*.dtd' \)                -a \
             -print | sort )
wc $(find .. \( -path '*.svn' -prune \) -o \
            \( -name 'README*'             -o \
               -name '*.txt'               -o \
               -name '*.html'              -o \
               -name '*.texinfo'           -o \
               -name '*.dtd' \)                -a \
             -print | sort ) | wc

cd -