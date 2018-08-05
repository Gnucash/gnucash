#!/bin/bash

#set -e

EXIT_VALUE=0
if [ "x$TEST_PATH" == "x" ] ; then
  TEST_PATH=.
fi
for i in $SRCDIR/test-files/xml2/*.gml2 ; do

  if [ ! -d $i ] ; then
    for j in account commodity transaction ; do

      rm -rf $j
      
      mkdir $j
      FILES=`perl $SRCDIR/grab-types.pl "gnc:$j" $i "$j/dataXXX.xml"`
      if [ ! -z "$FILES" ] ; then
	  if [ "x$VERBOSE" = "xyes" ] ; then
              echo "Testing $TEST_PATH/test-xml-$j $j/data*.xml # from `basename $i`:"
	  fi
        eval "$TEST_PATH/test-xml-$j $FILES 2>/dev/null"
        if [ $? != 0 ] ; then
          EXIT_VALUE=1
        fi
      fi
      rm -rf $j
    done
  fi
done

exit $EXIT_VALUE
