#!/bin/sh

#set -e

EXIT_VALUE=0

for i in test-files/xml2/*.gml2 ; do

  if [ ! -d $i ] ; then
    for j in account commodity transaction ; do

      rm -rf $j
      
      mkdir $j
      FILES=`perl grab-types.pl "gnc:$j" $i "$j/dataXXX.xml"`
      if [ ! -z "$FILES" ] ; then
        echo "Testing file $i, section $j"
      
        eval "./test-xml-$j $FILES"
        if [ $? != 0 ] ; then
          EXIT_VALUE=1
        fi
      fi
      rm -rf $j
    done
  fi
done

exit $EXIT_VALUE
