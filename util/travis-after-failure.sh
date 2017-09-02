#! /bin/bash

# Build logs are located in different places depending on the build environment used
if [[ "$BUILDTYPE" == "cmake-make" ]] || [[ "$BUILDTYPE" == "cmake-ninja" ]]; then
  echo "##### LastTest.log #####"
  echo "#########################"
  cat /tmp/gnucash-build-${BUILDTYPE}/Testing/Temporary/LastTest.log
elif [[ "$BUILDTYPE" == "autotools" ]]; then
  for logfile in $( find . -name 'test*.log' ); do
    SIZE=$((${#logfile} + 12))
    HRULE=$(head -c $SIZE < /dev/zero | tr '\0' '#')
    echo $HRULE
    echo "##### ${logfile} #####"
    echo $HRULE
    cat -- "$logfile"
    echo -e "\n\n"
  done
fi

