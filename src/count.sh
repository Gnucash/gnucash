#! /bin/sh
#
# Simple stupid utility to count lines of code
# The output may deceive you, remember to subtract 1 from file count

echo 
echo
echo "app utils:"
wc app-file/*.c app-utils/*.c calculation/*.c core-utils/*.c gnc-module/*.c network-utils/*.c tax/us/*.c
wc app-file/*.c app-utils/*.c calculation/*.c core-utils/*.c gnc-module/*.c network-utils/*.c tax/us/*.c | wc
echo 
echo
echo "app utils:"
wc app-file/*.h app-utils/*.h calculation/*.h core-utils/*.h gnc-module/*.h network-utils/*.h tax/us/*.h
wc app-file/*.h app-utils/*.h calculation/*.h core-utils/*.h gnc-module/*.h network-utils/*.h tax/us/*.h | wc
echo 
echo
echo "app utils:"
wc app-file/*.scm app-utils/*.scm calculation/*.scm core-utils/*.scm gnc-module/*.scm network-utils/*.scm tax/us/*.scm
wc app-file/*.scm app-utils/*.scm calculation/*.scm core-utils/*.scm gnc-module/*.scm network-utils/*.scm tax/us/*.scm | wc


echo 
echo
echo "import export:"
wc import-export/*.c import-export/*/*.c
wc import-export/*.c import-export/*/*.c |wc

echo 
echo
echo "import export:"
wc import-export/*.h import-export/*/*.h
wc import-export/*.h import-export/*/*.h | wc

echo 
echo
echo "import export:"
wc import-export/*.scm import-export/*/*.scm
wc import-export/*.scm import-export/*/*.scm | wc

echo 
echo
echo "reports:"
wc report/*/*.c
wc report/*/*.c |wc

echo 
echo
echo "reports:"
wc report/*/*.h
wc report/*/*.h |wc

echo 
echo
echo "reports:"
wc report/*/*.scm
wc report/*/*.scm |wc

echo 
echo
echo "scheme misc:"
wc scm/*.scm scm/*/*.scm
wc scm/*.scm scm/*/*.scm |wc

echo 
echo
echo "Business:"
wc business/*/*.c
wc business/*/*.c |wc

echo 
echo
echo "Business:"
wc business/*/*.h
wc business/*/*.h |wc 

echo 
echo
echo "Business:"
wc business/*/*.scm
wc business/*/*.scm | wc

echo 
echo
echo "test:"
wc test-core/*.c */test/*.c  */*/test/*.c */*/*/test/*.c 
wc test-core/*.c */test/*.c  */*/test/*.c */*/*/test/*.c  |wc

echo 
echo
echo "test:"
wc test-core/*.h */test/*.h  */*/test/*.h */*/*/test/*.h 
wc test-core/*.h */test/*.h  */*/test/*.h */*/*/test/*.h  |wc

echo 
echo
echo "test:"
wc test-core/*.scm */test/*.scm  */*/test/*.scm */*/*/test/*.scm 
wc test-core/*.scm */test/*.scm  */*/test/*.scm */*/*/test/*.scm  |wc

echo 
echo
echo "internal docs"
wc README* */README* */*/README* */*/*/README* *.txt */*.txt */*/*.txt */*/*/*.txt doc/*.html doc/*/*.html doc/*.texinfo doc/*/*.texinfo doc/xml/*.dtd
wc README* */README* */*/README* */*/*/README* *.txt */*.txt */*/*.txt */*/*/*.txt doc/*.html doc/*/*.html doc/*.texinfo doc/*/*.texinfo doc/xml/*.dtd |wc

