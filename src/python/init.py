import sys
import _sw_app_utils
from gnucash import *

print "Hello from python!"

print "test", sys.modules.keys()
print "test2", dir(_sw_app_utils)

root = _sw_app_utils.gnc_get_current_root_account()

print "test", dir(root), root.__class__
print "test2", dir(gnucash_core_c)

acct = Account(instance = root)

print "test3", dir(acct)
#print acct.GetName()
#print acct.GetBalance()
#print acct.GetSplitList()

#print "test2", dir(gnucash.gnucash_core_c)

