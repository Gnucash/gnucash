#!/usr/bin/env python

# str_methods.py -- Add __str__ and __unicode__ methods to financial objects
#
 
##  @file
#   @brief Add __str__ and __unicode__ methods to financial objects so that @code print object @endcode leads to human readable results
#   @author Christoph Holtermann, c.holtermann@gmx.de
#   @ingroup python_bindings_examples

import gnucash

def __split__unicode__(self):
  """__unicode__ method for split class"""
  from gnucash import Split
  import time
  self=Split(instance=self)

  lot=gnucash.GncLot(instance=self.GetLot())
  if lot:
    lot_str=lot.get_title()
  else:
    lot_str='---'

  transaction=self.GetParent()

  return u"{0:7}{1:20}{2:7}{3:7}{4:7}{5:30}{6:12}{7:15}{8:1}{9:30}{10:5}{11:10}{12:1}".format(
    'Konto:',self.GetAccount().name,
    'Value:',unicode(self.GetValue()),
    'Memo:',self.GetMemo(),
    'Transaction:',time.ctime(transaction.GetDate()),'-',transaction.GetDescription(),
    'Lot: ',lot_str,'\n')

def __split__str__(self):
  """__str__ method for split class"""
  
  from gnucash import Split
  self=Split(instance=self)

  return unicode(self).encode('utf-8')

gnucash.gnucash_core_c.__split__str__=__split__str__
gnucash.Split.add_method("__split__str__","__str__")

gnucash.gnucash_core_c.__split__unicode__=__split__unicode__
gnucash.Split.add_method("__split__unicode__","__str__")

def __transaction__str__(self):
  """__str__ method for Transaction class"""
  from gnucash import Transaction
  import time
  self=Transaction(instance=self)

  transaction_str = u"{0:7}{1:25}{2:14}{3:40}{4:7}{5:40}{6:1}".format(
      'Datum:',str(time.ctime(self.GetDate())),
      'Description:',str(self.GetDescription()),
      'Notes:',str(self.GetNotes()),'\n')

  splits_str=""

  splitlist = self.GetSplitList()
  for n,split in enumerate(splitlist):
    if not type(split)==gnucash.Split:
      split=gnucash.Split(instance=split)
    splits_str += u"[{0:>2}] ".format(str(n))
    splits_str += unicode(split)

  return transaction_str + splits_str

def __transaction__unicode__(self):
  """__unicode__ method for Transaction class"""
  from gnucash import Transaction

  self=Transaction(instance=self)
  return unicode(self).encode('utf-8')

# These lines add transaction_str as method __str__ to Transaction object
gnucash.gnucash_core_c.__transaction__str__=__transaction__str__
gnucash.Transaction.add_method("__transaction__str__","__str__")

gnucash.gnucash_core_c.__transaction__unicode__=__transaction__unicode__
gnucash.Transaction.add_method("__transaction__unicode__","__unicode__")
