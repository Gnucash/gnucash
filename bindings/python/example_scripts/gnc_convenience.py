#!/usr/bin/python
# -*- coding: UTF-8 -*-

##@file
# @ingroup python_bindings_examples
# @author Christoph Holtermann (c.holtermann@gmx.de)
# @date May 2011
# @brief some functions to make life easier when using python-bindings
#

from gnucash import Session, Account, Transaction, Split
import gnucash


def get_transaction_list(account):
    """Returns all transactions in account.

    Splits are derived from account.GetSplitList().
   
    options:

    account:    Account to get transactions from.
    
    """
    
    split_list=account.GetSplitList()
    transaction_list=[]
    for split in split_list:
        if type(split) != Split:
              split = Split(instance=split)
        transaction=split.GetParent()
        if not (transaction in transaction_list):       # this check may not be necessary.
          transaction_list.append(transaction)
    return transaction_list


def get_splits_without_lot(account=None,split_list=None):
  """Returns a list of those Splits in split_list or account which do not have an according lot.

  options:

  account:      (optional) Account to search in.
  split_list:   (optional) List of Splits to search in.

  one or the other has to be provided.

  """
  if split_list==None:
      if account==None:
          return []
      else:
          split_list=account.GetSplitList()
  
  rlist=[]
  for split in split_list:
      if type(split).__name__ == 'SwigPyObject':
          split = Split(instance=split) 
      lot=split.GetLot()
      if lot == None:
          rlist.append(split)
  return rlist


def find_account(account,name,account_list=None):
  """Recursively searches full names of account and descendents

  returns a list of accounts which contain name.
  
  options:
  
  account:      account to start search in.
  name:         name to search for.
  account_list: (optional) list to append accounts to.
  
  """

  if not account_list:
    account_list=[]

  for child in account.get_children():
    if type(child) != Account:
      child=Account(instance=child)
    account_list=find_account(child,name,account_list)
  
  account_name=account.GetName()
  if name in account_name:
    account_list.append(account)
  
  return account_list


def find_lot(lot_list,search_string):
  """Searches lots in lot_list for search_string.

  returns list of lots where title contains search_string.

  options:

  lot_list:       List of Lots to search in.
  search_string:  String to search for.
  
  """
  
  rlist=[]
  for lot in lot_list:
    if type(lot).__name__ == 'SwigPyObject':
        lot = gnucash.GncLot(instance=lot)
    ltitle=lot.get_title()
    if search_string in ltitle: 
      rlist.append(lot)
  return rlist


def find_split(split_list,search_string):
  """Searches a list of splits for search_string
  
  returns a list of splits that have search_string as part of
  memo or
  description of parent transaction.
  
  options:
 
  split_list:     List of Splits to search in.
  search_string:  String to search for.
  
  """
  
  rlist=[]
  for split in split_list:
      memo=split.GetMemo()
      transaction_description=split.GetParent().GetDescription()
      if (search_string in memo) or (search_string in transaction_description):
          rlist.append(split)
  return rlist


def find_split_recursive(account, search_string):
  """Searches account and descendants for Splits containing search_string
  
  returns a list of splits that have search_string as part of
  memo or
  description of parent transaction.
  
  options:
 
  account:        Account to search in.
  search_string:  String to search for.
  
  """
  
  rlist = []
  child_account_splits = []
  
  # Get all splits in descendants
  for child in account.get_children():
      if type(child) != Account:
          child = Account(instance=child)
      childsplits = find_split_recursive(child, search_string)
      for split in childsplits:
          if type(split) != Split:
              split = Split(instance=split)
      child_account_splits += childsplits

  # Get all splits in account
  splits=account.GetSplitList()
  for split in splits:
      if type(split) != Split:
          split = Split(instance=split)
  basic_account_splits=find_split(splits,search_string)

  rlist=child_account_splits+basic_account_splits
  return rlist


def find_transaction(account,name,ignore_case=True,transaction_list=None):
  """Searches the transactions of an account for name.
  
  Searches in description and in memo of each split.
  returns a list of transactions that match criteria.
  
  options:
  
  account:          Account to search in.
  name:             String to search for.
  ignore_case:      (optional, default=True) Ignore case if True.
  transaction_list: (optional) list of transactions to search in.
  
  """

  if not transaction_list:
      transaction_list=get_transaction_list(account)

  ret = []
  if ignore_case:
      name=name.lower()

  for transaction in transaction_list:
      found = False
      
      desc=transaction.GetDescription()
      if ignore_case:
          desc=desc.lower()
      
      if name in desc:
          found=True
      
      sl=transaction.GetSplitList()
      for split in sl:
          if type(split) != Split:
              split=Split(instance=split)
          
          memo = split.GetMemo()
          if ignore_case:
              memo=memo.lower()

          if name in memo:
              found=True

      if found:
          ret.append(transaction)

  return ret
