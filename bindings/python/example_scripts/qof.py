#!/usr/bin/env python3

##  @file
#   @brief Some examples for qof queries
#   @author Christoph Holtermann
#   @date 2022-03-06
#   @ingroup python_bindings_examples
#   @note incomplete set of qof examples, to be extended

import datetime
from random import randint
from gnucash import Session, SessionOpenMode, Account, Transaction, Split, \
                    GncNumeric, Query
from gnucash import QOF_COMPARE_GTE, QOF_COMPARE_GT, QOF_QUERY_AND, \
                    QOF_QUERY_OR, QOF_STRING_MATCH_NORMAL, QOF_COMPARE_CONTAINS, \
                    QOF_NUMERIC_MATCH_ANY
from gnucash import gnucash_core_c
from gnucash import gnucash_core

# We need to tell GnuCash the data format to create the new file as (xml://)
uri = "xml:///tmp/qof.gnucash"

def createAccounts(book):
    root_account = book.get_root_account()
    commodtable = book.get_table()
    currency = commodtable.lookup("CURRENCY", "EUR")
    ses.save()

    print('Create two accounts ("Account A", "Account B")')
    accountA = Account(book)
    accountA.SetCommodity(currency)
    accountA.SetName("Account A")
    root_account.append_child(accountA)

    accountB = Account(book)
    accountB.SetCommodity(currency)
    accountB.SetName("Account B")
    root_account.append_child(accountB)

    #ses.save()

    return accountA, accountB

def createRandomTransactions(book, accountA, accountB):
    split = Split(book)

    currency = book.get_table().lookup("CURRENCY", "EUR")

    print("Create 100 random transactions")
    for i in range(100):

        trans = Transaction(book)
        trans.BeginEdit()
        trans.SetCurrency(currency)	
        trans.SetDate(randint(1,28), randint(1,12), randint(1900,2000))
        trans.SetDescription("Transaction "+str(i))

        value = randint(0,10000)

        value1 = GncNumeric(value, 100)
        value2 = GncNumeric(-value, 100)

        split1 = Split(book)
        split1.SetValue(value1)
        split1.SetAccount(accountA)
        split1.SetMemo("A" + str(i))
        split1.SetParent(trans)
       
        split2 = Split(book)
        split2.SetValue(value2)
        split2.SetAccount(accountB)
        split2.SetMemo("B" + str(i))
        split2.SetParent(trans)

        trans.CommitEdit()

def query_transactions(book, terms=[]):

    query = Query()
    query.search_for('Trans')
    query.set_book(book)

    if terms:
        for term in terms:
            query.add_term(*term)

    transactions = []

    for transaction in query.run():
        transaction = Transaction(instance=transaction) # ToDo: query.run() should return objects
        transactions.append(transaction)	

    query.destroy()
    return transactions

def query_splits(book, terms=[]):

    query = Query()
    query.search_for('Split')
    query.set_book(book)

    if terms:
        for term in terms:
            query.add_term(*term)

    splits = []

    for split in query.run():
        split = Split(instance=split) # ToDo: query.run() should return objects
        splits.append(split)	

    query.destroy()
    return splits

with Session(uri, SessionOpenMode.SESSION_NEW_STORE) as ses:

    book = ses.get_book()
    accountA, accountB = createAccounts(book)
    createRandomTransactions(book, accountA, accountB)

    # TRANSACTIONS
    #
    # get all transactions
    transactions_all = query_transactions(book)
    print("Query all: " + str(len(transactions_all)) + " transactions.")

    # query date
    threshold = datetime.datetime(1950,1,1)
    QOF_DATE_MATCH_NORMAL = 2
    terms = [(['date-posted'], gnucash_core.QueryDatePredicate(QOF_COMPARE_GTE, QOF_DATE_MATCH_NORMAL, threshold), QOF_QUERY_AND)]
    transactions_2 = query_transactions(book, terms)
    print("Query transactions with date > 1950: " + str(len(transactions_2)) + " (Should be about 50).")

    # query description
    isRegex = False
    terms = [(['desc'], gnucash_core.QueryStringPredicate(QOF_COMPARE_CONTAINS, "Transaction 5", QOF_STRING_MATCH_NORMAL, isRegex), QOF_QUERY_AND)]
    transactions_3 = query_transactions(book, terms)
    print("Query transaction with description containing 'Transaction 5': " + str(len(transactions_3)) + " (Should be 11).")

    # SPLITS
    #
    # query memo
    isRegex = False
    terms = [(['memo'], gnucash_core.QueryStringPredicate(QOF_COMPARE_CONTAINS, "A22", QOF_STRING_MATCH_NORMAL, isRegex), QOF_QUERY_AND)]
    splits_1 = query_splits(book, terms)
    print("Query splits with memo containing 'A22': " + str(len(splits_1)) + " (Should be 1).")

    # query description
    isRegex = False
    terms = [(['trans', 'desc'], gnucash_core.QueryStringPredicate(QOF_COMPARE_CONTAINS, "Transaction 5", QOF_STRING_MATCH_NORMAL, isRegex), QOF_QUERY_AND)]
    splits_2 = query_splits(book, terms)
    print("Query splits with transaction description containing 'Transaction 5': " + str(len(splits_2)) + " (Should be 22).")

    # query memo and desc
    isRegex = False
    terms = [(['memo'], gnucash_core.QueryStringPredicate(QOF_COMPARE_CONTAINS, "A22", QOF_STRING_MATCH_NORMAL, isRegex), QOF_QUERY_AND)]
    terms += [(['trans', 'desc'], gnucash_core.QueryStringPredicate(QOF_COMPARE_CONTAINS, "Transaction 55", QOF_STRING_MATCH_NORMAL, isRegex), QOF_QUERY_OR)]
    splits_4 = query_splits(book, terms)
    print("Query splits with memo containing 'A22' or transaction desc containing 'Transaction 55': " + str(len(splits_4)) + " (Should be 3).")

    # query split value
    threshold = GncNumeric(5000, 100)
    terms = [(["amount"], gnucash_core.QueryNumericPredicate(QOF_COMPARE_GT, QOF_NUMERIC_MATCH_ANY, threshold), QOF_QUERY_AND)]
    splits_3 = query_splits(book, terms)
    print("Query splits with amount > " + str(threshold) + ": " + str(len(splits_3)) + " (Should be about 100).")
