#!/usr/bin/env python

# str_methods.py -- Add __str__ and __unicode__ methods to financial objects
#
 
##  @file
#   @brief Add __str__ and __unicode__ methods to financial objects so that @code print object @endcode leads to human readable results
#   @author Christoph Holtermann, c.holtermann@gmx.de
#   @ingroup python_bindings_examples
#   
#   Import this module and str(Object) and unicode(Object) where Object is Transaction or Split leads
#   to human readable results. That is handy when using @code print object @endcode
#
#   I chose to put these functions/methods in a seperate file to develop them like this and maybe if
#   they prove to be useful they can be put in gnucash_core.py
#
#   This is written as a first approach to a shell-environment using ipython to interactively manipulate
#   GnuCashs Data.
#

import gnucash

class ClassWithCutting__format__():
    def __init__(self,value):
      self.value = value

    def __format__(self, fmt):
        def get_width(fmt_spec):
            """Parse fmt_spec to obtain width"""

            def remove_alignment(fmt_spec):
                if fmt_spec[1] in ["<","^",">"]:
                    fmt_spec=fmt_spec[2:len(fmt_spec)]
                return fmt_spec

            def remove_sign(fmt_spec):
                if fmt_spec[0] in ["-","+"," "]:
                    fmt_spec=fmt_spec[1:len(fmt_spec)]
                return fmt_spec

            def remove_cross(fmt_spec):
                if fmt_spec[0] in ["#"]:
                    fmt_spec=fmt_spec[1:len(fmt_spec)]
                return fmt_spec

            def do_width(fmt_spec):
                n=""
                
                while len(fmt_spec)>0:
                    if fmt_spec[0].isdigit():
                      n+=fmt_spec[0]
                      fmt_spec=fmt_spec[1:len(fmt_spec)]
                    else:
                        break
                if n:
                    return int(n)
                else:
                    return None

            if len(fmt_spec)>=2:
                fmt_spec=remove_alignment(fmt_spec)
            if len(fmt_spec)>=1:
               fmt_spec=remove_sign(fmt_spec)
            if len(fmt_spec)>=1:
                fmt_spec=remove_cross(fmt_spec)
            width=do_width(fmt_spec)
            # Stop parsing here for we only need width

            return width

        def cut(s, width, replace_string="..."):
            """Cuts s to width and puts replace_string at it's end."""
            
            #s=s.decode('UTF-8', "replace")
            
            if len(s)>width:
                if len(replace_string)>width:
                    replace_string=replace_string[0:width]
                s=s[0:width-len(replace_string)]
                s=s+replace_string
            
            return s
     
        value=self.value

        # Replace Tabs and linebreaks
        import types
        if type(value) in [types.StringType, types.UnicodeType]:
            value=value.replace("\t","|")
            value=value.replace("\n","|")
        
        # Do regular formatting of object 
        value=value.__format__(fmt)

        # Cut resulting value if longer than specified by width
        width=get_width(fmt)
        if width:
            value=cut(value, width, "...")

        return value

def all_as_ClassWithCutting__format__(*args):
    """Converts every argument to instance of ClassWithCutting__format__"""

    import types
    l=[]
    for a in args:
        if type(a) in [types.StringType, types.UnicodeType]:
          a=a.decode("UTF-8")
        l.append(ClassWithCutting__format__(a))

    return l

def all_as_ClassWithCutting__format__keys(**keys):
    """Converts every argument to instance of ClassWithCutting__format__"""

    import types
    d={}
    for a in keys:
        if type(keys[a]) in [types.StringType, types.UnicodeType]:
          keys[a]=keys[a].decode("UTF-8")
        d[a]=ClassWithCutting__format__(keys[a])

    return d

def __split__unicode__(self):
    """__unicode__ method for Split"""

    from gnucash import Split
    import time
    self=Split(instance=self)

    lot=gnucash.GncLot(instance=self.GetLot())
    if lot:
        lot_str=lot.get_title()
    else:
        lot_str='---'

    transaction=self.GetParent()
   
    # This dict and the return statement can be changed according to individual needs 
    fmt_dict={
        "account_name":'Account:',
        "account_value":self.GetAccount().name,
        "value_name":'Value:',
        "value_value":self.GetValue(),
        "memo_name":'Memo:',
        "memo_value":self.GetMemo(),
        "transaction_name1":'Transaction:',
        "transaction_value1":time.ctime(transaction.GetDate()),
        "transaction_name2":u'-',
        "transaction_value2":transaction.GetDescription(),
        "lot_name":'Lot: ',
        "lot_value":lot_str}

    return (u"{account_name:8}{account_value:20} "+
            u"{value_name:7}{value_value:>10} "+
            u"{memo_name:7}{memo_value:30} "+
            u"{transaction_name1:12}{transaction_value1:15} "+
            u"{transaction_name2:1}{transaction_value2:30} "+
            u"{lot_name:5}{lot_value:10}").\
                format(**all_as_ClassWithCutting__format__keys(**fmt_dict))

def __split__str__(self):
    """__str__ method for split class"""
    
    from gnucash import Split
    self=Split(instance=self)

    return unicode(self).encode('utf-8')

gnucash.gnucash_core_c.__split__str__=__split__str__
gnucash.Split.add_method("__split__str__","__str__")

gnucash.gnucash_core_c.__split__unicode__=__split__unicode__
gnucash.Split.add_method("__split__unicode__","__unicode__")

def __transaction__unicode__(self):
    """__unicode__ method for Transaction class"""
    from gnucash import Transaction
    import time
    self=Transaction(instance=self)

    fmt_tuple=('Date:',str(time.ctime(self.GetDate())),
          'Description:',str(self.GetDescription()),
          'Notes:',str(self.GetNotes()))

    transaction_str = u"{0:6}{1:25} {2:14}{3:40} {4:7}{5:40}".format(
          *all_as_ClassWithCutting__format__(*fmt_tuple))

    splits_str=""
    for n,split in enumerate(self.GetSplitList()):
        if not type(split)==gnucash.Split:
            split=gnucash.Split(instance=split)
        splits_str += u"[{0:>2}] ".format(str(n))
        splits_str += unicode(split)

    return transaction_str + splits_str

def __transaction__str__(self):
    """__str__ method for Transaction class"""
    from gnucash import Transaction

    self=Transaction(instance=self)
    return unicode(self).encode('utf-8')

# These lines add transaction_str as method __str__ to Transaction object
gnucash.gnucash_core_c.__transaction__str__=__transaction__str__
gnucash.Transaction.add_method("__transaction__str__","__str__")

gnucash.gnucash_core_c.__transaction__unicode__=__transaction__unicode__
gnucash.Transaction.add_method("__transaction__unicode__","__unicode__")
