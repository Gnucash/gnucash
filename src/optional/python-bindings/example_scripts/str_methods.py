#!/usr/bin/env python
## @file @brief Add __str__ and __unicode__ methods to financial objects so that @code print object @endcode leads to human readable results
""" @package str_methods.py -- Add __str__ and __unicode__ methods to financial objects

   Import this module and str(Object) and unicode(Object) where Object is Transaction, Split,Invoice
   or Entry leads to human readable results. That is handy when using @code print object @endcode

   I chose to put these functions/methods in a seperate file to develop them like this and maybe if
   they prove to be useful they can be put in gnucash_core.py.

   I am searching to find the best way to serialize these complex objects. Ideally this serialization
   would be configurable.

   If someone has suggestions how to beautify, purify or improve this code in any way please feel
   free to do so.

   This is written as a first approach to a shell-environment using ipython to interactively manipulate
   GnuCashs Data."""

#   @author Christoph Holtermann, c.holtermann@gmx.de
#   @ingroup python_bindings_examples
#   @date May 2011
#   
#   ToDo :
#
#   * Testing for SWIGtypes
#   * dealing the cutting format in a bit more elegant way
#   * having setflag as a classmethod makes it probably impossible to have flags on instance level. Would changing that be useful ?
#   * It seems useful to have an object for each modification. That is because there is some Initialisation to be done.
#

import gnucash, function_class

# Default values for encoding of strings in GnuCashs Database
DEFAULT_ENCODING = "UTF-8"
DEFAULT_ERROR = "ignore"

def setflag(self, name, value):
    if not(name in self.OPTIONFLAGS_BY_NAME):
      self.register_optionflag(name)
    if value == True:
      self.optionflags |= self.OPTIONFLAGS_BY_NAME[name]
    else:
      self.optionflags &= ~self.OPTIONFLAGS_BY_NAME[name]

def getflag(self, name):
    if not(name in self.OPTIONFLAGS_BY_NAME):
      raise KeyError(str(name)+" is not a registered key.")
    return ((self.optionflags & self.OPTIONFLAGS_BY_NAME[name]) != 0)

def register_optionflag(self,name):
    """Taken from doctest.py"""
    # Create a new flag unless `name` is already known.
    return self.OPTIONFLAGS_BY_NAME.setdefault(name, 1 << len(self.OPTIONFLAGS_BY_NAME))

def ya_add_method(_class, function, method_name=None, clsmethod=False, noinstance=False):
    """Calls add_method from function_methods.py but makes it
    possible to use functions in this module. Also keeps the
    docstring"""

    if method_name == None:
      method_name = function.__name__
    
    setattr(gnucash.gnucash_core_c,function.__name__,function)
    if clsmethod:
      mf=_class.ya_add_classmethod(function.__name__,method_name)
    elif noinstance: 
      mf=_class.add_method(function.__name__,method_name)
    else:
      mf=_class.ya_add_method(function.__name__,method_name)
    if function.__doc__ != None:
      setattr(mf, "__doc__", function.__doc__)

def infect(_class, function, method_name):
    if not getattr(_class, "OPTIONFLAGS_BY_NAME", None):    
      _class.OPTIONFLAGS_BY_NAME={}
      _class.optionflags=0
      ya_add_method(_class,register_optionflag,clsmethod=True)
      ya_add_method(_class,setflag,clsmethod=True)
      ya_add_method(_class,getflag,clsmethod=True)
    ya_add_method(_class, function, method_name)

class ClassWithCutting__format__():
    """This class provides a __format__ method which cuts values to a certain width.
    
    If width is too big '...' will be put at the end of the resulting string."""

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

def all_as_classwithcutting__format__(*args):
    """Converts every argument to instance of ClassWithCutting__format__"""

    import types
    l=[]
    for a in args:
        if type(a) in [types.StringType, types.UnicodeType]:
          a=a.decode("UTF-8")
        l.append(ClassWithCutting__format__(a))

    return l

def all_as_classwithcutting__format__keys(encoding=None, error=None, **keys):
    """Converts every argument to instance of ClassWithCutting__format__"""

    import types
    d={}
    if encoding==None:
      encoding=DEFAULT_ENCODING
    if error==None:
      error=DEFAULT_ERROR
    for a in keys:
        if type(keys[a]) in [types.StringType, types.UnicodeType]:
          keys[a]=keys[a].decode(encoding,error)
        d[a]=ClassWithCutting__format__(keys[a])

    return d



# Split
def __split__unicode__(self, encoding=None, error=None):
    """__unicode__(self, encoding=None, error=None) -> object
    
    Serialize the Split object and return as a new Unicode object.
    
    Keyword arguments:
    encoding -- defaults to str_methods.default_encoding
    error -- defaults to str_methods.default_error
    See help(unicode) for more details or http://docs.python.org/howto/unicode.html.

    """

    from gnucash import Split
    import time
    #self=Split(instance=self)

    lot=self.GetLot()
    if lot:
        if type(lot).__name__ == 'SwigPyObject':  
          lot=gnucash.GncLot(instance=lot)
        lot_str=lot.get_title()
    else:
        lot_str='---'

    transaction=self.GetParent()
   
    # This dict and the return statement can be changed according to individual needs 
    fmt_dict={
        "account":self.GetAccount().name,
        "value":self.GetValue(),
        "memo":self.GetMemo(),
        "lot":lot_str}
        
    fmt_str= (u"Account: {account:20} "+
            u"Value: {value:>10} "+
            u"Memo: {memo:30} ")
    
    if self.optionflags & self.OPTIONFLAGS_BY_NAME["PRINT_TRANSACTION"]:
        fmt_t_dict={      
            "transaction_time":time.ctime(transaction.GetDate()),
            "transaction2":transaction.GetDescription()}
        fmt_t_str=(
            u"Transaction: {transaction_time:30} "+
            u"- {transaction2:30} "+
            u"Lot: {lot:10}")
        fmt_dict.update(fmt_t_dict)
        fmt_str += fmt_t_str 
                
    return fmt_str.format(**all_as_classwithcutting__format__keys(encoding,error,**fmt_dict))

def __split__str__(self):
    """Returns a bytestring representation of self.__unicode__"""
    
    from gnucash import Split
    #self=Split(instance=self)

    return unicode(self).encode('utf-8')

# This could be something like an __init__. Maybe we could call it virus because it infects the Split object which
# thereafter mutates to have better capabilities.
infect(gnucash.Split,__split__str__,"__str__")
infect(gnucash.Split,__split__unicode__,"__unicode__")
gnucash.Split.register_optionflag("PRINT_TRANSACTION")
gnucash.Split.setflag("PRINT_TRANSACTION",True)

def __transaction__unicode__(self):
    """__unicode__ method for Transaction class"""
    from gnucash import Transaction
    import time
    self=Transaction(instance=self)

    fmt_tuple=('Date:',time.ctime(self.GetDate()),
          'Description:',self.GetDescription(),
          'Notes:',self.GetNotes())

    transaction_str = u"{0:6}{1:25} {2:14}{3:40} {4:7}{5:40}".format(
          *all_as_classwithcutting__format__(*fmt_tuple))
    transaction_str += "\n"

    splits_str=""
    for n,split in enumerate(self.GetSplitList()):
        if not (type(split)==gnucash.Split):
            split=gnucash.Split(instance=split)

        transaction_flag = split.getflag("PRINT_TRANSACTION")
        split.setflag("PRINT_TRANSACTION",False)
        splits_str += u"[{0:>2}] ".format(unicode(n))
        splits_str += unicode(split)
        splits_str += "\n"
        split.setflag("PRINT_TRANSACTION",transaction_flag)

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

def __invoice__unicode__(self):
    """__unicode__ method for Invoice"""

    from gnucash.gnucash_business import Invoice
    self=Invoice(instance=self)

   
    # This dict and the return statement can be changed according to individual needs 
    fmt_dict={
        "id_name":"ID:",
        "id_value":self.GetID(),
        "notes_name":"Notes:",
        "notes_value":self.GetNotes(),
        "active_name":"Active:",
        "active_value":str(self.GetActive()),
        "owner_name":"Owner Name:",
        "owner_value":self.GetOwner().GetName(),
        "total_name":"Total:",
        "total_value":str(self.GetTotal()),
        "currency_mnemonic":self.GetCurrency().get_mnemonic()}

    ret_invoice= (u"{id_name:4}{id_value:10} {notes_name:7}{notes_value:20} {active_name:8}{active_value:7} {owner_name:12}{owner_value:20}"+
                  u"{total_name:8}{total_value:10}{currency_mnemonic:3}").\
                    format(**all_as_classwithcutting__format__keys(**fmt_dict))

    ret_entries=u""
    entry_list = self.GetEntries()
    for entry in entry_list: # Type of entry has to be checked
      if not(type(entry)==Entry):
        entry=Entry(instance=entry)
      ret_entries += "  "+unicode(entry)+"\n"
    
    return ret_invoice+"\n"+ret_entries
  
def __invoice__str__(self):
    """__str__ method for invoice class"""
    
    from gnucash.gnucash_business import Invoice
    self=Invoice(instance=self)

    return unicode(self).encode('utf-8')

from gnucash.gnucash_business import Invoice

gnucash.gnucash_core_c.__invoice__str__=__invoice__str__
gnucash.gnucash_business.Invoice.add_method("__invoice__str__","__str__")

gnucash.gnucash_core_c.__invoice__unicode__=__invoice__unicode__
gnucash.gnucash_business.Invoice.add_method("__invoice__unicode__","__unicode__")

def __entry__unicode__(self):
    """__unicode__ method for Entry"""

    from gnucash.gnucash_business import Entry
    self=Entry(instance=self)

    # This dict and the return statement can be changed according to individual needs 
    fmt_dict={
        "date_name":"Date:",
        "date_value":unicode(self.GetDate()),
        "description_name":"Description:",
        "description_value":self.GetDescription(),
        "notes_name":"Notes:",
        "notes_value":self.GetNotes(),
        "quant_name":"Quantity:",
        "quant_value":unicode(gnucash.GncNumeric(instance=self.GetQuantity())),
        "invprice_name":"InvPrice:",
        "invprice_value":unicode(gnucash.GncNumeric(instance=self.GetInvPrice()))}

    return (u"{date_name:6}{date_value:15} {description_name:13}{description_value:20} {notes_name:7}{notes_value:20}"+
            u"{quant_name:12}{quant_value:7} {invprice_name:10}{invprice_value:7}").\
                format(**all_as_classwithcutting__format__keys(**fmt_dict))

def __entry__str__(self):
    """__str__ method for Entry class"""
    
    from gnucash.gnucash_business import Entry
    self=Entry(instance=self)

    return unicode(self).encode('utf-8')

from gnucash.gnucash_business import Entry

gnucash.gnucash_core_c.__entry__str__=__entry__str__
gnucash.gnucash_business.Entry.add_method("__entry__str__","__str__")

gnucash.gnucash_core_c.__entry__unicode__=__entry__unicode__
gnucash.gnucash_business.Entry.add_method("__entry__unicode__","__unicode__")


