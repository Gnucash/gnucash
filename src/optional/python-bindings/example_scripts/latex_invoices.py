#!/usr/bin/env python
# -*- coding: UTF-8 -*-

##@file
# @brief Exports an invoice to lco-file for use with LaTeX, see \ref py_invoice_export
# @ingroup python_bindings_examples
# @author Christoph Holtermann (c.holtermann (at) gmx.de)
# @date May 2011
#
# @details
# The output file can be imported into KOMA-Script-letters.
# This works primarily for germany. Internationalization welcome!
#
# Additional files:
#
# - Invoice.tex\n
# Example template file. Should be modified according to personal needs.
# - rechnung.sty\n
# style file for invoices.\n
# This file is not part of the python-bindings!\n
# For an example where to get it see section credits below.
#
# Usage :
# \code latex_invoice -l -f -n INVOICE_NUMBER file://testfile \endcode
# will create file data.lco.
# \code latex --output-format=pdf Invoice.tex \endcode
# should run latex on file Invoice.tex and result in Invoice.pdf. Invoice.tex includes data.lco.
#
# Additional information :
#
# - Doxygen docs: see page \ref py_invoice_export at http://svn.gnucash.org/docs/HEAD/
# - http://www.uweziegenhagen.de/latex/documents/rechnung/rechnungen.pdf (german)
#
# Credits to and ideas from
#
# - Main function as proposed by Guido van Rossum
#   at http://www.artima.com/weblogs/viewpost.jsp?thread=4829
# - Invoice.tex is derived from\n
#   scrlttr2.tex v0.3. (c) by Juergen Fenn <juergen.fenn@gmx.de>\n
#   http://www.komascript.de/node/355\n
#   english translation: ftp://ftp.dante.de/tex-archive/info/templates/fenn/scrlttr2en.tex
# - rechnung.sty\n
#   from M G Berberich (berberic@fmi.uni-passau.de) and Ulrich Sibiller (uli42@web.de)
#   Ver3.10 from http://www.forwiss.uni-passau.de/~berberic/TeX/Rechnung/index.html
#
# To Do:
#
# - get own contact data from gnucash
# - have own bank information in footline
# - nicer formatting of invoice date and date due
# - is there anything else missing in this invoice ?

try:
    import sys
    import getopt
    import gnucash
    import str_methods
    from gncinvoicefkt import *
    from IPython import version_info as IPython_version_info
    if IPython_version_info[0]>=1:
        from IPython.terminal.ipapp import TerminalIPythonApp
    else:
        from IPython.frontend.terminal.ipapp import TerminalIPythonApp
    from gnucash.gnucash_business import Customer, Employee, Vendor, Job, \
        Address, Invoice, Entry, TaxTable, TaxTableEntry, GNC_AMT_TYPE_PERCENT, \
            GNC_DISC_PRETAX
    import locale
except ImportError as import_error:
    print "Problem importing modules."
    print import_error
    sys.exit(2)

class Usage(Exception):
    def __init__(self, msg):
        self.msg = msg

def invoice_to_lco(invoice):
  """returns a string which forms a lco-file for use with LaTeX"""

  lco_out=u"\ProvidesFile{data.lco}[]\n"

  def write_variable(ukey, uvalue, replace_linebreak=True):

    outstr = u""
    if uvalue.endswith("\n"):
        uvalue=uvalue[0:len(uvalue)-1]

    if not ukey in [u"fromaddress",u"toaddress",u"date"]:
        outstr += u'\\newkomavar{'
        outstr += ukey
        outstr += u"}\n"

    outstr += u"\\setkomavar{"
    outstr += ukey
    outstr += u"}{"
    if replace_linebreak:
        outstr += uvalue.replace(u"\n",u"\\\\")+"}"
    return outstr

  # Write owners address
  add_str=u""
  owner = invoice.GetOwner()
  if owner.GetName() != "":
    add_str += owner.GetName().decode("UTF-8")+"\n"

  addr  = owner.GetAddr()
  if addr.GetName() != "":
    add_str += addr.GetName().decode("UTF-8")+"\n"
  if addr.GetAddr1() != "":
    add_str += addr.GetAddr1().decode("UTF-8")+"\n"
  if addr.GetAddr2() != "":
    add_str += addr.GetAddr2().decode("UTF-8")+"\n"
  if addr.GetAddr3() != "":
    add_str += addr.GetAddr3().decode("UTF-8")+"\n"
  if addr.GetAddr4() != "":
    add_str += addr.GetAddr4().decode("UTF-8")+"\n"

  lco_out += write_variable("toaddress2",add_str)

  # Invoice number
  inr_str = invoice.GetID()
  lco_out += write_variable("rechnungsnummer",inr_str)

  # date
  date      = invoice.GetDatePosted()
  udate     = date.strftime("%d.%m.%Y")
  lco_out  += write_variable("date",udate)+"\n"

  # date due
  date_due  = invoice.GetDateDue()
  udate_due = date_due.strftime("%d.%m.%Y")
  lco_out  += write_variable("date_due",udate_due)+"\n"


  # Write the entries
  ent_str = u""
  locale.setlocale(locale.LC_ALL,"de_DE")
  for n,ent in enumerate(invoice.GetEntries()):

      line_str = u""

      if type(ent) != Entry:
        ent=Entry(instance=ent)                                 # Add to method_returns_list

      descr = ent.GetDescription()
      price = ent.GetInvPrice().to_double()
      n     = ent.GetQuantity()

      uprice = locale.currency(price).rstrip(" EUR")
      un = unicode(int(float(n.num())/n.denom()))               # choose best way to format numbers according to locale

      line_str =  u"\Artikel{"
      line_str += un
      line_str += u"}{"
      line_str += descr.decode("UTF-8")
      line_str += u"}{"
      line_str += uprice
      line_str += u"}"

      #print line_str
      ent_str += line_str

  lco_out += write_variable("entries",ent_str)

  return lco_out


def main(argv=None):
    if argv is None:
        argv = sys.argv
    try:
        prog_name = argv[0]
        with_ipshell = False
        ignore_lock = False
        no_latex_output = True
        list_invoices = False
        output_file_name = "data.lco"
        invoice_number = None

        try:
            opts, args = getopt.getopt(argv[1:], "fhiln:po:", ["help"])
        except getopt.error, msg:
             raise Usage(msg)

        for opt in opts:
            if opt[0] in ["-f"]:
                print "ignoring lock"
                ignore_lock = True
            if opt[0] in ["-h","--help"]:
                raise Usage("Help:")
            if opt[0] in ["-i"]:
                print "Using ipshell"
                with_ipshell = True
            if opt[0] in ["-l"]:
                print "listing all invoices"
                list_invoices=True
            if opt[0] in ["-n"]:
                invoice_number = int(opt[1])
                print "using invoice number", invoice_number
                no_latex_output = False
            if opt[0] in ["-o"]:
                output_file_name = opt[1]
                print "using output file", output_file_name
        if len(args)>1:
            print "opts:",opts,"args:",args
            raise Usage("Only one input can be accepted !")
        if len(args)==0:
            raise Usage("No input given !")
        input_url = args[0]
    except Usage, err:
        if err.msg == "Help:":
            retcode=0
        else:
            print >>sys.stderr, "Error:",err.msg
            print >>sys.stderr, "for help use --help"
            retcode=2

        print "Generate a LaTeX invoice or print out all invoices."
        print
        print "Usage:"
        print
        print "Invoke with",prog_name,"input."
        print "where input is"
        print "   filename"
        print "or file://filename"
        print "or mysql://user:password@host/databasename"
        print
        print "-f             force open = ignore lock"
        print "-h or --help   for this help"
        print "-i             for ipython shell"
        print "-l             list all invoices"
        print "-n number      use invoice number (no. from previous run with -l)"
        print "-o name        use name as outputfile. default: data.lco"

        return retcode

    # Try to open the given input
    try:
        session = gnucash.Session(input_url,ignore_lock=ignore_lock)
    except Exception as exception:
        print "Problem opening input."
        print exception
        return 2

    book = session.book
    root_account = book.get_root_account()
    comm_table = book.get_table()
    EUR = comm_table.lookup("CURRENCY", "EUR")

    invoice_list=get_all_invoices(book)

    if list_invoices:
        for number,invoice in enumerate(invoice_list):
            print str(number)+")"
            print invoice

    if not (no_latex_output):

        if invoice_number == None:
            print "Using the first invoice:"
            invoice_number=0

        invoice=invoice_list[invoice_number]
        print "Using the following invoice:"
        print invoice

        lco_str=invoice_to_lco(invoice)

        # Opening output file
        f=open(output_file_name,"w")
        lco_str=lco_str.encode("latin1")
        f.write(lco_str)
        f.close()

    if with_ipshell:
        app = TerminalIPythonApp.instance()
        app.initialize(argv=[]) # argv=[] instructs IPython to ignore sys.argv
        app.start()

    #session.save()
    session.end()

if __name__ == "__main__":
    sys.exit(main())

