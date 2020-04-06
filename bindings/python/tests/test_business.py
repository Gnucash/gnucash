from unittest import main

from datetime import datetime, timedelta

from gnucash import Account, \
    ACCT_TYPE_RECEIVABLE, ACCT_TYPE_INCOME, ACCT_TYPE_BANK, \
    GncNumeric
from gnucash.gnucash_business import Vendor, Employee, Customer, Job, Invoice, Entry

from test_book import BookSession

class BusinessSession(BookSession):
    def setUp(self):
        BookSession.setUp(self)

        self.today = datetime.today()

        self.bank = Account(self.book)
        self.bank.SetType(ACCT_TYPE_BANK)
        self.bank.SetCommodity(self.currency)
        self.income = Account(self.book)
        self.income.SetType(ACCT_TYPE_INCOME)
        self.income.SetCommodity(self.currency)
        self.receivable = Account(self.book)
        self.receivable.SetType(ACCT_TYPE_RECEIVABLE)
        self.receivable.SetCommodity(self.currency)

        self.customer = Customer(self.book,'CustomerID',self.currency)
        self.vendor = Vendor(self.book,'VendorID',self.currency)
        self.employee = Employee(self.book,'EmployeeID',self.currency)
        self.job = Job(self.book,'JobID',self.customer)

        self.invoice = Invoice(self.book,'InvoiceID',self.currency,self.customer)
        self.invoice.SetDateOpened(self.today)
        entry = Entry(self.book)
        entry.SetDate(self.today)
        entry.SetDescription("Some income")
        entry.SetQuantity(GncNumeric(1))
        entry.SetInvAccount(self.income)
        entry.SetInvPrice(GncNumeric(100))
        self.invoice.AddEntry(entry)

        self.invoice.PostToAccount(self.receivable,
            self.today, self.today, "", True, False)

class TestBusiness(BusinessSession):
    def test_equal(self):
        self.assertTrue( self.vendor.Equal( self.vendor.GetVendor() ) )
        self.assertTrue( self.customer.Equal( self.job.GetOwner() ) )
        self.assertTrue( self.customer.Equal( self.invoice.GetOwner() ) )

    def test_employee_name(self):
        NAME = 'John Doe'
        self.assertEqual( '', self.employee.GetUsername() )
        self.employee.SetUsername(NAME)
        self.assertEqual( NAME, self.employee.GetUsername() )

    def test_post(self):
        self.assertEqual(self.today - timedelta(0, 0, self.today.microsecond),
                         self.invoice.GetDatePosted())
        self.assertTrue( self.invoice.IsPosted() )

    def test_owner(self):
        OWNER = self.invoice.GetOwner()
        self.assertTrue( self.customer.Equal( OWNER ) )

    def test_commodities(self):
        self.assertTrue( self.currency.equal( self.customer.GetCommoditiesList()[0] ) )

if __name__ == '__main__':
    main()
