import unittest
from dogtail.utils import run
#from dogtail import tree, predicate
from os import system
from GnuCash import *
import GnuCash
import types
from validator import *

def appstate (func):
    """ Default application state when start run the application when done close it """
    def wrapper(* args, **kwargs):
        print "Run GnuCash"
        run('gnucash')
        sleep (10)
        gnuCash = GnuCashApp()
        gnuCash.dismiss_all_dialogs()

        func(*args, **kwargs)
        gnuCash.close_without_saving()
    return wrapper


class SimpleTest(unittest.TestCase):
    """ Test the Basic Gnucash functionality """
    def setUp(self):
        cleanup_all()

    def tearDown(self):
        ungraceful_close()


    def test_start_gnucash_first_time(self):
        """ Clean all gnucash data in gconf and start it then close it without saving """
        run('gnucash')
        sleep (10)
        gnuCash = GnuCashApp()
        gnuCash.dismiss_all_dialogs()
        gnuCash.close_without_saving()

    def test_start_gnucash_tip_of_day(self):
        """  Start gnucash Tip of days appear properly. """
        #cleanup_all()
        run('gnucash')
        sleep (10)
        gnuCash = GnuCashApp()
        # TODO : move Tip of Day Dialog to wrapper
        tip_of_day_dlg = gnuCash.findChild(predicate.GenericPredicate(roleName='dialog', name='GnuCash Tip Of The Day'))
        if tip_of_day_dlg == None:
            raise Exception()
        gnuCash.dismiss_all_dialogs()

    def test_disable_tip_of_day(self):
        """ test Disable tip of day open gnucash """
        run('gnucash')
        sleep(10)
        gnuCash = GnuCashApp()
        # TODO : move Tip of Day Dialog to wrapper
        tip_of_day_dlg = \
        gnuCash.findChild\
        (predicate.GenericPredicate(roleName='dialog', \
        name='GnuCash Tip Of The Day'))
        show_at_startup_cb = \
        tip_of_day_dlg.findChild\
        (predicate.GenericPredicate(roleName='check box', \
        name='Show tips at startup'))
        show_at_startup_cb.click()
        gnuCash.dismiss_all_dialogs()
        run('gnucash')
        sleep(10)
        gnuCash = GnuCashApp()
        tip_of_day_dlg = \
        gnuCash.findChild(predicate.GenericPredicate(roleName='dialog', \
        name='GnuCash Tip Of The Day'), \
        requireResult=False)
        if tip_of_day_dlg != None:
            raise Exception()


class DialogTest(unittest.TestCase):
    """ Test that dialog loaded as expected """
    def setUp(self):
        run('gnucash')
        sleep (10)
        gnuCash = GnuCashApp()
        gnuCash.dismiss_all_dialogs()
        gnuCash.add_new_account_page()

    def tearDown(self):
        gnuCash = GnuCashApp()
        gnuCash.close_without_saving()

    def create_test_suite(self):
        """ This helper method could create python code for this test """
        list_dialogs = [method for method in dir(GnuCash) if type(getattr(GnuCash, method)) == types.ClassType]
        for i in list_dialogs:
            if issubclass(getattr(GnuCash, i), GnucashWindow):
                print "    def test_invoke_" + str(i)+"(self):"
                var_name = i.lower()
                print "       "+var_name+ " = "+i+"()"
                print "       "+var_name+".invoke()"
                print "       "+var_name+".dismiss()"
                print


    def test_invoke_FindCustomer(self):
       findcustomer = FindCustomer()
       findcustomer.invoke()
       findcustomer.dismiss()

    def test_invoke_FindInvoice(self):
       findinvoice = FindInvoice()
       findinvoice.invoke()
       findinvoice.dismiss()

    def test_invoke_FindJob(self):
       findjob = FindJob()
       findjob.invoke()
       findjob.dismiss()

    def test_invoke_FindTransaction(self):
       findtransaction = FindTransaction()
       findtransaction.invoke()
       findtransaction.dismiss()

    def test_invoke_FindVendor(self):
       findvendor = FindVendor()
       findvendor.invoke()
       findvendor.dismiss()

    def test_invoke_FindVendorJob(self):
       findvendorjob = FindVendorJob()
       findvendorjob.invoke()
       findvendorjob.dismiss()

    def test_invoke_NewAccount(self):
       newaccount = NewAccount()
       newaccount.invoke()
       newaccount.dismiss()

    def test_invoke_NewCustomer(self):
       newcustomer = NewCustomer()
       newcustomer.invoke()
       newcustomer.dismiss()

    def test_invoke_NewInvoice(self):
       newinvoice = NewInvoice()
       newinvoice.invoke()
       newinvoice.dismiss()

    def test_invoke_NewJob(self):
       newjob = NewJob()
       newjob.invoke()
       newjob.dismiss()

    def test_invoke_NewVendor(self):
       newvendor = NewVendor()
       newvendor.invoke()
       newvendor.dismiss()

    def test_invoke_NewVendorJob(self):
       newvendorjob = NewVendorJob()
       newvendorjob.invoke()
       newvendorjob.dismiss()

    def test_invoke_Prefernces(self):
       preferences = Preferences()
       preferences.invoke()
       preferences.dismiss()

    def test_invoke_ProcessPayment(self):
       processpayment = ProcessPayment()
       processpayment.invoke()
       processpayment.dismiss()

    def test_invoke_ResetWarnings(self):
       resetwarnings = ResetWarnings()
       resetwarnings.invoke()
       resetwarnings.dismiss()

    def test_invoke_SetupAccountingPeriods(self):
        """ TODO: Setup Accounting Periods start wizard the roleName is frame not dialog """
        setupaccountingperiods = SetupAccountingPeriods()
        setupaccountingperiods.invoke()
        setupaccountingperiods.dismiss()

    def test_invoke_StyleSheets(self):
       stylesheets = StyleSheets()
       stylesheets.invoke()
       stylesheets.dismiss()

    def test_invoke_TaxOption(self):
       taxoption = TaxOption()
       taxoption.invoke()

class TestDialogs(unittest.TestCase):
    """ Test Each dialog functionality """
    def setUp(self):
        run('gnucash')
        sleep(20)
        gnuCash = GnuCashApp()
        gnuCash.dismiss_all_dialogs()

    def tearDown(self):
        gnuCash = GnuCashApp()
        gnuCash.close_without_saving()

    def test_add_remove_style_sheets(self):
        """ Test Adding new Style Sheet """
        gnucash = GnuCashApp()
        style_sheet = StyleSheets()
        style_sheet.invoke()
        style_sheet.new_btn.click()
        new_style_dlg = gnucash.findChild(predicate.GenericPredicate(roleName='dialog', name='New Style Sheet'))
        new_style = NewStyleSheet(new_style_dlg)
        new_style.style_name = "test"
        new_style.template = "Fancy"
        new_style.accept()
        sleep(5)
        html_style_dlg_list = gnucash.findChildren(predicate.GenericPredicate(roleName='dialog'))
        html_style_dlg = None
        for dialog in html_style_dlg_list:
            result = re.match('HTML Style Sheet Properties: .', dialog.name)
            if result != None:
                print dialog.name
                html_style_dlg = HTMLStyleSheet(dialog)
        html_style_dlg.defaults_btn.click()
        html_style_dlg.accept()
        # validate the the whole table 
        validate_node(style_sheet.style_sheets_table, 'test_style_sheets')
        test_cell = style_sheet.style_sheets_table.findChild\
        (predicate.GenericPredicate(roleName='table cell', name='test'))
        test_cell.doAction('activate')
        style_sheet.delete_btn.click()
        # after removing the node confirm that the state resotred to the previous State before Adding
        validate_node(style_sheet.style_sheets_table, 'test_style_sheets_2')
        style_sheet.dismiss()

class ScenarioTest(unittest.TestCase):
    """ Test a compelete Scenario """

    def transaction(self, account):
        """ Helper method to open the account register tab """
        config.childrenLimit = 500
        gnucash = GnuCashApp()
        accounts = gnucash.tab('Accounts')
        account = accounts.findChild(predicate.GenericPredicate(roleName='table cell', name=account))
        account.doAction('activate')


    def setUp(self):
        """ a setup  for the test case in this type of test just run gnucash and go dismiss first dialog """
        cleanup_all()
        run('gnucash')
        sleep (20)
        gnuCash = GnuCashApp()
        gnuCash.dismiss_all_dialogs()

    def tearDown(self):
        """ just close gnucash without Saving """
        gnuCash = GnuCashApp()
#        gnuCash.close_without_saving()

    def test_new_account_dialog(self):
        """ Test creating new Account currently I could able only to test an account with only 2 levels 
            No validation here 
            TODO: add validation
        """
        gnucash = GnuCashApp()
        gnucash.add_new_account_page()
        account_list = [
                ['Asset', 'Asset'],
                ['Liability','Liability'],
                ['Equity', 'Equity'],
                ['Income', 'Income'],
                ['Expense', 'Expense'],
                ['Checking', 'Bank', 'Asset'],
                ['Savings', 'Bank', 'Asset'],
                ['Visa', 'Credit Card', 'Liability'],
                ['Salary', 'Income', 'Income'],
                ['Phone', 'Expense', 'Expense'],
                ['Electricity', 'Expense', 'Expense'],
                ['Rent', 'Expense', 'Expense'],
                ['Groceries', 'Expense', 'Expense'],
                ['Taxes', 'Expense', 'Expense'],
                ['Opening Balance', 'Equity', 'Equity']
            ]
        for account in account_list:
            gnucash.add_account(*account)
        # Validation
        account_tab = gnucash.tab('Accounts')
        validate_node(account_tab, 'test_new_account_dialog')

    def test_perform_transaction(self):
        """ Call the previos test case and then perform some transaction 
            TODO: validatior not yet but it will be on the affect of the transaction by checking the account page not the register page
        """
        self.test_new_account_dialog()
        gnucash = GnuCashApp()
        self.transaction("Checking")
        reg = gnucash.tab("Checking").findChild(predicate.GenericPredicate(roleName='layered pane'))
        register = Register(reg)
        register.row = 0
        register.date = "07/15/2007"
        register.transfer = "Equity:Opening Balance"
        register.deposite = "1000"
        register.end_trans()

        self.transaction("Savings")
        reg = gnucash.tab("Savings").findChild(predicate.GenericPredicate(roleName='layered pane'))
        register = Register(reg)
        register.row = 0
        register.date = "07/15/2007"
        register.transfer = "Equity:Opening Balance"
        register.deposite = "1000"
        register.end_trans()

        self.transaction("Visa")
        reg = gnucash.tab("Visa").findChild(predicate.GenericPredicate(roleName='layered pane'))
        register = Register(reg)
        register.row = 0
        register.date = "07/15/2007"
        register.transfer = "Equity:Opening Balance"
        register.withdrawal = "500"
        register.end_trans()
        # Validation
        account_tab = gnucash.tab('Accounts')
        validate_node(account_tab, 'test_perform_transaction')

    def test_perform_reconcilation(self):
        """ Test Reconcilation """
        gnucash = GnuCashApp()
        gnucash.open_data_file('mytest2')
        reconcile = gnucash.reconcile_account('Asset')
        reconcile.include_subaccount = True
        reconcile.accept()
        reconcileFrame = ReconcileFrame()
        validate_node(reconcileFrame.funds_in, 'test_before_perform_reconcilation_funds_in')
        validate_node(reconcileFrame.funds_out, 'test_before_perform_reconcilation_funds_out')

        reconcileFrame.select_all_funds_out()
        reconcileFrame.select_all_funds_in()
        reconcileFrame.finish()

        # Validation Done by opening the Reconcile frame and check Funds In and Funds Out Table are empty
        reconcile = gnucash.reconcile_account('Asset')
        reconcile.accept()
        reconcileFrame = ReconcileFrame()
        validate_node(reconcileFrame.funds_in, 'test_after_perform_reconcilation_funds_in')
        validate_node(reconcileFrame.funds_out, 'test_after_perform_reconcilation_funds_out')

    def test_accounts_receivable(self):
        """ Test accounts receivable """
        gnucash = GnuCashApp()
        gnucash.add_new_account_page()
        account_list = [
                ['Asset', 'Asset'],
                ['Income', 'Income'],
                ['Checking', 'Bank', 'Asset'],
                ['Accounts Receivable', 'A/Receivable', 'Asset'],
                ['Sales', 'Income', 'Income'],
            ]
        for account in account_list:
            gnucash.add_account(*account)


if __name__ == "__main__":
    unittest.main()
