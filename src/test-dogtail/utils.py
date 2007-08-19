 ####################################################################
 # utils.py -- Helper Methods used to generate some testcases       #
 #             these mothod does not do anything with testsuite     #
 # Copyright (C) 2007 Ahmed Sayed Hassan, <ahmadsayed83@yahoo.com>  #
 #                                                                  #
 # This program is free software; you can redistribute it and/or    #
 # modify it under the terms of the GNU General Public License as   #
 # published by the Free Software Foundation; either version 2 of   #
 # the License, or (at your option) any later version.              #
 #                                                                  #
 # This program is distributed in the hope that it will be useful,  #
 # but WITHOUT ANY WARRANTY; without even the implied warranty of   #
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    #
 # GNU General Public License for more details.                     #
 #                                                                  #
 # You should have received a copy of the GNU General Public License#
 # along with this program; if not, contact:                        #
 #                                                                  #
 # Free Software Foundation           Voice:  +1-617-542-5942       #
 # 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       #
 # Boston, MA  02110-1301,  USA       gnu@gnu.org                   #
 #                                                                  #
 ####################################################################

from dogtail.utils import run
from os import system
from GnuCash import *
import GnuCash
import types

def generate_wizard_testcases():
    """ Create a TestWizard test cases """
    config.childrenLimit = 1500
    run('gnucash')
    sleep (20)
    gnuCash = GnuCashApp()
    gnuCash.dismiss_all_dialogs()
    gnuCash.menu('File').menu('New').menuItem('New File').click()
    duride_frame = gnuCash.findChild(\
        predicate.GenericPredicate(roleName='frame', \
        name='New Account Hierarchy Setup'))
    new_account_setup = NewAccountSetup(duride_frame)
    new_account_setup.clickForward()
    new_account_setup.clickForward()
    simple_checkbook = new_account_setup.currentPage().child(name='A Simple Checkbook')
    categorize_table = simple_checkbook.findAncestor(predicate.GenericPredicate(roleName='table'))
    cells = categorize_table.findChildren(predicate.GenericPredicate(roleName='table cell'))
    for cell in cells:
        name = cell.name
        if len(name) > 1:
            lower_under_score_name = name.lower().replace(' ', '_')
            print ""
            print "    def test_new_account_wizard_%s(self):" % lower_under_score_name
            print "        gnucash = GnuCashApp()"
            print "        \"\"\" Test Creating %s \"\"\" " % name
            print "        self.new_account_wizard(['%s'])" % name
            print "        #validation"
            print "        account_tab = gnucash.tab('Accounts')"
            print "        self.assertEquals(validate_node(account_tab, 'test_new_account_wizard_%s'), EXIT_SUCCESS)" % lower_under_score_name

def create_dialog_test_testcases():
    """ 
    This helper method could create python code for this test
    This method unlike generate_wizard_testcases that uses dogtail itself
    to generate the testcases it uses the wrapper class to generate the testcases
    as I don't call any way to browse the app dialogs without invoking them
    TODO: add comments to the generated testcases and update the testcases
    """
    list_dialogs = [method for method in dir(GnuCash) if type(getattr(GnuCash, method)) == types.ClassType]
    for i in list_dialogs:
        if issubclass(getattr(GnuCash, i), GnucashWindow):
            print "    def test_invoke_" + str(i)+"(self):"
            var_name = i.lower()
            print "       "+var_name+ " = "+i+"()"
            print "       "+var_name+".invoke()"
            print "       "+var_name+".dismiss()"
            print


if __name__ == "__main__":
    create_test_suite()