 ####################################################################
 # validator.py -- Gnucash testsuite generic validator              #
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
 
__author__ = "Ahmed Sayed <ahmadsayed83@yahoo.com>"
import os
spacer = ' '
EXIT_SUCCESS  = 0
EXIT_FAILURE  = 1
EXIT_TROUBLE  = 2

def validate_node (node, testname, ref_filename=None, act_filename=None, filter_command=None):
    """
        brows the children and out the result to file hold the same method name concatenated with _act to be diffed with the testcase name concatenated with _ref
        if i have a testcase called test1
        the generated file is act/test1_act the expected file to compare with is ref/test1_ref
    """
    if act_filename == None:
        act_filename = 'act/%s_act' % (testname)
    if ref_filename == None:
        ref_filename = 'ref/%s_ref' % (testname)
    act_file = open(act_filename, 'w')
    generate_act_file (node, act_file)
    act_file.close()
    # divided by 265 to get the exact system status
    if filter_command != None:
        os.system("sed -n %s < %s >%s" % (filter_command, act_filename, act_filename+"_filtered"))
        os.system("sed -n %s < %s >%s" % (filter_command, ref_filename, ref_filename+"_filtered"))
        error_code = os.system("diff %s %s" % (act_filename+"_filtered", ref_filename+"_filtered"))/256
    else:
        error_code = os.system("diff %s %s" % (act_filename, ref_filename))/256
    return error_code

def validate_files(testname, ref_filename=None, act_filename=None, filter_command=None):
    if act_filename == None:
        act_filename = 'act/%s_act' % (testname)
    if ref_filename == None:
        ref_filename = 'ref/%s_ref' % (testname)
    if filter_command != None:
        os.system("sed -n %s < %s > %s" % (filter_command, act_filename, act_filename+"_filtered"))
        os.system("sed -n %s < %s > %s" % (filter_command, ref_filename, ref_filename+"_filtered"))
        error_code = os.system("diff %s %s" % (act_filename+"_filtered", ref_filename+"_filtered"))/256
    else:
        error_code = os.system("diff %s %s" % (act_filename, ref_filename))/256
    return error_code

def generate_act_file (node, file, depth = 0):
    """ helper method that Generate the act file, The file Generated in the testcase """
    file.write(spacer*(depth) + str (node) + '\n' )
    try:
        for child in node.children:
            generate_act_file  (child, file, depth + 1)
    except AttributeError:
            pass

if __name__ == '__main__':
    validate_files('/home/ahmed/act', '/home/ahmed/ref', "\'/TABLE/,/\/TABLE/p\'")
