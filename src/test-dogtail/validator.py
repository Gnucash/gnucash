""" untility functions Work as a generic validator """

__author__ = "Ahmed Sayed <ahmadsayed83@yahoo.com>"
import os
spacer = ' '
EXIT_SUCCESS  = 0
EXIT_FAILURE  = 1
EXIT_TROUBLE  = 2

def validate_node (node, testname, ref_filename=None, act_filename=None):
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
    # get divided 265 to get the exact system status
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

