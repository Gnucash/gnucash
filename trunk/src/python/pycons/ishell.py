#! /usr/bin/env python
#
# Adapted from:
#
# Backend to the console plugin.
# @author: Eitan Isaacson
# @organization: IBM Corporation
# @copyright: Copyright (c) 2007 IBM Corporation
# @license: BSD
#
# All rights reserved. This program and the accompanying materials are made 
# available under the terms of the BSD which accompanies this distribution, and 
# is available at U{http://www.opensource.org/licenses/bsd-license.php}
#

import os
import sys
import re
from StringIO import StringIO
try:
    import IPython
    from IPython import ipapi
except Exception,e:
    raise "Error importing IPython (%s)" % str(e)


# ------------------------------------------------------------------ class Shell
class Shell:
    """ """

    def __init__(self,argv=None,user_ns=None,user_global_ns=None,
                 cin=None, cout=None,cerr=None, input_func=None):
        """ """
        if input_func:
            IPython.iplib.raw_input_original = input_func
        if cin:
            IPython.Shell.Term.cin = cin
        if cout:
            IPython.Shell.Term.cout = cout
        if cerr:
            IPython.Shell.Term.cerr = cerr
        if argv is None:
            argv=[]
        IPython.iplib.raw_input = lambda x: None
        self.term = IPython.genutils.IOTerm(cin=cin, cout=cout, cerr=cerr)
        os.environ['TERM'] = 'dumb'
        excepthook = sys.excepthook
        self.IP = IPython.Shell.make_IPython(argv,
                                             user_ns=user_ns,
                                             user_global_ns=user_global_ns,
                                             embedded=True,
                                             shell_class=IPython.Shell.InteractiveShell)
        self.IP.system = lambda cmd: self.shell(self.IP.var_expand(cmd),
                                                header='IPython system call: ',
                                                verbose=self.IP.rc.system_verbose)
        # Get a hold of the public IPython API object and use it
        self.ip = ipapi.get()
        self.ip.magic('colors LightBG')                
        sys.excepthook = excepthook
        self.iter_more = 0
        self.complete_sep =  re.compile('[\s\{\}\[\]\(\)]')


    def namespace(self):
        return self.IP.user_ns

    def eval(self, console):
        console.write ('\n')
        orig_stdout = sys.stdout
        sys.stdout = IPython.Shell.Term.cout
        try:
            line = self.IP.raw_input(None, self.iter_more)
            if self.IP.autoindent:
                self.IP.readline_startup_hook(None)
        except KeyboardInterrupt:
            self.IP.write('\nKeyboardInterrupt\n')
            self.IP.resetbuffer()
            self.IP.outputcache.prompt_count -= 1
            if self.IP.autoindent:
                self.IP.indent_current_nsp = 0
            self.iter_more = 0
        except:
            self.IP.showtraceback()
        else:
            self.iter_more = self.IP.push(line)
            if (self.IP.SyntaxTB.last_syntax_error and self.IP.rc.autoedit_syntax):
                self.IP.edit_syntax_error()
        if self.iter_more:
            self.prompt = str(self.IP.outputcache.prompt2).strip()
            if self.IP.autoindent:
                self.IP.readline_startup_hook(self.IP.pre_readline)
        else:
            self.prompt = str(self.IP.outputcache.prompt1).strip()
        sys.stdout = orig_stdout

        # System output (if any)
        while True:
            try:
                buf = os.read(console.piperead, 256)
            except:
                break
            else:
                console.write (buf)
            if len(buf) < 256: break

        # Command output
        rv = console.cout.getvalue()
        if rv:
            rv = rv.strip('\n')
        console.write (rv)
        if rv:
            console.write ('\n')
        console.cout.truncate(0)
        console.prompt()

    def complete(self, line):
        split_line = self.complete_sep.split(line)
        possibilities = self.IP.complete(split_line[-1])
        if possibilities:
            common_prefix = os.path.commonprefix (possibilities)
            completed = line[:-len(split_line[-1])]+common_prefix
        else:
            completed = line
        return completed, possibilities

    def shell(self, cmd,verbose=0,debug=0,header=''):
        stat = 0
        if verbose or debug: print header+cmd
        if not debug:
            input, output = os.popen4(cmd)
            print output.read()
            output.close()
            input.close()

