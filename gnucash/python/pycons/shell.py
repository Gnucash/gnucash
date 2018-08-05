#! /usr/bin/env python
#
# Copyright (c) 2008, Nicolas Rougier
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of the University of California, Berkeley nor the
#       names of its contributors may be used to endorse or promote products
#       derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE AUTHOR AND CONTRIBUTORS BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import os
import sys
import re
import rlcompleter
import traceback
import tempfile

if not hasattr(sys, 'ps1'): sys.ps1 = '>>> '
if not hasattr(sys, 'ps2'): sys.ps2 = '... '


class Shell:
    """ """

    def __init__(self, ns_globals={}, ns_locals={}):
        """ """

        self.completer = rlcompleter.Completer (ns_globals)
        self.command = ''
        self.globals = ns_globals
        self.locals = ns_locals
        self.complete_sep = re.compile('[\s\{\}\[\]\(\)]')
        self.prompt = sys.ps1


    def namespace(self):
        return self.globals

    def is_balanced (self, line):
        """ Checks line balance for brace, bracket, parenthese and string quote

        This helper function checks for the balance of brace, bracket,
        parenthese and string quote. Any unbalanced line means to wait until
        some other lines are fed to the console.
        """
        
        s = line
        s = filter(lambda x: x in '()[]{}"\'', s)
        s = s.replace ("'''", "'")
        s = s.replace ('"""', '"')
        instring = False
        brackets = {'(':')', '[':']', '{':'}', '"':'"', '\'':'\''}
        stack = []
        while len(s):
            if not instring:
                if s[0] in ')]}':
                    if stack and brackets[stack[-1]] == s[0]:
                        del stack[-1]
                    else:
                        return False
                elif s[0] in '"\'':
                    if stack and brackets[stack[-1]] == s[0]:
                        del stack[-1]
                        instring = False
                    else:
                        stack.append(s[0])
                        instring = True
                else:
                    stack.append(s[0])
            else:
                if s[0] in '"\'' and stack and brackets[stack[-1]] == s[0]:
                    del stack[-1]
                    instring = False
            s = s[1:]
        return len(stack) == 0
        

    def complete(self, line):
        split_line = self.complete_sep.split(line)
        possibilities = []
        i = 0
        c = self.completer.complete (split_line[-1], i)
        while c:
            possibilities.append(c)
            i = i+1
            c = self.completer.complete (split_line[-1], i)
        if possibilities:
            common_prefix = os.path.commonprefix (possibilities)
            completed = line[:-len(split_line[-1])]+common_prefix
        else:
            completed = line
        return completed, possibilities


    def eval (self, console):
        line = console.last_line()
        console.write ('\n')
        if line == '':
            self.execute (console)
            self.command = ''
            self.prompt = sys.ps1
            console.prompt('prompt')
            return
        self.command = self.command + line + '\n'
        if not self.is_balanced (self.command):
            self.prompt = sys.ps2
            console.prompt('prompt')
            return
        line = line.rstrip()
        if len(line) > 0:
            if line[-1] == ':' or line[-1] == '\\' or line[0] in ' \11':
                self.prompt = sys.ps2
                console.prompt('prompt')
                return
        self.execute (console)
        self.command = ''
        self.prompt = sys.ps1
        console.prompt('prompt')


    def execute (self, console):
        if not self.command:
            return
        try:
            try:
                r = eval (self.command, self.globals, self.locals)
                if r is not None:
                    # System output (if any)
                    while True:
                        try:
                            buf = os.read(console.piperead, 256)
                        except:
                            break
                        else:
                            console.write (buf, 'output')
                            if len(buf) < 256: break
                    # Command output
                    print(`r`)
            except SyntaxError:
                exec self.command in self.globals
        except:
            if hasattr (sys, 'last_type') and sys.last_type == SystemExit:
                console.quit()
            elif hasattr (sys, 'exc_type') and sys.exc_type == SystemExit:
                console.quit()
            else:
                try:
                    tb = sys.exc_traceback
                    if tb:
                        tb=tb.tb_next
                    traceback.print(_exception (sys.exc_type, sys.exc_value, tb))
                except:
                    sys.stderr, console.stderr = console.stderr, sys.stderr
                    traceback.print(_exc())

