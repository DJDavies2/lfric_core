#!/usr/bin/env python
# -*- coding: utf-8 -*-
##############################################################################
# (c) Crown copyright 2021 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################

import os
import re
import sys

from testframework import LFRicLoggingTest, TestEngine, TestFailed

class TLTest(LFRicLoggingTest):
  def __init__(self,flag):
    self._flag = flag
    if 'MPIEXEC_BROKEN' in os.environ:
      TLTest.set_mpiexec_broken()
    super(TLTest, self).__init__( [sys.argv[1],
                                    'semi_implicit_configuration.nml',
                                    'test_' + self._flag],
                                    processes=1,
                                    name='tl_test.Log' )


  def test( self, return_code, out, err ):
    if return_code != 0:
      message = 'Test program failed with exit code: {code}'
      raise TestFailed( message.format( code=return_code ),
                        stdout=out, stderr=err,
                        log=self.getLFRicLoggingLog() )

    if not self.test_passed( out ): # "out" becomes self.getLFRicLoggingLog() when PE>1
      message = 'Test {} failed'
      raise TestFailed( message.format( self._flag ),
                        stdout=out, stderr=err,
                        log=self.getLFRicLoggingLog() )

    return 'TL test : '+self._flag

  def test_passed(self, out):
    success = False
    pattern = re.compile( r'\s+test\s+.*?:\s*PASS\s*$' )
    for line in out.split("\n"):
      match = pattern.search( line )
      if match:
        success = True
    return success

class tl_test_semi_imp_alg(TLTest):
    def __init__(self):
        flag = "semi_imp_alg"
        super(tl_test_semi_imp_alg, self).__init__(flag)

class tl_test_rhs_alg(TLTest):
    def __init__(self):
        flag = "rhs_alg"
        super(tl_test_rhs_alg, self).__init__(flag)

class tl_test_rhs_eos(TLTest):
    def __init__(self):
        flag = "rhs_eos"
        super(tl_test_rhs_eos, self).__init__(flag)

class tl_test_advection_control(TLTest):
    def __init__(self):
        flag = "advection_control"
        super(tl_test_advection_control, self).__init__(flag)

if __name__ == '__main__':
    TestEngine.run( tl_test_rhs_eos() )
    TestEngine.run( tl_test_advection_control() )
    TestEngine.run( tl_test_semi_imp_alg() )
    TestEngine.run( tl_test_rhs_alg() )
