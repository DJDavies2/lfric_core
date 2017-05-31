#!/usr/bin/env python
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
'''
Implements a Jinja2 filter to run a macro specified by a string.
'''
from jinja2 import contextfilter

def dictToAssign(inDict):
    '''
    Takes a dictionary and returns a string of assigments k=v 
    @param [in]    inDict    Dictionary
    @return String resulting from setting the environment.
    '''
    envVariables=[]
    for key, value in inDict.items():
        envVariables.append('%s = %s' % (key, value) )
            
    return '\n'.join(envVariables)
