#!/usr/bin/env python
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
'''
Implements a Jinja2 filter to strip the resolutions info.
'''
from jinja2 import contextfilter
import repr

def unique_entries(inDict):
    ''' Returns unique entries from a dictionary of lists '''

    unique=set()
    for k,v in inDict.items():
        unique |= set(v)

    return list(unique)
