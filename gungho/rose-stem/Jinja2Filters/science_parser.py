#!/usr/bin/env python
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
'''
Implements a parser for the science mission command line
'''
def science_parser(s):
    """
    Split `s` by commas where those in parentheses, i.e. (),[],{}, are ignored.
    """

    # Parse the string tracking whether the current character is within
    # parentheses.
    balance = 0
    parts = []
    part = ''

    def c_comp(s):
        ' return complementary string to s '
        r=None
        if s=='(':r=')'
        if s=='{':r='}'
        if s=='[':r=']'
        return r

    for c in s:
        part += c
        if balance == 0: bc=None
        if c in ['(','{','['] and bc in [None,c]:
            balance += 1
            bc=c
        elif c == c_comp(bc):
            balance -= 1
        elif c == ',' and balance == 0:
            parts.append(part[:-1].strip())
            part = ''

    # Capture last part
    if len(part):
        parts.append(part.strip())

    return parts
