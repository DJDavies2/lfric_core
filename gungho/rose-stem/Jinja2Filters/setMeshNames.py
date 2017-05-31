#!/usr/bin/env python
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
'''
Return a list of meshnames to be generated for the list of science resolutions
'''

def setMeshNames(resolutionDict, groups):
    '''
    @param [in] nested dictionarys of rose/science groups and their resolution dependencies
    @param [in] list of rose groups to be used
    @return list of all the different resolutions
    '''
    meshnames=set()
    for group in groups:
        for sgroup,groupList in resolutionDict[group].items():
            meshnames |= set(groupList)
        
    return list(meshnames)
