#!/usr/bin/env python
# -*- coding: utf-8; -*-

# * Imports
import sys

from pdfrw import PdfReader


# * Functions
def chop_prefix(text, prefix):
    if text.startswith(prefix):
        return text[len(prefix):]
    return text  # or whatever


def chop_suffix(text, suffix):
    if text.endswith(suffix):
        return text[:-len(suffix)]
    return text  # or whatever


def clean_results(res):
    new_res = chop_prefix(chop_suffix(res, ')'), '(')
    return new_res


# * Main
inpfn = sys.argv[1]
attrib = '/' + sys.argv[2]
trailer = PdfReader(inpfn)

if trailer.Info[attrib]:
    results = clean_results(trailer.Info[attrib])
    print(results)
else:
    print('None')
