#!/usr/bin/env python
# -*- coding: utf-8; -*-

# * Imports
import os
import argparse

from pdfrw import PdfReader, PdfWriter

# * Functions


def parseArguments():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "pdfFile", help="PDF file whose meta needs to be changed.", type=str)

    parser.add_argument(
        # "-n",
        # "--notesFile",
        "notesFile",
        help="Note file path that is to be added as metadata.",
        type=str)

    parser.add_argument(
        '-f',
        '--force',
        help="Force update the notesFile meta.",
        action='store_true')

    parser.add_argument(
        '-a',
        '--add',
        help="Add the notes file to existing list",
        action='store_true')

    parser.add_argument(
        '-r',
        '--remove',
        help="Remove the metadata entry.",
        action='store_true')

    args = parser.parse_args()

    return args


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

if __name__ == '__main__':
    args = parseArguments()

    inpfn = args.pdfFile
    fname, ext = os.path.splitext(inpfn)
    outfn = fname + '_mod' + ext

    trailer = PdfReader(inpfn)

    if args.remove:
        try:
            trailer.Info.pop('/NotesFile')
        except KeyError:
            pass

        PdfWriter(outfn, trailer=trailer).write()
        os.rename(outfn, inpfn)

    elif not trailer.Info.NotesFile or args.force:
        trailer.Info.NotesFile = args.notesFile
        PdfWriter(outfn, trailer=trailer).write()
        os.rename(outfn, inpfn)

    elif args.add:
        notes_files = clean_results(trailer.Info.NotesFile).split(':')

        if not os.path.abspath(
                args.notesFile) in [os.path.abspath(f) for f in notes_files]:
            notes_files.append(args.notesFile)
            trailer.Info.NotesFile = ':'.join(notes_files)
            PdfWriter(outfn, trailer=trailer).write()
            os.rename(outfn, inpfn)
