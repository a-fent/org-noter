#!/usr/bin/env python
# -*- coding: utf-8; -*-

# * Imports

import os
import glob
import argparse
import subprocess

# * Code


def parseArguments():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "pdfFile", help="Images from this file will be extracted.", type=str)
    parser.add_argument(
        "-f", '--fromPage', help="Start from this page.", type=int, default=2)

    # Print version
    parser.add_argument(
        "--version", action="version", version='%(prog)s - Version 0.1')

    args = parser.parse_args()

    return args


def chooseImages(pdfFile, fromPage=2):
    """Choose images to fix"""
    tab = subprocess.check_output(
        'pdfimages -f {page} -list "{pdf}"'.format(page=fromPage, pdf=pdfFile),
        shell=True)
    tab = tab.splitlines()[2:]
    images = [
        # 注意这里都要有b，代表字节码（经过编码）。没有会报错。
        b'fig-' + row.split()[1].rjust(3, b'0') + b'.png' for row in tab
        if row.split()[2] != b'image'
    ]

    return images


def fixImage(imgfile):
    subprocess.call('mogrify -negate {}'.format(imgfile), shell=True)

    return 0


if __name__ == '__main__':
    args = parseArguments()

    subprocess.call(
        'pdfimages -f {page} -png "{pdf}" ./fig'.format(
            page=args.fromPage, pdf=args.pdfFile),
        shell=True)

    files = glob.glob('./*.png')
    imgsToFix = chooseImages(args.pdfFile)

    for f in files:
        if os.path.basename(f).encode() in imgsToFix:
            fixImage(f)
