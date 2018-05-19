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


def appendMark(filename):
    name, ext = os.path.splitext(filename)
    return "{name}s{ext}".format(name=name, ext=ext)


def resizeImage(imgfile):
    imageWidth = subprocess.check_output(
        ['identify', '-format', "%w", imgfile])
    if int(imageWidth) > maxWidth:
        subprocess.call(
            ['mogrify', '-resize', '{}x'.format(maxWidth), imgfile])

    imageHeight = subprocess.check_output(
        ['identify', '-format', "%h", imgfile])
    if int(imageHeight) > maxHeight:
        subprocess.call(
            ['mogrify', '-resize', 'x{}'.format(maxHeight), imgfile])

    return 0


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

    maxWidth = 560
    maxHeight = 560

    try:
        os.mkdir('raw')
    except FileExistsError:
        pass

    if not os.listdir('raw'):
        subprocess.call(
            'pdfimages -f {page} -png "{pdf}" ./raw/fig'.format(
                page=args.fromPage, pdf=args.pdfFile),
            shell=True)

        files = glob.glob('./raw/*.png')
        imgsToFix = chooseImages(args.pdfFile)

        for f in files:
            if os.path.basename(f).encode() in imgsToFix:
                fixImage(f)

    try:
        os.mkdir('small')
    except FileExistsError:
        pass

    if not os.listdir('small'):
        subprocess.call('cp -r ./raw/* ./small/', shell=True)

        files = glob.glob('./small/*.png')

        for f in files:
            resizeImage(f)
            os.rename(f, appendMark(f))
