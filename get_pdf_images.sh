#!/usr/bin/env bash

pdffile=$1

maxWidth=640
maxHeight=640


if [ ! -d raw ]; then
    mkdir raw
fi

if [ ! -n "$(ls -A raw)" ]; then
    pdfimages -all "$pdffile" ./raw/fig
fi

if [ ! -d small ]; then
    mkdir small
fi

if [ ! -n "$(ls -A small)" ]; then
    cp -r ./raw/* ./small/

    files=`find ./small -name '*.jpg' -o -name '*.png'`

    for f in $files
    do
        imageWidth=$(identify -format "%w" "$f")

        if [ "$imageWidth" -gt "$maxWidth" ]; then
            mogrify -resize ''"$maxWidth"x'' $f
        fi

        imageHeight=$(identify -format "%h" "$f")

        if [ "$imageHeight" -gt "$maxHeight" ]; then
            mogrify -resize ''x"$maxHeight"'' $f
        fi

        mv $f "${f%.*}s.${f##*.}"

        # 路径名
        # dirname $f
        # 文件名，去掉路径和后缀
        # basename $f .jpg
        # 文件名中除了后缀以外的部分（包含路径）
        # echo ${f%.*}
        # 文件名中的后缀
        # echo ${f##*.}
    done
fi
