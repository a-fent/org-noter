#!/usr/bin/env bash

pdffile=$1
# 默认从第二页开始提取，因为第一页有时会有logo图等
frompage=${2:-2}

maxWidth=560
maxHeight=560


if [ ! -d raw ]; then
    mkdir raw
fi

if [ ! -n "$(ls -A raw)" ]; then
    pdfimages -f $frompage -all "$pdffile" ./raw/fig
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
