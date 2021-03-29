#!/bin/bash
# -*- coding: utf-8 -*-
# <xbar.title>Lenodal - last videos</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Bastien L.</xbar.author>
# <xbar.author.github>Behel</xbar.author.github>
# <xbar.desc>Last 10 video published on Lenodal Website</xbar.desc>
# <xbar.image>https://attentionphilippelepara.pet/img/images/2021/01/19/bitbarlnd.png</xbar.image>
#

export PATH="/usr/local/bin:$PATH"

echo '|templateImage=iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAMAAACdt4HsAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAdpQTFRFAAAAurq6WVlZAAAAR0dHLi4uCQkJbm5uKysrampqPDw8AAAAbm5ujY2NGxsbAAAALy8vDg4OKysroaGhJSUlLCwsAwMDJSUlzs7OkJCQU1NTBwcHJCQkzs7OLCwskJCQKSkpoaGhurq6KSkpAAAAV1dXPz8/AAAAbGxsJCQkoqKix8fHoqKiT09PGhoaHBwcNzc3paWlqqqqBQUFCgoKfHx8MDAwkZGRbm5uPz8/kZGRurq6Dg4OKSkpICAgX19faWlpQUFBAAAAFhYWAAAAoqKiCQkJzs7OMjIyKCgoHx8fBgYGfn5+fHx8HBwcSUlJx8fHDg4OKSkpU1NTQUFBcnJyra2tbGxsKSkpoqKiNjY2QUFBYWFhGxsbx8fHCgoKKSkpR0dHAAAAGBgYIyMjICAgfHx8AAAAFxcXPj4+urq6LS0tx8fHSUlJFRUVGxsbSUlJeHh4PT09vr6+ZGRkDAwMOTk5qqqqd3d3urq6Gxsbenp6RkZGcHBwEhISUVFRSEhIHx8fKysrAgICBAQEAAAAenp6Hx8fGRkZODg4KioqAwMDra2tf39/CgoKcnJyHx8fBwcHYGBgurq6JSUlXV1dNjY2LCwsoqKiWVlZCAgIh4eHWlpag4ODccDfGwAAAJ50Uk5TADlVVcb//zlVxv//qpuqquP/qqqq+Kr/NuD//85I9ZX//1Wqxv/GOar//5SqOarj/6qq/+Oq/1Vx/6pxqnH/VVU541VxccZs4Pjjxptx/4YZVRxVjvhR/zkcVf9V/97/Vf8c//+OOY7GVRzbMf//xqrw+1H///9V/ylVVf8z/////zP/MzOqqsP7//+jqqqFVVX/TlXG//8fMzOe9P93oiKwAAACEElEQVR4nGNgGAWjYBSQCBiZmJlZWNnYydPMwczMycUNAjwka+bl4+cXEOSGASGSLBYW4ecXZeNGBmJE6+YR58YG+InRKyGJVS8xBjAzM0tJ49ZN0AB+vHrBQEYWnwGE9XPLyeMzQAFNtbiiErqjBJVJMIAfm6/wBgINDVBhViHGAFVMA/jALDV1Bg1NIgzQguuVgqnlATGYQZLKRBigDdOvw6Crp89tYMjAYGRsYsoIllSHSJmZ4zMBZgCIbcFvacXAYG1jC9EOCwQ7e2IM4LcG8RwYYeKOQk4wKWlnYgzgFnBBFnZ1QxQIkAAhaACyQncPbqzihAzwhAm6eAmSYYCxtw9c1FfZD8kA/wBiDFAJDEIWDw7RhEuFhhFhQLg6iBcBikFfsERkFAvMFdExeAyIhSqKA3HiwamOWzABKhlORCAkwpwQkaSWDEm2wEI5JTUNJJlOhAEwS7gzwKpBBoA9z5KZBUvKRBoAAdk5DAy5YFZefoE+MQYUohpQVMzAUAJlJ5cSY4A/qgFl5QwMFdzogLMStwHMqEo1Q6qqQzAMqKkl2gBu7rr6BgwDGptIMAAbaG6h0ADuVhoaAAZt7WL8/B2dXd3kGgAGOT29ffz8/Nmd8c0YBvQTYwDMnLAJE4F1fuGkZCQDKkgwAAomm08BGsM8dRq4vTOddAOgYMbMWa2ts+fMJduAUTAKhi0AAD4EZzpN8ZACAAAAAElFTkSuQmCC'
echo '---'

getnext () {
   local IFS='>'
   read -d '<' TAG VALUE
}

webpage=$(curl -s https://medias.lenodal.com/index.php)

videos=()
while read line
do
  videos+=("$line")
done < <(echo $webpage | while getnext ; do echo "<$TAG>{$VALUE}"; done | grep "<a class=" | iconv -f 'ISO-8859-1' -t "UTF-8" | grep -Eo "ChangeMessage\('.*','menu'\)\" on" | sed "s/ChangeMessage('//;s/','menu')\" on//;s/,/ - /")

ids=()
while read line
do
  ids+=("$line")
done < <(echo $webpage | while getnext ; do echo "<$TAG>{$VALUE}"; done | grep "<a class=" | iconv -f 'ISO-8859-1' -t "UTF-8" | grep -Eo 'id=[0-9]*' | sed 's/id=//')

imgs=()
while read line
do
  imgs+=("$line")
done < <(echo $webpage | while getnext ; do echo "<$TAG>{$VALUE}"; done | grep "<img ")

IMG=$(echo ${imgs[10]} | grep -Eo "src=\'.*jpg'" | sed 's/id=//' | sed "s/'//g;s/src=//") 
base64_img="$(curl -s https://medias.lenodal.com/$IMG --output - | base64)"

echo " | image="$base64_img" href=https://medias.lenodal.com color=#414C92"
echo "Les 10 dernières vidéos | href=https://medias.lenodal.com color=#414C92"
echo "---"

echo ${videos[0]}" | href=https://medias.lenodal.com/video.php?id="${ids[0]}
echo ${videos[1]}" | href=https://medias.lenodal.com/video.php?id="${ids[1]}
echo ${videos[2]}" | href=https://medias.lenodal.com/video.php?id="${ids[2]}
echo ${videos[3]}" | href=https://medias.lenodal.com/video.php?id="${ids[3]}
echo ${videos[4]}" | href=https://medias.lenodal.com/video.php?id="${ids[4]}
echo ${videos[5]}" | href=https://medias.lenodal.com/video.php?id="${ids[5]}
echo ${videos[6]}" | href=https://medias.lenodal.com/video.php?id="${ids[6]}
echo ${videos[7]}" | href=https://medias.lenodal.com/video.php?id="${ids[7]}
echo ${videos[8]}" | href=https://medias.lenodal.com/video.php?id="${ids[8]}
echo ${videos[9]}" | href=https://medias.lenodal.com/video.php?id="${ids[9]}
