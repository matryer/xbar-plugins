#!/bin/bash

# <bitbar.title>Bitcurex stats</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Tomasz Wierzbicki tomaszwierzbicki@icloud.com</bitbar.author>
# <bitbar.author.github>Bicki</bitbar.author.github>

bitcoin_icon='iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAA3NpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNS1jMDE0IDc5LjE1MTQ4MSwgMjAxMy8wMy8xMy0xMjowOToxNSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wTU09Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9tbS8iIHhtbG5zOnN0UmVmPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvc1R5cGUvUmVzb3VyY2VSZWYjIiB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iIHhtcE1NOk9yaWdpbmFsRG9jdW1lbnRJRD0ieG1wLmRpZDpmMDk3ZWJmYS0yODA4LTRkNDUtOTYxMC01YTkzNzU4YzY4YmEiIHhtcE1NOkRvY3VtZW50SUQ9InhtcC5kaWQ6REMzOTkzNzA0OEZGMTFFNkJCNTNCODBEODQ4N0M2NzgiIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6REMzOTkzNkY0OEZGMTFFNkJCNTNCODBEODQ4N0M2NzgiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChNYWNpbnRvc2gpIj4gPHhtcE1NOkRlcml2ZWRGcm9tIHN0UmVmOmluc3RhbmNlSUQ9InhtcC5paWQ6NDcyMGI2M2QtZjlmNS00MDlmLWE0NGMtYjExZTc3NTZmOWRjIiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOmYwOTdlYmZhLTI4MDgtNGQ0NS05NjEwLTVhOTM3NThjNjhiYSIvPiA8L3JkZjpEZXNjcmlwdGlvbj4gPC9yZGY6UkRGPiA8L3g6eG1wbWV0YT4gPD94cGFja2V0IGVuZD0iciI/Pokm66sAAAFeSURBVHjatJQ/SwNBEMXvQtRKI6KdFlaWQgqLFFG00EII4geQq2wtkvscdmqhBGxiYaGopBANVkkaSytBsbCIgmITxX9v5R2Mw6ymycDv7nZn9u3dzOyFgW3DYAHMgTEQgltwBqrgIejA8uAEfBl80pf/TyQWi55AGTyCLdBSorFPpKgCL0GGAj3g2njDohbJCuceWAZLYBDkGLMIjg2xbCIyAGrC4T5nXGwyBQpivK6EatQIIk9infWLcVWIvanYKO2pQJsl/wAvfK7Tt8KcSZtOs0+0uR2HWLEJzt3zPmnEj7rLqfFZz6CXHIALFiHZtKLincZPcn05yqi5Vc6vqfmylexXsM8FfeAINMA2x84OdbKt8lf+6HyXtw1f+XVD3oAm2OTh3QUpsGOU/VdDJlZSAVdghpXLcQMtUurk0DreeeLbhkjcld9I6BEbAfNgVvTOHTjnUWnpBd8CDAClYp90pzf2twAAAABJRU5ErkJggg=='

trades=$(curl -s "https://bitcurex.com/api/pln/trades.json")
price=$(echo -n "$trades" | egrep -m 1 -o '"price": [0-9]+' | sed 's/"price": //' | sed -n '1 p')
amount=$(echo "$trades" | egrep -m 1 -o '"amount": [0-9]+(\.)?([0-9]{0,2})?' | sed 's/"amount": //' | sed -e 's/$/ BTC/' | sed -n '1 p')
timestamp=$(echo "$trades" | egrep -m 1 -o '"date": [0-9]+' | sed 's/"date": //' | sed 's/\"//g' | sed -n '1 p')
lastbuy=$(TZ=GMT-1 date -r "$timestamp" +"%T - %d.%m.%Y")

echo "$price zł | templateImage=$bitcoin_icon"

echo "---"

echo "↳ $amount - $lastbuy"

ticker=$(curl -s "https://bitcurex.com/api/pln/ticker.json")
high=$(echo -n "$ticker" | egrep -m 1 -o '"highest_tx_price_h": "[0-9]+"?' | sed 's/"highest_tx_price_h": //' | sed 's/\"//g' )
low=$(echo -n "$ticker" | egrep -m 1 -o '"lowest_tx_price_h": "[0-9]+"?' | sed 's/"lowest_tx_price_h": //' | sed 's/\"//g')

let range=high-low
let current=price-low

if [ $range -ne 0 ]
then result=$(echo $current $range 100 | awk '{ print $1/$2*$3 }')
	result=$(printf '%.*f\n' 0 "$result");
else
	result=50;
fi

prev_price=$(echo -n "$trades" | egrep -m 1 -o '"price": [0-9]+' | sed 's/"price": //' | sed -n '2 p')

if [ "$price" -gt "$prev_price" ]
then symbol=">"; fi
if [ "$price" -lt "$prev_price" ]
then symbol="<"; fi
if [ "$price" = "$prev_price" ]
then symbol="●"; fi

case "$result" in
	0) echo "$low zł  ◉ ————————— $high zł" ;;
	?) echo "$low zł  $symbol ————————— $high zł" ;;
	1?) echo "$low zł — $symbol ———————— $high zł" ;;
	2?) echo "$low zł —— $symbol ——————— $high zł" ;;
	3?) echo "$low zł ——— $symbol —————— $high zł" ;;
	4?) echo "$low zł ———— $symbol ————— $high zł" ;;
	5?) echo "$low zł ————— $symbol ———— $high zł" ;;
	6?) echo "$low zł —————— $symbol ——— $high zł" ;;
	7?) echo "$low zł ——————— $symbol —— $high zł" ;;
	8?) echo "$low zł ———————— $symbol — $high zł" ;;
	9?) echo "$low zł ————————— $symbol  $high zł" ;;
	100) echo "$low zł ————————— ◉  $high zł" ;;
esac
