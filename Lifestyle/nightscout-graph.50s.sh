#!/usr/bin/env bash
#  <xbar.title>Nightscout Graph and Trend/xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Paul Barrass</xbar.author>
#  <xbar.author.github>pdaddyo</xbar.author.github>
#  <xbar.desc>Shows graph and latest data from your glucose sensor via Nightscout.  Requires `brew install fx && brew install gnuplot`</xbar.desc>
#  <xbar.image>https://github.com/pdaddyo/bitbar-nightscout/assets/7074964/f512b983-cca0-4adc-8583-355d2083ba8b</xbar.image>
#  <xbar.dependencies>curl, gnuplot, fx</xbar.dependencies>
#  <xbar.var>string(NIGHTSCOUT_URL=""): Your Nightscout site URL e.g. https://my-nightscout.com </xbar.var>

export PATH='/usr/local/bin:/usr/bin:/opt/homebrew/bin:$PATH'

if [ -n "$NIGHTSCOUT_URL" ]; then
  export NIGHTSCOUT_URL
else
  echo "Missing NIGHTSCOUT_URL plugin option! | color=red"
  exit 0
fi


TMP_PNG_FILE=/tmp/nightscout-plot.png
TMP_JSON_FILE=/tmp/nightscout-data.json


curl -s $NIGHTSCOUT_URL/api/v1/entries.json\?count\=24 > $TMP_JSON_FILE

MIN_SGV=$(fx $TMP_JSON_FILE 'x => Math.min(x.map(d=>d.sgv/18).reduce((a,b)=>Math.min(a,b)), 4)')
MAX_SGV=$(fx  $TMP_JSON_FILE 'x => Math.max(x.map(d=>d.sgv/18).reduce((a,b)=>Math.max(a,b)), 10)')

fx $TMP_JSON_FILE 'x => x.reverse().map(d=>d.sgv/18).join(`\n`)' \
| gnuplot -e "
    set terminal pngcairo size 64,36 transparent;
    set output '$TMP_PNG_FILE';
    unset title;
    unset xlabel;
    unset ylabel;
    unset key;
    unset xtics;
    unset ytics;
    set border 0;
    set lmargin 0;
    set rmargin 0.5;
    set tmargin 0.5;
    set bmargin 0.5;
    set yrange [$MIN_SGV:$MAX_SGV];
    set object 1 rect from graph 0, 0 to graph 1, first 4 fc rgb '#dc2626' fillstyle transparent solid 0.3;
    set object 2 rect from graph 0, first 10 to graph 1, first 24 fc rgb '#ea580c' fillstyle transparent solid 0.3;
    set object 2 front lw 0;
    plot '-' smooth csplines with lines linewidth 2 linecolor rgb '#e2e8f0' notitle
"


curl -s $NIGHTSCOUT_URL/pebble | fx \
   'x => ({sgv: x.bgs[0].sgv, delta: x.bgs[0].bgdelta, plusminus: x.bgs[0].bgdelta >= 0 ? `+` : ``, mins: Math.round((x.status[0].now - x.bgs[0].datetime) / 1000 / 60), direction: x.bgs[0].direction})' \
   'x => ({...x, arrow: x.direction === `Flat` ? `→` : x.direction === `FortyFiveDown` ? `↘` : x.direction === `SingleDown` ? `↓` : x.direction === `DoubleDown` ? `↓↓` : x.direction === `FortyFiveUp` ? `↗` : x.direction === `SingleUp` ? `↑` : x.direction === `DoubleUp` ? `↑↑` : x.direction, delta: `${x.plusminus}${x.delta}`, color: x.sgv < 4 ? "red" : x.sgv > 12 ? "orange" : "#e2e8f0"})' \
   'x => (x.mins > 15 ? `${x.mins}m  (${x.sgv}) | color=red | size=11` : `${x.sgv} ${x.arrow}  ${x.delta}  ${x.mins}m | size=13 | color=${x.color} | image=`) + `\n` +`---\n` + `Open Nightscout | href=NIGHTSCOUT_URL`' \
   | sed "s|image=|image=$(base64 -i $TMP_PNG_FILE)|" \
   | sed "s|NIGHTSCOUT_URL|$NIGHTSCOUT_URL|"

