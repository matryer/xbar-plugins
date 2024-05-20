#!/usr/bin/env bash
#  <xbar.title>Nightscout Blood Sugar</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Paul Barrass</xbar.author>
#  <xbar.author.github>pdaddyo</xbar.author.github>
#  <xbar.desc>Shows graph and latest data from your glucose sensor via Nightscout.  Requires `brew install fx && brew install gnuplot`</xbar.desc>
#  <xbar.image>https://github.com/pdaddyo/bitbar-nightscout/assets/7074964/f512b983-cca0-4adc-8583-355d2083ba8b</xbar.image>
#  <xbar.dependencies>curl, gnuplot, fx</xbar.dependencies>
#  <xbar.var>string(NIGHTSCOUT_URL=""): Your Nightscout site URL e.g. https://my-nightscout.com </xbar.var>


if [ -n "$NIGHTSCOUT_URL" ]; then
  export NIGHTSCOUT_URL
else
  echo "Missing NIGHTSCOUT_URL plugin option! | color=red"
  exit 0
fi

export PATH='/usr/local/bin:/usr/bin:/opt/homebrew/bin:$PATH'

TMPFILE=/tmp/nightscout-plot.png

curl -s $NIGHTSCOUT_URL/api/v1/entries.json\?count\=12 | fx 'x => x.reverse().map(d=>d.sgv/18).join(`\n`)' \
| gnuplot -e "
    set terminal pngcairo size 72,36 transparent;
    set output '$TMPFILE';
    unset title;
    unset xlabel;
    unset ylabel;
    unset key;
    unset xtics;
    set ytics format '';
    set border 0;
    set lmargin 0;
    set rmargin 1;
    set tmargin 0.75;
    set bmargin 0.75;
    set object 1 rect from graph 0, 0 to graph 1, first 4 fc rgb 'red' fillstyle transparent solid 0.2;
    set object 1 front lw 0;
    set object 2 rect from graph 0, first 10 to graph 1, first 24 fc rgb 'orange' fillstyle transparent solid 0.2;
    set object 2 front lw 0;
    plot '-' smooth csplines with lines linewidth 2 linecolor rgb '#eeeeee' notitle
"


curl -s $NIGHTSCOUT_URL/pebble | fx \
   'x => ({sgv: x.bgs[0].sgv, delta: x.bgs[0].bgdelta, plusminus: x.bgs[0].bgdelta >= 0 ? `+` : ``, mins: Math.round((x.status[0].now - x.bgs[0].datetime) / 1000 / 60), direction: x.bgs[0].direction})' \
   'x => ({...x, arrow: x.direction === `Flat` ? `→` : x.direction === `FortyFiveDown` ? `↘` :  x.direction === `SingleDown` ? `↓` :  x.direction === `DoubleDown` ? `↓↓` :     x.direction === `FortyFiveUp` ? `↗` :     x.direction === `SingleUp` ? `↑` :     x.direction === `DoubleUp` ? `↑↑` : x.direction, delta: `${x.plusminus}${x.delta}`, color: x.sgv < 4 ? "red" : x.sgv > 12 ? "orange" : "#eeeeee"})' \
   'x => (x.mins > 15 ? `${x.mins}m  (${x.sgv})  | color=red | size=11` : `${x.sgv} ${x.arrow}  ${x.delta}  ${x.mins}m | size=13 | color=${x.color} | image=`) + `\n` +`---\n` + `Open Nightscout | href=NIGHTSCOUT_URL`' \
   | sed "s|image=|image=$(base64 -i $TMPFILE)|" \
   | sed "s|NIGHTSCOUT_URL|$NIGHTSCOUT_URL|"

