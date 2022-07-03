#!/usr/bin/env -S -P/${HOME}/.deno/bin:/opt/homebrew/bin deno run --allow-net --allow-read --allow-write
/*
*  <xbar.title>NWS Weather and Alerts</xbar.title>
*  <xbar.version>v1.0</xbar.version>
*  <xbar.author>Theo Gainey</xbar.author>
*  <xbar.author.github>theogainey</xbar.author.github>
*  <xbar.desc>Weather and Severe Weather Alerts powered by The National Weather Service API</xbar.desc>
*  <xbar.image>https://www.theogainey.com/example.png</xbar.image>
*  <xbar.dependencies>deno</xbar.dependencies>
*  <xbar.abouturl>https://github.com/theogainey/xbar-weatheralerts</xbar.abouturl>
*  <xbar.var>string(VAR_ZIP=""): Enter Zip code for improved accuracy</xbar.var>
*  <xbar.var>boolean(VAR_USEZIP=true): Weather to use Zip code for location or not</xbar.var>
*  <xbar.var>string(VAR_Minor_Color=""): Enter 6 digit hex color to change display color of minor weather alerts</xbar.var>
*  <xbar.var>string(VAR_Moderate_Color=""): Enter 6 digit hex color to change display color of moderate weather alerts</xbar.var>
*  <xbar.var>string(VAR_Severe_Color=""): Enter 6 digit hex color to change display color of severe weather alerts</xbar.var>
*  <xbar.var>string(VAR_Extreme_Color=""): Enter 6 digit hex color to change display color of extreme weather alerts</xbar.var>
*/

import { separator, xbar } from 'https://deno.land/x/xbar@v2.0.0/mod.ts';
import {
  fetchLocation,
  fetchWeatherData,
  formatForXbar,
} from 'https://raw.githubusercontent.com/theogainey/xbar-weatheralerts/main/src/index.ts';

fetchLocation()
  .then((loc) => fetchWeatherData(loc))
  .then((data) => formatForXbar(data))
  .then((alerts) => xbar(alerts))
  .catch((err) =>
    xbar([
      {
        text: 'error',
      },
      separator,
      {
        text: err,
      },
    ])
  );
  