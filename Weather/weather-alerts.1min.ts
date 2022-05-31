#!/usr/bin/env -S -P/${HOME}/.deno/bin:/opt/homebrew/bin deno run --allow-net 
/*
*  <xbar.title>NWS Weather and Alerts</xbar.title>
*  <xbar.version>v1.0</xbar.version>
*  <xbar.author>Theo Gainey</xbar.author>
*  <xbar.author.github>theogainey</xbar.author.github>
*  <xbar.desc>Weather and Severe Weather Alerts powered by The National Weather Service API</xbar.desc>
*  <xbar.image>https://github.com/theogainey/xbar-weatheralerts/blob/main/example.png?raw=true</xbar.image>
*  <xbar.dependencies>deno</xbar.dependencies>
*  <xbar.abouturl>https://github.com/theogainey/xbar-weatheralerts</xbar.abouturl>
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
