#!/usr/bin/env node
/* jshint esversion: 6 */

// <xbar.title>emojiWeather</xbar.title>
// <xbar.version>v2.0</xbar.version>
// <xbar.author>Chandler Davidson, Luis Cruz</xbar.author>
// <xbar.author.github>Chandler-Davidson, sprak3000</xbar.author.github>
// <xbar.desc>Displays weather info as an emoji.</xbar.desc>
// <xbar.image>https://static1.squarespace.com/static/5818f78ef5e2314f65b76331/t/59ded8e6bce1769ff7340715/1507776754374/emojiWeather?format=1500w</xbar.image>
// <xbar.dependencies>axios, API key from openweathermap.org</xbar.dependencies>
// <xbar.abouturl>https://github.com/Chandler-Davidson/emojiWeather/blob/master/emojiWeather.1h.js</xbar.abouturl>
// <xbar.var>string(VAR_API_KEY): API key from https://home.openweathermap.org/api_keys</xbar.var>
// <xbar.var>string(VAR_CITY="Huntsville"): Get weather data for this city</xbar.var>


const axios = require('axios');

const apiKey = process.env.VAR_API_KEY;
const city = process.env.VAR_CITY;
const url = `https://api.openweathermap.org/data/2.5/weather?q=${city}&units=imperial&appid=${apiKey}`;

const emojis = [
    ':cry:',        // No connection / error
    '::',           // Not used
    ':zap:',        // Thunderstorm
    ':umbrella:',   // Drizzle
    '::',           // Not used
    ':umbrella:',   // Rain
    ':snowflake:',  // Snow
    ':cyclone:',    // Extreme
    ':cloud:'];     // Cloudy

const getEmoji = (weatherId) => {
    // Weather ID mapping: https://openweathermap.org/weather-conditions
    switch (weatherId) {
        // Clear / sunny is the first value in the 8XX range.
        case 800:
            return ':sunny:';
        default:
            // Otherwise, we can use the hundreds place to get the range.
            const index = Math.floor(weatherId / 100);
            return emojis[index];
    }
};

axios.get(url)
    .then(function (response) {
        const weather = response.data;
        const id = weather.weather[0].id; // Separate the weather ID

        console.log(getEmoji(id));        // Print the emoji
    })
    .catch(function (error) {
        // If no connection or other error, so sad...
        console.log(emojis[0]);
        console.log('---');
        console.log(error);
    });
