#!/usr/bin/env /usr/local/bin/node
// <xbar.title>Wakatime Duration</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>Tyler Torola</xbar.author>
// <xbar.author.github>TJTorola</xbar.author.github>
// <xbar.desc>Display wakatime duration for the day</xbar.desc>
// <xbar.dependencies>node</xbar.dependencies>

// VARIABLES
// <xbar.var>string(API_KEY=""): Your Wakatime API key.</xbar.var>
// <xbar.var>string(PROJECT_FILTER=""): Specific project you want to filter for. For all projects leave blank.</xbar.var>
// <xbar.var>string(LABEL="W: "): Label to render before time total.</xbar.var>

const https = require("https");
const {
  API_KEY,
  PROJECT_FILTER,
  LABEL,
} = require("./wakatime.1m.js.vars.json");

const ENDPOINT = "https://wakatime.com/api/v1/users/current/durations";
const LABEL = LABEL || 'W: ';

if (!API_KEY) {
  throw new Error('No API key set!')
}

const get = (url) =>
  new Promise((respond, reject) => {
    https
      .get(url, (res) => {
        let data = [];

        res.on("data", (chunk) => {
          data.push(chunk);
        });

        res.on("end", () => {
          respond(JSON.parse(Buffer.concat(data).toString()));
        });
      })
      .on("error", reject);
  });

const main = async () => {
  const now = new Date();
  const params = {
    date: `${now.getFullYear()}-${now.getMonth() + 1}-${now.getDate()}`,
    api_key: API_KEY,
  };

  const paramString = Object.entries(params)
    .map(([k, v]) => `${k}=${v}`)
    .join("&");

  const { data } = await get(ENDPOINT.concat(`?${paramString}`));
  const filteredData = PROJECT_FILTER
    ? data.filter(({ project }) => project === PROJECT_FILTER)
    : data;

  const total = filteredData.reduce((acc, { duration }) => acc + duration, 0);
  const hh = Math.floor(total / (60 * 60)).toString().padStart(2, '0');
  const mm = Math.floor((total / 60) % 60).toString().padStart(2, '0');
  console.log(`${LABEL}${hh}:${mm}`);
};

main();
