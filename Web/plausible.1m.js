#!/usr/bin/env /usr/local/bin/node

// Metadata allows your plugin to show up in the app, and website.
//
//  <xbar.title>Plausible tracker</xbar.title>
//  <xbar.version>v1.0</xbar.version>
//  <xbar.author>Tom Schönmann</xbar.author>
//  <xbar.author.github>flaming-codes</xbar.author.github>
//  <xbar.desc>See who's on your site at-a-glance.</xbar.desc>
//  <xbar.image>http://www.hosted-somewhere/pluginimage</xbar.image>
//  <xbar.dependencies>node</xbar.dependencies>
//  <xbar.abouturl>http://flaming.codes</xbar.abouturl>

//
// User data.
// ! Update the following values with your data.
//

const SITE_ID = "";
const API_KEY = "";

// Define custom icons for each step,
// where each value defines the lower
// bound for its activation.
const stepIcons = [
  [0, "👤"],
  [5, "💫"],
  [10, "⭐️"],
  [50, "🌟"],
  [100, "⚡️"],
  [500, "💥"],
];

//
// Imports.
//

const https = require("https");

//
// Utilities.
//

const linksMenu = [
  "---",
  `🔮 Open dashboard | href=https://plausible.io/${SITE_ID}`,
  `🔥 Made by flaming.codes | href=https://flaming.codes`,
];

async function fetcher() {
  return new Promise((resolve, reject) => {
    let body = "";
    const request = {
      host: "plausible.io",
      path: `/api/v1/stats/realtime/visitors?site_id=${SITE_ID}`,
      method: "GET",
      headers: {
        Authorization: `Bearer ${API_KEY}`,
      },
    };

    try {
      const req = https.get(request, (res) => {
        res.on("data", (data) => {
          body += data;
        });
        res.on("end", () => {
          resolve(JSON.parse(body));
        });
      });

      req.on("error", (error) => {
        console.error(error);
      });

      req.end();
    } catch (error) {
      reject(error);
    }
  });
}

function findCurrentStepIcon(count) {
  const tuple = stepIcons.reverse().find((tuple, i) => count >= tuple[0]);

  // Branch out early
  if (!tuple) return "";
  return tuple[1];
}

function renderData(props) {
  const { data } = props;
  const output = [`${findCurrentStepIcon(data)} ${data}`, ...linksMenu];

  console.log(output.join("\n"));
}

function renderError(props) {
  const { error } = props;
  const output = [
    "❔",
    "---",
    "No data accessible",
    "Please check your user data",
    ...linksMenu,
  ];

  console.log(output.join("\n"));
}

async function render() {
  const { data, error } = await fetcher()
    .then((data) => ({ data }))
    .catch((error) => ({ error }));

  if (data >= 0) return renderData({ data });
  if (error) return renderError({ error });
}

render();
