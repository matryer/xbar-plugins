#!/usr/local/bin/node
//  -*- coding: utf-8 -*-xbar
//  <xbar.title>WLED</xbar.title>
//  <xbar.version>v1.0</xbar.version>
//  <xbar.author>Billy Jacoby</xbar.author>
//  <xbar.author.github>billyjacoby</xbar.author.github>
//  <xbar.desc>Show and manage WLED devices from the menu bar.</xbar.desc>
//  <xbar.dependencies>node,wled,axios</xbar.dependencies>
// <xbar.image>https://imgur.com/a/a5gBbjd</xbar.image>
//  Please install the npm dependencies in a folder named "wled" - I'm open to better ways of doing this too...

// <xbar.var>string(VAR_HOST=""): WLED IP Address or hostname</xbar.var>
const axios = require("./wled/node_modules/axios");
const bitbar = require("./wled/node_modules/bitbar");
let VARS;

try {
  VARS = require("./wled.1m.js.vars.json");
} catch (e) {
  //* If no vars are defined continue...
  return;
}

//* Variables
let host;
if (VARS) {
  host = VARS.VAR_HOST;
} else {
  host = "http://wled.local";
}
const url = host + "/json";
let lightData = {};

function componentToHex(c) {
  var hex = c.toString(16);
  return hex.length == 1 ? "0" + hex : hex;
}

function rgbToHex([r, g, b]) {
  return "#" + componentToHex(r) + componentToHex(g) + componentToHex(b);
}

async function getData() {
  try {
    const response = await axios({
      method: "get",
      url,
    });
    lightData = response.data;
    let RGB = lightData.state.seg[0].col[0];

    bitbar([
      {
        text: "WLED",
        color: lightData.state.on ? rgbToHex(RGB) : "gray",
      },
      bitbar.separator,
      {
        text: `${lightData.info.name || "WLED"} - ${
          lightData.state.on ? "On" : "Off"
        }`,
        color: lightData.state.on ? rgbToHex(RGB) : "gray",
        size: 16,
        href: `${host}/win\&T=${lightData.state.on ? "0" : "1"}`,
        refresh: true,
      },
      {
        text: "Open Web UI",
        href: `${host}`,
      },
    ]);
  } catch (e) {
    if (e.code === "ECONNREFUSED") {
      bitbar([
        {
          text: "!!WLED",
          color: "red",
        },
        bitbar.separator,
        {
          text: "Error - bad connection...",
        },
      ]);
      console.log(
        "Please check host name and be sure to include 'http://', then refresh the plugin"
      );
    } else {
      bitbar([
        {
          text: "!!WLED",
          color: "red",
        },
        bitbar.separator,
        {
          text: "Error:",
        },
      ]);
      console.log(e.message);
    }
  }
}
getData();
