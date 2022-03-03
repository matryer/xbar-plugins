#!/usr/local/bin/node

//  <xbar.title>RL Clock</xbar.title>
//  <xbar.version>v1.0</xbar.version>
//  <xbar.author>Oliver van den Bosch</xbar.author>
//  <xbar.author.github>ovandenbosch</xbar.author.github>
//  <xbar.desc>An xbar plugin that provides a quick look at information about the Roxbury Latin school day.</xbar.desc>
//  <xbar.image>https://upload.wikimedia.org/wikipedia/en/6/66/Roxbury_Latin_School_coa.png</xbar.image>
//  <xbar.dependencies>node</xbar.dependencies>

import fetch from "node-fetch";
function getInfo() {
  let blockInfoUrl = "https://mod-clock-api.roxburylatin.org/daytype.json";
  let scheduleUrl =
    "https://mod-clock-api.roxburylatin.org/todays_schedule.json";

  fetch(blockInfoUrl)
    .then((res) => {
      return res.json();
    })
    .then((data) => {
      let dayType = data["dayType"];
      let hallLength = data["hallLength"];
      let currentBlock = data["currentBlock"]["block"];
      let remainingMin = data["remainingMin"];
      console.log(`${currentBlock} Block - ${remainingMin} minutes remaining`);
      console.log("---");
      if (hallLength > 0) {
        console.log(`Hall Length ${hallLength}`);
      }
      console.log(`${dayType} Day`);
    });

  fetch(scheduleUrl)
    .then((res) => {
      return res.json();
    })
    .then((data) => {
      console.log("---")
      let periods = data["periods"]
      periods.forEach(period => {
        let pName = period["name"]
        let block = period["block"]
        if (block == undefined) {
          block = "";
        } else {
          block += " Block"
        }
        let start = period["start"]
        let end = period["end"]
        console.log(`${pName} - ${block} ${start} - ${end} | href=`)
      });
      console.log(
        "App version: v1.0 🦊 | href=https://github.com/ovandenbosch/xbar-rlclock"
      );
    });
}

getInfo();
