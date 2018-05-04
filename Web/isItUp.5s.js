#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>Is It Up?</bitbar.title>
// <bitbar.version>v1.1</bitbar.version>
// <bitbar.author>Zachary David Saunders</bitbar.author>
// <bitbar.author.github>ZacharyDavidSaunders</bitbar.author.github>
// <bitbar.desc>A MacOs Menu Bar (BitBar) plugin that allows you to check whether or not a website is currently up (online).</bitbar.desc>
// <bitbar.dependencies>node.js</bitbar.dependencies>
// <bitbar.image>https://i.imgur.com/BxHFJMn.png</bitbar.image>
/*jshint esversion: 6 */



//👋👋👋 Feel free to change the following varaibles to best suit your needs.
var upMessage = 'Yes ✅';
var unknownMessage = 'Error, click for details ⚠️';
var downMessage = 'No 🔥';
var website = "www.google.com";



//<--- DO NOT EDIT THE CODE BELOW THIS LINE. --->
const https = require('https');

var version = "v1.1";

renderPlugin();

function renderPlugin(){
  checkWebsite(function(status){
    var menuBarIcon = "Is \""+website+"\" up?: " + getMessage(status);
    if(status === "up"){
      console.log(menuBarIcon+" |color:green");
      console.log("---");
      console.log("Visit \""+website+"\". [Click to visit site] |href=https://"+website);
    }else if (status === "down"){
      console.log(menuBarIcon+" |color:red");
    }else{
      console.log(menuBarIcon+" |color:yellow");
      console.log("---");
      console.log("Error: Unable to contact \""+ website+"\".");
      console.log("Please verify that you entered the website correctly (remember, you don't need a www/http/https prefix).");
      console.log("If this problem persists, isitdown.site's API may be unavailable.");
    }

    console.log("---");
    console.log("Powered by \"isitdown.site\". [Click to visit site] |href=https://isitdown.site");
    console.log("For more information, please see the github repository. [Click to visit site] |href=https://github.com/ZacharyDavidSaunders/IsItUp-BitBarPlugin");
    console.log("---");
    console.log("Version: "+version+"\n© Zachary David Saunders 2018 |size:10");
  });
}

function checkWebsite(callback){
  var result;
  https.get('https://isitdown.site/api/'+website, (resp) => {
    var data = '';
    resp.on('data', (chunk) => {
      data += chunk;
    });

    resp.on('end', () => {
      try{
        var response = JSON.parse(data);
        if(response.isitdown == false){
          result = "up";
        }else if (response.isitdown == true){
          result = "down";
        }else{
          result = "unknown";
        }
      }catch(error){
        result = "unknown";
      }
      callback(result);
    });

  }).on("error", (err) => {
      result = "unknown";
      callback(result);
  });
}

function getMessage(status){
  if(status === "up"){
    return upMessage;
  }else if (status === "down"){
    return downMessage;
  }else{
    return unknownMessage;
  }
}
