#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>YouTube Ticker</bitbar.title>
// <bitbar.version>v1.1</bitbar.version>
// <bitbar.author>Zachary David Saunders</bitbar.author>
// <bitbar.author.github>ZacharyDavidSaunders</bitbar.author.github>
// <bitbar.desc>A MacOs Menu Bar (BitBar) plugin that displays your YouTube subscriber count.</bitbar.desc>
// <bitbar.dependencies>node.js</bitbar.dependencies>
// <bitbar.image>https://i.imgur.com/0aNslKx.png</bitbar.image>
/*jshint esversion: 6 */



//👋👋👋 Change the following varaibles to best suit your needs.
const key = "<YOUR-KEY-HERE>";
const channelName = "PewDiePie";
const color = "white";

//<--- DO NOT EDIT THE CODE BELOW THIS LINE. --->
const https = require('https');
const warningIcon = '⚠️';
const version = "v1.1";
const setupVideoLink = "https://www.youtube.com/watch?v=DYpf6gyd-bs";
const googleCloudPlatformSite = "http://console.developers.google.com";

const youTubeIcon = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADdcAAA3XAUI" +
"om3gAAAMESURBVFhH7VbPS1RRGP2CogkRRAqyoH8gMsW0aX75AyWREFr0CysIgiByIUKL9pEUVus2SZm7CFqUSom1iTZBUdaqWkjawtGiotQ5r/O9+Wbe9HwjL/tBwRw" +
"4vDv3ft853333vntHSijhvwHaZS3apMxplApyfRFWaIzGWtrK4CSkDgk5iJSczCTlLH8PZBIyTD4iJ9j/Gkl5wzGX2ra+FxrD510+BzRXNVRLNU2+ODiDCJP6MylJO83" +
"i5NnkY2MR+uMKNFhImoX1My5idkvBSnucFktOLiUFEIZBua4mtRfj0mt2P4KVrWaV9zUoSPR3ULX5dsedOlljth4Qk00Meoam4OQ8U+QOY8L6wpLaXOKJL3HZYrYenJj" +
"UMGgKjQGJOaphlDy9B+jaClSz3WBjYUhtFjDDdoPZeliISTMNvrozDEpWagFqOn4TmH0PXDzFWZVl+3aRy+UqOc4CsJCSDrP1QPH2XNCSxBxzBYwOIY+nD4HeDqCe/TU" +
"WE5Rr1M1Ij/1m64Gdne7rD1PA8KC5GxbmgTtXgcPVQC3HYxYfxBYyJUfN1gMPin1o9gX7mS/ghjn78PkDMNgHtHJZdK8EadBjMSnHzdYDP8NfL+DjDDB0AWgrL16AvoG" +
"EHDNbD7oEepqtbAm+AbevAIf4Zeg+WG4JspM8YLYe2LnbDQhTwEjBJnwyBvS0Z88FXX+NCcpVUttJicMvrtNsPThxaeIFEu4zfHALSE8D509QNJLt01kvl6vMjs9zv7W" +
"arQfEZTsLeBfqIDqzFzjCHb+N7Z02FobZg2iahdSarQcGVHHwbxzFzzmJzWbrQS8jXhT3Ql1GP2tsNO2xwMtIQeE/fh3zDQZfxwq+mnUs4hKDZgv/TLjJhfT/EcnRH1e" +
"gwbc7xz12WT3Mrjh4sdTztOpaTEg3CzrHdbvG5wgFHnOGL8m3LHKSnDJOah/jXmkMOcr2de72PuZ1c6xrPhpwA4aFI7KKM4zwUyufi0olRTd8qpeNFK5Sum32cXaVGqO" +
"xmmPpJZTwL0PkOxoj5CNBuCXOAAAAAElFTkSuQmCC";

renderPlugin();

function renderPlugin(){
  if(key == "<YOUR-KEY-HERE>"){
    console.log("CLICK ME TO CONFIGURE YOUTUBE TICKER |color:red image="+youTubeIcon );
    console.log("---");
    console.log("Please watch the following setup video. [Click to visit site] |href="+setupVideoLink);
    console.log("---");
    console.log("Visit Google's Cloud Platform site. [Click to visit site] |href="+googleCloudPlatformSite);
    console.log("Refresh|refresh=true");
  }else{
    getChannelId(function(id){
      getChannelStatistics(id,function(arrayOfStats){
        addCommas(arrayOfStats, function(formattedArrayOfStats){
          console.log(channelName + ": "+formattedArrayOfStats[0]+" Subscribers| color="+color+" image="+ youTubeIcon);
          console.log("---");
          console.log("Refresh|refresh=true");
          console.log("---");
          console.log("More Channel Statistics");
          console.log("-- Total Number Of Videos: "+formattedArrayOfStats[3]);
          console.log("-- Total Number Of Views: "+formattedArrayOfStats[1]);
          console.log("-- Total Number Of Comments: "+formattedArrayOfStats[2]);
          console.log("-- Channel ID: "+ id);
          console.log("---");
          console.log("Visit "+channelName+"'s channel. [Click to visit YouTube channel]|href=https://www.youtube.com/user/"+channelName);
          console.log("Visit "+channelName+"'s Social-Blade page. [Click to visit site]|href=https://socialblade.com/youtube/user/"+channelName);
          console.log("---");
          console.log("For more information, please see the GitHub repository. [Click to visit site] |href=https://github.com/ZacharyDavidSaunders/YouTubeTicker-BitBarPlugin");
          console.log("---");
          console.log("Version: "+version+"\n© Zachary David Saunders 2018 |size:10");
        });
      });
    });
  }
}

function getChannelId(callback){
  var channelId;
  https.get('https://www.googleapis.com/youtube/v3/channels?key='+key+'&forUsername='+channelName+'&part=id', (resp) => {
    var data = '';
    resp.on('data', (chunk) => {
      data += chunk;
    });

    resp.on('end', () => {
        var response = JSON.parse(data);
        channelId = response.items[0].id;
        callback(channelId);
      });

    }).on("error", (err) => {
      console.log(youTubeIcon+warningIcon+"Error:1");
    });
}

function getChannelStatistics(channelId,callback){
  var arrayOfStats = [];
  https.get('https://www.googleapis.com/youtube/v3/channels?part=statistics&id='+channelId+'&key='+key, (resp) => {
    var data = '';
    resp.on('data', (chunk) => {
      data += chunk;
    });

    resp.on('end', () => {
        var response = JSON.parse(data);
        var numberOfSubs = response.items[0].statistics.subscriberCount;
        var numberOfViews = response.items[0].statistics.viewCount;
        var numberOfComments = response.items[0].statistics.commentCount;
        var numberOfVideos = response.items[0].statistics.videoCount;
        arrayOfStats.unshift(numberOfSubs,numberOfViews,numberOfComments,numberOfVideos);
        callback(arrayOfStats);
      });

    }).on("error", (err) => {
      console.log(youTubeIcon+warningIcon+"Error:2");
    });
}

function addCommas(statisticArray,callback){
  for(var i =0; i < statisticArray.length; i++){
    statisticArray[i] = statisticArray[i].toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
  }
  callback(statisticArray);
}
