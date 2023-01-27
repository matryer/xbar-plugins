#!/usr/local/bin/node

// <xbar.title>BrewFather Batch status</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>Joost Slijkoort</xbar.author>
// <xbar.author.github>JoostSlijkoort</xbar.author.github>
// <xbar.desc>Shows the current status of your batch fermentation in BrewFather.</xbar.desc>
// <xbar.dependencies>node</xbar.dependencies>
// <xbar.image>https://i.imgur.com/bJuU1Xp.png</xbar.image>

//  <xbar.var>string(VAR_USERID="ABCWHATEVER"): Your user ID, you see it when creating an API key.</xbar.var>
//  <xbar.var>string(VAR_APIKEY=123WHATEVER): Your API key.</xbar.var>

if(!process.env.VAR_USERID || !process.env.VAR_APIKEY) {
  console.log('🚱 please fill in your credentials in the plugin settings.');
  return;
}

const https = require('https');

const userid = process.env.VAR_USERID;
const apiKey = process.env.VAR_APIKEY;
const options = {
  headers: {
    'Authorization': 'Basic ' + Buffer.from(userid + ':' + apiKey).toString('base64')
  }
}

let brews = [];

function fetchBatchDetails(id) {
  return new Promise((resolve, reject) => {
        https.get(`https://api.brewfather.app/v1/batches/${id}/readings/last`, options, (res) => {
          let responseBody = '';

          res.on('data', (chunk) => {
            responseBody += chunk;
          });

          res.on('end', () => {
            const responseJSON = JSON.parse(responseBody);
            resolve({...responseJSON, _id: id});
          })

        }).on('error', (e) => {
          reject(e);
        });
    });
}

function getProgressPercentage(max,min,v) {
  const progress = (max - v) / ((max - min) / 100);
  return Math.round(progress * 10) / 10;
}

function mapBatchProgress(batches, batchProgresses) {
  return batches.map(batch => {
      const matchingProgress = batchProgresses.find((batchProgress) => batchProgress._id == batch._id);
      const sg = matchingProgress.sg;

      return {
        ...batch,
        progressPercentage: getProgressPercentage(batch.measuredOg || batch.estimatedOg, batch.estimatedFg, sg),
        sg,
      }
    }
  )
}

function fetchBatches() {
  https.get('https://api.brewfather.app/v1/batches?status=Fermenting&include=estimatedFg,estimatedOg,measuredOg', options, (resp) => {
    let data = '';

    // A chunk of data has been received.
    resp.on('data', (chunk) => {
      data += chunk;
    });

    // The whole response has been received. Print out the result.
    resp.on('end', () => {
      batches = JSON.parse(data);

      if(batches.length < 1) {
        console.log(':beer: No batches')
        return;
      }

      Promise.all(batches.map(v => fetchBatchDetails(v._id))).then((batchProgresses) => {
        const completeMap = mapBatchProgress(batches, batchProgresses);
        const titleString = completeMap.map(v => `${v.recipe.name}@${v.sg} (${v.progressPercentage}%)`).find((v, i) => i == 0);

        if(completeMap.length === 1) {
          console.log(`:beer: ${titleString}|dropdown=false`);
          return;
        }

        const contentString = completeMap.map(v => `\n${v.recipe.name}@${v.sg} (${v.progressPercentage}%)`).filter((v, i) => i > 0).join('');

        console.log(`:beer: ${titleString}|dropdown=false \n---${contentString}`);

      })
    });

  }).on("error", (err) => {
    console.log(':beer:Error|dropdown=false \n---\n ' + err.message);
  });
}

fetchBatches()
