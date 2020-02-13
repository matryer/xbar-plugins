#!/usr/bin/env /usr/local/bin/node

/**
 * <bitbar.title>CoronaVirus Satus</bitbar.title>
 * <bitbar.version>v1.0</bitbar.version>
 * <bitbar.author>MrFhitz</bitbar.author>
 * <bitbar.author.github>mrfhitz</bitbar.author.github>
 * <bitbar.desc>Get Coronavirus status (infected / death) from arcgis.com</bitbar.desc>
 * <bitbar.image>https://i.imgur.com/KrHJPJj.png</bitbar.image>
 * <bitbar.dependencies>node</bitbar.dependencies>
 */

const https = require('https');

const endpoint = encodeURI(
    "https://services1.arcgis.com/0MSEUqKaxRlEPj5g/arcgis/rest/services/ncov_cases/FeatureServer/1/query?f=json&where=1=1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&outStatistics=[" +
    '{"statisticType":"sum","onStatisticField":"Deaths","outStatisticFieldName":"death"},' +
    '{"statisticType":"sum","onStatisticField":"Confirmed","outStatisticFieldName":"infected"}' +
    ']&cacheHint=false'
);

https.get(endpoint, (resp) => {
    let data = '';

    // A chunk of data has been recieved.
    resp.on('data', (chunk) => {
        data += chunk;
    });

    // The whole response has been received. Print out the result.
    resp.on('end', () => {
        var res = JSON.parse(data);
        var infected = res.features[0].attributes.infected;
        var death = res.features[0].attributes.death;

        if(Number.isInteger(infected) && infected > 0) {
            infected = infected.toString().replace(/\B(?=(\d{3})+(?!\d))/g, " ");
            death = death.toString().replace(/\B(?=(\d{3})+(?!\d))/g, " ");
        }else {
            infected = 0;
            death = 0;
        }
        console.log(
            infected + ' 🦠 / ' + death + ' ☠️'
        );
        console.log('---');
    });
});
