#!/usr/bin/env /usr/local/bin/node

/*
 * <xbar.title>WakatimeBar</xbar.title>
 * <xbar.version>v1.0.1</xbar.version>
 * <xbar.author>Amir Canto</xbar.author>
 * <xbar.author.github>amircp</xbar.author.github>
 * <xbar.image>https://i.imgur.com/p5QSUcH.png</xbar.image>
 * <xbar.desc>Wakatime plugin for Bitbar, check your coding activity from your bar.</xbar.desc>
 * <xbar.dependencies>node</xbar.dependencies>
 *
 */

const WAKATIME_API_KEY = ''; // Get your API Key here: https://wakatime.com/settings/api-key
const https = require('https');


let currentDate =  new Date().toLocaleString().slice(0,10).replace(/,/g,'');

const icon  = '/9j/2wCEAAEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQECAgICAgICAgICAgMDAwMDAwMDAwMBAQEBAQEBAgEBAgICAQICAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDA//dAAQAAv/uAA5BZG9iZQBkwAAAAAH/wAARCAAQABADABEAAREBAhEB/8QAWAABAQEAAAAAAAAAAAAAAAAACAAFAQEAAAAAAAAAAAAAAAAAAAAAEAABBAMBAAMBAAAAAAAAAAAHAwQFBgIICQETFxglEQEAAAAAAAAAAAAAAAAAAAAA/9oADAMAAAERAhEAPwB+h8bU7uA5ltsN6zNcWOgBQOhK1p53aQD8uW8MDowxI1m7JGyRZNdmpz6AmySXig7Hcq5h4RV23wjm8e7TbKuPFEksQwS3U6PxHnZTZrQ0hmD8GAIuhcD9FNOyZdyCWwTTIA0WJpDNCNrHfby6nJeAPQTkbbEL2eEj3btFTGYYtnSyGaThuoH/0HdSreOuPQ6NnOro6DrjbeV9hIBNJmqO0lJE91MIZhRSSyB9kyGvGx1aHjSRuA1JAsvkh46h5hPH+nl6oq3xbpt8MlQiBdh/2PGoc5v84gfbqlyviriML5thtPbBPdQgGVw8MCCkSI3XnWauXdlDW2+EslXiJ8cScv4n57GZfEotiumuoqgH/9k=';
let url = 'https://wakatime.com/api/v1/users/current/summaries?api_key='+  WAKATIME_API_KEY+'&start='+ currentDate + '&end='+ currentDate;


https.get(url,(res) => {
    let body = "";

    res.on("data", (chunk) => {
        body += chunk;
    });

    res.on("end", () => {
        try {
      

            let wakaData = JSON.parse(body);
            if(wakaData  && wakaData.data && wakaData.data.length > 0){

                let categories = wakaData.data[0].categories || undefined;
                if(categories){
                    if(categories[0].text){
                        console.log(categories[0].text + ' | image='+icon);
                    } else {
                        console.log('00:00:00 hrs | image='+icon);
                    }
                   
                }   else {
                    console.log('00:00:00 hrs | image='+icon);
                }
                

            } else {
                console.log('00:00:00 hrs | image='+icon);
            }
            // do something with JSON
        } catch (error) {
            console.log('00:00:00 hrs | image='+icon);
        };
    });

}).on("error", (error) => {
    console.error(error.message);
});


