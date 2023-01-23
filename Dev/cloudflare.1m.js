#!/usr/bin/env /usr/local/bin/node


/* jshint esversion: 6 */

/*
 * <xbar.title>Cloudflare Development Mode</xbar.title>
 * <xbar.version>v1.0</xbar.version>
 * <xbar.author>Aaron Crawford</xbar.author>
 * <xbar.author.github>aaroncrawford</xbar.author.github>
 * <xbar.image>iVBORw0KGgoAAAANSUhEUgAAAMgAAADICAMAAACahl6sAAAAflBMVEX////959X84Mf82Ln6wI/7yJ3+7+P70Kv5sXP3mkn1gh/2ii34oVf5uYH4qWX2kjv+9/H+5sP7rkD7s0z8wnD91p/94bf+9ef8x3z8uFj9zIj/+vP90ZT8vWT+8Nv94Mf93Kv6wY/+68/+4bf/9ef916D926v3mUn96NX90pS1tvFHAAACyElEQVR4AezBgQAAAACAoP2pF6kCAAAAAAAAAAAAAADmzi4QHAWCMIz+eKcaS8WJ28r9D7gu8VATminm3eCjFajB84Pwp8j30VVxYnp0yqZh92rirEe32DxBhyQ9us9mcVcyCnrCxNAvKKgGU0K3OKV6bATNEku1pSW0Kg1JWF9rR4+EEmjkWRLLdHcImO53KC0pLb1RpHudCwRQxNDbWQ9qJPSKHrSILb0khBI5vciDCj69KoUKBb0sUL3SBQq1A6L/+hhHeUE/FXlS4oeAmpCiXX5Op4wP5NQI751fnIxHzch0XnDlrNLdSc77IB2U6Z9XuvatHrmGVoTkWqHzTVYuA/r8w2A4HI0ncCQi52Jgyv/Mqr62+5RgrfOZ2WiOpsXkXAL0+VI1V3CGyNgSWPCVwahre5YBMOMblis0KCXX1sCKbxt3KaQAsOE7qg6FRACGfM903pUQWwLg+5ZNlWTk1hbAih+ounGwFyWAHbstKf0wS3uut6yzEAd7Vxml5F6EGiGDCd4sNtQGg1/m/NjQxb9mBx3Akp1MrsBSKyL8tefHBnPIwVAr7BpA3SEZtftLUCIrcWK+lA+Jig4T49x82vAqcdtx8gn8yq6a8X1LR+sjvbK9Fq2vxXjksNvt+sfjcTEcDpd8ZgKJQHDRc27Dpz65+PJToA0LPjV0svHa9EK+vZasr+Gx3Q+bH1OrGg6HMz6H+mJqQ2Fu5cz31Ywf2okHxL28xLlDNeBnxvIBce/zWcl8wTWMUFdELZbgv92A65i2fxbK/kkfuZ4haiqJ3mNIKm46xKdWlYIOWUhIrVoL5pUsxLxDyI4dhKTth8xnHyDki2Ri/fRVaQiA+YAFRjpDcumA8E5nSARgxhIT1JRTm2JgxxIzlVct+iyeWRVq81tUApjvJL63BwcEAAAQAID8X+0FUAUAAAAAAAAAAAAAAADQLQGpkWNx7+1PhwAAAABJRU5ErkJggg==</xbar.image>
 * <xbar.desc>Enable or Disable Development Mode from BitBar.  Plugin will need to be edited to include your API key and email to work.</xbar.desc>
 * <xbar.dependencies>node</xbar.dependencies>
 */

// CHANGE THESE AREAS
let email = 'CHANGE@ME.com'; // Cloudflare email
let key = 'CHANGE ME'; // Global API Key, not Origin CA Key - Located under "My Profile"
//


var https = require('https');
var data = {
    zones: []
};

var headers = {
    'X-Auth-Email': email,
    'X-Auth-Key': key,
    'Content-Type': 'application/json'
};

function getZones() {
    var options = {
        host: 'api.cloudflare.com',
        path: '/client/v4/zones?per_page=50',
        method: 'GET',
        headers: headers
    };
    return new Promise((resolve, reject) => {
        var res = https.request(options, (res) => {
            var responseString = '';

            res.setEncoding('utf-8');

            res.on('data', function(data) {
                responseString += data;
            });

            res.on('end', function() {
                var parse = JSON.parse(responseString);
                parse.result.map((val, i) => {
                    if(val.development_mode > 0) {
                        dev = true;
                    }
                    else {
                        dev = false;
                    }
                    data.zones.push({name:val.name, id:val.id, dev:dev});
                });
                resolve();
            });

        });
        res.end();
    });
}

function flipSwitch(status) {
    var flag = (status == true) ? 'on' : 'off';
    var options = {
        host: 'api.cloudflare.com',
        path: `/client/v4/zones/${process.argv[3]}/settings/development_mode`,
        method: 'PATCH',
        headers: headers
    };

    return new Promise((resolve, reject) => {
        var postData = { value: flag };

        var res = https.request(options, (res) => {
            var body = [];
            var responseString = '';

            res.setEncoding('utf-8');
            res.on('data', function(data) {
                responseString += data;
            });
            res.on('end', function() {
                var parse = JSON.parse(responseString);
                resolve();
            });
            res.on('error', function(err) {
                reject(err);
            });
        });
        res.write(JSON.stringify(postData));
        res.end();
    });
}

console.log('⚙️');
console.log('---');

if(process.argv[2] == 'triggerUpdate') {
    var status = (process.argv[4] == 'Enable') ? true : false;
    flipSwitch(status)
    .then(getZones())
    .then(() => {
        data.zones.map((val, i) => {
            var status = (val.dev) ? 'Disable' : 'Enable';
            var color = (val.dev) ? 'red' : 'black';
            console.log(`${val.name}| color=${color}`);
            console.log(`--${status}| bash=${process.argv[0]} param1=${process.argv[1]} param2=triggerUpdate param3=${val.id} param4=${status} refresh=true terminal=false`);
        });
    });

}
else {
    getZones().then(() => {
        data.zones.map((val, i) => {
            var status = (val.dev) ? 'Disable' : 'Enable';
            var color = (val.dev) ? 'red' : 'black';
            console.log(`${val.name}| color=${color}`);
            console.log(`--${status}| bash=${process.argv[0]} param1=${process.argv[1]} param2=triggerUpdate param3=${val.id} param4=${status} refresh=true terminal=false`);
        });
    });
}