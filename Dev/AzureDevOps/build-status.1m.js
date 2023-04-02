#!/usr/local/bin/node

/* 
In case of an error: "node no such file or directory", there is a chance you are using nvm.
In this case, execute the following commands to create a symlink to the node binary used by nvm:

```
nodepath=$(which node);
sudo mkdir -p /usr/local/bin;
sudo ln -s $nodepath /usr/local/bin/node;
```

Then replace the shebang on the first line with:
#!/usr/local/bin/node
*/

//  <xbar.title>Azure DevOps latest pipeline build status</xbar.title>
//  <xbar.version>v1.0</xbar.version>
// <xbar.image>https://raw.githubusercontent.com/stormoz/public-images/main/Azure%20DevOps%20latest%20build%20status.png</xbar.image>
//  <xbar.author>Kon Stormozhev</xbar.author>
//  <xbar.author.github>stormoz</xbar.author.github>
//  <xbar.desc>Azure DevOps latest pipeline build status.</xbar.desc>
//  <xbar.dependencies>nodejs</xbar.dependencies>

//  <xbar.var>string(USERNAME=""): Azure DevOps username string.</xbar.var>
//  <xbar.var>string(PAT=""): Azure DevOps user PAT string.</xbar.var>
//  <xbar.var>string(ORGANIZATION=""): Azure DevOps organization string.</xbar.var>
//  <xbar.var>string(PROJECT=""): Azure DevOps project string.</xbar.var>
//  <xbar.var>string(PIPELINE_ID=""): Azure DevOps pipeline configuration id string.</xbar.var>
//  <xbar.var>string(PIPELINE_ALIAS=""): Alias for readability.</xbar.var>
//  <xbar.var>string(SUCCESS_TEXT=""): Text for successful result.</xbar.var>
//  <xbar.var>string(NOTSUCCESS_TEXT=""): Text for notsuccessful result.</xbar.var>

const username = process.env.USERNAME;
const pat = process.env.PAT;
const organization = process.env.ORGANIZATION;
const project = process.env.PROJECT;
const pipelineId = process.env.PIPELINE_ID;
const pipelineAlias = process.env.PIPELINE_ALIAS;
const success = process.env.SUCCESS_TEXT || `✅`;
const notSuccess = process.env.NOTSUCCESS_TEXT || `❌`;

main();

async function main() {
    const url = `https://dev.azure.com/${organization}/${project}/_apis/build/latest/${pipelineId}?api-version=5.1-preview.1`;
    const lastBuildResponse = await queryDevOpsApi(url, { username, pat });

    const statusValue = lastBuildResponse.result == 'succeeded'
        ? success
        : notSuccess;

    console.log(`${pipelineAlias}: ${statusValue}`);

    console.log('---');
    console.log(url);
    console.log(lastBuildJson.result);
}

async function queryDevOpsApi(url, credentials) {
    const auth = credentials && 'Basic ' + Buffer.from(credentials.username + ':' + credentials.pat).toString('base64');

    const responseJson = await getJson(url, auth && { 'Authorization': auth });
    return responseJson;
}

async function getJson(requestUrl, headers) {
    const url = require('url');
    const parsedUrl = url.parse(requestUrl);
    const options = { host: parsedUrl.host, path: parsedUrl.path, headers: headers };

    const httpClient = parsedUrl.protocol.indexOf('https') != -1
        ? require('https')
        : require('http');

    return new Promise((resolve, reject) => {
        const optionsForJson = {
            ...options, headers: { ...options.headers, 'Accept': 'application/json' }
        };

        httpClient.get(optionsForJson, res => {
            let body = '';
            res.on('data', chunk => {
                body += chunk;
            }).on('end', () => {
                try {
                    const json = JSON.parse(body);
                    resolve(json);
                } catch (error) {
                    reject(error.message);
                };
            }).on('error', err => { reject(err); }
            );
        });
    });
}