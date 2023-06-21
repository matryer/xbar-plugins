#!/usr/bin/env /usr/local/bin/node

// <xbar.title>GitHub PRNotii</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>aiden.ahn</xbar.author>
// <xbar.author.github>eu81273</xbar.author.github>
// <xbar.desc>Check Github Pull Reuqest</xbar.desc>
// <xbar.image>https://github-production-user-asset-6210df.s3.amazonaws.com/2687916/246659270-e08d6380-62f9-478b-b35b-e2222cf65791.png</xbar.image>
// <xbar.var>string(VAR_TOKEN=""): GitHub personal access token with repos scope</xbar.var>
// <xbar.var>string(VAR_ENDPOINT="https://api.github.com"): GitHub API endpoint</xbar.var>

// <swiftbar.hideAbout>true</swiftbar.hideAbout>
// <swiftbar.hideRunInTerminal>true</swiftbar.hideRunInTerminal>
// <swiftbar.hideLastUpdated>false</swiftbar.hideLastUpdated>
// <swiftbar.hideDisablePlugin>true</swiftbar.hideDisablePlugin>
// <swiftbar.hideSwiftBar>true</swiftbar.hideSwiftBar>

const https = require('https');
const {join} = require('path');
const {homedir} = require('os');
const {readFile} = require('fs/promises');

const ICON = {
  NORMAL: 'iVBORw0KGgoAAAANSUhEUgAAAC4AAAAkCAMAAADB/PhjAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAMlQTFRFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAqjwnTgAAAEN0Uk5TABowNigGAk1IEy81JkZKCAVMJEUqDwQtAQsyNEtCGzwOKRk9Fh4+FDgRJzcQKwoMSR8DRBU7GCFDHUEXIzEzBy5HDRqz7c4AAAFDSURBVHicvdTZUsIwFAbgQJcfKSIFqSytXRDBBUERd9ze/6E8TeiYXjScC8f/omk736RZeiLEP6ZWt2yHi10LlMYBkzcBr3WI9hFLd3x06drDMYv3gYCaE3i7F4N+x8CHwJCaEerqY+MQiE6rfYQ4cVIfmex6Apmz6tG0JVCdTwHbSc4RVXc/y/V8IO/HcgoXaoAVyfmlur1CStcAuGbxBRo3QrQAw+pofOlj1bSKmezl4lbOOzItvc7zfwKrwKBL/G5ND7FJa9y93+QPNoMHD4tNqPbUsOg7Hsc+inSNWqAU/9GsS9z3zCPR+VOaMWpW0nD0vNxPC/7CK1VKlvOJcSO1vKpxm3fyN2tEM/cNMNSnlgRIqJljy+J0cLgir4t3FqdjiQ6kj56sOkY+aZrpF0LmSgaRXJkaT5Ofbi37m6v/PD/IWBbfm7qK8QAAAABJRU5ErkJggg==',
  POINT: 'iVBORw0KGgoAAAANSUhEUgAAAC4AAAAkCAMAAADB/PhjAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAWhQTFRFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALZhACAAAAHh0Uk5TAFWgtIQUCP/wP5qwf+f1GxH8d+WKMg6UAzMSJKeerfjaWtzHLROH/VMEykox9mOGzAFDu6s5gLYNNZDuIH3+KPKzn2dOmwvheQVEw0aNqGLCdBwZyKQdob2B3yYw+aZha9jLS0F1Wy65owepjBfjiJjs9xoMV2Uqm8ejFQAAAa9JREFUeJy9k+dTAjEQxSOoz67YUGwIgh27oiJ4NmxgFwsq9t7bv2+SC87dSY775JubbN7Obya7ewkh/6gcmz03zyqcbwdVQaFFvAgoLilFWbklusKBSrpWodoSXgPU0uBEsUjU1btM8AaggYZGNHHram4B3K1y3gNvm8/fjg5mOrvA1S3Fe3o5YOMmAPT52vrhGZDyg4wequP7Yd7CiFqgRAwfVbdj8NM1CIxbwicwGSJkCmGT6WjwiAPTRQowI6e1OJllxjMn71SPk3l0RUxYA74QXTSHtfjS8soqi5FALB6PrWU+RsWDc7H1FmxQv1mtCPm3JPi2t52FYWpDO8qvdkKZcaFdQrYSikY2nwm+R92+otO+HD84JCTJGJrkK9snM+JHxyd8ECkjnsqEn6Zn4BQ4SeNOA93B8LOgcHEVFx9VXE+fq3V7B6zhUbgv8i/DuNIVky7IWMw1wHq/wa3qU2lc9GtotQbhOxpiuFd90ojrB+ly4IGQxyc8i0SW3/RCL8zzK97ehffZtHTC+Ao/PHwyn7+JLFfs4ythz/3WJLJc4L+KrLHnEcjyCiX6Aa9RYhMsIrs9AAAAAElFTkSuQmCC',
  ERROR: 'iVBORw0KGgoAAAANSUhEUgAAAC4AAAAkCAMAAADB/PhjAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVxQTFRFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA/6wA/6wAAAAAAAAA/6wA/6wA/6wA/6wAAAAA/6wA/6wA/6wA/6wA/6wA/6wAAAAA/6wA/6wAAAAA/6wA/6wA/6wA/6wAAAAA/6wAAAAA/6wAAAAA/6wA/6wA/6wAAAAA/6wA/6wA/6wAAAAA/6wA/6wA/6wA/6wAAAAA/6wA/6wA/6wAAAAA/6wA/6wA/6wAAAAAAAAA/6wA/6wA/6wA/6wA/6wA/6wA/6wAAAAA/6wA/6wA/6wA/6wAu4rhdAAAAHR0Uk5TABowNigGAk1IEy81JkZKCAVMJEUqDwQtAQsyNEtCGzwOKRk9Fh4+FDgRJzcQKwoMSR8YA0QlDDAVOxeA0v8jZfd8W194CVynORXyn/FDgx3YQQggGBcF+6ExwN0NuDNs7WgHCe7mLkdkr15z/e87DT3hC86uAu8OAAAB5UlEQVR4nL3U91cTQRAH8IGULwICATlKcqZJU5Ei0g8ILSC9SBFUpIoUAf//95ybvfhuQ+64n5gfbmb3fbLZ3be7RM8YZeWhcCQojobAUfEiIK8EqqpfoqY2kK6LoZ6/DXgViDcCBqcmVDkdzY11PrwFaOHUinL1Z/EEYL729iaSqUg6howMnYXEG+/Z1AhQg7cB4UiqHab38B227myWOi5L6FIT9Aibv1XlO6T5awDvA/FuVHwgqgZ8dsfFe2LorQwVVvIkpz67Yfb7aI3zmcj2+GGdfxz4JHlwaGjwSR4dHhnmNDo2bnGMj416c6O/eySBCW5PTllOTE168GQyZqc4N3MCp2fkR7OluRNzRPMybp4oL8WMD18gWvwsaoloSYrlRU++widl1dK5tVqSJ1rXZL/Xi/l6Kb7hXNVNq5hbm7rO2DxrqMbWY76t6S9q3klPvqXxAZgd0QlA3c+dXWX2iPZUtbvj1ikgxakT+6r9VaGDw8MDVX3TBueHI0r2vfiu2j+cKRwdOcVPjfOzxA/ScYPcOo6TU8t9CKyzE31jznmZ6QskCo/eL1GXRJdS/C7adcOUnSn733F1zerm9vaG0/UVFYfRth8K/3F13N0XDvD93SNdIh5ycj3+5h6CaDvcl+8fR8djIiI2FtwAAAAASUVORK5CYII=',
};

const request = (url, headers) => new Promise((resolve, reject) => {
  const TIMEOUT_DELAY = 5000; // 5 Sec
  const buffer = [];
  const req = https.get(url, {headers}, res => {
    res.on('data', chunk => buffer.push(chunk));
    res.on('end', () => {
      const data = Buffer.concat(buffer).toString();
      // 2xx status code
      if (((res.statusCode / 100) | 0) == 2) return resolve(data);

      try {
        reject(JSON.parse(data));
      } catch (e) {
        reject(new Error(`${res.statusCode} ${res.statusMessage}`));
      }
    });
  });
  req.on('error', reject);
  req.end();
  setTimeout(reject, TIMEOUT_DELAY, new Error('PRNOTII_ETIMEDOUT'));
});
const refine = items => items.map(item => ({
  repository: item.repository_url.replace(/.+\//, ''),
  href: item.html_url,
  title: item.title,
}));
const organize = refined => refined.reduce((prev, item) => {
  prev[item.repository] = prev[item.repository] || [];
  prev[item.repository].push(item);
  return prev;
}, {});
const githubQuery = (endpoint, token, query) => 
  request(endpoint + '/search/issues?q=' + encodeURIComponent(query), {'Authorization': 'token ' + token, 'User-Agent': 'xbar-app-prnotii'})
    .then(JSON.parse)
    .then(res => refine(res.items))
    .then(organize);
const subMenu = organized => Object.entries(organized)
  .map(([repository, items]) => '--' + repository + ' | disabled=true | size=12\n' + items.map(item => '--‣ ' + item.title + ' | href=' + item.href).join('\n'))
  .join('\n');
const printOut = str => str && console.log(str);

(async function () {
  try {
    // Check xbar variable file first. https://xbarapp.com/docs/2021/03/14/variables-in-xbar.html
    // Intended to generate ENOENT error, if xbar variable is not set.
    const configFile = !process.env.SWIFTBAR ? __filename + '.vars.json' : join(homedir(), '.prnotii') ;
    const {VAR_ENDPOINT: endpoint, VAR_TOKEN: token} = await readFile(configFile, {encoding: 'utf8'}).then(JSON.parse);

    const requested = await githubQuery(endpoint, token, 'is:open is:pr user-review-requested:@me');
    const requestedSubmenu = subMenu(requested);
    printOut(Object.keys(requested).length ? `| templateImage=${ICON.POINT}` : `| templateImage=${ICON.NORMAL}`);
    printOut('---');
    printOut('👩‍💻 Review Requested');
    printOut(requestedSubmenu);

    const reviewed = await githubQuery(endpoint, token, 'is:pr is:open reviewed-by:@me');
    const reviewedSubmenu = subMenu(reviewed);
    printOut('💬 Reviewed');
    printOut(reviewedSubmenu);

    const created = await githubQuery(endpoint, token, 'is:open is:pr author:@me');
    const createdSubmenu = subMenu(created);
    printOut('💪🏻 Created');
    printOut(createdSubmenu);

    const mentioned = await githubQuery(endpoint, token, 'is:open is:pr mentions:@me');
    const mentionedSubmenu = subMenu(mentioned);
    printOut('😀 Mentioned');
    printOut(mentionedSubmenu);
  } catch (e) {
    printOut(`| templateImage=${ICON.ERROR}`);
    printOut('---');

    switch (true) {
      case /ENOENT/.test(e.message):
        printOut('🌈 How to setup');
        printOut('---');
        printOut('❶ Generate Github token');
        printOut('⠀  Generate new personal access token with "repos scope". | size=12');
        printOut('❷ Create config file');
        printOut('⠀  Create ~/.prnotii File like below JSON format. | size=12');
        printOut('⠀  {"VAR_TOKEN": "kSEzyDP...", "VAR_ENDPOINT": "https://api.github.com"} | size=12');
        printOut('❸ Press "Refresh" menu below');
        printOut('⠀  After finish ❶,❷ steps, press "Refresh" menu to start again. | size=12');
        break;
      case /ETIMEDOUT|ECONNRESET/.test(e.message):
        printOut('🚧 Connection error');
        printOut('---');
        printOut('Check your internet connection or proxy settings. | size=12');
        break;
      default:
        printOut('⛑️ Unhandled error');
        printOut('---');
        printOut(e.message + ' | size=12');
    }
  } finally {
    printOut('---');
    printOut('🔄 Refresh | refresh=True');
    process.exit(0);
  }
}) ();
