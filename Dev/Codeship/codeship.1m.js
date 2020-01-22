#!/usr/bin/env /usr/local/bin/node
/* jshint esversion:9 */
const https = require('https');

/* EDIT HERE */
const USER = '';
const PASSWORD = '';
const ORGANIZATION_NAME = '';
const PROJECT_ID = '';
/* DON'T EDIT BELOW */
// <bitbar.title>Codeship</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Gil Barbara</bitbar.author>
// <bitbar.author.github>gilbarbara</bitbar.author.github>
// <bitbar.desc>List recent builds.</bitbar.desc>
// <bitbar.dependencies>node</bitbar.dependencies>
// <bitbar.abouturl>https://github.com/gilbarbara/bitbar-plugins</bitbar.abouturl>
const BASE_URL = 'api.codeship.com';
const PROJECT_URL = `https://app.codeship.com/projects/${PROJECT_ID}`;
const AUTH = Buffer.from(`${USER}:${PASSWORD}`).toString('base64');
const COLORS = {
  success: '#42A86F',
  testing: '#607192',
  error: '#D12C3F',
  infrastructure_failure: '#9E212F',
  stopped: '#C4CDCE',
  waiting: '#3FCBDA',
};

let ACCESS_TOKEN;
let ORGANIZATION_UUID;
let PROJECT_UUID;

const ICON = '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MCAwIDE2IDE2XSAvQ291bnQgMSAvS2lkcyBbIDIgMCBSIF0gPj4KZW5kb2JqCjE2IDAgb2JqCjw8IC9UeXBlIC9DYXRhbG9nIC9QYWdlcyAzIDAgUiAvVmVyc2lvbiAvMS40ID4+CmVuZG9iagoxNyAwIG9iagooTWFjIE9TIFggMTAuMTMuNiBRdWFydHogUERGQ29udGV4dCkKZW5kb2JqCjE4IDAgb2JqCihEOjIwMTgwODExMjIzMTI3WjAwJzAwJykKZW5kb2JqCjEgMCBvYmoKPDwgL1Byb2R1Y2VyIDE3IDAgUiAvQ3JlYXRpb25EYXRlIDE4IDAgUiAvTW9kRGF0ZSAxOCAwIFIgPj4KZW5kb2JqCnhyZWYKMCAxOQowMDAwMDAwMDAwIDY1NTM1IGYgCjAwMDAwMDY1MDUgMDAwMDAgbiAKMDAwMDAwMTQyNSAwMDAwMCBuIAowMDAwMDA2MjY1IDAwMDAwIG4gCjAwMDAwMDAwMjIgMDAwMDAgbiAKMDAwMDAwMTQwNSAwMDAwMCBuIAowMDAwMDAxNTA1IDAwMDAwIG4gCjAwMDAwMDE2MDAgMDAwMDAgbiAKMDAwMDAwMjE2NCAwMDAwMCBuIAowMDAwMDAyMTgzIDAwMDAwIG4gCjAwMDAwMDYyMjggMDAwMDAgbiAKMDAwMDAwMzQ1NSAwMDAwMCBuIAowMDAwMDAyMjUyIDAwMDAwIG4gCjAwMDAwMDM0MzQgMDAwMDAgbiAKMDAwMDAwMzQ5MiAwMDAwMCBuIAowMDAwMDA2MjA3IDAwMDAwIG4gCjAwMDAwMDYzNDYgMDAwMDAgbiAKMDAwMDAwNjQxMCAwMDAwMCBuIAowMDAwMDA2NDYzIDAwMDAwIG4gCnRyYWlsZXIKPDwgL1NpemUgMTkgL1Jvb3QgMTYgMCBSIC9JbmZvIDEgMCBSIC9JRCBbIDwwNGFjNGYxY2IwYjc0MzFmZGI2OTAxYzg3MDJjMWNjZD4KPDA0YWM0ZjFjYjBiNzQzMWZkYjY5MDFjODcwMmMxY2NkPiBdID4+CnN0YXJ0eHJlZgo2NTgwCiUlRU9GCg==';
const RELOAD_ICON = 'iVBORw0KGgoAAAANSUhEUgAAAAwAAAAMCAYAAABWdVznAAAAmElEQVR4AY3SJdYCYRhA4V+BRGY5rAF3ZwvkiSyANLugYgUtOJsg4s7LxV3uOU8a+fTrom9EUMEAfZQRwDEFuwxIQ9CEetCBIIk4BF/fSKMPL26LYgHZoQgEXjxKgQCgCpqfvQwaQMVtOhhv3X7wURU08apvxGB5v2giFwTmT7bVigly+H11cAnUIChAD3p+NbrIwotv7NoAffg2NR6lsPIAAAAASUVORK5CYII=';

function request(options = {}) {
  const OPTIONS = {
    hostname: BASE_URL,
    path: `/v2${options.path || '/auth'}`,
    port: 443,
    method: options.method || 'GET',
    headers: {
      ...options.headers,
    },
  };

  return new Promise((resolve, reject) => {
    const req = https.request(OPTIONS, (response) => {
      const { headers, statusCode } = response;

      if (statusCode < 200 || statusCode > 299) {
        reject(new Error(`Request failed - status code: ${response.statusCode}`));
      }

      const isJSON = headers['content-type'].includes('application/json');

      // temporary data holder
      const body = [];
      // on every content chunk, push it to the data array
      response.on('data', chunk => body.push(chunk));
      // we are done, resolve promise with those joined chunks
      response.on('end', () => {
        const content = body.join('');
        resolve(isJSON ? JSON.parse(content) : content);
      });
    });

    // handle connection errors of the request
    req.on('error', err => reject(err));
    req.end();
  });
}

function timeSince(dateString) {
  const date = new Date(dateString);
  const seconds = Math.floor((new Date() - date) / 1000);
  let intervalType;

  let interval = Math.floor(seconds / 31536000);
  if (interval >= 1) {
    intervalType = 'year';
  }
  else {
    interval = Math.floor(seconds / 2592000);
    if (interval >= 1) {
      intervalType = 'month';
    }
    else {
      interval = Math.floor(seconds / 86400);
      if (interval >= 1) {
        intervalType = 'day';
      }
      else {
        interval = Math.floor(seconds / 3600);
        if (interval >= 1) {
          intervalType = 'hour';
        }
        else {
          interval = Math.floor(seconds / 60);
          if (interval >= 1) {
            intervalType = 'minute';
          }
          else {
            interval = seconds;
            intervalType = 'second';
          }
        }
      }
    }
  }

  if (interval > 1 || interval === 0) {
    intervalType += 's';
  }

  return `${interval} ${intervalType}`;
}

function formatTitle(build) {
  return `${build.branch} | href=${PROJECT_URL} color=${COLORS[build.status]}`;
}

function formatDate(build) {
  return `${timeSince(build.finished_at)} ago - (${build.username}) | size=12`;
}

function formatBuild(build) {
  return [
    formatTitle(build),
    formatDate(build),
  ].join('\n');
}

function handleResponse(body) {
  const content = body.map(formatBuild).join('\n---\n');
  const output = [
    `|image=${ICON}`,
    content,
    `RELOAD | image=${RELOAD_ICON} refresh=true`,
  ];
  console.log(output.join('\n---\n'));
}

const login = () => request({ method: 'POST', headers: { Authorization: `Basic ${AUTH}` } })
  .then((d) => {
    if (d.access_token) {
      ACCESS_TOKEN = d.access_token;

      const organization = d.organizations.find(o => o.name === ORGANIZATION_NAME);

      if (!organization) {
        throw new Error(`Organization "${ORGANIZATION_NAME}" not found`);
      }

      ORGANIZATION_UUID = organization.uuid;
    }
  })
  .catch((error) => {
    throw new Error(`[Login] ${error.message}`);
  });

const getProjects = () => request({
  path: `/organizations/${ORGANIZATION_UUID}/projects`,
  headers: { Authorization: `Bearer ${ACCESS_TOKEN}` },
});

const getBuilds = () => request({
  path: `/organizations/${ORGANIZATION_UUID}/projects/${PROJECT_UUID}/builds`,
  headers: { Authorization: `Bearer ${ACCESS_TOKEN}` },
});

login()
  .then(() => {
    getProjects()
      .then((d) => {
        const project = d.projects.find(p => p.id === PROJECT_ID);

        if (!project) {
          throw new Error(`Project "${PROJECT_ID}" not found`);
        }

        PROJECT_UUID = project.uuid;
      })
      .then(() => getBuilds())
      .then(d => handleResponse(d.builds.filter((b, i) => i < 15)))
      .catch(err => console.log(err.toString()));
  })
  .catch(err => console.log(err.toString()));
