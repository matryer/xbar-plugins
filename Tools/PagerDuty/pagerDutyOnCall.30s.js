#!/usr/bin/env /usr/local/bin/node
/*
<bitbar.title>PagerDuty On-Call</bitbar.title>
<bitbar.version>v0.1.0</bitbar.version>
<bitbar.author>Pedro Pablo Fuentes Schuster</bitbar.author>
<bitbar.author.github>pedrofuentes</bitbar.author.github>
<bitbar.desc>Shows who is on call for all the active Escalation Policies and what services have active incidents. For installation instructions check https://github.com/PedroFuentes/bitbar-plugins/blob/master/pagerDutyOnCall/README.md</bitbar.desc>
<bitbar.image>http://cdn.pedrofuent.es/images/github/PagerDutyOnCall_Screenshot.png</bitbar.image>
<bitbar.dependencies>node, npm/node-fetch, npm/time-ago, npm/bitbar, node/home-config</bitbar.dependencies>
<bitbar.abouturl>https://github.com/PedroFuentes/bitbar-plugins/tree/master/pagerDutyOnCall</bitbar.abouturl>
*/
/* MIT Licensed https://opensource.org/licenses/MIT */
/* jshint esversion: 6 */
'use strict';
const fetch = require('node-fetch');
const ta = require('time-ago')();
const bitbar = require('bitbar');

const cfg = require('home-config').load('.bitbarrc');

if (!cfg.pagerdutyoncall['api.endpoint'] || !cfg.pagerdutyoncall['api.token']) {
  const json = [];

  json.push({
    text: 'Config Needed',
    dropdown: false,
  },
  bitbar.sep, {
    text: 'Add to your .bitbarrc config file on your',
  }, {
    text: 'home directory the following information:',
  },
  bitbar.sep, {
    text: '[pagerdutyoncall]',
  }, {
    text: 'api.endpoint=https://',
  }, {
    text: 'api.token={your-token}',
  }, {
    text: 'api.query={your-filter-query} *optional',
  }, {
    text: 'style.prefix={service-name-prefix} *optional',
  });

  bitbar(json);
  process.exit();
}

const config = {
  api: {
    endpoint: cfg.pagerdutyoncall['api.endpoint'],
    token: cfg.pagerdutyoncall['api.token'],
    query: cfg.pagerdutyoncall['api.query'] ? cfg.pagerdutyoncall['api.query'] : '',
  },
  icon: {
    active: 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyJpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuMC1jMDYwIDYxLjEzNDc3NywgMjAxMC8wMi8xMi0xNzozMjowMCAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENTNSBNYWNpbnRvc2giIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6REEwQjNEREYxNjBCMTFFNjgyODVBMzc1NTdCRDNBRUYiIHhtcE1NOkRvY3VtZW50SUQ9InhtcC5kaWQ6REEwQjNERTAxNjBCMTFFNjgyODVBMzc1NTdCRDNBRUYiPiA8eG1wTU06RGVyaXZlZEZyb20gc3RSZWY6aW5zdGFuY2VJRD0ieG1wLmlpZDpEQTBCM0RERDE2MEIxMUU2ODI4NUEzNzU1N0JEM0FFRiIgc3RSZWY6ZG9jdW1lbnRJRD0ieG1wLmRpZDpEQTBCM0RERTE2MEIxMUU2ODI4NUEzNzU1N0JEM0FFRiIvPiA8L3JkZjpEZXNjcmlwdGlvbj4gPC9yZGY6UkRGPiA8L3g6eG1wbWV0YT4gPD94cGFja2V0IGVuZD0iciI/Pv4ywmgAAADTSURBVHja1NOtCgJBFIbhGX8QNYkGg6jFm7CZNXodglGL1RsQMQsmjUaTyWYzWAzGFfxji+L6LpyFYZmybBAHnrDLme8MZ3a1UspTMVZCxVw/Cdjgar7wIqpiFzynQulbrPBBGT1kMMUJfv0FyWCDGXBDF02UMIOLGkbo4I0XtC3Akc4LZDHGHg+0MZe6ugRZh5gw042lQzNTthP4m+/oI4+1dK5giSGeMoOCLUBLuiNFLQyQwxkHqZugYbvGowzPjXKt5gyK0jEd5avS//8zfQUYAGZtQSNj7zxTAAAAAElFTkSuQmCC',
    inactive: 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyJpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuMC1jMDYwIDYxLjEzNDc3NywgMjAxMC8wMi8xMi0xNzozMjowMCAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENTNSBNYWNpbnRvc2giIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6MzI0MzNGNUIxNjBFMTFFNkEwRTE5QTI1QkJBMzA0MjYiIHhtcE1NOkRvY3VtZW50SUQ9InhtcC5kaWQ6MzI0MzNGNUMxNjBFMTFFNkEwRTE5QTI1QkJBMzA0MjYiPiA8eG1wTU06RGVyaXZlZEZyb20gc3RSZWY6aW5zdGFuY2VJRD0ieG1wLmlpZDozMjQzM0Y1OTE2MEUxMUU2QTBFMTlBMjVCQkEzMDQyNiIgc3RSZWY6ZG9jdW1lbnRJRD0ieG1wLmRpZDozMjQzM0Y1QTE2MEUxMUU2QTBFMTlBMjVCQkEzMDQyNiIvPiA8L3JkZjpEZXNjcmlwdGlvbj4gPC9yZGY6UkRGPiA8L3g6eG1wbWV0YT4gPD94cGFja2V0IGVuZD0iciI/Poh/go0AAADaSURBVHja1NM9CsIwFMDxRjuIqJuj4AX0DJ5BRPAKdRAn3Z0Fxc0TiKK7Lp7F0cmKg9/Gf+BVAlahFAQDP5K+ti8vaaq01k6clnBitt8nUEqN4L0CZg+ioO2xCK5dK7MZV1CVyjYY4IYWCjLO4hS851rVddDDHAe0TYE4oouVPK/wCEtQwhl1StNU1GdclARTQp5U6tOlwzbx24H4eM+uwMyewpBZLvQ1jHFFk9iOPoMctmEJglnySGJmkuEusbKsvYHl22ekTWS9KspntStYw498sP7/Z3oKMAD3DYQymUW7vgAAAABJRU5ErkJggg==',
  },
  colors: {
    critical: '#FF0000',
    warning: '#999900',
    regularText: '#808080',
  },
  style: {
    indentation: '      ',
    prefix: cfg.pagerdutyoncall['style.prefix'] ? cfg.pagerdutyoncall['style.prefix'] : '',
  },
};

// TODO: Add support more than 100 escalations
fetch(`${config.api.endpoint}/api/v1/escalation_policies/on_call?limit=100&query=${config.api.query}`, {
  method: 'GET',
  headers: {
    'Content-Type': 'application/json',
    Authorization: `Token token=${config.api.token}`,
  },
})
.then(res => res.json())
.then((json) => {
  const escalations = [];
  let activeIncident = false;

  json.escalation_policies.forEach((escalation) => {
    let activeServiceIncident = false;
    const services = [];
    let activeServices = 0;

    escalation.services.forEach((service) => {
      if (service.status !== 'disabled') {
        activeServices += 1;

        if (service.status !== 'active') {
          activeIncident = true;
          activeServiceIncident = true;

          services.push({
            text: `${config.style.indentation}${service.status === 'maintenance' ? ':construction:' : ':bangbang:'} ${cleanName(service.name)}, ${ta.ago(new Date(Date.parse(service.last_incident_timestamp)))}`,
            trim: false,
            color: service.status === 'critical' ? config.colors.critical : config.colors.warning,
            href: `${config.api.endpoint}${service.service_url}`,
          }, {
            text: `${config.style.indentation}:page_facing_up: Triggered: ${service.incident_counts.triggered} Acknowledged: ${service.incident_counts.acknowledged}`,
            trim: false,
            alternate: true,
          });
        }
      }
    });

    if (activeServices) {
      const onCallList = [];

      escalation.on_call.forEach((onCall) => {
        onCallList.push({
          text: `${onCall.level}. ${onCall.user.name}`,
          color: '#000000',
          href: `${config.api.endpoint}/users/${onCall.user.id}`,
        }, {
          text: `${onCall.level}. ${onCall.user.email}`,
          alternate: true,
        });
      });

      escalations.push({
        text: `${activeServiceIncident ? ':sos:' : ':cool:'} ${cleanName(escalation.name)}`,
        href: `${config.api.endpoint}/escalation_policies/${escalation.id}`,
        submenu: onCallList,
      });

      if (services.length) escalations.push(services[0]);

      escalations.push(bitbar.sep);
    }
  });

  escalations.unshift({
    text: '☎',
    dropdown: false,
    templateImage: activeIncident ? config.icon.active : config.icon.inactive,
    size: 8,
  },
  bitbar.sep);

  bitbar(escalations);
});

function cleanName(name) {
  return name.replace(`${config.style.prefix} - `, '').replace(config.style.prefix, '').trim();
}
