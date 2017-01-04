#!/usr/bin/env /usr/local/bin/node
/*
<bitbar.title>PagerDuty Incidents</bitbar.title>
<bitbar.version>v0.1.0</bitbar.version>
<bitbar.author>Pedro Pablo Fuentes Schuster</bitbar.author>
<bitbar.author.github>pedrofuentes</bitbar.author.github>
<bitbar.desc>Shows all the active incidents grouped by Service. For installation instructions check https://github.com/PedroFuentes/bitbar-plugins/blob/master/pagerDutyIncidents/README.md</bitbar.desc>
<bitbar.image>http://cdn.pedrofuent.es/images/github/PagerDutyIncidents_Screenshot.png</bitbar.image>
<bitbar.dependencies>node, npm/node-fetch, npm/time-ago, npm/bitbar, npm/home-config</bitbar.dependencies>
<bitbar.abouturl>https://github.com/PedroFuentes/bitbar-plugins/tree/master/pagerDutyIncidents</bitbar.abouturl>
*/
/* MIT Licensed https://opensource.org/licenses/MIT */
/* jshint esversion: 6 */
'use strict';
const fetch = require('node-fetch');
const ta = require('time-ago')();
const bitbar = require('bitbar');

const cfg = require('home-config').load('.bitbarrc');

if (!cfg.pagerdutyincidents['api.endpoint'] || !cfg.pagerdutyincidents['api.token']) {
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
    text: '[pagerdutyincidents]',
  }, {
    text: 'api.endpoint=https://',
  }, {
    text: 'api.token={your-token}',
  });

  bitbar(json);
  process.exit();
}

const config = {
  api: {
    endpoint: cfg.pagerdutyincidents['api.endpoint'],
    token: cfg.pagerdutyincidents['api.token'],
    query: cfg.pagerdutyincidents['api.query'] ? cfg.pagerdutyincidents['api.query'] : 'limit=100&status=triggered,acknowledged',
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
};

// TODO: load all available pages
fetch(`${config.api.endpoint}/api/v1/incidents?${config.api.query}`, {
  method: 'GET',
  headers: {
    'Content-Type': 'application/json',
    Authorization: `Token token=${config.api.token}`,
  },
})
.then(res => res.json())
.then((json) => {
  // Group incidents by Service
  const serviceIncidents = [];

  json.incidents.forEach((incident) => {
    if (!serviceIncidents[incident.service.id]) {
      serviceIncidents[incident.service.id] = {
        name: incident.service.name,
        html_url: incident.service.html_url,
        incidents: [],
      };
    }

    serviceIncidents[incident.service.id].incidents.push(incident);
  });

  return {
    serviceIncidents,
    total: json.incidents.length,
  };
})
.then((obj) => {
  const json = [];
  const serviceIncidents = obj.serviceIncidents;

  json.push({
    text: `[${obj.total}]`,
    dropdown: false,
    templateImage: config.icon.inactive,
    size: 8,
  },
  bitbar.sep);

  if (Object.keys(serviceIncidents).length) {
    Object.keys(serviceIncidents).forEach((prop) => {
      const incidents = [];

      serviceIncidents[prop].incidents.forEach((incident) => {
        const assignedTo = [];

        incident.assigned_to.forEach((user) => {
          assignedTo.push({
            text: `${user.object.name}, ${ta.ago(new Date(Date.parse(user.at)))}`,
            href: user.object.html_url,
            color: config.colors.regularText,
          }, {
            text: user.object.email,
            alternate: true,
          });
        });

        incidents.push({
          text: incident.incident_key,
          length: 50,
          href: incident.html_url,
          color: incident.status === 'triggered' ? config.colors.critical : config.colors.warning,
        }, {
          text: incident.incident_key,
          alternate: true,
        }, {
          text: 'Assigned To',
          submenu: assignedTo,
        }, {
          text: `Created\t\t: ${ta.ago(new Date(Date.parse(incident.created_on)))}`,
          color: config.colors.regularText,
        }, {
          text: `Created at\t: ${incident.created_on}`,
          alternate: true,
          color: config.colors.regularText,
        }, {
          text: `Status\t\t: ${incident.status}`,
          color: config.colors.regularText,
        }, {
          text: `Urgency\t\t: ${incident.urgency}`,
          color: config.colors.regularText,
        }, {
          text: `Escalations\t: ${incident.number_of_escalations}`,
          color: config.colors.regularText,
        });
      });

      json.push({
        text: `${serviceIncidents[prop].name} (${serviceIncidents[prop].incidents.length})`,
        href: serviceIncidents[prop].html_url,
        submenu: incidents,
      },
      bitbar.sep);
    });
  } else {
    json.push({
      text: 'No Open Incidents',
    });
  }

  bitbar(json);
});
