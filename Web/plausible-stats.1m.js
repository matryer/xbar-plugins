#!/usr/bin/env /usr/local/bin/node
// ! Run "which node" in your terminal to determine your path, if any error occurs.
// "/usr/local/bin/node" is just a default.

// Metadata allows your plugin to show up in the app, and website.
//
//  <xbar.title>Plausible Tracker</xbar.title>
//  <xbar.version>v1.0</xbar.version>
//  <xbar.author>Tom Schönmann</xbar.author>
//  <xbar.author.github>flaming-codes</xbar.author.github>
//  <xbar.desc>See who's on your site at-a-glance.</xbar.desc>
//  <xbar.dependencies>node</xbar.dependencies>
//  <xbar.abouturl>https://flaming.codes</xbar.abouturl>
//  <xbar.image>https://cdn.sanity.io/images/udzdriea/production/af38732db5fc82b0d705ec780c77fb4d4c92ce5b-732x212.jpg</xbar.image>
//  <xbar.var>string(ENV_SITE_ID=""): Your site's ID in Plausible.</xbar.var>
//  <xbar.var>string(ENV_API_KEY=""): The generated API-key in Plausible for the site.</xbar.var>
//
// User data.
// ! Update the following values with your data.
//

const SITE_ID = $ENV_SITE_ID;
const API_KEY = $ENV_API_KEY;

// Define custom icons for each step,
// where each value defines the lower
// bound for its activation.
//
// You can define a single string to render,
// or provide custom options accorindg to
// the xbar-API (https://github.com/matryer/xbar-plugins/blob/main/CONTRIBUTING.md).
//
// Pattern:
// 1. [number, string]              Simple icon to render.
// 2. [number, string, string]      Provide a string of options, where each index maps
//                                  a value (2nd param) to an accessor (3rd param).
const stepIcons = [
  [0, `${getPlausibleIconWhite()} Menlo white`, "image font color"],
  [100, "🎆"],
];

// https://plausible.io/docs/stats-api#time-periods
const timeRange = ["day", "7d", "30d", "month", "6mo", "12mo"][0];
const metrics = ["visitors", "pageviews", "bounce_rate", "visit_duration"];

//
// Imports.
//

const https = require("https");

//
// Main (app start).
//

renderApp();

//
// Render calls.
//

async function renderApp() {
  const connection = await getDirtyConnectionAvailability();
  if (connection.state === "offline") {
    renderError({ source: "offline" });
    return;
  }

  const [visitorsNowRes, aggregateRes, breakdownRes] = await Promise.all([
    fetcher({
      path: `/api/v1/stats/realtime/visitors?site_id=${SITE_ID}`,
    }),
    fetcher({
      path: `/api/v1/stats/aggregate?site_id=${SITE_ID}&period=${timeRange}&metrics=${metrics.join(
        ","
      )}`,
    }),
    fetcher({
      path: `/api/v1/stats/breakdown?site_id=${SITE_ID}&period=${timeRange}&property=visit:source&limit=5`,
    }),
  ]);

  const { error, data } = visitorsNowRes;
  if (data >= 0) renderMenuItemData({ data });
  else if (error) renderError({ error });

  renderTimeRangeData();
  if (aggregateRes.data) {
    renderAggregateData({ data: aggregateRes.data.results });
  }
  if (breakdownRes) {
    renderTopSourcesData({ data: breakdownRes.data.results });
  }

  render(getLinksMenu());
}

function renderError(props) {
  const { error, source } = props;

  render(
    [
      "❔",
      "---",
      "No data accessible",
      !source && "Please check your user data",
      source === "offline" && "No internet connection",
      ...getLinksMenu(),
    ].filter(Boolean)
  );
}

function renderTimeRangeData() {
  render(["---", formatTimeRange()]);
}

function renderAggregateData({ data }) {
  render(["---", ...formatAggregateData(parseAggregateData({ data }))]);
}

function renderTopSourcesData({ data }) {
  render([
    "---",
    "Top sources",
    ...data.map((tuple, i) => `${i + 1}. ${tuple.source}`),
  ]);
}

//
// Utilities.
//

function render(items) {
  console.log(items.join("\n"));
}

async function fetcher(params) {
  return new Promise((resolve, reject) => {
    let body = "";
    const request = {
      host: "plausible.io",
      path: params.path,
      method: params.method || "GET",
      headers: {
        Authorization: `Bearer ${API_KEY}`,
      },
    };

    try {
      const req = https.get(request, (res) => {
        res.on("data", (data) => {
          body += data;
        });
        res.on("end", () => {
          resolve(JSON.parse(body));
        });
      });

      req.on("error", (error) => {
        console.error(error);
      });

      req.end();
    } catch (error) {
      reject(error);
    }
  })
    .then((data) => ({ data }))
    .catch((error) => ({ error }));
}

function findCurrentStepIcon(count) {
  const tuple = stepIcons.reverse().find((tuple, i) => count >= tuple[0]);

  // Branch out early
  if (!tuple) return "";
  return [tuple[1], tuple[2]];
}

function composeStatusBarFieldFromIcon([icon, stringifiedFields]) {
  if (!stringifiedFields) {
    return icon;
  }

  const inputs = icon.split(" ");
  const fields = stringifiedFields.split(" ");

  return `| ${inputs.map((input, i) => `${fields[i]}=${input}`).join(" ")}`;
}

function renderMenuItemData(props) {
  const { data } = props;

  render([
    `${data} ${composeStatusBarFieldFromIcon(findCurrentStepIcon(data))}`,
  ]);
}

function parseAggregateData({ data }) {
  return Object.keys(data).map((key) => [key, data[key].value]);
}

function formatAggregateData(tuples) {
  const keyLablesMap = {
    bounce_rate: "Bounce rate",
    pageviews: "Pageviews",
    visit_duration: "Visit duration",
    visitors: "Visitors",
  };

  const valueFormattersMap = {
    bounce_rate: (v) => `${v}%`,
    pageviews: (v) => v,
    visit_duration: (v) => `${v} seconds`,
    visitors: (v) => v,
  };

  return tuples.map(
    ([k, v]) => `${keyLablesMap[k]}: ${valueFormattersMap[k](v)}`
  );
}

function formatTimeRange() {
  const rangeLabelsMap = {
    day: "Today",
    "7d": "Last 7 days",
    "30d": "Last 30 days",
    month: "This month",
    "6mo": "Last 6 months",
    "12mo": "Last 12 months",
  };

  const translated = rangeLabelsMap[timeRange];
  return translated || timeRange;
}

function getLinksMenu() {
  return [
    "---",
    `Open dashboard | href=https://plausible.io/${SITE_ID}?period=realtime | image=${getExternalLinkIconWhite()}`,
    `Made by flaming.codes | href=https://flaming.codes | image=${getExternalLinkIconWhite}`,
  ];
}

async function getDirtyConnectionAvailability() {
  return new Promise((res) =>
    require("dns").lookupService("8.8.8.8", 53, (err) => {
      if (err) res({ state: "offline" });
      else res({ state: "online" });
    })
  );
}

//
// Image sources.
//

function getPlausibleIconDefault() {
  return "iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAeGVYSWZNTQAqAAAACAAFARIAAwAAAAEAAQAAARoABQAAAAEAAABKARsABQAAAAEAAABSASgAAwAAAAEAAgAAh2kABAAAAAEAAABaAAAAAAAAAJAAAAABAAAAkAAAAAEAAqACAAQAAAABAAAAJKADAAQAAAABAAAAJAAAAACP5Tu0AAAACXBIWXMAABYlAAAWJQFJUiTwAAACymlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNi4wLjAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgICAgICAgICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leGlmLzEuMC8iPgogICAgICAgICA8dGlmZjpZUmVzb2x1dGlvbj4xNDQ8L3RpZmY6WVJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOlJlc29sdXRpb25Vbml0PjI8L3RpZmY6UmVzb2x1dGlvblVuaXQ+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjE0NDwvdGlmZjpYUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPGV4aWY6UGl4ZWxYRGltZW5zaW9uPjM2PC9leGlmOlBpeGVsWERpbWVuc2lvbj4KICAgICAgICAgPGV4aWY6Q29sb3JTcGFjZT4xPC9leGlmOkNvbG9yU3BhY2U+CiAgICAgICAgIDxleGlmOlBpeGVsWURpbWVuc2lvbj4zNjwvZXhpZjpQaXhlbFlEaW1lbnNpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgr8bfzqAAAEWUlEQVRYCe2Vz28bRRTH3+wPr9c/1k6dGBNRwDSuExGDW0qRyqVSBUcOBSEQVOIOqIeqB46oICSkCvFLCAkB/wAXQIIiDghRQCIlFFRIqJIQKkUusWNnE9vZX7N9b21XiWzicRU4eeT17M6+fe8z3/dmBmDYhgoMFehWwPd9Rlf3m8FHbtnJ/Lw5+ut85fGl5cZpc9ObYugpqkt/HLgr/OZ9U+lP8nmjPDgOwMBAMzO++uPlufNf/7DxIuc+xHQJFLnlxsPnzSYHCekeeTjx9tHCwTNHjjBnELCBgObmVuNvfPj3pdWql8uklCAOQQD+gobeZKnlslRxYTSpXH3l7KHD6TTbbFv07YSBrl3z9dfe/Xlxo8EzRlQG2+lQ9I4RUhls1D0Ia/LC+68fwpSKKSX1drdzlAr2y2+ufrZSdgMYy94dhr4mmziCl2vugfc++vPlnR7//UkIaGFh7Y4L39VO3J5SA2WogPs1siEoSu2Fi+svXbnyT6zfN/ReCGh+0TzR2OJo7gOq1eW3M9T9plNePsz+3jze9WGPAUGg5rGIxsDzCAiA4992MFnGmaEnGS/qOxc9M5wE9xT45bL9WI/4XUOtpdI1vHPAXLcnFQxKyzxQg6TAnATBcD8sVzA9DuURR4L9kfqWD0rdUgkgqdvFnV57PwkBYfBxUqR1kSMKjjPH/vqqB+UqqSKDh1klEGIhMLKiiciSB0xilPO+TQhIVfwRD6P5NxUIJAoUsywPdExnB4JAyC4YwPCqwqBkejA+FrrUlwYNhIDCYTnKOU1fupmKjnNZ4qhMO30I0rojbVqQKkZYqjowPZn4ovPNbr0QUMIIq4zV0E87H+11T+mRmAdy8CxhZql2CIZMGabUD3buiO7BQw+kvm292P0f10H/ljA0aTytgWW5WCsE1S4WDCihQlQjEl4ywVFPNcNcMCI+fL+4BedOjXycy6XM/pFwgiJGCtZBfmIEIniQ2g4Fp6+CqkGF8DBtQwVgCEWqGVEfSms2HD8oW888MfqCSByyEQKSESgeU+H+6VEIhxjuwKQAqoPZCakcQgrH4uWg4X1U5xDROMwuNuHONIPzr+YLmUymLgokVEMKVS5WdDymQbEwBgt/rcNazcFVxvGsskAJTngJGhaqYnIwwhKcefa25adO5o+NjUVXRGHITggI9x8MBTotfVWV4Z67k5CsWtBo2FCYwI1xzcKyYpDapy3nJhJfHS6kP8hmUzNY5EJ7z3ZgISDb4XXH8XVa866LyxyPEAnPi3vzSXj6yexRFWAWg7vbHd/qvVANOQ6vIBQ4LgIhDBW0uelCdr/2eYixn/YKhiYhBGQ5vGTbHFcYD3bneoPDiCHz4nT8OXKyl00IyHVgzsFU0VlWb3hQMx04+WimaBgGHqt724SAtpruxUrVhZXrNqgy23j+1P7s1NS+3/YWpeVNqKhTUenTaE5fnszF3nmwmH4La8b+L2CGPocKDBX4PxS4Advtw1RbKwz0AAAAAElFTkSuQmCC";
}

function getPlausibleIconWhite() {
  return "iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAeGVYSWZNTQAqAAAACAAFARIAAwAAAAEAAQAAARoABQAAAAEAAABKARsABQAAAAEAAABSASgAAwAAAAEAAgAAh2kABAAAAAEAAABaAAAAAAAAAJAAAAABAAAAkAAAAAEAAqACAAQAAAABAAAAJKADAAQAAAABAAAAJAAAAACP5Tu0AAAACXBIWXMAABYlAAAWJQFJUiTwAAACymlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNi4wLjAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgICAgICAgICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leGlmLzEuMC8iPgogICAgICAgICA8dGlmZjpZUmVzb2x1dGlvbj4xNDQ8L3RpZmY6WVJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOlJlc29sdXRpb25Vbml0PjI8L3RpZmY6UmVzb2x1dGlvblVuaXQ+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjE0NDwvdGlmZjpYUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPGV4aWY6UGl4ZWxYRGltZW5zaW9uPjM2PC9leGlmOlBpeGVsWERpbWVuc2lvbj4KICAgICAgICAgPGV4aWY6Q29sb3JTcGFjZT4xPC9leGlmOkNvbG9yU3BhY2U+CiAgICAgICAgIDxleGlmOlBpeGVsWURpbWVuc2lvbj4zNjwvZXhpZjpQaXhlbFlEaW1lbnNpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgr8bfzqAAABnUlEQVRYCe1VPUsDQRC9k0QQDUGSymBhZxNIo4WVYO/f8A/4G/wBllb+hxQ2FjYpUwVCUAu1sRAbv8Ainm+iCcdll3lzG1LtwpC9mTfvvZu7zSVJXHECcQLzE8iyLJWYr9gzK/aWvw4YaCJOEENkfiRk/59rluU190GwijhHaEswVbOApQECNcSt5iRXF+yGRYPGgngN8ZwTY7f3AC52UiCUl/aadeDAnbF3Tp0MCGyD8Ikl9eBqaZp+eGqzNHvKjmYd5TeHTCtr6IAhUzDHSn1SZg3tMmQKpqPUTYa2GDIFI3+e6mIntKky6YC+DkkS1tA6Q6ZgrpT6pMwe+zHQrHmfbh3H/s1XnOZZERY35S3+XjJmpImdUFZUMFx/A9uAoU+mJ/TOGY02a0bIWEPUkS24e8R1C2buCvnwS3zLvhwfTFfqAckLxD6CvVm7QZC/IHxrD4WKnTWgA4Ijj5tuAG35Vpi5cRgaI9coz+ruZJ/zyNHewQv76sgHpVhDvZzKO/Y7MDPI5Za7xaOpI+QEnSJWl6se1eIE4gQWO4Ff9nC4OJnhSVYAAAAASUVORK5CYII=";
}

function getPlausibleIconBlack() {
  return "iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAeGVYSWZNTQAqAAAACAAFARIAAwAAAAEAAQAAARoABQAAAAEAAABKARsABQAAAAEAAABSASgAAwAAAAEAAgAAh2kABAAAAAEAAABaAAAAAAAAAJAAAAABAAAAkAAAAAEAAqACAAQAAAABAAAAJKADAAQAAAABAAAAJAAAAACP5Tu0AAAACXBIWXMAABYlAAAWJQFJUiTwAAACymlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNi4wLjAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgICAgICAgICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leGlmLzEuMC8iPgogICAgICAgICA8dGlmZjpZUmVzb2x1dGlvbj4xNDQ8L3RpZmY6WVJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOlJlc29sdXRpb25Vbml0PjI8L3RpZmY6UmVzb2x1dGlvblVuaXQ+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjE0NDwvdGlmZjpYUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPGV4aWY6UGl4ZWxYRGltZW5zaW9uPjM2PC9leGlmOlBpeGVsWERpbWVuc2lvbj4KICAgICAgICAgPGV4aWY6Q29sb3JTcGFjZT4xPC9leGlmOkNvbG9yU3BhY2U+CiAgICAgICAgIDxleGlmOlBpeGVsWURpbWVuc2lvbj4zNjwvZXhpZjpQaXhlbFlEaW1lbnNpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgr8bfzqAAABZklEQVRYCWNgGAWjITAaAlhDgBEoCsIDCkSAtqcD8TUg/g/FIDZIDCRHN8AKtGkSEMMcgYsGqQGppSngBZp+C4hxOQJdHKSWh1Yu4gQa/JwEx8Acdweoh+ohBUqwe8hwDMxRbUC9VAWyQNNghpNLExV1TEQ625lIdfiUOeCThMkR6yArmAYKaD9i9BLrIA1iDCOgxoCAPFiaWAdJEWMYATX/CMiT5CBBYgwjoOYsAXmSHMRNjGEE1GwnIE+S9F+ganKzO0wfHzE2EpuGiFWHy84FQIlPuCTJEYf5khz6B9BCakQ5irvJcQhMjyqKSVTikJOGHgDtpkZxgdUL34CiMB/jo0GOmAXEZkBMaboDGoEbvAZK4XKIKVCOBbdW2sjcwOGgzbSxjrCpB7A4CJSuhAlrJU0FsfEMCiF0AKos36IL0osfC7QIloZABZwCvSzGZQ8/UOIBEJcAMRsQj4LREBgNgSEbAgD6d7AouLcxEAAAAABJRU5ErkJggg==";
}

function getExternalLinkIconWhite() {
  return "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABYWlDQ1BrQ0dDb2xvclNwYWNlRGlzcGxheVAzAAAokWNgYFJJLCjIYWFgYMjNKykKcndSiIiMUmB/yMAOhLwMYgwKicnFBY4BAT5AJQwwGhV8u8bACKIv64LMOiU1tUm1XsDXYqbw1YuvRJsw1aMArpTU4mQg/QeIU5MLikoYGBhTgGzl8pICELsDyBYpAjoKyJ4DYqdD2BtA7CQI+whYTUiQM5B9A8hWSM5IBJrB+API1klCEk9HYkPtBQFul8zigpzESoUAYwKuJQOUpFaUgGjn/ILKosz0jBIFR2AopSp45iXr6SgYGRiaMzCAwhyi+nMgOCwZxc4gxJrvMzDY7v////9uhJjXfgaGjUCdXDsRYhoWDAyC3AwMJ3YWJBYlgoWYgZgpLY2B4dNyBgbeSAYG4QtAPdHFacZGYHlGHicGBtZ7//9/VmNgYJ/MwPB3wv//vxf9//93MVDzHQaGA3kAFSFl7jXH0fsAAAB4ZVhJZk1NACoAAAAIAAUBEgADAAAAAQABAAABGgAFAAAAAQAAAEoBGwAFAAAAAQAAAFIBKAADAAAAAQACAACHaQAEAAAAAQAAAFoAAAAAAAAASAAAAAEAAABIAAAAAQACoAIABAAAAAEAAAAQoAMABAAAAAEAAAAQAAAAAIinlEMAAAAJcEhZcwAACxMAAAsTAQCanBgAAAKaaVRYdFhNTDpjb20uYWRvYmUueG1wAAAAAAA8eDp4bXBtZXRhIHhtbG5zOng9ImFkb2JlOm5zOm1ldGEvIiB4OnhtcHRrPSJYTVAgQ29yZSA2LjAuMCI+CiAgIDxyZGY6UkRGIHhtbG5zOnJkZj0iaHR0cDovL3d3dy53My5vcmcvMTk5OS8wMi8yMi1yZGYtc3ludGF4LW5zIyI+CiAgICAgIDxyZGY6RGVzY3JpcHRpb24gcmRmOmFib3V0PSIiCiAgICAgICAgICAgIHhtbG5zOnRpZmY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vdGlmZi8xLjAvIgogICAgICAgICAgICB4bWxuczpleGlmPSJodHRwOi8vbnMuYWRvYmUuY29tL2V4aWYvMS4wLyI+CiAgICAgICAgIDx0aWZmOllSZXNvbHV0aW9uPjcyPC90aWZmOllSZXNvbHV0aW9uPgogICAgICAgICA8dGlmZjpSZXNvbHV0aW9uVW5pdD4yPC90aWZmOlJlc29sdXRpb25Vbml0PgogICAgICAgICA8dGlmZjpYUmVzb2x1dGlvbj43MjwvdGlmZjpYUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPGV4aWY6UGl4ZWxYRGltZW5zaW9uPjE4PC9leGlmOlBpeGVsWERpbWVuc2lvbj4KICAgICAgICAgPGV4aWY6UGl4ZWxZRGltZW5zaW9uPjE4PC9leGlmOlBpeGVsWURpbWVuc2lvbj4KICAgICAgPC9yZGY6RGVzY3JpcHRpb24+CiAgIDwvcmRmOlJERj4KPC94OnhtcG1ldGE+CmqR6/EAAAIqSURBVDgRbZNLixNREIU7nUc7jWaRbBwUMqAMMfor/AfCBBQy4NaNC0Hc5ke4GdGVG4kZZMCFizEo6mRciUqCmGwM2CqEKHm/43eu3aEHvFCpe0+dU7duVceyQmu1WsWCI3sbc7GNYrFoh/A1R1gkHIhEInMEDti16XSa53xW2HK5/BWNRvexZ5yncGLCpTWZAwC/vVgs3mB7xJzJZPJpPB7X2Z/G9sBfg2UllgbMsoINxO35fO6NRqOjZrN50QRDP57nXUL8jiRet9vNhrVK4iA+ljik+e+Wp72F+16aNYHDDcA/rVbrgp/ZHgwGtyAfcusj/EP8dcVqtdpluF005qzbbYL7s9nswBfrvdZwONzB7vV6vQqcFaU/EK4F/wB+uVQqRdXEDbq7Sae/Kliv19XlqOu65X6//zEej5/HvyLBUnEt9k3cuXw+f0oJIqoCMyPN5XIa0aLdbt9Mp9NPqOIO5xexWCwjsb/ENaZRjDAPkuk8ftjpdArJZPI+k7mdSqWe8ww1+bcvtqgqA++7rzWjLIgQNJFb71L2rgR6ju9NhTwxwxM64AXhZjFjl6Z8QFgJMHnhEBOYaawwRv1SXDBX5/WHBHCFKn5COGw0GmachuD/VKvVLcW43RM30JqyAMy3rQDTeIzfIlkF8hfbti3em6WJV8G/Ed91HOdzoDEJ1tn+feMqbUd/JsSbSkCyH4lEogz+VE0OxNKdWAoEgPbYGd9O4AFH/i/7sLSf4VUakQAAAABJRU5ErkJggg==";
}

function getPlausibleBrandingWhite() {
  return "iVBORw0KGgoAAAANSUhEUgAAAFAAAAAQCAYAAACBSfjBAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAA7GVYSWZNTQAqAAAACAAHARIAAwAAAAEAAQAAARoABQAAAAEAAABiARsABQAAAAEAAABqASgAAwAAAAEAAgAAATEAAgAAACgAAAByATIAAgAAABQAAACah2kABAAAAAEAAACuAAAAAAAAAEgAAAABAAAASAAAAAFBZG9iZSBQaG90b3Nob3AgRWxlbWVudHMgMTguMCAoV2luZG93cykAMjAyMTowNDozMCAxOTo1ODozNAAAA5AEAAIAAAAUAAAA2KACAAQAAAABAAAAUKADAAQAAAABAAAAEAAAAAAyMDIxOjA0OjMwIDE5OjU4OjM0AC3/DTwAAAAJcEhZcwAACxMAAAsTAQCanBgAAAqbaVRYdFhNTDpjb20uYWRvYmUueG1wAAAAAAA8eDp4bXBtZXRhIHhtbG5zOng9ImFkb2JlOm5zOm1ldGEvIiB4OnhtcHRrPSJYTVAgQ29yZSA2LjAuMCI+CiAgIDxyZGY6UkRGIHhtbG5zOnJkZj0iaHR0cDovL3d3dy53My5vcmcvMTk5OS8wMi8yMi1yZGYtc3ludGF4LW5zIyI+CiAgICAgIDxyZGY6RGVzY3JpcHRpb24gcmRmOmFib3V0PSIiCiAgICAgICAgICAgIHhtbG5zOmV4aWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vZXhpZi8xLjAvIgogICAgICAgICAgICB4bWxuczpkYz0iaHR0cDovL3B1cmwub3JnL2RjL2VsZW1lbnRzLzEuMS8iCiAgICAgICAgICAgIHhtbG5zOnhtcE1NPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvbW0vIgogICAgICAgICAgICB4bWxuczpzdEV2dD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL3NUeXBlL1Jlc291cmNlRXZlbnQjIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgICAgICAgICAgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIgogICAgICAgICAgICB4bWxuczpwaG90b3Nob3A9Imh0dHA6Ly9ucy5hZG9iZS5jb20vcGhvdG9zaG9wLzEuMC8iPgogICAgICAgICA8ZXhpZjpDb2xvclNwYWNlPjE8L2V4aWY6Q29sb3JTcGFjZT4KICAgICAgICAgPGV4aWY6UGl4ZWxYRGltZW5zaW9uPjEwMDwvZXhpZjpQaXhlbFhEaW1lbnNpb24+CiAgICAgICAgIDxleGlmOlBpeGVsWURpbWVuc2lvbj4yMDwvZXhpZjpQaXhlbFlEaW1lbnNpb24+CiAgICAgICAgIDxkYzpmb3JtYXQ+aW1hZ2UvanBlZzwvZGM6Zm9ybWF0PgogICAgICAgICA8eG1wTU06SGlzdG9yeT4KICAgICAgICAgICAgPHJkZjpTZXE+CiAgICAgICAgICAgICAgIDxyZGY6bGkgcmRmOnBhcnNlVHlwZT0iUmVzb3VyY2UiPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6c29mdHdhcmVBZ2VudD5BZG9iZSBQaG90b3Nob3AgRWxlbWVudHMgMTguMCAoV2luZG93cyk8L3N0RXZ0OnNvZnR3YXJlQWdlbnQ+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDp3aGVuPjIwMjEtMDQtMzBUMTk6NTg6MzQrMDI6MDA8L3N0RXZ0OndoZW4+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDppbnN0YW5jZUlEPnhtcC5paWQ6ZGIwN2QyNTktZDlmZS00YjRlLTk3NTYtNzI1MmU5ZGM0NzczPC9zdEV2dDppbnN0YW5jZUlEPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6YWN0aW9uPmNyZWF0ZWQ8L3N0RXZ0OmFjdGlvbj4KICAgICAgICAgICAgICAgPC9yZGY6bGk+CiAgICAgICAgICAgICAgIDxyZGY6bGkgcmRmOnBhcnNlVHlwZT0iUmVzb3VyY2UiPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6c29mdHdhcmVBZ2VudD5BZG9iZSBQaG90b3Nob3AgRWxlbWVudHMgMTguMCAoV2luZG93cyk8L3N0RXZ0OnNvZnR3YXJlQWdlbnQ+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDpjaGFuZ2VkPi88L3N0RXZ0OmNoYW5nZWQ+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDp3aGVuPjIwMjEtMDQtMzBUMTk6NTg6MzQrMDI6MDA8L3N0RXZ0OndoZW4+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDppbnN0YW5jZUlEPnhtcC5paWQ6ZTc1ZTJjMDUtNDI4MC04MTQwLWE2ZWEtNzYxYmQ4ZTNjN2NhPC9zdEV2dDppbnN0YW5jZUlEPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6YWN0aW9uPnNhdmVkPC9zdEV2dDphY3Rpb24+CiAgICAgICAgICAgICAgIDwvcmRmOmxpPgogICAgICAgICAgICA8L3JkZjpTZXE+CiAgICAgICAgIDwveG1wTU06SGlzdG9yeT4KICAgICAgICAgPHhtcE1NOk9yaWdpbmFsRG9jdW1lbnRJRD54bXAuZGlkOmRiMDdkMjU5LWQ5ZmUtNGI0ZS05NzU2LTcyNTJlOWRjNDc3MzwveG1wTU06T3JpZ2luYWxEb2N1bWVudElEPgogICAgICAgICA8eG1wTU06RG9jdW1lbnRJRD5hZG9iZTpkb2NpZDpwaG90b3Nob3A6YTRmMzczYTktYTlkZC0xMWViLTlmNDgtZGYzYzdkYTY5NzA5PC94bXBNTTpEb2N1bWVudElEPgogICAgICAgICA8eG1wTU06SW5zdGFuY2VJRD54bXAuaWlkOmU3NWUyYzA1LTQyODAtODE0MC1hNmVhLTc2MWJkOGUzYzdjYTwveG1wTU06SW5zdGFuY2VJRD4KICAgICAgICAgPHRpZmY6UmVzb2x1dGlvblVuaXQ+MjwvdGlmZjpSZXNvbHV0aW9uVW5pdD4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPHRpZmY6WFJlc29sdXRpb24+NzI8L3RpZmY6WFJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOllSZXNvbHV0aW9uPjcyPC90aWZmOllSZXNvbHV0aW9uPgogICAgICAgICA8eG1wOkNyZWF0b3JUb29sPkFkb2JlIFBob3Rvc2hvcCBFbGVtZW50cyAxOC4wIChXaW5kb3dzKTwveG1wOkNy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";
}
