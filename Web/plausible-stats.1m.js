#!/usr/bin/env /usr/local/bin/node

// Metadata allows your plugin to show up in the app, and website.
//
//  <xbar.title>Plausible Tracker</xbar.title>
//  <xbar.version>v1.0</xbar.version>
//  <xbar.author>Tom Schönmann</xbar.author>
//  <xbar.author.github>flaming-codes</xbar.author.github>
//  <xbar.desc>See who's on your site at-a-glance.</xbar.desc>
//  <xbar.dependencies>node</xbar.dependencies>
//  <xbar.abouturl>https://flaming.codes</xbar.abouturl>
//  <xbar.image>https://raw.githubusercontent.com/flaming-codes/xbar-plausible-stats/main/plausible-icon-36-36-144.png</xbar.image>

//
// User data.
// ! Update the following values with your data.
//

const SITE_ID = "";
const API_KEY = "";

// Possible images to render.
const plausibleIconDefault = getPlausibleIconDefault();
const plausibleIconWhite = getPlausibleIconWhite();
const plausbileBlackIcon = getPlausibleIconBlack();

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
  [0, `${plausibleIconWhite} Menlo white`, "image font color"],
  [5, "💫"],
  [10, "⭐️"],
  [50, "🌟"],
  [100, "⚡️"],
  [500, "💥"],
];

//
// Imports.
//

const https = require("https");

//
// Utilities.
//

const linksMenu = [
  "---",
  `🔮 Open dashboard | href=https://plausible.io/${SITE_ID}`,
  `🔥 Made by flaming.codes | href=https://flaming.codes`,
];

async function fetcher() {
  return new Promise((resolve, reject) => {
    let body = "";
    const request = {
      host: "plausible.io",
      path: `/api/v1/stats/realtime/visitors?site_id=${SITE_ID}`,
      method: "GET",
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
  });
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

function renderData(props) {
  const { data } = props;

  const output = [
    `${data} ${composeStatusBarFieldFromIcon(findCurrentStepIcon(data))}`,
    ...linksMenu,
  ];

  console.log(output.join("\n"));
}

function renderError(props) {
  const { error } = props;
  const output = [
    "❔",
    "---",
    "No data accessible",
    "Please check your user data",
    ...linksMenu,
  ];

  console.log(output.join("\n"));
}

async function render() {
  const { data, error } = await fetcher()
    .then((data) => ({ data }))
    .catch((error) => ({ error }));

  if (data >= 0) return renderData({ data });
  if (error) return renderError({ error });
}

render();

//
// Image sources.
//

function getPlausibleIconDefault() {
  return "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";
}

function getPlausibleIconWhite() {
  return "iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAeGVYSWZNTQAqAAAACAAFARIAAwAAAAEAAQAAARoABQAAAAEAAABKARsABQAAAAEAAABSASgAAwAAAAEAAgAAh2kABAAAAAEAAABaAAAAAAAAAJAAAAABAAAAkAAAAAEAAqACAAQAAAABAAAAJKADAAQAAAABAAAAJAAAAACP5Tu0AAAACXBIWXMAABYlAAAWJQFJUiTwAAACymlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNi4wLjAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgICAgICAgICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leGlmLzEuMC8iPgogICAgICAgICA8dGlmZjpZUmVzb2x1dGlvbj4xNDQ8L3RpZmY6WVJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOlJlc29sdXRpb25Vbml0PjI8L3RpZmY6UmVzb2x1dGlvblVuaXQ+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjE0NDwvdGlmZjpYUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPGV4aWY6UGl4ZWxYRGltZW5zaW9uPjM2PC9leGlmOlBpeGVsWERpbWVuc2lvbj4KICAgICAgICAgPGV4aWY6Q29sb3JTcGFjZT4xPC9leGlmOkNvbG9yU3BhY2U+CiAgICAgICAgIDxleGlmOlBpeGVsWURpbWVuc2lvbj4zNjwvZXhpZjpQaXhlbFlEaW1lbnNpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgr8bfzqAAABnUlEQVRYCe1VPUsDQRC9k0QQDUGSymBhZxNIo4WVYO/f8A/4G/wBllb+hxQ2FjYpUwVCUAu1sRAbv8Ainm+iCcdll3lzG1LtwpC9mTfvvZu7zSVJXHECcQLzE8iyLJWYr9gzK/aWvw4YaCJOEENkfiRk/59rluU190GwijhHaEswVbOApQECNcSt5iRXF+yGRYPGgngN8ZwTY7f3AC52UiCUl/aadeDAnbF3Tp0MCGyD8Ikl9eBqaZp+eGqzNHvKjmYd5TeHTCtr6IAhUzDHSn1SZg3tMmQKpqPUTYa2GDIFI3+e6mIntKky6YC+DkkS1tA6Q6ZgrpT6pMwe+zHQrHmfbh3H/s1XnOZZERY35S3+XjJmpImdUFZUMFx/A9uAoU+mJ/TOGY02a0bIWEPUkS24e8R1C2buCvnwS3zLvhwfTFfqAckLxD6CvVm7QZC/IHxrD4WKnTWgA4Ijj5tuAG35Vpi5cRgaI9coz+ruZJ/zyNHewQv76sgHpVhDvZzKO/Y7MDPI5Za7xaOpI+QEnSJWl6se1eIE4gQWO4Ff9nC4OJnhSVYAAAAASUVORK5CYII=";
}

function getPlausibleIconBlack() {
  return "iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAeGVYSWZNTQAqAAAACAAFARIAAwAAAAEAAQAAARoABQAAAAEAAABKARsABQAAAAEAAABSASgAAwAAAAEAAgAAh2kABAAAAAEAAABaAAAAAAAAAJAAAAABAAAAkAAAAAEAAqACAAQAAAABAAAAJKADAAQAAAABAAAAJAAAAACP5Tu0AAAACXBIWXMAABYlAAAWJQFJUiTwAAACymlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNi4wLjAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgICAgICAgICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leGlmLzEuMC8iPgogICAgICAgICA8dGlmZjpZUmVzb2x1dGlvbj4xNDQ8L3RpZmY6WVJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOlJlc29sdXRpb25Vbml0PjI8L3RpZmY6UmVzb2x1dGlvblVuaXQ+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjE0NDwvdGlmZjpYUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPGV4aWY6UGl4ZWxYRGltZW5zaW9uPjM2PC9leGlmOlBpeGVsWERpbWVuc2lvbj4KICAgICAgICAgPGV4aWY6Q29sb3JTcGFjZT4xPC9leGlmOkNvbG9yU3BhY2U+CiAgICAgICAgIDxleGlmOlBpeGVsWURpbWVuc2lvbj4zNjwvZXhpZjpQaXhlbFlEaW1lbnNpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgr8bfzqAAABZklEQVRYCWNgGAWjITAaAlhDgBEoCsIDCkSAtqcD8TUg/g/FIDZIDCRHN8AKtGkSEMMcgYsGqQGppSngBZp+C4hxOQJdHKSWh1Yu4gQa/JwEx8Acdweoh+ohBUqwe8hwDMxRbUC9VAWyQNNghpNLExV1TEQ625lIdfiUOeCThMkR6yArmAYKaD9i9BLrIA1iDCOgxoCAPFiaWAdJEWMYATX/CMiT5CBBYgwjoOYsAXmSHMRNjGEE1GwnIE+S9F+ganKzO0wfHzE2EpuGiFWHy84FQIlPuCTJEYf5khz6B9BCakQ5irvJcQhMjyqKSVTikJOGHgDtpkZxgdUL34CiMB/jo0GOmAXEZkBMaboDGoEbvAZK4XKIKVCOBbdW2sjcwOGgzbSxjrCpB7A4CJSuhAlrJU0FsfEMCiF0AKos36IL0osfC7QIloZABZwCvSzGZQ8/UOIBEJcAMRsQj4LREBgNgSEbAgD6d7AouLcxEAAAAABJRU5ErkJggg==";
}
