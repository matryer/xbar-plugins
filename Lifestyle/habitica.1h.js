#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>Habitica</bitbar.title>
// <bitbar.version>v0.4</bitbar.version>
// <bitbar.author>Stefan du Fresne</bitbar.author>
// <bitbar.author.github>SCdF</bitbar.author.github>
// <bitbar.desc>Allows you to manage your Habitica tasks. Just dailies for now. See: habitica.com</bitbar.desc>
// <bitbar.image>http://i.imgur.com/CUO445t.png</bitbar.image>
// <bitbar.dependencies>node6</bitbar.dependencies>

/*jshint esversion: 6 */
'use strict';

// Go here: https://habitica.com/#/options/settings/api
// And put your values in these vars below!
const USER_ID = 'YOUR_USER_ID_HERE';
const API_TOKEN = 'YOUR_TOKEN_HERE';

const HABITICA_ICON =
  'iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAACXBIWXMAABYlAAAWJQFJUiTwAA' +
  'A6HWlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJX' +
  'NU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpucz' +
  'ptZXRhLyIgeDp4bXB0az0iQWRvYmUgWE1QIENvcmUgNS42LWMxMzIgNzkuMTU5Mjg0LCAyMDE2' +
  'LzA0LzE5LTEzOjEzOjQwICAgICAgICAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly' +
  '93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2Ny' +
  'aXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG' +
  '9iZS5jb20veGFwLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcv' +
  'ZGMvZWxlbWVudHMvMS4xLyIKICAgICAgICAgICAgeG1sbnM6cGhvdG9zaG9wPSJodHRwOi8vbn' +
  'MuYWRvYmUuY29tL3Bob3Rvc2hvcC8xLjAvIgogICAgICAgICAgICB4bWxuczp4bXBNTT0iaHR0' +
  'cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIKICAgICAgICAgICAgeG1sbnM6c3RFdnQ9Im' +
  'h0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZUV2ZW50IyIKICAgICAg' +
  'ICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iCiAgICAgIC' +
  'AgICAgIHhtbG5zOmV4aWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vZXhpZi8xLjAvIj4KICAgICAg' +
  'ICAgPHhtcDpDcmVhdGVEYXRlPjIwMTYtMDctMzBUMTM6MzA6MTErMDE6MDA8L3htcDpDcmVhdG' +
  'VEYXRlPgogICAgICAgICA8eG1wOk1vZGlmeURhdGU+MjAxNi0wNy0zMFQxMzozMTo0NSswMTow' +
  'MDwveG1wOk1vZGlmeURhdGU+CiAgICAgICAgIDx4bXA6TWV0YWRhdGFEYXRlPjIwMTYtMDctMz' +
  'BUMTM6MzE6NDUrMDE6MDA8L3htcDpNZXRhZGF0YURhdGU+CiAgICAgICAgIDx4bXA6Q3JlYXRv' +
  'clRvb2w+QWRvYmUgUGhvdG9zaG9wIENDIDIwMTUuNSAoTWFjaW50b3NoKTwveG1wOkNyZWF0b3' +
  'JUb29sPgogICAgICAgICA8ZGM6Zm9ybWF0PmltYWdlL3BuZzwvZGM6Zm9ybWF0PgogICAgICAg' +
  'ICA8cGhvdG9zaG9wOkNvbG9yTW9kZT4zPC9waG90b3Nob3A6Q29sb3JNb2RlPgogICAgICAgIC' +
  'A8eG1wTU06SW5zdGFuY2VJRD54bXAuaWlkOmI1Y2U5ZjZlLWRmZjEtNDYzNS05Yzk0LTkzN2I2' +
  'OTI2NzliMTwveG1wTU06SW5zdGFuY2VJRD4KICAgICAgICAgPHhtcE1NOkRvY3VtZW50SUQ+eG' +
  '1wLmRpZDplYTUwZTRiMi0zZmVjLTQ1MDktOWVhMS02MGIxYTZhZDRhODU8L3htcE1NOkRvY3Vt' +
  'ZW50SUQ+CiAgICAgICAgIDx4bXBNTTpPcmlnaW5hbERvY3VtZW50SUQ+eG1wLmRpZDplYTUwZT' +
  'RiMi0zZmVjLTQ1MDktOWVhMS02MGIxYTZhZDRhODU8L3htcE1NOk9yaWdpbmFsRG9jdW1lbnRJ' +
  'RD4KICAgICAgICAgPHhtcE1NOkhpc3Rvcnk+CiAgICAgICAgICAgIDxyZGY6U2VxPgogICAgIC' +
  'AgICAgICAgICA8cmRmOmxpIHJkZjpwYXJzZVR5cGU9IlJlc291cmNlIj4KICAgICAgICAgICAg' +
  'ICAgICAgPHN0RXZ0OmFjdGlvbj5zYXZlZDwvc3RFdnQ6YWN0aW9uPgogICAgICAgICAgICAgIC' +
  'AgICA8c3RFdnQ6aW5zdGFuY2VJRD54bXAuaWlkOmVhNTBlNGIyLTNmZWMtNDUwOS05ZWExLTYw' +
  'YjFhNmFkNGE4NTwvc3RFdnQ6aW5zdGFuY2VJRD4KICAgICAgICAgICAgICAgICAgPHN0RXZ0On' +
  'doZW4+MjAxNi0wNy0zMFQxMzozMTowMiswMTowMDwvc3RFdnQ6d2hlbj4KICAgICAgICAgICAg' +
  'ICAgICAgPHN0RXZ0OnNvZnR3YXJlQWdlbnQ+QWRvYmUgUGhvdG9zaG9wIENDIDIwMTUuNSAoTW' +
  'FjaW50b3NoKTwvc3RFdnQ6c29mdHdhcmVBZ2VudD4KICAgICAgICAgICAgICAgICAgPHN0RXZ0' +
  'OmNoYW5nZWQ+Lzwvc3RFdnQ6Y2hhbmdlZD4KICAgICAgICAgICAgICAgPC9yZGY6bGk+CiAgIC' +
  'AgICAgICAgICAgIDxyZGY6bGkgcmRmOnBhcnNlVHlwZT0iUmVzb3VyY2UiPgogICAgICAgICAg' +
  'ICAgICAgICA8c3RFdnQ6YWN0aW9uPnNhdmVkPC9zdEV2dDphY3Rpb24+CiAgICAgICAgICAgIC' +
  'AgICAgIDxzdEV2dDppbnN0YW5jZUlEPnhtcC5paWQ6YjVjZTlmNmUtZGZmMS00NjM1LTljOTQt' +
  'OTM3YjY5MjY3OWIxPC9zdEV2dDppbnN0YW5jZUlEPgogICAgICAgICAgICAgICAgICA8c3RFdn' +
  'Q6d2hlbj4yMDE2LTA3LTMwVDEzOjMxOjQ1KzAxOjAwPC9zdEV2dDp3aGVuPgogICAgICAgICAg' +
  'ICAgICAgICA8c3RFdnQ6c29mdHdhcmVBZ2VudD5BZG9iZSBQaG90b3Nob3AgQ0MgMjAxNS41IC' +
  'hNYWNpbnRvc2gpPC9zdEV2dDpzb2Z0d2FyZUFnZW50PgogICAgICAgICAgICAgICAgICA8c3RF' +
  'dnQ6Y2hhbmdlZD4vPC9zdEV2dDpjaGFuZ2VkPgogICAgICAgICAgICAgICA8L3JkZjpsaT4KIC' +
  'AgICAgICAgICAgPC9yZGY6U2VxPgogICAgICAgICA8L3htcE1NOkhpc3Rvcnk+CiAgICAgICAg' +
  'IDx0aWZmOk9yaWVudGF0aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgICAgIDx0aWZmOl' +
  'hSZXNvbHV0aW9uPjE0NDAwMDAvMTAwMDA8L3RpZmY6WFJlc29sdXRpb24+CiAgICAgICAgIDx0' +
  'aWZmOllSZXNvbHV0aW9uPjE0NDAwMDAvMTAwMDA8L3RpZmY6WVJlc29sdXRpb24+CiAgICAgIC' +
  'AgIDx0aWZmOlJlc29sdXRpb25Vbml0PjI8L3RpZmY6UmVzb2x1dGlvblVuaXQ+CiAgICAgICAg' +
  'IDxleGlmOkNvbG9yU3BhY2U+NjU1MzU8L2V4aWY6Q29sb3JTcGFjZT4KICAgICAgICAgPGV4aW' +
  'Y6UGl4ZWxYRGltZW5zaW9uPjM2PC9leGlmOlBpeGVsWERpbWVuc2lvbj4KICAgICAgICAgPGV4' +
  'aWY6UGl4ZWxZRGltZW5zaW9uPjM2PC9leGlmOlBpeGVsWURpbWVuc2lvbj4KICAgICAgPC9yZG' +
  'Y6RGVzY3JpcHRpb24+CiAgIDwvcmRmOlJERj4KPC94OnhtcG1ldGE+CiAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAog' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAK' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'CiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'IAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCi' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIA' +
  'ogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgIC' +
  'AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg' +
  'ICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgCjw/eHBhY2tldC' +
  'BlbmQ9InciPz5J4M7HAAAAIGNIUk0AAHolAACAgwAA+f8AAIDpAAB1MAAA6mAAADqYAAAXb5Jf' +
  'xUYAAAT0SURBVHja7JhfbFN1FMc/567rsBsERiSAhBBoy5LxgMQnHrCsk39BMEaCIQaFGE1IiG' +
  'BcNybGG2LCVkAJ+kBMDIhIVIhGIIrZmGVogkQkmkzZn+AS3RDDABUurN29x4e1446uawsL8uB5' +
  'ur2/3/3db8/5nu8554qqcj+ZwX1mntSFiLAguKmsT+13iq/7lh3pNq17BcIdpUEesp24IRC2Sq' +
  'yjj082ff95yHzW6M5+yMy/XmwdDpWbJfcakKTcJSIAhAORc8DM5PpZO6FLYp3b/sj34BXlprfn' +
  '5o1HRPquHu/Y8fOKctPbk7BmO4bxZ+xcXWemkKUBqghU7xJ0vWv/BRFndWPb9sZcgISmmaM8nh' +
  'sbMDSiyrjk7R5gDFAI9Kowr6ktenooQJ50hjl7EXEDmqRqNFQGInvEYHNDa7R7EICQ6Snour4I' +
  'lVEizFastQqTGKwm413XRaK6FjidU9o3dWz7QeFEGk5Y4zicDwcieyv8VXMGAMVwQGoRDiq8Ck' +
  'zKThQm5qVDovY6IDHEUhHwrIicCQcizZXBVypNTMdO+CpQtopwJQuU3xD9XFWO5UzqlIX91c8g' +
  '+kF2EeFjn+Vbe6TbtPrDZx0FFg4pet7E+K9a3rqcsw657XhH/X5HWQ5czOL+lZbPOmRiGrGY2a' +
  'dD8TJpfXHv8znrUKisZlrIH5niXvy6I3rY9vr8IrwIfCjCGeDHZNa4QS0+GbjxZsZEueWL+rA/' +
  '8sVi//oxWQEV2M6vBQbfpKVxCxYwXYVTDrpb0EPA70O87KVwoGq3QDCLRxfH5YFjoZDpGZZD4W' +
  'B1TJBLiO5X6HbrxG1iOTL1C13T1L5tb0YONbVHQwbOGyifCVrrfthRIiNeIpCncm8/VMoqgxGz' +
  '0l8TSnEJ4b0RxhQyMY1c+6GZqryO4YRSN0oLfeuA90cQ0M3v/D0lw+rQY4Gq2Q5y1rXeiug5lY' +
  'JdTa11Tf18qloKsgV4+C7A9JZ6fWMOtpjxnHUo5SlUluPYUwf0qX3b0ePt0TmKPIqyQ5Vv02Qg' +
  'uxVdiVtLh+0YARLe4g5P7835GM4iVapv+VHSXNvUXt8MNAOE/JEphiEvi+rGPFj9BPBpTqWjIl' +
  'D1nCB7XNu67ETvrFjnzqvp5YU60IfyT3tONLVHQzmFbLy3+ICdiI+zE/FxHm9iPDjdBR7vZvee' +
  'ymDkhf5alz+YZNpfzhqylCXJFh/wWDCyT2BZ6vfC8o2lfXG232Vn33jHY5AoqwRtHiiSvYVPA6' +
  'PvAs5Fu8i3744AhYNVtSBT+7zFO13+nnsXYP4xDJbGWsxrOYcsZfODVUtQ2aywaNABwmTufOA9' +
  '39Aa/T7vyTU0o8ZvqHwkwoZkirtTZFSOL7dHZJQ2MY0Cwz6A8EljW/TdIVI2noV1XYquKfX6fL' +
  'bXNxpla96jtNtO+q0nQYJ2oa8iA8ktJOOZv9jSNy/WtuNS8nccqK0IVJcIuh4dnrdDN/nCbOBS' +
  'JuIhXM54oMFqF5hbtUKtzUAvBmPyBqToKWBGOBDZsqLc9N4+CALTMxXNsR7fT6l5zf3slx1v/w' +
  '10oExMnpHf1FEZjNT11zPpAm0EuSqipY6ySODBYf7kBcACpgKWndCygiKdgG3UISxOdREiXFNl' +
  'lgg7G1rra7ICAlgQ3FRmq71KYa70e2UCUJzfxwNZqaKzgNcybPmrsa1+bBqg/7+gZbB/BwBXfQ' +
  'a0r0oBWwAAAABJRU5ErkJggg==';

//   ================================================================================================
//   ===     =====    ====  =======  ===      ===        =====  =====  =======  ==        ===      ==
//   ==  ===  ===  ==  ===   ======  ==  ====  =====  =======    ====   ======  =====  =====  ====  =
//   =  ========  ====  ==    =====  ==  ====  =====  ======  ==  ===    =====  =====  =====  ====  =
//   =  ========  ====  ==  ==  ===  ===  ==========  =====  ====  ==  ==  ===  =====  ======  ======
//   =  ========  ====  ==  ===  ==  =====  ========  =====  ====  ==  ===  ==  =====  ========  ====
//   =  ========  ====  ==  ====  =  =======  ======  =====        ==  ====  =  =====  ==========  ==
//   =  ========  ====  ==  =====    ==  ====  =====  =====  ====  ==  =====    =====  =====  ====  =
//   ==  ===  ===  ==  ===  ======   ==  ====  =====  =====  ====  ==  ======   =====  =====  ====  =
//   ===     =====    ====  =======  ===      ======  =====  ====  ==  =======  =====  ======      ==
//   ================================================================================================

// NB: DEBUG on means refreshing post-action doesn't work
const DEBUG = false;

const UNCHECKED = '◻️';
const CHECKED = '☑️';
const HEALTH = '💗';
const EXP = '⭐';
const MAGIC = '🔥';

const SCORE_UP   = '➕';
const SCORE_DOWN = '➖';

const ACTIONS = {
  COMPLETE_TASK: 'completeTask',
  UNCOMPLETE_TASK: 'uncompleteTask',
  COMPLETE_CHECKLIST_ITEM: 'completeChecklistItem',
  UNCOMPLETE_CHECKLIST_ITEM: 'uncompleteCheckItem'
};

const failure = function(reason) {
  console.log('☹');
  console.log('---');
  console.log(reason);
};

//   ========================================================================
//   =  ====  =====  =====      ====    ==        ==    ====     ======  ====
//   =  ====  ====    ====  ===  ====  ======  ======  ====  ===  ====    ===
//   =  ====  ===  ==  ===  ====  ===  ======  ======  ===  =========  ==  ==
//   =  ====  ==  ====  ==  ===  ====  ======  ======  ===  ========  ====  =
//   =        ==  ====  ==      =====  ======  ======  ===  ========  ====  =
//   =  ====  ==        ==  ===  ====  ======  ======  ===  ========        =
//   =  ====  ==  ====  ==  ====  ===  ======  ======  ===  ========  ====  =
//   =  ====  ==  ====  ==  ===  ====  ======  ======  ====  ===  ==  ====  =
//   =  ====  ==  ====  ==      ====    =====  =====    ====     ===  ====  =
//   ========================================================================

const https = require('https');

const options = function(method, endpoint) {
  return {
    method: method,
    hostname: 'habitica.com',
    path: '/api/v3/' + endpoint,
    headers: {
      'x-api-user': USER_ID,
      'x-api-key': API_TOKEN
    }
  };
};

const request = function(method, endpoint) {
  return new Promise((resolve, reject) => {
    var req = https.request(options(method, endpoint), (res) => {
      if (res.statusCode !== 200) {
        console.log(res);
        return reject('HTTP'+res.statusCode+' when '+method+'ing ['+endpoint+']');
      }

      let body = '';
      res.on('data', data => body += data);
      res.on('end',  ()   => resolve(JSON.parse(body)));
      res.on('error', reject);
    });

    req.end();
    req.on('error', failure);
  });
};

const get = function(endpoint) {
  return request('GET', endpoint);
};

//   =====================================================================
//   ====  =======     ===        ==    ====    ====  =======  ===      ==
//   ===    =====  ===  =====  ======  ====  ==  ===   ======  ==  ====  =
//   ==  ==  ===  ===========  ======  ===  ====  ==    =====  ==  ====  =
//   =  ====  ==  ===========  ======  ===  ====  ==  ==  ===  ===  ======
//   =  ====  ==  ===========  ======  ===  ====  ==  ===  ==  =====  ====
//   =        ==  ===========  ======  ===  ====  ==  ====  =  =======  ==
//   =  ====  ==  ===========  ======  ===  ====  ==  =====    ==  ====  =
//   =  ====  ===  ===  =====  ======  ====  ==  ===  ======   ==  ====  =
//   =  ====  ====     ======  =====    ====    ====  =======  ===      ==
//   =====================================================================

const scoreTask = (id, direction) => request('POST', 'tasks/'+id+'/score/'+direction);
const completeTask = (id) => scoreTask(id, 'up');

const completeChecklistItem = (taskId, checklistItemId) =>
  request('POST', 'tasks/'+taskId+'/checklist/'+checklistItemId+'/score');

const processArguments = function() {
  const action = process.argv[2];

  switch (action) {
    case ACTIONS.COMPLETE_TASK:
      const id = process.argv[3];
      if (id) {
        return completeTask(process.argv[3]);
      } else {
        throw Error(ACTIONS.COMPLETE_TASK + ' requires an id');
      }
      break;
    case ACTIONS.COMPLETE_CHECKLIST_ITEM:
      const taskId = process.argv[3],
            checklistItemId = process.argv[4];

      if (taskId && checklistItemId) {
        return completeChecklistItem(taskId, checklistItemId);
      } else {
        throw Error(ACTIONS.COMPLETE_CHECKLIST_ITEM + ' missing params');
      }
      break;
    default:
      throw Error('Unsupported action ' + action);
  }
};

//   ==============================================
//   =  ==========    =====      ===    ====     ==
//   =  =========  ==  ===   ==   ===  ====  ===  =
//   =  ========  ====  ==  ====  ===  ===  =======
//   =  ========  ====  ==  =========  ===  =======
//   =  ========  ====  ==  =========  ===  =======
//   =  ========  ====  ==  ===   ===  ===  =======
//   =  ========  ====  ==  ====  ===  ===  =======
//   =  =========  ==  ===   ==   ===  ====  ===  =
//   =        ====    =====      ===    ====     ==
//   ==============================================

const now = new Date();
const days = ['su', 'm', 't', 'w', 'th', 'f', 's'];

const dailyForToday = task =>
  task.type === 'daily' &&
  task.repeat[days[now.getDay()]];
const habit = task => task.type === 'habit';
const completed = task => task.completed;
const incomplete = task => !completed(task);

//   ============================================================
//   ===    ====  ====  ==        ==       ===  ====  ==        =
//   ==  ==  ===  ====  =====  =====  ====  ==  ====  =====  ====
//   =  ====  ==  ====  =====  =====  ====  ==  ====  =====  ====
//   =  ====  ==  ====  =====  =====  ====  ==  ====  =====  ====
//   =  ====  ==  ====  =====  =====       ===  ====  =====  ====
//   =  ====  ==  ====  =====  =====  ========  ====  =====  ====
//   =  ====  ==  ====  =====  =====  ========  ====  =====  ====
//   ==  ==  ===   ==   =====  =====  ========   ==   =====  ====
//   ===    =====      ======  =====  =========      ======  ====
//   ============================================================

const sep = () => console.log('---');
const title = text => console.log(text + '|size=10');

const action = function(action, params) {
  params = Array.prototype.slice.call(arguments).slice(1);
  return ['terminal='+DEBUG+' refresh=true bash=' + process.argv[0],
          'param1=' + process.argv[1],
          'param2=' + action
         ].concat(params.map((p, i) => 'param'+(i+3)+'='+p))
         .join(' ');
};

const outputIncompleteDailies = function(dailies) {
  title('Dailies');

  dailies.forEach(task => {
    console.log([UNCHECKED, task.text, '|', action(ACTIONS.COMPLETE_TASK, task._id)].join(' '));
    task.checklist.forEach(item => {
      console.log(
        ['--', (completed(item) ? CHECKED : UNCHECKED), item.text, '|',
         action(ACTIONS.COMPLETE_CHECKLIST_ITEM, task._id, item.id)].join(' '));
    });
  });
};

const outputHabits = function(habits) {
  title('Habits');

  habits.forEach(habit => {
    if (habit.up) {
      console.log([SCORE_UP, habit.text, '|', action(ACTIONS.COMPLETE_TASK, habit._id)].join(' '));
    }
    if (habit.down) {
      console.log([SCORE_DOWN, habit.text, '|', action(ACTIONS.UNCOMPLETE_TASK, habit._id)].join(' '));
    }
  });
};

const outputProfile = function(userData) {
  console.log(userData.profile.name +
    ', lvl ' + userData.stats.lvl + ' ' +
    (n => n[0].toUpperCase() + n.slice(1))(userData.stats.class),
    '|color=black');
  const smallFont = '| color=black size=10';
  console.log([HEALTH, Math.ceil(userData.stats.hp), '/', userData.stats.maxHealth, smallFont].join(' '));
  console.log([EXP, Math.ceil(userData.stats.exp), '/', userData.stats.toNextLevel, smallFont].join(' '));
  console.log([MAGIC, Math.ceil(userData.stats.mp), '/', userData.stats.maxMP, smallFont].join(' '));
};

//   ==============================================
//   =        ==  ==========    ====  ====  ====  =
//   =  ========  =========  ==  ===  ====  ====  =
//   =  ========  ========  ====  ==  ====  ====  =
//   =  ========  ========  ====  ==  ====  ====  =
//   =      ====  ========  ====  ==   ==    ==  ==
//   =  ========  ========  ====  ===  ==    ==  ==
//   =  ========  ========  ====  ===  ==    ==  ==
//   =  ========  =========  ==  =====    ==    ===
//   =  ========        ====    =======  ====  ====
//   ==============================================

if (USER_ID === 'YOUR_USER_ID_HERE' || API_TOKEN === 'YOUR_TOKEN_HERE') {
  return failure('Please configure the plugin with your userid and token');
}

get('status')
.then(result => {
  if (result.data.status !== 'up') {
    throw Error('habitica api is down');
  }
})
.then(() => {
  if (process.argv.length > 2) {
    return processArguments();
  } else {
    return Promise.all([
      get('tasks/user'),
      get('user')])
    .then(([tasks, user]) => {
      const incompleteDailies = tasks.data
        .filter(dailyForToday)
        .filter(incomplete);

      const habits = tasks.data
        .filter(habit);

      if (incompleteDailies.length) {
        console.log(incompleteDailies.length + '|image="' + HABITICA_ICON + "'\n");
      } else {
        console.log('|templateImage="' + HABITICA_ICON+ '"');
      }

      if (incompleteDailies.length) {
        sep();
        outputIncompleteDailies(incompleteDailies);
      }

      if (habits.length) {
        sep();
        outputHabits(habits);
      }

      sep();
      outputProfile(user.data);
      sep();
      console.log('Go to website|href="https://habitica.com"');
    });
  }
})
.catch(failure);
