#!/usr/bin/env /usr/local/bin/node

// Metadata allows your plugin to show up in the app, and website.
//
//  <xbar.title>Jira</xbar.title>
//  <xbar.version>v1.0</xbar.version>
//  <xbar.author>Matthew Jacobs</xbar.author>
//  <xbar.author.github>codingmatty</xbar.author.github>
//  <xbar.desc>Jira issue display. Use custom JQL, and group by custom fields.</xbar.desc>
//  <xbar.image>http://www.hosted-somewhere/pluginimage</xbar.image>
//  <xbar.dependencies>node</xbar.dependencies>
//  <xbar.abouturl>https://github.com/codingmatty/xbar-jira</xbar.abouturl>

// Variables become preferences in the app:
//
//  <xbar.var>string(JIRA_BASE_URL="yourteam.atlassian.net"): Your specific JIRA Url.</xbar.var>
//  <xbar.var>string(JIRA_USER_NAME=""): Your Username/Email.</xbar.var>
//  <xbar.var>string(JIRA_API_KEY=""): Your User API Key. Generate one at https://id.atlassian.com/manage-profile/security/api-tokens</xbar.var>
//  <xbar.var>string(JIRA_JQL=""): The JQL used to query Jira Issues. Learn more at https://confluence.atlassian.com/jira064/advanced-searching-720416661.html</xbar.var>
//  <xbar.var>select(JIRA_GROUPBY_FIELD="status"): The issue field to group by. [status, issuetype, project, priority, custom]</xbar.var>
//  <xbar.var>string(JIRA_GROUPBY_CUSTOM_FIELD=""): The custom issue field to group by if above is set to `custom`. </xbar.var>

/******/ (() => { // webpackBootstrap
/******/ 	"use strict";
/******/ 	var __webpack_modules__ = ({

/***/ 481:
/***/ ((module) => {



const separator = Symbol('separator');

const encodeHref = url => {
	url = encodeURI(url);
	url = url.replace(/'/g, '%27');
	url = url.replace(/&/g, '%26');
	return url;
};

const create = (input, options = {}, menuLevel = 0) => {
	if (typeof options.text !== 'undefined') {
		throw new TypeError('The `text` option is not supported as a top-level option. Use it on an item instead.');
	}

	return input.map(line => {
		if (typeof line === 'string') {
			line = {text: line};
		}

		if (line === separator) {
			return '--'.repeat(menuLevel) + '---';
		}

		line = {
			...options,
			...line
		};

		const {text} = line;
		if (typeof text !== 'string') {
			throw new TypeError('The `text` property is required and should be a string');
		}

		delete line.text;

		let submenuText = '';
		if (typeof line.submenu === 'object' && line.submenu.length > 0) {
			submenuText = `\n${create(line.submenu, options, menuLevel + 1)}`;
			delete line.submenu;
		}

		const prefix = '--'.repeat(menuLevel);

		return text.split('\n').map(textLine => {
			const options = Object.keys(line).map(key => {
				const value = key === 'href' ? encodeHref(line[key]) : line[key];
				return `${key}="${value}"`;
			}).join(' ');

			return `${prefix}${textLine}|${options}`;
		}).join('\n').concat(submenuText);
	}).join('\n');
};

module.exports = (input, options) => {
	console.log(create(input, options));
};

module.exports.separator = separator;
module.exports.darkMode = process.env.BitBarDarkMode === '1';
module.exports.create = create;


/***/ }),

/***/ 276:
/***/ (function(__unused_webpack_module, exports, __webpack_require__) {


var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.fetch = void 0;
var https_1 = __importDefault(__webpack_require__(211));
function fetch(url, options) {
    return new Promise(function (resolve, reject) {
        https_1.default
            .get(url, options, function (res) {
            var data = [];
            res.on("data", function (chunk) {
                data.push(chunk);
            });
            res.on("end", function () {
                resolve(JSON.parse(Buffer.concat(data).toString()));
            });
        })
            .on("error", function (error) {
            reject(error);
        });
    });
}
exports.fetch = fetch;


/***/ }),

/***/ 64:
/***/ (function(__unused_webpack_module, exports) {


var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
var __spreadArray = (this && this.__spreadArray) || function (to, from) {
    for (var i = 0, il = from.length, j = to.length; i < il; i++, j++)
        to[j] = from[i];
    return to;
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.groupBy = void 0;
function groupBy(array, fn) {
    return array.reduce(function (result, instance) {
        var _a;
        var _b;
        var key = fn(instance);
        return __assign(__assign({}, result), (_a = {}, _a[key] = __spreadArray(__spreadArray([], ((_b = result[key]) !== null && _b !== void 0 ? _b : [])), [instance]), _a));
    }, {});
}
exports.groupBy = groupBy;


/***/ }),

/***/ 607:
/***/ (function(__unused_webpack_module, exports, __webpack_require__) {


var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
var __spreadArray = (this && this.__spreadArray) || function (to, from) {
    for (var i = 0, il = from.length, j = to.length; i < il; i++, j++)
        to[j] = from[i];
    return to;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
var bitbar_1 = __importDefault(__webpack_require__(481));
var groupBy_1 = __webpack_require__(64);
var jira_1 = __webpack_require__(794);
var _a = process.env, JIRA_BASE_URL = _a.JIRA_BASE_URL, JIRA_USER_NAME = _a.JIRA_USER_NAME, JIRA_API_KEY = _a.JIRA_API_KEY, JIRA_GROUPBY_FIELD = _a.JIRA_GROUPBY_FIELD, JIRA_GROUPBY_CUSTOM_FIELD = _a.JIRA_GROUPBY_CUSTOM_FIELD;
function main() {
    return __awaiter(this, void 0, void 0, function () {
        var error, _a, fields, issues, groupedByKey, groupedByLabel, customField, sprintField, issuesInActiveSprint, groupedIssues;
        return __generator(this, function (_b) {
            switch (_b.label) {
                case 0:
                    if ([JIRA_BASE_URL, JIRA_USER_NAME, JIRA_API_KEY].filter(function (value) { return !!value; })
                        .length < 3) {
                        bitbar_1.default([
                            { text: "Jira Error", color: "white", dropdown: false },
                            bitbar_1.default.separator,
                            "Error: Base URL, Username, and API Key all need to be set in xbar",
                        ]);
                        return [2 /*return*/];
                    }
                    return [4 /*yield*/, Promise.all([
                            jira_1.fetchFields(),
                            jira_1.fetchIssues(),
                        ]).catch(function (e) {
                            error = e.message;
                            return [[], { issues: [] }];
                        })];
                case 1:
                    _a = _b.sent(), fields = _a[0], issues = _a[1].issues;
                    groupedByKey = JIRA_GROUPBY_FIELD;
                    groupedByLabel = JIRA_GROUPBY_FIELD;
                    if (JIRA_GROUPBY_FIELD === "custom") {
                        customField = fields.find(function (_a) {
                            var name = _a.name;
                            return name.toLowerCase() === (JIRA_GROUPBY_CUSTOM_FIELD === null || JIRA_GROUPBY_CUSTOM_FIELD === void 0 ? void 0 : JIRA_GROUPBY_CUSTOM_FIELD.toLowerCase());
                        });
                        groupedByKey = customField === null || customField === void 0 ? void 0 : customField.key;
                        groupedByLabel = customField === null || customField === void 0 ? void 0 : customField.name;
                    }
                    sprintField = fields.find(function (_a) {
                        var name = _a.name;
                        return name.toLowerCase() === "sprint";
                    });
                    issuesInActiveSprint = sprintField
                        ? issues.filter(function (issue) {
                            var _a;
                            return (_a = issue.fields[sprintField.key]) === null || _a === void 0 ? void 0 : _a.some(function (_a) {
                                var state = _a.state;
                                return state === "active";
                            });
                        })
                        : issues;
                    groupedIssues = groupBy_1.groupBy(issuesInActiveSprint, function (issue) { var _a; return (_a = issue.fields[groupedByKey]) === null || _a === void 0 ? void 0 : _a.name; });
                    if (error) {
                        bitbar_1.default([
                            {
                                text: "Jira Error",
                                color: "white",
                                dropdown: false,
                            },
                            bitbar_1.default.separator,
                            { text: "Error: " + error },
                        ]);
                    }
                    else {
                        bitbar_1.default(__spreadArray([
                            {
                                text: "Jira: " + issues.length + " Issues",
                                color: "white",
                                dropdown: false,
                            },
                            bitbar_1.default.separator,
                            "Grouped By: " + (groupedByLabel !== null && groupedByLabel !== void 0 ? groupedByLabel : "No Valid Grouping Applied"),
                            bitbar_1.default.separator
                        ], Object.keys(groupedIssues).reduce(function (result, groupKey, i, array) {
                            var group = groupedIssues[groupKey];
                            return __spreadArray(__spreadArray(__spreadArray([], result), [
                                array.length === 1 && groupKey === "undefined"
                                    ? null
                                    : { text: groupKey }
                            ]), group.map(function (_a) {
                                var key = _a.key, fields = _a.fields;
                                return ({
                                    text: "\u2022\t" + key + ": " + fields.summary,
                                    href: "https:" + JIRA_BASE_URL + "/browse/" + key,
                                });
                            })).filter(Boolean);
                        }, [])));
                    }
                    return [2 /*return*/];
            }
        });
    });
}
try {
    main();
}
catch (e) {
    console.error(e.message);
}


/***/ }),

/***/ 794:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {


Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.fetchIssues = exports.fetchFields = void 0;
var fetch_1 = __webpack_require__(276);
var _a = process.env, JIRA_BASE_URL = _a.JIRA_BASE_URL, JIRA_USER_NAME = _a.JIRA_USER_NAME, JIRA_API_KEY = _a.JIRA_API_KEY, JIRA_JQL = _a.JIRA_JQL;
function fetchFields() {
    var Authorization = "Basic " + Buffer.from(JIRA_USER_NAME + ":" + JIRA_API_KEY).toString("base64");
    return fetch_1.fetch("https://" + JIRA_BASE_URL + "/rest/api/3/field", {
        headers: { Authorization: Authorization },
    });
}
exports.fetchFields = fetchFields;
function fetchIssues() {
    var Authorization = "Basic " + Buffer.from(JIRA_USER_NAME + ":" + JIRA_API_KEY).toString("base64");
    return fetch_1.fetch("https://" + JIRA_BASE_URL + "/rest/api/3/search?jql=" + encodeURIComponent(JIRA_JQL), { headers: { Authorization: Authorization } });
}
exports.fetchIssues = fetchIssues;


/***/ }),

/***/ 211:
/***/ ((module) => {

module.exports = require("https");;

/***/ })

/******/ 	});
/************************************************************************/
/******/ 	// The module cache
/******/ 	var __webpack_module_cache__ = {};
/******/ 	
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/ 		// Check if module is in cache
/******/ 		var cachedModule = __webpack_module_cache__[moduleId];
/******/ 		if (cachedModule !== undefined) {
/******/ 			return cachedModule.exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = __webpack_module_cache__[moduleId] = {
/******/ 			// no module.id needed
/******/ 			// no module.loaded needed
/******/ 			exports: {}
/******/ 		};
/******/ 	
/******/ 		// Execute the module function
/******/ 		__webpack_modules__[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/ 	
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/ 	
/************************************************************************/
/******/ 	
/******/ 	// startup
/******/ 	// Load entry module and return exports
/******/ 	// This entry module is referenced by other modules so it can't be inlined
/******/ 	var __webpack_exports__ = __webpack_require__(607);
/******/ 	
/******/ })()
;