#!/usr/bin/env /usr/local/bin/node


/* jshint esversion: 6 */

/*
 * <bitbar.title>Gulp Launcher</bitbar.title>
 * <bitbar.version>v1.0</bitbar.version>
 * <bitbar.author>Aaron Crawford</bitbar.author>
 * <bitbar.author.github>aaroncrawford</bitbar.author.github>
 * <bitbar.image>https://i.imgur.com/RAxo5tG.png</bitbar.image>
 * <bitbar.desc>Quickly launch gulp processes on projects.  Useful for agency developers with a lot of projects.  Editing of file required to list your projects.</bitbar.desc>
 * <bitbar.dependencies>node</bitbar.dependencies>
 */

// EDIT ME
var file = '/path/to/_projects.json'
// Example _projects.json file : (you can just create an array in this file, but our file was huge so we decided to store in a separate file)
/*

[
  {
    "name": "Project 1",
    "path": "/path/to/readable/file/project1/src"
  },
  {
    "name": "Brohn Homes",
    "path": "/path/to/readable/file/project2/src"
  }
]

*/


var fs = require('fs')

fs.readFile(file, 'utf8', (e, data) => {
	console.log('🥤')
	console.log('---')

	var projects = JSON.parse(data)

	projects.map((d) => {
		var path = d.path.replace(" ", "\\\\ ")
		// Currently just goes to the directory and runs 'gulp'.  If you want to modify this process, edit this line
		console.log(`${d.name}|bash="cd ${path} && gulp" terminal=true`)
	})	
})


