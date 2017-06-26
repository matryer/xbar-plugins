#!/usr/bin/env /usr/local/bin/node

/*
  <bitbar.title>Microsoft Remote Desktop - Quick Connect</bitbar.title>
  <bitbar.version>v0.1.0</bitbar.version>
  <bitbar.author>Marco Ferrer</bitbar.author>
  <bitbar.author.github>marrferr</bitbar.author.github>
  <bitbar.desc>Quick and easy access to Microsoft Remote Desktop from any desktop space. Allows connection opening and switching. Only displays connections with labels assigned.</bitbar.desc>
  <bitbar.image>https://www.dropbox.com/s/aiodjoketuujbaf/mrd-quick-connect.png?raw=1</bitbar.image>
  <bitbar.dependencies>node,node-simple-plist,node-bitbar</bitbar.dependencies>

	************** Decoded Contents of the Apple Script Helper ****************
	***************************************************************************
	on run connectionLabel
		set appTitle to "Microsoft Remote Desktop"
		set connectionLabel to (text of item 1 of connectionLabel as string)
		#set connectionLabel to "show_connection_list"
		tell application appTitle
			activate
			tell application "System Events"
				tell process appTitle
					
					#Make sure the frontmost windows from mrdp is the connection list
					set frontmost to true
					delay 0.4
					set menuItems to every menu item of menu "Window" of menu bar 1
					set connectionListMenuItem to false
					set targetMenuItem to false
					
					try
						repeat with menuItem in menuItems
							if (name of menuItem as string) is (connectionLabel & " ") then
								set targetMenuItem to menuItem
							else if name of menuItem as string is appTitle then
								set connectionListMenuItem to menuItem
							end if
						end repeat
					end try
					
					if targetMenuItem is not false then
						click targetMenuItem
					else
						click connectionListMenuItem
						delay 0.3
						
						set connectionListWindow to windows where title contains appTitle
						--Initial focusing of window to prevent problems with closing it later on.
						repeat with w in connectionListWindow
							set focused of w to true
						end repeat
						
						--Clear the search input
						keystroke tab
						key code 115 --home
						perform action "AXRaise" of (first window whose title is appTitle)
						keystroke "f" using {command down} --Focus on search input
						keystroke "a" using {command down} --Select all of inputs contents
						key code 51 --delete
						
						if (connectionLabel as string) is not "show_connection_list" then
							--Clear any previously selected list item before filtering
							keystroke tab
							key code 115 --home
							perform action "AXRaise" of (first window whose title is appTitle)
							keystroke "f" using {command down} --Focus on search input
							keystroke connectionLabel --search query
							delay 0.1
							keystroke tab
							key code 123 --left arrow 
							key code 124 --right arrow 
							key code 125 --down
							delay 0.3
							key code 36 --enter	
							
							--This method of closing connection window is unreliable 
							#perform action "AXPress" of button 1 of (first window whose title is appTitle)
							--Work around for consisten results. Using two forms of closing the connection list window
							--Each method covers a particular window state
							--Closes window when it is outside of a desktop with a open rdc connection.
							try
								repeat with w in windows
									if title of w contains appTitle then
										set focused of w to true
										perform action "AXPress" of button 1 of w
									end if
								end repeat
							end try
							
							--Allows window to be close it is focused into a desktop with a running rdc connection
							tell process "Finder"
								click button 1 of item 1 of connectionListWindow
							end tell
							
						end if
					end if
				end tell
			end tell
		end tell
	end run
	*******************************************************************
	*******************************************************************
 */

'use strict';

var mrdIcon = "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",
	mrdAppleScriptHelper = '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AHQAaQBvAG4AIAB3AGkAbgBkAG8AdwAgAGkAcwAgAHUAbgByAGUAbABpAGEAYgBsAGUAIAIBQwACAUcBSA0BRwADbAAGAdgB2P9JAUkBSgH/SQAADAFJAFQATnBlcmZvcm0gYWN0aW9uICJBWFByZXNzIiBvZiBidXR0b24gMSBvZiAoZmlyc3Qgd2luZG93IHdob3NlIHRpdGxlIGlzIGFwcFRpdGxlKQACAAAOAUoAAbEBSxEBSwCcAHAAZQByAGYAbwByAG0AIABhAGMAdABpAG8AbgAgACIAQQBYAFAAcgBlAHMAcwAiACAAbwBmACAAYgB1AHQAdABvAG4AIAAxACAAbwBmACAAKABmAGkAcgBzAHQAIAB3AGkAbgBkAG8AdwAgAHcAaABvAHMAZQAgAHQAaQB0AGwAZQAgAGkAcwAgAGEAcABwAFQAaQB0AGwAZQApAgFIAAIBTAFNDQFMAANsAAEB2AHY/0gBTgFPAf9IAAAMAU4AXgBYV29yayBhcm91bmQgZm9yIGNvbnNpc3RlbiByZXN1bHRzLiBVc2luZyB0d28gZm9ybXMgb2YgY2xvc2luZyB0aGUgY29ubmVjdGlvbiBsaXN0IHdpbmRvdwACAAAOAU8AAbEBUBEBUACwAFcAbwByAGsAIABhAHIAbwB1AG4AZAAgAGYAbwByACAAYwBvAG4AcwBpAHMAdABlAG4AIAByAGUAcwB1AGwAdABzAC4AIABVAHMAaQBuAGcAIAB0AHcAbwAgAGYAbwByAG0AcwAgAG8AZgAgAGMAbABvAHMAaQBuAGcAIAB0AGgAZQAgAGMAbwBuAG4AZQBjAHQAaQBvAG4AIABsAGkAcwB0ACAAdwBpAG4AZABvAHcCAU0AAgFRAVINAVEAA2wAAQHYAdj/RwFTAVQB/0cAAAwBUwAyACxFYWNoIG1ldGhvZCBjb3ZlcnMgYSBwYXJ0aWN1bGFyIHdpbmRvdyBzdGF0ZQACAAAOAVQAAbEBVREBVQBYAEUAYQBjAGgAIABtAGUAdABoAG8AZAAgAGMAbwB2AGUAcgBzACAAYQAgAHAAYQByAHQAaQBjAHUAbABhAHIAIAB3AGkAbgBkAG8AdwAgAHMAdABhAHQAZQIBUgACAVYBVw0BVgADbAABAdgB2P9GAVgBWQH/RgAADAFYAE8ASUNsb3NlcyB3aW5k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',
	packageJson	= {
  		name: "mrd-quick-connect",
  		version: "0.1.0",
  		description: "BitBar plugin for quick and easy access to Microsoft Remote Desktop from any desktop space. Allows connection opening and switching. Only displays connections with labels assigned.",
  		repository: "",
  		license: "MIT",
  		dependencies: {
    		"bitbar": "^0.3.0",
    		"simple-plist": "^0.2.1"
  		},
  		appleScriptVer: "0.1.0"
	},
	os 		= require('os'),
	fs  	= require('fs'),
	bitbar 	= null,
	plist 	= null,
	dependencyDirPath 	= __dirname + '/' + packageJson.name,
	mrdContainerBasePath= os.homedir() + '/Library/Containers/com.microsoft.rdc.mac',
	mrdPreferencePath 	= mrdContainerBasePath + '/Data/Library/Preferences/com.microsoft.rdc.mac.plist',
	mrdWindowListPath	= mrdContainerBasePath + '/Data/Library/Saved Application State/com.microsoft.rdc.mac.savedState/windows.plist',
	appleScriptPath		= dependencyDirPath + '/'+ packageJson.name + '.' + packageJson.appleScriptVer + '.scpt';
	

function getBarContents(){
	var barContents = [{
			text: ' ',
			image: mrdIcon,
			dropdown: false
		},
		bitbar.sep,
		{
			text: "☰  Open Connection List",
			bash: "/bin/bash",
			param1: "-c",
			param2: "osascript " + bitbarEscape(appleScriptPath) + " show_connection_list",
			terminal: false
		},
		bitbar.sep
	];

	var mrdPrefs = getPlistJson(mrdPreferencePath);
	if(mrdPrefs == null){
		barContents.push({
			text: '❌ Preference file could not be found.'
		});
		barContents.push({
			text: mrdPreferencePath
		});
		return barContents;
	}

	var windowList = getWindowList(),
		connectionIds = mrdPrefs["bookmarkorder.ids"];

	for (var i = 0; i < connectionIds.length; i++) {		
		
		var id 		 = connectionIds[i],
			label 	 = mrdPrefs["bookmarks.bookmark."+id+".label"],
			username = mrdPrefs["bookmarks.bookmark."+id+".username"],
			hostname = mrdPrefs["bookmarks.bookmark."+id+".hostname"];
		
		//For now only connections with labels will be displayed
		if(label != undefined && label != ''){
			var connectionIsOpen = windowList.hasOwnProperty(label);

			//◉ ◯ ◎			
			var barItem = {
					text: (connectionIsOpen ? '◉  ' : '◯  ') + label,
					size: "13",
					//Only config that succesfully runs apple script help with terminal false
					//https://github.com/matryer/bitbar/issues/162 
					bash: "/bin/bash",
					param1: "-c",
					param2: "osascript " + bitbarEscape(appleScriptPath) + " " + bitbarEscape(label),
					terminal: false
			};

			//Stylize connection based on state
			if(connectionIsOpen){
				barItem.font = "HelveticaNeue-Bold";
			} else {
				barItem.color = "gray";
			}

			barContents.push(barItem);
			barContents.push({ 
				text: "Username: " + username,
				size: "10"
			});
			barContents.push(bitbar.sep);
		}
	}	

	return barContents;
}

function bitbarEscape(text){
	return text.replace(" ","\\\ ");
}

function getWindowList(){
	var mrdWindowStates = getPlistJson(mrdWindowListPath),
		windowList = {};

	if (mrdWindowStates != null){
		for(var i = 0; i < mrdWindowStates.length; i++){

			var mrdWindow = mrdWindowStates[i];
			if(mrdWindow.hasOwnProperty("NSTitle")){

				//The window title is stored in the plist with trailing spaces. Triming before storing
				mrdWindow.NSTitle = mrdWindow.NSTitle.trim();
				windowList[mrdWindow.NSTitle] = mrdWindow;
			}
		}
	}

	return windowList;
}

function getPlistJson(filePath){
	var result = null;
	try{
		result = plist.readFileSync(filePath);		
	}catch(ignored){
		//When an mrd connection is holding up the quiting of the application
		//it deletes the window plist and fails to quit.
	}
	return result;
}

function initDependencies(){
	if (!fs.existsSync(dependencyDirPath)){
		fs.mkdirSync(dependencyDirPath);	
	} 		

	if (!fs.existsSync(appleScriptPath)){
		fs.writeFileSync(appleScriptPath, Buffer.from(mrdAppleScriptHelper, 'base64'));
	}

	try{
		bitbar 	= require( dependencyDirPath + '/node_modules/bitbar' );
		plist 	= require( dependencyDirPath + '/node_modules/simple-plist' );
		return true;
	}catch(e){
	}
	return false;
}

function prepareForInstall(){

	if (!fs.existsSync(dependencyDirPath + "/package.json")){
		fs.writeFileSync(dependencyDirPath + "/package.json", JSON.stringify(packageJson), { flag: 'wx' });
	}

	console.log(
		"⬇\n---\n Click to Install Dependencies | bash='cd " + 
		dependencyDirPath.replace(" ","\\\\ ") + " && npm install'"
	);
}

if(initDependencies()){
	bitbar(getBarContents());	
}else{
	prepareForInstall();
}

