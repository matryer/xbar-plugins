#!/usr/bin/env xcrun swift

/*
 * <bitbar.title>Battery Dot</bitbar.title>
 * <bitbar.version>v1.0</bitbar.version>
 * <bitbar.author>alex_rockt</bitbar.author>
 * <bitbar.author.github>alex_rockt</bitbar.author.github>
 * <bitbar.desc>Displays a coloured dot for trackpad, keyboard and laptop battery in the menu bar that changes from green to yellow to red the lower the power gets - based on the trackpad-dot plugin by Manu Wallner (https://getbitbar.com/plugins/System/trackpad-dot.1h.swift).
	The plugin uses ANSI colors to display all three battery levels next to each other - the downside is that it's limited to the ANSI colors - so no orange available</bitbar.desc>
 * <bitbar.image>http://i.imgur.com/3EimpGH.png</bitbar.image>
 * <bitbar.dependencies>Xcode,swift</bitbar.dependencies>
 */

import Foundation

typealias Color = (red : UInt, green : UInt, blue : UInt)

struct capacity {
    var colorString: String
    var dropdownString: String
    var ansiColorString: String
    
    init() {
        colorString = ""
        dropdownString = ""
        ansiColorString = ""
    }
}

struct batteryCapacities {
    var trackpad: capacity
    var keyboard: capacity
    var battery: capacity
    
    init() {
        trackpad = capacity()
        keyboard = capacity()
        
        battery = capacity()
    }
}

func shell(args: String...) -> String {
    let task = Process()
    task.launchPath = "/bin/bash"
    task.arguments = ["-c"] + args
    
    let pipe = Pipe()
    task.standardOutput = pipe
    task.launch()
    
    let data = pipe.fileHandleForReading.readDataToEndOfFile()
    let output: String = String(data: data, encoding: String.Encoding.utf8)!
    
    return output.trimmingCharacters(in: CharacterSet.whitespacesAndNewlines)
}

func rgb(r : UInt, g : UInt , b : UInt) -> Color {
    return (red: r, green: g, blue: b)
}

let colors = [
    rgb(r: 231, g: 76, b: 60),
    rgb(r: 241, g: 196, b: 15),
    rgb(r: 0, g: 177, b: 106)
]

func linear(initial: UInt, next: UInt, percent: Double) -> Double {
    return Double(initial) * (1.0 - percent) + Double(next) * (percent)
}


func interpolate(first : Color, second : Color, percent: Double) -> Color {
    let r = linear(initial: first.red, next: second.red, percent: percent)
    let g = linear(initial: first.green, next: second.green, percent: percent)
    let b = linear(initial: first.blue, next: second.blue, percent: percent)
    return rgb(r: UInt(r), g: UInt(g), b: UInt(b))
}

func componentToHex(component : UInt) -> String {
    return String(format:"%02X", component)
}

func colorToHex(color: Color) -> String {
    return [
        "#",
        componentToHex(component: color.red),
        componentToHex(component: color.green),
        componentToHex(component: color.blue)
        ].joined(separator: "")
}

func calculateColorStrings(percentString: String) -> (colorString: String, dropdownString: String, ansiColorString: String) {
    var dropdownString = "\(percentString)%"
    var colorString = ""
    var ansiColor = ""
    
    if let percent = Int(percentString) {
        var interpolationColors : (first: Color, second: Color) = (first: rgb(r: 0,g: 0,b: 0), second: rgb(r: 0,g: 0,b: 0))
        switch(percent) {
        case 0...50: interpolationColors = (first: colors[0], second: colors[1])
        case 50...100: interpolationColors = (first: colors[1], second: colors[2])
        // Catch all to satisfy the compiler
        default: break
        }
        
        switch(percent) {
        case 0...25: ansiColor = "[31m" //red
        case 25...50: ansiColor = "[35m" // magenta
        case 50...75: ansiColor = "[33m" // yellow
        case 75...100: ansiColor = "[32m" // green
        default: break
        }
        
        let percent = percent % 50 == 0 ? 1.0 : Double(percent % 50) / 50.0
        let color = interpolate(first: interpolationColors.first, second: interpolationColors.second, percent: percent)
        colorString = colorToHex(color: color)
        
        
    } else {
        colorString = "#bdc3c7"
        dropdownString = "Not connected"
    }
    return (colorString, dropdownString, ansiColor)
}

func trackpadBattery() -> (c: String, s: String, a: String) {
    let commandString = "ioreg -n AppleDeviceManagementHIDEventService | fgrep BatteryPercent | fgrep -v { | sed 's/[^[:digit:]]//g'"
    let percentString = shell(args: commandString)
    let (colorString, dropdownString, ansiColor) = calculateColorStrings(percentString: percentString)
    return (colorString, dropdownString, ansiColor)
}

func keyboardBattery() -> (c: String, s: String, a: String) {
    let commandString = "ioreg -c AppleBluetoothHIDKeyboard | grep BatteryPercent | fgrep -v { | sed 's/[^[:digit:]]//g'"
    let percentString = shell(args: commandString)
    let (colorString, dropdownString, ansiColor) = calculateColorStrings(percentString: percentString)
    return (colorString, dropdownString, ansiColor)
}

func macbookBattery() -> (c: String, s: String, a: String) {
    
    var percentAsDouble = 0.0
    
    let currentPowerCommand = "system_profiler SPPowerDataType | grep 'Charge Remaining' | awk '{print $4}'"
    let fullPowerCommand = "system_profiler SPPowerDataType | grep 'Full Charge Capacity' | awk '{print $5}'"
    let currentCapacityString = shell(args: currentPowerCommand)
    let fullCapacityString = shell(args: fullPowerCommand)
    
    if let current = Int(currentCapacityString), let full = Int(fullCapacityString) {
        percentAsDouble = (Double(current)/Double(full)*100)
    }
    
    let percentString = String(Int(floor(percentAsDouble)))
    let (colorString, dropdownString, ansiColor) = calculateColorStrings(percentString: percentString)
    return (colorString, dropdownString, ansiColor)
}

extension String {
    func padding(length: Int) -> String {
        return self.padding(toLength: length, withPad: " ", startingAt: 0)
    }
}

var powerStatus = batteryCapacities()

var (c,s,a) = macbookBattery()
powerStatus.battery.colorString = c
powerStatus.battery.dropdownString = s
powerStatus.battery.ansiColorString = a

(c,s,a) = keyboardBattery()
powerStatus.keyboard.colorString = c
powerStatus.keyboard.dropdownString = s
powerStatus.keyboard.ansiColorString = a

(c,s,a) = trackpadBattery()
powerStatus.trackpad.colorString = c
powerStatus.trackpad.dropdownString = s
powerStatus.trackpad.ansiColorString = a

print("B: \u{001B}\(powerStatus.battery.ansiColorString)●\u{001B}[0m K: \u{001B}\(powerStatus.keyboard.ansiColorString)●\u{001B}[0m T: \u{001B}\(powerStatus.trackpad.ansiColorString)●\u{001B}[0m| size=10 ansi=true")
print("---")
print("Battery:".padding(length: 11) + powerStatus.battery.dropdownString + "| font=Courier color=\(powerStatus.battery.colorString)")
print("Keyboard:".padding(length: 11) + powerStatus.keyboard.dropdownString + "| font=Courier color=\(powerStatus.keyboard.colorString)")
print("Trackpad:".padding(length: 11) + powerStatus.trackpad.dropdownString + "| font=Courier color=\(powerStatus.trackpad.colorString)")
