#!/usr/bin/env xcrun swift

/*
 * <bitbar.title>Trackpad Dot</bitbar.title>
 * <bitbar.version>v1.0</bitbar.version>
 * <bitbar.author>Manu Wallner</bitbar.author>
 * <bitbar.author.github>milch</bitbar.author.github>
 * <bitbar.desc>Displays a coloured dot in the menu bar that gradually changes from green to yellow to red the lower your Magic Trackpad's battery</bitbar.desc>
 * <bitbar.image>http://i.imgur.com/4G7B76u.png</bitbar.image>
 * <bitbar.dependencies>Xcode,swift</bitbar.dependencies>
 */

import Foundation

typealias Color = (red : UInt, green : UInt, blue : UInt)

func shell(_ args: String...) -> String {
    let task = Process()
    task.launchPath = "/bin/bash"
    task.arguments = ["-c"] + args
    
    let pipe = Pipe()
    task.standardOutput = pipe
    task.launch()
    
    let data = pipe.fileHandleForReading.readDataToEndOfFile()
    let output: String = NSString(data: data, encoding: String.Encoding.utf8.rawValue) as! String
    
    return output.trimmingCharacters(in: CharacterSet.whitespacesAndNewlines)
}

func rgb(_ r : UInt, _ g : UInt , _ b : UInt) -> Color {
    return (red: r, green: g, blue: b)
}

func linear(_ initial: UInt, next: UInt, percent: Double) -> Double {
    return Double(initial) * (1.0 - percent) + Double(next) * (percent)
}

func interpolate(_ first : Color, second : Color, percent: Double) -> Color {
    let r = linear(first.red, next: second.red, percent: percent)
    let g = linear(first.green, next: second.green, percent: percent)
    let b = linear(first.blue, next: second.blue, percent: percent)
    return rgb(UInt(r), UInt(g), UInt(b))
}

func componentToHex(_ component : UInt) -> String {
    return String(format:"%02X", component)
}

func colorToHex(_ color: Color) -> String {
    return [
        "#",
        componentToHex(color.red),
        componentToHex(color.green),
        componentToHex(color.blue)
        ].joined(separator: "")
}

let commandString = "ioreg -c BNBTrackpadDevice | egrep -o 'BatteryPercent. = \\d{0,3}' | sed -E 's/[^0-9]+([0-9]{0,3})/\\1/g'"
let trackpadPercentString = shell(commandString)
var trackpadDropdownString = "\(trackpadPercentString)%"
var colorString = ""

let colors = [
    rgb(231, 76, 60),
    rgb(241, 196, 15),
    rgb(0, 177, 106)
]

if let trackpadPercent = Int(trackpadPercentString) {
    var interpolationColors : (first: Color, second: Color) = (first: rgb(0,0,0), second: rgb(0,0,0))
    
    switch(trackpadPercent) {
    case 0...50: interpolationColors = (first: colors[0], second: colors[1])
    case 50...100: interpolationColors = (first: colors[1], second: colors[2])
    // Catch all to satisfy the compiler
    default: break
    }
    let percent = trackpadPercent % 50 == 0 ? 1.0 : Double(trackpadPercent % 50) / 50.0
    let color = interpolate(interpolationColors.first, second: interpolationColors.second, percent: percent)
    colorString = colorToHex(color)
} else {
    colorString = "#bdc3c7"
    trackpadDropdownString = "Not connected"
}

print("● | color=\(colorString)")
print("---")
print("Trackpad: \(trackpadDropdownString)")

