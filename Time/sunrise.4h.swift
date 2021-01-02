#!/usr/bin/env xcrun swift

/*
 * <bitbar.title>Sunrise</bitbar.title>
 * <bitbar.version>v1.0</bitbar.version>
 * <bitbar.author>Brad Greenlee</bitbar.author>
 * <bitbar.author.github>bgreenlee</bitbar.author.github>
 * <bitbar.desc>Displays sunrise, sunset, and day length for a location.</bitbar.desc>
 * <bitbar.image>https://raw.githubusercontent.com/bgreenlee/SunriseBitBar/main/sunrise-bitbar.png</bitbar.image>
 * <bitbar.dependencies>Xcode,swift</bitbar.dependencies>
 * <bitbar.abouturl>https://github.com/bgreenlee/SunriseBitBar</bitbar.abouturl>
 */

// Customizations

let LOCATION = "Seattle, WA" // or lat,lon coordinates
let DAYS = 7 // number of days to show
let PAST_DAYS = 0 // number of days in the past to show
let LOCATION_FONT = "Menlo"
let LOCATION_COLOR = "black"
let SUNRISE_FONT = "Menlo"
let SUNRISE_COLOR = "blue"
let SYMBOL_COLOR = "orange"

// Main code

let coordinate = try parseLocation(LOCATION)
// Reverse geocode so we can get the timezone in the location
let coder = CLGeocoder()
let loc = CLLocation(latitude: coordinate.latitude, longitude: coordinate.longitude)
var placemark: CLPlacemark?
var timezone: TimeZone?
coder.reverseGeocodeLocation(loc) { (placemarks, error) in
    placemark = placemarks?.last
    timezone = placemark?.timeZone
    CFRunLoopStop(CFRunLoopGetCurrent())
}
CFRunLoopRun() // wait for reverse geocode to finish

print(":sunrise:\n---\n")

if let placemark = placemark {
    if let city = placemark.locality, let administrativeArea = placemark.administrativeArea, let country = placemark.country {
        if country == "United States" {
            print("\(city), \(administrativeArea)|font=\(LOCATION_FONT) color=\(LOCATION_COLOR)")
        } else {
            print("\(city), \(administrativeArea), \(country)|font=\(LOCATION_FONT) color=\(LOCATION_COLOR)")
        }
    } else {
        print("\(placemark.name ?? "Unknown")")
    }
}

let today = Date()
for i in -PAST_DAYS..<DAYS-PAST_DAYS {
    let date = Calendar.current.date(byAdding: .day, value: i, to: today)!
    if let solar = Solar(for: date, coordinate: coordinate) {
        let dateFormatter = DateFormatter()
        dateFormatter.setLocalizedDateFormatFromTemplate("MMMd")
        print(dateFormatter.string(from: date).padding(toLength: 7, withPad: " ", startingAt: 0), terminator:"")

        let timeFormatter = DateFormatter()
        timeFormatter.dateStyle = .none
        timeFormatter.timeStyle = .short
        if let timezone = timezone {
            timeFormatter.timeZone = timezone
        }

        let sunrise = timeFormatter.string(from: solar.sunrise!)
        let sunset = timeFormatter.string(from: solar.sunset!)
        let daylengthFormatter = DateComponentsFormatter()
        daylengthFormatter.unitsStyle = .abbreviated
        daylengthFormatter.allowedUnits = [.minute, .hour]
        let formattedDayLength = daylengthFormatter.string(from: solar.sunrise!, to: solar.sunset!)!

        print(":sunrise: \(sunrise) :sunset: \(sunset) :clock: \(formattedDayLength)|font=\(SUNRISE_FONT)\(i == 0 ? "-Bold" : "") color=\(SUNRISE_COLOR) sfcolor=\(SYMBOL_COLOR)")
    }
}

struct ValidationError: Error, CustomStringConvertible {
    var description:String

    init(_ description:String) {
        self.description = description
    }
}

func parseLocation(_ argument: String) throws -> CLLocationCoordinate2D {
    let parts = argument.components(separatedBy: ",")
    if parts.count == 2, let latitude = Double(parts[0]), let longitude = Double(parts[1]) {
        return CLLocationCoordinate2D(latitude: latitude, longitude: longitude)
    } else {
        // see if we can geocode it
        let coder = CLGeocoder()
        var location: CLLocation?
        coder.geocodeAddressString(argument) { (placemarks, error) in
            location = placemarks?.last?.location
            CFRunLoopStop(CFRunLoopGetCurrent())
        }
        CFRunLoopRun()
        if let location = location {
            return location.coordinate
        }
        throw ValidationError("Could not parse location. Provide either a valid place name or latitude,longitude.")
    }
}

//
//  Solar.swift
//  SolarExample
//
//  Created by Chris Howell on 16/01/2016.
//  Copyright © 2016 Chris Howell. All rights reserved.
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the “Software”), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
//  THE SOFTWARE.
//
import Foundation
import CoreLocation

public struct Solar {
    
    /// The coordinate that is used for the calculation
    public let coordinate: CLLocationCoordinate2D
    
    /// The date to generate sunrise / sunset times for
    public fileprivate(set) var date: Date
    
    public fileprivate(set) var sunrise: Date?
    public fileprivate(set) var sunset: Date?
    public fileprivate(set) var civilSunrise: Date?
    public fileprivate(set) var civilSunset: Date?
    public fileprivate(set) var nauticalSunrise: Date?
    public fileprivate(set) var nauticalSunset: Date?
    public fileprivate(set) var astronomicalSunrise: Date?
    public fileprivate(set) var astronomicalSunset: Date?
    
    // MARK: Init
    
    public init?(for date: Date = Date(), coordinate: CLLocationCoordinate2D) {
        self.date = date
        
        guard CLLocationCoordinate2DIsValid(coordinate) else {
            return nil
        }
        
        self.coordinate = coordinate
        
        // Fill this Solar object with relevant data
        calculate()
    }
    
    // MARK: - Public functions
    
    /// Sets all of the Solar object's sunrise / sunset variables, if possible.
    /// - Note: Can return `nil` objects if sunrise / sunset does not occur on that day.
    public mutating func calculate() {
        sunrise = calculate(.sunrise, for: date, and: .official)
        sunset = calculate(.sunset, for: date, and: .official)
        civilSunrise = calculate(.sunrise, for: date, and: .civil)
        civilSunset = calculate(.sunset, for: date, and: .civil)
        nauticalSunrise = calculate(.sunrise, for: date, and: .nautical)
        nauticalSunset = calculate(.sunset, for: date, and: .nautical)
        astronomicalSunrise = calculate(.sunrise, for: date, and: .astronimical)
        astronomicalSunset = calculate(.sunset, for: date, and: .astronimical)
    }
    
    // MARK: - Private functions
    
    fileprivate enum SunriseSunset {
        case sunrise
        case sunset
    }
    
    /// Used for generating several of the possible sunrise / sunset times
    fileprivate enum Zenith: Double {
        case official = 90.83
        case civil = 96
        case nautical = 102
        case astronimical = 108
    }
    
    fileprivate func calculate(_ sunriseSunset: SunriseSunset, for date: Date, and zenith: Zenith) -> Date? {
        guard let utcTimezone = TimeZone(identifier: "UTC") else { return nil }
        
        // Get the day of the year
        var calendar = Calendar(identifier: .gregorian)
        calendar.timeZone = utcTimezone
        guard let dayInt = calendar.ordinality(of: .day, in: .year, for: date) else { return nil }
        let day = Double(dayInt)
        
        // Convert longitude to hour value and calculate an approx. time
        let lngHour = coordinate.longitude / 15
        
        let hourTime: Double = sunriseSunset == .sunrise ? 6 : 18
        let t = day + ((hourTime - lngHour) / 24)
        
        // Calculate the suns mean anomaly
        let M = (0.9856 * t) - 3.289
        
        // Calculate the sun's true longitude
        let subexpression1 = 1.916 * sin(M.degreesToRadians)
        let subexpression2 = 0.020 * sin(2 * M.degreesToRadians)
        var L = M + subexpression1 + subexpression2 + 282.634
        
        // Normalise L into [0, 360] range
        L = normalise(L, withMaximum: 360)
        
        // Calculate the Sun's right ascension
        var RA = atan(0.91764 * tan(L.degreesToRadians)).radiansToDegrees
        
        // Normalise RA into [0, 360] range
        RA = normalise(RA, withMaximum: 360)
        
        // Right ascension value needs to be in the same quadrant as L...
        let Lquadrant = floor(L / 90) * 90
        let RAquadrant = floor(RA / 90) * 90
        RA = RA + (Lquadrant - RAquadrant)
        
        // Convert RA into hours
        RA = RA / 15
        
        // Calculate Sun's declination
        let sinDec = 0.39782 * sin(L.degreesToRadians)
        let cosDec = cos(asin(sinDec))
        
        // Calculate the Sun's local hour angle
        let cosH = (cos(zenith.rawValue.degreesToRadians) - (sinDec * sin(coordinate.latitude.degreesToRadians))) / (cosDec * cos(coordinate.latitude.degreesToRadians))
        
        // No sunrise
        guard cosH < 1 else {
            return nil
        }
        
        // No sunset
        guard cosH > -1 else {
            return nil
        }
        
        // Finish calculating H and convert into hours
        let tempH = sunriseSunset == .sunrise ? 360 - acos(cosH).radiansToDegrees : acos(cosH).radiansToDegrees
        let H = tempH / 15.0
        
        // Calculate local mean time of rising
        let T = H + RA - (0.06571 * t) - 6.622
        
        // Adjust time back to UTC
        var UT = T - lngHour
        
        // Normalise UT into [0, 24] range
        UT = normalise(UT, withMaximum: 24)
        
        // Calculate all of the sunrise's / sunset's date components
        let hour = floor(UT)
        let minute = floor((UT - hour) * 60.0)
        let second = (((UT - hour) * 60) - minute) * 60.0
        
        let shouldBeYesterday = lngHour > 0 && UT > 12 && sunriseSunset == .sunrise
        let shouldBeTomorrow = lngHour < 0 && UT < 12 && sunriseSunset == .sunset
        
        let setDate: Date
        if shouldBeYesterday {
            setDate = Date(timeInterval: -(60 * 60 * 24), since: date)
        } else if shouldBeTomorrow {
            setDate = Date(timeInterval: (60 * 60 * 24), since: date)
        } else {
            setDate = date
        }
        
        var components = calendar.dateComponents([.day, .month, .year], from: setDate)
        components.hour = Int(hour)
        components.minute = Int(minute)
        components.second = Int(second)
        
        calendar.timeZone = utcTimezone
        return calendar.date(from: components)
    }
    
    /// Normalises a value between 0 and `maximum`, by adding or subtracting `maximum`
    fileprivate func normalise(_ value: Double, withMaximum maximum: Double) -> Double {
        var value = value
        
        if value < 0 {
            value += maximum
        }
        
        if value > maximum {
            value -= maximum
        }
        
        return value
    }
    
}

extension Solar {
    
    /// Whether the location specified by the `latitude` and `longitude` is in daytime on `date`
    /// - Complexity: O(1)
    public var isDaytime: Bool {
        guard
            let sunrise = sunrise,
            let sunset = sunset
            else {
                return false
        }
        
        let beginningOfDay = sunrise.timeIntervalSince1970
        let endOfDay = sunset.timeIntervalSince1970
        let currentTime = self.date.timeIntervalSince1970
        
        let isSunriseOrLater = currentTime >= beginningOfDay
        let isBeforeSunset = currentTime < endOfDay
        
        return isSunriseOrLater && isBeforeSunset
    }
    
    /// Whether the location specified by the `latitude` and `longitude` is in nighttime on `date`
    /// - Complexity: O(1)
    public var isNighttime: Bool {
        return !isDaytime
    }
    
}

// MARK: - Helper extensions
private extension Double {
    var degreesToRadians: Double {
        return Double(self) * (Double.pi / 180.0)
    }
    
    var radiansToDegrees: Double {
        return (Double(self) * 180.0) / Double.pi
    }
}