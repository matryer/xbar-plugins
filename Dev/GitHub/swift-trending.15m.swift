#!/usr/bin/swift

// # <bitbar.title>Trending Swift on GitHub</bitbar.title>
// # <bitbar.version>v1.0</bitbar.version>
// # <bitbar.author>Reda Lemeden</bitbar.author>
// # <bitbar.author.github>kaishin</bitbar.author.github>
// # <bitbar.desc>List Swift repositories tending on GitHub in a given period.</bitbar.desc>
// # <bitbar.image>https://github.com/kaishin/swift-trending-bitbar/raw/master/screenshot.png</bitbar.image>
// # <bitbar.dependencies>swift</bitbar.dependencies>
// # <bitbar.abouturl>https://github.com/kaishin/swift-trending-bitbar</bitbar.abouturl>

import Foundation

// PREFERENCES (Feel free to change these to your liking)

var displayCount = 15 // Min: 10, Max: 25
var maxSubtitleLineLength = 70
var trendingPeriod = "daily" // Possible values: "daily", "weekly", "monthly"

// Try not to edit the code below.

let icon = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABGdBTUEAALGPC/xhBQAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAuBJREFUWAm1l1uITlEUx8c9GmmIKR4UXsjtgZGaeHBLlEYpD0p5EZLMA0nJg0TzRKI8KJ6IlHiYKKE0D5PEvEgyLil3ocn98vvrW1/Lts85+3zzWfWbvfbaa62zzv725UxDQ32liXRLy6QcWMY5wfcdPvNhZ4Lvf3MZSeZX0JHyhEEFTpMY11uVka84N8KuSnulTPCAwHka/ZWBLaU7HicV8gu2pQSYTyuKptDLbjpt3pCon8FPBfyA5IU5GOfj0AwmQ1C6YbUZEttV+KkA8RzGQZJMxasHpjjv2eh9sMbZitQRONjPoCLOFwX48S10XsAcZ9yD/h3WOluRegcHmwW1C4sC/PhFOh9hWcWon6cLVMT6iq2oOYeDL0A/ZbKMwfMpfAN74ET0t/ATNEtFchIHX4D0lqIgP76Ajt5YgftB23QFqADZ9kKeHGMwLOBoXkBsTL+9JbmAPgr2Odsp9KEQkyMYLdba19jC8yYWW7XpvugES3AffRZcdrYb6LFtdtb5WLza6VBKRuPdC5ZEW7IdHjub1ss88HKTjsX4dpN3StVn4qhd4ROF2+wL4z75m8DfYrU2ahLdC7YoLVmsPY3fXIiNyXYVapYNRGYl9vbPOX73an56JXBHTnJfRJb+rL8FKP5gP4rQ4q2KTrxapYPArLfMs9/yD9TlshlKHQ4uwQH0vIfFxk64+D/qJf52Q2s4kNgvuya2h3nHYngEqlbfcIshdUZ0S26ETxB725hNV30T/CUz6H0AC3iIrulVMToJvUygo2taC/EJWEyZ9jBx/8hyLLp+Y4m0r99DykEUiw9tmrHm8LP8AcYeaANNrRf1h4Eup3qI8r3MSqRjtBfCquvd78wqQPZGOAT6rK7Hg/UdoBnWhWX5MmcAn6q0oF0HCyrbXiPWb2990NgpqmKSZRGemjL/BnnF3MVX/xtkyToG+lL3uk+in2YJ6K0mg47y4aBVrfv/NuhrqQuKZOtv52B4+dBxxIcAAAAASUVORK5CYII="
let templateImage = "|templateImage=\(icon)"

// Mark: - Regex

infix operator =~

func =~ (value: String, pattern: String) -> RegexResult {
  var error: NSError?
  let string = value as NSString
  let options = NSRegularExpression.Options(rawValue: 0)
  let regex: NSRegularExpression?

  do {
    regex = try NSRegularExpression(pattern: pattern, options: options)
  } catch let error1 as NSError {
    error = error1
    regex = nil
  }

  if error != nil { return RegexResult(results: []) }

  let all = NSRange(location: 0, length: string.length)
  let matchingOptions = NSRegularExpression.MatchingOptions(rawValue: 0)
  var matches: [String] = []

  regex?.enumerateMatches(in: value, options: matchingOptions, range: all) { result, _, _ in
    guard let result = result else { return }
    let subString = string.substring(with: result.range)
    matches.append(subString)
  }

  return RegexResult(results: matches)
}

struct RegexResult {
  let isMatching: Bool
  let matches: [String]

  init(results: [String]) {
    matches = results
    isMatching = matches.count > 0
  }
}

// Mark: - Foundation Extensions

extension String {
  func matches(pattern: String) -> [String] {
    let regexResult = (self =~ pattern)

    if regexResult.isMatching {
      return regexResult.matches
    } else {
      return []
    }
  }

  func condenseWhitespace() -> String {
    let components = self.components(separatedBy: NSCharacterSet.whitespacesAndNewlines)
    return components.filter { !$0.isEmpty }.joined(separator: " ")
  }

  func trunc(length: Int, trailing: String = "…") -> String {
    return (self.count > length) ? self.prefix(length) + trailing : self
  }
}

extension Array {
  func chunk(_ chunkSize: Int) -> [[Element]] {
    return stride(from: 0, to: self.count, by: chunkSize).map({ (startIndex) -> [Element] in
      let endIndex = (startIndex.advanced(by: chunkSize) > self.count) ? self.count-startIndex : chunkSize
      return Array(self[startIndex..<startIndex.advanced(by: endIndex)])
    })
  }
}

// Mark: - Repository

struct Repository {
  let authorName: String
  let projectName: String
  let description: String
  let starCount: Int
  let newStarCount: Int

  init?(string: String) {
    let properties = string.split(separator: "|")
    if properties.count != 4 { return nil }

    self.authorName = String(describing: properties[0].split(separator: "/").first ?? "").trimmingCharacters(in: .whitespacesAndNewlines)
    self.projectName = String(describing: properties[0].split(separator: "/").last ?? "").trimmingCharacters(in: .whitespacesAndNewlines)
    self.description = String(describing: properties[1])
      .trimmingCharacters(in: .whitespacesAndNewlines)
      .replacingOccurrences(of: "&amp;", with: "&")

    let stars = String(describing: properties[2]
      .split(separator: " ").first?.replacingOccurrences(of: ",", with: "") ?? "0")
    self.starCount = Int(stars) ?? 0

    let newStarsString = String(describing: properties[3]
      .split(separator: " ").first?.replacingOccurrences(of: ",", with: "") ?? "0")

    self.newStarCount = Int(newStarsString) ?? 0
  }

  var gitHubURL: String {
    return "https://github.com/\(authorName)/\(projectName)/"
  }

  var firstLine: String {
    return "\(projectName) (by \(authorName))" + "| href=\(gitHubURL)"
  }

  var secondLine: String {
    return "★\(starCount) (+\(newStarCount)) — \(description)" + "| size=12 length=\(maxSubtitleLineLength)"
  }

  func multiLineDescription(wordCount count: Int) -> String {
    let fullDescriptionWords = description.split(separator: " ")
    let chunkedDescription = fullDescriptionWords.chunk(count).map { chunk in
      return chunk.joined(separator: " ")
    }

    return  chunkedDescription.joined(separator: "| size=12 \n")
  }
}

enum Period: String {
  case daily, weekly, monthly

  var title: String {
    switch self {
    case .daily:
      return "today"
    case .weekly:
      return "this week"
    default:
      return "this month"
    }
  }
}

// Mark: - Free Functions

func trendingRepositories(html: String) -> [Repository] {
  let repos = html.matches(pattern: "<ol class=\"repo-list\">(.|\n)*?</ol>")[0]
  let repoList = repos.matches(pattern: "<li class=(.|\n)*?</li>")

  return repoList.flatMap { repo in
    let sanitizedString = repo.replacingOccurrences(of: "<[^>]+>", with: "", options: .regularExpression)
      .replacingOccurrences(of: "\n    Star|Built by\n|\n          Swift", with: "|", options: .regularExpression)
      .condenseWhitespace()

    return Repository(string: sanitizedString)
  }
}

func printOutput(responseHTML html: String) {
  print(templateImage)
  print("---")

  guard let period = Period(rawValue: trendingPeriod) else {
    fatalError("Frequency specified should be one of the following options: 'daily', 'weekly', 'monthly'.")
  }

  print("Trending Swift \(period.title.capitalized)")
  print("---")

  let count = max(min(displayCount, 25), 10)

  for repo in trendingRepositories(html: html)[0..<count] {
    print(repo.firstLine)
    print(repo.secondLine)
  }
}

// Mark: - Output

let url = URL(string: "https://github.com/trending/swift?since=\(trendingPeriod)")!
let html = try? String(contentsOf: url)

printOutput(responseHTML: html!)

