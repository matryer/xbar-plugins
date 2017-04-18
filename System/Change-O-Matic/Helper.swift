import Foundation 

class Regex : StringLiteralConvertible {
    let expression : NSRegularExpression

    required convenience init(unicodeScalarLiteral value: String) {
        self.init(pattern: "\(value)")
    }

    required convenience init(extendedGraphemeClusterLiteral value: String) {
        self.init(pattern: "\(value)")
    }

    required convenience init(stringLiteral value: StringLiteralType) {
        self.init(pattern: "\(value)")
    }

    init(pattern : String) {
        expression = try! NSRegularExpression(pattern: pattern, options: [])
    }

    func matchesString(string : String) -> Bool {
        return self.expression.firstMatchInString(string, options: [], range: NSMakeRange(0, string.utf16.count)) != nil
    }
}

infix operator =~ {}
func =~ (input: String, pattern: Regex) -> Bool {
  return pattern.matchesString(input)
}

// Shamelessly stolen from Elixir, because it makes for really nice chaining
// Take some value on the left, and apply it to the first argument of the function on the right 
// This is a poor man's version of Elixir's operator though, since it doesn't allow rhs functions with more than one parameter
infix operator |> {associativity left precedence 200}
func |> <T, S> (lhs : T, rhs : (T) -> S ) -> S {
  return rhs(lhs)
}

typealias URL = NSURLComponents

extension URL {

    // Taken from http://stackoverflow.com/a/36490559/1805855
    func appendQueryItem(name name: String, value: String) {
        if #available(OSX 10.10, *) {
            var queryItems: [NSURLQueryItem] = self.queryItems ?? [NSURLQueryItem]()
            queryItems.append(NSURLQueryItem(name: name, value: value))
            self.queryItems = queryItems
        }
    }

    func hostMatches(string : String) -> Bool {
        return self.host! =~ Regex(pattern: string)
    }
}

extension NSURLSession {
  func synchronousTask<T>(url : NSURL, 
                          authHeader : String?, 
                          taskCreator : (NSURLRequest, (T?, NSURLResponse?, NSError?) -> Void) -> NSURLSessionTask) -> T {
    var result : T?

    let request = NSMutableURLRequest(URL: url)
    let wait = dispatch_semaphore_create(0)

    if let authHeader = authHeader {
      request.addValue(authHeader, forHTTPHeaderField: "Authorization")
    }

    taskCreator(request) { (data, _, _) in
      result = data
      dispatch_semaphore_signal(wait)
    }.resume()

    dispatch_semaphore_wait(wait, DISPATCH_TIME_FOREVER)

    return result!
  }

  func synchronouslyFetchURL(url : NSURL, authHeader : String?) -> NSData {
    return self.synchronousTask(url, authHeader: authHeader, taskCreator: self.dataTaskWithRequest(_:completionHandler:))
  }

  func synchronouslyDownloadURL(url : NSURL, toFile : NSURL, authHeader : String?) -> NSURL {
    let downloadTaskWrapper : (NSURLRequest, (NSURL?, NSURLResponse?, NSError?) -> Void) -> NSURLSessionTask = { request, completionBlock in
      self.downloadTaskWithRequest(request) { (downloadedFileURL, response, error) in
        let fileManager = NSFileManager.defaultManager()
        do {
          try fileManager.moveItemAtURL(downloadedFileURL!, toURL: toFile)
          completionBlock(toFile, response, error)
        } catch {
          completionBlock(nil, nil, nil)
        }
      }
    }
    return self.synchronousTask(url, authHeader: authHeader, taskCreator: downloadTaskWrapper)
  }
}
