import Foundation

enum RedditSortOptions : String {
    case Relevance = "relevance"
    case Hot = "hot"
    case Top = "top"
    case New = "new"
    case Comments = "comments"
}

enum RedditTimeOptions : String {
    case Hour = "hour"
    case Day = "day"
    case Week = "week"
    case Month = "month"
    case Year = "year"
    case All = "all"
}

class RedditFinder {
    let subreddits : [String]
    let baseURL = URL(string: "http://www.reddit.com/search.json")!
    let sort : RedditSortOptions
    let time : RedditTimeOptions

    convenience init() {
        self.init(subreddits: ["wallpaper", "wallpapers"], sort: .Hot, time: .Day)
    }

    init(subreddits : [String], sort : RedditSortOptions, time : RedditTimeOptions) {
        self.subreddits = subreddits
        self.sort = sort
        self.time = time
    }

    func subredditSearchQuery() -> String {
        return "(\(subreddits.map({"subreddit:\($0)"}).joinWithSeparator(" OR "))) AND self:no"
    }

    func subredditSearchURL(lastModhash : String? = nil) -> URL {
        let copy =  self.baseURL.copy() as! URL
        copy.appendQueryItem(name: "q", value: self.subredditSearchQuery())
        copy.appendQueryItem(name: "limit", value: "25")
        copy.appendQueryItem(name: "sort", value: self.sort.rawValue)
        copy.appendQueryItem(name: "t", value: self.time.rawValue)
        if let lastModhash = lastModhash {
            copy.appendQueryItem(name: "after", value: lastModhash)
        }
        return copy
    }

    /// Finds and returns all image posts in the specified subreddits with the modhash from the last object for paging
    func findPosts( postNormalizationHandler : (URL) -> [URL] ) -> AnyGenerator<URL> {
        var lastModhash : String? = nil
        var postStack = [ (url : URL, modhash : String) ]()

        return AnyGenerator {
            if postStack.isEmpty {
                guard let results = try? NSJSONSerialization.JSONObjectWithData(self.downloadSearchData(lastModhash), options: []) else {
                    return nil
                }

                let posts = self.postsArrayFromJSONData(results).flatMap { (url, modhash) -> [(url: URL, modhash: String)] in
                    let normalizedPosts = postNormalizationHandler(url)
                    let modhashes = Repeat(count: normalizedPosts.count, repeatedValue: modhash)
                    // Posts that were expanded/normalized get the same modhash
                    return Array(zip(normalizedPosts, modhashes))
                } 
                if !posts.isEmpty {
                    postStack.appendContentsOf(posts)
                } else {
                    return nil
                }
            }

            let (url, modhash) = postStack.removeFirst()
            lastModhash = modhash
            return url
        }
    }

    /// Searches in the subreddits for image posts and downloads the resulting data synchronously
    func downloadSearchData(lastModhash : String?) -> NSData {
      let session = NSURLSession.sharedSession()
      return session.synchronouslyFetchURL(self.subredditSearchURL(lastModhash).URL!, authHeader: nil)
    }

    /// Parses the JSON objects that are received from reddit and returns just the URLs and modhashes for each post
    func postsArrayFromJSONData(jsonDictionary : AnyObject) -> [ (url: URL, modhash: String) ] {
      var posts = [ (url: URL, modhash: String) ]()

      if let data = jsonDictionary["data"] as? [String : AnyObject], let children = data["children"] as? [[String : AnyObject]] {
        for child in children {
          if let dataDict = child["data"] as? [String : AnyObject], let url = dataDict["url"] as? String, let modhash = dataDict["name"] as? String {
            posts.append( (url: URL(string: url)!, modhash: modhash) )
          }
        }
      } 

      return posts
    }
}

