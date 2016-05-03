import Foundation

protocol RemoteImageHandler {
    /// Return true to indicate that the image handler can work with this domain
    func canHandleURL(url : URL) -> Bool

    /// Return the width/height of the remote image
    func findSizeForURL(url : URL) -> (width : Double, height : Double) 
    /// Change the URL so it is appropriate to download (e.g. for galleries/albums, return each separate downloadable entry)
    func normalizeURL(url : URL) -> [URL] 
}

class DefaultImageHandler: RemoteImageHandler {
    func canHandleURL(url : URL) -> Bool {
        return true
    }

    func findSizeForURL(url : URL) -> (width : Double, height : Double) {
      return (0, 0)
    }

    func normalizeURL(url : URL) -> [URL] {
        return [url]
    }
}

class ImgurImageHandler: RemoteImageHandler {
    let imageAPIURLString = "https://api.imgur.com/3/image/"
    let galleryAPIURLString = "https://api.imgur.com/3/gallery/"
    let clientID = "4e25c0b2c5b557f"

    // These are some keywords which can appear after the image ID
    let apiKeywords = [
        "images",
        "image",
        "favorite",
        "replies",
        "new"
    ]

    func canHandleURL(url : URL) -> Bool {
        return url.hostMatches("imgur.com")
    }

    private func imageIDFromURL(url : URL) -> String {
        var truncatedURL = url.URL!.URLByDeletingPathExtension! 
        var possibleID = ""
        repeat {
            possibleID = truncatedURL.lastPathComponent! 
            truncatedURL = truncatedURL.URLByDeletingLastPathComponent!
        } while(self.apiKeywords.contains { $0.lowercaseString == possibleID })

        return possibleID
    }

    func findSizeForURL(url : URL) -> (width : Double, height : Double) {
      let session = NSURLSession.sharedSession()

      let imageURL =  url 
                        |> self.imageIDFromURL
                        |> NSURL(string: imageAPIURLString)!.URLByAppendingPathComponent

      let imageData = session.synchronouslyFetchURL(imageURL, authHeader: "Client-ID \(self.clientID)")
      guard let imageModel = try? NSJSONSerialization.JSONObjectWithData(imageData, options: []) else {
          return (0, 0)
      }

      guard let data = imageModel["data"] as? [String : AnyObject], let width = data["width"] as? Double, let height = data["height"] as? Double else {
        return (0, 0)
      }

      return (width, height)
    }

    func normalizeURL(url : URL) -> [URL] {
        // If it's just an ID, we can use the direct link
        if url.path! =~ "^/[a-zA-Z0-9]*[^/]$" {
            let id = self.imageIDFromURL(url)
            return [URL(string: "http://i.imgur.com/\(id).jpg")!]
        }

        // Return the same url if the image is not a gallery
        guard (url.path! =~ "/gallery/") || (url.path! =~ "/a/") else {
            return [url]
        }

        let session = NSURLSession.sharedSession()

        // Builds a URL of the format https://api.imgur.com/3/gallery/{id}/images
        // It's undocumented but apparently the only way to get the images in a gallery
        let galleryURL = (url 
                            |> self.imageIDFromURL
                            |> NSURL(string: galleryAPIURLString)!.URLByAppendingPathComponent).URLByAppendingPathComponent("images")

        let galleryData = session.synchronouslyFetchURL(galleryURL, authHeader: "Client-ID \(self.clientID)")
        guard let galleryModel = try? NSJSONSerialization.JSONObjectWithData(galleryData, options: []) else {
            // Might not have been a gallery url after all, so leave as is
            return [url]
        }

        // The imgur API is a bit weird, a gallery can either return a single image or a list of images
        if let data = galleryModel["data"] as? [String : AnyObject], let link = data["link"] as? String {
            return [URL(string: link)!]
        } else if let data = galleryModel["data"] as? [ [String : AnyObject] ] {
            var urls = [URL]()
            for image in data {
                if let link = image["link"] as? String {
                    urls.append(URL(string: link)!)
                }
            }
            return urls
        }

        return []
    }
}

