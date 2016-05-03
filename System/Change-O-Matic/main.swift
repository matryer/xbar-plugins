import Foundation
import AppKit

//MARK: - Global data

let iconData = "iVBORw0KGgoAAAANSUhEUgAAABMAAAATCAYAAAByUDbMAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAQZJREFUeNrMkr0NwjAQhc1PTzYgI4QNKCkZIZmAjEAmQExASQkdZUJHR0qooKQLG8Cz9IxOBieOoOCkT7Yvp+d35yj1r9FpUZuCIdiDO5gxn3ANey3E1uAGJmAODuAMTmAHAl+hCFTivCJa4Mh9Y+jiMYtzipr8m5CcWQhizkNxJjpXWDVbkFG8BEt+K6WDC1iwHX3jtMatdJTTveqKmWwpoNcRb0qtwQbCUeKaTUhHucjp2x98OVUz7JezPhNX0buJgo5LX0dd6zwQ+4wPVHwQipp+B9Oq6/kjsVYcQSDbtCNm4SchI2DWB+udYsahS8hm0yTmKyQZ+/yQX8XPhFRd7z7xFGAAqsVZ170xiqsAAAAASUVORK5CYII="

//MARK: - Main section

// Sets the wallpaper to the specified path
func setWallpaper(path : String, forScreen screen : NSScreen) {
  let workspace = NSWorkspace.sharedWorkspace()
  let fileURL = NSURL.fileURLWithPath(path)
  do {
    try workspace.setDesktopImageURL(fileURL, forScreen: screen, options: [:])
  } catch let err as NSError {
    print("Error setting wallpaper \(fileURL.absoluteString) for screen \(screen) because of error \(err)")
  }
}

func downloadImage(url : URL, cacheDir : NSURL = NSFileManager.defaultManager().URLsForDirectory(.CachesDirectory, inDomains:[.UserDomainMask]).first!.URLByAppendingPathComponent("rwcom")) -> String {
  let imageURL = url.URL!
  let fileName = imageURL.lastPathComponent!
  let destination = cacheDir.URLByAppendingPathComponent(fileName)
  let destinationPath = destination.path!

  if NSFileManager.defaultManager().fileExistsAtPath(destinationPath) {
    return destinationPath
  }

  let finalDestination = NSURLSession.sharedSession().synchronouslyDownloadURL(imageURL, toFile: destination, authHeader: nil)
  return finalDestination.path!
}

func main() {
  print("| templateImage=\(iconData)")
  print("---")

  let reddit = RedditFinder()

  let imageHandlers : [RemoteImageHandler] = [ImgurImageHandler(), DefaultImageHandler()]

  // Generates a sequence of reddit posts 
  // The trailing block is to expand links that are found using the handlers we have here 
  // (e.g. make a list of urls from a link to a gallery)
  let postSequence = reddit.findPosts { url in
    let handler = imageHandlers.filter { handler in
      handler.canHandleURL(url)
    }.first!

    return handler.normalizeURL(url)
  }

  // Make a lazy sequence that fetches the size of each image
  let postsWithSize = postSequence.lazy.map { post in  
    (post, imageHandlers.filter({$0.canHandleURL(post)}).first!)
  }.map { (post, handler) in
    (post, handler.findSizeForURL(post))
  }

  // Group the screens by screen size, using a string of the size as key
  var screensBySize = [String : [NSScreen]]()
  NSScreen.screens()?.forEach { screen in
    let key = String(screen.frame.size)
    if screensBySize[key] == nil {
      screensBySize[key] = [NSScreen]()
    }

    screensBySize[key]!.append(screen)
  }

  // For each group of screens, find a list of suitable wallpapers and apply them
  screensBySize.flatMap { (_, screens) -> [(URL, NSScreen)] in
    let screenSize = screens.first!.frame.size
    let minWidth = Double(screenSize.width)
    let minHeight = Double(screenSize.height)

    let suitableWallpapers = postsWithSize.filter { (post, size) in 
      let (width, height) = size
      return (width >= minWidth && height >= minHeight && width/height == minWidth / minHeight)
    }.map { (post, size) in
      return post
    }
    return Array(zip(suitableWallpapers.prefix(screens.count), screens))
  }.forEach { (wallpaper, screen) in
    let path = downloadImage(wallpaper)
    setWallpaper(path, forScreen: screen)
  }

}

main()
