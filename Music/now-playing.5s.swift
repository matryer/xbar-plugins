#!/usr/bin/env xcrun swift

/*
 * <xbar.title>Now Playing</xbar.title>
 * <xbar.version>v1.0</xbar.version>
 * <xbar.author>Patrick Coffey</xbar.author>
 * <xbar.author.github>patricktcb</xbar.author.github>
 * <xbar.desc>Get's information on the currently playing music from the same APIs control centre uses. Works on Music.app, Manet.app, Plexamp, Jelly Jams, JellyBox or any other app that uses MediaRemote.</xbar.desc>
 * <xbar.image>http://images.coffey.app/now-playing.png</xbar.image>
 * <xbar.dependencies>Xcode,swift</xbar.dependencies>
 * Due to changes in macOS 15.4 this API is only available via scripts
 * or via Apple signed binaries
 */

import Foundation

struct NowPlaying {
    let title: String
    let artist: String
    let album: String
    let mediaType: String?
    let app: String?
    let isPlaying: Bool

    // Media types MediaRemote reports for things that aren't music. Try to ignore
    // not music rather than find stuff that
    static let nonMusicTypes: Set = [
        "kMRMediaRemoteNowPlayingInfoTypeVideo",
        "MRMediaRemoteMediaTypePodcast",
        "MRMediaRemoteMediaTypeAudioBook",
        "MRMediaRemoteMediaTypeITunesU",
    ]

    var isMusic: Bool {
        guard let mediaType else { return true }
        return !Self.nonMusicTypes.contains(mediaType)
    }
}

typealias GetInfo = @convention(c) (DispatchQueue, @escaping @convention(block) (CFDictionary?) -> Void) -> Void
typealias GetIsPlaying = @convention(c) (DispatchQueue, @escaping @convention(block) (Bool) -> Void) -> Void
typealias GetClient = @convention(c) (DispatchQueue, @escaping @convention(block) (AnyObject?) -> Void) -> Void

struct MediaRemote {
    let getInfo: GetInfo
    let getIsPlaying: GetIsPlaying
    let getClient: GetClient

    init?() {
        guard let handle = dlopen("/System/Library/PrivateFrameworks/MediaRemote.framework/MediaRemote", RTLD_LAZY),
              let getInfo = dlsym(handle, "MRMediaRemoteGetNowPlayingInfo"),
              let getIsPlaying = dlsym(handle, "MRMediaRemoteGetNowPlayingApplicationIsPlaying"),
              let getClient = dlsym(handle, "MRMediaRemoteGetNowPlayingClient")
        else { return nil }
        self.getInfo = unsafeBitCast(getInfo, to: GetInfo.self)
        self.getIsPlaying = unsafeBitCast(getIsPlaying, to: GetIsPlaying.self)
        self.getClient = unsafeBitCast(getClient, to: GetClient.self)
    }

    func nowPlaying(timeout: TimeInterval = 2) -> NowPlaying? {
        let queue = DispatchQueue(label: "now-playing")
        let group = DispatchGroup()
        var info: [String: Any] = [:]
        var isPlaying = false
        var app: String?

        group.enter()
        getInfo(queue) { dictionary in
            info = dictionary as? [String: Any] ?? [:]
            group.leave()
        }
        group.enter()
        getIsPlaying(queue) { playing in
            isPlaying = playing
            group.leave()
        }
        group.enter()
        getClient(queue) { client in
            if let client = client as? NSObject {
                app = property(client, "displayName") ?? property(client, "bundleIdentifier")
            }
            group.leave()
        }

        guard group.wait(timeout: .now() + timeout) == .success,
              let title = info["kMRMediaRemoteNowPlayingInfoTitle"] as? String, !title.isEmpty
        else { return nil }

        // Artist and Album have to be optional as the script's forgiving attitude on what
        // counts as music can result in items without these values being returned.
        return NowPlaying(
            title: title,
            artist: info["kMRMediaRemoteNowPlayingInfoArtist"] as? String ?? "",
            album: info["kMRMediaRemoteNowPlayingInfoAlbum"] as? String ?? "",
            mediaType: info["kMRMediaRemoteNowPlayingInfoMediaType"] as? String,
            app: app,
            isPlaying: isPlaying
        )
    }
}

// The client is a private MediaRemote class, so check a property exists before asking for it.
func property(_ object: NSObject, _ key: String) -> String? {
    guard object.responds(to: NSSelectorFromString(key)) else { return nil }
    return object.value(forKey: key) as? String
}

// SwiftBar starts parsing parameters at the first "|" on a line, so keep it out of the metadata.
func clean(_ text: String) -> String {
    text.components(separatedBy: .newlines)
        .joined(separator: " ")
        .replacingOccurrences(of: "|", with: "│")
}

guard let mediaRemote = MediaRemote() else {
    FileHandle.standardError.write(Data("MediaRemote.framework isn't available on this version of macOS\n".utf8))
    exit(1)
}

guard let nowPlaying = mediaRemote.nowPlaying() else {
    print(":music.note: | symbolize=true")
    print("---")
    print("Nothing playing")
    exit(0)
}

let title = clean(nowPlaying.title)
let artist = clean(nowPlaying.artist)
let album = clean(nowPlaying.album)

var headline = artist.isEmpty ? title : "\(title) - \(artist)"
if headline.count > 50 {
    headline = String(headline.prefix(47)) + "..."
}

if nowPlaying.isPlaying {
    print("\(nowPlaying.isPlaying ? ":music.note:" : ":pause.fill:") \(headline) | symbolize=true")
    print("---")
    print("Track: \(title)")
    if !artist.isEmpty { print("Artist: \(artist)") }
    if !album.isEmpty { print("Album: \(album)") }
    if let app = nowPlaying.app { print("App: \(clean(app))") }    
} else {
    print(":music.note: | symbolize=true")
    print("---")
    print("Nothing playing")
}