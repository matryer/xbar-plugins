import xml.etree.ElementTree as ET
import os
import urllib.request
import ssl

# <bitbar.title>Plex Status - Improved</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Patrick Coffey</bitbar.author>
# <bitbar.author.github>patricktcb</bitbar.author.github>
# <bitbar.desc>See what's currently playing on your Plex Server</bitbar.desc>
# <bitbar.image>https://res.cloudinary.com/cyberge/image/upload/v1550627901/icons/plex_878759_eey690.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
# plextoken will need to be set manually. You can follow this guide from Plex on how to get one: https://support.plex.tv/articles/204059436-finding-an-authentication-token-x-plex-token/
# plexhost should be whatever IP/domain name your local machine uses to connect.
# myip is used in two ways. The first is to show where media is being played if it's not local, the second is when there are multiple streams being played to prefer the local one over another.
# myuserid is the id of your preferred watcher. This is typically 1, but depending on how your server admin set things up it could be different

def plexMusicInfo(el):
    trackInfo = "[" + el.attrib['title'] + " by " + el.attrib['grandparentTitle'] + "]\n---\nAlbum: " + el.attrib['parentTitle']
    if el.find("Player").attrib["state"] == "paused":
        trackInfo = "  " + trackInfo
    return trackInfo

def plexMovieInfo(el):
    trackInfo = "[🎥  " + el.attrib['title']
    return trackInfo

def plexTVInfo(el):
    trackInfo = "[📺  " + el.attrib['grandparentTitle'] + " - " + el.attrib['title'] + "]\n---\n" + el.attrib['parentTitle'] + " Episode " + el.attrib['parentIndex']
    return trackInfo

def plexMediaInfo(mc, myip, myuserid):
    trackInfo = "|\n---\nNothing Playing"
    if mc.find("User").attrib["id"] == myuserid:
        if str(mc.tag) == "Track":
            if mc.find("Player").attrib["state"] == "playing":
                trackInfo = plexMusicInfo(mc)
        elif str(mc.tag) == "Video":
            if mc.find("Player").attrib["state"] == "playing":
                if mc.attrib['type'] == "movie":
                    trackInfo = plexMovieInfo(mc)
                elif mc.attrib['type'] == "episode":
                    trackInfo = plexTVInfo(mc)
        if mc.find("Player").attrib['address'] != myip:
            trackInfo = trackInfo + "\n" + mc.find("Player").attrib['product'] + " on " + mc.find("Player").attrib['platform']
    return trackInfo

def getPlexStatus(plextoken, plexhost):
    ctx = ssl.create_default_context()
    ctx.check_hostname = False
    ctx.verify_mode = ssl.CERT_NONE
    # Plex uses self signed certs, so they can't be verified.
    plexAddress = 'https://' + plexhost + ':32400/status/sessions?X-Plex-Token=' + plextoken 
    plexXML = urllib.request.urlopen(plexAddress, context=ctx).read()
    return plexXML

def parseXML(rawXML, myip, myuserid):
    tree = ET.ElementTree(ET.fromstring(rawXML))
    root = tree.getroot()
    trackInfo = "|\n---\nNothing Playing"
    if root.attrib['size'] == "1":
        trackInfo = plexMediaInfo(root[0])
    else:
        for mc in root:
            if mc.find("Player").attrib['address'] == myip:
                trackInfo = plexMediaInfo(mc, myuserid)
    return trackInfo

#This is the function to be used with the script is called by itself by BitBar. But the file is written so that if you want, you can just import these functions into another python script if you want to.
if __name__ == '__main__':
    plextoken = "YOUR TOKEN"
    plexhost = "YOUR PLEX HOST"
    myip = "YOUR IP" #This is your IP, from the Plex Host's perspective. So your local IP if the server is on your network, and public IP if it's not
    myuserid = "1" #This is usually 1, but might not be.
    plexXML = getPlexStatus(plextoken, plexhost)
    print(parseXML(plexXML, myip, myuserid))
