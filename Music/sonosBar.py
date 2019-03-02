#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Control you Sonos system from you Mac Menu Bar
"""

# <bitbar.title>SonosBar</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jonas Marcello</bitbar.author>
# <bitbar.author.github>anergictcell</bitbar.author.github>
# <bitbar.desc>Control you Sonos system from you Mac Menu Bar.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/anergictcell/SonosBar/master/resources/SonosBar.png</bitbar.image>
# <bitbar.dependencies>python,SoCo</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/anergictcell/SonosBar/</bitbar.abouturl>

import argparse
import socket
import os
import sys
import warnings

try:
    import soco
    from soco.music_services import MusicService
    from soco.data_structures import DidlItem, to_didl_string
except ImportError:
    print("Error")
    print("---")
    print("You need to install >>soco<< | href=https://github.com/SoCo/SoCo")
    sys.exit(0)

def parse_ip(ip_string):
    """Parsing the user supplied IP address to use on the local subnet"""
    host_ip = socket.gethostbyname(socket.gethostname())
    subnets = host_ip.split(".")
    sonos_subnets = ip_string.split(".")
    new_ip = subnets[0:(4-len(sonos_subnets))] + sonos_subnets
    return ".".join(new_ip)


def parse_cli_arguments():
    """Main function that parses command line arguments"""
    parser = argparse.ArgumentParser(description='Control your Sonos')

    player_args = parser.add_mutually_exclusive_group()
    player_args.add_argument(
        "-p", "--player",
        metavar="SPEAKER_NAME",
        type=str,
        # default="Living Room",
        help="The name of the player/zone")

    player_args.add_argument(
        "-i", "--ip",
        metavar="IP_ADDRESS",
        type=str,
        help="The IP address of the player/zone")

    control_args = parser.add_mutually_exclusive_group()
    control_args.add_argument(
        "-l", "--playlist",
        metavar="PLAYLIST_NAME",
        type=str,
        help="The name of the playlist to play")

    control_args.add_argument(
        "-r", "--radio",
        metavar="RADIO_STATION",
        type=str,
        help="The name of the radio station to play")

    control_args.add_argument(
        "-v", "--vol",
        metavar="VOLUME",
        type=int,
        choices=range(0, 101),
        help="0-100")

    control_args.add_argument(
        "-j", "--join",
        metavar="SPEAKER_NAME",
        type=str,
        help="Name of the speaker to join")

    control_args.add_argument(
        "-k", "--ipjoin",
        metavar="SPEAKER_IP",
        type=str,
        help="IP of the speaker to join")

    control_args.add_argument(
        "-u", "--unjoin",
        action='store_const',
        const=True,
        help="Unjoin the player from all groups")

    control_args.add_argument(
        'action',
        metavar='action',
        nargs="?",
        choices=["play", "pause", "next", "previous", "shuffle", "normal"],
        help="""Action to take if non is set via flags.
          Can be either: play, pause, next, previous, shuffle, normal""")

    parser.add_argument(
        "-g", "--group",
        action='store_const',
        const=True,
        help="Apply the action to the whole group")

    output = parser.add_mutually_exclusive_group()
    output.add_argument(
        "-o", "--verbose",
        action='store_const',
        const=True,
        help="Display feedback about current actions")

    output.add_argument(
        "-b", "--bitbar",
        action='store_const',
        const=True,
        help="Display bitbar controls")

    args = parser.parse_args()

    if args.ip:
        args.ip = parse_ip(args.ip)

    if args.ipjoin:
        args.ipjoin = parse_ip(args.ipjoin)

    return args

def output_for_bitbar(zones):
    """Prints the topology display"""
    print("🔊Sonos")
    print("---")
    for zone in zones:
        print_zone(zone)

def print_zone(zone):
    """Prints basic info about the zone and calls functions to
    print more detailed info"""
    print("---")
    print("Zone:")
    print("{0}: {1}".format(zone["kind"], zone["master"].player_name))
    if zone["kind"] == "P":
        print_single_player(zone["master"])
    else:
        print_group(zone["master"])

def print_single_player(player):
    """Controls printing of control elements for a single-player zone"""
    print_music_controls(player, "--")
    print_player_controls(player, "--")
    print_top_level_controls(player, "")

def print_group(master):
    """Controls printing of control elements for a multi-player zone"""
    print_music_controls(master, "--")
    print_top_level_controls(master, "")
    for player in master.group.members:
        print("➤ {0}".format(player.player_name))
        print_player_controls(player, "--")
        print("--Volume")
        print_volume_controls(player, "--")

def create_command(player, *params):
    """Creates the Bitbar specific command"""
    string = "bash={0} param1=-i param2={1}"
    i = 3
    for param in params:
        string += " param{0}={1}".format(i, param)
        i += 1
    string += " terminal=false refresh=true"
    return string.format(PATH_TO_SCRIPT, player.ip_address)

def print_player_controls(player, indent):
    """Prints Player controls for Bitbar"""

    print("{0}Join".format(indent))
    for single_player in player.all_zones:
        if single_player != player:
            print("{0}--{1} | ".format(indent, single_player.player_name) +
                  create_command(player, "--ipjoin", single_player.ip_address)
                 )
    print("{0}Unjoin | ".format(indent) +
          create_command(player, "--unjoin")
         )

def print_music_controls(player, indent):
    """Prints Music controls for Bitbar"""
    print("{0}Playlists".format(indent))
    for playlist in player.get_sonos_playlists():
        print("{0}--{1} | ".format(indent, playlist.title) +
              create_command(player, "-gl", '"' + playlist.title + '"')
             )

    print("{0}Radios".format(indent))
    for station in player.get_favorite_radio_stations()["favorites"]:
        print("{0}--{1} | ".format(indent, station["title"]) +
              create_command(player, "-gr", '"' + station["uri"] + '"')
            )

def print_top_level_controls(player, indent):
    """Prints the controls that are displayed on the base level for each
    player / group"""
    playing = player.get_current_transport_info()["current_transport_state"]
    if playing == "PLAYING":
        print("{0}├ Pause | ".format(indent) +
              create_command(player, "pause", "-g"))
        print("{0}├ Next | ".format(indent) +
              create_command(player, "next", "-g"))
    else:
        print("{0}├ Play | ".format(indent) +
              create_command(player, "play", "-g"))

    print("{0}└ Volume | ".format(indent))
    print_volume_controls(player, indent)

def print_volume_controls(player, indent):
    """Prints controls to adjust the volume"""
    for vol in range(0, 11):
        if (vol-1) * 10 < player.volume and vol*10 >= player.volume:
            # print checkmark
            print(("{0}--{1}{2}").format(indent, u'\u2713'.encode("utf-8"), vol))
        else:
            print("{0}--{1} | ".format(indent, vol) +
                  create_command(player, "--vol", vol*10)
                 )

# soco prints some usage warnings about functions where the output
# will change in the future
# Those warnings don't work well with the output for Bitbar
warnings.filterwarnings("ignore")

PATH_TO_SCRIPT = os.path.realpath(__file__)
ARGUMENTS = parse_cli_arguments()
GROUP = ARGUMENTS.group

def get_player_by_name(name):
    """Returns a SoCo object for the given name (if it exists)"""
    for device in soco.discover():
        if device.player_name == name:
            return device

def define_player(ip_address, name):
    """Returning a SoCo object of the chosen player"""
    player = None
    if ip_address:
        player = soco.SoCo(ip_address)
    if name:
        player = get_player_by_name(name)

    if player and GROUP:
        # Change player to be the coordinator of the group
        player = player.group.coordinator

    return player

def find_random_player():
    """Searches the network for Sonos zones and picks one randomly"""
    zones = soco.discover()

    if zones:
        # picking a random player
        player = next(iter(zones))
        return player

    return None

def parse_zone_groups(player):
    """Creates a list of all Zones with attrbute
    whether they are a group or a single player"""
    all_zones = []
    for group in player.all_groups:
        if len(group.members) > 1:
            all_zones.append({"kind":"G", "master":group.coordinator})
        else:
            all_zones.append({"kind":"P", "master":group.coordinator})
    return all_zones



def verbose_output(string):
    """Printing the passed commands to stdout"""
    if ARGUMENTS.verbose:
        print("{0}: {1}".format(
            ("Group " if GROUP else "Player "), string))

def group_coordinate(function):
    """Wrapper function to ensure unjoining for single players"""
    def inner_function(*arguments):
        """Inner function"""
        if GROUP:
            function(*arguments)
        else:
            # First argument always has to be the player SoCo object
            arguments[0].unjoin()
            function(*arguments)
    return inner_function

def get_songs_from_playlist(player, playlist_name):
    """Returns a list of songs from the given playlist"""
    lists = player.get_sonos_playlists()
    for playlist in lists:
        if playlist.title == playlist_name:
            return player.music_library.browse(playlist)

@group_coordinate
def play_playlist(player, playlist_name):
    """Replaces the queue with the selected playlist"""
    verbose_output("Play playlist {0}".format(playlist_name))
    songs = get_songs_from_playlist(player, playlist_name)
    player.clear_queue()
    for song in songs:
        player.add_to_queue(song)
    player.play_from_queue(0)

@group_coordinate
def play_radio_station(player, uri):
    """Plays the selected radio station. The URI must be in the
    format as it is currently returned from soco:
        x-sonosapi-stream:s25111?sid=254&flags=32
    """
    verbose_output("Switching to radio station {0}".format(uri))
    service = MusicService('TuneIn')
    didl = DidlItem(
        title="DUMMY", parent_id="DUMMY", item_id="DUMMY", desc=service.desc)
    meta = to_didl_string(didl)
    player.avTransport.SetAVTransportURI(
        [('InstanceID', 0), ('CurrentURI', uri), ('CurrentURIMetaData', meta)])
    player.play()

@group_coordinate
def play(player):
    """Play the selected song"""
    verbose_output("Play")
    player.play()

@group_coordinate
def pause(player):
    """Pause the current playback"""
    verbose_output("Pause")
    player.pause()

@group_coordinate
def next_track(player):
    """Play the next track"""
    verbose_output("Next track")
    player.next()

@group_coordinate
def previous_track(player):
    """Play the previous track"""
    verbose_output("Previous track")
    player.previous()

@group_coordinate
def turn_on_shuffle(player):
    """Turn on shuffle"""
    verbose_output("Shuffle ON")
    player.play_mode = "SHUFFLE_NOREPEAT"

@group_coordinate
def turn_off_shuffle(player):
    """Turn off shuffle"""
    verbose_output("Shuffle OFF")
    player.play_mode = "NORMAL"

def set_volume(player, volume):
    """Sets the volume"""
    verbose_output("Setting the volume to {0}".format(volume))
    player.volume = volume

def join(source, target):
    """Joining another group"""
    if target is None:
        return invalid_command("Target to join is not known")
    if GROUP:
        for single_player in source.group.members:
            single_player.join(target.group.coordinator)
    else:
        source.join(target.group.coordinator)

def invalid_command(err):
    """Handles errors and prints error messages"""
    print("ERROR: {0}".format(err))
    return

def main(args):
    """Main function"""
    player = define_player(args.ip, args.player)

    if player is None or args.bitbar:
        player = player or find_random_player()
        print_bitbar_controls(player)
        return

    if GROUP:
        # Change player to the coordinator of the group
        player = player.group.coordinator

    if args.playlist:
        return play_playlist(player, args.playlist)

    if args.radio:
        return play_radio_station(player, args.radio)

    if args.vol is not None:
        return set_volume(player, args.vol)

    if args.join:
        verbose_output("Joining {0}".format(args.join))
        to_join = define_player(None, args.join)
        return join(player, to_join)

    if args.ipjoin:
        verbose_output("Joining {0}".format(args.ipjoin))
        to_join = define_player(args.ipjoin, None)
        return join(player, to_join)

    if args.unjoin:
        verbose_output("Unjoin")
        player.unjoin()
        return

    if args.action is None:
        return

    if args.action.lower() == "play":
        play(player)
        return

    if args.action.lower() == "pause":
        pause(player)
        return

    if args.action.lower() == "next":
        next_track(player)
        return

    if args.action.lower() == "previous":
        previous_track(player)
        return

    if args.action.lower() == "shuffle":
        turn_on_shuffle(player)
        return

    if args.action.lower() == "normal":
        turn_off_shuffle(player)
        return

def print_bitbar_controls(player):
    """Prints the lines used for Bitbar to stdout"""
    if player is None:
        print("🔇 Sonos")
        print("---")
        print("No Sonos Zone present")
    else:
        output_for_bitbar(parse_zone_groups(player))

if __name__ == "__main__":
    main(ARGUMENTS)
