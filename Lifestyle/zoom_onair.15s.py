#!/usr/bin/env python3

#  <xbar.title>Zoom On-Air</xbar.title>
#  <xbar.version>v0.1</xbar.version>
#  <xbar.author>David Bayer</xbar.author>
#  <xbar.author.github>drbayer</xbar.author.github>
#  <xbar.desc>Changes color of Magic Hue smart bulbs to indicate when you're in a Zoom meeting.</xbar.desc>                         # noqa: E501
#  <xbar.image>https://drbayer.github.io/xbar-plugin-onair/onair.jpeg</xbar.image>
#  <xbar.dependencies>python3</xbar.dependencies>
#  <xbar.abouturl>http://url-to-about.com/</xbar.abouturl>
#
#  <xbar.var>string(ONAIR_LIGHTS="first"): Addresses of lights to control. Defaults to first bulb found on network.</xbar.var>      # noqa: E501
#  <xbar.var>boolean(ONAIR_OFFAIR_LIGHTON=false): Should the light be turned on when not in a meeting?</xbar.var>                   # noqa: E501
#  <xbar.var>string(ONAIR_ONAIR_COLOR="ff0000"): Hex code for bulb color when on-air. Defaults to red.</xbar.var>                   # noqa: E501
#  <xbar.var>number(ONAIR_ONAIR_BRIGHTNESS=255): Brightness level when on-air. Range is 0-255.</xbar.var>                           # noqa: E501
#  <xbar.var>string(ONAIR_OFFAIR_COLOR="00ff00"): Hex code for bulb color when off-air. Defaults to green.</xbar.var>               # noqa: E501
#  <xbar.var>number(ONAIR_OFFAIR_BRIGHTNESS=255): Brightness level when off-air. Range is 0-255.</xbar.var>                         # noqa: E501
#
# Use Magic Hue smart light bulbs to indicate when you are in a Zoom meeting.
#
# xbar-onair.py built from original work by Tim Toll.


import time
import os
import subprocess
import sys


class Color:
    def __init__(self, color):
        try:
            if type(color) == str:
                assert len(color) == 6, \
                    'Color must be a valid 6 digit hex string'
            if type(color) == tuple:
                assert len(color) == 3, \
                    'Color must be a valid 3-tuple'
            self.color = color
        except AssertionError as a:
            print(a)
            self.color = '000000'

    def toHex(self):
        if type(self.color) == tuple:
            return '%02x%02x%02x' % self.color
        else:
            return self.color

    def toRGB(self):
        if type(self.color) == tuple:
            return self.color
        else:
            r = int(self.color[0:2], 16)
            g = int(self.color[2:4], 16)
            b = int(self.color[4:6], 16)
            return (r, g, b)


class Config:
    def __init__(self, color, brightness, lighton):
        self.color = Color(color)
        self.brightness = int(brightness)
        if lighton == 'TRUE':
            self.lighton = True
        else:
            self.lighton = False
        try:
            assert 0 <= self.brightness <= 255, \
                'Brightness must be 0-255'
        except AssertionError as a:
            print(a)
            self.brightness = 0


def in_meeting():
    in_meeting = False
    processes = subprocess.Popen(['lsof', '-i', '4UDP'],
                                 stdout=subprocess.PIPE).stdout.readlines()
    for process in processes:
        if 'zoom' in str(process):
            in_meeting = True
            break
    return in_meeting


def get_onair_lights(light_list):
    addresses = light_list.split(',')
    lights = []
    if addresses[0] == 'first':
        lights_found = magichue.discover_bulbs()
        try:
            lights.append(magichue.Light(lights_found[0]))
        except Exception as e:
            msg = 'On-Air light not found: %s | alternate=true'
            print(msg % (str(e)))
    else:
        for address in addresses:
            try:
                lights.append(magichue.Light(address))
            except ConnectionRefusedError:
                msg = 'Connection refused for light at %s | alternate=true'
                print(msg % (address))
            except Exception as e:
                msg = 'Unable to connect to light at %s: %s | alternate=true'
                print(msg % (address, str(e)))
    return lights


def set_light_state(light, config):
    transition_effect = getattr(magichue, 'NORMAL')
    if not light.on == config.lighton:
        light.on = config.lighton
    if config.lighton:
        light.is_white = False
        light.mode = transition_effect
        time.sleep(light.speed)
        light.rgb = config.color.toRGB()
        light.brightness = config.brightness


def install(package):
    subprocess.check_call([sys.executable, '-m', 'pip', 'install',
                           '--user', package],
                          stdout=subprocess.DEVNULL,
                          stderr=subprocess.DEVNULL)


if __name__ == "__main__":
    install('python-magichue')
    import magichue

    messages = []

    config = {}
    config['onair'] = Config(os.getenv('ONAIR_ONAIR_COLOR'),
                             os.getenv('ONAIR_ONAIR_BRIGHTNESS'),
                             'TRUE')
    config['offair'] = Config(os.getenv('ONAIR_OFFAIR_COLOR'),
                              os.getenv('ONAIR_OFFAIR_BRIGHTNESS'),
                              os.getenv('ONAIR_OFFAIR_LIGHTON'))
    config['lights'] = get_onair_lights(os.getenv('ONAIR_LIGHTS'))

    if in_meeting():
        state = 'onair'
        state_label = 'ON AIR'
    else:
        state = 'offair'
        state_label = 'OFF AIR'

    print("🎙️ %s | color=#%s" % (state_label, config[state].color.toHex()))
    print('---')
    for light in config['lights']:
        set_light_state(light, config[state])
