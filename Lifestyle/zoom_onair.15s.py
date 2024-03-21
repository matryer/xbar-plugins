#!/usr/bin/env PYTHONENCODING=UTF-8 python3

#  <xbar.title>Zoom On-Air</xbar.title>
#  <xbar.version>v0.2</xbar.version>
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


from time import sleep
from os import getenv
from platform import node
import subprocess
from sys import executable


messages = []


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
            messages.append(a)
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
            messages.append(a)
            self.brightness = 0


def in_meeting():
    in_meeting = False
    processes = subprocess.Popen(['lsof', '-i', '4UDP'],
                                 stdout=subprocess.PIPE).stdout.readlines()
    for process in processes:
        if 'zoom' in str(process) and node().split('.')[0] in str(process):
            in_meeting = True
            break
    return in_meeting


def get_onair_lights(light_list):
    addresses = light_list.split(',')
    lights = []
    if addresses[0] == 'first':
        lights_found = magichue.discover_bulbs()
        try:
            lights.append(magichue.LocalLight(lights_found[0]))
        except Exception as e:
            messages.append(f'On-Air light not found: {str(e)} | alternate=true')
    else:
        for address in addresses:
            try:
                lights.append(magichue.LocalLight(address))
            except ConnectionRefusedError:
                messages.append(f'Connection refused for light at {address}')
            except Exception as e:
                messages.append(f'Unable to connect to light at {address}: {str(e)}')
    return lights


def set_light_state(light, config):
    transition_effect = getattr(magichue, 'NORMAL')
    if not light.on == config.lighton:
        light.on = config.lighton
    if config.lighton:
        light.is_white = False
        light.mode = transition_effect
        sleep(light.speed)
        light.rgb = config.color.toRGB()
        light.brightness = config.brightness


def install(package):
    try:
        subprocess.check_call([executable, '-m', 'pip', 'install',
                               '--user', package],
                              stdout=subprocess.DEVNULL,
                              stderr=subprocess.DEVNULL)
    except subprocess.CalledProcessError:
        try:
            subprocess.check_call([executable, '-m', 'pip', 'install',
                                   '--user', package, '--break-system-packages'],
                                  stdout=subprocess.DEVNULL,
                                  stderr=subprocess.DEVNULL)
        except subprocess.CalledProcessError as e:
            messages.append(f'Error installing {package}: {e}')


if __name__ == "__main__":
    config = {}
    config['onair'] = Config(getenv('ONAIR_ONAIR_COLOR'),
                             getenv('ONAIR_ONAIR_BRIGHTNESS'),
                             'TRUE')
    config['offair'] = Config(getenv('ONAIR_OFFAIR_COLOR'),
                              getenv('ONAIR_OFFAIR_BRIGHTNESS'),
                              getenv('ONAIR_OFFAIR_LIGHTON'))

    if in_meeting():
        state = 'onair'
        state_label = 'ON AIR'
    else:
        state = 'offair'
        state_label = 'OFF AIR'

    messages.append(f'🎙️ {state_label} | color=#{config[state].color.toHex()}')
    messages.append('---')

    install('python-magichue')
    import magichue

    for light in get_onair_lights(getenv('ONAIR_LIGHTS')):
        set_light_state(light, config[state])

    for message in messages:
        print(message)
