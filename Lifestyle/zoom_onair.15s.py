#!/usr/bin/env PYTHONIOENCODING=UTF-8 python3

#  <xbar.title>Zoom On-Air</xbar.title>
#  <xbar.version>v0.1</xbar.version>
#  <xbar.author>David Bayer</xbar.author>
#  <xbar.author.github>drbayer</xbar.author.github>
#  <xbar.desc>Changes color of Magic Hue smart bulbs to indicate when you're in a Zoom meeting.</xbar.desc>                         # noqa: E501
#  <xbar.image>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o5WBDqgwjqf4WReMV1OkeFLLW9DFzC7QaiZigkkdjAUBGdyAE9M4xWlo/gi8sf7Ua+ltn3QqLSWBi4yNxbejqGHagDzrcW3MepLE/UnNejfD6PztC8SxH+K4jH4+Sa83A6/U16l8MED6V4hHpeW/6xNVLcTOM1mHY11x96LP4o6n/Gucrt/EtsYrm6Qjj9+B9GRiK4inPcUdgoooqCgooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKVQWIA7mgD17wKF/sNWcMFE0zHy1DOQoydq8ZP41q2Ws2GoPqlvAHD2iSH5wQsibCcqTjnsRXnmg+NX0W3SxewS4tldmDpK0cw3c99yn9K6i18Z+C5UvJAk1jdT28yt50JZXYoQBvhyP0FIDytTnPGMkmvVPhX/wAg7xH/ANfVr/6LevLra3urue3tLWJ5bi4dY4o0GWZm7f417t4a0CHwzo6WxcSXly6z30i8oZNuBGn+yvT35NWhM47xlDi5ZsdQM/yrzKvV/F48x3Ydgv8AMV5Sep+ppy6CiJRRRUFBRRRQAUUUUAFFFFABRRRQAUUUUAFFFFABRRRQAUUUUAFFFFABRRRQAUUUUAFFFFABRRRQAUUUUAFFFFABRRRQAUUUUAFFFFABRRRQAUUUUAFFFFABRRRQAUUUUAFFFFABRRRQAUAkEEHBHIIoooA9UuPBvh26htZFhltpJLS1kLW8h2l3iViSj5HJrnr/AMC3MCSy2d9FKiKW2ToY3wOeq5H8q63RNZS/0O0u7xo4p41+zhYwdrrB+6BPJIOACfrUMuuaRNHNCZzFIVK7ZlIznjhhlf1rRWZGqNfwr4Pg8OWwurkxz6rcIPMlTJjt4mGfKhLAHn+I456dOu3fXAVUJIACliScAAdyTVS78VaEECW8xuGVFH7pW8vIGMbyK828ba1qF19it1mMdvIkrSQxEhXwQF3nqe9K9h7knifxJpzma3sz9omJw0i/6lCD2Pc/SuDPOT60UVLdxpWCiiikMKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigDd8NzXM1/ZaUZWFre3McbqOTGZGCF4/f/AAq74hRNE1aex3PcCNInEhAQkON2CORxWf4W48Q6D/1/23/oxa0fH/8AyMt5/wBcbb/0CnbS4up1Njo6HRH1lpUz9jW6jhWPjLIHAds/geK80vbu5vZ3muH3N91QBhVUdFUDtXr1kSPBI/7A8J/8gJXjLfeb6n+dOQkJRRRUlBRRRQAUUUUAFFFFABRRRQAUU+ZBHLNGM4SR0GeuFYjnFMoAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKANjwz/yH9C/6/wC1/wDRi1p+Pl/4qK7b+9HEPyXFZvhcZ8Q+Hx/1ELX/ANGCtf4ggDXJT6gD8lWq6E9Tt7M/8UWv/YGi/wDRCV4y33m+pr2O1H/FFqcn/kDw/wDolK8cb7zfU05AhKKKKgoKKKKACiiigAooooAKKK14dMtZIoZC82XjRzhkxllB4ytAH//Z</xbar.image>
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
