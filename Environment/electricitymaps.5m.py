#!/usr/bin/env -S PATH="${PATH}:/opt/homebrew/bin:/usr/local/bin" PYTHONIOENCODING=UTF-8 python3

# <xbar.title>Electricity Maps</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Olivier Corradi</xbar.author>
# <xbar.author.github>corradio</xbar.author.github>
# <xbar.desc>This plugin displays how clean the electricity is in your current location</xbar.desc>
# <xbar.dependencies>python, Electricity Maps API</xbar.dependencies>
# <xbar.abouturl>https://docs.electricitymaps.com/</xbar.abouturl>
# <xbar.image>https://static.electricitymaps.com/xbar-screenshot.png</xbar.image>
# <xbar.var>string(VAR_API_KEY=""): Electricity Maps API key (get one at https://api-portal.electricitymaps.com/).</xbar.var>
# <xbar.var>string(VAR_ZONE_KEY=""): Zone key (select from list at http://api.electricitymap.org/v3/zones). Leave empty to use your IP to determine your location.</xbar.var>
# <xbar.var>select(VAR_CARBON_INTENSITY_MODE="Carbon intensity"): What value to display. [Carbon intensity, Carbon Free Energy (CFE) percentage]</xbar.var>

from datetime import datetime
import os
import requests
import sys

# user settings 
# get your Electricity Maps API token at https://api-portal.electricitymaps.com/
# insert your specific country code from this list http://api.electricitymap.org/v3/zones
# have fun ^^

myapitoken = os.environ.get('VAR_API_KEY', '')
myZoneKey = os.environ.get('VAR_ZONE_KEY', '') # if none are provided, your IP will be used to determined your location
carbon_intensity_mode = os.environ.get('VAR_CARBON_INTENSITY_MODE', 'Carbon intensity') == 'Carbon intensity'

class ElectricityMaps:

    def __init__(self, authToken, zoneKey, is_carbon_intensity_mode):
        self.authToken = authToken
        self.zoneKey = zoneKey
        if is_carbon_intensity_mode:
            self.data_type = 'carbon-intensity'
        else:
            self.data_type = 'power-breakdown'

    def request(self):
        url = 'https://api.electricitymap.org/v3/' + self.data_type + '/latest'
        if self.zoneKey:
            url += '?zone=' + self.zoneKey
        headers = {'auth-token': self.authToken}
        self.resDict = requests.get(url, headers=headers).json()
        self.zoneKey = self.resDict.get('zone', '')

    def displayResponse(self):
        if 'error' in self.resDict:
            print('Electricity Maps error')
            print('---')
            print('Error: ' + self.resDict['error'] + ' | color=darkred | disabled=true')
            if 'auth-token' in self.resDict['error']:
                print('Get a free API key | href=https://portal.electricitymaps.com/?utm_source=xbar')
        else:
            stringToDisplay = self.countryFlag(self.zoneKey) + ' '
            if self.data_type == 'carbon-intensity':
                max_value = 1500
                value = self.resDict['carbonIntensity']
                stringToDisplay += str(round(value)) + ' g/kWh'
            else:
                max_value = 100
                value = self.resDict['fossilFreePercentage']
                stringToDisplay += str(round(value)) + ' % CFE'
            stringToDisplay += ' | color=' + self.color(value, max_value, asc=self.data_type == 'carbon-intensity')
            print(stringToDisplay)
            print('---')

        print('---')
        print('Open Electricity Maps App | href=https://app.electricitymaps.com/zone/' + self.zoneKey + '?utm_source=xbar')
        print('Last refreshed: ' + datetime.now().strftime('%a %d %b %Y %X') + ' | disabled=true')


    def countryFlag(self, zoneKey=None):
        # https://www.unicode.org/charts/PDF/U1F100.pdf
        # see regional indicator symbols
        if not zoneKey:
            return '🏳'
        start = 0x1F1E6
        # unicode start for letter 'A' in regional Symbols
        letterOffset1 = ord(zoneKey[0]) - ord('A')
        letterOffset2 = ord(zoneKey[1]) - ord('A')
        # calculation of number which is added
        # e.g. 'D' as regional letter = 'A' as regional letter + 3
        letter1 = start + letterOffset1
        letter2 = start + letterOffset2
        return (chr(letter1) + chr(letter2))

    def rgb_to_hex(self, rgb):
        # https://www.codespeedy.com/convert-rgb-to-hex-color-code-in-python/
        return '%02x%02x%02x' % rgb

    def hex_to_rgb(self, hex):
        h = hex.lstrip('#')
        return tuple(int(h[i:i+2], 16) for i in (0, 2, 4))

    def linearGradient(self, col1, col2, splitFactor):
        # this function returns the color which is in between those two colors and
        # is splits those two colors according to the split factor
        # e.g. col1 = (0, 0, 0); col2 = (40, 60, 80); SF = 0.4
        # linearGradient(col1, col2, SF) -> 0.4*((40, 60, 80) - (0, 0, 0))
        # similar to finding a point on straight between two points
        dif = (col1[0] - col2[0], col1[1] - col2[1], col1[2] - col2[2])
        dif = (dif[0]*splitFactor, dif[1]*splitFactor, dif[2]*splitFactor)
        result = (col1[0] - dif[0], col1[1] - dif[1], col1[2] - dif[2])
        result = (int(result[0]), int(result[1]), int(result[2]))
        return result
     
    def color(self, value, max_value, asc=True):
        # this function should determine a color given the carbon intensity
        # low intensity -> more green
        # middle intensity -> yellow
        # high intensity -> brown
        # similar to the color scale at: app.electricitymaps.com

        if not asc:
            value = max_value - value

        steps = [
            0 / 1500 * max_value,
            150 / 1500 * max_value,
            600 / 1500 * max_value,
            800 / 1500 * max_value,
            1100 / 1500 * max_value,
            1500 / 1500 * max_value,
        ]
        colors = [
            self.hex_to_rgb(col)
            for col in ['#2AA364', '#F5EB4D', '#9E4229', '#381D02', '#381D02', '#000000']
        ]
        k = 0
        while k < len(steps) - 1 and value >= steps[k + 1]:
            k += 1
        if k + 1 >= len(steps):
            return colors[k]
        else:
            # Interpolate
            splitFactor = (value - steps[k]) / (steps[k+1] - steps[k])
            assert splitFactor >= 0
            assert splitFactor <= 1
            carbonColor = self.linearGradient(colors[k], colors[k+1], splitFactor)
            carbonColor = self.rgb_to_hex(carbonColor)
            return('#' + carbonColor)


myElectricityMaps = ElectricityMaps(myapitoken, myZoneKey, carbon_intensity_mode)
myElectricityMaps.request()
myElectricityMaps.displayResponse()
