#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python3
# <xbar.title>airspace.py</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Victor Nazario</xbar.author>
# <xbar.author.github>victor-nazario</xbar.author.github>
# <xbar.image>https://images.emojiterra.com/google/android-11/512px/2708.png</xbar.image>
# <xbar.desc>Displays tracking information about airplanes in the airspace above you</xbar.desc>
# <xbar.abouturl>https://github.com/victor-nazario/airspace</xbar.abouturl>
# <xbar.dependencies>python3,urllib,json</xbar.dependencies>


import math
import urllib.request
import urllib.error
import json
from socket import timeout


def perform_request(url):
    """
        Performs HTTP GET Request to the provided url and returns a map containing the loaded JSON.
        :param url: the url to be opened on the request
        :return: a dict containing the keys and value obtained from response body
    """
    try:
        req = urllib.request.urlopen(url, timeout=3)
        json_map = json.loads(req.read().decode(req.info().get_param('charset') or 'utf-8'))
        return json_map
    except urllib.error.HTTPError as e:
        print(e.__dict__)
    except urllib.error.URLError as e:
        print(e.__dict__)
    except timeout:
        print('socket timed out - URL %s', url)


def print_menu(result):
    """
        Prints the application menu according to XBar needs.
        :param result: the resulting map from a call to the airspace endpoint
        :return: none
    """
    print("✈︎")
    print("---")
    print("--")
    print("Airspace Above")
    print("--Flights")
    print_flights(result)
    print("--About|href='https://github.com/victor-nazario/airspace")


def print_flights(result):
    """
        Prints the flight information for each of the flights in the airspace.
        :param result: the resulting map from a call to the airspace endpoint
        :return: none
    """
    for vector in result["states"]:
        try:
            info_line = 'Flight: %s * Origin Country: %s * Ground Speed: %d ' \
                        'mph * Altitude: %d ft' % \
                        (vector[1].strip(), vector[2], math.ceil(vector[9] * 2.23694), math.ceil(vector[13] * 3.28084))
            track_url = f"----{info_line}|href='https://flightaware.com/live/flight/{vector[1].strip()}' font='Menlo'"
            print(track_url)
        except TypeError:
            info_line = 'Flight: %s * Origin Country: %s * Ground Speed: %d ' \
                        'mph * Altitude: %d ft' % \
                        (vector[1].strip(), vector[2], math.ceil(0 * 2.23694), math.ceil(0 * 3.28084))
            track_url = f"----{info_line}|href='https://flightaware.com/live/flight/{vector[1].strip()}' font='Menlo'"
            print(track_url)


def main():
    # Obtain the user's location based on their public ip address.
    location_data = perform_request("https://ipapi.co/json/")

    # We want to create an airspace area, thus we add some margin to the user's lon and lat
    variance = 1.2
    lat = location_data["latitude"]
    lon = location_data["longitude"]

    url = 'https://opensky-network.org/api/states/all?lamin=%d&lomin=%d&lamax=%d&lomax=%d' % \
          (lat - variance, lon - variance, lat + variance, lon + variance)

    air = perform_request(url)
    print_menu(air)


if __name__ == '__main__':
    main()
