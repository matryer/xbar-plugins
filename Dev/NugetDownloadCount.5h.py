#!/usr/bin/env -S PATH="${PATH}:/opt/homebrew/bin:/usr/local/bin" PYTHONIOENCODING=UTF-8 python3 -i
# Copyright 2024 Tony Clark

# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 
# <xbar.title>NuGet Package Download Count</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Tony Clark</xbar.author> 
# <xbar.author.github>TheCriticalPath</xbar.author.github>
# <xbar.desc>Displays the download count for each version of a specified nuget package.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/TheCriticalPath/NuGetDownloadCount/main/screenshot.png</xbar.image>
# <xbar.dependencies>python3,requests</xbar.dependencies>
# <xbar.abouturl>https://github.com/TheCriticalPath/NuGetDownloadCount/</xbar.abouturl>

# Variables become preferences in the app:
#
#  <xbar.var>string(VAR_PACKAGES="Seq.App.AzureSecretCheck,Newtonsoft.json"): Comma delimited list of Nuget Packages.</xbar.var>
import requests
import os
#import urllib.request

packages = os.environ['VAR_PACKAGES']
#packages = 'Seq.App.AzureSecretCheck,Newtonsoft.json'

def fetch_nuget_data():
    service_index = fetch_service_index()
    for package_name in packages.split(','):
        service_query = service_index + f"?q={package_name.lower()}&prerelease=false&listed=true"
        jsonData = fetch_nuget_metadata(service_query)
        dataObj = jsonData['data']
        for item in dataObj:
            if item['id'].lower() == package_name.lower():
                print (f"Package: {item['id']}")
                print (f"--Description: {item['description']} | color=#defdef")
                print (f"--Author(s): {', '.join(item['authors'])} | color=#defdef")
                print(f"--Total Downloads: {item['totalDownloads']} | color=#defdef")
                print(f'--Listed Downloads: {sum([v["downloads"] for v in item["versions"]])} | color=#defdef')        
                for version in item['versions'][::-1]:
                    print(f"--Version: {version['version']}, Total Downloads: {version['downloads']} | color=#defdef")
                print(f'---')        

def fetch_nuget_metadata(service_index):
    response = requests.get(service_index)
    response.raise_for_status()
    data = response.json()
    return data

def fetch_service_index():
    url = "https://api.nuget.org/v3/index.json"
    response = requests.get(url)
    response.raise_for_status()  # Raise an exception for HTTP errors
    data = response.json()
    
    for resource in data.get('resources', []):
        if 'SearchQueryService' in resource.get('@type', ''):
            return resource.get('@id')

# if __name__ == "__main__":
#    fetch_nuget_data()

print ('🍗')
print ('---')
try:
    fetch_nuget_data()
    
except Exception as e:
    print(f"Error: {e}")
    print ('---')
    print ('🚫')
