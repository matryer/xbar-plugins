#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python3

# Copyright 2019 Jacob Vossen 

# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# <bitbar.title>NASA Picture of the Day</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jake Vossen</bitbar.author>
# <bitbar.author.github>jakevossen5</bitbar.author.github>
# <bitbar.desc>Displays a the NASA picture of the day</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/jakevossen5/apodbitbar/master/example.png</bitbar.image>
# <bitbar.dependencies>python3,pillow</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/jakevossen5/apodbitbar/tree/master</bitbar.abouturl>



# May need to pip(3) install pillow
# May need to install certificates: https://stackoverflow.com/questions/50236117/scraping-ssl-certificate-verify-failed-error-for-http-en-wikipedia-org
import base64
import urllib.request
from PIL import Image
import io
from io import BytesIO

# with open("/tmp/temp.jpg", "rb") as image_file:
    # encoded_string = str(base64.b64encode(image_file.read()))[2:][:-1]
# print(encoded_string)
print('🔭') # The telescope emoji is here, if your text editor does not render it
print('---')
url = 'https://apod.nasa.gov/apod/'
req = urllib.request.Request(url)
# try:
r = str(urllib.request.urlopen(req).read())
# print(r)
# print(r.find('.jpg'))
try:
    image_url = ""
    for i in range(r.find('.jpg'), 0, -1):
        if r[i] != '"':
            image_url = r[i] + image_url
        else:
            break
    image_url = image_url + "jpg"
    image_url = "https://apod.nasa.gov/apod/" + image_url


    image_req = urllib.request.Request(image_url)

    image_path = io.BytesIO(urllib.request.urlopen(image_req).read())
    pillow_image = Image.open(image_path)
    # pillow_image.show()
    maxsize = (720, 720)

    pillow_image.thumbnail(maxsize)
    buff = BytesIO()
    pillow_image.save(buff, format="JPEG")
    encoded_string = base64.b64encode(buff.getvalue())
    # base64_data = base64.b64encode(binary_data)


    # encoded_string = ""
    # except:
    # print("There was an error, most likely with SSL certificates. Click me to fix |href=https://stackoverflow.com/questions/50236117/scraping-ssl-certificate-verify-failed-error-for-http-en-wikipedia-org")
    
    # print(r)
    # image_string = "image=" + encoded_string
    print("| href=https://apod.nasa.gov/apod/ image=" +  str(encoded_string)[2:][:-1])
except:
    print("Could not complete the request. Either the picture of the day is a video, or you do not have the proper dependencies installed")
