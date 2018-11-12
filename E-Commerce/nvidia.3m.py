#!/Library/Frameworks/Python.framework/Versions/3.6/bin/python3

# <bitbar.title>1080ti Stock Checker</bitbar.title>
# <bitbar.version>v2.0</bitbar.version>
# <bitbar.author>Alex Haynes</bitbar.author>
# <bitbar.author.github>alexh</bitbar.author.github>
# <bitbar.desc>Checks nvidia site if 1080ti are in stock</bitbar.desc>
# <bitbar.image>https://images.nvidia.com/pascal/img/gtx1080ti/gallery/gallery-2.jpg</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

import urllib.request, json, time
with urllib.request.urlopen("http://api.findgpu.com/gpus?" + str(int(time.time()))) as url:
	data = json.loads(url.read().decode())
	my_item = next((item for item in data if item['gpu_id'] == "900-1G611-2550-000"), None)
	in_stock = my_item['in_stock']
	if in_stock == 'false':
		status = ":heavy_multiplication_x:"
	else:
		status = ":white_check_mark:"
print ("1080Ti: " + status)
print ("---")
print ("Buy Now" + "| href=https://www.nvidia.com/en-us/geforce/products/10series/geforce-gtx-1080-ti/")
