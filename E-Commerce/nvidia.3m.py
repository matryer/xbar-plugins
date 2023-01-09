#!/usr/bin/env python3

# <xbar.title>1080ti Stock Checker</xbar.title>
# <xbar.version>v2.0</xbar.version>
# <xbar.author>Alex Haynes</xbar.author>
# <xbar.author.github>alexh</xbar.author.github>
# <xbar.desc>Checks nvidia site if 1080ti are in stock</xbar.desc>
# <xbar.image>https://images.nvidia.com/pascal/img/gtx1080ti/gallery/gallery-2.jpg</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>

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
