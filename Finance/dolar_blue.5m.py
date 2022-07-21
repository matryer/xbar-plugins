#!/opt/homebrew/bin/python3
# coding=utf-8

# <xbar.title>ARS/USD Blue tracker</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Ignacio Van Droogenbroeck</xbar.author>
# <xbar.author.github>xe-nvdk</xbar.author.github>
# <xbar.desc>Displays the latest dollar blue rate</xbar.desc>
# <xbar.image>https://i.imgur.com/zJsoTl8.jpg</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.dependencies>scrapy</xbar.dependencies>

from lxml import html
import requests

page = requests.get('https://www.dolarhoy.com')

tree = html.fromstring(page.content)
buy = tree.xpath('/html/body/div/div[2]/div[1]/div[1]/div[2]/section/div/div/div/div[1]/div/div[1]/div/div[1]/div[1]/div[2]')
sale = tree.xpath('/html/body/div/div[2]/div[1]/div[1]/div[2]/section/div/div/div/div[1]/div/div[1]/div/div[1]/div[2]/div[2]')

print('B: ' + buy[0].text + ' S: ' + sale[0].text)