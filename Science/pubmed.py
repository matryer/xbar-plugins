#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Pubmed Articles</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Dogancan Ozturan</bitbar.author>
# <bitbar.author.github>ozturan</bitbar.author.github>
# <bitbar.desc>Number of publications for a given keyword on Pubmed</bitbar.desc>
# <bitbar.image>http://i.imgur.com/lqCaAJP.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

import urllib2
from HTMLParser import HTMLParser

# add as much as you want
keyword_list = ["CRISPR","TALEN", "Zinc+Finger"]

def get_html(keyword):
	f = urllib2.urlopen("https://www.ncbi.nlm.nih.gov/pubmed/?term=%s" %keyword)
	return str(f.read())

class MyHTMLParser(HTMLParser):
	def __init__(self):
		HTMLParser.__init__(self)
		self.__text = []

	def handle_starttag(self, tag, attrs):
		if tag == "meta":
			self.__text.append(attrs[1][1])

	def text(self):
		return self.__text


def html(text):
	parser = MyHTMLParser()
	parser.feed(text)
	parser.close()
	return parser.text()

if __name__ == '__main__':
	print " 🏛️ "
	print "---"
	for i in keyword_list:
		print "%s" %i + " -- " + html(get_html(i))[15] + " | href=https://www.ncbi.nlm.nih.gov/pubmed/?term=%s" %i