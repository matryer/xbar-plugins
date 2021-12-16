#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <xbar.title>Pubmed Articles</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Dogancan Ozturan</xbar.author>
# <xbar.author.github>ozturan</xbar.author.github>
# <xbar.desc>Number of publications for a given keyword on Pubmed</xbar.desc>
# <xbar.image>http://i.imgur.com/lqCaAJP.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>

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