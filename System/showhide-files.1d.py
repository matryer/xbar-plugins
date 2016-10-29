#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# <bitbar.title>Show hidden files in Finder</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Thomas Kurz</bitbar.author>
# <bitbar.author.github>dashorty</bitbar.author.github>
# <bitbar.desc>Show hidden files in Finder</bitbar.desc>
# <bitbar.image>https://github.com/dashorty/bitbar-showhide-files/blob/master/screenshot.png?raw=true</bitbar.image>
# <bitbar.abouturl>https://github.com/dashorty/bitbar-showhide-files</bitbar.abouturl>

import sys
import os

if len(sys.argv) == 1:
    print 'S/H'
    print '---'
    print 'show hidden files | terminal=false bash=%s param1=show' % sys.argv[0]
    print 'hide hidden files | terminal=false bash=%s param1=hide' % sys.argv[0]
elif sys.argv[1] == 'show':
    os.system('defaults write com.apple.finder AppleShowAllFiles YES && killall Finder')
elif sys.argv[1] == 'hide':
    os.system('defaults write com.apple.finder AppleShowAllFiles NO && killall Finder')
