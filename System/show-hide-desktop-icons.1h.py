#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Show/Hide Desktop Icons</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jordi Petit</bitbar.author>
# <bitbar.author.github>jordi-petit</bitbar.author.github>
# <bitbar.desc>This plugin displays a menu with two items to show or hide the desktop icons with one click.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/c3KYTAc.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

import sys, os

if len(sys.argv) == 1:
#   print 'Ξ'
    print '💻'
    print '---'
    print 'Hide desktop icons | terminal=false bash=%s param1=hide' % sys.argv[0]
    print 'Show desktop icons | terminal=false bash=%s param1=show' % sys.argv[0]
elif sys.argv[1] == 'hide':
   os.system('defaults write com.apple.finder CreateDesktop false && killall Finder')  
elif sys.argv[1] == 'show':
   os.system('defaults write com.apple.finder CreateDesktop true  && killall Finder')     
