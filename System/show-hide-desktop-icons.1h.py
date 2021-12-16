#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <xbar.title>Show/Hide Desktop Icons</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Jordi Petit</xbar.author>
# <xbar.author.github>jordi-petit</xbar.author.github>
# <xbar.desc>This plugin displays a menu with two items to show or hide the desktop icons with one click.</xbar.desc>
# <xbar.image>http://i.imgur.com/c3KYTAc.png</xbar.image>
# <xbar.dependencies></xbar.dependencies>

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
