#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# <bitbar.title>Show hidden files in Finder</bitbar.title>
# <bitbar.version>v0.2</bitbar.version>
# <bitbar.author>Thomas Kurz</bitbar.author>
# <bitbar.author.github>dashorty</bitbar.author.github>
# <bitbar.desc>Show hidden files in Finder</bitbar.desc>
# <bitbar.image>https://github.com/dashorty/bitbar-showhide-files/blob/master/screenshot.png?raw=true</bitbar.image>
# <bitbar.abouturl>https://github.com/dashorty/bitbar-showhide-files</bitbar.abouturl>

import sys
import os

if len(sys.argv) == 1:
    print "| templateImage=iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAQAAABKfvVzAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAAmJLR0QAAKqNIzIAAAAJcEhZcwAADdcAAA3XAUIom3gAAAAHdElNRQfiAwIWGC2lnrmcAAAA8UlEQVQ4y92SMW7CUBBEX9JZQikxDVS0VKShceEbcBkkDhHREwquQIm4ABUHQOIAlKZCisJLE+z/bUeK6JLp/uxo/87uwL+FXbu/E45derBQtfDg0vHP4ql777hZYe+0Ke64LgUbcxNTM3clt7YTykcey9IsajQv+aOjOzn0XHUHsO+bKycAwS9nh4A9T8G8OTjwquqnEzALqid7uA2Imwm4KN8rMDXE9jny/sQLUNkrgAvG+4lHysBXP1S92gfz2kg107vv8727cADgpma6sdZ59P+sZa2Nw+3MTE3Mg+7x4VqiEYajLRoPhe+heP9JfAEpl32XjyFcEgAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAxOC0wMy0wMlQyMjoyNDo0NSswMTowMFm39GAAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMTgtMDMtMDJUMjI6MjQ6NDUrMDE6MDAo6kzcAAAAGXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAAABJRU5ErkJggg=="
    print '---'
    print 'show hidden files | terminal=false bash=%s param1=show' % sys.argv[0]
    print 'hide hidden files | terminal=false bash=%s param1=hide' % sys.argv[0]
    print '---'
    print 'show desktop icons | terminal=false bash=%s param1=showicn' % sys.argv[0]
    print 'hide desktop icons | terminal=false bash=%s param1=hideicn' % sys.argv[0]
elif sys.argv[1] == 'show':
    os.system('defaults write com.apple.finder AppleShowAllFiles YES && killall Finder')
elif sys.argv[1] == 'hide':
    os.system('defaults write com.apple.finder AppleShowAllFiles NO && killall Finder')
elif sys.argv[1] == 'showicn':
    os.system('defaults write com.apple.finder CreateDesktop YES && killall Finder')
elif sys.argv[1] == 'hideicn':
    os.system('defaults write com.apple.finder CreateDesktop NO && killall Finder')

