#!/usr/bin/env python3
#
# <xbar.title>Show/hide hidden files and desktop icons</xbar.title>
# <xbar.version>v0.3</xbar.version>
# <xbar.author>Thomas Kurz, Robin Moser</xbar.author>
# <xbar.author.github>dashorty</xbar.author.github>
# <xbar.desc>Show hidden files in Finder</xbar.desc>
# <xbar.image>https://i.imgur.com/h2Mdxye.png</xbar.image>
# <xbar.abouturl>https://github.com/dashorty/bitbar-showhide-files</xbar.abouturl>

import sys
import os

if len(sys.argv) == 1:

    print("| templateImage=iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAQAAABKfvVzAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAAmJLR0QAAKqNIzIAAAAJcEhZcwAADdcAAA3XAUIom3gAAAAHdElNRQfiAwIWGC2lnrmcAAAA8UlEQVQ4y92SMW7CUBBEX9JZQikxDVS0VKShceEbcBkkDhHREwquQIm4ABUHQOIAlKZCisJLE+z/bUeK6JLp/uxo/87uwL+FXbu/E45derBQtfDg0vHP4ql777hZYe+0Ke64LgUbcxNTM3clt7YTykcey9IsajQv+aOjOzn0XHUHsO+bKycAwS9nh4A9T8G8OTjwquqnEzALqid7uA2Imwm4KN8rMDXE9jny/sQLUNkrgAvG+4lHysBXP1S92gfz2kg107vv8727cADgpma6sdZ59P+sZa2Nw+3MTE3Mg+7x4VqiEYajLRoPhe+heP9JfAEpl32XjyFcEgAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAxOC0wMy0wMlQyMjoyNDo0NSswMTowMFm39GAAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMTgtMDMtMDJUMjI6MjQ6NDUrMDE6MDAo6kzcAAAAGXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAAABJRU5ErkJggg==")

    print('---')
    print('show hidden files | terminal=false bash={} param1=showHidden'.format(sys.argv[0]))
    print('hide hidden files | terminal=false bash={} param1=hideHidden'.format(sys.argv[0]))

    print('---')
    print('show desktop icons | terminal=false bash={} param1=showIcons'.format(sys.argv[0]))
    print('hide desktop icons | terminal=false bash={} param1=hideIcons'.format(sys.argv[0]))

elif sys.argv[1] == 'showHidden':
    os.system('defaults write com.apple.finder AppleShowAllFiles YES && killall Finder')

elif sys.argv[1] == 'hideHidden':
    os.system('defaults write com.apple.finder AppleShowAllFiles NO && killall Finder')

elif sys.argv[1] == 'showIcons':
    os.system('defaults write com.apple.finder CreateDesktop YES && killall Finder')

elif sys.argv[1] == 'hideIcons':
    os.system('defaults write com.apple.finder CreateDesktop NO && killall Finder')
