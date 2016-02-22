#!/bin/bash

#
# Include BitBar metadata like this at the top of the file
# (commented out, of course):
#
# <bitbar.title>Cycle text and detail text</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Mat Ryer</bitbar.author>
# <bitbar.author.github>matryer</bitbar.author.github>
# <bitbar.desc>Example of how to include items that cycle in the top, and items that only appear in the dropdown.</bitbar.desc>
# <bitbar.image>https://camo.githubusercontent.com/5cec3248a9fc4eede235ead682a65f977577f670/68747470733a2f2f7261772e6769746875622e636f6d2f6d6174727965722f6269746261722f6d61737465722f446f63732f4269744261722d4578616d706c652d4d656e752e706e67</bitbar.image>
# <bitbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/Tutorial/cycle_text_and_detail.sh</bitbar.abouturl>
#
# Text above --- will be cycled through in the menu bar,
# whereas text underneath will be visible only when you
# open the menu.
#

echo one
echo two
echo three
echo ---
echo These lines are only visible
echo when you open the menu.
