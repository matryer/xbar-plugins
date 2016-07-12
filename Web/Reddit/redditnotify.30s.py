#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Reddit Notifications</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>HeyItsShuga</bitbar.author>
# <bitbar.author.github>Shugabuga</bitbar.author.github>
# <bitbar.desc>Check your Reddit messages in your status bar!</bitbar.desc>
# <bitbar.image>http://i.imgur.com/ohM8E6u.png</bitbar.image>
# <bitbar.dependencies>python,praw</bitbar.dependencies>

############################################################################
#                      Reddit Notifications for BitBar                     #
#                            by /u/HeyItsShuga                             #
#                                                                          #
#                              Configuration                               #
#  Tip: Make sure that the quotes stay as real quotes (if using TextEdit). #
############################################################################

USERNAME = "" # Please put your username inside of the quotes.
PASSWORD = "" # Please put your password inside of the quotes.

############################################################################
#                        End Configuration. Enjoy!                         #
############################################################################

import os
import praw

if USERNAME == '':
    print "Not Configured! | color=#e51b1b"
    print "---"
    print "Click here to open the config in nano. | color=#e51b1b bash=' nano", __file__, '"'
    os.system("open " + __file__)

message_noteInt = "No Messages!"
messageCount = 0

r = praw.Reddit(user_agent="Reddit Notifications for BitBar",disable_update_check=True)
r.login(USERNAME, PASSWORD, disable_warning=True)

unreadInt = r.get_unread()
unread = r.get_unread()

for messages in unreadInt:
    message_noteInt = str(messages)
    messageCount = messageCount + 1

if message_noteInt == "No Messages!":
    print "| image=iVBORw0KGgoAAAANSUhEUgAAABkAAAAQCAMAAADUOCSZAAAAA3NCSVQICAjb4U/gAAAACXBIWXMAAA3XAAAN1wFCKJt4AAAAGXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAAJ9QTFRFAAAAAAAAgICAgICAbW1tgICAeHh4gICAgICAfHx8fHx8fX19fX19e3t7fX19fHx8fn5+fHx8fn5+fHx8fn5+fn5+fHx8fX19fn5+fX19fHx8fn5+fX19fn5+fX19fX19fX19fX19fX19fX19fX19fn5+fn5+fX19fX19fX19fX19fX19fX19fX19fX19fX19fX19fX19fX19fX19fX19ui5QtQAAADR0Uk5TAAECBgcIERIUISUvNTY7REdISUpLT1BWYWhpgIWOj5Gjpau/wMHFyNbX2+Dh4uPk5efo6c9RgL0AAACeSURBVBjTddHZEoIwDAXQiyKItYribkVRcANaEP7/25TRUsGat+RkMk0KT5S64B7ELiFoB0l8jhLzYtaCcbF81UtgmG0asMpHeAuc675T143tfQApsE7H3ge6h3MfSupc9UiRM9RcJSwWFFTEzGiKGYSWm7PctcLA/BY7qnJyIVVPZCtxbn9eTbO1ftPF73Ue1XW4r79oiinX/kI6eQKV6hmFxUk2wQAAAABJRU5ErkJggg=="
else:
    print "| image=iVBORw0KGgoAAAANSUhEUgAAABkAAAAQCAMAAADUOCSZAAAAA3NCSVQICAjb4U/gAAAACXBIWXMAAA3XAAAN1wFCKJt4AAAAGXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAAJ9QTFRFAAAA/wAA/wAA1Ssr2yQk3yAg4R4e4xwc5hoa6BcX4xwc5Bsb5x0d4xwc5Roa5Roa5h0d5hwc5xwc4xwc5Bsb5Roa5hoa5Bsb5Roa5Bsb5Bsb5Rwc5Bsb5hsb5Bsb5Roa5Bsb5Roa5hsb5hsb5Bsb5Roa5Rsb5hsb5Rsb5Rsb5Rsb5Rsb5Rsb5Rsb5Rsb5Rsb5Rsb5Roa5Rsb5Rsb5RsbcU/E7wAAADR0Uk5TAAECBgcIERIUISUvNTY7REdISUpLT1BWYWhpgIWOj5Gjpau/wMHFyNbX2+Dh4uPk5efo6c9RgL0AAACeSURBVBjTddHZEoIwDAXQiyKItYribkVRcANaEP7/25TRUsGat+RkMk0KT5S64B7ELiFoB0l8jhLzYtaCcbF81UtgmG0asMpHeAuc675T143tfQApsE7H3ge6h3MfSupc9UiRM9RcJSwWFFTEzGiKGYSWm7PctcLA/BY7qnJyIVVPZCtxbn9eTbO1ftPF73Ue1XW4r79oiinX/kI6eQKV6hmFxUk2wQAAAABJRU5ErkJggg=="

print "---"
print "Messages: ", messageCount, "| font=HelveticaNeue-Bold href=https://www.reddit.com/message/unread/"
print "---"
if message_noteInt == "No Messages!":
    print "No Unread Messages! | color=teal"
for messages in unread:
    print str(messages), " | color=teal"
    print "---"
