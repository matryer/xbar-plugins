#!/bin/bash

# <bitbar.title>Mu Email Checker</bitbar.title>
# <bitbar.version>v1.0.1</bitbar.version>
# <bitbar.author>Andrew Sanchez</bitbar.author>
# <bitbar.author.github>andrewsanchez</bitbar.author.github>
# <bitbar.desc>Displays results of basic mu find commands</bitbar.desc>
# <bitbar.image>http://i67.tinypic.com/104pa1w.png</bitbar.image>

# Count total number of emails in inbox, unread mail and drafts indexed with mu
# Also includes the output for unread emails in the drop down menu
# Based on a blog post by Ben Maughan at the excellent pragmaticemacs.com
# http://pragmaticemacs.com/emacs/an-unobtrusive-email-monitor-for-mu4e-on-the-mac/
# See related post to include mails in outbox using postfix
# http://pragmaticemacs.com/emacs/using-postfix-instead-of-smtpmail-to-send-email-in-mu4e/

# Total mails in inbox
total="$(/usr/local/bin/mu find maildir:/INBOX | wc -l)"
# Unread mails in inbox
unread="$(/usr/local/bin/mu find maildir:/INBOX AND flag:unread 2> /dev/null)"
unread_total="$(echo $unread | wc -l)"
# Drafts
drafts="$(/usr/local/bin/mu find maildir:/drafts | wc -l)"

printf "📪 %s/%s/%s\n" $unread_total $total $drafts
echo ---
echo $unread
