#!/bin/bash

# <xbar.title>Mu Email Checker</xbar.title>
# <xbar.version>v1.0.1</xbar.version>
# <xbar.author>Andrew Sanchez</xbar.author>
# <xbar.author.github>andrewsanchez</xbar.author.github>
# <xbar.desc>Displays results of basic mu find commands</xbar.desc>
# <xbar.image>http://i67.tinypic.com/104pa1w.png</xbar.image>

# Count total number of emails in inbox, unread mail and drafts indexed with mu
# Also includes the output for unread emails in the drop down menu
# Based on a blog post by Ben Maughan at the excellent pragmaticemacs.com
# http://pragmaticemacs.com/emacs/an-unobtrusive-email-monitor-for-mu4e-on-the-mac/
# See related post to include mails in outbox using postfix
# http://pragmaticemacs.com/emacs/using-postfix-instead-of-smtpmail-to-send-email-in-mu4e/

mu="/usr/local/bin/mu"

# mu find patterns
pinbox="maildir:/INBOX"
punread="$pinbox AND flag:unread"
pdrafts="maildir:/drafts"

# Total mail in inbox
total="$("$mu" find "$pinbox" | wc -l)"

# Unread mails in inbox
unread="$("$mu" find "$punread" 2> /dev/null)"
unread_total="$("$mu" find "$punread" 2> /dev/null | wc -l)"

# Drafts
drafts="$("$mu" find "$pdrafts" | wc -l)"

if [ "$unread_total" -gt 0 ]
then
    printf "📪 %i/%i/%i\n" "$unread_total" "$total" "$drafts"
    echo ---
    echo "$unread"
fi
