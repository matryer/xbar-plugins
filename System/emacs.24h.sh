#!/bin/bash
#
# <bitbar.title>Emacs Menu Controller</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Ken Mankoff</bitbar.author>
# <bitbar.author.github>mankoff</bitbar.author.github>
# <bitbar.desc>Emacs controlled</bitbar.desc>
# <bitbar.image>http://i.imgur.com/ZTScWDa.png</bitbar.image>

PATH=/usr/local/bin:$PATH  # where is emacsclient?

case "$1" in
    "email")
	open -a Emacs    # bring to front
	emacsclient -e '(mu4e-compose-new)'
	exit
	;;
    "buffer")
	open -a Emacs
	emacsclient -e '(progn (select-frame (make-frame))(switch-to-buffer (get-buffer-create "*scratch*")) (set-frame-size (selected-frame) 92 72)(set-frame-position (selected-frame) 840 0))'
	exit
	;;
esac

echo '🦄'
echo "---";
echo "📬 Email | bash=\"$0\" param1=email terminal=false"
echo "📠 *scratch* Buffer | bash=\"$0\" param1=buffer terminal=false"
