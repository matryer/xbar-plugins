#!/bin/bash
#
# <xbar.title>Emacs Menu Controller</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>Ken Mankoff</xbar.author>
# <xbar.author.github>mankoff</xbar.author.github>
# <xbar.desc>Emacs controlled</xbar.desc>
# <xbar.image>http://i.imgur.com/ZTScWDa.png</xbar.image>

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
