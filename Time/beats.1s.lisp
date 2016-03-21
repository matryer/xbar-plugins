#!/usr/local/bin/clisp

; <bitbar.title>Beats Time</bitbar.title>
; <bitbar.version>v1.0</bitbar.version>
; <bitbar.author>Jannis Segebrecht</bitbar.author>
; <bitbar.author.github>queitsch</bitbar.author.github>
; <bitbar.desc>Displays Swatch .beats time.</bitbar.desc>
; <bitbar.dependencies>clisp</bitbar.dependencies>
; <bitbar.image>http://imgur.com/W0iKDfu</bitbar.image>

(format t "~A" ;print without quotes
	(concatenate 'string
		"@"
		(format nil "~$" ;print two decimal places
			(/ (mod (+ (mod 
				(get-universal-time) 86400) 3600) 86400) 86.4))))
