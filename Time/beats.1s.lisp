#!/usr/local/bin/clisp

; <xbar.title>Beats Time</xbar.title>
; <xbar.version>v1.1</xbar.version>
; <xbar.author>Jannis Segebrecht</xbar.author>
; <xbar.author.github>queitsch</xbar.author.github>
; <xbar.desc>Displays Swatch .beats time.</xbar.desc>
; <xbar.dependencies>clisp</xbar.dependencies>
; <xbar.image>http://i.imgur.com/W0iKDfu.png</xbar.image>

(format t "~A" ;print without quotes
	(concatenate 'string
		"@"
		(format nil "~$" ;print two decimal places
			(/ (mod (+ (mod 
				(get-universal-time) 86400) 3600) 86400) 86.4))))
