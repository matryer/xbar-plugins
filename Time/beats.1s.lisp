#!/usr/local/bin/clisp

; <xbar.title>Beats Time</xbar.title>
; <xbar.version>v1.1</xbar.version>
; <xbar.author>Jannis Segebrecht</xbar.author>
; <xbar.author.github>queitsch</xbar.author.github>
; <xbar.desc>Displays Swatch .beats time.</xbar.desc>
; <xbar.dependencies>clisp</xbar.dependencies>
; <xbar.image>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</xbar.image>

(format t "~A" ;print without quotes
	(concatenate 'string
		"@"
		(format nil "~$" ;print two decimal places
			(/ (mod (+ (mod 
				(get-universal-time) 86400) 3600) 86400) 86.4))))
