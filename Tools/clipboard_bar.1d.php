#!/usr/bin/env php
<?php

# <bitbar.title>Clipboard Text Manipulator</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Adi</bitbar.author>
# <bitbar.author.github>gomedia-adi</bitbar.author.github>
# <bitbar.desc>Manipulate Text in the Clipboard.</bitbar.desc>
# <bitbar.image>http://www.greatoceanmedia.com.au/images/64.png</bitbar.image>
# <bitbar.dependencies>php,python,perl</bitbar.dependencies>
# <bitbar.abouturl>http://www.greatoceanmedia.com.au/bitbar/clipboard-bar</bitbar.abouturl>

/*
	BitBar plugin help: https:github.com/matryer/bitbar

	Plugin updated 25/9/20

	Version history
		1.0	- initial release
*/

//-------------------------------------------------------------------------------------------------

//??? PROCESS MENU ACTIONS

// WARNING: sed & awk add newlines to output

if (isset($argv[1])) { // menu option selected
	switch ($argv[1]) {
		case 'remove_whitespace':
			$cmd = "pbpaste | tr -d '[:blank:]' | pbcopy";
			break;
		case 'squeeze_whitespace':
			$cmd = "pbpaste | tr -s '[:blank:]' | pbcopy";
			break;
		case 'trim_whitespace':
			$cmd = "pbpaste | python -c 'import sys; sys.stdout.write(sys.stdin.read().strip())' | pbcopy";
			break;
		case 'remove_newlines':
			$cmd = "pbpaste | tr -d '\n\r' | pbcopy";
			break;
		case 'newlines_to_spaces':
			$cmd = "pbpaste | tr '\n\r' ' ' | pbcopy";
			break;
		case 'remove_blanklines':
			$cmd = "pbpaste | awk 'NF' | pbcopy";
			break;
		case 'make_lowercase':
			$cmd = "pbpaste | tr '[:upper:]' '[:lower:]' | pbcopy";
			break;
		case 'make_uppercase':
			$cmd = "pbpaste | tr '[:lower:]' '[:upper:]' | pbcopy";
			break;
		case 'make_plaintext':
			$cmd = "pbpaste | pbcopy";
			break;
		case 'remove_xml':
			$cmd = "pbpaste | sed 's|<[^>]*>| |g' | tr -s '[:blank:]' | sed 's/^[[:blank:]]*//;s/[[:blank:]]*$//' | pbcopy"; // convert <xml> to space, multiple blanks squeezed, string trimmed (sed version)
			break;
		case 'reverse':
			$cmd = "pbpaste | rev | pbcopy";
			break;
		case 'capitalise_first':
			$cmd = "pbpaste | perl -ne 'print ucfirst' | pbcopy";
			break;
		case 'capitalise_all':
			$cmd = "pbpaste | perl -pe 's/(\\w+)/\\u\\L\$1/g' | pbcopy";
			break;
		case 'remove_non_alphanum':
			$cmd = "pbpaste | tr -cd [:alnum:] | pbcopy";
			break;
		case 'remove_punc':
			$cmd = "pbpaste | tr -d [:punct:] | pbcopy";
			break;
		default:
			$cmd = '';
	}
	if ($cmd)
		exec($cmd, $out, $err);
}

//??? MENUBAR

// display icon: 36x36 png (32x32max image + 2px transparent border), 144 dpi, colour mode grey, encoded using "base64 icon.png" ... prepend "|templateImage=" & append "\n"
echo "|templateImage=iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAQAAABLCVATAAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAAFiUAABYlAUlSJPAAAAAHdElNRQfkBhUWDSFI6hDoAAAEuUlEQVRIx82V64tVZRTGf+t933OOM+owjjYqVjIRmYS3CKkMBD2imXSBhEKLiOgOZoQVFFF9iAK7QJAVQlQgiJQU4mWOFlTCfChvQRbdFCHRUHTGGZ2937X68O59zlH6A9wfNpy993netZ71PM+Cy+0SgHpA0OKXAQ7DcIAiCIZl8m0Mt9u9zGOKVWSYYwy4LfkPi4KzRiyBKkTAAxGHJ6IEjByHQzH8qbD/e7vRYSiuOE/gM32wXmlk4ABQIOBQHIFIxGPkCB4w/Fi3f28JIy0Y7AH3Gh5KIPAIOYYjb+R4hIgjAIoLbvtVOldSNW90zqheI6skNYPc3X+hBSQFjEfRelmbw4iATMuyxe4gGG7DuZcW/LXgaNxUvckwBJsk1k62AR69hGLFIajRUDHouO6GI905XMGm2N15ZlAc2Cmb2ALyyP9TjBCBgBLxdX0hdnbZ1LwvX8XqYrSnYw9AKDgK5MQCRvAYhseTYVSIjfxOf6i251F55XxXSyGKwywBuILqvJGnNpCSYhw5ikctPuW2vX/kjK2XLin+pmfVHDSBQjH+kmLw0BgFIAdYjMTp8uHD9njiQQ/LO9UDlT8ZGh5CLEmnWVFiQ1PFLpetzsTEnPm1uzP83lr8wDDA1tvMFzfe9uPNp6rjEEOQi4AcENM0Tb80t7B8qG8DHK0hhVOeq9cGRKzLnZtqApDuLUHmhUW02/UstW4rpg+Ve3xUK9Vcfagn74sT5Kfqhe+KkYd203qMgJAjPW7LP9rTJqaGLZkz5tAgwXCY6Xq/leX6PD6By4h2tgO5pBj1A93njjtpuQk6pyw6vW2h7pI2pZbvDKGzd+hkyyIQAbdndPiJBCM/pxONkdclj/3+WTAcgiHI/ur19rkhwLmP2y2SdKPn2Zulc3jG+lgDgp2wyfXa0uzV8efX2nwN8rf/JNu7yB3sOntrjKI+XNje3ponr2n/HflXqXyVSm88LmIY1RULd2okwKx8GjsqKIrHk2MEtJVHhba3af5kguELmHdGshSU2YZdSffxkOyokHr05ERcgmnpKCfWQ9dUW5ZyRjYuCdPV1mmSwdTpVfOFfZKNHTkxZWrdtytbCTUZWitF/q3aaXJa9T03Cobzx97aPYrhMPJkNZSU9I6LgAJsy7g/zUne/DSisKLKL2kyumbi2CLoymwITTXE9taM3N/ClUkhYz+CRlwsX2f2cqnpwfnjrKA4UZ1qE5S2hKyQO2nsY7YlwY0UdSBIRwKXQ3F2PTRzi0ZWd4TUYmO0uUXMrYEZCdnQDumgQzukQzrKSLdZYWZzNSSKQzM3WuHfJ3c9Tc2aQV2qmObWNOyR/gyttyhOjBW5VbR2TH4dLmyYyemUNIXfzLqpgsAFHVOvFqtNikUhKDRiUVGw368Wn+oIq3Wy9lqv9mqv9VqvTnb3pc+sFh6zdoo9rhnNlLG0WVYahhuK46Hu8cQyWgYmDJ8sGj6gc5sUp9XARRYZP0lWportmwKmXEY5NvivbS54mzO2U0JBoCNDL7HI8J0lxfLu4lAIzpXbd3lw65ylTBjZXLOmRZQAxHpoBqUtsz+cITaie6SCpsgpV+OoXXvit8NWE8Osb5/MLN9VSBIVLsvrPxPRatOBpsPTAAAAAElFTkSuQmCC\n";

//??? TOP-LEVEL MENU

//??? MENU ACTIONS

echo "---\n";

echo "Clipboard: | color=gray\n";

echo
	'Remove Whitespace…'
	." |  bash=\"".$argv[0]."\" param1=remove_whitespace refresh=false terminal=false"
	."\n";

echo
	'Squeeze Whitespace…'
	." |  bash=\"".$argv[0]."\" param1=squeeze_whitespace refresh=false terminal=false"
	."\n";

echo
	'Trim Whitespace…'
	." |  bash=\"".$argv[0]."\" param1=trim_whitespace refresh=false terminal=false"
	."\n";

echo
	'Remove Newlines…'
	." |  bash=\"".$argv[0]."\" param1=remove_newlines refresh=false terminal=false"
	."\n";

echo
	'Replace Newlines with Spaces…'
	." |  bash=\"".$argv[0]."\" param1=newlines_to_spaces refresh=false terminal=false"
	."\n";

echo
	'Remove Blank Lines…'
	." |  bash=\"".$argv[0]."\" param1=remove_blanklines refresh=false terminal=false"
	."\n";

echo
	'Make Lowercase…'
	." |  bash=\"".$argv[0]."\" param1=make_lowercase refresh=false terminal=false"
	."\n";

echo
	'Make Uppercase…'
	." |  bash=\"".$argv[0]."\" param1=make_uppercase refresh=false terminal=false"
	."\n";

echo
	'Capitalise First…'
	." |  bash=\"".$argv[0]."\" param1=capitalise_first refresh=false terminal=false"
	."\n";

echo
	'Capitalise All…'
	." |  bash=\"".$argv[0]."\" param1=capitalise_all refresh=false terminal=false"
	."\n";

echo
	'Remove Punctuation…'
	." |  bash=\"".$argv[0]."\" param1=remove_punc refresh=false terminal=false"
	."\n";

echo
	'Remove Non-Alphanumeric…'
	." |  bash=\"".$argv[0]."\" param1=remove_non_alphanum refresh=false terminal=false"
	."\n";

echo
	'Make Plain Text…'
	." |  bash=\"".$argv[0]."\" param1=make_plaintext refresh=false terminal=false"
	."\n";

echo
	'Strip XML…'
	." |  bash=\"".$argv[0]."\" param1=remove_xml refresh=false terminal=false"
	."\n";

echo
	'Reverse…'
	." |  bash=\"".$argv[0]."\" param1=reverse refresh=false terminal=false"
	."\n";

echo "---\n";

echo "Online Help… | href=http:www.greatoceanmedia.com.au/bitbar/clipboard-bar\n";

?>
