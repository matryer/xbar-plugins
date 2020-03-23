#!/usr/bin/env php
<?php

# <bitbar.title>Password Generator</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Adi</bitbar.author>
# <bitbar.author.github>gomedia-adi</bitbar.author.github>
# <bitbar.desc>Generates human-friendly strong passwords.</bitbar.desc>
# <bitbar.image>http://www.greatoceanmedia.com.au/images/63.png</bitbar.image>
# <bitbar.dependencies>php</bitbar.dependencies>
# <bitbar.abouturl>http://www.greatoceanmedia.com.au/bitbar</bitbar.abouturl>

/*
	BitBar plugin help: https:github.com/matryer/bitbar

	Plugin updated 23/3/20

	Version history
		1.0	- initial release
*/

/* TODO
	- format massaging/validation?
*/

define('CONFIG_FILE', "/var/tmp/bitbar.password_bar.config.php");
define('DEFAULT_FORMAT', "3,n,5,p,4");
define('PWD_PUNC', "!\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~");

function update_config($format) {
// rewrite config file with given values
	file_put_contents(CONFIG_FILE, "<?php\n");
	file_put_contents(CONFIG_FILE, "// PASSWORD_BAR CONFIGURATION\n", FILE_APPEND);
	file_put_contents(CONFIG_FILE, "\$format = '$format';\n", FILE_APPEND);
	file_put_contents(CONFIG_FILE, "?>\n", FILE_APPEND);
}

function generate_pwd($format) {
// generate password using format
	global $words1, $words2, $words3, $words4, $words5, $words6, $words7, $words8, $words9; // make word lists available between function calls

	$password = '';
	$parts = explode(',', $format);
	foreach ($parts as $part) {
		// random single digit
		if ($part == 'n')
			$password .= rand(1,9);
		// random punctuation character (from predefined list)
		if ($part == 'p') {
			$punc = PWD_PUNC;
			$random_punc = $punc[rand(0, strlen($punc)-1)];
			$password .= $random_punc;
		}
		// random word of n letters
		if (strpos('123456789', $part) !== FALSE) {
			$words_var = 'words'.$part;
			if (!isset($$words_var)) { // create n-letter word array for first time
				$num_char = (int)$part;
				$cmd = "grep -x '.\{".$num_char."\}' /usr/share/dict/words | grep -v '[[:upper:]]'";
				$$words_var = array();
				exec($cmd, $$words_var, $err);
			}
			$words = $$words_var;
			$rand_key = array_rand($words); // get random key
			$password .= ucfirst($words[$rand_key]); // get random Word
		}
	}
	return $password;
}

//-------------------------------------------------------------------------------------------------

//??? CONFIG FILE

// initialise config file
if (!file_exists(CONFIG_FILE))
	touch(CONFIG_FILE);

// read config file
include(CONFIG_FILE);

// provide default
if (!isset($format) || !$format) $format = DEFAULT_FORMAT;

//??? MENU ACTIONS

// menu option selected
if (isset($argv[1])) {
	// prompt for password format
	if ($argv[1] == "set_format") {
		$cmd = "osascript -e 'set theString to text returned of (display dialog \"Password format \" default answer \"".$format."\" buttons {\"Cancel\", \"Save\"} default button 2)'";
		exec($cmd, $out, $err);
		$new_format = implode('', $out); // text entered by user
		if (!$err) { // something entered
			$format = ($new_format ? $new_format : DEFAULT_FORMAT); // reset to default if blank format entered
			update_config($format);
		}
	}
	// copy password to clipboard
	if ($argv[1] == "copy_pwd") {
		$cmd = "echo '".$argv[2]."' | base64 --decode | pbcopy"; // decode & copy to clipboard
		exec($cmd, $out, $err);
		exit;
	}
	// help dialog - some special chars used below to avoid CLI interpretation: backslash=⧵ (U+29F5); apostrophe=＇(U+FF07), double quote=″ (U+2033)
	if ($argv[1] == "help") {
		$cmd = "osascript -e 'display dialog \"Password format is a comma-separated set of components:

	1-9	- random word with the specified number of letters
		  (e.g. 3 becomes a three letter Capitalised word)
	n	- random single digit number
	p	- random punctuation character
		  (taken from !″#$%&＇()*+,-./:;<=>?@[⧵]^_`{|}~)

Default format:  3,n,5,p,4
- this generates a password containing a 3 letter word, a digit, a 5 letter word, a punctuation character, and a 4 letter word
(e.g. Tig5Pesky?Muff)\" buttons {\"OK\"} default button 1'";
		exec($cmd, $out, $err);
		exit;
	}
}

//??? MENUBAR

// display icon - 32x32 png, 144 DPI, base64 encoded using "openssl base64 -A -in icon.png" (recommended 36px dimension too big) & prepend "|image="
echo
"|image=iVBORw0KGgoAAAANSUhEUgAAAB4AAAAgCAYAAAAFQMh/AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4wUBBy0oPIjPCQAAAcRJREFUSMe9l79LXEEQx7+fd4WSw1JTKgbLgEECuRM1MRwYwc7eIqL5C/IPxB9NGkEUSZHOWkhxfWJIHUhhIcTCKiIGgnDccW9t3oXj2PXdzb5kYHmwOzufnf0xM0+KEKDzfQi8AerAJXADnAEfgWUVKV3QTcDltDYw1T0vFlrvA9rd1qK8TZJEwPGA0E6bB4TR4wlJP43rvnLOjZWM4K+SRj1Dt5LeSzqQ9EDSpKSkR6cMfLNAxwJbeOHRfRzQPbeAZzyGUmAyoL/rgyeGnS57+tqSrn2XUNKJ94IawC2fY5KGMtBfSdNUksaLChyPAuf2LvDW/3h0m1Z46I0eAiOZzjTwPaC3YwVvG4NHx1tT/HgaAXXAkQX6MhJat0BrWZaxQveBZNDsVLnHYAvYyvKvb+wz8MKSEp/lePK82yDwBFgEot5vLQdavW9yqWTKQVrKOdOK/oHM5pQxC/8b6oCFqLopkCReAad5N9w5V6iny1le7edNvi0KOmcICK9joZWIaFQ1nzHwIWLRTfNMoGH0djW2ougtzBvApxzoehGlzBDwIytRfgPTWf+XAHSj6J+wYSDp6TspdHsHXNAe8AtYKdLuHajxM/2P7HLwAAAAAElFTkSuQmCC\n";

//??? TOP-LEVEL MENU

// generate a number of passwords
echo "---\n";
echo "Click to Copy:\n";
foreach (range(1, 10) as $i) {
	$pwd = generate_pwd($format);				// generate password
 	$pwd_nobar = str_replace('|', '｜', $pwd);	// "|" interpreted by bitbar, so substitute with a vertical line (U+FF5C)
	$pwd_enc = base64_encode($pwd);				// encode password to hide unix special characters from CLI
	echo $pwd_nobar." | terminal=false bash=\"".$argv[0]."\" param1=copy_pwd param2=".$pwd_enc." refresh=false terminal=false"."\n";
}

// regenerate passwords (i.e. refresh)
echo "---\n";
echo
	'Regenerate'
	." | terminal=false bash=\"".$argv[0]."\" refresh=true terminal=false"
	."\n"
;

// display current format
echo "---\n";
echo
	"Format: $format\n"
;

// set new password format
echo
	'Set Format...'
	." |  bash=\"".$argv[0]."\" param1=set_format refresh=true terminal=false"
	."\n"
;

// display help dialog
echo
	'Help'
	." |  bash=\"".$argv[0]."\" param1=help terminal=false"
	."\n"
;

?>
