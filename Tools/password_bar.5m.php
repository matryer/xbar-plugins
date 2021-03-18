#!/usr/bin/env php
<?php

# <bitbar.title>Password Generator</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Adi</bitbar.author>
# <bitbar.author.github>gomedia-adi</bitbar.author.github>
# <bitbar.desc>Generates human-friendly strong passwords.</bitbar.desc>
# <bitbar.image>http://www.greatoceanmedia.com.au/images/63.png</bitbar.image>
# <bitbar.dependencies>php</bitbar.dependencies>
# <bitbar.abouturl>http://www.greatoceanmedia.com.au/bitbar/password-bar</bitbar.abouturl>

/*
	BitBar plugin help: https:github.com/matryer/bitbar

	Plugin updated 22/6/20

	Version history
		1.0	- also now generates a single obscure password (i.e. one that doesn't contain words)
			- length of obscure password user definable
			- changed format element "p" to "s" (coz they're symbols more than punctuation)
			- option to select subset of symbols
			- Dark Mode compatible menubar icon
		0.1	- initial release

	Credits
		- plugin inspired by a Mac app called Arcana by Tekuris
		- thanks to Christian S for v1.0 improvement suggestions
*/

/* TODO
	- format massaging/validation?
*/

define('CONFIG_FILE', "/var/tmp/bitbar.password_bar.config.php");
define('DEFAULT_FORMAT', "3,n,5,s,4");
define('FULL_SYM', "!\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~");
define('PARTIAL_SYM', "!#$%&*+,-.:;=?@^_|~"); // without the quotes, brackets, slashes
define('LIMITED_SYM', "!#$%*+-=@^_"); // the supposed eBay subset
define('ALPHA_NUM', "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789");
define('DEFAULT_LENGTH', 14);

function update_config($format, $symbols, $length) {
// rewrite config file with given values

	file_put_contents(CONFIG_FILE, "<?php\n");
	file_put_contents(CONFIG_FILE, "// PASSWORD_BAR CONFIGURATION\n", FILE_APPEND);
	file_put_contents(CONFIG_FILE, "\$format = '$format';\n", FILE_APPEND);
	file_put_contents(CONFIG_FILE, "\$symbols = '$symbols';\n", FILE_APPEND);
	file_put_contents(CONFIG_FILE, "\$length = '$length';\n", FILE_APPEND);
	file_put_contents(CONFIG_FILE, "?>\n", FILE_APPEND);
}

function generate_pwd($format, $symbols) {
// generate password using format
	global $words1, $words2, $words3, $words4, $words5, $words6, $words7, $words8, $words9; // make word lists available between function calls

	$password = '';

	$symbol = FULL_SYM;
	if ($symbols == 'partial') $symbol = PARTIAL_SYM;
	if ($symbols == 'limited') $symbol = LIMITED_SYM;

	$parts = explode(',', $format);
	foreach ($parts as $part) {
		// random single digit
		if ($part == 'n')
			$password .= rand(1,9);
		// random symbol character (from predefined list)
		if (($part == 'p') || ($part == 's')) {
			$random_symbol = $symbol[rand(0, strlen($symbol)-1)];
			$password .= $random_symbol;
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

function generate_obscure_pwd($length, $symbols) {
// generate an obscure password

	$password = array();

	$string = ALPHA_NUM.FULL_SYM;
	if ($symbols == 'partial') $string = ALPHA_NUM.PARTIAL_SYM;
	if ($symbols == 'limited') $string = ALPHA_NUM.LIMITED_SYM;

	$chars = str_split($string); // convert string to array
	shuffle($chars); // randomise chars
	$rand_keys = array_rand($chars, $length); // randomly pick array keys
	foreach ($rand_keys as $key) // create password from alpha/num/symbols array using random keys
		$password[] = $chars[$key];
	// rotate password chars until begins with alpha
	while (!ctype_alpha($password[0]))
		array_push($password, array_shift($password));

	return implode('', $password); // return string
}

//-------------------------------------------------------------------------------------------------

//??? CONFIG FILE

// initialise config file
if (!file_exists(CONFIG_FILE))
	touch(CONFIG_FILE);

// read config file
include(CONFIG_FILE);

// provide defaults
if (!isset($format) || $format == '') $format = DEFAULT_FORMAT;
if (!isset($symbols) || $symbols == '' || !in_array($symbols, array('full', 'partial', 'limited'))) $symbols = 'partial';
if (!isset($length) || !$length) $length = '14';

//??? MENU ACTIONS

// menu option selected
if (isset($argv[1])) {
	// prompt for readable password format & save
	if ($argv[1] == "set_format") {
		$cmd = "osascript -e 'set theString to text returned of (display dialog \"Readable Password Format \" default answer \"".$format."\" buttons {\"Cancel\", \"Save\"} default button 2)'";
		exec($cmd, $out, $err);
		if (!$err) { // something entered
			$str = implode('', $out); // text entered by user
			$format = ($str ? $str : DEFAULT_FORMAT); // reset to default if blank or zero
			update_config($format, $symbols, $length);
		}
	}
	// prompt for obscure password length & save
	if ($argv[1] == "set_length") {
		$cmd = "osascript -e 'set theString to text returned of (display dialog \"Obscure Password Length \" default answer \"".$length."\" buttons {\"Cancel\", \"Save\"} default button 2)'";
		exec($cmd, $out, $err);
		if (!$err) { // something entered
			$str = implode('', $out); // text entered by user
			$length = (int)$str; // convert str to int (will become zero if not int)
			$length = ($length ? $length : DEFAULT_LENGTH); // reset to default if blank or zero
			update_config($format, $symbols, $length);
		}
	}
	// save symbol subset
	if ($argv[1] == "set_symbols")
		update_config($format, $argv[2], $length);
	// copy password to clipboard
	if ($argv[1] == "copy_pwd") {
		$cmd = "echo '".$argv[2]."' | base64 --decode | pbcopy"; // decode & copy to clipboard
		exec($cmd, $out, $err);
	}
	// help dialog - some special chars used below to avoid CLI interpretation: backslash=⧵ (U+29F5); apostrophe=＇(U+FF07), double quote=″ (U+2033)
	if ($argv[1] == "help") {
		$cmd = "osascript -e 'display dialog \"Readable Passwords Format
	1-9	- random word with the specified number of letters
		  (e.g. 3 becomes a three letter Capitalised word)
	n	- random single digit number
	s	- random symbol character (see below)

	Default format:  3,n,5,s,4
	- generates a password containing a 3 letter word,
	a digit, a 5 letter word, a symbol character,
	and finally a 4 letter word (e.g. Tig5Pesky?Muff)

Obscure Password
	This is a completely randomised string of letters,
	digits and symbols. Set the length from the menu
	(default = 14).

Symbol Sets
	Full:		!″#$%&＇()*+,-./:;<=>?@[⧵]^_`{|}~
	Partial:	!#$%&*+,-.:;=?@^_|~
	Limited:	!#$%*+-=@^_
\" buttons {\"OK\"} default button 1'";
		exec($cmd, $out, $err);
		exit;
	}
}

//??? MENUBAR

// display icon: 36x36 png (32x32max image + 2px transparent border), 144 dpi, colour mode grey, encoded using "base64 icon.png" ... prepend "|templateImage=" & append "\n"
echo "|templateImage=iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAQAAABLCVATAAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAAFiUAABYlAUlSJPAAAAAHdElNRQfkBhUWCih+dz6LAAABaUlEQVRIx9XWv0oDQRAG8PnSRBBbLRXFUoiI4EX8TyQG7CzsLBT1CXwBjdaCIGJhZx0QuV7zAoJCihQWVgFRiEW4eFmL6GWyenuzi01um+Tj9pfN7jB3RD17YQj78PGCN1RwhYIrsweljRDj9oz/i2mPLTvmOoZRUJiXMyMGRqEmh57YtA8UsYlbfLJsVcYMsinPUTrB0qoMmoomtDDK8pMO1U5SCVJ/9CmkV5aX9BuToGZnEZRm+bBtBY2x3ThkeT1KAynFD/scA0TI4IFlx1KoaKyjgCBzpo2MwoWMWUlgfBmTQ2hkzpCSMF7XpCaOUGHf7rAkW82M9uuL31s/iWVY1FBOY7Ju7TCv7Y3nxsxqDXXhPxglZ7oPcA1l/excVlNA649qObBl5mILb9uG8Yw1nBXvES6NdwXyBtYwrGfD5SHYwI3G7Ni11DQeUcc7MkS4Z8yuy4tC309bQMnhT8Wgp6hhnXr6+gJZx0AEmEK/dgAAAABJRU5ErkJggg==\n";

//??? TOP-LEVEL MENU

// display readable human-friendly passwords
echo "---\n";
echo "Click to Copy:\n";
foreach (range(1, 10) as $i) {
	$pwd = generate_pwd($format, $symbols);		// generate password
 	$pwd_nobar = str_replace('|', '｜', $pwd);	// "|" interpreted by bitbar, so substitute with a vertical line (U+FF5C)
	$pwd_enc = base64_encode($pwd);				// encode password to hide unix special characters from CLI
	echo $pwd_nobar." | terminal=false bash=\"".$argv[0]."\" param1=copy_pwd param2=".$pwd_enc." refresh=false terminal=false"."\n";
}

// display an obscure password
echo "…\n";
$pwd = generate_obscure_pwd($length, $symbols);
$pwd_nobar = str_replace('|', '｜', $pwd);	// "|" interpreted by bitbar, so substitute with a vertical line (U+FF5C)
$pwd_enc = base64_encode($pwd);				// encode password to hide unix special characters from CLI
echo $pwd_nobar." | terminal=false bash=\"".$argv[0]."\" param1=copy_pwd param2=".$pwd_enc." refresh=false terminal=false"."\n";

// regenerate passwords (i.e. refresh)
echo "---\n";
echo
	'Regenerate'
	." | terminal=false bash=\"".$argv[0]."\" refresh=true terminal=false"
	."\n"
;

// display current settings
echo "---\n";
echo "Readable Format: $format\n";
echo "Obscure Length: $length\n";
echo "---\n";

// set readable password format
echo
	'Set Readable Format…'
	." |  bash=\"".$argv[0]."\" param1=set_format refresh=true terminal=false"
	."\n"
;

// set obscure password length
echo
	'Set Obscure Length…'
	." |  bash=\"".$argv[0]."\" param1=set_length refresh=true terminal=false"
	."\n"
;

// symbols subset options
echo "Symbol Sets\n";
foreach (array('full', 'partial', 'limited') as $this_symbol_set)
	echo
		'--'
		.ucfirst($this_symbol_set)
		.($this_symbol_set == $symbols ? ' ✓' : '')
		." | terminal=false bash=\"".$argv[0]."\" param1=set_symbols param2=\"$this_symbol_set\" refresh=true"
		."\n"
	;

// display help dialog
echo
	'Help'
	." |  bash=\"".$argv[0]."\" param1=help terminal=false"
	."\n"
;

?>
