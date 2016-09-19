#!/usr/bin/perl -w

# <bitbar.title>Logitech Media Server Status Display</bitbar.title>
# <bitbar.version>v1.2.0</bitbar.version>
# <bitbar.author>Michael Herger</bitbar.author>
# <bitbar.author.github>michaelherger</bitbar.author.github>
# <bitbar.desc>Show whether Logitech media Server is running or not. Quickly start/stop it, access its UI etc.</bitbar.desc>
# <bitbar.image>http://www.herger.net/slim/BitBar/lms-bitbar.png</bitbar.image>
# <bitbar.dependencies>Logitech Media Server</bitbar.dependencies>

use strict;

BEGIN {
	if (!$ENV{BitBar}) {
		require Data::Dump;
	}
}

use constant STRINGS => sub {
	my $lang = uc(substr(`/usr/bin/defaults read -g AppleLocale` || 'EN', 0, 2));

	my $localizations = {
		NOT_FOUND => {
			DE => 'Logitech Media Server wurde nicht gefunden.',
			EN => 'Logitech Media Server can\'t be found'
		},
		LMS_RUNNING => {
			DE => 'Logitech Media Server wird ausgeführt',
			EN => 'Logitech Media Server is running'
		},
		LMS_NOT_RUNNING => {
			DE => 'Logitech Media Server läuft nicht',
			EN => 'Logitech Media Server is not running'
		},
		LMS_IS_STARTING => {
			DE => 'Logitech Media Server startet…',
			EN => 'Logitech Media Server is starting…'
		},
		START_LMS => {
			DE => 'Logitech Media Server starten',
			EN => 'Start Logitech Media Server'
		},
		STOP_LMS => {
			DE => 'Logitech Media Server anhalten',
			EN => 'Stop Logitech Media Server'
		},
		OPEN_WEB_CONTROL => {
			DE => 'Web-Steuerung öffnen…',
			EN => 'Open Web Control…'
		},
		OPEN_SETTINGS => {
			DE => 'Einstellungen öffnen…',
			EN => 'Open Settings…'
		},
		OPEN_PREF_PANE => {
			DE => 'Systemeinstellungen öffnen…',
			EN => 'Open Preference Pane…'
		},
		UPDATE_AVAILABLE => {
			DE => 'Software aktualisieren…',
			EN => 'Install software update…'
		}
	};

	return { map {
		$_ => ($localizations->{$_}->{$lang} || $localizations->{$_}->{EN})
	} keys %$localizations };
}->();

use constant PREFPANE_LINK => STRINGS->{'OPEN_PREF_PANE'} . '|bash=/usr/bin/open param1=/Library/PreferencePanes/Squeezebox.prefPane terminal=false';

use constant LMS_ON => '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';
use constant LMS_OFF => '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';

my $status = LMS_OFF;

my $INSTALLATION_FOLDER = '/Library/PreferencePanes/Squeezebox.prefPane/Contents/Resources';

my $items;

if ( !-d $INSTALLATION_FOLDER ) {
	$items = STRINGS->{'NOT_FOUND'} . '|color=#ff0000';
}
else {
	my $port = `$INSTALLATION_FOLDER/check-web.pl`;
	$port *= 1;
	
	if ( $port ) {
		$items = sprintf(
 			"%s\n%s| href=http://localhost:$port\n%s| href=http://localhost:$port/settings/index.html\n---\n%s|bash=$INSTALLATION_FOLDER/stop-server.sh terminal=false refresh=true", 
			STRINGS->{LMS_RUNNING},
			STRINGS->{OPEN_WEB_CONTROL},
			STRINGS->{OPEN_SETTINGS},
			STRINGS->{STOP_LMS}
		);
			
		$status = LMS_ON;
	}
	else {
		my $server = `$INSTALLATION_FOLDER/get-server.sh`;
 		$server =~ s/\s*//sig;

		if ( $server ) {
			$items = STRINGS->{LMS_IS_STARTING};
		}
		else {
			$items = sprintf(
				"%s\n%s|bash=$INSTALLATION_FOLDER/start-server.sh terminal=false refresh=true", 
				STRINGS->{LMS_NOT_RUNNING}, 
				STRINGS->{START_LMS}
			);
		}
	}
		
	$items .= "\n---\n" . PREFPANE_LINK;

	my $versionFile = $ENV{"HOME"} . '/Library/Caches/Squeezebox/updates/server.version';
	my $installer = '';

	open(UPDATEFLAG, $versionFile) && do {
		local $_;
		while ( <UPDATEFLAG> ) {

			chomp;

			if (/(?:LogitechMediaServer|Squeezebox|SqueezeCenter).*/i) {
				$installer = $_;
				last;
			}
		}
		
		close UPDATEFLAG;
	};	
	
	if ($installer) {
		$items .= "\n" . STRINGS->{UPDATE_AVAILABLE} . "|color=green terminal=false bash=/usr/bin/open param1=$installer";
		print '⇧';
	}
}

print qq(|image=$status
---
$items
);


1;