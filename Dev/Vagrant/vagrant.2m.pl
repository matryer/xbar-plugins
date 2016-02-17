#!/usr/bin/perl -w
# <bitbar.title>Vagrant</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author.github>axeloz</bitbar.author.github>
# <bitbar.author>Axel</bitbar.author>
# <bitbar.desc>Vagrant status checker.</bitbar.desc>
#<bitbar.dependencies>perl,vagrant</bitbar.dependencies>

use strict;
use Cwd 'abs_path';

my @output;
my @found;
my $me = abs_path($0);
my $content;
my $vagrant;
my $running = 0;
my $path = $ENV{PATH}.':/usr/local/bin';

# Locating the Vagrant binary
foreach $a (split(/:/, $path)) {
	if (-x $a."/vagrant") {
		$vagrant = $a."/vagrant";
		last;
	}
}

# If Vagrant could not be found
if (! defined $vagrant) {
	print "⚠️\n";
	print "---\n";
	print "Vagrant binary could not be found in paths. Is it installed?";
	exit 1;
}

# When script is call with 2 arguments
# $ARGV[0] : the action (up, halt, suspend, resume)
# $ARGV[1] : the path of the Vagrant environment
if ( ($#ARGV + 1) == 2) {

	# Running the action
	system("export PATH=$path && cd $ARGV[1] && $vagrant $ARGV[0]");
	# Checking the result of the action
	if ($? eq 0) {
		print "OK\n";
		exit 0;
	}
	else {
		print "An error occurred\n";
		exit 1;
	}
	# Not needed, just safer...
	exit 0
}

# Getting the list of all Vagrant VMs
@output = `$vagrant global-status |tail -n +3`;

# Looping in the list
foreach $a (@output) {
	# Triming spaces
	$a =~ s/^\s+|\s+$//g;
	# Removing excessive spaces
	$a =~ s/ {1,}/ /g;

	# Cutting output on first empty line as Vagrant is too verbose
	last if ($a eq '');

	# Exploding row on spaces
	@found = split / /, $a;

	# This VM is currently running
	if ($found[3] eq 'running') {
		# Counting the running VMs
		$running ++;

		$content .= "✅ Machine $found[0] is running | size=14 color=green\n";
		$content .= " $found[4] | size=11 \n";
		$content .= "  | size=14 color=black \n";
		$content .= "🔄 Reload $found[0] | size=12 bash=\"$me\" param1=reload param2=\"".$found[4]."\" terminal=false refresh=true \n";
		$content .= "🔽 Suspend $found[0] | size=12 bash=\"$me\" param1=suspend param2=\"".$found[4]."\" terminal=false refresh=true \n";
		$content .= "⏬ Stop $found[0] | size=12 bash=\"$me\" param1=halt param2=\"".$found[4]."\" terminal=false refresh=true \n";
	}
	# This VM is currently saved
	elsif ($found[3] eq 'saved') {
		$content .= "📴 Machine $found[0] is suspended | size=14 color=orange\n";
		$content .= " $found[4] | size=11 \n";
		$content .= "  | size=14 color=black \n";
		$content .= "▶️ Resume $found[0] | size=12 bash=\"$me\" param1=resume param2=\"".$found[4]."\" terminal=false refresh=true \n";
		$content .= "⏬ Stop $found[0] | size=12 bash=\"$me\" param1=halt param2=\"".$found[4]."\" terminal=false refresh=true \n";
	}
	# This VM is currently powered off
	elsif ($found[3] eq 'poweroff') {
		$content .= "🚫 Machine $found[0] is stopped | size=14 color=red\n";
		$content .= " $found[4] | size=11 \n";
		$content .= "  | size=14 color=black \n";
		$content .= "▶️ Start $found[0] | size=12 bash=\"$me\" param1=up param2=\"".$found[4]."\" terminal=false refresh=true \n";
	}
	# This VM is in an unknown state
	else {
		$content .= "❓ Machine $found[0] is ".$found[3]." | size=14 color=red\n";
		$content .= " $found[4] | size=11 \n";
		$content .= "  | size=14 color=black \n";
		$content .= "This is an unknown state\n";
	}

	# Adding the terminal separator
	$content .= "---\n";
}

# Adding the menu title with the number of running VMs
print "Vagrant [$running]\n";
print "---\n";
print $content unless !defined $content;
exit 0;
