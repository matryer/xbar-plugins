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
my $total = 0;
my $path = $ENV{PATH}.':/usr/local/bin';

# This function allows me to run Apple Scripts
sub osascript($) { system 'osascript', map { ('-e', $_) } split(/\n/, $_[0]); }




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
# $ARGV[2] : the ID of the VM
if ( ($#ARGV + 1) == 3) {

	my $title = "Vagrant machine #$ARGV[2]";
	my $description = "";
	my $newstatus = "unknown";

	# Running the SSH action
	if ($ARGV[0] eq 'ssh') {
		
		&osascript ('
			tell application "Terminal"
				if (count of windows) is 0 then reopen
			  activate
			  do script "cd '.$ARGV[1].' && vagrant ssh" in window 1
			end tell
		');
		$description = "You are now connected to your Vagrant machine";
	}
	else {
		if ($ARGV[0] eq "up" || $ARGV[0] eq "resume") {
			$newstatus = "running";
		}
		elsif  ($ARGV[0] eq "halt" || $ARGV[0] eq "suspend") {
			$newstatus = "stopped";
		}
		elsif ($ARGV[0] eq "reload") {
			$newstatus = "reloaded";
		}

		system("export PATH=$path && cd $ARGV[1] && $vagrant $ARGV[0]");
		$description = "Vagrant virtual machine status is now ".$newstatus;
	}
	
	# Checking the result of the action
	if ($? eq 0) {
		&osascript (
		 'display notification "'.$description.'" with title "'.$title.'"'
		);
		exit 0;
	}
	else {
		&osascript (
		 'display notification "Could not execute operation" with title "'.$title.'"'
		);
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
	
	# Counting total
	$total ++;

	# Exploding row on spaces
	@found = split / /, $a;

	# This VM is currently running
	if ($found[3] eq 'running') {
		# Counting the running VMs
		$running ++;

		$content .= "✅ Machine $found[0] is running | size=14 color=green\n";
		$content .= " $found[4] | size=11 \n";
		$content .= "  | size=14 color=black \n";
		
		$content .= "#️⃣ SSH $found[0] | size=12 bash=\"$me\" param1=ssh param2=\"".$found[4]."\" param3=\"".$found[0]."\" terminal=false refresh=false \n";
		$content .= "🔄 Reload $found[0] | size=12 bash=\"$me\" param1=reload param2=\"".$found[4]."\" param3=\"".$found[0]."\" terminal=false refresh=true \n";
		$content .= "🔽 Suspend $found[0] | size=12 bash=\"$me\" param1=suspend param2=\"".$found[4]."\" param3=\"".$found[0]."\" terminal=false refresh=true \n";
		$content .= "⏬ Stop $found[0] | size=12 bash=\"$me\" param1=halt param2=\"".$found[4]."\" param3=\"".$found[0]."\" terminal=false refresh=true \n";
	}
	# This VM is currently saved
	elsif ($found[3] eq 'saved') {
		$content .= "📴 Machine $found[0] is suspended | size=14 color=orange\n";
		$content .= " $found[4] | size=11 \n";
		$content .= "  | size=14 color=black \n";
		$content .= "▶️ Resume $found[0] | size=12 bash=\"$me\" param1=resume param2=\"".$found[4]."\" param3=\"".$found[0]."\" terminal=false refresh=true \n";
		$content .= "⏬ Stop $found[0] | size=12 bash=\"$me\" param1=halt param2=\"".$found[4]."\" param3=\"".$found[0]."\" terminal=false refresh=true \n";
	}
	# This VM is currently powered off
	elsif ($found[3] eq 'poweroff') {
		$content .= "🚫 Machine $found[0] is stopped | size=14 color=red\n";
		$content .= " $found[4] | size=11 \n";
		$content .= "  | size=14 color=black \n";
		$content .= "▶️ Start $found[0] | size=12 bash=\"$me\" param1=up param2=\"".$found[4]."\" param3=\"".$found[0]."\" terminal=false refresh=true \n";
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
print "V [$running/$total]\n";
print "---\n";
print $content unless !defined $content;
exit 0;
