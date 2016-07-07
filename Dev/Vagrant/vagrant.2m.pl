#!/usr/bin/perl
# <bitbar.title>Vagrant</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author.github>axeloz</bitbar.author.github>
# <bitbar.author>Axel</bitbar.author>
# <bitbar.desc>Vagrant status checker.</bitbar.desc>
# <bitbar.dependencies>perl,vagrant</bitbar.dependencies>
# <bitbar.image>https://i.imgur.com/Yzrcz9k.png</bitbar.image>

use strict;
use Cwd 'abs_path';

my @output;
my @found;
my $machinePath;
my $readablePath;
my $me = abs_path($0);
my $content;
my $vagrant;
my $running = 0;
my $total = 0;

# HACK as $PATH is incorrect when Bitbar run the script
# Must add /usr/local/bin manually
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

# When script is called with arguments
# $ARGV[0] : the action (up, halt, suspend, resume, ssh)
# $ARGV[1] : the path of the Vagrant environment
# $ARGV[2] : the ID of the VM
if ( ($#ARGV + 1) == 3) {

	my $title = "Vagrant machine #$ARGV[2]";
	my $description = "";
	my $newstatus = "unknown";

	# Running the SSH action
	if ($ARGV[0] eq 'ssh') {
		$ARGV[1] =~ s/\\/\\\\/g; # extra escape; escape from osascript and do script
		&osascript ('
			tell application "Terminal"
				if (count of windows) is 0 then reopen
			  activate
			  do script "cd '.$ARGV[1].' && vagrant ssh"
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

# Checking whether there is at least one VM
# TODO: clean this
foreach $a (@output) {
	if ($a =~ "There are no active") {
		print "⚠️\n";
		print "---\n";
		print "There is no Vagrant VM yet.";
		exit 1;		
	}
}


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

	$machinePath  = join("\\ ", @found[4..$#found]);
	$readablePath = join(" ", @found[4..$#found]);

	# This VM is currently running
	if ($found[3] eq 'running') {
		# Counting the running VMs
		$running ++;

		$content .= "✅ Machine #$found[0] is running | size=14 color=green\n";
		$content .= " $readablePath | size=11 \n";
		$content .= "  | size=14 color=black \n";

		$content .= "#️⃣ SSH $found[0] | size=12 bash=\"$me\" param1=ssh param2=\"".$machinePath."\" param3=\"".$found[0]."\" terminal=false refresh=false \n";
		$content .= "🔄 Reload $found[0] | size=12 bash=\"$me\" param1=reload param2=\"".$machinePath."\" param3=\"".$found[0]."\" terminal=false refresh=true \n";
		$content .= "🔽 Suspend $found[0] | size=12 bash=\"$me\" param1=suspend param2=\"".$machinePath."\" param3=\"".$found[0]."\" terminal=false refresh=true \n";
		$content .= "⏬ Stop $found[0] | size=12 bash=\"$me\" param1=halt param2=\"".$machinePath."\" param3=\"".$found[0]."\" terminal=false refresh=true \n";
	}
	# This VM is currently saved
	elsif ($found[3] eq 'saved' || $found[3] eq 'suspended') {
		$content .= "📴 Machine #$found[0] is suspended | size=14 color=orange\n";
		$content .= " $readablePath | size=11 \n";
		$content .= "  | size=14 color=black \n";
		$content .= "▶️ Resume $found[0] | size=12 bash=\"$me\" param1=resume param2=\"".$machinePath."\" param3=\"".$found[0]."\" terminal=false refresh=true \n";
		$content .= "⏬ Stop $found[0] | size=12 bash=\"$me\" param1=halt param2=\"".$machinePath."\" param3=\"".$found[0]."\" terminal=false refresh=true \n";
	}
	# This VM is currently powered off
	elsif ($found[3] eq 'poweroff' || $found[3] eq 'aborted' || ($found[3] eq 'not' && $found[4] eq "running")) {
		if ($found[3] eq 'not' && $found[4] eq "running") {
			$machinePath  = join("\\ ", @found[5..$#found]);
			$readablePath = join(" ", @found[5..$#found]);
		}
		$content .= "🚫 Machine #$found[0] is stopped | size=14 color=red\n";
		$content .= " $readablePath | size=11 \n";
		$content .= "  | size=14 color=black \n";
		$content .= "▶️ Start $found[0] | size=12 bash=\"$me\" param1=up param2=\"".$machinePath."\" param3=\"".$found[0]."\" terminal=false refresh=true \n";
	}
	# This VM is in an unknown state
	else {
		$content .= "❓ Machine #$found[0] is ".$found[3]." | size=14 color=red\n";
		$content .= " $machinePath | size=11 \n";
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
