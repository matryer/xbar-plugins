#!/usr/bin/perl -w
# <bitbar.title>Gmail Checker Improved</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author.github>axeloz</bitbar.author.github>
# <bitbar.author>Axel</bitbar.author>
# <bitbar.desc>Gmail unread emails checker.</bitbar.desc>
# <bitbar.dependencies>perl,gmail</bitbar.dependencies>
# <bitbar.image>https://i.imgur.com/gYYJB7U.png</bitbar.image>

use strict;
use LWP::Simple;
use LWP::UserAgent;
use Mozilla::CA;
use XML::Simple;
use Encode;
use POSIX;

my $url = 'https://mail.google.com/mail/feed/atom';
my $user = '<username>';
my $password = '<password>';
my $output;

my $ua = LWP::UserAgent->new;
$ua->credentials('mail.google.com:443', 'mail.google.com', $user, $password);

my $content = $ua->get($url);
die "Couldn't get $url" unless defined $content;

sub clevercut {
	my ($txt, $max) = @_;
	
	if (length($txt) > $max) {
		return (substr $txt, 0, $max ).' [...]';
	}
	return $txt;
}


if ($content->status_line eq '200 OK') {
	my $xs = XML::Simple->new();
	my $ref = XMLin($content->content, ForceArray=>'entry');

	if ($ref->{fullcount}->[0] > 0) {
		$output .= "📬 ".$ref->{fullcount}->[0]."\n";
		$output .= "---\n";
		
		if ($ref->{fullcount}->[0] == 1) {
			$output .= "You have 1 new message | size=16 \n";
		}
		else {
			$output .= "You have ".$ref->{fullcount}->[0]." new messages | size=16 \n";
		}
		$output .= "  | size=14 color=black \n";
		
		foreach my $email (@{ $ref->{entry} }) {
			$output .= "» ".clevercut(encode("utf8", $email->{title}->[0]), 30)." | href=".$email->{link}->[0]->{href}."\n";
		}
		
	}
	else {
		$output .= "📪\n";
		$output .= "---\n";
		$output .= "No new message | size=16 \n";
	}
}
$output .= "---\n";
$output .= "➤ Open Mailbox |href=https://www.gmail.com\n";
print $output."\n";
