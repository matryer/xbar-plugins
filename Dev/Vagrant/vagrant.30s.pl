#!/usr/bin/env perl
# <bitbar.title>Vagrant Global Status</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Alexandre Espinosa Menor</bitbar.author>
# <bitbar.author.github>alexandregz</bitbar.author.github>
# <bitbar.desc>Show vagrant images running, from vagrant global-status command</bitbar.desc>
# <bitbar.image>http://i.imgur.com/4YAFZC6.png</bitbar.desc>
#
# "running" line run command "vagrant suspend"
# "saved, poweroff, aborted" line run command "vagrant up"
#

use strict;

$ENV{'PATH'} = $ENV{'PATH'}.':/usr/local/bin'; 

# action => [status1 from machine, status2, ...]
my $actions_from_status = {
        up => ["saved", "poweroff", "aborted"],
        suspend => ["running"]
};

my $status = `vagrant global-status`;

if($status =~ /^\-{10,}\n(.*)\n\s+\n/sm) {
        my @images = split(/\n/, $1);

        print "🇻"; print "(".scalar(@images).")" if(@images);
        print "\n";
        print "---\n";

        foreach my $i(@images) {
                my @data_image = split(/\s+/, $i);
                my $i_id = $data_image[0];
                my $i_image = $data_image[1];
                my $i_provider = $data_image[2];
                my $i_status = $data_image[3];
                my $i_path = $data_image[4];

                my $color = "black";
                $color = "green" if($i_status eq 'running');
                $color = "red" if($i_status eq 'saved');

                print "$i_id - $i_image ($i_provider) | color=black\n";
                print "   $i_path\n";

                print "   $i_status | color=$color ";
                foreach my $action(keys(%{$actions_from_status})) {
                        print " | bash=vagrant param1=$action param2=$i_id " if(grep $_ eq $i_status, @{$actions_from_status->{$action}});
                }
                print "\n";
                print "---\n";
        }
}
else{
        #print "Without Vagrant images running?";
        print "🇻(0)";
}

