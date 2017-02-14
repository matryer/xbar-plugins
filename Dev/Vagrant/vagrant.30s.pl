#!/usr/bin/env perl
# <bitbar.title>Vagrant Global Status</bitbar.title>
# <bitbar.version>1.1</bitbar.version>
# <bitbar.author>Alexandre Espinosa Menor</bitbar.author>
# <bitbar.author.github>alexandregz</bitbar.author.github>
# <bitbar.desc>Show vagrant images running, from vagrant global-status command</bitbar.desc>
# <bitbar.image>http://i.imgur.com/4YAFZC6.png</bitbar.image>
#
# "running" line run command "vagrant suspend"
# "saved, poweroff, aborted" line run command "vagrant up"
#

use strict;

$ENV{'PATH'} = $ENV{'PATH'}.':/usr/local/bin'; 

if ($#ARGV >= 1) {
        exit exec_sub_command(@ARGV);
}

# action => [status1 from machine, status2, ...]
my $actions_from_status = {
        up => ["saved", "poweroff", "aborted"],
        suspend => ["running"],
        resume => ["suspended"]
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
                print "   $i_path | bash=$0 param1=path_copy param2=$i_path color=gray trim=false terminal=false\n";

                print "$i_status | color=$color ";
                foreach my $action(keys(%{$actions_from_status})) {
                        print "bash=vagrant param1=$action param2=$i_id terminal=true" if(grep $_ eq $i_status, @{$actions_from_status->{$action}});
                }
                print "\n";
                print "---\n";
        }
}
else{
        #print "Without Vagrant images running?";
        print "🇻(0)";
}

sub exec_sub_command {
        my ($sub_command, @args) = @_;
        my $sub_command_methods = {
                'path_copy' => \&sub_command_path_copy,
        };
        if (!defined $sub_command_methods->{$sub_command}) {
                die "Undefined sub command: $sub_command";
        }
        return $sub_command_methods->{$sub_command}(@args);
}

sub sub_command_path_copy {
        my $path = shift;
        open my $fh, '|/usr/bin/pbcopy' or die $!;
        print $fh $path;
        close $fh;
        return;
}
