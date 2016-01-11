#!/usr/bin/env perl
# <bitbar.title>Battery Apple Bluetooth keyboard</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Alexandre Espinosa Menor</bitbar.author>
# <bitbar.author.github>alexandregz</bitbar.author.github>
# <bitbar.desc>Show vagrant images running, from vagrant global-status command</bitbar.desc>
#
# command from https://github.com/matryer/bitbar-plugins/issues/84 by @keithamus
#

use strict;

my $output = `system_profiler SPBluetoothDataType`;

if($output =~ /Minor Type: Keyboard.*Battery Level: (\d+)/sm) {
        print "Keyboard: $1%";
}