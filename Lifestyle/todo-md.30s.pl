#!/usr/bin/env perl

# Inspired from [Simple Todo Tracker](https://getbitbar.com/plugins/Lifestyle/todo.30s.sh)

use strict;
use warnings;

# Edit the path to the file describe your ToDo
my $file_path = "$ENV{HOME}/Dropbox/TODO.md";

# Select your editor
my $editor = "/Applications/Atom.app/Contents/Resources/app/atom.sh"; # Atom
# my $editor = "/Applications/TextMate.app/Contents/Resources/mate"; # TextMate
# my $editor = "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl"; # Sublime Text
# my $editor = "/Applications/CotEditor.app/Contents/SharedSupport/bin/cot"; # CotEditor
# my $editor = "/Applications/Emacs.app/Contents/MacOS/Emacs"; # Emacs

# Change here depending on your preference
my $font_color_finished = "#C0C0C0";
my $font_color_unfinieshed = "black";
my $menu_bar_icon = ":ballot_box_with_check:";

# <bitbar.title>Simple ToDo Tracker for Markdown</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Kenji Akiyama</bitbar.author>
# <bitbar.author.github>artifactsauce</bitbar.author.github>
# <bitbar.desc>Tracking ToDo list described in Markdown</bitbar.desc>
# <bitbar.image>http://i.imgur.com/nIEVIdR.png</bitbar.image>
# <bitbar.dependencies>perl</bitbar.dependencies>

my $unfinished_task_cnt = 0;
my @tasks = ();

open my $fh, "<", $file_path or die $!;
while ( <$fh> ) {
    next unless $_ =~/^(?:[\-\*]|\d\.?) \[(.)\] (.+)$/;
    my $task = {
        checked => ($1 eq "x"),
        title => $2,
    };
    $unfinished_task_cnt++ unless $task->{checked};
    push @tasks, $task;
}
close $fh;

print "$menu_bar_icon:$unfinished_task_cnt\n";
print "---\n";

@tasks = sort { $a->{checked} <=> $b->{checked} } @tasks;
for (my $i = 0; $i <= $#tasks; $i++) {
    my $font_color = $tasks[$i]{checked} ? $font_color_finished : $font_color_unfinieshed;
    printf "%d. %s | color=$font_color\n", $i+1, $tasks[$i]{title}
}

print "---\n";
print "Open file | bash=$editor param1=$file_path terminal=false\n";
# print "Refresh | refresh=true\n"; # enable to refresh manually
