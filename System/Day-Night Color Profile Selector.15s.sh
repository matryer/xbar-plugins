#!/bin/bash
#
# <bitbar.title>Day/Night Color Profile Selector</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Nadav G</bitbar.author>
# <bitbar.author.github>nadigo</bitbar.author.github>
# <bitbar.desc>Simple menubar day/night icc selector</bitbar.desc>
# <bitbar.dependencies>bash, customdisplayprofiles</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/nadigo/</bitbar.abouturl>
#-----------------------------------------------------------------
#----- required: https://github.com/timsutton/customdisplayprofiles 
#----- move 'customdisplayprofiles' to /usr/local/bin
#----- set your own profile paths for 'day_profile' and 'night_profile'
#-----------------------------------------------------------------
TOOL_PATH="/usr/local/bin/customdisplayprofiles"
var=$($TOOL_PATH current-path)
#--------- set your own profile paths for 'day_profile' and 'night_profile'
day_profile="/Library/ColorSync/Profiles/Displays/Color LCD-12362588-7C1D-77EE-5963-4AEFA92D88E2.icc"
night_profile="/System/Library/ColorSync/Profiles/AdobeRGB1998.icc"
#--------------------------------------------------------------------
if [ "$1" == "day" ]
then
	$TOOL_PATH set "$day_profile"
	exit
elif [ "$1" == "night" ]
then
	$TOOL_PATH set "$night_profile"
	exit
fi 
#------- build manu --------------------------------
echo "ICC"
echo "---"
echo "${var[*]##*/} | length=20"
echo "---"
echo "Day | bash='$0' param1=day color=white templateImage="iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeCAYAAAA7MK6iAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAHHSURBVEhL3dbPK2ZRHMfxp7ExfixmkCgMsvFrg4WykJpY+lOUmTE7LGTnV0q21iRJ2IlSShP+AEsL2VIyjXl/zu08HTPnuc7z3Oupx6dedc95uuebe+85X5lSSw1usWpGRUwPnnFiRkXMmxXuRlV06U1o4Q7otQSlC39wgVpNeFIN/T5tRv6M4xHHZhSQCmhR/UVxxeNii2qN75oIjYrZ4pua8OQzPkaXL9IIW3RGE/lGxXcwYUZRhrCNe2hhucY87JOpxD6+mVHClGEJtpjPHYaRapbhK/YvPYl+pBI9Xl+RXK7wAYmjd+orEGcMQfmEPof2qc0DfIvHWYBNK+y6vdD3ks0N3Bt/QamDOx9qC8oAdBi5v80imxWcO+yJpH3q3hRqA0o9DmHXPcUogqJ96ls8zk8kjg4H3+K5/EY7Esc2fV8RnzWklhGEfN16h2oyidMEnb2KtsMlfAWfsA7bNFrgayBBsa1t14yi6ETS4bAIdS19vVNw32kzdF9BLdXtp5OayCPlKKiftyGknw6iIbr8L24/39NESHTTGX6YkT9foBNJfTdXtM4B5swopby/f29fSydU+MiMihhtK314X82odJLJ/AWeXdquW1Zv2QAAAABJRU5ErkJggg==" terminal=false"
echo "Night | bash='$0' param1=night color=gray image="iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeCAYAAAA7MK6iAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAE0SURBVEhL7dY9S0JhGIfxI4VBg01RUxBNDVLUB/AL5Bq2VrS6NigivYD0AVyKxK9QS9gW2lBTey2t0hBkg0Vdt/HAQe7q2HluafCCH+hy/nA8Lwaj/mPLGP/6OJxWcIWL3rchlEABb/jAJsyT0RPIoDMP83YRHn2BefKbutPrPMK8JsKjog3TVtE/6kzArCNoo2IJZt1AGxVFmPUEbVTcQW4z741BGwzbgEmv0AadB0zBe/fQBsPOIWfHa2fQxvqdIglvbUMb0sgbaw5emoY8l7UhTQcVzCJ2+9BGfvKOFvawgzXkkEcNM/i1FKJcZFEdIHJpPEM70CAaGPgOyEDeStoBo7jEJP7UAq6hHfg7XRwi9h9DeT6v4xbakCNXeB2L8J7ct1so4xhVlJCFyaN0VMyC4BNzsdCubdoJYQAAAABJRU5ErkJggg=="  terminal=false"
