#!/bin/bash
#
# <xbar.title>Fip - Now Playing</xbar.title>
# <xbar.version>v1.2</xbar.version>
# <xbar.author>Bastien L.</xbar.author>
# <xbar.author.github>Behel</xbar.author.github>
# <xbar.desc>What's playing currently on FIP french radio</xbar.desc>
# <xbar.image>https://attentionphilippelepara.pet/img/images/2021/01/18/bitbar.png</xbar.image>
# <xbar.dependencies>jq</xbar.dependencies>
#

export PATH="/usr/local/bin:$PATH"

FIP_NOWPLAYING_URL='https://api.radiofrance.fr/livemeta/pull/7'

JSON="$(curl -s ${FIP_NOWPLAYING_URL})"
json_length="$(echo $JSON | jq '[.steps[]] | length')"

NOW_PLAYING_STEP="$(expr $json_length - 2)"
PREVIOUS_STEP="$(expr $json_length - 3)"
NEXT_STEP="$(expr $json_length - 1)"

echo '| templateImage=iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAEhQTFRFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAq4SsmwAAABh0Uk5TADCv3/9g7yCAQJDQwFDgED/Pb19/8LBwGpMSKgAAAMZJREFUeJzNkssSgyAMRaEGEtQWtEL//09LqMg4gq6caRYBwhFvHkL8iclHBwBKRqdr96iBjQz7GtDzxTA+m8DIn8eVgVcLME2B1rkpArNzNm6dFBg9CvteZpcAgmzmp4EDo+G0YLItQK8hHQlJ5OM2EGEBIBhUftVeRBZg4LDa6nYE+hSHnN0R2L98CtAVoM4Af/EL5CPWgVTCsPXuCEC3zA/IEiqAX0sd9u2W6c2kATnmP9X+Z5EGGwNS0rwNiANC7Qm9w75xIgrxpBUclgAAAABJRU5ErkJggg=='
echo '---'

echo ":notes: Go to FIP website | href=https://www.fip.fr"

echo "---"
echo ":sound: Now Playing | color=#e20179"

title="$(echo $JSON | jq '[.steps[]]['$NOW_PLAYING_STEP'].title' | sed 's/\"//g')"
titleSlug="$(echo $JSON | jq '[.steps[]]['$NOW_PLAYING_STEP'].titleSlug' | sed 's/\"//g' | sed 's/-/ /g')"
artist="$(echo $JSON | jq '[.steps[]]['$NOW_PLAYING_STEP'].authors' | sed 's/\"//g')"
album="$(echo $JSON | jq '[.steps[]]['$NOW_PLAYING_STEP'].titreAlbum' | sed 's/\"//g')"
year="$(echo $JSON | jq '[.steps[]]['$NOW_PLAYING_STEP'].anneeEditionMusique')"
itunes="$(echo $JSON | jq '[.steps[]]['$NOW_PLAYING_STEP'].path' | sed 's/\"//g')"
visual="$(echo $JSON | jq '[.steps[]]['$NOW_PLAYING_STEP'].visual' | sed 's/\"//g' | sed 's/400x400/200x200/g')"
base64_img="$(curl -s $visual --output - | base64)"

echo "| image="$base64_img
echo $title" - "$artist" | href="$itunes
echo "Album : "$album" ("$year")" 
echo "Search on Spotify | href=https://open.spotify.com/search/$(echo $artist | sed 's/ /%20/g')""%20""$(echo $titleSlug | sed 's/ /%20/g')"

echo "---"
echo ":point_left: Last Title | color=red"

ptitle="$(echo $JSON | jq '[.steps[]]['$PREVIOUS_STEP'].title' | sed 's/\"//g')"
ptitleSlug="$(echo $JSON | jq '[.steps[]]['$PREVIOUS_STEP'].titleSlug' | sed 's/\"//g' | sed 's/-/ /g')"
partist="$(echo $JSON | jq '[.steps[]]['$PREVIOUS_STEP'].authors' | sed 's/\"//g')"
pyear="$(echo $JSON | jq '[.steps[]]['$PREVIOUS_STEP'].anneeEditionMusique')"
pitunes="$(echo $JSON | jq '[.steps[]]['$PREVIOUS_STEP'].path' | sed 's/\"//g')"

echo $ptitle" - "$partist" ("$pyear")" "| href="$pitunes
echo "Search on Spotify | href=https://open.spotify.com/search/$(echo $partist | sed 's/ /%20/g')""%20""$(echo $ptitleSlug | sed 's/ /%20/g')"


echo "---"
echo ":point_right: Next Title | color=green"

ntitle="$(echo $JSON | jq '[.steps[]]['$NEXT_STEP'].title' | sed 's/\"//g')"
nartist="$(echo $JSON | jq '[.steps[]]['$NEXT_STEP'].authors' | sed 's/\"//g')"
nyear="$(echo $JSON | jq '[.steps[]]['$NEXT_STEP'].anneeEditionMusique')"
nitunes="$(echo $JSON | jq '[.steps[]]['$NEXT_STEP'].path' | sed 's/\"//g')"

echo $ntitle" - "$nartist" ("$nyear")" "| href="$nitunes