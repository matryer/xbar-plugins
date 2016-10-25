#!/bin/bash
# <bitbar.title>CoreAudio Samplerate Display</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Anthony Lauzon</bitbar.author>
# <bitbar.author.github>anthonylauzon</bitbar.author.github>
# <bitbar.desc>Displays current samplerate for an audio device.</bitbar.desc>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl></bitbar.abouturl>

IFS='
'

device_index=0

devices=($(/usr/sbin/system_profiler  \
  SPAudioDataType 2>/dev/null | \
  sed '1,/Devices/d' | \
  grep "^        \w" | \
  sed 's/;$//'))

if (( $# != 0 )); then
  device_index=$(($1-1))
  echo $device_index > ~/.bitbar_audio_device_index
elif [[ -f ~/.bitbar_audio_device_index ]]; then
  device_index=$(cat ~/.bitbar_audio_device_index)
fi

default_device="${devices[$device_index]}"
samplerate=($(/usr/sbin/system_profiler  \
  SPAudioDataType 2>/dev/null | \
  sed '1,/'"${default_device}"'/d' | \
  grep SampleRate | \
  head -n 1 | \
  sed 's/^          Current SampleRate: //'))

echo \( "${samplerate[0]}" \)
echo '---'

i=0
for device in "${devices[@]}"; do
  ((i += 1))
  device=$(echo "$device" | tr -d ' ' | sed 's/:$//')
  echo "$i $device | bash=$0 param1=$i terminal=false refresh=true"
done
