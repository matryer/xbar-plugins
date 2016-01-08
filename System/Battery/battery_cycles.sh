#!/bin/bash

cycles=$(system_profiler SPPowerDataType | grep "Cycle Count" | awk '{print $3}')

# If you want to change the emoticon before the number, here are some
#echo "🔋 $cycles"
#echo "♾ $cycles"
#echo "♽ $cycles"

echo "♾ $cycles"
