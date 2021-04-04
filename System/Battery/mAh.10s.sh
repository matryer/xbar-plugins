#!/bin/sh

output="$(system_profiler SPPowerDataType | grep "Charge Remaining" | awk '{print $4}')";

charge="${output} mAh"

echo "$charge"

# <xbar.title>Battery mAh</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Luca Angioloni</xbar.author>
# <xbar.author.github>LucaAngioloni</xbar.author.github>
# <xbar.desc>Shows the mAh of battery remaining.</xbar.desc>
# <xbar.image>http://s32.postimg.org/6k1iq3051/Screen_Shot_2016_04_27_at_20_0$
# <xbar.dependencies>none</xbar.dependencies>
