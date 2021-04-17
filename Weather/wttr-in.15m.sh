#!/bin/bash

# Metadata:
# <xbar.title>wttr.in</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>kawarimidoll</xbar.author>
# <xbar.author.github>kawarimidoll</xbar.author.github>
# <xbar.desc>Show current weather using wttr.in.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/kawarimidoll/kawarimidoll/master/assets/xbar-wttr-in.png</xbar.image>
# <xbar.dependencies>curl</xbar.dependencies>

# Variables:
# <xbar.var>string(VAR_LOCATION="Cupertino"): Your location.</xbar.var>
# <xbar.var>string(VAR_FORMAT="3"): Display format. Ref: https://github.com/chubin/wttr.in#one-line-output</xbar.var>

curl "wttr.in/${VAR_LOCATION}?format=${VAR_FORMAT}"
