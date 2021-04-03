#!/bin/bash

# <xbar.title>Shiller PE ratio</xbar.title>
# <xbar.author>Jørgen</xbar.author>
# <xbar.author.github>jtorvald</xbar.author.github>
# <xbar.desc>Gets the current Shiller PE ratio. Information is provided ‘as is’ and solely for informational purposes, not for trading purposes or advice, and may be delayed.</xbar.desc>
# <xbar.dependencies>bash, curl</xbar.dependencies>

SHILLER_PE=$(curl -s https://www.multpl.com/shiller-pe | grep "Current Shiller PE Ratio is" | awk '{print $16}' | cut -d, -f1)

if [ "$(echo "17 > $SHILLER_PE" | bc -l)" -eq 1 ]; then
	ICON="💸 "	
	COLOR=green
elif [ "$(echo "21 > $SHILLER_PE" | bc -l)" -eq 1 ]; then
	ICON="💵 "
	COLOR=orange
else
	ICON=""
	COLOR=black
fi

echo "${ICON}$SHILLER_PE | color=${COLOR}"
echo "---"
echo "Current Shiller PE ${SHILLER_PE} | color=${COLOR}"
echo "See chart | href=https://www.multpl.com/shiller-pe"
