#!/usr/bin/env bash
#
# <xbar.title>Ethereum Base Fee</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>David Dornseifer</xbar.author>
# <xbar.author.github>dpdornseifer</xbar.author.github>
# <xbar.desc>Ethereum EIP1559 base fee</xbar.desc>
# <xbar.abouturl>https://ethgasstation.info/</xbar.abouturl>
# <xbar.image>https://i.imgur.com/66j14MI.png</xbar.image>
# <xbar.dependencies>jq</xbar.dependencies>

# current Ethereum base fee and prediction for next block

export PATH="/usr/local/bin:/opt/homebrew/bin:$PATH"

jq=$(command -v jq)

response=$(curl -s https://api.ethgasstation.info/api/fee-estimate)
read baseFee nextBaseFee priorityFee < <(echo $(echo ${response} | jq -r '.baseFee, .nextBaseFee, .priorityFee.fast'))

if (( ${nextBaseFee} > ${baseFee} )); then
    nextBaseFeeColor="red"
elif (( ${nextBaseFee} < ${baseFee} )); then
    nextBaseFeeColor="green"
else
    nextBaseFeeColor="white"
fi

echo "Ξ BaseFee: \t ${baseFee} | color=white"
echo "Ξ NextBlock: \t ${nextBaseFee} | color=${nextBaseFeeColor}"