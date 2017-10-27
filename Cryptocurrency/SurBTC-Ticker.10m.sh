#!/bin/sh

# <bitbar.title>SurBTC Ticker</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Agustin Feuerhake</bitbar.author>
# <bitbar.author.github>agustinf</bitbar.author.github>
# <bitbar.desc>Shows prices and volume for cryptocurrencies in SurBTC</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/platanus/surbtc-bitbar-plugin/master/image/image.png</bitbar.image>
# <bitbar.dependencies>bash<bitbar.dependencies>

bitcoin_icon='iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAQAAABLCVATAAAACXBIWXMAABYlAAAWJQFJUiTwAAABY0lEQVRIx2P4z0AdyEBzg1DAdIYfQJgCZHmCWdsYMAFRBs0BC2UAWT5g1p6hbZAggwIcrgALVQNZSWDWAQY24g3qwRtJ/xgeMqxkCGJgotQgGLzAoEUdg/4zvGQQIxzYLAyODF/gQv0MlgwWDK4MOQxbgV5DKG0nLtZ2wIUykII2EMmoU8QZtAWrQQwMB+HiDygzaDNc/CQlBskwfIKLN5JrkAxDFsMTuOh9BiFSDXoHDI2HDB9RlJ1kECc2r20hkI5OMXhQxyAQzCTNoDJgaAgAvaLLEMkwn+EbkuLvDBLkR78yUoD/Z0gn3yAGhnwk5V2UGBRGLYNmICkvIGzQLqwG8TA0oJQAVvgMymcoYehg+AUXWgoM0kygWC/DbpQ4+89wjYERt0FiRNeNX4GlFJ505EykMacZDPGn7HwCBnxiOMcwjcGJcOEvzqADh2vBQk1AVhaYdZCBc7TKpqJBA9ZiAwDMH49EXcmY2QAAAABJRU5ErkJggg=='
eth_icon='iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAACXBIWXMAABYlAAAWJQFJUiTwAAAC/UlEQVRYw+WYX6gMcRTH997158GD/7Z7mzm/8/vN7KZNN1ohL+vBG+GmyN8Hrrh0S3nQxQPPIg9S8i9SJEUePVGKuOF6Q0qkkD9JQv6M7xmzzO6dXcvO7JJbp+bOTr/zmXO+55zfb1Ke56X+Jkv980D1/GnS8zXRPFy21/N8okDGmEnG5luOUjsN8/qWAznERx2bHgqQo/jl5M7O8S0DcpWaBYivISDPKHWoVUDtABgQiDCQAGaZZzYdCJHYEABUAokN1BJ47ECiE9FLDSBPK9XbNCA4Oxh2HgUEe5XryE1IHMjYZjqcfakDCPf5SNJAbYbo2lDHVYAgcKnExIBcorURTmsBedrmm5UCjwWIiMZiPDyPdEp8Drpajut3kcBEm2IHMor3l785fQTEcehkAaLTD1ukLLUM9/phjyu09Np13YmxAWWVmoqFPwfpeSrpQbQWoxdd8AUeThnR3Sxxn8O8GjPu6s8o8bG4gNqw4BV/gGrdA0frAHD7lxpCVAC8G88vBMwpiShgZzcMlCPSkg7YLjh+9ruilsgC7IxjYQ3mpQ0DyRvi7T5UcVYPUNhOgifdsIZc5iLCPfinQBD5W6R8az6fH9F4hBzHDrSYRpPbWDbD6miMSNcJtIwOf+8UrNUQkCyGijoPmGmymGVZ4yDSAz+qriqQulHq0thVjsb/+1zSK2Mpe7xlt5S3DNXSwNRad2GMXBoCBOHjuTVSnWJyHRTD2bgb4+HSFEeE+iSFfhpsXgKwywDaZoj3SDT83oVNGu5d/76L5CcS2ViBujKZUXBwL5SSO7A58lsxVRwmFkQuI03Q39oGOsKpZG4iwxXOZiAFn8JakR6DqFChUBgOwC1oEW/Kfifem+x+iHhHRFnfB8z2iKE6iBk2MukdY1pGSR2j472xzJSmbGGVUlyWmgggpHJzU08dEO6q6kDqYlD2zT0oomGergTSil+UOnPTgZC6MYB6VHFy7W7t2R69CFF54H9sQPNs+ceG4JzWi0itkOaZONB/9wXtG+MF7xgwhWr8AAAAAElFTkSuQmCC'

# shellcheck disable=SC2154
if [ "$BitBarDarkMode" ]; then
  # OSX has Dark Mode enabled.
  base_color=white
else
  # OSX does not have Dark Mode
  base_color=black
fi


btc="$(curl -s 'https://www.surbtc.com/api/v2/markets/btc-clp/ticker.json')"
eth="$(curl -s 'https://www.surbtc.com/api/v2/markets/eth-clp/ticker.json')"
preprocess (){
  echo "$1"|
  egrep -o "\"$2\":\[\"([0-9]+\.?[0-9]+)\"" |
  sed "s/\"$2\":\[//" |
  sed 's/\"//g' |
  sed 's/\.[0-9]\{0,9\}//' |
  sed -e ': L
  s/\([0-9]\{1,19\}\)\([0-9]\{3\}\)/\1\.\2/
  t L' |
  sed -e "s/^/$3 $/"|
  sed -e "s/$/$4/"
}
btc_last_price() {
  preprocess "$btc" "last_price" "" ""
}
eth_last_price() {
  preprocess "$eth" "last_price" "" ""
}
echo "|templateImage=$bitcoin_icon"
echo "---"
echo "BITCOIN $(btc_last_price) |templateImage=$bitcoin_icon"
preprocess "$btc" "min_ask" "  ASK" "|size=11 color=green"
preprocess "$btc" "max_bid" "  BID" "|size=11 color=red"
preprocess "$btc" "volume" "24hr VOL" "|size=11 color=$base_color"
echo "---"
echo "ETHER $(eth_last_price) |templateImage=$eth_icon"
preprocess "$eth" "min_ask" "  ASK" "|size=11 color=green"
preprocess "$eth" "max_bid" "  BID" "|size=11 color=red"
preprocess "$eth" "volume" "24hr VOL" "|size=11 color=$base_color"
echo "---"
echo "SurBTC.com | href=\"https://www.surbtc.com/\""
