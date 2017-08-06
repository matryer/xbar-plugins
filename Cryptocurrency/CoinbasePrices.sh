#!/bin/bash

# <bitbar.title>Coinbase Prices</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Mustafa Türksavaş</bitbar.author>
# <bitbar.author.github>mustafaturksavas</bitbar.author.github>
# <bitbar.desc>Displays Coinbase buy prices of Bitcoin, Ethereum, Litecoin.</bitbar.desc>
# <bitbar.image>https://media.giphy.com/media/3oEhmKvAlkJHoqalFK/giphy.gif</bitbar.image>
# <bitbar.dependencies>bash, python</bitbar.dependencies>
#
# Based on Cryptocurrency Prices plugin by viiraj (github.com/viiraj).
#
# I hope this plugin provides value for you and if you feel like it, tips are always appreciated.
# Bitcoin: 1MYkRDn8KtGeeQSoiafymPPB3mncVi2rKN
# Ethereum: 0x2F0138E7035d0dB3D5e0B5b3008D8E0746641E09
# Litecoin: LVYrJcvdBmhazNoAtJhBvaSsyY2kWHwraJ

bitcoin_icon='iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAQAAABLCVATAAAACXBIWXMAABYlAAAWJQFJUiTwAAABY0lEQVRIx2P4z0AdyEBzg1DAdIYfQJgCZHmCWdsYMAFRBs0BC2UAWT5g1p6hbZAggwIcrgALVQNZSWDWAQY24g3qwRtJ/xgeMqxkCGJgotQgGLzAoEUdg/4zvGQQIxzYLAyODF/gQv0MlgwWDK4MOQxbgV5DKG0nLtZ2wIUykII2EMmoU8QZtAWrQQwMB+HiDygzaDNc/CQlBskwfIKLN5JrkAxDFsMTuOh9BiFSDXoHDI2HDB9RlJ1kECc2r20hkI5OMXhQxyAQzCTNoDJgaAgAvaLLEMkwn+EbkuLvDBLkR78yUoD/Z0gn3yAGhnwk5V2UGBRGLYNmICkvIGzQLqwG8TA0oJQAVvgMymcoYehg+AUXWgoM0kygWC/DbpQ4+89wjYERt0FiRNeNX4GlFJ505EykMacZDPGn7HwCBnxiOMcwjcGJcOEvzqADh2vBQk1AVhaYdZCBc7TKpqJBA9ZiAwDMH49EXcmY2QAAAABJRU5ErkJggg=='
eth_icon='iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAACXBIWXMAABYlAAAWJQFJUiTwAAAC/UlEQVRYw+WYX6gMcRTH997158GD/7Z7mzm/8/vN7KZNN1ohL+vBG+GmyN8Hrrh0S3nQxQPPIg9S8i9SJEUePVGKuOF6Q0qkkD9JQv6M7xmzzO6dXcvO7JJbp+bOTr/zmXO+55zfb1Ke56X+Jkv980D1/GnS8zXRPFy21/N8okDGmEnG5luOUjsN8/qWAznERx2bHgqQo/jl5M7O8S0DcpWaBYivISDPKHWoVUDtABgQiDCQAGaZZzYdCJHYEABUAokN1BJ47ECiE9FLDSBPK9XbNCA4Oxh2HgUEe5XryE1IHMjYZjqcfakDCPf5SNJAbYbo2lDHVYAgcKnExIBcorURTmsBedrmm5UCjwWIiMZiPDyPdEp8Drpajut3kcBEm2IHMor3l785fQTEcehkAaLTD1ukLLUM9/phjyu09Np13YmxAWWVmoqFPwfpeSrpQbQWoxdd8AUeThnR3Sxxn8O8GjPu6s8o8bG4gNqw4BV/gGrdA0frAHD7lxpCVAC8G88vBMwpiShgZzcMlCPSkg7YLjh+9ruilsgC7IxjYQ3mpQ0DyRvi7T5UcVYPUNhOgifdsIZc5iLCPfinQBD5W6R8az6fH9F4hBzHDrSYRpPbWDbD6miMSNcJtIwOf+8UrNUQkCyGijoPmGmymGVZ4yDSAz+qriqQulHq0thVjsb/+1zSK2Mpe7xlt5S3DNXSwNRad2GMXBoCBOHjuTVSnWJyHRTD2bgb4+HSFEeE+iSFfhpsXgKwywDaZoj3SDT83oVNGu5d/76L5CcS2ViBujKZUXBwL5SSO7A58lsxVRwmFkQuI03Q39oGOsKpZG4iwxXOZiAFn8JakR6DqFChUBgOwC1oEW/Kfifem+x+iHhHRFnfB8z2iKE6iBk2MukdY1pGSR2j472xzJSmbGGVUlyWmgggpHJzU08dEO6q6kDqYlD2zT0oomGergTSil+UOnPTgZC6MYB6VHFy7W7t2R69CFF54H9sQPNs+ceG4JzWi0itkOaZONB/9wXtG+MF7xgwhWr8AAAAAElFTkSuQmCC'
ltc_icon='iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAQAAABLCVATAAAACXBIWXMAABYlAAAWJQFJUiTwAAABVklEQVRIx2P8z0AdwMRALfAfC8QA3ijSW8l3kRkK7xT5BpkSNog4r71GkRYh1yBFFMl75McaESFEnEGoIXQahZfAYEC81w6hSNrCxUUZ1jP8YeAm1iBmhi9IUgiNAQwvgfxLxAe2LooURCMfwwIofw5MGQsZacgRaIwcRtATdNEMFKl8hgkM/+C8VQwCxBt0FkXqA5z1jiGSlATJwfALq5IdDFKkpWwLLNJfGDJIzyJ5GJJHGJTJyWuLUSR+MpTjzAsEDLqJImFFXgkpwrAWRfg1eUWtH8MLNOFt5BjUh0W4kXSDGBk+YhH2Jt0gRazCoqTXa/pA/IahAEXsIf7Axm6QHsMmBh2Gx8QUsfiLkVUMTRhl9XlgvsMN/uFLR3uxSmKHq3EX/owMJiTU/KdxG6QBLFCJB6eYiKzN8IN/DGepY9BNhs+4C39Dhr9EG3QSGKSDrsUGAN8MmUJvvpOgAAAAAElFTkSuQmCC'

bit_price=$(curl -s -H "CB-Version: 2015-04-08" "https://api.coinbase.com/v2/prices/BTC-USD/buy" | python -c "import sys, json; print json.load(sys.stdin)['data']['amount']")
eth_price=$(curl -s -H "CB-Version: 2015-04-08" "https://api.coinbase.com/v2/prices/ETH-USD/buy" | python -c "import sys, json; print json.load(sys.stdin)['data']['amount']")
ltc_price=$(curl -s -H "CB-Version: 2015-04-08" "https://api.coinbase.com/v2/prices/LTC-USD/buy" | python -c "import sys, json; print json.load(sys.stdin)['data']['amount']")

echo "$bit_price | templateImage=$bitcoin_icon"
echo "$eth_price | templateImage=$eth_icon"
echo "$ltc_price | templateImage=$ltc_icon"
