#!/usr/bin/env LC_ALL=en_US.UTF-8 ruby

# <bitbar.title>Keybase Encrypt/Decrypt</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Ash Wu(hSATAC), Aaron Huang(aar0ntw)</bitbar.author>
# <bitbar.author.github>pan-cat</bitbar.author.github>
# <bitbar.desc>A helper for keybase encryption and decryption. Simply copy content you want to decrypt/encrypt and click action, the result will be copied into your clipboard.</bitbar.desc>
# <bitbar.image>http://ash.cat/2FT8+</bitbar.image>
# <bitbar.dependencies>ruby,keybase</bitbar.dependencies>

#--- User parameters ----------------------------------------------------------
KEYBASE = "/usr/local/bin/keybase"

#--- Script internals ---------------------------------------------------------
# TODO: Handle keybase cli error with color or icon?

REFRESH = "---\nRefresh | refresh=true"

# decrypt
if ARGV[0] == "decrypt"
  `/usr/bin/pbpaste | #{KEYBASE} decrypt | /usr/bin/pbcopy`
  exit
end

# ecrypt
if ARGV[0] == "encrypt"
  target = ARGV[1]
  `/usr/bin/pbpaste | #{KEYBASE} encrypt #{target} | /usr/bin/pbcopy`
  exit
end


icon = "🔑"


following = `#{KEYBASE} list-following`.split("\n").map{|f|
  "--#{f} | bash=#{__FILE__} param1=encrypt param2=#{f} terminal=false"
}.join("\n")


puts """
#{icon}
---
Encrypt
#{following}
---
Decrypt | bash='#{__FILE__}' param1=decrypt terminal=false
---
#{REFRESH}
"""
