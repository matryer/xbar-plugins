 #!/usr/bin/env ruby

 # <xbar.title>The Chronophage</xbar.title>
 # <xbar.version>v1.0.0</xbar.version>
 # <xbar.author>Henrik Nyh</xbar.author>
 # <xbar.author.github>henrik</xbar.author.github>
 # <xbar.desc>A memento mori in your menu. Shows the current second; the Chronophage will get the minute. In homage to John C Taylor's "Corpus Clock" (https://www.johnctaylor.com/the-chronophage/).</xbar.desc>
 # <xbar.dependencies>ruby</xbar.dependencies>

 # By Henrik Nyh <https://henrik.nyh.se> 2022-08-01 under the MIT license.

 PHAGE = "🦗"
 DOT = "·"
 STARVING = false  # Good for debugging.

 render = ->(string) { puts "#{string} | font=Monaco" }

 seconds = Time.now.strftime("%S")
 case STARVING ? seconds.chars.last : seconds
 when STARVING ? "0" : "00"
   render.(PHAGE + DOT + DOT + DOT + DOT)
 when STARVING ? "9" : "59"
   render.(seconds + PHAGE + DOT + DOT)
 when STARVING ? "8" : "58"
   render.(seconds + DOT + PHAGE + DOT)
 else
   render.(seconds + DOT + DOT + PHAGE)
 end
