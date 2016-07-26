#!/bin/bash

# Categories
# (Uncomment ONE category at a time!)
#
# Default 
cat_name="Default"
cat_url="https://gist.githubusercontent.com/JanGross/fb6ada3292474f300ef4/raw/quotes_default"
#
#Computer Science
#cat_name="Computer Science"
#cat_url=https://gist.githubusercontent.com/JanGross/2ac95e44ca7d0a7e0663/raw/quotes_cscience
#
#--------------------------------------------------------------
#Settings:
#----------
# The amount of characters shown before the quote gets trimmed. (Default: 25)
# The full quote can always be seen in the dropdown menu!
max_chars="25"
#----------
# Defines the color of the quote (status bar and dropdown)
quote_color="black"
#----------

#BitBar Metadata
# <bitbar.title>Random Quotes for BitBar</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Jan Groß</bitbar.author>
# <bitbar.author.github>JanGross</bitbar.author.github>
# <bitbar.desc>Displays a random quote from the selected category</bitbar.desc>
# <bitbar.image>http://i.imgur.com/0yTkSta.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl></bitbar.abouturl>

mkdir -p ~/BitBar_quotes
curl -s $cat_url > ~/BitBar_quotes/quotes
line_c=$(wc -l ~/BitBar_quotes/quotes)
line=$(jot -r 1  2 "$line_c")
quote=$(tail -n+"$line" ~/BitBar_quotes/quotes | head -n1)
echo "$quote | color=$quote_color length=$max_chars"
echo "---"
echo "$quote | color=$quote_color size=20"
echo "Quote from line $line"
echo "---"
echo "Category: $cat_name | color=black size=20"
echo "Category URL: $cat_url | size=10"
echo "Total Quotes in this category: $line_c "


