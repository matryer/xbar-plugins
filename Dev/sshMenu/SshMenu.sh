# <bitbar.title>SSH Menu</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Christopher Haen</bitbar.author>
# <bitbar.author.github>Chrisischris</bitbar.author.github>
# <bitbar.desc>Provides a simple menu of ssh hosts when clicked on opens a new ssh session in terminal</bitbar.desc>
# <bitbar.dependencies>bash</bitbar.dependencies>

#!/bin/bash
echo "ssh | color='white'"
echo "---"
#Enter list of servers below give it a name and an address
echo "ServerName(Edit script file) | bash='ssh user@server'"
#^Copy to add more servers
#After adding or changing refresh plugins
