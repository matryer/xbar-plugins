# BitBar Plugins

This repo contains scripts, programs and command-line tools that add functionality to [BitBar](https://github.com/matryer/bitbar#get-started).

* [Available Plugins](https://github.com/matryer/bitbar-plugins#available-plugins)
* [Contributors](https://github.com/matryer/bitbar-plugins#contributors)
* [Add your own plugin](https://github.com/matryer/bitbar-plugins#add-your-own-plugin)

### Reporting issues

The best way to report an issue with a plugin is to find the plugin on https://getbitbar.com and click "Report issue". If possible, the author will be tagged in the issue - which greatly increases your chances of getting the issue looked at quickly.

### How to use them

  * Just drop the plugin into your BitBar plugins folder (if you have the repo, why not use the `Enabled` folder?)
  * Make sure it's executable (in Terminal, do `chmod +x plugin.sh`)
  * Then choose `Reset` (or `Restart`) from the BitBar menus

###Available Plugins

####AWS
- ELB (shows percentage of in service instances with total in/out in dropdown)

####Bitcoin
- Bitstamp last BTC Price
- Coinbase.com BTC Index
- WinkDex BTC Index

####Developer
- Homebrew available updates
- Jenkins build status
- TravisCI check
- Docker status (docker-machine and running containers status)
- Xcode version
- Github Notifications
- Semaphoreci project list

####Finance
- Stock tracker
- Currency tracker
- hour_logger (money management for freelance work). [Visit hour_logger website](https://github.com/udeyrishi/hour_logger).

####Lifestyle
- Sleeping Time Cycles
- Current Task Reminder
- Show TODO list and no. of items

####Music
- iTunes (shows current track information from iTunes)
- Spotify (Shows current track information from Spotify)

####Network
- Bandwidth Usage
- DNS Switcher
- External IP
- Internal IP
- Ping
- SSH
- VPN connection checker

####System
- Clipboard History
- Real CPU Usage
- Real CPU Usage Chart
- Unix time
- Uptime
- USB Device Info
- Screen Lock
- Ejector
- Power Status/source
- Memory usage
- Trash Collector

#####Battery
- Battery percentage for bluetooth Mouse
- Battery percentage for bluetooth Keyboard
- Battery percentage for bluetooth Trackpad
- Battery cycles
####Time
- Fuzzy clock

####Web
- SAP version
- StackOverflow
- HackerNews Top Stories
- Reddit, Top/New Links in Subreddits
- Twitch, list top streams by game and launch with [livestreamer](http://docs.livestreamer.io)

####Weather
- forecast.io
- Open Weather Map
- Weather Underground

**NOTE**: Often this list isn't exhaustive as it's manually maintained, to see the latest plugins it is recommended that you browse around this repository.

##Contributors

Special thanks to everyone who has contributed:

- Bhagya Silva - [http://about.me/bhagyas](http://about.me/bhagyas)
- Jason Tokoph - [http://jasontokoph.com](http://jasontokoph.com)
- Trung Đinh Quang - [https://github.com/trungdq88](https://github.com/trungdq88)
- Dylan Evans - [https://github.com/whonut](https://github.com/whonut)
- Daniel Seripap - [https://github.com/seripap](https://github.com/seripap)
- Alexandre Espinosa Menor - [https://github.com/alexandregz](https://github.com/alexandregz)
- Damien Lajarretie - [@dqms_output](https://twitter.com/dqms_output)
- Dan Turkel - [https://danturkel.com/](https://danturkel.com/)
- Marian Schubert - [https://github.com/maio](https://github.com/maio)
- Stratos Xakoustos - [https://github.com/Stratouklos](https://github.com/Stratouklos)
- Chris Tomkins-Tinch - [https://github.com/tomkinsc](https://github.com/tomkinsc)
- Raemond Bergstrom-Wood - [https://github.com/RaemondBW](https://github.com/RaemondBW)
- Ant Cosentino - [https://github.com/skibz](https://github.com/skibz)
- Nicolas Lassaux - [https://github.com/nico401](https://github.com/nico401)
- Pierre-Louis Dubouilh - [https://github.com/pldubouilh](https://github.com/pldubouilh)
- Jonathan Keith - [http://jonkeith.com](http://jonkeith.com)
- Jean Caillé - [http://jcaille.github.io](http://jcaille.github.io)
- Carlson Orozco - [https://github.com/carlsonorozco](https://github.com/carlsonorozco)
- Taylor Zane Glaeser - [https://www.taylorzane.com](https://www.taylorzane.com)
- Wiktor Mociun - [https://medium.com/@voter101](https://medium.com/@voter101)
- Srinivas Gorur-Shandilya - [http://srinivas.gs](http://srinivas.gs)
- Adam Snodgrass - [https://github.com/asnodgrass](https://github.com/asnodgrass)
- M Saiqul Haq - [https://github.com/saiqulhaq](https://github.com/saiqulhaq)
- Baron Reznik [http://www.reznik.net](http://www.reznik.net)
- Steve Grosbois - [https://github.com/kwiky](https://github.com/kwiky)
- Manoj Mahalingam - [https://github.com/manojlds](https://github.com/manojlds)
- Florian Hirschmann - [https://github.com/hirschfl](https://github.com/hirschfl)
- Maxime Bertheau - [https://github.com/maxoumime](https://github.com/maxoumime)
- Joe Canero - [https://github.com/caneroj1](https://github.com/caneroj1)
- Goran Gajic - [https://github.com/gorangajic](https://github.com/gorangajic)
- Thameera Senanayaka - [http://thameera.com](http://thameera.com)
- Jeff Beadles - [https://github.com/jeffbeadles](https://github.com/jeffbeadles)
- Gautam krishna R - [https://github.com/gautamkrishnar](https://github.com/gautamkrishnar)

## Add your own plugin

To contribute your own plugin, consult the [guide to writing plugins](https://github.com/matryer/bitbar#writing-plugins)
