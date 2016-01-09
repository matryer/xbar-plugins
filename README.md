# BitBar Plugins

This repo contains scripts, programs and command-line tools that add functionality to [BitBar](https://github.com/matryer/bitbar#get-started).

* [Available Plugins](https://github.com/matryer/bitbar-plugins#available-plugins)
* [Contributors](https://github.com/matryer/bitbar-plugins#contributors)
* [Add your own plugin](https://github.com/matryer/bitbar-plugins#add-your-own-plugin)

### How to use them

  * Just drop the plugin into your BitBar plugins folder (if you have the repo, why not use the `Enabled` folder?)
  * Make sure it's executable (in Terminal, do `chmod +x plugin.sh`)
  * Then choose `Reset` (or `Restart`) from the BitBar menus

### Available Plugins (61)

#### AWS
- [elb](AWS/elb.30s.sh) `shell`

#### Bitcoin

##### bitstamp.net
- [Last](Bitcoin/bitstamp.net/last.10s.sh) `shell`

##### coinbase.com
- [Balance](Bitcoin/coinbase.com/balance.1h.sh) `shell`
- [Buy And Sell](Bitcoin/coinbase.com/buy-and-sell.10s.sh) `shell`
- [Buy](Bitcoin/coinbase.com/buy.10s.sh) `shell`
- [Sell](Bitcoin/coinbase.com/sell.10s.sh) `shell`
- [Spotrate](Bitcoin/coinbase.com/spotrate.10s.sh) `shell`

##### winkdex.com
- [Winkdex](Bitcoin/winkdex.com/winkdex.1m.sh) `shell`

#### Dev

##### Docker
- [Docker Status](Dev/Docker/docker-status.1m.sh) `shell`

##### GitHub
- [Notifications](Dev/GitHub/notifications.30s.py) `python`
- [Pull Requests](Dev/GitHub/pull-requests.5m.sh) `shell`

##### Homebrew
- [Brew Updates](Dev/Homebrew/brew-updates.1h.sh) `shell`

##### Jenkins
- [Jenkins Status](Dev/Jenkins/jenkins-status.1m.sh) `shell`

##### RescueTime
- [Rescuetime](Dev/RescueTime/rescuetime.1h.py) `python`

##### Semaphoreci
- [Project List](Dev/Semaphoreci/project-list.5m.js) `javascript`

##### Travis
- [Travis Check](Dev/Travis/travis-check.2m.py) `python`

##### Xcode
- [Xcode Version](Dev/Xcode/xcode-version.1h.sh) `shell`

#### Finance
- [Currency Tracker](Finance/currency-tracker.py) `python`
- [Stock](Finance/stock.5s.sh) `shell`
- [Yahoo Stock](Finance/yahoo-stock.1m.sh) `shell`

#### Lifestyle
- [Current Task](Lifestyle/current_task.1m.rb) `ruby`
- [Sleepingtime](Lifestyle/sleepingtime.1m.sh) `shell`
- [Todo](Lifestyle/todo.30s.sh) `shell`

#### Music
- [cmus](Music/cmus.10s.sh) `shell`
- [itunes](Music/itunes.10s.sh) `shell`
- [Spotify](Music/spotify.10s.sh) `shell`
- [Vox](Music/vox.30s.sh) `shell`

#### Network
- [Bandwidth](Network/bandwidth.1s.sh) `shell`
- [Dnsswitcher](Network/dnsswitcher.1d.sh) `shell`
- [External Ip](Network/external-ip.1h.sh) `shell`
- [Internal Ip](Network/internal-ip.1h.sh) `shell`
- [Ping](Network/ping.10s.sh) `shell`
- [vpn check](Network/vpn-check.3s.sh) `shell`
- [Website Status](Network/website-status.sh) `shell`
- [wifiname](Network/wifiname.sh) `shell`

#### System

##### Battery
- [Battery Cycles](System/Battery/battery_cycles.sh) `shell`
- [Keyboard](System/Battery/keyboard.1m.sh) `shell`
- [Mouse](System/Battery/mouse.1m.sh) `shell`
- [Trackpad](System/Battery/trackpad.1.sh) `shell`
- [Trackpad](System/Battery/trackpad.1m.sh) `shell`
- [Clipboard History](System/clipboard-history.3s.sh) `shell`
- [Lock Screen](System/lock-screen.10h.sh) `shell`
- [Memusage](System/memusage.5s.py) `python`
- [Mounted Disk Capacity](System/mounted-disk-capacity.5s.sh) `shell`
- [real cpu usage chart](System/real-cpu-usage-chart.10s.sh) `shell`
- [real cpu usage](System/real-cpu-usage.10s.sh) `shell`
- [Uptime](System/uptime.1m.sh) `shell`
- [usbInfo](System/usbInfo.10s.py) `python`

#### Time
- [Fuzzyclock](Time/fuzzyclock.1s.py) `python`
- [Unixtime](Time/unixtime.5s.sh) `shell`

#### Tutorial
- [Cycle Text And Detail](Tutorial/cycle_text_and_detail.sh) `shell`
- [is bitbar](Tutorial/is_bitbar.sh) `shell`

#### Weather

##### ForecastIO
- [Weather](Weather/ForecastIO/weather.15m.py) `python`
- [GeoIPWeather](Weather/GeoIPWeather.15m.rb) `ruby`

##### OpenWeatherMap
- [Weather](Weather/OpenWeatherMap/weather.15m.py) `python`
- [Wunderground](Weather/Wunderground.30m.rb) `ruby`

#### Web

##### EasyDigitalDownloads
- [Edd](Web/EasyDigitalDownloads/edd.15m.py) `python`

##### HackerNews
- [Hacker News](Web/HackerNews/hacker_news.rb) `ruby`

##### OpenUI5
- [openui5ver](Web/OpenUI5/openui5ver.1h.sh) `shell`

##### Reddit
- [Subreddit Links](Web/Reddit/subreddit_links.rb) `ruby`

##### StackOverflow
- [Reputationizer](Web/StackOverflow/reputationizer.5m.sh) `shell`

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

## Add your own plugin

To contribute your own plugin, consult the [guide to writing plugins](https://github.com/matryer/bitbar#writing-plugins)
