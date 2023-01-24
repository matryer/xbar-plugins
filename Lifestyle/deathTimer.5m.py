#!/usr/bin/env python3

# <xbar.title>Death Timer</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Thomas Wolfe</xbar.author>
# <xbar.author.github>twolfe2</xbar.author.github>
# <xbar.desc>Counts down how approximately how many days you have left to live</xbar.desc>
# <xbar.image>http://i.imgur.com/2KjZaFb.jpg</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>



from datetime import datetime


death = datetime(2081,5,31, 12, 0, 0) #enter your estimated death year here (get the estimation here: https://www.myabaris.com/tools/life-expectancy-calculator-how-long-will-i-live/)
birth = datetime(1994, 5,31, 12, 0, 0) #enter the day you were born here



diff = death - datetime.now()
remain = format(int(diff.total_seconds()/(3600*24)),' ,d')
print ('~{0} days remaining'.format(remain))

print ('---')


diff1 = death - datetime.now()
remainH = format(int(diff1.total_seconds()/(3600)),' ,d')
print ('~{0} hours remaining'.format(remainH))

remainM = format(int(diff.total_seconds()/60),' ,d')
print ('~{0} minutes remaining'.format(remainM))




total = death - birth
diff = datetime.now() - birth
totalF = format(int(total.total_seconds()/60),' ,d')
remain = format(int(diff.total_seconds()/60),' ,d')
percent = format(((diff.total_seconds()/60)/(total.total_seconds()/60))*100,'.2f')
print ('~{0}% elapsed \nWe are all inventors...guided each by a private chart, of which there is no duplicate.\n-Ralph Waldo Emerson'.format(percent))
