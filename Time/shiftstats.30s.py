#!/usr/bin/python
# -*- coding: utf-8 -*-

# <bitbar.title>Shift Stats</bitbar.title>
# <bitbar.version>v2.0</bitbar.version>
# <bitbar.author>Jan Groß</bitbar.author>
# <bitbar.author.github>JanGross</bitbar.author.github>
# <bitbar.desc>Shows the remaining time of today's work shift as well as the time of arrival and when its time to leave</bitbar.desc>
# <bitbar.image>http://i.imgur.com/6VTobzU.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>


#----- About ---------------------------------------#
#   This plugin shows the remaining time
#   of today's work shift as well as the
#   time of arrival and when its time to
#   leave. Tested on Yosemite only!
#
#   Written by Jan M. Groß
#   Icons from http://icons8.com
#
#----- Settings -------------------------------------#
#                                                    # Use decimal values to represent minutes (0.5 = 30 min etc.)
shift_length = 8                                     # Shift length in hours
lunch_break = 1                                      # Length of the lunch break in hours
log_path = '/var/log/accountpolicy.log'              # Path to the file containing information about logins - Standard is "/var/log/accountpolicy.log"
lunchsave_path = '/.lunchbreak.sst'                  # File that the lunchtime is being saved to (relative to the current directory)
#----------------------------------------------------#

import os, sys
dir_path = os.path.dirname(os.path.realpath(__file__))
from datetime import datetime, timedelta

lunchsave_path = dir_path + lunchsave_path
if(len(sys.argv) >= 2):
    print "Parameters " + str(sys.argv)
    if(sys.argv[1] == "--start-lunch"):
        with open(lunchsave_path, 'w') as file_:
            file_.write(datetime.now().strftime('%Y-%m-%d %H:%M:%S'))
            print "Lunchfile has been saved!"
            sys.exit(1)

# Images
bitBarDarkMode = os.getenv('BitBarDarkMode', 0)

if (bitBarDarkMode==0) :
    cool_img = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABUElEQVQ4T32TAVEDQRAEiQJAARcFxAFBAXFAogBQwKOAoIBHAaAgcUBQwKMAcMB0aubr60jYqqnbu9udnd+9Hx38tSMdXQozAR/7Fl6EJ/t91qjKb7S/En6c0Pm+mPBQ61K4S96QoHXQtVb8XTY3wbPWBQEhoDKJU2GzJznHEzlrEzUQ8J0fws0/lWtOlNwLYwiQS2VA82KdHJoHipE7momKFQQEntQlvH/3errn/hMCvpkRQfRYBZ57v6rOaSCqphAgBQLGAxgj9irwFjBizuw/aKXhYBYCVHCA0VTQeZ+lyElPOKPYBAISqTquEuotpChqfUGBZcbIZviAmPWbA1HHnk+aWwUrCkoeEsm3Ak3LQ6Iiidh6IIczmspz3iqItXIurIQ57zIq84Cipn/KCUZJI3wJGS13ReD7j32P/K3VfyNnSKdSRphYCFuBSfT2C22RSmAfHmuiAAAAAElFTkSuQmCC"
    desk_img = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAABPUlEQVRYR+2U4RHBQBCFkwrQQXRABXSADnRgVCAqoAQdoAMqoAPpABXw3szdzEnuxiY5kh+3MzuSS+z7bl/24qjhiBvWjwJAmQ4ksGuDHCjbrvhdIrM6NkoBKH5BdnNiD9wP60BIAQ4QmSDPyKmC4NoIeTTWSjdDCsCddpA9JK8Z7MZd7b5fWln9QQrwUu+bALTlVrJOgVMKoC04ocJMVdnjd/wvC7hbfvW0wYwnbjgV2a8tSCGwcoissc7nlUJiAb96tpvBuacdjLkBRVv0Op9tkZwaxg5JSGtIALT/FGdhM1IFYY4i31nk3nN2SQJgmwBdP8FFfhL0yPKAYvAAy5DWUZUAuLrnZd0GoHfsRcBS5EOzlQCE1l3wZZGznksgAIQOhA60pgO+T8PC2H87BxoD8C3srOfrqK0MHADeV1FAIfPgToUAAAAASUVORK5CYII="
    exit_img = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAB70lEQVRYR92X4TEEQRBGXQRkYEWACBABGVgRIAKXASJwIkAETgSEcDIgAr5XNa36pmZOb+3u/bipmtrd0df9+uvemTXZWh6NHl81uY4xFnJ6qPllzidZlOMEMEZw83mim3kE4FFGEA8xUPQ8OQoDLBn2pPDKbi7AlVS60dxJNb7W9SMpN7oCZwr0lJXJd/zoAM8KflroE6v35gOUSvApRQ402XRGVwD1acKp5rbmW3oetAnncnpU2Q9oOILdpzcgN+utQCuPD5Xg+fKFFmbZYm8AMtzVpK7mHMmRuzRyiF4APntz7B3WhPEQvQB89k2KNte11A80H51vypSAO50FBLTaR7Lfk71txbXydAJAfo5Rav9f9i+yYT9goAIqlSA6AfBqAYEz7n0tU6y/S358A8xvGdwP8j1wJ0eXeWQ9U3vgasODd1KAzP3gudR8pXff/24wgJ9KioCR2agK+CwI9K3pm2zVJ1xYgUZO25TGTNeFS4md7zY9E5xOpzENYpUKYQD8Y1yqM+tMBocOQPkxXFMhDEBW7y7r2i0bjqlD5gZcUyEM4A1rwS17+3uTlaKkQhggkHzRZKpVvogZ/EPTZlZhAEpgjeY/qyNgNOR+MvQlYikMUDUMEPj+yVVYCwCMfqv2KqwNwI5iSuFVqAL8AnfXxCEwgBYZAAAAAElFTkSuQmCC"
    timespan_img = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAACyElEQVRYR8WXjZHUMAyFuQqADkIFQAWECoAKCBUAFRAqACpgqeCgAkIFQAWEDqACeF/G70brc2JvZhk0o8nGkaWnJ/lnL278Z7nYEf+x5tyVdklxMSf9puenU3y2Aujl9LmU4C3yUUbvpFPNuAaALF9Jh4KjnylrPt1KrORmBw28DnbX3GwBuCfrz8k5E39LycxaSg6mYAnAN5PBLz0fSilPMwAcvC9NaPQBI2+lT4P9M/2GkSMpMUDmX5MVWQOGLNZk2vgGGwQ1G/dzJnIAXQpOBgh0F6kLQanxlpAQIAFBIoCYPSEHAFrTRhdvZW4fYwUAn2HiMtl90HMoAeg1SNMhILZjasnvNTBfGgBgEpO7KkVkALofSV33F/r9oMF5bSnbBWWdpZSCzWrZU+LkP8kS6gmODFIYcBPBDN2Moz2CLzY02LwdAcQa5Z3ayRB22H4RJsfGZI3nwhzYpN6xdDHOE/yagVEv7HgRVO7U6D2OYwKYLQcdNEbnI6W1b6ZZPaMBHPRC99NQ/fWErkb4BhsuyazfsEFwB43TYzk9jj1sLt8MYNILDVcDgBOaCcBQXJLv6TtAAZjLUaw9AOzQjnjnYKJEa0EjiCIAMqIE0EMTtgiUj8mQZ2zMrfnFEuCg1oQtoFpsik0YlwfLCpr+hawuQ4KVNqJzg/BSZrelmY92QhqIzmZ930nPcwIg4I8UuLgV9/row+joxDoTioP8+KQtHkbEiUbLVnmm4LH2q8cxsTopy8SXh9W73AnAWK6+W1J73mfPr13J6Af2871MkDl3y6XhJNUrmYENaaLfKc1LKYBahIBvpPixNF9KPQGqJmm8XgOEsbV/P2TMmUJgZw3tvbS4U9ZuM50mjtJ4vTZAHJoRggE4FxqO+XPh2zJUA+B53vfJxIys+STjKQWung+tAGIwaAYQ7KAIGaIEPKlh9wBYy3zX+F+K66UhhM4TEQAAAABJRU5ErkJggg=="
    watch_img ="iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABBElEQVQ4T7WTaxHCQAyEqQNwUByAAoqDSqgEJCABCZWAA4oCcEAdgAT2u8l1QntX+MPNZBLy2N3LlWIxPaVSfSJPalIrEo2tck0G4KT8wddSAJ0aLrKtUwLzTbaXVXMAGxVhwXaytTU/5K/GjoJ7BPEKlkoiP16Bxt4aUQBorDWKX9Q8wNGQKdZepmLAAaHn7OIPAAowwhSZwWE4Lg8QCPCBxCugaSiYgkqeqzAUJJsCYoDrXwAYBuQrwNwVeJ1Ohs9e4agiz0PDeImmPiwwu8TxM0ZAhmHmd2sEw07GXyKN8b2JS6PunTr2gdJwcp9yZ6xP61vZUCWPDecvfyZkIzl1JrU3QEtCEdpZuxAAAAAASUVORK5CYII="
    coffee_img = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAABjUlEQVRYR+2W600DQQyESQeUcHQAFRAqACogVEA6gBKgAlICVECoADrIlZAOyHxoHW2OZO92T16EhCX/uIft8Xj2MTkab1dKsZSvS1JNSoI6MV96vgggstONBdCo4kp+Im+zqytgLIAX5QDEaSh+nDuKMQAeVexOfib/DADQAqD4NshKATwo+zz4IuqecdwGEG4Apsr8Jo+FB/XPcrRg43ADQMcUZPmZQTuFeWfjcAOwVGacMWAU/pDHWhhUnJ9KNDALXVqnUz3DAKxkWwmAfUVgIYt6S5ICQNLL7JZ2A177gKUAMGOWWlFnimvkrZwRHbQ+AAQnEyRy00Bv/D8AbwbuoxGhpYX8KR6bNwB2RoSMoYfu+ZHciAaJKFOEnJIs762wvRnoroIfTdUGgAaa32QAIXJuwMS31WTATs3rAKI6ALszMIKt1WAAurk7coNClDtnizcA24jeVXjvweYNgI2Irtcx7bVHAICD5s0Axf82gBt1MEtRmPjGvm/iKxoBt1zW7nkhAIoDvk3FbwCAylwh1H8uYQAAAABJRU5ErkJggg=="
else:
    cool_img = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4AcBBhwWxiKe6QAAASZJREFUOMuNk9FRxDAMRFcM/4QKSAekhNABHVyogHRAqIBcBZfr4KiAXAVABYQKEip4/Mjg8cRw+tFYu7JkeWVKDCgkbSTdSio8vEg6SNqb2RLzLUnuJN1L+vKEyaHSL7yQ1JvZo1YqD8ACNMoY0DhnlwKdA5X+MaBybvfzZmD+q3KmkxkoziX1/uYReIh4kw9v8RmUEbb3nCcDJklXmWLv7q8z+KcBb9HEdwnhxv1LEr/zjuqz0KaZDZK2EenZzEYzGyUdo/jWuUsYyAj0sZCAcmVwpYssnHtgFNACHydMvoh/CpiANgBLAlb82qv7Q+ggElQRElr/1yqpWAP1ipBmoF2T8gxsThDQkFumVlInac4s06Wkzsz61QuidW48IbaDpCFd52/RjRADacgLxAAAAABJRU5ErkJggg=="
    desk_img = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4AcBBh0oHliyAwAAANBJREFUWMPtldENgzAMREnVfxghI3QURugIbFBG6CawAWzCCLDB648rWWlaUAOpVPkk/1wgPvtsKAqDYSMAD3TAJNEBPmfymVfMWUQAvSQcgUpiFK7PIWCRZJXiKuGmlLtPG58rI9xTTFYLBmXBkNMCr2zQWFKHcKsF1zc2lHJ2aPW1qriRbnigVXwdvHNX34vbXv43kbM2nANJHqJNEUC4gsFsABBZ2YtE8qoeCher+NCEzrlvtiD7n489O/Hpvp93wASYABNwXtvfv++AwfAAuelGAznB3AUAAAAASUVORK5CYII="
    exit_img = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4AcBBh4QHXdZXgAAAcJJREFUWMPFV9FxgzAMfcrlv9mgjMAGZQQ2iEdIJyidoMkEpROknQC6AZ0gZIKGCV5/7DuVYoIJUN1xh42MpadnSQaUkIxInjifnEhu9J4r/JbIPnNJBCDWE+se5TcA9YQbb69qkUwUXMlUbvf9d4V/ltWEXu5IflsvC5LxYgaQTAG8AHAMTwAc24yfEwEzhPGzhuC/OZB3zJ0BVIsYICLvAB4BNHbqE0AqIpdra9cBRCsBPHg+19bbVETKyUNA0vRs7giXAiis7mAZikCm4urivQNw16H7ShIikk+CgPXo3hkiIhmA0rO5NsKMSSh/cjbJ2o5rzQdPuS1JXtTYBNWYtiJJc+VnbYlIxi0j2hJUjJzyWcU08+h+iEgtIpVd10xBwp09XqXzvuc07FVeqGwxMpP2AyT3vtgv1Q/EAal4llrgg387uwEdIdEkS8a2cOuOnP6s3n3wN3ZcqYT05Mg62gARqVXR2ZLsOpIAkFtdA+CoUQgtRm2Y44EXjMiTFYtbT8FmgJ0HEalb7VhzKxdubUoz5WE++r5hQ1CEtNVqbdUVojEhSOyzCQTCdPQPy11MbBE6uMTURmGptjwD8BWCwg9iV3+aGz6VPAAAAABJRU5ErkJggg=="
    timespan_img = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4AcBBh0DsuRLQwAAAhJJREFUWMPVV+2R00AMfcvcf1xCOjhTAdcB7oB0ACWECs5UEHdwSwUxFXipgFBBlgoeP9DO6Hz76UuGQTM7ifdLsvSeJAP/WEzrAZIDgHsAOxkAcJbhjDHfrm4lyQeST6yXJ5IP11C8I3lMKDmTnGW4xJ4jyd1W5T3Ji7rMk5wkBDlPjbI3yIVk36p8X+vrxPlOjNWyrwKhWLvI428AewA+iWJj5gJgJwBvZeqdMcYlDZB4LQA6mbIAXJZGxnwphRLALEZ4MeIc1t+s9h+U8q8AfoiRuYGCgU68CLn7EPWA0OYkj7PaOMp/n1DwvRJXE4CPqVCApFVoH4Rem0CYAWZgh41tCDKu2KApddrM67/3jYGaL9CqlPSRZORWvD6FkUlgn0h2GT2DXjiUXKqs14aMEaVLjvtq7fAMIDI5V9QEHZKfkveXBETGyB3uxZoC3FwJJpvBpSP5OYWVta675vptjAcwyAXvZfqX0NXqJNOCzhCCpbFYWRl9w7lnIbhTDQUA9A2ecACGDWy8l1+vU7FbZcSbyKqUu2IiuoEBgco+tmgVv7sbKO9Ug2NTHA8y3cCAKZVtU5uGa8a+6uUknfrNvVy5t/TFYiYHdL4fXvnml6LrK5rSYwswBXDHmqa05Il1e/1I8kPhjR8j7Xy/1YW7SHsdZFF9QaoaTq9pYGJ531d0ab6lPmz9OO0zH6cW/5P8ARJNcwtJJuvFAAAAAElFTkSuQmCC"
    watch_img = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4AcBBhs1KwR5XAAAAO9JREFUOMutk9FtwzAMRN8FHcAjaAR1g4yQETJCNqhH8AgZoSNkgyoTVBvUG7A/FEAosvOREBBkiMez7khBF2aW2Ii9XARdd3JLf/YxwCUz+wI+gdrOgB8g9+BD94cGuAO/Xpj8+95hHq43mdm3mZ18T1F7l5tGBHMDbJBnLz6b2TySkIEjcOmLgcX9mB2XRwQVSJJqKD4CV+AiaZVU3JO13fTA81j3kpEgATWaJ+nmknLoQAUmSaeeoAA310sgWZ0I96D4etrGHOdjq40aDNLixmWX1QwuwNkNLcNRllTMjDCBf8GftWHe+ph49Tn/A5oeu+j1XBWdAAAAAElFTkSuQmCC"
    coffee_img = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAm0lEQVQ4T2NkIAwSgEoW4FLGSEC/A1DeAIgnkGMATGMAUDPIFReA+AC6QfhcAHJ2ARArAHEDEIMMwgD4DABpBjl9A9QFH4gxAORsfjzh8hHqFbgSdBeA/GiPx4CDQDkHZPkBMQAUuKBYAQXqB3JdAIuRDeQaAHLBAyA+QK4B8KglxwBQGIAAmCbHANpGIyjp5uNJSBOBcqAkDgcAGnooEcKNcYIAAAAASUVORK5CYII="

def get_first_login():
    #Reading the first line of the Log file
    with open(log_path, 'r') as logFile:
        first_line = logFile.readline()
        return first_line

def get_lunchbreak_time():
    ''' Returns the first line from the .lunchbreak file '''
    if(os.path.exists(lunchsave_path)):
        #Reading the lunchbreak file
        with open(lunchsave_path, 'r') as lunchFile:
            line = lunchFile.readline()
            return line
    else:
        return "N/A"

def start_lunch_button():
    #Call this script with the --start-lunch flag
    print "-- Start lunch break | refresh=true | color=indianred bash="+__file__+ " param1='--start-lunch' terminal='false' color=green"

def print_lunch_info():
    last_lunch = get_lunchbreak_time()[:19]  #Has to be cut after 19 characters because unconverted data remains otherwise (probably the newline)
    if(last_lunch== "N/A"):
        print "-- No lunch break in the records"
        start_lunch_button()
    else:
        lunch_begin = datetime.strptime(last_lunch,"%Y-%m-%d %H:%M:%S") #lunch_begin = the time logged in the .lunchbreak file
        lunch_end = lunch_begin + timedelta(hours=(lunch_break))
        remaining_lunch =  lunch_end  - datetime.now()
        if(lunch_begin.date() == datetime.now().date()):
            if not remaining_lunch.seconds/3600 > lunch_break :
                print "-- Lunch time ends in: %dh %dm | image=" % (remaining_lunch.seconds/3600, (remaining_lunch.seconds/60)%60) + coffee_img
            else:
                print "-- No lunchtime left for today!"
        else:
            start_lunch_button()

        dts_units = 1
        dts_span = 8
        dts_unit = "week"

        # Last lunch time stuff
        print ("-- Last lunch break: \n--%s") % (str(((lunch_begin if (datetime.now().date() - lunch_begin.date()).days < dts_span else 'More than {0} {1} ago'.format(dts_units, dts_unit)) if lunch_begin.date() != datetime.now().date() else 'Today at ' + str(lunch_begin.time()))))


parts = get_first_login().split() #Splitting the first line of the logfile into parts

#Time calculatios
arrival_time = datetime.strptime(parts[2],"%H:%M:%S") #Stripping the time from the log. Be aware that because we are not stripping any date, it will be set to January 1st, 1900
shift_end = arrival_time
shift_end += timedelta(hours=(shift_length + lunch_break))
remaining_time = shift_end - datetime.now()




# Printing out the main countdown
def print_main():
    if remaining_time.seconds/3600 > shift_length : #Assuming that the shift ended when the shift_length is exceeded since the time of arrival is way in the past (as mentioned above)
        print "Shift ended! Have a nice evening |  image=" + cool_img
    else :
        print "Shift ends in: %dh %dm | image=" % (remaining_time.seconds/3600, (remaining_time.seconds/60)%60) + watch_img# Calculating the hours and minutes based on the remaining seconds
# Printing out the sub menus
def print_sub():
    print "---"
    print "Now: " + datetime.now().strftime("%x-%H:%M:%S")
    print "Arrived at " + arrival_time.strftime("%H:%M:%S") + "| color=green image=" + desk_img
    print "Shift ends at " + shift_end.strftime("%H:%M:%S") + "| color=green image=" + exit_img
    print "---"
    print "Shift length: " + str(shift_length) + " Hours | color=red image=" + timespan_img
    print "---"
    print "Lunch break " + str(lunch_break) + " Hours | color=black image=" + coffee_img
    print_lunch_info()
    print "---"
    print "About this plugin"
    print "-- Developed by Jan M. Groß | color=black"
    print "-- Images/Icons from icons8.com | href=http://icons8.com"
    print "-- Version 2.0"
    print "-- ---"
    print "-- Report an issue | href=https://github.com/matryer/bitbar-plugins/issues/new?assignee=jangross&title=re:%20Workshift%20Stats&body=FAO:+(@jangross)"

print_main()
print_sub()
