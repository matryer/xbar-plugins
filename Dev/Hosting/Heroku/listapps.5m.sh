#!/bin/bash
# <bitbar.title>Heroku List List</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Cody Brumfield</bitbar.author>
# <bitbar.author.github>cody1213</bitbar.author.github>
# <bitbar.desc>Lists Heroku apps with links to both the live version and, while holding ALT/OPTION, the Heroku dashboard page).</bitbar.desc>
# <bitbar.dependencies>heroku-toolbelt</bitbar.dependencies>

#If your Heroku Toolbelt command isn't /usr/local/bin/heroku, change it here:
heroku_cmd="/usr/local/bin/heroku"

#If you have a custom URL system like https://instance-name.mydomain.com, you can change the default Heroku URLs below to match your system. 
#If your URLs don't use the instance name, you could replace this with a function to parse the output of heroku domains -a instance-name
urlprefix="https://" #part of URL before your instance name
urlsuffix=".herokuapp.com" #part of URL after your instance name

function prepLine {
  # This function preps the lines for each instance and can be used to show different info or customize the URLs
  echo "$1 | href=$urlprefix$1$urlsuffix"
  echo "$1 on Heroku | alternate=true href=https://dashboard.heroku.com/apps/$1"
  count=$((count+1));
}

echo "| templateImage=iVBORw0KGgoAAAANSUhEUgAAADIAAAAyCAYAAAAeP4ixAAAAAXNSR0IArs4c6QAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAACXBIWXMAABYlAAAWJQFJUiTwAAABWWlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNS40LjAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyI+CiAgICAgICAgIDx0aWZmOk9yaWVudGF0aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgpMwidZAAADzUlEQVRoBe2YuYsUQRjFvQ+8EFHEI1lcDwzFQIw0UvAPUGMRjA3ExEQwMjAVM9mNBE0UwcDIQDxBVJAFQRDFAzzwvt9vu99S9PY0PVU9PYH9wes6uo73vq+qpnpmzOis80Dngc4D/5sHZkqwkaKdMaJtTnTPrOOskv5/S+qKVbH9iuMkl1O8VyZiKIQg8kdYJRwXlgjfheXCLeGMgCG2GB333a53R4SvAmMtFF4Jp4TPgtspOzibnQ+9WSlEQ9wIpi16Pozi5UI/xhgP+oZtg+pmsxYyqmHfCZDAi6RXBFtRyNz8xW6ltCUSRIT8W2FEwKL2bXGybKh6T7zm/sW0OALvf+aVx/KUskmzHJ8KOOmX0Io5Ihs12wcBj37L06tKbRZH2dE4qDztIetosK/83mOravDmyeoKcftNovZRQIjxQ3k2PmYxWanPp0PbZ7fazcOTa0S9xgRHcbHy14XbQrj0VGzH7OFRTWdSZUsLD4fLqxe7Rk6opiMSkvLmRsA8YZGAuC/CJ8GG2N8uxKZNCWHNY6x5bK2wQ9gmbBXWC8sEhCJiQrgojAuIIMrJYjRGX1a2tBCAmCfCBeF1Xvam7pVeUjsihdVZhlnLhp6hkPcaE5J4kx+4kDDi2DssMdfTjusM9RzB1PtKMzQhG0TCQiALIGmCJk/KO/ZGWOcoEr01Aha91KM7ZvNOrWu8GXr0mcr3hbvCI+GFwA/gauGEsFPwwbBS+S0CbVynbH+WIgTvhuv7scrXctxR+kYo2gNVQBwhoS0NCzH5FCFEgPsRV4wx4aYQHqsqTi4Ve5m5iApHcOMWI8TH5HOx2SXwHWGDtA8D2oUXQA4DzMKyUkPPGCEmg3eBybPUIBuSV3HKLIB2jVusEMhADJDvRV6v2rFYIbCzZ522w7jHLOGR2aNJZTUiUseonKDuyxQSjib7whu87ryNt4sR4j7rxOakgCBOKAtTtn0zqX5m9unDfemocD7vzIYfmpgYITnvyaOWfz8OCGfzSsQMZZmlCKGvo3NY+XO5GJZZyrj5MP0lqRO6Pz+Mh4TT+fScZhaZVw02MZHUWfjLk+s4/yBi/qHMSi08Uzcn3xTYS2GfcE9gj/g+pmw7liIEsisEbrx7BK7o3Gz5iKoy3wToT+QQ7rqqfpXvUpYW3yITwl6hrgjILOAhmy/4hCOPRe+rmIj4Os6/hvsFxECobiQeqi3/nvDpiyPhwBhY60sy9Jy9mlGpfob9ii2r3hXbTiundKYvHu3Xi2VzJu+RskGnqR1ARXHeZCED4NgN2Xmg80DngQF64B9GCddbdS0DAwAAAABJRU5ErkJggg=="
echo "---"
count=0;

for x in $("$heroku_cmd" apps --all | awk '{if(NR>1)print}'); do prepLine "$x"; done
echo "---"
echo "$count instances | href=https://dashboard.heroku.com/"
