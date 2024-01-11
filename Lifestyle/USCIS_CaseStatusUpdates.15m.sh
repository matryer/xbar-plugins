#!/bin/bash
# <xbar.title>USCIS Case Status Updates</xbar.title>
# <xbar.author>Nicolas G.</xbar.author>
# <xbar.author.github>nicolas-g</xbar.author.github>
# <xbar.desc>Alerts when your USCIS cases status is updated</xbar.desc>
# <xbar.version>0.6</xbar.version>
#
# NOTE:
# You first need to get the current case status update hash by running
# $ curl -s -X POST -d "appReceiptNum=$uscis_case_id"  https://egov.uscis.gov/casestatus/mycasestatus.do | grep $uscis_case_id | grep $uscis_case_id > $tmp_file
# $ /usr/local/bin/md5sum $tmp_file | cut -d" " -f1
#
# Once you get the result hash result update the `last_cases_status_hash` variable
# so you have a value to compare and know when the latest case status hash is different and alert
# You will need to get the new hash value and update the last_cases_status_hash
# everytime your case gets update so you can alert on the next update.
#

tmp_file="/tmp/uscis_status_hash.txt"

# see above the NOTE to run the command and update this
# "0cace5e185e984ab3adf7a4b14111d11"
last_cases_status_hash="CHANGE_ME"

# This is usually a 3 letter code followed by a 10 digit number
# Example: LIN111222333
uscis_case_id="CHANGE_ME"



curl -s --retry 5 --retry-delay 15 -X POST -d "appReceiptNum=$uscis_case_id"  https://egov.uscis.gov/casestatus/mycasestatus.do | grep $uscis_case_id | grep $uscis_case_id > $tmp_file

last_update_date=$(grep On $tmp_file | cut -d"," -f 1 | cut -d">" -f 2 | awk '{ print $2 $3}')

current_case_status_hash=$(/usr/local/bin/md5sum $tmp_file | cut -d" " -f1)

if [[ ${current_case_status_hash} == "${last_cases_status_hash}" ]]; then
    echo "USCIS| color=green"
    echo "---"
    echo "Last case update: $last_update_date"
elif [[ ${current_case_status_hash} == "d41d8cd98f00b204e9800998ecf8427e" ]]; then
    # ^ When USCIS site is not reachable the m5sum
    # hash value is d41d8cd98f00b204e9800998ecf8427e
    echo "USCIS!| color=red"
    echo "---"
    echo "$last_update_date SITE NOT REACHABLE"
    /usr/bin/say --rate 230 "Immigration site not reachable"
else
    echo "USCIS| color=red"
    echo "---"
    echo "Last case update: $last_update_date"
    /usr/bin/say --rate 230 "Immigration status, updated"
fi
