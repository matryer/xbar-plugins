#!/bin/sh
# Bitbar plugin: Automatically update vpn password. The password is collected from the the VPNBOOK page, then changed on the network settings panel. Required: VPNBOOK must be installed
# metadata
# <bitbar.title>Using free VPN more easily: how to automatically update VPNBOOK password</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Joce ‘El Francés’</bitbar.author>
# <bitbar.author.github>jm462</bitbar.author.github>
# <bitbar.desc>Automatically update VPNBOOK free password. Go and collect password on the VPNBOOK page, then change it on the network settings panel. The password is managed by the keychain application </bitbar.desc>
# <bitbar.image>http://colombien.somee.com/images/bitbarpluginpicture.PNG</bitbar.image>
# <bitbar.dependencies>https://www.vpnbook.com</bitbar.dependencies>
#
# You have to manually give the rights to this script:
# *  Launch terminal and go inside the bitbar plugins folder
# *  chmod 751 updatevpnbookpass.1d.sh
# Some commands of the script needs admin rights.
# It can be done adding the name of the script to the parameter file 'sudoers', giving the possibility to anybody to execute it:
# *  Launch terminal
# *  sudo su - root (to connect with root user)
# *  (enter the root password)
# *  sudo visudo
# *  <your user>      ALL= NOPASSWD: <your path>/updatevpnbookpass.1d.sh
# (This line is to add in the User privilege specification block )
# (where <your user> is your session account
# (and <your path> is the directory where you place your bitbar plugins)
# *  :wq! (to leave visudo)
#
if [ "$1" = 'with_root_rights' ]; then

#echo "SCRIPT 'updatevpnbookpass' STARTS"
#echo "THIS SCRIPT UPDATES, IF NECESSARY, THE VPN PASSWORDS"


FONT="Lucida Grande size=14"
IMAGE1="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"
# GET VPNPAGE
i=1
while [ $i -lt 6 ]; do
#echo "Essai numero " $i " de connection a la page VPNBOOK"
   if curl https://www.vpnbook.com/freevpn > pagetemp.html; then
      break
   fi
i=$((i+1))
sleep 5
done

if [ $i -eq 6 ]
then
echo "PAGE VPNBOOK INACCESSIBLE | $FONT color=black terminal=true templateImage=$IMAGE1"
exit 1
fi

# GET SERVICE AND PASS FROM KEYCHAIN
KEYCHAIN_SERV="571D5A91-9040-4D11-9BAC-685BCEF36270"
KEYCHAIN_PASS=$(sudo security 2>&1 >/dev/null find-generic-password -l 'VPN (PPTP)' -ga vpnbook | awk -F'password: \"' '{print $2}' | awk -F'\"' '{print $1}')

# GET PASSWORD IN VPNBOOK PAGE
ligne=$(grep Password pagetemp.html)
PAGE_PASS=$(echo "$ligne" | awk -F'<strong>' '{print $2}' | awk -F'</strong>' '{print $1}')
#echo "PAGE_PASS = " $PAGE_PASS
#echo "KEYCHAIN_PASS = " $KEYCHAIN_PASS


if [ "$KEYCHAIN_PASS" = '' ] || [ "$KEYCHAIN_PASS" != "$PAGE_PASS" ]
then

# PUT PASS ON KEYCHAIN
#echo "----------------"
#echo "KEYPASS DELETING"
sudo security >/dev/null delete-generic-password -a vpnbook -s "$KEYCHAIN_SERV" /Library/Keychains/System.keychain 2>&1

#echo "----------------"
#echo "KEYPASS CREATING"
#sudo security 2>&1 >/dev/null add-generic-password -a vpnbook -l "VPN (PPTP)" -D "Mot de passe PPP" -s $KEYCHAIN_SERV -w $PAGE_PASS -T "/Applications/Utilities/Keychain Access.app/Contents/Resources/kcproxy" -T "/Applications/System Preferences.app/Contents/MacOS/System Preferences" -T "/usr/bin/security" -T "/usr/sbin/racoon" -T "/usr/sbin/pppd" -T "/System/Library/CoreServices/SystemUIServer.app/Contents/MacOS/SystemUIServer" /Library/Keychains/System.keychain
sudo security >/dev/null add-generic-password -a vpnbook -l "VPN (PPTP)" -D "Mot de passe PPP" -s "$KEYCHAIN_SERV" -w "$PAGE_PASS" -A /Library/Keychains/System.keychain 2>&1
#echo "----------------"
echo "PASS VPN RAFRAICHI | $FONT color=black terminal=true templateImage=$IMAGE1"
else
#echo "THE KEYCHAIN PASSWORD IS THE SAME THAN THE WEBPAGE PASSWORD: NOTHING TO DO"
echo "PASS VPN OK | $FONT color=black terminal=true templateImage=$IMAGE1"
fi
#echo "SCRIPT 'updatevpnbookpass' ENDS"
rm pagetemp.html
exit 0

else
# Script called without parameter $1 'with_root_rights', which means it is the first call

sudo "$(dirname "$0")/updatevpnbookpass.1d.sh" with_root_rights

fi
