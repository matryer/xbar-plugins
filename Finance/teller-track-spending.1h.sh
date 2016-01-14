#!/bin/sh
#
# Teller.io Banking via the OSX menu bar
# Requires:
# - a Teller.io account
# - a UK Bank
# - teller-cli: https://github.com/sebinsua/teller-cli#from-release
# - pcregrep: `brew install pcre`
#
# <bitbar.title>Track spending at UK banks with Teller</bitbar.title>
# <bitbar.version>v1.3.1</bitbar.version>
# <bitbar.author>Seb Insua</bitbar.author>
# <bitbar.author.github>sebinsua</bitbar.author.github>
# <bitbar.desc>Track your spending with teller-cli and an account at Teller.io</bitbar.desc>
# <bitbar.image>https://camo.githubusercontent.com/e0215e6736172334f62effff36ff8df1ab38fed1/687474703a2f2f692e696d6775722e636f6d2f627638545a4c652e706e67</bitbar.image>
# <bitbar.dependencies>teller-cli, pcregrep</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/sebinsua/teller-cli</bitbar.abouturl>

export PATH="/usr/local/bin:/usr/bin/:$PATH";

SPENDING_LIMIT='3000.00'; # Change this to a suitable spending limit.

exit_if_zero() {
  RETURN_CODE=$1;
  ERROR_MESSAGE=$2;
  if [ "$ERROR_MESSAGE" = "" ]; then
    ERROR_MESSAGE="Offline";
  fi;
  if [ "$RETURN_CODE" -ne 0 ]; then
    echo "$ERROR_MESSAGE|color=#7e7e7e";
    exit 1;
  fi;
}

# If we're offline we shouldn't output junk in the menu bar.
curl --connect-timeout 5 www.google.com > /dev/null 2> /dev/null;
exit_if_zero $? "Offline";

CURRENT_OUTGOING=$(teller show outgoing current --hide-currency);
exit_if_zero $? "Error";

CURRENT_BALANCE=$(teller show balance current --hide-currency);
exit_if_zero $? "Error";

LAST_TRANSACTION=$(teller list transactions | tail -n 1 | pcregrep -o1 "[0-9]+[ ]+(.*)");
exit_if_zero $? "Error";

if [ "$(echo "$CURRENT_OUTGOING > $SPENDING_LIMIT" | bc)" -ne 0 ]; then
  OVERSPEND=$(echo "scale=2; $CURRENT_OUTGOING - $SPENDING_LIMIT" | bc);
  echo "🚨 £$OVERSPEND OVERSPENT|color=red";
else
  UNDERSPEND=$(echo "scale=2; $SPENDING_LIMIT - $CURRENT_OUTGOING" | bc);
  if [ "$(echo "$UNDERSPEND > ($SPENDING_LIMIT/2)" | bc)" -ne 0 ]; then
    echo "🏦 £$UNDERSPEND remaining|color=green";
  else
    echo "🏦 £$UNDERSPEND remaining|color=#ffbf00";
  fi;
fi;
echo "---";
echo "Current Account: £$CURRENT_BALANCE";
echo "Current Outgoing: £$CURRENT_OUTGOING";
echo "Last TX: $LAST_TRANSACTION";
