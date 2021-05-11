#! /bin/bash
# Metadata allows your plugin to show up in the app, and website.
#
#  <xbar.title>Kaiten Dashboard</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Riccardo Ancona</xbar.author>
#  <xbar.author.github>raasoft</xbar.author.github>
#  <xbar.desc>This plugin displays the cards in your Kaiten dashboard</xbar.desc>
#  <xbar.image>https://i.gyazo.com/ad9cfe01c760a5887e9be8b298479aac.png</xbar.image>
#  <xbar.dependencies></xbar.dependencies>
#  <xbar.abouturl>http://url-to-about.com/</xbar.abouturl>
#
# Variables become preferences in the app:
#
#  <xbar.var>string(KAITEN_EMAIL=""): The email address used for logging in to Kaiten</xbar.var>
#  <xbar.var>string(KAITEN_TOKEN=""): You personal API token, get one at https://buildo.kaiten.io/profile/api-key</xbar.var>


PATH=$PATH:/usr/local/bin

KAITEN_API="https://buildo.kaiten.io/api/v1"
KAITEN_USER=$(curl -s --request GET $KAITEN_API/users/ --header "Authorization: Bearer $KAITEN_TOKEN" | jq '.[] | select(.email == "'"$KAITEN_EMAIL"'") | .id')
KAITEN_COM_RESULT=$(curl -s --request GET $KAITEN_API/users/"$KAITEN_USER"/cards --header "Authorization: Bearer $KAITEN_TOKEN")

KAITEN_MEMBERS_QUERY=".[] | select((.column.type == 1 or .column.type ==2) and has(\"members\")) | .members[] | select(.user_id == $KAITEN_USER and .type == 1)"
KAITEN_RESPONSIBLE_QUERY=".[] | select((.column.type == 1 or .column.type ==2) and has(\"members\")) | .members[] | select(.user_id == $KAITEN_USER and .type == 2)"

KAITEN_MEMBERS_COUNT=$(($(echo "$KAITEN_COM_RESULT" | jq -c "$KAITEN_MEMBERS_QUERY" | wc -l)+0))
KAITEN_RESPONSIBLE_COUNT=$(($(echo "$KAITEN_COM_RESULT" | jq -c "$KAITEN_RESPONSIBLE_QUERY" | wc -l)+0))

COUNT=$((KAITEN_MEMBERS_COUNT+KAITEN_RESPONSIBLE_COUNT))


function colorForCount {
  if [ "$1" = 0 ]; then echo 'green'; else echo 'red'; fi
}

COLOR=$(colorForCount $COUNT)

echo "⛩ $COUNT ($KAITEN_RESPONSIBLE_COUNT/$KAITEN_MEMBERS_COUNT)"
echo "---"
echo "Responsible of: $KAITEN_RESPONSIBLE_COUNT cards | href=https://buildo.kaiten.io/dashboard/cards?member=0&responsible=1 color=$COLOR"
echo "Member of: $KAITEN_MEMBERS_COUNT cards| href=https://buildo.kaiten.io/dashboard/cards?member=1&responsible=0 color=$COLOR"