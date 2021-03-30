#!/bin/bash
# -*- coding: utf-8 -*-

# Metadata allows your plugin to show up in the app, and website.
#
#  <xbar.title>Today's YNAB</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Daniel Mathiot</xbar.author>
#  <xbar.author.github>danymat</xbar.author.github>
#  <xbar.desc>Display the amount earned today</xbar.desc>
#  <xbar.image>https://raw.githubusercontent.com/danymat/personal-xbar-plugins/main/images/ynab_2021_03_30.png</xbar.image>
#  <xbar.dependencies>jq, curl</xbar.dependencies>
#  <xbar.var>string(YNAB_TOKEN=""): API key to get access to remote data.</xbar.var>
#  <xbar.var>string(BUDGET_NUMBER="1"): The budget number for your YNAB account.</xbar.var>
#  <xbar.abouturl></xbar.abouturl>

export PATH='/usr/local/bin:/usr/bin:/bin:$PATH'

ICON_MONEY_FLIES="💸"
ICON_AGE_OF_MONEY="🍃"
ICON_YNAB="Ynab: "
#####

today=$(date "+%Y-%m-%d")
this_month=$(date "+%Y-%m-01")

#######
uri_get_budgets="https://api.youneedabudget.com/v1/budgets"
execute_command="curl -H 'accept: application/json' \
    --compressed -H 'Authorization: Bearer $YNAB_TOKEN' \
    -X GET"
budget_id=$(eval $execute_command $uri_get_budgets | jq '.data.budgets['$BUDGET_NUMBER-1'].id')
uri_get_transactions="https://api.youneedabudget.com/v1/budgets/$budget_id/transactions?since_date=$today"

transactions=$(eval $execute_command $uri_get_transactions)

# echo salut
ynab_today=$(echo $transactions | jq '.data.transactions[] | select(.date == "'$today'") | {amount}' | jq '.amount' | awk '{sum+=$0} END{print sum/1000, "€"}')
echo $ICON_YNAB$ynab_today
echo "---"
uri_get_months="https://api.youneedabudget.com/v1/budgets/$budget_id/months"
months=$(eval $execute_command $uri_get_months)
json_this_month=$(echo $months | jq '.data.months[] | select (.month == "'$this_month'")')
spend_this_month=$(echo $json_this_month | jq '.activity/1000' )
age_of_money=$(echo $json_this_month | jq '.age_of_money' )

echo $ICON_MONEY_FLIES This month: $spend_this_month €
echo $ICON_AGE_OF_MONEY Age of money: $age_of_money days
