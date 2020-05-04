#!/usr/bin/env LC_ALL=en_US.UTF-8 /usr/local/bin/python3
#
# <bitbar.title>Yahoo Stock Ticker</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Long Do</bitbar.author>
# <bitbar.author.github>longpdo</bitbar.author.github>
# <bitbar.desc>Shows major stock indices in the menu bar and stock symbols in the dropdown menu by pulling data from the Yahoo Finance API. Similar to finance.yahoo.com the prices are delayed, but no API key is necessary. You can also set price alarms for BUY/SELL limits, which will notify you when the limit is reached.</bitbar.desc>
# <bitbar.image>https://github.com/longpdo/bitbar-plugins-custom/raw/master/images/yahoo-stock-ticker.png</bitbar.image>
# <bitbar.dependencies>python3</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/longpdo/bitbar-plugins-custom/blob/master/README.md#yahoo-stock-ticker</bitbar.abouturl>
#
# by longpdo (https://github.com/longpdo)

from datetime import datetime
import json
import os
import re
import sys
import subprocess

# ---------------------------------------------------------------------------------------------------------------------
# Enter your stock symbols here in the format: ["symbol1", "symbol2", ...]
symbols = ["FB", "AAPL", "AMZN", "NFLX", "GOOG", "BIDU", "BABA", "TCEHY"]

# Enter the order how you want to sort the stock list:
# 'name'                     : Sort alphabetically by name from A to Z
# 'market_change_winners'    : Sort by value from top winners to losers
# 'market_change_losers'     : Sort by value from top losers to winners
# 'market_change_volatility' : Sort by absolute value from top to bottom
# '' or other values         : Sort by your custom order from the symbols array above
sort_by = 'market_change_winners'
# ---------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------
# CODE STARTING BELOW HERE, DO NOT EDIT IF YOU ARE A REGULAR USER
# Variables
indices_dict = {
    '^GSPC': '🇺🇸 S&P 500',
    '^DJI': '🇺🇸 DOW 30',
    '^IXIC': '🇺🇸 NASDAQ',
    '^GDAXI': '🇩🇪 DAX',
    '^FTSE': '🇬🇧 FTSE 100',
    '^FCHI': '🇫🇷 CAC 40',
    '^STOXX50E': '🇪🇺 EURO STOXX 50',
}
GREEN = '\033[32m'
RED = '\033[31m'
RESET = '\033[0m'
FONT = "| font='Menlo'"
# ---------------------------------------------------------------------------------------------------------------------


# macOS Alerts, Prompts and Notifications -----------------------------------------------------------------------------
# Display a macOS specific alert dialog to get confirmation from user to continue
def alert(alert_title='', alert_text='', alert_buttons=['Cancel', 'OK']):
    try:
        d = locals()
        user_input = subprocess.check_output(['osascript', '-l', 'JavaScript', '-e', '''
            const app = Application.currentApplication()
            app.includeStandardAdditions = true
            const response = app.displayAlert('{alert_title}', {{
                message: '{alert_text}',
                as: 'critical',
                buttons: ['{alert_buttons[0]}', '{alert_buttons[1]}'],
                defaultButton: '{alert_buttons[1]}',
                cancelButton: '{alert_buttons[0]}'
            }})
            response
        '''.format(**d)]).decode('ascii').rstrip()
        return user_input
    except subprocess.CalledProcessError:
        pass


# Display a macOS specific prompt dialog to get text input from the user
def prompt(prompt_text=''):
    try:
        d = locals()
        user_input = subprocess.check_output(['osascript', '-l', 'JavaScript', '-e', '''
            const app = Application.currentApplication()
            app.includeStandardAdditions = true
            const response = app.displayDialog('{prompt_text}', {{
                defaultAnswer: '',
                buttons: ['Cancel', 'OK'],
                defaultButton: 'OK'
            }})
            response.textReturned
        '''.format(**d)]).decode('ascii').rstrip()
        if user_input == '':
            sys.exit()
        return user_input
    except subprocess.CalledProcessError:
        pass


# Display a macOS specific prompt dialog prompting user for a choice from a list
def prompt_selection(prompt_text='', choices=''):
    try:
        d = locals()
        user_selection = subprocess.check_output(['osascript', '-l', 'JavaScript', '-e', '''
            const app = Application.currentApplication()
            app.includeStandardAdditions = true
            var choices = {choices}
            const response = app.chooseFromList(choices, {{
                withPrompt: '{prompt_text}',
                defaultItems: [choices[0]]
            }})
            response
        '''.format(**d)]).decode('ascii').rstrip()
        if user_selection == 'false':
            sys.exit()
        return user_selection
    except subprocess.CalledProcessError:
        pass


# Display a macOS specific notification
def notify(text, title, subtitle, sound='Glass'):
    cmd = 'osascript -e \'display notification "{}" with title "{}" subtitle "{}" sound name "{}"\''
    os.system(cmd.format(text, title, subtitle, sound))
# ---------------------------------------------------------------------------------------------------------------------


# Methods to read, write, remove data from the hidden .db file --------------------------------------------------------
def read_data_file(data_file):
    with open(data_file, 'r') as f:
        content = f.readlines()
    f.close()
    content = [x.strip() for x in content]
    return content


def write_data_file(data_file, imit_type, symbol, price):
    with open(data_file, 'a') as f:
        f.write(limit_type + ' ' + symbol + ' ' + price + '\n')
    f.close()


def remove_line_from_data_file(data_file, line_to_be_removed):
    with open(data_file, 'r') as f:
        content = f.readlines()
    with open(data_file, 'w') as f:
        for line in content:
            if line.strip('\n') != line_to_be_removed:
                f.write(line)
    f.close()
# ---------------------------------------------------------------------------------------------------------------------


# Curl the yahoo api for data
def get_stock_data(symbol):
    # Building the curl command as a string
    library = 'curl --silent '
    api = 'https://query1.finance.yahoo.com/v7/finance/quote?'
    fields = ['symbol', 'marketState', 'regularMarketTime', 'regularMarketPrice', 'regularMarketChangePercent',
              'fullExchangeName', 'currency', 'regularMarketPreviousClose', 'regularMarketOpen', 'bid', 'ask',
              'regularMarketDayRange', 'fiftyTwoWeekRange', 'fiftyDayAverage', 'twoHundredDayAverage', 'shortName',
              'fiftyDayAverageChangePercent', 'twoHundredDayAverageChangePercent']
    fields_string = 'fields=' + ','.join(fields)
    cmd = library + "'" + api + fields_string + '&symbols=' + symbol + "'"

    # Popen to run the curl command and retrieve the output
    output = os.popen(cmd).read()
    # Jsonify the output from the curl command
    json_output = json.loads(output)

    # Check if a valid symbol was used
    try:
        stock_data = json_output['quoteResponse']['result'][0]
    except IndexError:
        alert('Error', 'Invalid stock symbol: ' + symbol)
        sys.exit()

    return stock_data


# Check a given stock symbol against the price limit list
def check_price_limits(symbol_to_be_checked, current_price, price_limit_list, data_file):
    for limit_entry in price_limit_list:
        if symbol_to_be_checked in limit_entry:
            # Get the limit price, limits are saved in the format: TYPE SYMBOL PRICE
            limit_price = float(limit_entry.split()[2])
            notification_text = 'Current price is: ' + str(current_price)
            notification_title = 'Price Alarm'

            # Notify user if current price is lower than the BUY limit, then remove the limit from list
            if 'BUY' in limit_entry and current_price < limit_price:
                notification_subtitle = 'BUY Limit: ' + str(limit_price)
                notify(notification_text, notification_title, notification_subtitle)
                remove_line_from_data_file(data_file, limit_entry)

            # Notify user if current price is higher than the SELL limit, then remove the limit from list
            if 'SELL' in limit_entry and current_price > limit_price:
                notification_subtitle = 'SELL Limit: ' + str(limit_price)
                notify(notification_text, notification_title, notification_subtitle)
                remove_line_from_data_file(data_file, limit_entry)


# Print the indices information in the menu bar
def print_index(index, name):
    market_state = index['marketState']
    change = index['regularMarketChangePercent']

    # Setting color and emojis depending on the market state and the market change
    if market_state != 'REGULAR':
        # Set change with a moon emoji for closed markets
        colored_change = '🌛' + '(' + '{:.2f}'.format(change) + '%) '
    if market_state == 'REGULAR':
        # Set color for positive and negative values
        color = ''
        if change > 0:
            color = GREEN + '▲'
        if change < 0:
            color = RED + '▼'
        # Format change to decimal with a precision of two and reset ansi color at the end
        colored_change = color + '(' + '{:.2f}'.format(change) + '%) ' + RESET

    # Print the index info only to the menu bar
    print(name, colored_change, '| dropdown=false', sep=' ')


# Print the stock info in the dropdown menu with additional info in the submenu
def print_stock(s):
    market_state = s['marketState']
    change = s['regularMarketChangePercent']

    # Setting color and emojis depending on the market state and the market change
    if market_state != 'REGULAR':
        market = 'CLOSED'
        # Set change with a moon emoji for closed markets
        colored_change = '🌛' + '(' + '{:.2f}'.format(change) + '%) '
    if market_state == 'REGULAR':
        # Set color for positive and negative values
        color = ''
        market = 'OPEN'
        if change > 0:
            color = GREEN + '▲'
        if change < 0:
            color = RED + '▼'
        # Format change to decimal with a precision of two and reset ansi color at the end
        change_in_percent = '(' + '{:.2f}'.format(change) + '%)'
        colored_change = color + change_in_percent + RESET

    # Remove appending stock exchange symbol for foreign exchanges, e.g. Apple stock symbol in Frankfurt: APC.F -> APC
    symbol = s['symbol'].split('.')[0]
    # Convert epoch to human readable time HH:MM:SS
    time = datetime.fromtimestamp(s['regularMarketTime']).strftime('%X')
    # Convert float values to decimals with a precision of two
    fifty_day = '{:.2f}'.format(s['fiftyDayAverage'])
    two_hundred_day = '{:.2f}'.format(s['twoHundredDayAverage'])
    fifty_day_change = '(' + '{:.2f}'.format(s['fiftyDayAverageChangePercent'] * 100) + '%)'
    two_hundred_day_change = '(' + '{:.2f}'.format(s['twoHundredDayAverageChangePercent'] * 100) + '%)'

    # Print the stock info seen in the dropdown menu
    stock_info = '{:<5} {:>10} {:<10}' + FONT
    print(stock_info.format(symbol, s['regularMarketPrice'], colored_change))
    # Print additional stock info in the submenu
    stock_submenu = '{:<17} {:<17}' + FONT
    print('--' + s['shortName'] + FONT)
    print('--' + s['fullExchangeName'] + ' - Currency in ' + s['currency'] + FONT)
    print('--' + time + ' - Market is ' + market + FONT)
    print('-----')
    print(stock_submenu.format('--Previous Close:', s['regularMarketPreviousClose']))
    print(stock_submenu.format('--Open:', s['regularMarketOpen']))
    print(stock_submenu.format('--Bid:', s['bid']))
    print(stock_submenu.format('--Ask:', s['ask']))
    print(stock_submenu.format("--Day's Range:", s['regularMarketDayRange']))
    print(stock_submenu.format('--52 Week Range:', s['fiftyTwoWeekRange']))
    print(stock_submenu.format('--50 MA:', fifty_day + ' ' + fifty_day_change))
    print(stock_submenu.format('--200 MA:', two_hundred_day + ' ' + two_hundred_day_change))


# Print the price limits in the dropdown menu
def print_price_limits(price_limit_list):
    PARAMETERS = FONT + " refresh=true terminal='false' bash='" + __file__ + "'"

    print('---')
    print('Price Limits' + FONT)
    # Print available price limits in the submenu
    for limit_entry in price_limit_list:
        # Split the limit entry, limits are saved in the format: TYPE SYMBOL PRICE
        limit_type = limit_entry.split()[0]
        symbol = limit_entry.split()[1]
        limit_price = limit_entry.split()[2]
        price_limit_submenu = '{:<6} {:<4} {:<10}'
        # Print the price limit data into the submenu
        # onClick will rerun this script with parameters 'remove' and the {limit_entry} to remove clicked the limit
        print(price_limit_submenu.format('--' + limit_type, symbol, limit_price + PARAMETERS + " param1='remove' param2='" + limit_entry + "'"))
    print('-----')
    print('--To remove a limit, click on it.' + FONT)
    # Print the clickable fields to set new limits or clear all price limits
    # onClick will rerun this script with parameters 'set' to set a new limit
    print('Set new Price Limit...' + PARAMETERS + " param1='set'")
    # onClick will rerun this script with parameters 'clear' to clear the hidden .db file
    print('Clear all Price Limits...' + PARAMETERS + " param1='clear'")


if __name__ == '__main__':
    data_file = os.path.join(os.path.dirname(os.path.realpath(__file__)), '.' + os.path.basename(__file__) + '.db')

    # Normal execution by BitBar without any parameters
    if len(sys.argv) == 1:
        stocks = []

        # Check if hidden .db file exists
        try:
            price_limit_list = read_data_file(data_file)
        except FileNotFoundError:
            price_limit_list = []

        # Print the menu bar information
        for symbol, name in indices_dict.items():
            # For each symbol: curl the data, then print it
            index = get_stock_data(symbol)
            print_index(index, name)

        # For each symbol: curl the data, check against the .db file for limits
        for symbol in symbols:
            stock = get_stock_data(symbol)
            stocks.append(stock)
            check_price_limits(symbol, stock['regularMarketPrice'], price_limit_list, data_file)

        # Set order of stocks
        if sort_by == 'name':
            stocks = sorted(stocks, key=lambda k: k['shortName'])
        if sort_by == 'market_change_winners':
            stocks = sorted(stocks, key=lambda k: k['regularMarketChangePercent'], reverse=True)
        if sort_by == 'market_change_losers':
            stocks = sorted(stocks, key=lambda k: k['regularMarketChangePercent'])
        if sort_by == 'market_change_volatility':
            stocks = sorted(stocks, key=lambda k: abs(k['regularMarketChangePercent']), reverse=True)

        # Print the stock information inside the dropdown menu
        print('---')
        for stock in stocks:
            print_stock(stock)

        # Print the price limit section inside the dropdown
        print_price_limits(price_limit_list)

    # Script execution with parameter 'set' to set new price limits
    if len(sys.argv) == 2 and sys.argv[1] == 'set':
        # Run this until user does not want to continue
        while True:
            # Get the user selection of whether he wants to set 'BUY' or 'SELL'
            limit_type_prompt = 'Select the type of your limit: BUY (SELL) limits are triggered, when the price is lower (higher) than the limit.'
            limit_type_choices = '["BUY", "SELL"]'
            limit_type = prompt_selection(limit_type_prompt, limit_type_choices)

            # Get the user selection of all tracked symbols
            symbol = prompt_selection('Select stock symbol:', symbols)

            # Get the user input for a price limit, info message includes the current market price
            price = prompt('Current price of ' + symbol + ' is ' + str(get_stock_data(symbol)['regularMarketPrice']) + '. Enter a value for your price limit.')
            # Check if the user input are decimals with a precision of two
            if not re.match(r'^\d+(\.\d{1,2})?$', price):
                # Alert the user on invalid value and stop the script
                alert('Error', 'You entered an invalid value: ' + price + ' - valid values are decimals with a precision of 2, e.g 25.70!')
                sys.exit()

            # Write the limit to the hidden .db file
            write_data_file(data_file, limit_type, symbol, price)

            # Ask user if he wants to add another limit
            add_another_limit = alert('Question', 'Do you want to add another price limit?', ['No', 'Yes'])
            # If the user clicked the 'No' button, stop the script
            if add_another_limit is None:
                sys.exit()

    # Script execution with parameter 'clear' to clear the .db file
    if len(sys.argv) == 2 and sys.argv[1] == 'clear':
        # Ask for user confirmation
        warning = alert('Warning', 'This will clear your price limits! Do you want to continue?')
        if warning is None:
            sys.exit()

        # Clear the file
        open(data_file, 'w').close()

    # Script execution with the parameters 'remove' and the line to be removed
    if len(sys.argv) == 3 and sys.argv[1] == 'remove':
        limit_to_be_removed = sys.argv[2]
        remove_line_from_data_file(data_file, limit_to_be_removed)
