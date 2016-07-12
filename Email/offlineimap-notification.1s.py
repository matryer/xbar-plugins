#!/usr/bin/env python
# - * -coding: utf - 8 - * -
#
# <bitbar.title>Offlineimap Checker and notification</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Andros Fenollosa</bitbar.author>
# <bitbar.author.github>tanrax</bitbar.author.github>
# <bitbar.desc>Alert new emails and displays inbox count</bitbar.desc>
# <bitbar.image>https://programadorwebvalencia.com/wp-content/uploads/2016/07/Screen-Shot-2016-07-06-at-18.42.35.jpg</bitbar.image>
import os
import pickle
import tempfile
import re
from os.path import expanduser

# Location
PATH_MAIL = os.path.join(expanduser("~"), 'Mail')
DIR_NEW = 'INBOX/new'
ICON_EMPTY = '📪'
ICON_NEWS = '📬'

# Variables
SAVE_LOCATION = os.path.join(tempfile.gettempdir(),
                             'offlineimap-notification.pkl')
data_save = False
num_news = 0
data_temp = {}

try:
    dateFile = open(SAVE_LOCATION)
    data_temp = pickle.load(dateFile)
except:
    pass


def send_alert_osx(mail_from, mail_subject):
    os.system('osascript -e \'display notification \
    "{mail_subject}" with title "{mail_from}" sound name "Blow"\''
              .format(mail_from=mail_from, mail_subject=mail_subject))


def get_fields(path_mail):
    '''
    Function get elements: email from, email to and Subject

    return list(mail_from, mail_to, mail_subject)
    '''
    mail_from = False
    mail_from_temp = False
    mail_to = False
    mail_to_temp = False
    mail_subject = False
    mail_subject_temp = False
    f = open(path_mail, 'r')
    for line in f:
        mail_from_temp = re.search('(?<=[Ff]rom\: )(.*)(?=)', line)
        mail_to_temp = re.search('(?<=[Tt]o\: )(.*)(?=)', line)
        mail_subject_temp = re.search('(?<=[Ss]ubject\: )(.*)(?=)', line)
        if mail_from_temp:
            mail_from = mail_from_temp.group(1)
        if mail_to_temp:
            mail_to = mail_to_temp.group(1)
        if mail_subject_temp:
            mail_subject = mail_subject_temp.group(1)

    return {'mail_from': mail_from,
            'mail_to': mail_to,
            'mail_subject': mail_subject}


# Get info INGOX news
# Loop folders INBOX
list_dirs = os.listdir(os.path.join(PATH_MAIL))
data_news_emails = []
for name in list_dirs:
    path_news_INBOX = os.path.join(PATH_MAIL, name, DIR_NEW)
    if os.path.isdir(path_news_INBOX):
        # Loop mails news
        for name in os.listdir(path_news_INBOX):
            mail = os.path.join(path_news_INBOX, name)
            if os.path.isfile(mail):
                num_news += 1
                data_news_emails.append(get_fields(mail))

# Send notifications
# Check changes
if data_news_emails != data_temp:
    # Send native alert
    for item in data_news_emails:
        if item not in data_temp:
            print(item)
            send_alert_osx(item['mail_from'], item['mail_subject'])

# Save
data_save = open(SAVE_LOCATION, 'w+')
pickle.dump(data_news_emails, data_save)

# Print
icon = ICON_EMPTY
if num_news > 0:
    icon = ICON_NEWS
print('{icon}{num_news}'.format(icon=icon, num_news=num_news))
