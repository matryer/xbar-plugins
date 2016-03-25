#!/usr/local/bin/python
# -*- coding: utf-8 -*-

# <bitbar.title>Live Cricket Scores</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Anup Sam Abraham</bitbar.author>
# <bitbar.author.github>anupsabraham</bitbar.author.github>
# <bitbar.desc>Show live scores of cricket matches happening around the world using Cricinfo api. </bitbar.desc>
# <bitbar.image>http://i.imgur.com/xiQTWZ4.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl></bitbar.abouturl>

import re
import urllib2
import json
from datetime import datetime
from time import tzname


FAVORITE_CRICKET_TEAMS = [
    # Update the list with the names of all your favorite teams.
    # If any match is not showing up in the plugin, take a look at the 
    # json feed. Find your team and copy paste the teamname in the list
    'India', 
    'New Zealand',
    'Australia',
    'England',
    # '*',  # uncomment this if you want to track all the matches
]
TIME_DELTA = datetime.utcnow() - datetime.now()  # for converting gmt to local time

# fetch the json feed listing all the matches
cricinfo_base_url = "http://www.espncricinfo.com"
summary_url = cricinfo_base_url + '/netstorage/summary.json'
summary_response = urllib2.urlopen(summary_url)
summary_data = json.load(summary_response)

# fetch the url for matches that needs to be shown in the plugin
matches = 0
live_matches = 0
list_links = []
for match_number, match in summary_data['matches'].iteritems():
    team_names = [match['team1_name'], match['team2_name']]
    if "*" in FAVORITE_CRICKET_TEAMS or (set(FAVORITE_CRICKET_TEAMS) & set(team_names)):
        # At least one team in the favorite team list is playing
        matches += 1
        if match['live_match'] == "Y":
            live_matches += 1
        list_links.append(cricinfo_base_url + match['url'])

if matches:
    print '🏏' + str(matches) + ' | dropdown=false'
    print '---'
    print "%s/%s matches live" % (live_matches, matches)
    print "---"
    for match_html_url in list_links:
        match_url = match_html_url.split('.html')[0] + '.json'
        match_data_response = urllib2.urlopen(match_url)
        match_data = json.load(match_data_response)

        # get team_data
        teams = {
            x['team_id']: {
                'name': x['team_abbreviation'], 
                'score': ''
            } 
            for x in match_data['team']
        }

        if match_data['match']['international_class_card'] == "Test":
            test_match = True
        elif match_data['match']['general_class_card'] == "First-class":
            test_match = True
        else:
            test_match = False

        # get innings data
        remaining_balls = None
        required_runrate = None
        if match_data['innings']:
            match_started = True
            for innings in match_data['innings']:
                if teams[innings['batting_team_id']]['score']:
                    # if its a test match and this is the second innings
                    # of the team add '&'' before adding the score
                    teams[innings['batting_team_id']]['score'] += ' & '
                innings_score = innings['runs'] + '/' + innings['wickets'] + ' (' + \
                                    innings['overs'] + ' ov)'
                if innings['live_current'] == '1':
                    innings_score += '*'
                teams[innings['batting_team_id']]['score'] += innings_score
                if innings['live_current'] == '1':
                    batting_team = teams[innings['batting_team_id']]['name']
                    lead = innings['lead']
                    # my guess is that the below two lines should throw error
                    # for test matches. Lets see
                    if not test_match:
                        remaining_balls = match_data['live']['innings']['remaining_balls']
                        required_runrate = match_data['live']['innings']['required_run_rate']
                    break
        else:
            match_started = False

        # get the scores of batsmen at crease
        batsmen = []
        for batsman in match_data['live']['batting']:
            # get batsman name from team info
            # That's right. The live batsman name is not in this 
            # dictionary. Have to go through each players of each team
            # in the match_data to fetch the batsman's name
            batsman_name = ''
            for team in match_data['team']:
                if team['team_id'] == batsman['team_id']:
                    for player in team['player']:
                        if player['player_id'] == batsman['player_id']:
                            batsman_name = player['card_long']
                            break
                    break
            batsman_score = batsman['runs'] + '(' + batsman['balls_faced'] + ')'
            if batsman['live_current_name'] == 'striker':
                batsman_score += '*'
            batsmen.append(batsman_name + ': ' + batsman_score)
        
        # get the match status.
        match_status = match_data['live']['status']
        if test_match:
            status_text = 'Day ' + match_data['match']['actual_days'] + ': '
        else:
            status_text = ''
        if 'scheduled' in match_status.lower():
            # match hasn't started yet. Show the start time of the match in local time
            gmt = match_data['match']['start_datetime_gmt']
            local_datetime = datetime.strptime(gmt, '%Y-%m-%d %H:%M:%S') - TIME_DELTA
            local_time_string = local_datetime.strftime('%I:%M %p')
            status_text +=  'Starting at: ' + local_time_string + " (" + tzname[0] + ")"
        elif 'elected to' in match_status.lower():
            # first innings started. show who won the toss
            toss_winner = match_data['match']['toss_winner_team_id']
            status_text += teams[toss_winner]['name'] + ' elected to ' + match_data['match']['toss_decision_name'] + ' first'
        elif 'trail by' in match_status.lower() or 'lead by' in match_status.lower():
            # batting team is either trailing or leading which means it is
            # either 2nd or 3rd innings of a test match
            if int(lead) >= 0:
                status_text += batting_team + " lead by " + lead + " runs"
            else:
                status_text += batting_team + " trail by " + str(abs(int(lead))) + " runs"
        elif 'require another' in match_status.lower():
            # last innings of the match
            status_text += batting_team + " need " + str(abs(int(lead)) + 1) + " runs to win"
            if remaining_balls and required_runrate:
                status_text += " from " + remaining_balls + " balls(RRR: " + required_runrate + ")"
        elif 'won by' in match_status.lower():
            # match is over. show who won the match
            winner = teams[match_data['match']['winner_team_id']]['name']
            win_status = re.match("^.*(won by (an innings and )?\d* (runs?|wickets?))( \(.*\))?", match_status)
            status_text += winner + " " + win_status.group(1)
            if match_data['match']['rain_rule'] == '1':
                # It can be confusing if the score doesn't look complete
                # and the match result is decided using something like D/L method
                status_text += ' (D/L)'
            # I also wanted to show the Man of the Match info. But can't find it anywhere in the json
        else:
            status_text += match_status

        # print teams and score
        print_params = {
            'team_name': " size=14 ",
            'status_text': " color=blue ",
            'batsman_score': " size=12 "
        }
        if match_started:
            for team_id, each_team in teams.iteritems():
                if each_team['score']:
                    print each_team['name'] + ': ' + each_team['score'] + ' |' + print_params['team_name']
                else:
                    print each_team['name'] + ': Yet to bat |' + print_params['team_name']
        else:
            match_desc = ""
            for team_data in match_data['team']:
                if match_desc:
                    match_desc += " vs "
                match_desc += team_data['team_name']
            print match_desc + ' | ' +print_params['team_name']

        print status_text + ' | href=' + match_html_url + print_params['status_text']
        for batsman in batsmen:
            print batsman + ' |' + print_params['batsman_score']
        print '---'
else:
    # no matches are live
    print '🏏'
    print '---'
    print 'No matches live'
