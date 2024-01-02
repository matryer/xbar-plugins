#!/usr/bin/env python3

# <xbar.title>Live Cricket Scores</xbar.title>
# <xbar.version>v2.0</xbar.version>
# <xbar.author>Anup Sam Abraham</xbar.author>
# <xbar.author.github>anupsabraham</xbar.author.github>
# <xbar.desc>Show live scores of cricket matches happening around the world using Cricinfo api. </xbar.desc>
# <xbar.image>https://i.imgur.com/xiQTWZ4.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl></xbar.abouturl>

# <xbar.var>string(FAVORITE_TEAMS_FILE="/var/tmp/bitbar_cricket.data"): File location for storing your favorite teams' name.</xbar.var>

import os
import sys
import getopt
import http.client
import json
from datetime import datetime
from pathlib import Path


if len(sys.argv) > 1:
    opts, args = getopt.getopt(sys.argv[1:], "i:d:", [])
    FAVORITE_TEAMS_FILE = Path(args[0])
    for opt, arg in opts:
        if opt == "-i":
            # this is for inserting the team name into our file
            with open(FAVORITE_TEAMS_FILE, "a") as f:
                f.write(arg+"\n")

        elif opt == "-d":
            # this option is for deleting a team name from the file
            if os.path.exists(FAVORITE_TEAMS_FILE):
                with open(FAVORITE_TEAMS_FILE, "r") as f:
                    current_teamnames = [x.strip() for x in f.readlines()]
                    if arg in current_teamnames:
                        current_teamnames.remove(arg)
                with open(FAVORITE_TEAMS_FILE, "w") as f:
                    f.writelines([x+"\n" for x in current_teamnames])
    exit()
else:
    FAVORITE_TEAMS_FILE = Path(os.environ.get('FAVORITE_TEAMS_FILE'))

FAVORITE_TEAMS_FILE.parent.mkdir(parents=True, exist_ok=True)

if os.path.exists(FAVORITE_TEAMS_FILE):
    with open(FAVORITE_TEAMS_FILE, "r") as f:
        favorite_teams = [x.strip() for x in f.readlines()]
else:
    # create an empty file if it doesn't exist
    open(FAVORITE_TEAMS_FILE, "a").close()
    favorite_teams = []

untracked_teams = []
if "*" in favorite_teams:
    track_all_teams = True
else:
    track_all_teams = False

# fetch the json feed listing all the matches
CRICINFO_BASE_URL = "https://espncricinfo.com"
CRICINFO_API_BASE_URL = "hs-consumer-api.espncricinfo.com"

conn = http.client.HTTPSConnection(CRICINFO_API_BASE_URL)
conn.request('GET', '/v1/pages/matches/current?lang=en&latest=true')
response = conn.getresponse().read()
summary_data = json.loads(response)

# fetch the url for matches that needs to be shown in the plugin
matches = 0
live_matches = 0
list_links = []
for match in summary_data['matches']:
    team_names = []
    for team in match['teams']:
        if team['team']['name'] not in favorite_teams:
            untracked_teams.append(team['team']['name'])
        team_names.append(team['team']['name'])
    if track_all_teams or (set(favorite_teams) & set(team_names)):
        # At least one team in the favorite team list is playing
        matches += 1
        match_url = f"{CRICINFO_BASE_URL}/series/{match['series']['slug']}-{match['series']['objectId']}/{match['slug']}-{match['objectId']}/live-cricket-score"
        match_api_url = f"/v1/pages/match/home?lang=en&seriesId={match['series']['objectId']}&matchId={match['objectId']}"
        live_match = False
        if match['stage'] == "RUNNING" and match['state'] == "LIVE":
            live_matches += 1
            live_match = True
        list_links.append((match_url, match_api_url, live_match))

if matches:
    print('🏏' + str(matches) + ' | dropdown=false')
    print('---')
    print("%s/%s matches live" % (live_matches, matches))
    print("---")
    for match_url in list_links:
        match_api_url = match_url[1]
        conn = http.client.HTTPSConnection('hs-consumer-api.espncricinfo.com')
        conn.request('GET', match_api_url)
        response = conn.getresponse().read()
        match_data = json.loads(response)

        # get team_data
        teams = {}
        for team_info in match_data['match']['teams']:
            if 'id' in team_info['team']:
                team_id = team_info['team']['id']
            else:
                team_id = "TBA"

            if 'name' in team_info['team']:
                team_short_name = team_info['team']['name']
            else:
                team_short_name = "TBA"
            teams[team_id] = {
                'name': team_short_name,
                'score': ''
            }

        if match_data['match']['format'] == "TEST":
            test_match = True
        else:
            test_match = False

        # get innings data
        remaining_balls = None
        required_runrate = None
        if match_data['content']['innings']:
            match_started = True
            for innings in match_data['content']['innings']:
                if teams[innings['team']['id']]['score']:
                    # if its a test match and this is the second innings
                    # of the team add '&'' before adding the score
                    teams[innings['team']['id']]['score'] += ' & '
                innings_score = f"{innings['runs']}/{innings['wickets']} ({innings['overs']} ov)"
                # if innings['live_current'] == '1':
                #     innings_score += '*'
                teams[innings['team']['id']]['score'] += innings_score
                # if innings['live_current'] == '1':
                batting_team = teams[innings['team']['id']]['name']
                lead = innings['lead']
                # my guess is that the below two lines should throw error
                # for test matches. Lets see

        else:
            match_started = False

        # get the scores of batsmen at crease
        batsmen = []
        if match_data['content']['supportInfo']['liveSummary']:
            for batsman in match_data['content']['supportInfo']['liveSummary']['batsmen']:
                batsman_name = batsman['player']['name']
                
                batsman_score = f"{batsman['runs']}({batsman['balls']})"
                if batsman['currentType'] == 1:
                    batsman_score += '*'
                batsmen.append(batsman_name + ': ' + batsman_score)

        # get the match status.
        # match_status = match_data['status']
        if test_match:
            if 'actualDays' in match_data['match'] and match_data['match']['actualDays']:
                status_text = f"Day {match_data['match']['actualDays']}:"
            else:
                status_text = 'Day 0:'
        else:
            status_text = ''
        status_text += match_data['match']['statusText']
        if "MATCH_START_HOURS" in status_text:
            start_time = datetime.strptime(match_data['match']['startTime'], "%Y-%m-%dT%H:%M:%S.%fZ")
            current_time = datetime.utcnow()
            time_diff = start_time-current_time
            match_start_hours = time_diff.seconds//3600
            match_start_minutes = (time_diff.seconds//60) % 60
            if match_start_hours:
                match_start_hours = f"{match_start_hours} hours"
            else:
                match_start_hours = ""
            if match_start_minutes:
                if match_start_hours:
                    match_start_hours += " and"
                match_start_minutes = f"{match_start_minutes} minutes"
            else:
                match_start_minutes = ""
            status_text = status_text.replace(
                "{{MATCH_START_HOURS}}", 
                match_start_hours
            ).replace("{{MATCH_START_MINS}}", match_start_minutes)

        # print(teams and score)
        print_params = {
            'team_name': " size=14 ",
            'status_text': " color=blue ",
            'batsman_score': " size=12 "
        }
        if match_started:
            for team_id, each_team in teams.items():
                if each_team['score']:
                    print(each_team['name'] + ': ' + each_team['score'] + ' |' + print_params['team_name'])
                else:
                    print(each_team['name'] + ': Yet to bat |' + print_params['team_name'])
        else:
            match_desc = ""
            for team_data in match_data['match']['teams']:
                if match_desc:
                    match_desc += " vs "
                if 'name' in team_data['team']:
                    match_desc += team_data['team']['name']
                else:
                    match_desc += "TBA"
            print(match_desc + ' | ' + print_params['team_name'])

        print(status_text + ' | href=' + match_url[0] + print_params['status_text'])
        for batsman in batsmen:
            print(batsman + ' |' + print_params['batsman_score'])
        print('---')
else:
    # no matches are live
    print('🏏')
    print('---')
    print('No matches live')
    print("---")

print("Favorite Teams | color=green")
print("--Click on any to remove from favorites")
print("-----")
if favorite_teams:
    for each_team in favorite_teams:
        print(f"--{each_team if each_team != '*' else 'TRACK ALL TEAMS'} | "
              f"terminal=false bash=\"{sys.argv[0]}\" param1=\"-d\" "
              f"param2=\"{each_team}\" param3=\"{FAVORITE_TEAMS_FILE.absolute().as_posix()}\" refresh=true")
else:
    print("--You don't have any favorite teams set")

print("Other Teams | color=red")
print("--Click on any to add to favorites")
print("-----")
for each_team in untracked_teams:
    print(f"--{each_team} | terminal=false bash=\"{sys.argv[0]}\" "
          f"param1=\"-i\" param2=\"{each_team}\" "
          f"param3=\"{FAVORITE_TEAMS_FILE.absolute().as_posix()}\" refresh=true")
if not track_all_teams:
    print(f"--TRACK ALL TEAMS | terminal=false bash=\"{sys.argv[0]}\" "
          f"param1=\"-i\" param2=\"*\" param3=\"{FAVORITE_TEAMS_FILE.absolute().as_posix()}\" refresh=true")
