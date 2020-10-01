#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python3

# <bitbar.title>Live NBA Game Stat</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Chen Luxin</bitbar.author>
# <bitbar.author.github>chenlujjj</bitbar.author.github>
# <bitbar.desc>Show NBA game scores in bar.</bitbar.desc>
# <bitbar.dependencies>python3</bitbar.dependencies>

import json
import urllib.request
from datetime import datetime, timezone


def time_convert(dt):
    local_datetime = dt.replace(tzinfo=timezone.utc).astimezone(tz=None)
    # only care about time, not date
    return datetime.strftime(local_datetime, '%H:%M')


class Game:

    @classmethod
    def from_data(cls, data: dict):
        game = cls()
        fmt = '%Y-%m-%dT%H:%M:%S.%f%z'
        game.start_time = datetime.strptime(data['startTimeUTC'], fmt)
        if 'endTimeUTC' in data:
            game.end_time = datetime.strptime(data['endTimeUTC'], fmt)
        else:
            game.end_time = None
        game.hTeam = Team.from_data(data['hTeam'])
        game.vTeam = Team.from_data(data['vTeam'])
        return game

    def __repr__(self):
        # example: 09:00-11:35  MIA 98-116 LAL
        start_time = time_convert(self.start_time)
        end_time = time_convert(self.end_time) if self.end_time else 'now'
        return f'{start_time}-{end_time} {self.vTeam.triCode} {self.vTeam.score}-{self.hTeam.score} {self.hTeam.triCode}'

    def details(self):
        vTeam_linescores = self.vTeam.linescores
        hTeam_linescores = self.hTeam.linescores
        return " ".join(["%s:%s" % (vTeam_linescores[i], hTeam_linescores[i]) for i in range(len(vTeam_linescores))])


class Team:
    def __init__(self, triCode: str, score: int, linescore):
        self.triCode = triCode
        self.score = score
        self.linescore = linescore

    @classmethod
    def from_data(cls, data: dict):
        team = cls(data['triCode'], int(data['score']), data['linescore'])
        return team

    @property
    def linescores(self):
        if not self.linescore:
            return []
        return [line['score'] for line in self.linescore]


BASE_URL = 'http://data.nba.net/10s'
TODAY_URL = BASE_URL + '/prod/v1/today.json'

today_resp = urllib.request.urlopen(TODAY_URL).read()
today_resp_json = json.loads(today_resp.decode('utf-8'))

score_board_path = today_resp_json['links']['currentScoreboard']
score_board_url = BASE_URL + score_board_path
score_board_resp = urllib.request.urlopen(score_board_url).read()
score_board_resp_json = json.loads(score_board_resp.decode('utf-8'))

games_num = score_board_resp_json['numGames']
games_data = score_board_resp_json['games']


if games_num != 0:
    print('🏀%d' % games_num)
    print('---')
    for game_data in games_data:
        game = Game.from_data(game_data)
        print("%s | size=14 color=black" % game)
        print('-- %s | size=13' % game.details())
else:
    print('🏀')
