#!/usr/bin/env ruby
#
# Slack Mutli-Teams Overview.
#
# by Harry Löwen
#
# Provides an overview of unread channels, unread messages and channel histories.
# Features: multiple teams (workspaces) handling,
# 'mark as read' and 'open in slack' buttons,
# display all channels or only unread ones.
#
# Uses Slack Legacy Token (from now)
# https://api.slack.com/custom-integrations/legacy-tokens
#
# Refresh rate is set to every minute.
# Because: 180+ lines of code and 4+ requests running per team (one channel, one user)
# For a simple unread indicator check out: https://github.com/matryer/bitbar-plugins/blob/master/Messenger/slack-unread.1s.py
#
# Feel free to customize settings, colors, all-done-messages, etc.
#
# metadata
# <bitbar.title>Slack Multi-Teams Overview</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Harry Löwen</bitbar.author>
# <bitbar.author.github>harryloewen</bitbar.author.github>
# <bitbar.desc>Provides an overview of unread channels, unread messages and channel histories.</bitbar.desc>
# <bitbar.image>https://drive.google.com/uc?export=preview&id=1vxQ5qr8opWaHhiqFlJZmi0oCOG3ik0uJ</bitbar.image>
# <bitbar.dependencies>ruby<bitbar.dependencies>
# <bitbar.abouturl>https://github.com/harryloewen/bitbar-slack-multi-teams/</bitbar.abouturl>

require 'net/http'
require 'open-uri'
require 'json'

# your token(s) please
TOKENS = [
  'xoxp-your-slack-token',
  'xoxp-another-slack-token',
].freeze

# display all channels or only those with unread messages
ALL_CHANNELS = false

# your default color
COLOR = '#696969'.freeze # '#696969' works in darkmode as well

# your random messages if there're no unreads (and if ALL_CHANNELS is set to false)
ALL_DONE_MESSAGES = [
  ":v: All caught up. | color=#{COLOR}
    What’s next? | color=#{COLOR}",
  ":octopus: All done. | color=#{COLOR}
    The world is your oyster. | color=#{COLOR}",
  ":clap: Everything unread is now read. | color=#{COLOR}
    You’ve done it. | color=#{COLOR}",
  ":boom: Boom. | color=#{COLOR}
    You’re up to date. | color=#{COLOR}",
  ":seedling: Everything’s sorted! | color=#{COLOR}
    Let’s start something new. | color=#{COLOR}",
  ":car: There. | color=#{COLOR}
    All caught up. | color=#{COLOR}",
  ":balloon: There! Caught up. | color=#{COLOR}
    Set your mind to something new. | color=#{COLOR}",
  ":rocket: All done. | color=#{COLOR}
    The future is yours. | color=#{COLOR}",
  ":raised_hands: That’s everything! | color=#{COLOR}",
  ":tractor: You’re all read. | color=#{COLOR}
    Here’s a tractor. | color=#{COLOR}"
].freeze

# some helpful methods
def load_content(api_method, options = nil)
  url = "https://slack.com/api/#{api_method}?token=#{@team[:token]}#{options}"
  @content = JSON.parse(open(url).read)
  return if @content['ok']
  @output += "🚫\n"
  @output += "#{api_method}: #{@content['error']} | color=red"
end

def load_team
  load_content('team.info')
  return unless @content['ok']
  @team[:id] = @content['team']['id']
  @team[:name] = @content['team']['name']
  @team.merge!(unreads: 0, users: [], channels: [])
  @teams << @team
end

def load_channels
  load_content('users.conversations', '&types=public_channel%2Cprivate_channel%2Cmpim%2Cim')
  @content['channels'].each do |channel|
    @team[:channels] <<
      { id: channel['id'], name: channel['name'], user: channel['user'],
        is_channel: channel['is_channel'], is_im: channel['is_im'] }
  end
end

def load_users
  load_content('users.list')
  @content['members'].each do |user|
    @team[:users] << { id: user['id'], name: user['name'] }
  end
end

def find_user(message)
  return '...' unless message['type'] == 'message'
  if message['user']
    '@' + message['user']
  elsif message['bot_id']
    if message['attachments']
      message['attachments'][0]['service_name']
    else
      'Bot'
    end
  end
end

def find_text(message)
  return '...' unless message['type'] == 'message'
  if !message['text'].nil? && !message['text'].empty?
    message['text'].tr("\n", ' ').tr("\r", ' ')
  elsif message['attachments']
    message['attachments'].first['text'].tr("\n", ' ').tr("\r", ' ')
  end
end

def handle_messages(channel, red_messages)
  history = []
  @content['messages'].each do |message|
    color = red_messages > 0 ? 'red' : COLOR
    history << "--#{find_user(message)}: #{find_text(message)}|length=90 color=#{color}\n"
    red_messages -= 1
  end
  history << "\n-----\n"
  history << "--🔗 open in Slack | href=slack://channel?id=#{channel}&team=#{@team[:id]}\n"
end

def marking_url(channel)
  timestamp = @content['messages'].first['ts']
  "https://slack.com/api/channels.mark?token=#{@team[:token]}&channel=#{channel[:id]}&ts=#{timestamp}"
end

def load_history
  @team[:channels].each do |channel|
    method = channel[:is_im] ? 'im.history' : 'channels.history'
    load_content(method, "&channel=#{channel[:id]}&count=6&unreads=true")
    channel[:unread] = @content['unread_count_display'].to_i

    if channel[:unread] > 0
      @team[:unreads] += 1
      channel[:history] = handle_messages(channel[:id], channel[:unread])
      channel[:history] << "--✅ mark as read | bash='/usr/bin/curl' param1='#{marking_url(channel)}' refresh=true terminal=false\n"
    elsif ALL_CHANNELS
      channel[:history] = handle_messages(channel[:id], channel[:unread])
    end
  end
end

# everything starts here
@teams = []
@output = ''

TOKENS.each do |token|
  @team = { token: token }
  load_team
end

@teams.each do |team|
  @team = team
  load_channels
  load_history

  @output += "\n---\n#{@team[:name]}\n"

  if @team[:unreads] > 0 || ALL_CHANNELS
    @team[:channels].each do |channel|
      next unless channel[:unread] > 0 || ALL_CHANNELS
      @output += channel[:is_im] ? "<@#{channel[:user]}>" : "##{channel[:name]}"
      @output += channel[:unread] > 0 ? " (#{channel[:unread]})|color=red\n" : "\n"
      channel[:history].each { |message| @output += message.to_s }
    end

    load_users
    @team[:users].each do |user|
      @output = @output.gsub(user[:id], user[:name]).gsub("<@#{user[:name]}>", "@#{user[:name]}")
      @output = @output.gsub('<!channel>', '@channel').gsub('<!here>','@here')
    end
  else
    @output += ALL_DONE_MESSAGES.sample
  end
end

@unread_channels = @teams.map { |t| t[:unreads] }.inject(0, :+)
@unread_channels = '' if @unread_channels.zero?

ICON='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'.freeze

# and finally print it!
puts "
#{@unread_channels} | image=#{ICON}
---
#{@output}
---
Settings
--Tokens: #{TOKENS.count}
----Total: #{TOKENS.count}
----Valid: #{@teams.count} | color=#{@teams.count < TOKENS.count ? 'red' : 'green'}
----Generate tokens | href=https://api.slack.com/custom-integrations/legacy-tokens
--Show all channels: #{ALL_CHANNELS} | color=#{ALL_CHANNELS ? 'green' : 'red'}
---
Refresh ⟳| refresh=true
"
