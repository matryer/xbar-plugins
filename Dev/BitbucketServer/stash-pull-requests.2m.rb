#!/usr/bin/env ruby

require 'duration'
require 'json'
require 'rest-client'
require 'pp'

# rubocop:disable all
# <bitbar.title>Bitbucket Server (Stash) Pull Requests</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Adam Snodgrass</bitbar.author>
# <bitbar.author.github>asnodgrass</bitbar.author.github>
# <bitbar.desc>Displays a list of open pull requests in for all repositories in a specified project on Bitbucket Server (aka Stash) in which the user is a participant. Results are sorted by last update (ascending), and are prefixed with a character to indicate participation type: A for author, R for reviewer, and nothing for other (e.g. added a comment). Each entry is clickable and will take you to the pull request page.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/9vp4D6e.png</bitbar.image>
# <bitbar.dependencies>rest-client gem, ruby_duration gem</bitbar.dependencies>
# rubocop:enable all

# Change these, or use a wrapper script and pass in via ARGV.
# Project is optional, and will be ~user if not specified.
USER = nil
PASS = nil
HOST = nil
PROJECT = nil

# Stash RESTful API
class Stash
  def initialize(options = {})
    @host = options.delete(:host) || 'localhost'
    @port = options.delete(:port) || 443
    @user = options.delete(:user)
    @pass = options.delete(:pass)
    @baseurl = "https://#{@host}:#{@port}/rest/api"
    @rest = RestClient::Resource.new @baseurl, @user, @pass
  end

  # Returns an array of repository names for the given project
  def list_repos(project, repos = [], start = 0)
    url = "1.0/projects/#{project}/repos"
    url << "?start=#{start}" if start > 0
    j = JSON.parse(@rest[url].get)
    repos.concat(j['values'].map { |v| v['slug'] })
    list_repos(project, repos, j['nextPageStart']) unless j['isLastPage']
    repos
  end

  # Return an array of pull request IDs for a repo
  # where the user is some kind of participant
  def pull_requests(project, repo, user, prs = [], start = 0)
    url = "1.0/projects/#{project}/repos/#{repo}/pull-requests"
    url << "?start=#{start}&username.1=#{user}"
    j = JSON.parse(@rest[url].get)
    prs.concat(j['values'].map { |v| v['id'] })
    unless j['isLastPage']
      list_prs(project, repo, user, prs, j['nextPageStart'])
    end
    prs
  end

  # Return the message body for a pull request
  def pull_request(project, repo, id)
    url = "1.0/projects/#{project}/repos/#{repo}/pull-requests/#{id}"
    JSON.parse(@rest[url].get)
  end
end

# main plugin class
class StashPRPlugin
  def initialize(user, pass, host, project = nil)
    @user = user
    @host = host
    @stash = Stash.new(
      host: host,
      user: user,
      pass: pass
    )
    @project = project || "~#{user}"
  end

  def output
    prs = with_error_handling { all_user_prs }

    puts "🔧 Pull Requests (#{prs.size})"
    puts '---'
    prs.each { |pr| puts output_line(pr) }
    puts '---'
    puts 'Refresh | refresh=true'
  end

  private

  def all_user_prs
    prs = []
    @stash.list_repos(@project).each do |repo|
      msg("Trying #{@project}/#{repo}")
      @stash.pull_requests(@project, repo, @user).each do |id|
        msg("Adding PR##{id}")
        prs << @stash.pull_request(@project, repo, id)
      end
    end
    prs.sort { |a, b| b['updatedDate'] <=> a['updatedDate'] }
  end

  def with_error_handling
    yield
  rescue SocketError => e
    puts "😡 Pull Requests"
    puts '---'
    puts e
  rescue RestClient::RequestTimeout
    puts "😡 Pull Requests"
    puts '---'
    puts "Connection to #{@host} timed out"
  end

  # rubocop:disable Metrics/AbcSize
  def output_line(pr)
    format('<%s> %s/%s - %s [%s] | href=%s',
           status(pr),
           pr['toRef']['repository']['project']['key'],
           pr['toRef']['repository']['slug'],
           pr['title'],
           duration(pr['updatedDate'] / 1000),
           pr['links']['self'].first['href']
          )
  end
  # rubocop:enable Metrics/AbcSize

  def status(pr)
    return 'A' if author?(pr)
    return 'R' if reviewer?(pr)
    ' '
  end

  def reviewer?(pr)
    !pr['reviewers'].select { |r| r['user']['slug'].eql?(@user) }.empty?
  end

  def author?(pr)
    pr['author']['user']['slug'].eql?(@user)
  end

  # rubocop:disable Metrics/AbcSize
  def duration(tstamp)
    return 'never' if tstamp == 0
    duration = ''
    d = Duration.new(Time.now.to_f - tstamp)
    duration << d.format('%w %~w ') if d.weeks > 0
    duration << d.format('%d %~d ') if d.days > 0
    duration << d.format('%H:%M:%S') if d.weeks <= 0
    duration.strip
  end
  # rubocop:enable Metrics/AbcSize

  def msg(text)
    return if ENV['BitBar']
    puts text
  end

  def debug(obj)
    return if ENV['BitBar']
    pp obj
  end
end

if ARGV.empty?
  ARGV << USER
  ARGV << PASS
  ARGV << HOST
  ARGV << PROJECT unless PROJECT.nil?
end
plugin = StashPRPlugin.new(*ARGV)
plugin.output
