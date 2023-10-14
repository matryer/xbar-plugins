#!/usr/bin/env ruby
# frozen_string_literal: true

# <xbar.title>Github Contribution</xbar.title>
# <xbar.version>v0.0.3</xbar.version>
# <xbar.author>mizoR, geekness (ISSUE #1550), sprak3000 (xbar vars)</xbar.author>
# <xbar.author.github>mizoR</xbar.author.github>
# <xbar.image>https://user-images.githubusercontent.com/1257116/34550684-37da7286-f156-11e7-9299-5873b6bb2fd7.png</xbar.image>
# <xbar.dependencies>ruby</xbar.dependencies>
# <xbar.var>string(VAR_USERNAME=mizoR): Your GitHub username</xbar.var>
# <xbar.var>number(VAR_MAX_CONTRIBUTIONS=10): The maximum number of contributions to display</xbar.var>
#
# To setup, create or edit your ~/.bitbarrc file with a new section:
#
# [github_contribution]
# username = mizoR
# max_contributions = 10

require 'erb'
require 'date'
require 'open-uri'

module BitBar
  module GitHubContribution
    ConfigurationError = Class.new(StandardError)

    class Contribution < Struct.new(:username, :contributed_on, :count)
      RE_CONTRIBUTION = %r|<rect .*class="ContributionCalendar-day" .*data-date="(\d\d\d\d-\d\d-\d\d)" .*data-level="(\d+)" .*>.*<\/rect>|

      def self.find_all_by(username:)
        [].tap do |contributions|
          html = URI.parse(url_for(username: username)).open { |f| f.read }
          html.scan(RE_CONTRIBUTION) do |date, count|
            contributions << Contribution.new(username, Date.parse(date), count.to_i)
            break if Date.parse(date) == Date.parse(DateTime.now.to_s)
          end
        end
      end

      def color
        count <= 0 ? 'brown' : 'green'
      end

      def icon
        case count
        when 0    then '👻'
        when 1..3 then '🌱'
        when 4..9 then '🌿'
        else           '🌳'
        end
      end

      def self.url_for(username:)
        "https://github.com/users/#{username}/contributions"
      end
    end

    class View
      TEMPLATE = <<-EOT.gsub(/^ */, '')
        <%= @contribution.icon %><%= @contribution.count %> | color=<%= @contribution.color %>
        ---
        <% @contributions.each do |c| -%>
        <%= @helper.link_to(@helper.contribution_text_for(c), @helper.contribution_activity_for(c)) %>
        <% end -%>
      EOT

      class Helper
        def link_to(text, href)
          if text =~ / | /
            "#{text} href=#{href}"
          else
            "#{text} | href=#{href}"
          end
        end

        def contribution_text_for(contribution)
          "#{contribution.icon} #{contribution.contributed_on.strftime('%Y-%m-%d (%a)')}   \t#{contribution.count} | color=#{contribution.color}"
        end

        def contribution_activity_for(contribution)
          query    = "from=#{contribution.contributed_on}"
          fragment = "year-link-#{contribution.contributed_on.year}"

          "https://github.com/#{contribution.username}?#{query}##{fragment}"
        end
      end

      def initialize(contributions:)
        @contribution  = contributions.fetch(0)
        @contributions = contributions
        @helper        = Helper.new
      end

      def render
        puts ERB.new(TEMPLATE, nil, '-').result(binding)
      end
    end

    class App
      DEFAULT_CONFIG = { max_contributions: 10 }

      def initialize(config = {})
        config = cast_config()

        @username, @max_contributions = config.values_at(:username, :max_contributions)
      end

      def run
        # (DateTime.now-7).to_s
        contributions = Contribution.find_all_by(username: @username)
                                    .sort_by(&:contributed_on)
                                    .select{|l| l.contributed_on < DateTime.now}
                                    .reverse
                                    .slice(0, @max_contributions)
        View.new(contributions: contributions).render
      end

      private

      def cast_config()
        username          = ENV["VAR_USERNAME"]
        max_contributions = ENV["VAR_MAX_CONTRIBUTIONS"].to_i

        if username.empty?
          raise ConfigurationError, 'GitHub username is not given.'
        end

        if !max_contributions.positive?
          raise ConfigurationError,
            "Max contributions should be positive integer, but it was #{max_contributions}"
        end

        { username: username, max_contributions: max_contributions }
      end
    end
  end
end

if __FILE__ == $0
  begin
    BitBar::GitHubContribution::App.new().run
  rescue BitBar::GitHubContribution::ConfigurationError => e
    puts <<-EOM.gsub(/^ */, '')
      ⚠️
      ---
      #{e.message}
    EOM
  end
end
