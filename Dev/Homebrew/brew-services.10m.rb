#!/usr/bin/env ruby
# frozen_string_literal: true

# <xbar.title>Brew Services</xbar.title>
# <xbar.version>v3.0.1</xbar.version>
# <xbar.author>Jim Myhrberg</xbar.author>
# <xbar.author.github>jimeh</xbar.author.github>
# <xbar.desc>List and manage Homebrew Services</xbar.desc>
# <xbar.image>https://i.imgur.com/gIQki4q.png</xbar.image>
# <xbar.dependencies>ruby</xbar.dependencies>
# <xbar.abouturl>https://github.com/jimeh/dotfiles/tree/main/xbar</xbar.abouturl>
#
# <xbar.var>boolean(VAR_GROUPS=true): List services in started/stopped groups?</xbar.var>
# <xbar.var>string(VAR_BREW_PATH="/usr/local/bin/brew"): Path to "brew" executable.</xbar.var>
# <xbar.var>string(VAR_HIDDEN_SERVICES=""): Comma-separated list of services to hide.</xbar.var>

# rubocop:disable Lint/ShadowingOuterLocalVariable
# rubocop:disable Metrics/AbcSize
# rubocop:disable Metrics/BlockLength
# rubocop:disable Metrics/ClassLength
# rubocop:disable Metrics/CyclomaticComplexity
# rubocop:disable Metrics/MethodLength
# rubocop:disable Metrics/PerceivedComplexity
# rubocop:disable Style/IfUnlessModifier

require 'open3'
require 'json'

module Xbar
  class Runner
    attr_reader :service

    def initialize(service)
      @service = service
    end

    def run(argv = [])
      return service.run if argv.empty?
      return unless service.respond_to?(argv[0])

      service.public_send(*argv)
    end
  end

  class Config < Hash
    def initialize
      super

      return unless File.exist?(filename)

      merge!(JSON.parse(File.read(filename)))
    end

    def filename
      @filename ||= "#{__FILE__}.vars.json"
    end

    def save
      File.write(filename, JSON.pretty_generate(self))
    end
  end

  class Printer
    attr_reader :nested_level

    SUB_STR = '--'
    SEP_STR = '---'
    PARAM_SEP = '|'

    def initialize(nested_level = 0)
      @nested_level = nested_level
    end

    def item(label = nil, **props)
      print_item(label, **props) if !label.nil? && !label.empty?

      yield(sub_printer) if block_given?
    end

    def separator
      print_item(SEP_STR)
    end
    alias sep separator

    private

    def print_item(text, **props)
      props = props.dup
      alt = props.delete(:alt)

      output = [text]
      unless props.empty?
        props = normalize_props(props)
        output << PARAM_SEP
        output += props.map { |k, v| "#{k}=\"#{v}\"" }
      end

      $stdout.print(SUB_STR * nested_level, output.join(' '))
      $stdout.puts

      return if alt.nil? || alt.empty?

      print_item(alt, **props.merge(alternate: true))
    end

    def plugin_refresh_uri
      @plugin_refresh_uri ||= 'xbar://app.xbarapp.com/refreshPlugin' \
                              "?path=#{File.basename(__FILE__)}"
    end

    def normalize_props(props = {})
      props = props.dup

      if props[:rpc] && props[:shell].nil?
        props[:shell] = [__FILE__] + props[:rpc]
        props.delete(:rpc)
      end

      if props[:shell].is_a?(Array)
        cmd = props[:shell]
        props[:shell] = cmd[0]
        cmd[1..].each_with_index do |c, i|
          props["param#{i + 1}".to_sym] = c
        end
      end

      # Refresh Xbar after shell command has run in terminal
      if props[:terminal] && props[:refresh] && props[:shell]
        props[:refresh] = false
        i = 1
        i += 1 while props.key?("param#{i}".to_sym)
        props["param#{i}".to_sym] = ';'
        props["param#{i + 1}".to_sym] = 'open'
        props["param#{i + 2}".to_sym] = '-jg'
        props["param#{i + 3}".to_sym] = "'#{plugin_refresh_uri}'"
      end

      props
    end

    def sub_printer
      @sub_printer || self.class.new(nested_level + 1)
    end
  end
end

module Brew
  class CommandError < StandardError; end

  class Common
    def self.prefix(value = nil)
      return @prefix if value.nil? || value == ''

      @prefix = value
    end

    private

    def prefix
      self.class.prefix
    end

    def default_printer
      @default_printer ||= ::Xbar::Printer.new
    end

    def cmd(*args)
      out, err, s = Open3.capture3(*args)
      raise CommandError, "#{args.join(' ')}: #{err}" if s.exitstatus != 0

      out
    end

    def brew_path
      @brew_path ||= ENV.fetch('VAR_BREW_PATH', '/usr/local/bin/brew')
    end

    def brew_check(printer = nil)
      printer ||= default_printer
      return if File.exist?(brew_path)

      printer.item("#{prefix}↑:warning:", dropdown: false)
      printer.sep
      printer.item('Homebrew not found', color: 'red')
      printer.item("Executable \"#{brew_path}\" does not exist.")
      printer.sep
      printer.item(
        'Visit https://brew.sh/ for installation instructions',
        href: 'https://brew.sh'
      )

      exit 0
    end
  end

  class Service
    attr_reader :name, :status, :user, :file, :exit_code, :hidden

    def initialize(args = {})
      @name = args.key?('name') ? args['name'] : args[:name]
      @status = args.key?('status') ? args['status'] : args[:status]
      @user = args.key?('user') ? args['user'] : args[:user]
      @file = args.key?('file') ? args['file'] : args[:file]
      @exit_code = args.key?('exit_code') ? args['exit_code'] : args[:exit_code]
      @hidden = (args.key?('hidden') ? args['hidden'] : args[:hidden]) || false
    end

    def started?
      @started ||= %w[started scheduled].include?(@status.downcase)
    end

    def stopped?
      @stopped ||= %w[stopped none].include?(@status.downcase)
    end

    def error?
      @error ||= @status.downcase == 'error'
    end

    def unknown_status?
      @unknown_status ||= @status.downcase == 'unknown'
    end

    def hidden?
      @hidden
    end
  end

  class ServiceList < Array
    def initialize(items)
      super

      replace(items)
    end

    def select
      self.class.new(super)
    end

    def reject
      self.class.new(super)
    end

    def started
      @started ||= select(&:started?)
    end

    def stopped
      @stopped ||= select(&:stopped?)
    end

    def errored
      @errored ||= select(&:error?)
    end

    def unknown_status
      @unknown_status ||= select(&:unknown_status?)
    end

    def hidden
      @hidden ||= select(&:hidden?)
    end

    def visible
      @visible ||= reject(&:hidden?)
    end
  end

  class Services < Common
    prefix ':bulb:'

    def run
      printer = default_printer

      brew_check(printer)

      visible = all_services.visible

      printer.item("#{prefix}#{visible.started.size}", dropdown: false)
      printer.sep
      printer.item('Brew Services')

      printer.item(status_label(visible)) do |printer|
        printer.sep
        printer.item(':hourglass: Refresh', refresh: true)

        unless all_services.empty?
          printer.sep
          if visible.stopped.size.positive?
            printer.item(
              "Start All (#{visible.stopped.size} services)",
              terminal: false, refresh: true,
              shell: [brew_path, 'services', 'start', '--all']
            )
          else
            printer.item("Start All (#{visible.stopped.size} services)")
          end
          if visible.started.size.positive?
            printer.item(
              "Stop All (#{visible.started.size} services)",
              terminal: false, refresh: true,
              shell: [brew_path, 'services', 'stop', '--all']
            )
          else
            printer.item("Stop All (#{visible.started.size} services)")
          end
          if visible.size.positive?
            count = visible.started.size + visible.stopped.size
            printer.item(
              "Restart All (#{count} services)",
              terminal: false, refresh: true,
              shell: [brew_path, 'services', 'restart', '--all']
            )
          else
            printer.item("Restart All (#{visible.size} services)")
          end
        end

        printer.sep
        if use_groups?
          printer.item('Disable groups', rpc: ['disable_groups'], refresh: true)
        else
          printer.item('Enable groups', rpc: ['enable_groups'], refresh: true)
        end
      end

      print_services(printer, visible)

      hidden = all_services.hidden
      return if hidden.empty?

      printer.sep
      printer.item("Hidden (#{hidden.size})") do |printer|
        unless use_groups?
          printer.item(status_label(hidden))
        end
        print_services(printer, hidden)
      end
    end

    def enable_groups
      config['VAR_GROUPS'] = true
      config.save
    end

    def disable_groups
      config['VAR_GROUPS'] = false
      config.save
    end

    def hide(*args)
      hidden = config['VAR_HIDDEN_SERVICES']&.split(',')&.map(&:strip) || []
      hidden += args

      config['VAR_HIDDEN_SERVICES'] = hidden.uniq.sort.join(',')
      config.save
    end

    def show(*args)
      hidden = config['VAR_HIDDEN_SERVICES']&.split(',')&.map(&:strip) || []
      hidden -= args

      config['VAR_HIDDEN_SERVICES'] = hidden.uniq.sort.join(',')
      config.save
    end

    private

    def config
      @config ||= Xbar::Config.new
    end

    def use_groups?
      [true, 'true'].include?(config.fetch('VAR_GROUPS', 'true'))
    end

    def status_label(services)
      label = []
      if services.started.size.positive?
        label << "#{services.started.size} started"
      end
      if services.stopped.size.positive?
        label << "#{services.stopped.size} stopped"
      end
      if services.errored.size.positive?
        label << "#{services.errored.size} error"
      end
      if services.unknown_status.size.positive?
        label << "#{services.unknown_status.size} unknown"
      end

      label = ['no services available'] if label.empty?
      label.join(', ')
    end

    def print_services(printer, services)
      return print_service_groups(printer, services) if use_groups?

      printer.sep
      services.each do |service|
        print_service(printer, service)
      end
    end

    def print_service_groups(printer, services)
      if services.started.size.positive?
        printer.sep
        printer.item("Started (#{services.started.size}):")
        services.started.each do |service|
          print_service(printer, service)
        end
      end
      if services.stopped.size.positive?
        printer.sep
        printer.item("Stopped (#{services.stopped.size}):")
        services.stopped.each do |service|
          print_service(printer, service)
        end
      end
      if services.errored.size.positive?
        printer.sep
        printer.item("Error (#{services.errored.size}):")
        services.errored.each do |service|
          print_service(printer, service)
        end
      end
      if services.unknown_status.size.positive?
        printer.sep
        printer.item("Unknown Status (#{services.unknown_status.size}):")
        services.unknown_status.each do |service|
          print_service(printer, service)
        end
      end
    end

    def print_service(printer, service)
      icon = if service.started?
               ':white_check_mark:'
             elsif service.stopped?
               ':ballot_box_with_check:'
             elsif service.error?
               ':warning:'
             elsif service.unknown_status?
               ':question:'
             end

      printer.item("#{icon} #{service.name}") do |printer|
        if service.stopped? || service.unknown_status?
          printer.item(
            'Start',
            terminal: false, refresh: true,
            shell: [brew_path, 'services', 'start', service.name]
          )
        end
        if service.started? || service.error? || service.unknown_status?
          printer.item(
            'Stop',
            terminal: false, refresh: true,
            shell: [brew_path, 'services', 'stop', service.name]
          )
          printer.item(
            'Restart',
            terminal: false, refresh: true,
            shell: [brew_path, 'services', 'restart', service.name]
          )
        end

        printer.sep
        printer.item("Status: #{service.status}")
        printer.item("User: #{service.user || '<none>'}")
        if !service.exit_code.nil? && !service.started?
          printer.item("Exit code: #{service.exit_code}")
        end

        printer.sep
        if service.hidden?
          printer.item('Unhide', rpc: ['show', service.name], refresh: true)
        else
          printer.item('Hide', rpc: ['hide', service.name], refresh: true)
        end

        if service.stopped?
          printer.item('Uninstall') do |printer|
            printer.item('Are you sure?')
            printer.sep
            printer.item(
              'Yes',
              terminal: true, refresh: true,
              shell: [brew_path, 'uninstall', service.name]
            )
          end
        end
      end
    end

    def all_services
      return @all_services if @all_services

      output = cmd(brew_path, 'services', 'list', '--json')
      return ServiceList.new([]) if output == ''

      data = JSON.parse(output)

      @all_services = ServiceList.new(
        data.each_with_object([]) do |item, memo|
          item['hidden'] = hidden_services.include?(item['name'])
          memo.push(Service.new(item))
        end
      )
    end

    def hidden_services
      @hidden_services ||= config.fetch('VAR_HIDDEN_SERVICES', '')
                                 .split(',').uniq.map(&:strip)
    end
  end
end

begin
  services = Brew::Services.new
  Xbar::Runner.new(services).run(ARGV)
rescue StandardError => e
  puts "ERROR: #{e.message}:\n\t#{e.backtrace.join("\n\t")}"
  exit 1
end

# rubocop:enable Style/IfUnlessModifier
# rubocop:enable Metrics/PerceivedComplexity
# rubocop:enable Metrics/MethodLength
# rubocop:enable Metrics/CyclomaticComplexity
# rubocop:enable Metrics/ClassLength
# rubocop:enable Metrics/BlockLength
# rubocop:enable Metrics/AbcSize
# rubocop:enable Lint/ShadowingOuterLocalVariable
