#!/usr/bin/env ruby
# frozen_string_literal: true

# <xbar.title>Brew Services</xbar.title>
# <xbar.version>v3.2.3</xbar.version>
# <xbar.author>Jim Myhrberg</xbar.author>
# <xbar.author.github>jimeh</xbar.author.github>
# <xbar.desc>List and manage Homebrew Services</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/jimeh/dotfiles/70e616332e9cc196d365e5375156b27ce8c9451e/xbar/img/brew-services.png</xbar.image>
# <xbar.dependencies>ruby</xbar.dependencies>
# <xbar.abouturl>https://github.com/jimeh/dotfiles/tree/main/xbar</xbar.abouturl>
#
# <xbar.var>boolean(VAR_GROUPS=true): List services in started/stopped groups?</xbar.var>
# <xbar.var>string(VAR_BREW_PATH=""): Path to "brew" executable.</xbar.var>
# <xbar.var>string(VAR_HIDDEN_SERVICES=""): Comma-separated list of services to hide.</xbar.var>

# rubocop:disable Metrics/AbcSize
# rubocop:disable Metrics/BlockLength
# rubocop:disable Metrics/ClassLength
# rubocop:disable Metrics/CyclomaticComplexity
# rubocop:disable Metrics/MethodLength
# rubocop:disable Metrics/PerceivedComplexity
# rubocop:disable Style/IfUnlessModifier

require 'fileutils'
require 'json'
require 'open3'
require 'set'

# Xbar is a tiny helper library for creating Xbar and SwiftBar plugins in Ruby.
module Xbar
  class CommandError < StandardError; end
  class RPCError < StandardError; end

  module Helpers
    def plugin_data_path
      @plugin_data_path ||= swiftbar_data_path || File.dirname(__FILE__)
    end

    def plugin_file_path
      @plugin_file_path ||= swiftbar_plugin_path || __FILE__
    end

    def plugin_filename
      @plugin_filename ||= File.basename(plugin_file_path)
    end

    def plugin_name
      @plugin_name ||= begin
        parts = plugin_filename.split('.')
        if parts.size < 3
          raise "Invalid plugin name: #{plugin_filename}"
        end

        parts[0..-3].join('.')
      end
    end

    def swiftbar?
      ENV['SWIFTBAR'] == '1'
    end

    def swiftbar_cache_path
      @swiftbar_cache_path ||= ENV['SWIFTBAR_PLUGIN_CACHE_PATH'] if swiftbar?
    end

    def swiftbar_data_path
      @swiftbar_data_path ||= ENV['SWIFTBAR_PLUGIN_DATA_PATH'] if swiftbar?
    end

    def swiftbar_plugin_path
      @swiftbar_plugin_path ||= ENV['SWIFTBAR_PLUGIN_PATH'] if swiftbar?
    end
  end

  module Service
    include Helpers

    private

    def config
      @config ||= Xbar::Config.new
    end

    def printer
      @printer ||= ::Xbar::Printer.new
    end

    def cmd(*args, dir: nil)
      opts = {}
      opts[:chdir] = File.expand_path(dir) if dir

      out, err, s = Open3.capture3(*args, opts)
      if s.exitstatus != 0
        msg = "Command failed: #{args.join(' ')}"
        msg += ": #{err}" unless err.empty?

        raise CommandError, msg
      end

      out
    end
  end

  class Runner
    attr_reader :service

    def initialize(service)
      @service = service
    end

    def run(argv = [])
      return service.run if argv.empty?
      unless service.respond_to?(argv[0])
        raise RPCError, "Unknown RPC method: #{argv[0]}"
      end

      service.public_send(*argv)
    end
  end

  # Config is a simple wrapper around a JSON file that contains the plugin's
  # configuration. It is compatible with Xbar's behavior of loading environment
  # variables from it. We don't rely on that however and directly read the file
  # ourselves.
  #
  # In SwiftBar, we use `SWIFTBAR_PLUGIN_DATA_PATH` to determine where to store
  # the configuration in a `config.json` file.
  class Config < Hash
    include Helpers

    def initialize
      super

      return unless File.exist?(filename)

      merge!(JSON.parse(File.read(filename)))
    end

    def as_set(name)
      values = self[name]&.to_s&.split(',')&.map(&:strip)&.reject(&:empty?)

      ::Set.new(values || [])
    end

    def filename
      @filename ||= File.join(
        plugin_data_path,
        swiftbar? ? 'config.json' : "#{plugin_filename}.vars.json"
      )
    end

    def dirname
      @dirname ||= File.dirname(filename)
    end

    def save
      FileUtils.mkdir_p(dirname)
      File.write(filename, JSON.pretty_generate(self))
    end
  end

  class Printer
    include Helpers

    attr_reader :nested_level

    SUB_STR = '--'
    SEP_STR = '---'
    PARAM_SEP = '|'

    def initialize(nested_level = 0)
      @nested_level = nested_level
    end

    def item(label = nil, **props)
      return if label.nil? || label.empty?

      props = normalize_props(props.dup)
      alt = props.delete(:alt)
      nested_items = props.delete(:nested) || :both

      yield_main = block_given? && [:main, :both].include?(nested_items)
      yield_alt = block_given? && [:alt, :both].include?(nested_items)

      print_item(label, **props)
      yield(sub_printer) if yield_main

      return if alt.nil? || alt.strip.empty?

      print_item(alt, **props.merge(alternate: true))
      yield(sub_printer) if yield_alt
    end

    def separator
      print_item(SEP_STR)
    end
    alias sep separator

    private

    def print_item(text, **props)
      output = [text]
      unless props.empty?
        output << PARAM_SEP
        output += props.map { |k, v| "#{k}=\"#{v}\"" }
      end

      $stdout.print(SUB_STR * nested_level, output.join(' '))
      $stdout.puts
    end

    def plugin_refresh_uri
      return @plugin_refresh_uri if @plugin_refresh_uri

      @plugin_refresh_uri = if swiftbar?
                              "swiftbar://refreshplugin?name=#{plugin_name}"
                            else
                              'xbar://app.xbarapp.com/refreshPlugin' \
                                                    "?path=#{plugin_filename}"
                            end
    end

    def normalize_props(props = {})
      props = props.dup

      # Explicitly set terminal to false for SwiftBar, as it seems to default
      # to true when not specified. At least with SwiftBar 2.0.1.
      if swiftbar? && !props.key?(:terminal)
        props[:terminal] = false
      end

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

      # Always refresh SwiftBar via refresh plugin URI so as to temporarily
      # disable the menu bar icon while refresh is running.
      #
      # For Xbar this does not work when terminal is false, so we only trigger
      # refresh via the plugin refresh URI when terminal is true.
      if props[:refresh] && props[:shell] && (swiftbar? || props[:terminal])
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

#
# ------------------------------------------------------------------------------
#

# Brew module contains classes for managing Homebrew services.
module Brew
  class Common
    include Xbar::Service

    def self.prefix(value = nil)
      return @prefix if value.nil? || value == ''

      @prefix = value
    end

    private

    def prefix
      self.class.prefix
    end

    def brew_path
      @brew_path ||= brew_path_from_env ||
                     brew_path_from_which ||
                     brew_path_from_fs_check ||
                     raise('Unable to find "brew" executable')
    end

    def brew_path_from_env
      env_value = config['VAR_BREW_PATH']&.to_s&.strip || ''

      return if env_value == ''
      return unless File.exist?(env_value)

      env_value
    end

    def brew_path_from_which
      detect = cmd('which', 'brew').strip
      return if detect == ''

      detect
    rescue Xbar::CommandError
      nil
    end

    def brew_path_from_fs_check
      ['/usr/local/bin/brew', '/opt/homebrew/bin/brew'].each do |path|
        return path if File.exist?(path)
      end

      nil
    end

    def brew_check(printer = nil)
      printer ||= default_printer
      return if File.exist?(brew_path)

      printer.item("#{prefix}↑⚠️:", dropdown: false)
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
    prefix '💡'

    def run
      brew_check(printer)

      visible = all_services.visible

      printer.item("#{prefix}#{visible.started.size}", dropdown: false)
      printer.sep
      printer.item('Brew Services') do |printer|
        print_settings(printer)
      end

      printer.item(status_label(visible)) do |printer|
        printer.sep
        printer.item('⏳ Refresh', alt: '⏳ Refresh (⌘R)', refresh: true)

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

    def use_groups(*args)
      config['VAR_GROUPS'] = truthy?(args.first)
      config.save
    end

    def hide(*args)
      hidden = hidden_services.clone
      hidden += args.map(&:strip).reject(&:empty?)

      config['VAR_HIDDEN_SERVICES'] = hidden.sort.join(',')
      config.save
    end

    def show(*args)
      hidden = hidden_services.clone
      hidden -= args.map(&:strip).reject(&:empty?)

      config['VAR_HIDDEN_SERVICES'] = hidden.sort.join(',')
      config.save
    end

    private

    def use_groups?
      [true, 'true'].include?(config.fetch('VAR_GROUPS', 'true'))
    end

    def hidden_services
      @hidden_services ||= config.as_set('VAR_HIDDEN_SERVICES')
    end

    def truthy?(value)
      %w[true yes 1 on y t].include?(value.to_s.downcase)
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

    def print_settings(printer)
      printer.item('Settings')
      printer.sep

      print_rpc_toggle(printer, 'Use groups', 'use_groups', use_groups?)
    end

    def print_rpc_toggle(printer, name, rpc, current_value)
      if current_value
        icon = '✅'
        value = 'false'
      else
        icon = '☑️'
        value = 'true'
      end

      printer.item("#{icon} #{name}", rpc: [rpc, value], refresh: true)
    end

    def print_services(printer, services)
      return print_service_groups(printer, services) if use_groups?

      printer.sep
      services.each do |service|
        print_service(printer, service)
      end
    end

    # rubocop:disable Style/GuardClause
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
    # rubocop:enable Style/GuardClause

    def print_service(printer, service)
      icon = if service.started?
               '🟢'
             elsif service.stopped?
               '🔴'
             elsif service.error?
               '⚠️'
             elsif service.unknown_status?
               '❓'
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
  end
end

begin
  service = Brew::Services.new
  Xbar::Runner.new(service).run(ARGV)
rescue StandardError => e
  puts ":warning: #{File.basename(__FILE__)}"
  puts '---'
  puts 'exit status 1'
  puts '---'
  puts 'Error:'
  puts e.message
  e.backtrace.each do |line|
    puts "--#{line}"
  end
  exit 0
end

# rubocop:enable Style/IfUnlessModifier
# rubocop:enable Metrics/PerceivedComplexity
# rubocop:enable Metrics/MethodLength
# rubocop:enable Metrics/CyclomaticComplexity
# rubocop:enable Metrics/ClassLength
# rubocop:enable Metrics/BlockLength
# rubocop:enable Metrics/AbcSize
