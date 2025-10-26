#!/usr/bin/env ruby
# frozen_string_literal: true

# rubocop:disable Layout/LineLength

# <xbar.title>Brew Updates</xbar.title>
# <xbar.version>v2.7.3</xbar.version>
# <xbar.author>Jim Myhrberg</xbar.author>
# <xbar.author.github>jimeh</xbar.author.github>
# <xbar.desc>List and manage outdated Homebrew formulas and casks</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/jimeh/dotfiles/70e616332e9cc196d365e5375156b27ce8c9451e/xbar/img/brew-updates.png</xbar.image>
# <xbar.dependencies>ruby</xbar.dependencies>
# <xbar.abouturl>https://github.com/jimeh/dotfiles/tree/main/xbar</xbar.abouturl>
#
# <xbar.var>string(VAR_BREW_PATH=""): Path to "brew" executable.</xbar.var>
# <xbar.var>boolean(VAR_GREEDY_LATEST=false): Run "brew outdated" with --greedy-latest flag..</xbar.var>
# <xbar.var>boolean(VAR_GREEDY_AUTO_UPDATES=false): Run "brew outdated" with --greedy-auto-updates flag.</xbar.var>
# <xbar.var>boolean(VAR_POST_RUN_CLEANUP=false): Run "brew cleanup" after package changes.</xbar.var>
# <xbar.var>boolean(VAR_POST_RUN_DOCTOR=false): Run "brew cleanup" after package changes.</xbar.var>
# <xbar.var>string(VAR_UPGRADE_ALL_EXCLUDE=""): Comma-separated list formulas/casks to exclude from upgrade all operations.</xbar.var>

# rubocop:enable Layout/LineLength

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

# Brew module contains classes for managing Homebrew formulas and casks.
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

      printer.item("#{prefix}↑⚠️", dropdown: false)
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

  class Package
    attr_reader :name, :installed_versions, :latest_version,
                :pinned, :pinned_version

    def initialize(attributes = {})
      @name = attributes['name']
      @installed_versions = Array(attributes['installed_versions'])
      @latest_version = attributes['current_version']
      @pinned = attributes['pinned']
      @pinned_version = attributes['pinned_version']
    end

    def current_version
      installed_versions.last
    end
  end

  class Formula < Package; end
  class Cask < Package; end

  class FormulaUpdates < Common
    prefix '🍻'

    def run
      brew_check(printer)
      brew_update

      printer.item("#{prefix}↑#{formulas.size + casks.size}", dropdown: false)
      printer.sep
      printer.item('Brew Updates️') do |printer|
        print_settings(printer)
      end

      printer.item(status_label) do |printer|
        printer.item('⏳ Refresh', alt: '⏳ Refresh (⌘R)', refresh: true)

        printer.sep
        all_formulas = formulas.reject { |f| upgrade_all_exclude?(f.name) }
        all_casks = casks.reject { |c| upgrade_all_exclude?(c.name) }
        excluded = (formulas - all_formulas) + (casks - all_casks)

        if all_formulas.size.positive? && all_casks.size.positive?
          cmds = []
          if all_formulas.size.positive?
            cmds += [brew_path, 'upgrade', '--formula'] +
                    all_formulas.map(&:name)
          end

          if all_casks.size.positive?
            cmds << '&&' if cmds.size.positive?
            cmds += [brew_path, 'upgrade', '--cask'] +
                    all_casks.map(&:name)
          end

          printer.item(
            "⬆️ Upgrade All (#{all_formulas.size + all_casks.size})",
            terminal: true, refresh: true,
            shell: (cmds + post_commands).flatten
          )
        end
        if all_formulas.size.positive?
          names = all_formulas.map(&:name)
          printer.item(
            "⬆️ Upgrade All Formulas (#{all_formulas.size})",
            terminal: true, refresh: true,
            shell: [
              brew_path, 'upgrade', '--formula'
            ] + names + post_commands
          )
        end
        if all_casks.size.positive?
          names = all_casks.map(&:name)
          printer.item(
            "⬆️ Upgrade All Casks (#{all_casks.size})",
            terminal: true, refresh: true,
            shell: [
              brew_path, 'upgrade', '--cask'
            ] + names + post_commands
          )
        end
        if excluded.size.positive?
          printer.sep
          printer.item("Excluded (#{excluded.size}):")
          excluded.sort_by(&:name).each do |item|
            type = item.is_a?(Formula) ? 'Formula' : 'Cask'
            printer.item("#{item.name} (#{type})")
          end
        end
      end

      print_formulas(printer)
      print_casks(printer)
      print_pinned(printer)
      printer.sep
    end

    def greedy_latest(*args)
      config['VAR_GREEDY_LATEST'] = truthy?(args.first)
      config.save
    end

    def greedy_auto_updates(*args)
      config['VAR_GREEDY_AUTO_UPDATES'] = truthy?(args.first)
      config.save
    end

    def post_run_cleanup(*args)
      config['VAR_POST_RUN_CLEANUP'] = truthy?(args.first)
      config.save
    end

    def post_run_doctor(*args)
      config['VAR_POST_RUN_DOCTOR'] = truthy?(args.first)
      config.save
    end

    def exclude_upgrade_all(*args)
      exclude = upgrade_all_exclude.clone
      exclude += args.map(&:strip).reject(&:empty?)

      config['VAR_UPGRADE_ALL_EXCLUDE'] = exclude.sort.join(',')
      config.save
    end

    def include_upgrade_all(*args)
      exclude = upgrade_all_exclude.clone
      exclude -= args.map(&:strip).reject(&:empty?)

      config['VAR_UPGRADE_ALL_EXCLUDE'] = exclude.sort.join(',')
      config.save
    end

    private

    def greedy_latest?
      @greedy_latest ||= truthy?(config['VAR_GREEDY_LATEST'])
    end

    def greedy_auto_updates?
      @greedy_auto_updates ||= truthy?(config['VAR_GREEDY_AUTO_UPDATES'])
    end

    def post_run_cleanup?
      @post_run_cleanup ||= truthy?(config['VAR_POST_RUN_CLEANUP'])
    end

    def post_run_doctor?
      @post_run_doctor ||= truthy?(config['VAR_POST_RUN_DOCTOR'])
    end

    def upgrade_all_exclude?(name)
      upgrade_all_exclude.include?(name)
    end

    def upgrade_all_exclude
      @upgrade_all_exclude ||= config.as_set('VAR_UPGRADE_ALL_EXCLUDE')
    end

    def truthy?(value)
      %w[true yes 1 on y t].include?(value.to_s.downcase)
    end

    def brew_update
      cmd(brew_path, 'update')
    rescue Xbar::CommandError
      # Continue as if nothing happened when brew update fails, as it likely
      # to be due to another update process is already running.
    end

    def status_label
      label = []
      label << "#{formulas.size} formulas" if formulas.size.positive?
      label << "#{casks.size} casks" if casks.size.positive?
      label << "#{pinned.size} pinned" if pinned.size.positive?

      label = ['no updates available'] if label.empty?
      label.join(', ')
    end

    def print_settings(printer)
      printer.item('Settings')
      printer.sep

      print_rpc_toggle(
        printer, 'Greedy: Latest', 'greedy_latest', greedy_latest?
      )

      print_rpc_toggle(
        printer, 'Greedy: Auto Updates', 'greedy_auto_updates',
        greedy_auto_updates?
      )

      print_rpc_toggle(
        printer, 'Post Run: Cleanup', 'post_run_cleanup', post_run_cleanup?
      )

      print_rpc_toggle(
        printer, 'Post-Run: Doctor', 'post_run_doctor', post_run_doctor?
      )
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

    def print_formulas(printer)
      return unless formulas.size.positive?

      printer.sep
      printer.item("Formulas (#{formulas.size}):")
      formulas.each do |formula|
        name = formula.name
        name += ' ⤫' if upgrade_all_exclude?(name)
        printer.item(name) do |printer|
          printer.item(
            '⬆️ Upgrade',
            alt: '⬆️ Upgrade ' \
                 "(#{formula.current_version} → #{formula.latest_version})",
            terminal: true, refresh: true,
            shell: [
              brew_path, 'upgrade', '--formula', formula.name
            ] + post_commands
          )
          printer.sep
          printer.item("→ Installed: #{formula.installed_versions.join(', ')}")
          printer.item("↑ Latest: #{formula.latest_version}")
          printer.sep
          printer.item(
            '📌 Pin',
            alt: "📌 Pin (to #{formula.current_version})",
            terminal: false, refresh: true,
            shell: [brew_path, 'pin', formula.name]
          )
          if upgrade_all_exclude?(formula.name)
            printer.item(
              '✅ Upgrade All: Exclude',
              terminal: false, refresh: true,
              rpc: ['include_upgrade_all', formula.name]
            )
          else
            printer.item(
              '☑️ Upgrade All: Exclude ',
              terminal: false, refresh: true,
              rpc: ['exclude_upgrade_all', formula.name]
            )
          end
          printer.item('🚫 Uninstall') do |printer|
            printer.item('Are you sure?')
            printer.item(
              'Yes',
              terminal: true, refresh: true,
              shell: [
                brew_path, 'uninstall', '--formula', formula.name
              ] + post_commands
            )
          end
        end
      end
    end

    def print_casks(printer)
      return unless casks.size.positive?

      printer.sep
      printer.item("Casks (#{casks.size}):")
      casks.each do |cask|
        name = cask.name
        name += ' ⤫' if upgrade_all_exclude?(name)
        printer.item(name) do |printer|
          printer.item(
            '⬆️ Upgrade',
            alt: '⬆️ Upgrade '\
                 "(#{cask.current_version} → #{cask.latest_version})",
            terminal: true, refresh: true,
            shell: [
              brew_path, 'upgrade', '--cask', cask.name
            ] + post_commands
          )
          printer.sep
          printer.item("→ Installed: #{cask.installed_versions.join(', ')}")
          printer.item("↑ Latest: #{cask.latest_version}")
          printer.sep
          if upgrade_all_exclude?(cask.name)
            printer.item(
              '✅ Upgrade All: Exclude',
              terminal: false, refresh: true,
              rpc: ['include_upgrade_all', cask.name]
            )
          else
            printer.item(
              '☑️ Upgrade All: Exclude',
              terminal: false, refresh: true,
              rpc: ['exclude_upgrade_all', cask.name]
            )
          end
          printer.item('🚫 Uninstall') do |printer|
            printer.item('Are you sure?')
            printer.sep
            printer.item(
              'Yes',
              terminal: true, refresh: true,
              shell: [
                brew_path, 'uninstall', '--cask', cask.name
              ] + post_commands
            )
          end
        end
      end
    end

    def print_pinned(printer)
      return unless pinned.size.positive?

      printer.sep
      printer.item("Pinned Formulas (#{pinned.size}):")
      pinned.each do |formula|
        printer.item(formula.name) do |printer|
          printer.item(
            '⬆ Upgrade',
            alt: '⬆ Upgrade ' \
                 "(#{formula.current_version} → #{formula.latest_version})"
          )
          printer.sep
          printer.item("→ Pinned: #{formula.pinned_version}")
          if formula.installed_versions.size > 1
            printer.item("→ Installed: #{formula.installed_versions.join(', ')}")
          end
          printer.item("↑ Latest: #{formula.latest_version}")
          printer.sep
          printer.item(
            '📌 Unpin',
            terminal: false, refresh: true,
            shell: [brew_path, 'unpin', formula.name]
          )
          printer.item('🚫 Uninstall') do |printer|
            printer.item('Are you sure?')
            printer.item(
              'Yes',
              terminal: true, refresh: true,
              shell: [
                brew_path, 'uninstall', '--formula', formula.name
              ] + post_commands
            )
          end
        end
      end
    end

    def post_commands
      cmds = []
      cmds += ['&&', brew_path, 'cleanup'] if post_run_cleanup?
      cmds += ['&&', brew_path, 'doctor'] if post_run_doctor?

      cmds
    end

    def formulas
      @formulas ||= all_formulas.reject(&:pinned)
    end

    def pinned
      @pinned ||= all_formulas.select(&:pinned)
    end

    def all_formulas
      @all_formulas ||= outdated['formulae'].map { |f| Formula.new(f) }
    end

    def casks
      @casks ||= outdated['casks'].map { |c| Cask.new(c) }
    end

    def greedy_args
      args = []
      args << '--greedy-latest' if greedy_latest?
      args << '--greedy-auto-updates' if greedy_auto_updates?
      args
    end

    def outdated_args
      ['outdated', greedy_args, '--json=v2'].flatten.compact
    end

    def outdated
      @outdated ||= JSON.parse(cmd(brew_path, *outdated_args))
    end
  end
end

begin
  service = Brew::FormulaUpdates.new
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
