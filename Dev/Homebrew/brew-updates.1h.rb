#!/usr/bin/env ruby
# frozen_string_literal: true

# <xbar.title>Brew Updates</xbar.title>
# <xbar.version>v2.4.0</xbar.version>
# <xbar.author>Jim Myhrberg</xbar.author>
# <xbar.author.github>jimeh</xbar.author.github>
# <xbar.desc>List and manage outdated Homebrew formulas and casks</xbar.desc>
# <xbar.image>https://i.imgur.com/HLKYqYc.png</xbar.image>
# <xbar.dependencies>ruby</xbar.dependencies>
# <xbar.abouturl>https://github.com/jimeh/dotfiles/tree/main/xbar</xbar.abouturl>
#
# <xbar.var>string(VAR_BREW_PATH=""): Path to "brew" executable.</xbar.var>
# <xbar.var>boolean(VAR_GREEDY=false): Pass --greedy to brew outdated command</xbar.var>

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
      @brew_path ||= brew_path_from_env ||
                     brew_path_from_which ||
                     brew_path_from_fs_check ||
                     raise('Unable to find "brew" executable')
    end

    def brew_path_from_env
      return if ENV['VAR_BREW_PATH'].to_s == ''

      ENV['VAR_BREW_PATH']
    end

    def brew_path_from_which
      detect = cmd('which', 'brew').strip
      return if detect == ''

      detect
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

    def brew_update
      cmd(brew_path, 'update')
    rescue CommandError
      # Continue as if nothing happened when brew update fails, as it likely
      # to be due to another update process is already running.
    end
  end

  class Formula
    attr_reader :name, :installed_versions, :latest_version,
                :pinned, :pinned_version

    def initialize(attributes = {})
      @name = attributes['name']
      @installed_versions = attributes['installed_versions']
      @latest_version = attributes['current_version']
      @pinned = attributes['pinned']
      @pinned_version = attributes['pinned_version']
    end

    def current_version
      installed_versions.last
    end
  end

  class Cask
    attr_reader :name, :installed_version, :latest_version

    def initialize(attributes = {})
      @name = attributes['name']
      @installed_version = attributes['installed_versions']
      @latest_version = attributes['current_version']
    end

    alias current_version installed_version
  end

  class FormulaUpdates < Common
    prefix ':beers:'

    def run
      printer = default_printer

      brew_check(printer)
      brew_update

      printer.item("#{prefix}↑#{formulas.size + casks.size}", dropdown: false)
      printer.sep
      printer.item('Brew Updates')

      printer.item(status_label) do |printer|
        printer.item(':hourglass: Refresh', refresh: true)
        printer.sep
        if formulas.size.positive? && casks.size.positive?
          printer.item(
            "Upgrade All (#{formulas.size + casks.size})",
            terminal: true, refresh: true,
            shell: [brew_path, 'upgrade'] +
            formulas.map(&:name) + casks.map(&:name)
          )
        end
        if formulas.size.positive?
          printer.item(
            "Upgrade All Formulas (#{formulas.size})",
            terminal: true, refresh: true,
            shell: [brew_path, 'upgrade', '--formula'] + formulas.map(&:name)
          )
        end
        if casks.size.positive?
          printer.item(
            "Upgrade All Casks (#{casks.size})",
            terminal: true, refresh: true,
            shell: [brew_path, 'upgrade', '--cask'] + casks.map(&:name)
          )
        end

        printer.sep
        if use_greedy?
          printer.item('Disable greedy', rpc: ['disable_greedy'], refresh: true)
        else
          printer.item('Enable greedy', rpc: ['enable_greedy'], refresh: true)
        end
      end

      print_formulas(printer)
      print_casks(printer)
      print_pinned(printer)
      printer.sep
    end

    def enable_greedy
      config['VAR_GREEDY'] = true
      config.save
    end

    def disable_greedy
      config['VAR_GREEDY'] = false
      config.save
    end

    private

    def config
      @config ||= Xbar::Config.new
    end

    def use_greedy?
      [true, 'true'].include?(config.fetch('VAR_GREEDY', 'false'))
    end

    def status_label
      label = []
      label << "#{formulas.size} formulas" if formulas.size.positive?
      label << "#{casks.size} casks" if casks.size.positive?
      label << "#{pinned.size} pinned" if pinned.size.positive?

      label = ['no updates available'] if label.empty?
      label.join(', ')
    end

    def print_formulas(printer)
      return unless formulas.size.positive?

      printer.sep
      printer.item("Formulas (#{formulas.size}):")
      formulas.each do |formula|
        printer.item(formula.name) do |printer|
          printer.item(
            'Upgrade',
            alt: 'Upgrade ' \
                 "(#{formula.current_version} → #{formula.latest_version})",
            terminal: true, refresh: true,
            shell: [brew_path, 'upgrade', formula.name]
          )
          printer.sep
          printer.item("Installed: #{formula.installed_versions.join(', ')}")
          printer.item("Latest: #{formula.latest_version}")
          printer.sep
          printer.item(
            'Pin',
            alt: "Pin (to #{formula.current_version})",
            terminal: false, refresh: true,
            shell: [brew_path, 'pin', formula.name]
          )
          printer.item('Uninstall') do |printer|
            printer.item('Are you sure?')
            printer.item(
              'Yes',
              terminal: true, refresh: true,
              shell: [brew_path, 'uninstall', formula.name]
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
        printer.item(cask.name) do |printer|
          printer.item(
            'Upgrade',
            alt: 'Upgrade '\
                 "(#{cask.current_version} → #{cask.latest_version})",
            terminal: true, refresh: true,
            shell: [brew_path, 'upgrade', '--cask', cask.name]
          )
          printer.sep
          printer.item("Installed: #{cask.installed_version}")
          printer.item("Latest: #{cask.latest_version}")
          printer.sep
          printer.item('Uninstall') do |printer|
            printer.item('Are you sure?')
            printer.sep
            printer.item(
              'Yes',
              terminal: true, refresh: true,
              shell: [brew_path, 'uninstall', '--cask', cask.name]
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
            'Upgrade',
            alt: 'Upgrade ' \
                 "(#{formula.current_version} → #{formula.latest_version})"
          )
          printer.sep
          printer.item("Pinned: #{formula.pinned_version}")
          if formula.installed_versions.size > 1
            printer.item("Installed: #{formula.installed_versions.join(', ')}")
          end
          printer.item("Latest: #{formula.latest_version}")
          printer.sep
          printer.item(
            'Unpin',
            terminal: false, refresh: true,
            shell: [brew_path, 'unpin', formula.name]
          )
          printer.item('Uninstall') do |printer|
            printer.item('Are you sure?')
            printer.item(
              'Yes',
              terminal: true, refresh: true,
              shell: [brew_path, 'uninstall', formula.name]
            )
          end
        end
      end
    end

    def formulas
      @formulas ||= all_formulas.reject(&:pinned)
    end

    def pinned
      @pinned ||= all_formulas.select(&:pinned)
    end

    def all_formulas
      @all_formulas ||= outdated['formulae'].map { |line| Formula.new(line) }
    end

    def casks
      @casks ||= outdated['casks'].map { |line| Cask.new(line) }
    end

    def outdated_args
      [
        'outdated',
        (use_greedy? ? '--greedy' : nil),
        '--json=v2'
      ].compact
    end

    def outdated
      @outdated ||= JSON.parse(cmd(brew_path, *outdated_args))
    end
  end
end

begin
  updates = Brew::FormulaUpdates.new
  Xbar::Runner.new(updates).run(ARGV)
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
