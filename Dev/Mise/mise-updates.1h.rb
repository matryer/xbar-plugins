#!/usr/bin/env ruby
# frozen_string_literal: true

# rubocop:disable Layout/LineLength

# <xbar.title>Mise Updates</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Jim Myhrberg</xbar.author>
# <xbar.author.github>jimeh</xbar.author.github>
# <xbar.desc>List and manage outdated tools installed with mise</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/jimeh/dotfiles/70e616332e9cc196d365e5375156b27ce8c9451e/xbar/img/mise-updates.png</xbar.image>
# <xbar.dependencies>ruby</xbar.dependencies>
# <xbar.abouturl>https://github.com/jimeh/dotfiles/tree/main/xbar</xbar.abouturl>
#
# <xbar.var>string(VAR_MISE_PATH=""): Path to "mise" executable.</xbar.var>
# <xbar.var>string(VAR_ENVIRONMENT_ROOTS=""): Comma-separated list of extra paths to process in addition to $HOME.</xbar.var>
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

module Mise
  module Helpers
    def relative_path(path)
      if path&.start_with?(ENV['HOME'])
        path.sub(ENV['HOME'], '~')
      else
        path
      end
    end
  end

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

    def truthy?(value)
      %w[true yes 1 on y t].include?(value.to_s.downcase)
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

    def mise_path
      @mise_path ||= mise_path_from_env ||
                     mise_path_from_which ||
                     mise_path_from_fs_check ||
                     raise(Xbar::DependencyError)
    end

    def mise_path_from_env
      env_value = config['VAR_MISE_PATH']&.to_s&.strip || ''

      return if env_value == ''
      return unless File.exist?(env_value)

      env_value
    end

    def mise_path_from_which
      detect = cmd('which', 'mise').strip
      return if detect == ''

      detect
    rescue Xbar::CommandError
      nil
    end

    def mise_path_from_fs_check
      [
        "#{ENV['HOME']}/.local/bin/mise",
        "#{ENV['HOME']}/.local/share/mise/bin/mise",
        '/usr/local/bin/mise',
        '/opt/homebrew/bin/mise'
      ].each do |path|
        return path if File.exist?(path)
      end

      nil
    end

    def mise_check(printer = nil)
      printer ||= default_printer
      begin
        return if File.exist?(mise_path)
      rescue Xbar::DependencyError
        # do nothing
      end

      printer.item("#{prefix}↑⚠️", dropdown: false)
      printer.sep
      printer.item('Mise not found', color: 'red')
      printer.item("Executable \"#{mise_path}\" does not exist.")
      printer.sep
      printer.item(
        'Visit https://mise.jdx.dev for installation instructions',
        href: 'https://mise.jdx.dev'
      )

      exit 0
    end
  end

  class Env
    include Helpers

    attr_reader :path

    def initialize(path)
      @path = File.expand_path(path || ENV['HOME'])
    end

    def is?(other_path)
      other_path = File.expand_path(other_path)

      if global?
        other_path == ENV['HOME'] ||
          other_path.start_with?("#{ENV['HOME']}/.config/mise/")
      elsif !other_path.nil? && !other_path.empty?
        path == other_path || path == File.dirname(other_path)
      end
    end

    def ==(other)
      return false unless other.is_a?(Env)

      path == other.path
    end

    def global?
      path == ENV['HOME']
    end

    def name
      if global?
        'Global'
      else
        File.basename(path)
      end
    end

    def full_name
      if global?
        '~ (Global)'
      else
        relative_path(path)
      end
    end
  end

  class Tool
    attr_reader :name

    def initialize(name:, versions: [], outdated: [])
      @name = name
      @versions = versions.each_with_object({}) do |v, memo|
        ver = ToolVersion.new(v)

        memo[ver.env_path] ||= []
        memo[ver.env_path] << ver
      end

      @outdated = outdated&.each_with_object({}) do |od, memo|
        o = ToolOutdated.new(od)
        memo[o.env_path] = ToolOutdated.new(od)
      end
    end

    def for_env?(env)
      source_version(env)&.source&.for_env?(env)
    end

    def upgrade_operation(env)
      if missing?(env)
        :install
      elsif outdated?(env)
        active = active_version(env)
        if !active.nil? && !desired_in_other?(env, active)
          :upgrade
        else
          :install
        end
      end
    end

    # Install arg is the name of the tool followed by the exact version,
    # separated by @.
    def install_arg(env)
      [name, latest_version(env)].compact.join('@')
    end

    # Upgrade arg is just the name of the tool. If a specific version is given,
    # mise skips removing old versions.
    def upgrade_arg(_env)
      name
    end

    def latest_version(env)
      return outdated(env)&.latest if outdated?(env)

      source_version(env).version if missing?(env)
    end

    def requested_version(env)
      source_version(env)&.requested_version
    end

    def source_version(env)
      versions(env)&.find { |v| !v.source.nil? }
    end

    def versions(env)
      @versions[env.path] || []
    end

    def outdated(env)
      @outdated&.dig(env.path)
    end

    def outdated_count
      @outdated&.map { |_, od| od.latest }&.uniq&.size || 0
    end

    def active(env)
      versions(env).find(&:active)
    end

    def desired_in_other?(env, version)
      @versions.any? do |env_path, vers|
        next if env_path == env.path

        outdated = @outdated&.dig(env_path)
        if outdated
          outdated.requested == version
        else
          vers.any? { |v| v.active && v.version == version }
        end
      end
    end

    def active_in_other?(env, version)
      @versions.any? do |env_path, vers|
        next if env_path == env.path

        vers.any? { |v| v.active && v.version == version }
      end
    end

    def active_version(env)
      active(env)&.version
    end

    def installed_versions(env)
      versions(env).select(&:installed)
    end

    def installed?(env)
      installed_versions(env).any?
    end

    def outdated?(env)
      od = outdated(env)

      od.nil? || od.current != od.latest
    end

    def missing?(env)
      sv = source_version(env)

      sv.nil? || sv.installed == false
    end
  end

  class ToolVersion
    include Helpers

    attr_reader :env, :active, :install_path, :installed, :requested_version,
                :source, :version

    def initialize(attributes = {})
      @env = attributes['env']
      @active = attributes['active']
      @installed = attributes['installed']
      @install_path = relative_path(attributes['install_path'])
      @version = attributes['version']
      @requested_version = attributes['requested_version']
      @source = ToolSource.new(attributes['source']) if attributes['source']
    end

    def env_path
      env&.path
    end
  end

  class ToolSource
    include Helpers

    attr_reader :type, :path, :key, :value

    def initialize(attributes = {})
      @type = attributes['type']
      @path = attributes['path']
      @key = attributes['key']
      @value = attributes['value']
    end

    def for_env?(env)
      return false if path.nil? || path.empty?

      env.is?(path)
    end

    def pretty
      @pretty ||= case type
                  when 'environment'
                    "env #{key}=#{value}"
                  when !path.nil? && !path.empty?
                    relative_path(File.dirname(path))
                  end
    end
  end

  class ToolOutdated
    attr_reader :env, :name, :current, :latest, :requested

    def initialize(attributes = {})
      @env = attributes['env']
      @name = attributes['name']
      @current = attributes['current']
      @latest = attributes['latest']
      @requested = attributes['requested']
    end

    def env_path
      env&.path
    end
  end

  class ToolUpdates < Common
    include Helpers

    prefix '🍱'

    def run
      mise_check(printer)

      printer.item("#{prefix}↑#{outdated_count}", dropdown: false)
      printer.sep
      printer.item('Mise Updates️') do |printer|
        printer.item('⏳ Refresh', alt: '⏳ Refresh (⌘R)', refresh: true)
        printer.sep
        print_settings(printer)
      end

      # Show mise version status and, if outdated, a self-update action.
      if mise_outdated?
        printer.item(
          "⬆️ Update mise (#{mise_current_version} → #{mise_latest_version})",
          terminal: true, refresh: true,
          shell: [mise_path, 'self-update', '--yes']
        )
      end

      print_tools(printer)
      printer.sep
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

    def outdated_count
      @outdated_count ||= begin
        count = tools.sum(&:outdated_count)
        count += 1 if mise_outdated?
        count
      end
    end

    def upgrade_all_exclude?(name)
      upgrade_all_exclude.include?(name)
    end

    def upgrade_all_exclude
      @upgrade_all_exclude ||= config.as_set('VAR_UPGRADE_ALL_EXCLUDE')
    end

    def status_label
      label = []
      label << "#{tools.size} tools" if tools.size.positive?

      label = ['no updates available'] if label.empty?
      label.join(', ')
    end

    def mise_version_info
      @mise_version_info ||= JSON.parse(
        cmd(mise_path, 'version', '--json')
      )
    rescue StandardError
      {}
    end

    def mise_current_version
      version = mise_version_info['version'].to_s
      version.split(' ').first
    end

    def mise_current_version_label
      mise_version_info['version'].to_s
    end

    def mise_latest_version
      mise_version_info['latest'].to_s
    end

    def mise_outdated?
      current = mise_current_version
      latest = mise_latest_version
      return false if current.nil? || current == ''
      return false if latest.nil? || latest == ''

      version_greater?(latest, current)
    end

    def version_segments(version_str)
      dotted = version_str.to_s[/\A\d+(?:\.\d+)*/]
      return [] if dotted.nil? || dotted.empty?

      dotted.split('.').map(&:to_i)
    end

    def version_greater?(a_str, b_str)
      a = version_segments(a_str)
      b = version_segments(b_str)
      max = [a.size, b.size].max
      (0...max).each do |i|
        ai = a[i] || 0
        bi = b[i] || 0
        return true if ai > bi
        return false if ai < bi
      end
      false
    end

    def print_settings(printer)
      printer.item('Settings')
      printer.sep

      extra_roots = config.as_set('VAR_ENVIRONMENT_ROOTS')
      printer.item('Environment Root Paths:')
      printer.item('~ (Home Directory / Global)')
      extra_roots.each do |root|
        printer.item("  #{root}")
      end
    end

    def print_tools(printer)
      envs.each do |env|
        print_env_tools(printer, env)
      end
    end

    def print_env_tools(printer, env)
      env_tools = tools.select do |tool|
        tool.for_env?(env) && tool.latest_version(env)
      end
      return unless env_tools.size.positive?

      mise_cmd_args = [mise_path]
      mise_cmd_args += ['--cd', env.path] unless env.global?

      printer.sep
      printer.item(env.full_name)

      all_tools = env_tools.reject { |tool| upgrade_all_exclude?(tool.name) }
      excluded = (env_tools - all_tools)
      printer.item("Upgrade All (#{all_tools.size})") do |printer|
        to_install = []
        to_upgrade = []

        all_tools.each do |tool|
          case tool.upgrade_operation(env)
          when :install
            to_install << tool
          when :upgrade
            to_upgrade << tool
          end
        end

        cmds = []
        if to_install.size.positive?
          cmds += mise_cmd_args + ['install'] + to_install.map do |t|
            t.install_arg(env)
          end
        end
        if to_upgrade.size.positive?
          cmds << '&&' if cmds.size.positive?
          cmds += mise_cmd_args + ['upgrade'] + to_upgrade.map do |t|
            t.upgrade_arg(env)
          end
        end

        if all_tools.size.positive?
          printer.item(
            "⬆️ Upgrade (#{all_tools.size})",
            terminal: true, refresh: true,
            shell: cmds
          )
        end

        if excluded.size.positive?
          printer.sep
          printer.item("Excluded (#{excluded.size}):")
          excluded.sort_by(&:name).each do |item|
            printer.item(item.name)
          end
        end
      end

      printer.sep
      env_tools.each do |tool|
        next if tool.latest_version(env).nil?

        name = tool.name
        name += ' ⤫' if upgrade_all_exclude?(name)
        printer.item(name) do |printer|
          mise_operation = tool.upgrade_operation(env)

          if mise_operation == :install
            tool_arg = tool.install_arg(env)
            text = "➡️ Install (→ #{tool.latest_version(env)})"
          else
            tool_arg = tool.upgrade_arg(env)
            text = "⬆️ Upgrade (↑ #{tool.latest_version(env)})"
            alt_text = '⬆️ Upgrade ' \
                       "(#{tool.active_version(env)} → " \
                       "#{tool.latest_version(env)})"
          end

          printer.item(
            text,
            alt: alt_text || text,
            terminal: true, refresh: true,
            shell: mise_cmd_args + [mise_operation.to_s, tool_arg]
          )
          printer.sep

          latest_version = tool.latest_version(env)
          if latest_version
            printer.item("↑ Latest: #{latest_version}")
          end

          active_version = tool.active_version(env)
          printer.item("→ Active: #{active_version || '<Missing>'}")

          requested_version = tool.requested_version(env)
          if requested_version
            printer.item("→ Requested: #{requested_version}")
          end

          other_envs = envs.select { |e| env != e && tool.for_env?(e) }
          if other_envs.size.positive?
            printer.sep
            printer.item('Other Environments') do |printer|
              other_envs.each_with_index do |other_env, index|
                printer.item(other_env.full_name)
                latest_version = tool.latest_version(other_env)
                if latest_version
                  printer.item("↑ Latest: #{latest_version}")
                end

                active_version = tool.active_version(other_env)
                printer.item("→ Active: #{active_version || '<Missing>'}")

                requested_version = tool.requested_version(other_env)
                if requested_version
                  printer.item("→ Requested: #{requested_version}")
                end

                printer.sep if index < other_envs.size - 1
              end
            end
          end

          printer.sep
          printer.item('Installed:')
          tool.versions(env).each do |v|
            next unless v.installed

            icon = if v.active
                     '✔️'
                   elsif tool.active_in_other?(env, v.version)
                     '➕'
                   else
                     '➖'
                   end

            printer.item("#{icon} #{v.version}") do |printer|
              printer.item("Active: #{v.active ? 'Yes' : 'No'}")
              if v.requested_version
                printer.item("Requested: #{v.requested_version}")
              end
              if v.source&.path
                printer.item("Set by: #{relative_path(v.source.path)}")
              end
              printer.item("Path: #{v.install_path}")

              active_envs = envs.select do |e|
                active_version = tool.active_version(e)
                !active_version.nil? && active_version == v.version
              end

              printer.sep
              text = ['Active in Environments:']
              text << '<None>' if active_envs.empty?
              printer.item(text.join(' '))
              active_envs.each do |e|
                printer.item(e.full_name)
              end

              printer.sep
              printer.item('🚫 Uninstall') do |printer|
                printer.item('Are you sure?')
                printer.item(
                  'Yes',
                  terminal: true, refresh: true,
                  shell: mise_cmd_args + [
                    'uninstall', "#{tool.name}@#{v.version}"
                  ]
                )
              end
            end
          end
          printer.sep
          if upgrade_all_exclude?(tool.name)
            printer.item(
              '✅ Upgrade All: Exclude',
              terminal: false, refresh: true,
              rpc: ['include_upgrade_all', tool.name]
            )
          else
            printer.item(
              '☑️ Upgrade All: Exclude ',
              terminal: false, refresh: true,
              rpc: ['exclude_upgrade_all', tool.name]
            )
          end
        end
      end
    end

    def envs
      @envs ||= ([ENV['HOME']] + config.as_set('VAR_ENVIRONMENT_ROOTS').to_a)
                .uniq.map { |p| Env.new(p) }
    end

    def tools
      return @tools if @tools

      versions = envs.each_with_object({}) do |env, memo|
        tool_list[env.path].each do |name, versions|
          memo[name] ||= []
          memo[name] += versions.map do |v|
            v.merge('env' => env)
          end
        end
      end

      outdated = envs.each_with_object({}) do |env, memo|
        outdated_list[env.path]&.each do |name, od|
          memo[name] ||= []
          memo[name] << od.merge('name' => name, 'env' => env)
        end
      end

      versions.map do |name, vers|
        Tool.new(
          name: name,
          versions: vers,
          outdated: outdated[name]
        )
      end
    end

    def tool_list
      @tool_list ||= envs.each_with_object({}) do |env, memo|
        memo[env.path] = JSON.parse(
          cmd(mise_path, 'list', '--json', dir: env.path)
        )
      end
    end

    def outdated_list
      @outdated_list ||= envs.each_with_object({}) do |env, memo|
        out = cmd(mise_path, 'outdated', '--json', dir: env.path)
        out = '{}' if out.empty?
        memo[env.path] = JSON.parse(out)
      end
    end
  end
end

begin
  service = Mise::ToolUpdates.new
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
