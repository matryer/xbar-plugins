#!/usr/bin/env ruby

# <bitbar.title>Desktop cleaner</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Tomoki Yamashita</bitbar.author>
# <bitbar.author.github>tomorrowkey</bitbar.author.github>
# <bitbar.desc>Move desktop files to backup dir</bitbar.desc>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/tomorrowkey/desktop_cleaner</bitbar.abouturl>

require 'date'
require 'yaml'
require 'pathname'
require 'FileUtils'

Pathname.class_eval do
  def is_older_than(time)
    if directory?
      children.all? { |path| path.is_older_than(time) }
    elsif file?
      mtime < time
    else
      raise "unknown file type"
    end
  end

  def dot_file?
    basename.to_s.start_with?('.')
  end

  def move_to!(dir)
    FileUtils.mkdir_p(dir)
    FileUtils.mv(expand_path, dir)
  end

  def archive_to!(dir)
    FileUtils.mkdir_p(dir)
    `zip -rq '#{dir}#{basename.to_s}.zip' '#{expand_path}'`
  end

  def delete_dir
    FileUtils.remove_dir(expand_path)
  end

  def encode
    to_s.gsub(' ', '%20')
  end
end

class DesktopCleaner
  ONE_DAY_IN_SECONDS = 24 * 60 * 60

  def run
    unless backup_dir_accesible?
      STDERR.puts "#{backup_dir} is not accesible"
      exit 1
    end

    desktop_path.children.each do |path|
      next if path.dot_file?
      next if path.symlink?

      if path.is_older_than(threshold_time)
        if archive_extnames.include?(path.extname)
          path.archive_to!(destination_dir)
          path.delete_dir
        else
          path.move_to!(destination_dir)
        end
      end
    end
  end

  def desktop_path
    Pathname.new("#{ENV['HOME']}/Desktop")
  end

  def backup_dir
    @backup_dir ||= begin
      backup_dir = config['backup_dir']
      raise 'backup_dir is not configured' unless backup_dir
      Pathname.new(backup_dir)
    end
  end

  private

  def timestamp
    Date.today.strftime('%Y-%m-%d')
  end

  def destination_dir
    "#{backup_dir}/#{timestamp}/"
  end

  def config
    @config ||= begin
      path = "#{ENV['HOME']}/.desktop_cleaner.yml"
      raise "Missing config file at #{path}" unless File.exist? path

      YAML.load(File.read(path)) || {}
    end
  end

  def backup_dir_accesible?
    File.exist?(backup_dir)
  end

  def threshold_time
    @threshold_time ||= begin
      threshold_days = config['threshold_days'].to_i.tap do |it|
        raise 'threshold_days must be positive' if it <= 0
      end

      Time.now - (threshold_days * ONE_DAY_IN_SECONDS)
    end
  end

  def archive_extnames
    config['archive_extnames'] || []
  end
end

begin
  cleaner = DesktopCleaner.new
  cleaner.run

  puts 'Desktop'
  puts '---'
  puts "Open | href='file://#{cleaner.desktop_path.encode}'"
  puts "Open Backups | href='file://#{cleaner.backup_dir.encode}'"
rescue StandardError => e
  puts '⚠ Desktop | color=yellow'
  puts "---"
  puts e.message
  puts "Open usage | href='https://github.com/tomorrowkey/desktop_cleaner'"
end
