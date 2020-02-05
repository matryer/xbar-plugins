#!/usr/bin/env ruby

# <bitbar.title>Speedwifi-next W06 transfer amount during 1day</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>positrium</bitbar.author>
# <bitbar.author.github>positrium</bitbar.author.github>
# <bitbar.desc>show Speedwifi-next w06 transfer amount during 1day for bitbar.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/positrium/wifi-transfer-meter/master/image20200122.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/positrium/wifi-transfer-meter</bitbar.abouturl>

require 'open-uri'
require 'nokogiri'

class TransferAmount
	def initialize
		@has_error = false

		@scale = {
			kb: 1024,
			mb: 1024 * 1024,
			gb: 1024 * 1024 * 1024,
			tb: 1024 * 1024 * 1024 * 1024
		}
		@scale.freeze

		@payload = {
			yesterday_download: -9,
			yesterday_upload: -9,
			yesterday_duration: -9,
			today_download: -9,
			today_upload: -9,
			today_duration: -9,
			is_yesterday_flux_over_limit: -9,
			last_clear_time_3days: -9
		}

		@doc = Nokogiri.HTML(open("http://speedwifi-next.home/api/monitoring/statistics_3days"))
		@doc.xpath('//response/*').each do |e|
			case e.name
			when 'toyestodaydownload'
				@payload[:yesterday_download] = e.children[0].text
			when 'toyestodayupload'
				@payload[:yesterday_upload] = e.children[0].text
			when 'toyestodayduration'
				@payload[:yesterday_duration] = e.children[0].text
			when 'totodaydownload'
				@payload[:today_download] = e.children[0].text
			when 'totodayupload'
				@payload[:today_upload] = e.children[0].text
			when 'totodayduration'
				@payload[:today_duration] = e.children[0].text
			when 'isyestodayfluxoverlimit'
				@payload[:is_yesterday_flux_over_limit] = e.children[0].text
			when 'lastcleartime3days'
				@payload[:last_clear_time_3days] = e.children[0].text
			end
		end
		@payload.freeze
	rescue
		@has_error = true
	end

	def yesterday_data_usage
		yesterday_data = @payload[:yesterday_download].to_i + @payload[:yesterday_upload].to_i

		scale = transfer_scale(yesterday_data)
		yesterday_usage = ( yesterday_data.to_f / scale[:size] ).round(2)

		{amount: yesterday_usage, label: scale[:label], percentage: yesterday_usage / 10.00 * 100}
	end

	def today_data_usage
		today_data = @payload[:today_download].to_i + @payload[:today_upload].to_i

		scale = transfer_scale(today_data)
		today_usage = ( today_data.to_f / scale[:size] ).round(2)

		{amount: today_usage, label: scale[:label], percentage: today_usage / 10.00 * 100}
	end

	def limited?
		limited = @payload[:is_yesterday_flux_over_limit].to_i

		false if limited == 0
		true if limited != 0
	end

	def has_error?
		@has_error
	end

	def scale_down(value)
		value * 1024
	end

	private

	def transfer_scale(byte)
		scale_info = {size: 0, label: ''}

		if @scale[:mb] > byte
			scale_info[:size] = @scale[:kb]
			scale_info[:label] = 'KB'
		elsif @scale[:gb] > byte
			scale_info[:size] = @scale[:mb]
			scale_info[:label] = 'MB'
		elsif @scale[:tb] > byte
			scale_info[:size] = @scale[:gb]
			scale_info[:label] = 'GB'
		else
			scale_info[:size] = @scale[:tb]
			scale_info[:label] = 'TB'
		end

		scale_info
	end

end

class WarningDetector
	attr_reader :sign, :amount

	def initialize(amount, percentage, symbols={over: "x", warn: "!", ok: "o", limited: "-"}, limited=false)
		@amount = amount.round(2)
		@percentage = percentage
		@symbols = symbols
		@limited = limited
	end

	def total_status
		@status = :ok

		if @percentage >= 100.00
			@status = :over
		elsif @percentage >= 70.00
			@status = :warn
		end

		@sign = @symbols[@status]
		@status
	end

	def today_left_value
		value = 0

		case @status
		when :warn
			value = scale_down(10.00-@amount).round(2)
			value = value.round
		when :ok
			value = scale_down(7.00-@amount).round(2)
			value = value.round
		end

		value = 0 if value < 1

		value
	end

	private

	def scale_down(n)
		n * 1024
	end
end

a = TransferAmount.new

if a.has_error?
	puts "<!> connect to w06"

else
	symbols = {over: ":broken_heart:", warn: ":yellow_heart:", ok: ":green_heart:", limited: ":no_entry_sign:"}
	symbols.freeze

	usage = a.today_data_usage
	wc = WarningDetector.new(usage[:amount], usage[:percentage], symbols, a.limited?)
	wc.total_status

	y_usage = a.yesterday_data_usage
	wt = WarningDetector.new(y_usage[:amount], y_usage[:percentage], symbols, false)
	wt.total_status

	puts "#{wc.sign}#{wc.today_left_value}MB(#{wc.amount}#{usage[:label]})"
	puts "---"
	puts "admin page|href=http://speedwifi-next.home"
	puts "hardware page|href=https://www.uqwimax.jp/wimax/products/w06/"
	puts "---"
	puts "until today usage"
	puts "#{wc.sign}#{wc.amount}#{usage[:label]}"
	puts "--#{symbols[:limited]} restricted now"
	puts "--#{symbols[:over]} over 10GB (100%)"
	puts "--#{symbols[:warn]} over  7GB ( 70%)"
	puts "--#{symbols[:ok]} less 7GB"
	puts "--today + 1 day ago + 2 days ago"
	puts "until yesterday usage"
	puts "#{wt.sign}#{wt.amount}#{y_usage[:label]}"
	puts "--#{symbols[:over]} over 10GB (100%)"
	puts "--#{symbols[:warn]} over  7GB ( 70%)"
	puts "--#{symbols[:ok]} less  7GB"
	puts "--1 day ago + 2 days ago + 3 days ago"
end
