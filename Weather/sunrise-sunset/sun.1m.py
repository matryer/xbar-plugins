#!/usr/bin/env python3

# Copyright (c) 2024 Kipras Melnikovas
# MIT License
# https://github.com/kiprasmel/xbar-plugins

# <xbar.title>Sun indicator</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>Kipras Melnikovas</xbar.author>
# <xbar.author.github>kiprasmel</xbar.author.github>
# <xbar.desc>Displays a pretty, dynamically generated indicator of what point in the day or night you are</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/kiprasmel/xbar-plugins/main/Weather/sunrise-sunset/sun-indicator.png</xbar.image>
# <xbar.dependencies>python,suntime,pillow,ansicolors</xbar.dependencies>
# <xbar.abouturl>https://github.com/kiprasmel/xbar-plugins/tree/main/Weather/sunrise-sunset</xbar.abouturl>

# <xbar.var>string(VAR_LATITUDE=0.00000): Your latitude to calculate the sun position.</xbar.var>
# <xbar.var>string(VAR_LONGITUDE=0.00000): Your longitude to calculate the sun position.</xbar.var>
# <xbar.var>number(VAR_WIDTH_COMPRESSION_FACTOR=20): The bigger the compression, the smaller the width. Explanation: To accuratelly represent all 1440 minutes in a day, you'd need 1440px of width. Instead, choose how much to compress the width. A compression factor of 20 means that you'll see the visual indicator move slightly every 20 minutes. min = 2, max = 60 (1440/60 = 24 = 1 per hour).</xbar.var>
# <xbar.var>number(VAR_HEIGHT=8): How tall do you want the indicator to be?</xbar.var>
# <xbar.var>number(VAR_BORDER_WIDTH=1): How big should the borders be?</xbar.var>
# <xbar.var>number(VAR_HOURS_OFFSET=0): Debug: how many hours to offset?</xbar.var>

import os
import io
import json
import base64
from math import ceil, floor
from datetime import datetime, timezone, timedelta

# deps
from suntime import Sun
from PIL import Image, ImageDraw
from colors import color as ansicolor

def main():
	HOURS = 24
	MINUTES = HOURS * 60

	# ------------------ #
	# begin customizable #
	# ------------------ #

	LATITUDE  = env("VAR_LATITUDE",  0.00000, float)
	LONGITUDE = env("VAR_LONGITUDE", 0.00000, float)

	WIDTH_COMPRESSION_FACTOR = env("VAR_WIDTH_COMPRESSION_FACTOR", 20, int)
	HOURS_OFFSET             = env("VAR_HOURS_OFFSET",              0, int)

	height       = env("VAR_HEIGHT",       10, int)
	border_width = env("VAR_BORDER_WIDTH",  1, int)

	# ---------------- #
	# end customizable #
	# ---------------- #

	# RGB
	# TODO customizable
	border_color  = (  0,   0,   0)
	chevron_color = (  0,   0,   0)
	COLOR_DAY     = (255, 255,   0)
	COLOR_NIGHT   = ( 40, 180, 255)
	colors = {
		"day": COLOR_DAY,
		"night": COLOR_NIGHT,
	}
	YEAR_COLOR_DAY   = (255, 255,   0) # TODO
	YEAR_COLOR_NIGHT = ( 40, 180, 255) # TODO
	colors_year = {
		"day": YEAR_COLOR_DAY,
		"night": YEAR_COLOR_NIGHT,
	}

	UNITS = MINUTES // WIDTH_COMPRESSION_FACTOR

	log("vars:")
	log("MINUTES %d" % MINUTES)
	log("UNITS %d" % UNITS)
	log("WIDTH_COMPRESSION_FACTOR %d" % WIDTH_COMPRESSION_FACTOR)
	log("HOURS_OFFSET %d" % HOURS_OFFSET)
	log("LATITUDE %d" % LATITUDE)
	log("LONGITUDE %d" % LONGITUDE)
	log("height %d" % height)
	log("border_width %d" % border_width)
	log("")

	assert(-90  < LATITUDE  <  90)
	assert(-180 < LONGITUDE < 180)
	assert(1 < WIDTH_COMPRESSION_FACTOR <= 60)
	assert(height >= 0)
	assert(border_width >= 0)

	width = UNITS
	border_offset = 0.5 if border_width == 1 else border_width // 2

	log("width %d height %d" % (width, height))
	log("")

	# +1 because otherwise missing 1 px
	image_width  = width  + 2 * border_width + 1
	image_height = height + 2 * border_width + 1

	image = Image.new("RGB", (image_width, image_height), "white")
	draw = ImageDraw.Draw(image)

	draw_border(draw, width, height, border_width, border_color)

	# local time
	now = (datetime.now(timezone.utc) + timedelta(hours=HOURS_OFFSET)).astimezone()
	# now = datetime(2024, 2, 17, 22, 10, 0).astimezone() # night showcase
	# now = datetime(2024, 2, 16, 12, 0, 0).astimezone() # day showcase

	(now_minute,
	sunrise_minute,
	sunset_minute) = get_time_info_for_latlon(now, LATITUDE, LONGITUDE)

	(colors_by_unit,
	sun_count_by_minute,
	night_count_by_minute) = classify_units_of_time(now_minute,
	                                                sunrise_minute,
	                                                sunset_minute,
	                                                MINUTES,
	                                                UNITS,
	                                                WIDTH_COMPRESSION_FACTOR)

	log("sun_count_by_minute %d night_count_by_minute %d" % (sun_count_by_minute, night_count_by_minute))

	draw_day_night_indicator(draw, colors_by_unit, colors, height, border_width)
	draw_chevron(draw, image_width, image_height, border_width, chevron_color)

	image_base64 = enc_image_base64(image)

	# colors
	colw = lambda x: ansicolor(x, fg="#dddddc") # xbar reverts to black all equal
	cold = lambda x: ansicolor(x, fg=COLOR_DAY)
	coln = lambda x: ansicolor(x, fg=COLOR_NIGHT)

	set_rise_info, percent_done_info = get_sunset_sunrise_info(now_minute, sunrise_minute, sunset_minute, HOURS, MINUTES, cold, coln)

	sun_percent, night_percent = get_percents(sun_count_by_minute, night_count_by_minute)

	day_night_ratio_info = "%s%s%s %s%s%s" % (
		cold("day"),       colw(":"), coln("night"),
		cold(sun_percent), colw(":"), coln(night_percent),
	)

	# equal_spacing_font_arg = "font='Monaco' size=13"
	equal_spacing_font_arg = ""

	xbar_output = f"""
	| image={image_base64}
	---
	{set_rise_info[0]} {colw("(")}{percent_done_info}{colw(")")} | {equal_spacing_font_arg}
	{set_rise_info[1]}                                           | {equal_spacing_font_arg}

	{day_night_ratio_info}
	"""

	log("")
	print(xbar_output.strip())
	# done

# utils

def env(x, default, transform = lambda y: y):
	return transform(json.loads(os.environ.get(x, default)))

DEBUG = int(os.environ.get("DEBUG", 0))
def log(*x, lvl=1):
	if DEBUG >= lvl:
		print(*x)

def classify_units_of_time(now_minute, sunrise_minute, sunset_minute, MINUTES, UNITS, WIDTH_COMPRESSION_FACTOR):
	# calculate uncompressed to get proper accuracy
	sun_count_by_minute   = 0
	night_count_by_minute = 0
	for minute in range(MINUTES):
		is_day = sunrise_minute <= minute <= sunset_minute
		if is_day:
			sun_count_by_minute   += 1
		else:
			night_count_by_minute += 1

	# calculate compressed
	colors_by_unit = [None] * UNITS

	sunrise_unit = sunrise_minute // WIDTH_COMPRESSION_FACTOR
	sunset_unit  = sunset_minute  // WIDTH_COMPRESSION_FACTOR
	for unit in range(UNITS):
		is_day = sunrise_unit <= unit <= sunset_unit
		if is_day:
			color = "day"
		else:
			color = "night"
		colors_by_unit[unit] = color

	# calculate uncompressed to get proper accuracy
	center_minute_abs = MINUTES / 2
	center_minute_rel = now_minute - center_minute_abs
	rotate_index = norm_mod(center_minute_rel, MINUTES) / WIDTH_COMPRESSION_FACTOR
	rotate_index = ceil(rotate_index)
	# why ceil?
	# focus at the time when sunset will be in exactly 12 hours.  this means
	# that all the "day" colors are to the left, and all the night colors to the
	# right (because 12 hours is half of 24 hours, and we're 12 hours away from
	# the center, so nothing from the "day" colors should show up on the right
	# just yet).
	# 
	# now, as soon as 1 more second passes, we're < 12 hours away from sunrise,
	# so we should show at least 1 "day" on the right.  we don't have "seconds",
	# but we have "minutes", hence, once at least 1 minute passes, we show at
	# least one "day" color on the right.  hence, `ceil`.

	# rotate the array to center everything around the current position in time
	colors_by_unit = colors_by_unit[rotate_index:] + colors_by_unit[:rotate_index]

	return colors_by_unit, sun_count_by_minute, night_count_by_minute

def norm_mod(x, mod):
	return ((x % mod) + mod) % mod

# guarantees percents add up to 100, and are properly split up
def get_percents(*counts, total_count=None):
	N = len(counts)

	counts_sum = sum(counts)
	has_custom_count = False

	if total_count == None:
		total_count = counts_sum
	elif total_count != counts_sum:
		has_custom_count = True

	sum_percents = 0
	data = [None] * N
	for i, count in enumerate(counts):
		data[i] = {}
		data[i]["i"] = i
		data[i]["count"] = count
		data[i]["frac"] = count / total_count
		data[i]["percent"] = round(data[i]["frac"] * 100)
		data[i]["rem"] = data[i]["frac"] * (100 * 10) % 10 / 10
		sum_percents += data[i]["percent"]

	if sum_percents < 100 and not has_custom_count:
		# too little, need to add to those who barely didn't round up.
		# too much is impossible, because round() goes up iff > 0.5

		sorted(data, key=lambda x: x["rem"])

		mid = 0
		while data[mid]["frac"] > 0.5 and mid < N:
			mid += 1

		while sum_percents != 100:
			data[mid]["percent"] += 1
			sum_percents += 1
			mid += 1

	# sort by orig idx
	sorted(data, key=lambda x: x["i"])

	log(data)

	return [x["percent"] for x in data]

def draw_border(draw, width, height, border_width, border_color):
	bx1 = 0
	by1 = 0
	bx2 = width  + border_width * 2
	by2 = height + border_width * 2
	draw.rectangle([bx1, by1, bx2, by2], outline=border_color, width=border_width)

def draw_day_night_indicator(draw, colors_by_unit, colors, height, border_width):
	for i, color in enumerate(colors_by_unit):
		x1 = (i	)    + border_width
		x2 = (i + 1) + border_width
		y1 =		   border_width
		y2 = height +  border_width

		draw.rectangle([x1, y1, x2, y2], fill=colors[color])
		log("i %d coords: (%d %d) (%d %d) color %s" % (i, x1, y1, x2, y2, color), lvl=2)

def draw_chevron(draw, image_width, image_height, border_width, chevron_color):
	# TODO: adjust properly when between day & night (now incorrect placement)
	chevron_height = 6
	chevron_width  = 8
	chevron_x      = (image_width - chevron_width) // 2
	chevron_bottom = image_height - border_width
	chevron_top    = chevron_bottom - chevron_height + 2 # TODO AUTO
	chevron_middle = chevron_bottom - chevron_height // 2
	BL = (chevron_x, chevron_bottom)
	MT = (chevron_x + chevron_width // 2, chevron_top)
	BR = (chevron_x + chevron_width, chevron_bottom)
	chevron_points = [BL, MT, BR]
	draw.polygon(chevron_points, fill=chevron_color)
	log(f"chevron polygon: {chevron_points}", lvl=2)

def enc_image_base64(image):
	buffer = io.BytesIO()
	image.save(buffer, format="PNG")
	imgb64 = base64.b64encode(buffer.getvalue()).decode("utf-8")
	return imgb64

def get_sunset_sunrise_info(now_minute, sunrise_minute, sunset_minute, HOURS, MINUTES, cold, coln):
	sunrise_in = norm_mod(sunrise_minute - now_minute, MINUTES)
	sunset_in  = norm_mod(sunset_minute  - now_minute, MINUTES)
	
	sunrise_info = cold("sunrise in %s" % format_hours_mins(timedelta(minutes=sunrise_in)))
	sunset_info  = coln("sunset  in %s" % format_hours_mins(timedelta(minutes=sunset_in)))
	
	percent_done_info = None
	set_rise_info = None
	if sunrise_minute <= now_minute <= sunset_minute:
		set_rise_info = [sunset_info, sunrise_info]

		total = norm_mod(sunset_minute - sunrise_minute, MINUTES)
		done = norm_mod(now_minute - sunrise_minute, MINUTES)
		night_done, *_rest = get_percents(done, total, total_count=total)
		percent_done_info = cold("day %d%% done" % night_done)
	else:
		set_rise_info = [sunrise_info, sunset_info]

		total = norm_mod(sunrise_minute - sunset_minute, MINUTES)
		done = norm_mod(now_minute - sunset_minute, MINUTES)
		night_done, *_rest = get_percents(done, total, total_count=total)
		percent_done_info = coln("night %d%% done" % night_done)
	
	return set_rise_info, percent_done_info

def format_hours_mins(timedelta):
	dt = datetime(1,1,1) + timedelta
	# h = int(dt.strftime("%H"))
	# m = int(dt.strftime("%M"))
	# return "%dh %dm" % (h, m)
	return dt.strftime("%Hh %Mm")

if __name__ == '__main__':
	main()
