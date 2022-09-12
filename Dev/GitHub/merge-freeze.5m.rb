#!/usr/bin/ruby

require 'uri'
require 'net/http'
require 'json'

# <xbar.title>Merge Freeze</xbar.title>
# <xbar.version>v0.2</xbar.version>
# <xbar.author>Ryan Kulp</xbar.author>
# <xbar.author.github>ryanckulp</xbar.author.github>
# <xbar.desc>Manage GitHub repository branch freezes from the Mac menu bar.</xbar.desc>
# <xbar.image>https://user-images.githubusercontent.com/3083888/188286665-131e3cb8-9b1d-45e2-af4a-2852201c334e.png</xbar.image>
# <xbar.dependencies>ruby</xbar.dependencies>
# <xbar.abouturl>https://github.com/Merge-Freeze/mac-menu-plugin</xbar.abouturl>

# Variables become preferences in the app:
# <xbar.var>string(API_KEY=""): Organization-level API key from MergeFreeze.com.</xbar.var>
# <xbar.var>string(ORGANIZATION_SLUG=""): Organization "slug" username on GitHub.com.</xbar.var>

module Icons
  # red fill, 1+ repos are frozen
  def frozen_icon
    "iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAA4ZpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDcuMi1jMDAwIDc5LjU2NmViYzViNCwgMjAyMi8wNS8wOS0wODoyNTo1NSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wTU09Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9tbS8iIHhtbG5zOnN0UmVmPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvc1R5cGUvUmVzb3VyY2VSZWYjIiB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iIHhtcE1NOk9yaWdpbmFsRG9jdW1lbnRJRD0ieG1wLmRpZDpmNGQ5N2MwNS1lYzhiLTQzMmItYTUzNS0wNjUxMjVkMzZiNjQiIHhtcE1NOkRvY3VtZW50SUQ9InhtcC5kaWQ6QjE3MjJDMDIyM0M1MTFFREJDODZCMzlCMDBDMEMwMjkiIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6QjE3MjJDMDEyM0M1MTFFREJDODZCMzlCMDBDMEMwMjkiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIDIzLjQgKE1hY2ludG9zaCkiPiA8eG1wTU06RGVyaXZlZEZyb20gc3RSZWY6aW5zdGFuY2VJRD0ieG1wLmlpZDpmOTgxZDFhZS1iMWQ5LTQ2YTYtYjIxOC0xM2M4ZDRlNjY1OWIiIHN0UmVmOmRvY3VtZW50SUQ9ImFkb2JlOmRvY2lkOnBob3Rvc2hvcDpmY2JhYmFiNy0xM2JkLTQzNGMtODI4Zi04NDlhMzAwMWIyNjQiLz4gPC9yZGY6RGVzY3JpcHRpb24+IDwvcmRmOlJERj4gPC94OnhtcG1ldGE+IDw/eHBhY2tldCBlbmQ9InIiPz5N1I0IAAAENklEQVR42oRVS2hcVRj+zmNu5t4mzWTSxMkkmJAqodSWVusjDWQjRcFlkdKNW1eC4kIQXAriJqIbERQXglRBMN2KLvpItIpgwSRKbUd0Mnk1TULm3pl7Hv7nzCTO5EEvc+be+9///87/+P7/MKUUyuUytKrDWA0hsoxWN+cyBOMSAMfBl4G1ypg00Tp5QMsyJsg+g2KxCFkuL2Ji4jkkSTDaVxh/PYyK56XI9jMHDNKkv0OALf20NZqA45W4WpldWZr5kLHN+Rs3ZiG1rmG7yke78y9eZ7xvoFpNySIlG3cHNOFK/2SbaKxF1tiWITPE+ejZrlzPyyuVbybTtDbHLYVfGJh4I4r6BoA6OLcQnFFIHIFgGBQJpHDvwi8nG9qVca/rbJxtGOWPFQYn33Lbc8onD6PBCcpVW5wJkzhnlvBVPI0xuw6CQpVkT5LsSnwVJ0hWg2hPunHgA+NShoEk4C4hwz5AtSllKJI7vBtTwTn8zY6ikzyyluEPnifZU/iHHUGG6teWdGtd8fO0cpIxGXIustamu3VizYwukfEG68Db9VmfW97M67Q8jlUWQVizr57EjA7GM5Fk3FFKtFFKEUTINN6pz+BZtYgvgpOYFqNefkGX8G7tGr6TI3gveIYoyknaugEXjJB5w0HbAsyQEsteTX/DC+ldfBycwTFbRY+tYQ0hPpcn8VlwGhfTBbyS/o4aE3u99mHvI39KohG7gQvqrg/3lijgI3mW8tyJLPkcWoVbvIB1lsVL6g4Kdpvot7+H9kkU4zhuNnDEpj7EHHm6Tnk2FAlzDKfvOZt4XXcfsZvemYcCu/JkmwwR9HY5nUM3mW4R+CatPJHskpr3eszr6p1WabvkIb3a4DJBP60X8UHyPX6Qj3pWPK9KeMys+2+HgR4KzFruDmDMrOGJ+oqX1endydge3YOAaVNmWnOzhcDzdsew3gLk8rnz7GbGFrVJez6ZD0JaY5RrxtaOW6DuWmWhL46reJ0o9QsxwQGeMsukYwhOY5F34S+W29OBxlhrFA0hFRN2wppuCFKqEM2+zowRa5V/d14uiF7M03JNIkkW0Poyc8Iz5v8GcS2ha9akVRqbyRbN01UgGNopW5a4ekWOoZMod0nN4RHi6mu1nxs5Ju8dOz7NnMZV6kanu5sE8k7p5D5hrjtgE2+XZ7h4/IzLZmNf6+fAJ5lTuCmKmND/op+6z11lapRrYtCny4GyFl5wnkFcLf2oVFyX7jhZrsxMHc33Xoyi3n43+tyUQjPc2+jBr7wXvFETmg3M16FD15vjn3lPaTzQKbT5YKVy/X3XSjQ2Axp11T/vL387KQrn34yi4riQ2T6aelk3UKIDGcWbdDeaDopEq2Q1jks/rS3fnBJ887aQAViaprh3rwR3RFE1aZPQzegchRU9/DA1ylDxtYrpMI1TN1gd6PDwMP4TYACvGt76h/NzmgAAAABJRU5ErkJggg=="
  end

  # white fill, 0 repos are frozen
  def unfrozen_icon
    "iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyhpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDcuMi1jMDAwIDc5LjU2NmViYzViNCwgMjAyMi8wNS8wOS0wODoyNTo1NSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIDIzLjQgKE1hY2ludG9zaCkiIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6NjhGREVGRTAyM0I0MTFFREJDODZCMzlCMDBDMEMwMjkiIHhtcE1NOkRvY3VtZW50SUQ9InhtcC5kaWQ6NjhGREVGRTEyM0I0MTFFREJDODZCMzlCMDBDMEMwMjkiPiA8eG1wTU06RGVyaXZlZEZyb20gc3RSZWY6aW5zdGFuY2VJRD0ieG1wLmlpZDo2OEZERUZERTIzQjQxMUVEQkM4NkIzOUIwMEMwQzAyOSIgc3RSZWY6ZG9jdW1lbnRJRD0ieG1wLmRpZDo2OEZERUZERjIzQjQxMUVEQkM4NkIzOUIwMEMwQzAyOSIvPiA8L3JkZjpEZXNjcmlwdGlvbj4gPC9yZGY6UkRGPiA8L3g6eG1wbWV0YT4gPD94cGFja2V0IGVuZD0iciI/Pmm4/+YAAAQ9SURBVHjajFVZbFtFFD0zb+z4vSxNnbjYTmikiABtqdiEVMcQVgGtUJCo+ClCQmITEmURYukn208oVJQSKgS/LRSJRUCRkhZEm5JQJBLaoBpQIFaF69ppFoe8Z/tt3HmxEztJCVdvbM12Zu69555hlmUhlUrBtopwXBuKEmDU1nAuVDAuAHCsbA5c13IcM2/b+WlqLmMK7fchGo1CpFLnEI9vQT7vbw+FY8+oWrRTKIF1TAKDVtLPRYBd+mzXsQnYyBp6eih7fnAvY7nEiRNDELZdwJzO29cE7x5gPBTRdZN2mLRH/ruwTBs+3yL+sjHv87Vy3n5tfePa+7PpT7tMs3CGu+R+OBJ/VtNCEaAIzl0onJFLDH6fwPr1zRBC8forjtFauUfuVbVgc7il60V5PKd4clVriVOsqvw0jCLisQ5827cLV21qhW6YmJsrILZlfmzz5kspfFZ10B0JHokJofoFAdcrQg0B1Yv8foHE7+fw8mufYWwsg/q6AOXKxeivZ/EKjSWTE6VwVASd5gkvSK1RMCZUzpWA65oLcWSM0SIQW6YwPa1jd88OL7qcc2/zgY8GcT6Tg0/wEmnchXwSM2oY92mCcUkppYpSlmUjEPDh7bcexE03XoHe/Ufw8aEhmDTefc91eG/fQzj8zS94/qWD5L5DB1YShyuMkPn8Nd0q4ELBwq4XunFz15V4482vEYk0oqmpDtnsLPa+24d9vf3YtvVqPPXknV4ulrDQO2UZ+U2iUsdlYWy/7wavf+x4Aq++/jn+/CsLVfWhrrYG3x9LeHMP7IijpSVIHjrLSL4MuEjAGzZQ5Yj5qWCwDhcm//FclrGX883N9d6cPOTyjrB3mVWBZdZU1b/QfeLx29HQoGJmxsAUJTK4thaPPXLrwnwg4PcSutQEVrFO4vInB3biy8PDlGKOe7uvp1BdUnmPFW1VYGkbN7Z4bSVjF1GSEgmZs7iQkds6/q/lcvoSujHPB+46jiWLsbLiTp06i9xsfjGhRRvHB37zWmWi0ukZnEmkvD2Vhe26jkUiZBmEnS+7JMXl79Qk9r9/tIKCFn4eHsfwSNJjRdkknyeJMbIiS7eVSlpwHVMn2czPkp5O0F1b50vThabV4IMPv0NDvYpHH74FtUSrp3feVVVA7/T24eChQW+uXNIyjJadnyTMKQnsGHOpQa50XCOlr7xA3rxn91foPzqKO27bhNbWoEcrKT79R0ZxmsRIU2uqE8Z9MPTkj5ZlFIV8TjLpwT0Nwabtmta0TkpfmZdSL0ZGxnHypzHwUqwogJ4mqyX+ulTB8iIkDySjuelseqCHeiDZ9JPU6X9MZr7oUsKdz2laNKaIQIhULyAFhcLyH0+TY9NDkbet/IRhJE9eyPywR+G504rwg5mmifHxJOQTRdmkQ1Sp0Y3klrb6Y+pYDiXftgx6TA2T3hJI0La2NvwrwACK6ehm4BCh9gAAAABJRU5ErkJggg=="
  end
end

class MergeFreezeApplet
  include Icons

  BASE_URL = 'https://www.mergefreeze.com'.freeze
  attr_accessor :api_key, :organization_slug, :repositories

  def initialize
    @api_key = ENV['API_KEY']
    @organization_slug = ENV['ORGANIZATION_SLUG']
  end

  def call
    if (api_key && api_key.length > 0) && (organization_slug && organization_slug.length > 0)
      get_repositories
      set_icon
      insert_separator
      build_menu
    else
      puts "Merge Freeze - Setup"
      insert_separator
      puts "Press ⌘E to add config keys | disabled=true"
    end
  end

  def get_repositories
    uri = URI("#{BASE_URL}/api/branches/#{@organization_slug}?access_token=#{@api_key}")
    res = Net::HTTP.get_response(uri)
    @repositories = JSON.parse(res.body)
  end

  def sort_and_print(collection)
    repositories[collection].sort_by { |k| k['name'] }.each do |repo| # list alphabetically
      puts "-- #{repo['name']} | href=#{repo['url']}"
    end
  end

  def frozen_repos_exist?
    repositories['frozen'].count > 0
  end

  def set_icon
    puts "|image=#{frozen_repos_exist? ? frozen_icon : unfrozen_icon}"
  end

  def insert_separator
    puts "---"
  end

  def build_menu
    if frozen_repos_exist?
      puts "Frozen repos"
      sort_and_print('frozen')
    else
      puts "All repos are clear!"
    end

    puts "Unfrozen repos"
    sort_and_print('unfrozen')

    puts ""
    puts "Contribute / Docs | href=https://github.com/Merge-Freeze/mac-menu-plugin"
  end
end

# check for ENV vars, fetch projects, build interactive menu
MergeFreezeApplet.new.call
