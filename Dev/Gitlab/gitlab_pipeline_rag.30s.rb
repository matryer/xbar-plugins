#!/usr/bin/env ruby

# <xbar.title>Gitlab Pipeline RAG</xbar.title>
# <xbar.desc>Shows the RAG status of your gitlab piplines.</xbar.desc>
# <xbar.author>roovo</xbar.author>
# <xbar.author.github>roovo</xbar.author.github>
# <xbar.version>v0.01</xbar.version>
# <xbar.dependencies>ruby</xbar.dependencies>
# <xbar.image>https://raw.githubusercontent.com/roovo/xbar_gitlab_pipeline_rag/refs/heads/main/gitlab_pipeline_rag.png</xbar.image>
# <xbar.abouturl>https://github.com/roovo/xbar_gitlab_pipeline_rag</xbar.abouturl>

# <xbar.var>string(GITLAB_URL=""): URL of your gitlab instance</xbar.var>
# <xbar.var>string(GITLAB_TOKEN=""): Gitlab access token</xbar.var>
# <xbar.var>string(PROJECTS_JSON=""): JSON object literal of projects and their Gitlab IDs, e.g. {"Project 1":123, "Project 2":17}</xbar.var>

require 'net/http'
require 'json'

def api_fetch(project_id)
  uri = URI("#{ENV['GITLAB_URL']}/api/v4/projects/#{project_id}/pipelines")
  params = { private_token: ENV['GITLAB_TOKEN'],
             order_by: 'updated_at',
             sort: 'desc',
             page: 1,
             per_page: 20 }
  uri.query = URI.encode_www_form(params)

  res = Net::HTTP.get_response(uri)
  if res.is_a?(Net::HTTPSuccess)
    JSON.parse res.body
  else
    []
  end
end

def latest_pipeline(pipelines)
  pipelines
    .filter { |p| p['status'] != 'canceled' }
    .first || {}
end

def overall_status(statuses)
  if statuses.include? 'failed'
    'failed'
  elsif statuses.uniq == ['success']
    'success'
  else
    'running'
  end
end

def icon(status)
  case status
  when 'success'
    '🟢'
  when 'failed'
    '🔴'
  else
    '🟠'
  end
end

projects = JSON.parse ENV['PROJECTS_JSON']
project_pipelines = projects.map { |name, id| [name, api_fetch(id)] }
latest_pipelines = project_pipelines.map { |name, p| [name, latest_pipeline(p)] }
                                    .reject { |_, p| p.empty? }
latest_statuses = latest_pipelines.map(&:last).map { |p|  p.fetch('status', 'unknown') }
overall = overall_status(latest_statuses)

puts icon(overall)
puts "---"

latest_pipelines.each do |name, pipline|
  puts "#{icon(pipline.fetch('status', 'running'))} #{name} | href=#{pipline.fetch('web_url').gsub(/ *\d+$/, '')}"
end
