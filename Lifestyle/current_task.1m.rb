#!/usr/bin/ruby

# You need to call it with string argument to change a task like this:
# `./current_task.1m.rb "Finish important business"`
#
# The task is stored in a file

TASK_FILE_NAME = File.dirname(__FILE__) + "/task"

`touch #{TASK_FILE_NAME}`

def change_task(task)
  File.write(TASK_FILE_NAME, task)
end

def read_task
  puts IO.read(TASK_FILE_NAME)
end

if ARGV[0].nil?
  read_task
else
  change_task(ARGV[0])
end
