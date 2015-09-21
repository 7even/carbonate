$LOAD_PATH.unshift File.expand_path('../../lib', __FILE__)
require 'carbonate'
require 'awesome_pry'

RSpec.configure do |config|
  config.filter_run focus: true
  config.run_all_when_everything_filtered = true
end
