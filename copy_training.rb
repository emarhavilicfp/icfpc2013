#!/usr/bin/env ruby

require 'rubygems'

require 'json'

# This is a one-off designed to scrape the file found at:
# https://dl.dropboxusercontent.com/u/10451141/s/3ab76922281901ca48c9034d0eadb802b29d2d6c

File.open("training_examples") do |file|
  while !file.eof? do
    result = {}
    file.readline  # discard
    result['challenge'] = file.readline.strip
    result['id'] = file.readline.strip
    result['size'] = file.readline.strip.to_i
    result['operators'] = file.readline.strip.split(", ")
    file.readline  # discard
    File.open("training/#{result['id']}", "w") do |outfile|
      outfile << result.to_json
      outfile << "\n"
    end
  end
end
