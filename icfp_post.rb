#!/usr/bin/env ruby

require 'rubygems'

require 'net/http'
require 'JSON'

AUTHKEY = "0132X9AqfwFADPNaFAspP3eFjUdCx0fVpxWS6pHd"

def icfp_post(method, body)
  uri = URI("http://icfpc2013.cloudapp.net/")
  http = Net::HTTP.new(uri.host, uri.port)
  req = Net::HTTP::Post.new("/#{method.to_s}?auth=#{AUTHKEY}vpsH1H")
  req.body = body.to_json
  http.request(req)
end

if ARGV[0] == 'myproblems'
  puts icfp_post(:myproblems, nil).body
elsif ARGV[0] == 'status'
  puts icfp_post(:status, nil).body
elsif ARGV[0] == 'eval'
  lines = STDIN.read.split("\n")
  pid = lines.shift
  args = lines
  puts icfp_post(:eval, {:id => pid, :arguments => args}).body
elsif ARGV[0] == 'eval_prog'
  lines = STDIN.read.split("\n")
  pid = lines.shift
  args = lines
  puts icfp_post(:eval, {:program => pid, :arguments => args}).body
elsif ARGV[0] == 'guess'
  lines = STDIN.read.split("\n")
  pid = lines.shift
  program = lines.shift
  result = icfp_post(:guess, {:id => pid, :program => program}).body
  result = JSON.parse(result)
  puts result['status']
  if result['values']
    puts result['values'].join("\n")
  elsif result['message']
    puts result['message']
  end
elsif ARGV[0] == 'train'
  lines = STDIN.read.split("\n")
  reqbody = {}
  if not lines.empty?
    reqbody['size'] = lines.shift.to_i
  end
  if not lines.empty?
    reqbody['operators'] = [lines.shift]
  end
  result = icfp_post(:train, reqbody).body
  result = JSON.parse(result)
  puts result['challenge']
  puts result['id']
  puts result['size']
  puts result['operators'].join(", ")
else
  puts "USAGE: icfp_post.rb <method>"
  puts " * You MAY wish to pipe json output to 'jq .' for formatting."
  puts " * But in case of error, you may get crash spew, so watch for it."
  puts ""
  puts " METHODS:"
  puts " - myproblems"
  puts "   - takes no input, outputs json info about our problems"
  puts " - status"
  puts "   - takes no input, outputs json info about our status"
  puts " - eval"
  puts "   - takes one line of problem id, many lines of arguments to try, EOF"
  puts "   - outputs one line of status ok/error, many lines of output or a line of message"
  puts " - eval_prog"
  puts "   - takes one line of program, many lines of arguments to try, EOF"
  puts "   - outputs one line of status ok/error, many lines of output or a line of message"
  puts " - guess"
  puts "   - takes one line of problem id, one line of program"
  puts "   - outputs:"
  puts "     - one line of status win/mismatch/error;"
  puts "     - for win, no more lines"
  puts "     - for mismatch, 3 lines w/ a faulty input, the correct output, the faulty output"
  puts "     - for error, the error message"
  puts " - train"
  puts "   - takes one optional line of size, one optional line of 'fold' or 'tfold'"
  puts "   - outputs four lines: a problem, a problem id, a size, a list of operators"
end
