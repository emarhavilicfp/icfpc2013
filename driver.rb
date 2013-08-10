#!/usr/bin/env ruby

require 'rubygems'

require 'net/http'
require 'json'
require 'optparse'

AUTHKEY = "0132X9AqfwFADPNaFAspP3eFjUdCx0fVpxWS6pHd"

def main
  @options = {}
  @options[:transcript] = true

  OptionParser.new do |opts|
    opts.banner = "Usage: driver.rb [--tty] [--quiet] {--real ID | [-n N] [--fold fold|tfold|nofold]} [-- <solver args>]"

    opts.on("-r", "--real ID", "Real mode, with problem ID to solve (default is training mode)") do |r|
      @options[:pid] = r
    end

    opts.on("-n", "--num N", "Number of operators to request in training example") do |n|
      @options[:nops] = n
    end

    opts.on("--fold fold|nofold|tfold", "Whether and which fold to allow in training example") do |f|
      @options[:fold] = f
    end

    opts.on("--tty", "Talk to the tty instead of the solver") do |t|
      @options[:tty] = t
    end

    opts.on("-q", "--quiet", "Don't dump all communications. (You shouldn't use this.)") do |t|
      @options[:transcript] = false
    end
  end.parse!

  example = nil
  if @options[:pid]
    myproblems = JSON.parse(File.open('myproblems').readlines.join(''))
    example = myproblems.select{|x| x['id'] == @options[:pid]}[0]
  else
    trainopts = {}
    if @options[:nops]
      trainopts[:size] = @options[:nops].to_i
    end
    if @options[:fold] == 'nofold'
      trainopts[:operators] = []
    elsif @options[:fold]
      trainopts[:operators] = [@options[:fold]]
    end
    example = do_train trainopts
  end

  solver_args = "--length=#{example['size']} "
  solver_args += example['operators'].map {|x| "--has-#{x}-op"}.join(" ")
  solver_args += ' ' + ARGV.join(" ")
  
  def with_pipe(solver_args)
    if @options[:tty]
      puts "Args to solver would have been: #{solver_args}"
      yield File.open("/dev/tty", "r+")
    else
      yield IO.popen("sh -c 'bin/solve #{solver_args}'", "r+")
    end
  end

  transcribe "INVOKE_SOLVER: sh -c 'bin/solve #{solver_args}'"
  with_pipe solver_args do |solver_io|
    begin
      line = nil
      while(line = solver_io.readline)
        transcribe "SOLVER_IN: #{line}"
        cmd, arg = line.split(":")
        if cmd == 'GUESS'
          result = do_guess example['id'], arg.strip
          if result['status'] == 'win'
            transcribe "SOLVER_OUT: RIGHT"
            solver_io << "RIGHT\n"
            exit 0
          elsif result['status'] == 'mismatch'
            alert_the_media "FAILURE on problem #{example['id']}"
            transcribe "SOLVER_OUT: WRONG"
            solver_io << "WRONG\n"
            transcribe result['values'].map {|x| "SOLVER_OUT: #{x}"}
            solver_io << result['values'].join("\n") + "\n"
          elsif result['status'] == 'error'
            alert_the_media "GUESS ERROR on problem #{example['id']}: #{pphash result}"
            transcribe "SOLVER_OUT: MAYBE"
            solver_io << "MAYBE\n"
          else
            raise "Something went wrong: we got: #{result}"
          end
        elsif cmd == 'EVAL'
          args = []
          (1 .. arg.to_i).each do |_|
            args << solver_io.readline.strip
          end
          transcribe args.map {|x| "SOLVER_IN: #{x}"}
          result = do_eval example['id'], args
          if result['status'] == 'ok'
            transcribe "SOLVER_OUT: OKAY"
            solver_io << "OKAY\n"
            transcribe result['outputs'].map {|x| "SOLVER_OUT: #{x}"}
            solver_io << result['outputs'].join("\n") + "\n"
          elsif result['status'] == 'error'
            raise "API ERROR: #{result['message']} (#{pphash result})"
          else 
            raise "Something went wrong: we got: #{result}"
          end
        else
          raise "Bad line from solver: #{line}"
        end
      end
    rescue => e
      puts "DONE: Exception was #{e}"
      alert_the_media "EXCEPTION on problem #{example['id']}: #{e}"
    end
  end

  exit 0
end

def alert_the_media(message)
  uri = URI("http://icfp.nyus.compound.emarhavil.com/")
  http = Net::HTTP.new(uri.host, uri.port)
  req = Net::HTTP::Post.new("/alert")
  req.body = message + "\n" + `id -un`.strip + '@' + `hostname`.strip
  http.request(req)
end

def transcribe(x)
  if @options[:transcript]
    if x.kind_of?(Array) 
      x.each do |l|
        $stderr.puts l
      end
    else
      $stderr.puts x
    end
  end
end

def pretty(x)
  x.to_s
end

def pphash(h)
  "{#{
  h.map {|k, v|
    "#{k} => #{pretty v}"
  }.join(", ")
  }}"
end

def assert(x)
  if not x
    raise "assert"
  end
end

DELAY = 5
def icfp_post(method, body)
  uri = URI("http://icfpc2013.cloudapp.net/")
  http = Net::HTTP.new(uri.host, uri.port)
  req = Net::HTTP::Post.new("/#{method.to_s}?auth=#{AUTHKEY}vpsH1H")
  req.body = body.to_json
  keep_going = true
  while keep_going do
    keep_going = false
    starttime = Time.now.to_i
    transcribe "API_REQUEST: #{method} #{req.body}"
    result = http.request(req)
    transcribe "API_RESPONSE: #{result.body}"
    if result.body =~ /Too many requests/
      if Time.now.to_i - starttime < DELAY
        sleep (Time.now.to_i - starttime)
      end
      keep_going = true
    end
  end
  result
end

def do_train(opts)
  result = icfp_post(:train, opts).body
  JSON.parse(result)
end

def do_guess(id, program)
  result = icfp_post(:guess, {:id => id, :program => program}).body
  JSON.parse(result)
end

def do_eval(id, args)
  result = icfp_post(:eval, {:id => id, :arguments => args}).body
  JSON.parse(result)
end
  
main

exit 0

if ARGV[0] == 'myproblems'
  puts icfp_post(:myproblems, nil).body
elsif ARGV[0] == 'status'
  puts icfp_post(:status, nil).body
elsif ARGV[0] == 'eval'
  lines = STDIN.read.split("\n")
  pid = lines.shift
  args = lines
  puts do_eval pid, args
elsif ARGV[0] == 'eval_prog'
  lines = STDIN.read.split("\n")
  pid = lines.shift
  args = lines
  puts icfp_post(:eval, {:program => pid, :arguments => args}).body
elsif ARGV[0] == 'guess'
  lines = STDIN.read.split("\n")
  pid = lines.shift
  program = lines.shift
  result = do_guess pid, program
  puts result['status']
  if result['values']
    puts result['values'].join("\n")
  elsif result['message']
    puts result['message']
  end
elsif ARGV[0] == 'train'
  lines = STDIN.read.split("\n")
  opts = {}
  if not lines.empty?
    opts['size'] = lines.shift.to_i
  end
  if not lines.empty?
    opts['operators'] = [lines.shift]
  end
  result = do_train opts
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
