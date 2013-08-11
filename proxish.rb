#!/usr/bin/env ruby


require 'socket'
 
if ARGV.length < 1
    $stderr.puts "Usage: #{$0} remoteHost:remotePort [ localPort [ localHost ] ]"
    exit 1
end
 
$remoteHost, $remotePort = ARGV.shift.split(":")
puts "target address: #{$remoteHost}:#{$remotePort}"
localPort = ARGV.shift || $remotePort
localHost = ARGV.shift
 
$blockSize = 1024
 
server = TCPServer.open(localHost, localPort)
 
port = server.addr[1]
addrs = server.addr[2..-1].uniq
 
puts "*** listening on #{addrs.collect{|a|"#{a}:#{port}"}.join(' ')}"
 
# abort on exceptions, otherwise threads will be silently killed in case
# of unhandled exceptions
Thread.abort_on_exception = true
 
# have a thread just to process Ctrl-C events on Windows
# (although Ctrl-Break always works)
Thread.new { loop { sleep 1 } }
 
def connThread(local)
    port, name = local.peeraddr[1..2]
    puts "*** receiving from #{name}:#{port}"
 
    # open connection to remote server
    remote = TCPSocket.new($remoteHost, $remotePort)
     
    # start reading from both ends
    loop do
        ready = select([local, remote], nil, nil)
        if ready[0].include? local
            # local -> remote
            data = local.recv($blockSize)
            if data.empty?
                puts "local end closed connection"
                break
            end
            remote.write(data)
        end
        if ready[0].include? remote
            # remote -&gt; local
            data = remote.recv($blockSize)
            if data.empty?
                puts "remote end closed connection"
                break
            end
            local.write(data)
        end
    end
     
    local.close
    remote.close
     
    puts "*** done with #{name}:#{port}"
end
 
loop do
    # whenever server.accept returns a new connection, start
    # a handler thread for that connection
    Thread.start(server.accept) { |local| connThread(local) }
end
