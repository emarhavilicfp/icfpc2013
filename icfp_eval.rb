#!/usr/bin/env ruby

require 'rubygems'

require 'json'

SIXTY_FOUR = 0xffffffffffffffff

OPS1 = {
  :not => 1,
  :shl1 => 1,
  :shr1 => 1,
  :shr4 => 1,
  :shr16 => 1 }

OPS2 = {
  :and => 1,
  :or => 1,
  :xor => 1,
  :plus => 1 }

def assert(x)
  if not x
    raise "oops"
  end
end

def icfp_bytes(x)
  rv = []
  (1 .. 8).each do |_|
    rv << (x & 0xFF)
    x = x >> 8
  end
  rv
end

def pretty_list(p)
  p.map{|x| pretty x}.join(" ")
end

def pretty(p)
  #puts "pretty #{p}"
  if p.kind_of?(Array)
    "(#{pretty_list p})"
  elsif p.kind_of?(Fixnum)
    "0x#{p.to_s(16)}"
  elsif p.kind_of?(Symbol)
    p.to_s
  elsif p.nil?
    "#ERROR<NIL>"
  else
    "#ERROR<#{p}>" 
  end
end

def pphash(h)
  "{#{
  h.map {|k, v|
    "#{k} => #{pretty v}"
  }.join(", ")
  }}"
end

@ctr = 0
def gensym
  @ctr += 1
  "x_#{@ctr}".to_sym
end

def op1_not(x)
  x ^ SIXTY_FOUR
end

def op1_shl1(x)
  (x << 1) & SIXTY_FOUR
end

def op1_shr1(x)
  x >> 1
end

def op1_shr4(x)
  x >> 4
end

def op1_shr16(x)
  x >> 16
end

def op2_and(x, y)
  x & y
end

def op2_or(x, y)
  x | y
end

def op2_xor(x, y)
  x ^ y
end

def op2_plus(x, y)
  (x + y) & SIXTY_FOUR
end

def do_if0(e1, e2, e3, env)
  if icfp_eval(e1, env) == 0
    icfp_eval(e2, env)
  else
    icfp_eval(e3, env)
  end
end

def do_fold(val, acc, e, env)
  b = icfp_bytes(icfp_eval(val, env))
  assert(e[0] == :lambda)
  p0 = e[1][0]
  p1 = e[1][1] 
  b.reduce(acc) do |acc, byte|
    icfp_eval(e[2], env.merge({p0 => byte, p1 => acc}))
  end
end

def lam
  var = gensym
  exp = yield var
  [:lambda, [var], exp]
end

def if0(e0, e1, e2)
  [:if0, e0, e1, e2]
end

def op1(op, e)
  if not OPS1.has_key?(op)
    raise "Bad op #{op}"
  end
  [op, e]
end

def op2(op, e1, e2)
  if not OPS2.has_key?(op)
    raise "Bad op #{op}"
  end
  [op, e1, e2]
end

def fold(val, acc)
  var1 = gensym
  var2 = gensym
  exp = yield (var1, var2)
  [:fold, val, acc, [:lambda, [var1, var2], exp]]
end

def icfp_run(p, args)
  puts "running #{pretty p} on #{args.join(' ')}"
  args.map do |arg|
    icfp_eval(p[2], {p[1][0] => arg})
  end
end

def icfp_eval(p, env)
  puts "eval #{pretty p} in env #{pphash env}"
  if p.kind_of?(Fixnum)
    p
  elsif p.kind_of?(Symbol)
    puts "p #{p} class #{p.class} env[p] #{env[p]}"
    env[p]
  elsif p.kind_of?(Array)
    if OPS1.has_key?(p[0])
      send("op1_#{p[0]}", icfp_eval(p[1], env))
    elsif OPS2.has_key?(p[0])
      send("op2_#{p[0]}", icfp_eval(p[1], env), icfp_eval(p[2], env))
    elsif p[0] == :if0
      do_if0(p[1], p[2], p[3], env)
    elsif p[0] == :fold
      do_fold(p[1], p[2], p[3], env)
    end
  else
    raise "Bad eval #{p} (#{env})"
  end
end

def brute_1op(env, ops)
  
end

p = lam do |x|
  op1 :shl1, x
end

puts pretty p

puts icfp_run(lam {|x|
  op2(:and, op1(:shl1, x), 0xF)
}, [0x0, 0xF, 0xFF])

p1 = lam do |x|
  fold(x, 0) do |y, z| 
    op2(:or, y, z)
  end
end

puts pretty p1

puts icfp_run(p1, [0x1122334455667788])
