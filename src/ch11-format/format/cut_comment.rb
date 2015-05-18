require 'pry'
# type the file path on stdin,
# then generate nc_[filepath]
# (specific common lisp)

def read_file(path)
  file = open(path)
end

def write_file(path, ary, prefix=nil)
  path = prefix + path if prefix
  File.open(path,'w'){|f| ary.each{ |line| f << line unless line.empty? }}
end

def cut_comment(file)
  output = []
  in_comment = false
  file.each do |line|
    case line
    when /^\;/
      next
    when /^\#\|/
      in_comment = true
    when /^\|\#/
      in_comment = false
      next
    end
    output << line unless in_comment
  end
  output
end

prefix = "nc_"
filename = gets.chomp
output = cut_comment( read_file(filename) )
write_file(filename, output, prefix)
