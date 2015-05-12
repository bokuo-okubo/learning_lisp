require 'pry'

def open_file(path)
  file = open(path)
  toOut=[]
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
    toOut << line unless in_comment
  end

  toOut
end

def write_file(path, ary)
  File.open(path,'w'){|f| ary.each{ |line| f << line}}
end


filename = gets.chomp
write_file("nc_#{filename}" ,open_file(filename) )
