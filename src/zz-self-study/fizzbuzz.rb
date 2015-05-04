def fizzbuzz(n)
  str = ""
  str << "Fizz" if (n % 3).zero? 
  str << "Buzz" if (n % 5).zero? 
  str.empty? ? n : str
end

p (1..100).map {|n| fizzbuzz n }