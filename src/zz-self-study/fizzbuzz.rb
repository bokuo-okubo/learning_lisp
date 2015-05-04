fb = -> (n){ "#{"Fizz" if n % 3 == 0}#{"Buzz" if n % 5 == 0}" }
(1..100).each { |n| p fb.(n).empty? ? n : fb.(n) }