# Carbonate

Carbonate is a Lisp dialect heavily influenced by Clojure. It is transpiled into Ruby code.

Carbonate tries to cover all of Ruby's functionality while giving a more concise form to the code.

Here's what it looks like:

``` clojure
(defclass User
  (defmethod initialize [first-name last-name email]
    (def @first-name first-name)
    (def @last-name last-name)
    (def @email email))
  (defmethod full-name []
    (join [@first_name @last_name]))
  (defmethod each-name []
    (each [@first-name @last-name] #([name] (@yield name)))))
```

The code above is equvalent to the following ruby:

``` ruby
class User
  def initialize(first_name, last_name, email)
    @first_name = first_name
    @last_name  = last_name
    @email      = email
  end

  def full_name
    [@first_name, @last_name].join
  end

  def each_name
    [@first_name, @last_name].each do |name|
      yield name
    end
  end
end
```

## Installation

``` ruby
# Gemfile
gem 'carbonate'
```

``` sh
$ bundle
```

## Usage

Currently there are 2 ways to run Carbonate code: convert it to Ruby statically and use the resulting `.rb` files as you normally would or evaluate Carbonate source files dynamically.

### Converting from Carbonate to Ruby

The gem ships with a `crb2rb` utility that converts Carbonate source code into Ruby source code. You can use it to convert a Carbonate file to a Ruby file:

``` sh
$ crb2rb < source.crb > target.rb
# or
$ crb2rb -i source.crb -o target.rb
```

### Using Carbonate sources directly

Carbonate source code can be transpiled and instantly evaluated by Ruby code. This allows you to plug it in a Ruby application and use it right away.

Carbonate gives 2 functions to transpile and evaluate Carbonate sources: `Carbonate.require` and `Carbonate.require_relative` - they work exactly like their counterparts from Ruby's `Kernel` but they are searching for a `.crb` file instead of a `.rb` one, and they transpile it to Ruby before evaluating.

## Syntax

### Literal values

Numbers in Carbonate look exactly like they do in Ruby:

``` clojure
127
-32
3.14
```

Strings are always double-quoted and support all the usual control characters like `\n` and `\t`. Double quotes have to be escaped with a backslash. String interpolation is not supported.

``` clojure
"Hello world!"
"Line 1\nLine 2\n\tIndented line"
"Yukihiro \"Matz\" Matsumoto"
```

Symbols look similarly to Ruby symbols but use dashes (`-`) instead of underscores (`_`):

``` clojure
:north
:user-name
:exists?
```

Regular expressions are written as double-quoted strings prefixed with a pound sign (`#`). Like strings, they support control characters and require you to escape double quotes.

``` clojure
#"[A-Za-z]+"
```

`true`, `false`, and `nil` are the same as in Ruby.

Arrays are enclosed within brackets (`[]`) but do not require commas between elements. In fact, comma is treated as a whitespace character in Carbonate - you can use it but you don't have to.

``` clojure
[1 2 3]
["Yukihiro Matsumoto" "Rich Hickey"]
```

Hashes are represented by key-value pairs inside curly brackets (`{}`). In contrast to Ruby, there are no delimiters between a key and a value. Separating pairs with commas can sometimes be useful to keep readability.

``` clojure
{:type :book
 :title "SICP"
 :authors ["Harold Abelson" "Gerald Jay Sussman" "Julie Sussman"]}
```

Sets are also enclosed within curly brackets but prefixed with a pound sign.

``` clojure
#{"one" "two" "three"}
```

Be sure to `require 'set'` to use them - sets live in a standard library package in Ruby (read on to learn how to call methods like `require` in Carbonate).

Ranges look exactly like in Ruby - values separated with two dots for inclusive ranges and values separated with three dots for exclusive ones:

``` clojure
"a".."z"
0...10
```

Constants are written down using the same CamelCase'd words as in Ruby but `.` is used as a delimiter:

``` clojure
Carbonate.Parser
```

Explicit top-level constants are prefixed with `.` (exactly like they are with `::` in Ruby):

``` clojure
.Hash
```

The current object known as `self` in Ruby is written down as `@` in Carbonate.

### Calling functions/methods

In Lisp function calls are written down using prefix notation in S-expressions; basically this means that every operation is a list of elements enclosed within parentheses where the first element represents the function and all the other elements are it's arguments.

Here's some basic arithmetic:

``` clojure
(+ 2 2)
(- 2 1)
(* 2 3)
```

Of course S-expressions can be nested:

``` clojure
(/ (* 3 4) 2)
(** (- 4 2) 3)
```

Comparison operators also mirror the Ruby ones with an exception of equality - it is represented with a single `=`:

``` clojure
(= x y)
(!= x y)
(< 1 2)
(> 2 1)
(>= 2 2)
(<= 2 2)
(<=> a b)
(=== a b)
```

Binary operators `&`, `|`, `^`, `~`, `<<` and `>>` follow their Ruby counterparts. Logic operators, on the other hand, are slightly changed:

``` clojure
(and (= x y) (!= x z))
(or (> x z) (> y z))
(! false)
```

### Variables and assignment

Carbonate supports local and instance variables. They look like in Ruby but use `-` separator instead of `_`:

``` clojure
local-variable
@instance-variable
```

You can assign a value to a variable using `def` keyword:

``` clojure
(def a 1)
(def @age 35)
```

This also works for constants:

``` clojure
(def DAYS-IN-WEEK 7)
```

Carbonate also supports the so-called conditional assignment (`||=` in Ruby) using `def-or` keyword:

``` clojure
(def-or name "Steve")
(def-or @city "NYC")
```

(This is not supported with constants for obvious reasons)

There are a few more special cases. First you can assign a value to an object's attribute (like you would do with `user.name = 'John'` in Ruby):

``` clojure
(def user.name "John")
```

Second you can both read from and write to an array or a hash member using essentially the same syntax as in Ruby:

``` clojure
user[:name]
(def user[:name] "John")
```

Both attribute and collection member writers support conditional assignment with the aforementioned `def-or`.

### Conditional statements

Almost any code contains conditional execution - you won't go far without `if` & `unless` statements so here they are:

``` clojure
(if (> 2 1) "2 is greater" "1 is greater")
(if (= x 5) "x is 5")
(unless (>= age 18) "too young")
```

As you can see from the snippet above, `if` can be used in 2 variations: if you pass it a condition and 2 more forms (S-expressions, literal values, variables or anything that returns a value) the first form will be used for "truthy" condition value and the second for "falsy". If you just pass one form it will be used for the "truthy" case.

`unless` doesn't have a 2-form mode - it's a bad practice anyway - so the only form after the condition will be used for the falsy condition.

Carbonate also has a `case` statement:

``` clojure
(case x
  1 "one"
  2 "two")
```

Pretty self-explanatory. It also supports an else clause as the last form of the statement:

``` clojure
(case lang
  "clojure" "great!"
  "ruby" "cool"
  "crap")
```

### Loop statements

There are 2 main loop statements in Carbonate: `while` and `until`. Both of them take a condition as the first argument and the loop body as the second:

``` clojure
(while (< x 5)
       (def x (+ x 1)))
(until (>= x 5)
       (def x (+ x 1)))
```

### Calling methods

The most common construct in Ruby code is a method call. Carbonate allows you to call a method within an S-expression consisting of the method name and the receiver object:

``` clojure
(name user)
```

This is equivalent to the following Ruby:

``` ruby
user.name
```

*(all Carbonate snippets are followed by equivalent Ruby snippets later on)*

If you need to pass some arguments to a method you do so after the receiver:

``` clojure
(include? [1 2 3] 4)
```

``` ruby
[1, 2, 3].include?(4)
```

Carbonate supports splat arguments - if you have some `Enumerable` collection you can pass it to the method as several separate arguments. Ruby uses `*` for that goal, Carbonate uses `&` (note that `&` and the argument are separated by space):

``` clojure
(add-tags article & tags)
```

``` ruby
article.add_tags(*tags)
```

Class methods are invoked a little differently - the method name is prefixed with the class name separated with `/`, and all following elements are method's arguments:

``` clojure
(User/count)
(User/find-by {:first-name "John"})
```

``` ruby
User.count
User.find_by(first_name: 'John')
```

Method calls without an explicit receiver (which are implicitly called on `self`) are written with a method name prefixed by `@`:

``` clojure
(@attr-reader :first-name)
```

``` ruby
attr_reader :first_name
```

Carbonate offers a special syntax for class constructor calls - it looks like a class name followed by a dot:

``` clojure
(User. {:name "John"})
```

``` ruby
User.new(name: 'John')
```

Another special case is `super` - a call to parent class' respective method:

``` clojure
(super)
(super "parameter")
```

``` ruby
super()
super('parameter')
```

The tricky part here is a call to `super` with implicit parameters - as you may know, calling `super` without parameters and without parentheses in Ruby actually passes it all the parameters passed to the enclosing method, and if you need to force `super` call without parameters you have to write `super()`. The latter is written as just `(super)` in Carbonate and the former is `(zsuper)` (Zero-arity super).

Like in Ruby, you can pass a block to a method - it is enclosed within parentheses prefixed with `#`, and the first element inside the parentheses is the block parameters list:

``` clojure
(map users #([user] (upcase (name user))))
```

``` ruby
users.map do |user|
  user.name.upcase
end
```

You don't have to specify an empty parameters list if your block has no parameters - just type your block body inside `#(...)`:

``` clojure
(times 5 #(@puts "Hello"))
```

``` ruby
5.times { puts 'Hello' }
```

If you want to use the `Symbol#to_proc` trick you can just pass the symbol after `#` (like `&`, it needs to be separated from the following element with a space):

``` clojure
(map users # :name)
```

``` ruby
users.map(&:name)
```

This works with plain procs too:

``` clojure
(each users # some-proc)
```

``` ruby
users.each(&some_proc)
```

### Defining methods

A method definition consists of the `defmethod` keyword, then the name of the method, then the parameters list and the method body. As usual, all the identifiers (method name, parameters and variables) use `-` instead of `_`. It looks like this:

``` clojure
(defmethod full-name []
  (join [@first-name @last-name]))
```

``` ruby
def full_name
  [@first_name, @last_name].join
end
```

The parameters list supports splat arguments and blocks with basically the same syntax as in method invocation:

``` clojure
(defmethod iterate [a b c & d # block]
  (@puts a b c d)
  (@yield a)
  (@yield b)
  (@yield c)
  (each d # block))
```

``` ruby
def iterate(a, b, c, *d, &block)
  puts a, b, c, d
  yield a
  yield b
  yield c
  d.each(&block)
end
```

Carbonate also supports default parameter values - just put the parameter name with a default value inside the brackets:

``` clojure
(defmethod int-to-string [int [base 10]]
  (to-s int base))
```

``` ruby
def int_to_string(int, base = 10)
  int.to_s(base)
end
```

If you need to use a rescue clause inside your method you can just put it at the end of the method body:

``` clojure
(defmethod read-file [path]
  (File/read path)
  (rescue Errno.ENOENT e
    (@puts "No file found.")))
```

``` ruby
def read_file(path)
  File.read(path)
rescue Errno::ENOENT => e
  puts 'No file found.'
end
```

*(you can have as many rescue clauses as you want - just be sure to keep them at the end of method body)*

The ensure clause is available as well, you can put it after all rescue clauses:

``` clojure
(defmethod read-file [path]
  (File/read path)
  (rescue Errno.ENOENT e (@puts "No file found."))
  (ensure (@puts "Tried to read a file.")))
```

``` ruby
def read_file(path)
  File.read(path)
rescue Errno::ENOENT => e
  puts 'No file found.'
ensure
  puts 'Tried to read a file.'
end
```

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake[ spec]` to run the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

To install this gem onto your local machine, run `bundle exec rake install`. To release a new version, update the version number in `version.rb`, and then run `bundle exec rake release`, which will create a git tag for the version, push git commits and tags, and push the `.gem` file to [rubygems.org](https://rubygems.org).

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/7even/carbonate.

## License

The gem is available as open source under the terms of the [MIT License](http://opensource.org/licenses/MIT).
