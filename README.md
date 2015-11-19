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

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake[ spec]` to run the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

To install this gem onto your local machine, run `bundle exec rake install`. To release a new version, update the version number in `version.rb`, and then run `bundle exec rake release`, which will create a git tag for the version, push git commits and tags, and push the `.gem` file to [rubygems.org](https://rubygems.org).

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/7even/carbonate.

## License

The gem is available as open source under the terms of the [MIT License](http://opensource.org/licenses/MIT).
