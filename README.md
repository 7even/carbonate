# Carbonate

Carbonate is a Lisp dialect heavily influenced by Clojure. It is transpiled into Ruby code.

Carbonate tries to cover all of Ruby's functionality while giving a more concise form to the code.

## Installation

```ruby
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

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake spec` to run the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

To install this gem onto your local machine, run `bundle exec rake install`. To release a new version, update the version number in `version.rb`, and then run `bundle exec rake release`, which will create a git tag for the version, push git commits and tags, and push the `.gem` file to [rubygems.org](https://rubygems.org).

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/7even/carbonate.

## License

The gem is available as open source under the terms of the [MIT License](http://opensource.org/licenses/MIT).
