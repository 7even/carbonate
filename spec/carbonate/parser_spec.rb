require 'spec_helper'

RSpec.describe Carbonate::Parser do
  class << self
    def s(type, *children)
      Parser::AST::Node.new(type, children)
    end

    def should_parse(from:, to:, debug: false)
      options = debug ? { focus: true } : {}
      it 'parses the source into AST', **options do
        expect(subject.parse(from, debug)).to eq(to)
      end
    end
  end

  context 'with numbers' do
    context 'integer' do
      should_parse(from: '1',  to: s(:int, 1))
      should_parse(from: '-1', to: s(:int, -1))
    end

    context 'float' do
      should_parse(from: '3.14',  to: s(:float, 3.14))
      should_parse(from: '-3.14', to: s(:float, -3.14))
    end
  end

  context 'with strings' do
    context 'without escaped characters' do
      should_parse(
        from: '"some string"',
        to: s(:str, 'some string')
      )
    end

    context 'with escaped characters' do
      should_parse(
        from: '"line1\nline2\rline3\tline4 \'single-quoted text\' \"double-quoted text\""',
        to: s(:str,
          "line1\nline2\rline3\tline4 'single-quoted text' \"double-quoted text\""
        )
      )
    end
  end

  context 'with symbols' do
    should_parse(
      from: ':some-symbol',
      to: s(:sym, :some_symbol)
    )

    should_parse(
      from: ':exists?',
      to: s(:sym, :exists?)
    )
  end

  context 'with regexps' do
    should_parse(
      from: '#"[A-Za-z]+"',
      to: s(:regexp,
        s(:str, '[A-Za-z]+'),
        s(:regopt)
      )
    )
  end

  context 'with arrays' do
    should_parse(
      from: '[1 2 3]',
      to: s(:array,
        s(:int, 1),
        s(:int, 2),
        s(:int, 3)
      )
    )

    should_parse(from: '[]', to: s(:array))
  end

  context 'with hashes' do
    should_parse(
      from: '{:first-name "Rich", :last-name "Hickey"}',
      to: s(:hash,
        s(:pair,
          s(:sym, :first_name),
          s(:str, 'Rich')
        ),
        s(:pair,
          s(:sym, :last_name),
          s(:str, 'Hickey')
        )
      )
    )

    should_parse(from: '{}', to: s(:hash))

    context 'with odd number of elements' do
      it 'raises a FormatError' do
        expect {
          subject.parse('{:key1 :value1 :key2}')
        }.to raise_error(Carbonate::Parser::FormatError)
      end
    end
  end

  context 'with sets' do
    should_parse(
      from: '#{123 "string" :symbol}',
      to: s(:send,
        s(:const, nil, :Set),
        :new,
        s(:array,
          s(:int, 123),
          s(:str, 'string'),
          s(:sym, :symbol)
        )
      )
    )
  end

  context 'with ranges' do
    should_parse(from: '1..10',  to: s(:irange, s(:int, 1), s(:int, 10)))
    should_parse(from: '1...11', to: s(:erange, s(:int, 1), s(:int, 11)))
  end

  context 'with operators' do
    should_parse(from: '(+ 2 2)',  to: s(:send, s(:int, 2), :+, s(:int, 2)))
    should_parse(from: '(- 2 1)',  to: s(:send, s(:int, 2), :-, s(:int, 1)))
    should_parse(from: '(* 2 3)',  to: s(:send, s(:int, 2), :*, s(:int, 3)))
    should_parse(from: '(/ 6 2)',  to: s(:send, s(:int, 6), :/, s(:int, 2)))
    should_parse(from: '(% 7 3)',  to: s(:send, s(:int, 7), :%, s(:int, 3)))
    should_parse(from: '(** 2 8)', to: s(:send, s(:int, 2), :**, s(:int, 8)))

    should_parse(from: '(= x y)',   to: s(:send, s(:lvar, :x), :==, s(:lvar, :y)))
    should_parse(from: '(!= x y)',  to: s(:send, s(:lvar, :x), :!=, s(:lvar, :y)))
    should_parse(from: '(< 1 2)',   to: s(:send, s(:int, 1), :<, s(:int, 2)))
    should_parse(from: '(> 2 1)',   to: s(:send, s(:int, 2), :>, s(:int, 1)))
    should_parse(from: '(>= 2 2)',  to: s(:send, s(:int, 2), :>=, s(:int, 2)))
    should_parse(from: '(<= 2 2)',  to: s(:send, s(:int, 2), :<=, s(:int, 2)))
    should_parse(from: '(<=> a b)', to: s(:send, s(:lvar, :a), :<=>, s(:lvar, :b)))
    should_parse(from: '(=== a b)', to: s(:send, s(:lvar, :a), :===, s(:lvar, :b)))

    should_parse(from: '(& 1 3)',  to: s(:send, s(:int, 1), :&, s(:int, 3)))
    should_parse(from: '(| 1 3)',  to: s(:send, s(:int, 1), :|, s(:int, 3)))
    should_parse(from: '(^ 1 2)',  to: s(:send, s(:int, 1), :^, s(:int, 2)))
    should_parse(from: '(~ 3)',    to: s(:send, s(:int, 3), :~))
    should_parse(from: '(<< 2 4)', to: s(:send, s(:int, 2), :<<, s(:int, 4)))
    should_parse(from: '(>> 4 1)', to: s(:send, s(:int, 4), :>>, s(:int, 1)))

    should_parse(from: '(&& a b)', to: s(:send, s(:lvar, :a), :and, s(:lvar, :b)))
    should_parse(from: '(|| a b)', to: s(:send, s(:lvar, :a), :or, s(:lvar, :b)))
    should_parse(from: '(! x)',    to: s(:send, s(:lvar, :x), :!))
  end

  context 'with collection member reader' do
    should_parse(
      from: 'hash[:key]',
      to: s(:send,
        s(:lvar, :hash),
        :[],
        s(:sym, :key)
      )
    )

    should_parse(
      from: 'array[1, 5]',
      to: s(:send,
        s(:lvar, :array),
        :[],
        s(:int, 1),
        s(:int, 5)
      )
    )
  end

  context 'with collection member writer' do
    should_parse(
      from: '(def hash[:key] value)',
      to: s(:send,
        s(:lvar, :hash),
        :[]=,
        s(:sym, :key),
        s(:lvar, :value)
      )
    )

    should_parse(
      from: '(def-or hash[:key] value)',
      to: s(:or_asgn,
        s(:send,
          s(:lvar, :hash),
          :[],
          s(:sym, :key)
        ),
        s(:lvar, :value)
      )
    )

    should_parse(
      from: '(def array[1 2] 3)',
      to: s(:send,
        s(:lvar, :array),
        :[]=,
        s(:int, 1),
        s(:int, 2),
        s(:int, 3)
      )
    )
  end

  context 'with true, false and nil' do
    should_parse(from: 'true', to: s(:true))
    should_parse(from: 'false', to: s(:false))
    should_parse(from: 'nil', to: s(:nil))
  end

  context 'with self' do
    should_parse(
      from: '(+ 2 @)',
      to: s(:send, s(:int, 2), :+, s(:self))
    )
  end

  context 'with a constant' do
    context 'unscoped' do
      should_parse(
        from: 'User',
        to: s(:const, nil, :User)
      )
    end

    context 'scoped' do
      should_parse(
        from: 'Carbonate.Parser',
        to: s(:const, s(:const, nil, :Carbonate), :Parser)
      )
    end

    context 'top level' do
      should_parse(
        from: '.User',
        to: s(:const, s(:cbase), :User)
      )

      should_parse(
        from: '.Carbonate.Parser',
        to: s(:const, s(:const, s(:cbase), :Carbonate), :Parser)
      )
    end
  end

  context 'with an assignment' do
    context 'of a local variable' do
      should_parse(
        from: '(def username "7even")',
        to: s(:lvasgn, :username, s(:str, '7even'))
      )

      should_parse(
        from: '(def-or age 31)',
        to: s(:or_asgn, s(:lvasgn, :age), s(:int, 31))
      )
    end

    context 'of an instance variable' do
      should_parse(
        from: '(def @first-name "John")',
        to: s(:ivasgn, :@first_name, s(:str, 'John'))
      )

      should_parse(
        from: '(def-or @first-name "John")',
        to: s(:or_asgn, s(:ivasgn, :@first_name), s(:str, 'John'))
      )
    end

    context "of an object's attribute" do
      should_parse(
        from: '(def user.name "John")',
        to: s(:send,
          s(:lvar, :user),
          :name=,
          s(:str, 'John')
        )
      )

      should_parse(
        from: '(def @user.name "John")',
        to: s(:send,
          s(:ivar, :@user),
          :name=,
          s(:str, 'John')
        )
      )

      should_parse(
        from: '(def-or user.name "John")',
        to: s(:or_asgn,
          s(:send,
            s(:lvar, :user),
            :name
          ),
          s(:str, 'John')
        )
      )
    end

    context 'of a constant' do
      should_parse(
        from: '(def DAYS-IN-WEEK 7)',
        to: s(:casgn, nil, :DAYS_IN_WEEK, s(:int, 7))
      )
    end
  end

  context 'with an if statement' do
    context 'without an else clause' do
      should_parse(
        from: '(if (valid? user) (save user))',
        to: s(:if,
          s(:send, s(:lvar, :user), :valid?),
          s(:send, s(:lvar, :user), :save),
          nil
        )
      )
    end

    context 'with an else clause' do
      should_parse(
        from: '(if (> a b) a b)',
        to: s(:if,
          s(:send, s(:lvar, :a), :>, s(:lvar, :b)),
          s(:lvar, :a),
          s(:lvar, :b)
        )
      )
    end
  end

  context 'with an unless statement' do
    should_parse(
      from: '(unless (persisted? user) (save user))',
      to: s(:if,
        s(:send, s(:lvar, :user), :persisted?),
        nil,
        s(:send, s(:lvar, :user), :save)
      )
    )
  end

  context 'with a case statement' do
    context 'without an else clause' do
      should_parse(
        from: '(case x 1 "one" 2 "two")',
        to: s(:case,
          s(:lvar, :x),
          s(:when, s(:int, 1), s(:str, 'one')),
          s(:when, s(:int, 2), s(:str, 'two')),
          nil
        )
      )
    end

    context 'with an else clause' do
      should_parse(
        from: (<<-CRB),
(case lang
  "clojure" "great!"
  "ruby" "cool"
  "crap")
      CRB
        to: s(:case,
          s(:lvar, :lang),
          s(:when,
            s(:str, 'clojure'),
            s(:str, 'great!')
          ),
          s(:when,
            s(:str, 'ruby'),
            s(:str, 'cool')
          ),
          s(:str, 'crap')
        )
      )
    end
  end

  context 'with a while loop' do
    should_parse(
      from: '(while (< x 5) (def x (+ x 1)))',
      to: s(:while,
        s(:send, s(:lvar, :x), :<, s(:int, 5)),
        s(:lvasgn, :x, s(:send, s(:lvar, :x), :+, s(:int, 1)))
      )
    )
  end

  context 'with an until loop' do
    should_parse(
      from: '(until (>= x 5) (def x (+ x 1)))',
      to: s(:until,
        s(:send, s(:lvar, :x), :>=, s(:int, 5)),
        s(:lvasgn, :x, s(:send, s(:lvar, :x), :+, s(:int, 1)))
      )
    )
  end

  context 'with a try statement' do
    context 'with a rescue clause' do
      should_parse(
        from: '(try (File/read path) (rescue Errno.ENOENT e (@puts (message e)) (@raise)))',
        to: s(:kwbegin,
          s(:rescue,
            s(:send, s(:const, nil, :File), :read, s(:lvar, :path)),
            s(:resbody,
              s(:array, s(:const, s(:const, nil, :Errno), :ENOENT)),
              s(:lvasgn, :e),
              s(:begin,
                s(:send, nil, :puts, s(:send, s(:lvar, :e), :message)),
                s(:send, nil, :raise)
              )
            ),
            nil
          )
        )
      )
    end

    context 'with multiple rescue clauses' do
      should_parse(
        from: <<-CRB,
(try (Faraday/get path)
     (rescue Faraday.ConnectionFailed e (@puts "Connection failed"))
     (rescue Timeout.Error e (@puts "Request timed out")))
        CRB
        to: s(:kwbegin,
          s(:rescue,
            s(:send, s(:const, nil, :Faraday), :get, s(:lvar, :path)),
            s(:resbody,
              s(:array, s(:const, s(:const, nil, :Faraday), :ConnectionFailed)),
              s(:lvasgn, :e),
              s(:send, nil, :puts, s(:str, 'Connection failed'))
            ),
            s(:resbody,
              s(:array, s(:const, s(:const, nil, :Timeout), :Error)),
              s(:lvasgn, :e),
              s(:send, nil, :puts, s(:str, 'Request timed out'))
            ),
            nil
          )
        )
      )
    end

    context 'with an ensure clause' do
      should_parse(
        from: '(try (File/read path) (ensure (@puts "Tried to read a file.")))',
        to: s(:kwbegin,
          s(:ensure,
            s(:send, s(:const, nil, :File), :read, s(:lvar, :path)),
            s(:send, nil, :puts, s(:str, 'Tried to read a file.'))
          )
        )
      )
    end

    context 'with rescue and ensure clauses' do
      should_parse(
        from: <<-CRB,
(try (File/read path)
     (rescue Errno.ENOENT e (@puts (message e)))
     (ensure (@puts "Tried to read a file.")))
        CRB
        to: s(:kwbegin,
          s(:ensure,
            s(:rescue,
              s(:send, s(:const, nil, :File), :read, s(:lvar, :path)),
              s(:resbody,
                s(:array, s(:const, s(:const, nil, :Errno), :ENOENT)),
                s(:lvasgn, :e),
                s(:send, nil, :puts, s(:send, s(:lvar, :e), :message))
              ),
              nil
            ),
            s(:send, nil, :puts, s(:str, 'Tried to read a file.'))
          )
        )
      )
    end
  end

  context 'with a method call' do
    context 'of an instance method' do
      should_parse(
        from: '(name user)',
        to: s(:send, s(:lvar, :user), :name)
      )

      context 'ending with a special char' do
        should_parse(
          from: '(include? [1 2 3] 4)',
          to: s(:send,
            s(:array,
              s(:int, 1),
              s(:int, 2),
              s(:int, 3)
            ),
            :include?,
            s(:int, 4)
          )
        )
      end

      context 'with a splat argument' do
        should_parse(
          from: '(add-tags article & tags)',
          to: s(:send, s(:lvar, :article), :add_tags, s(:splat, s(:lvar, :tags)))
        )

        should_parse(
          from: '(sum calculator a b c & others)',
          to: s(:send,
            s(:lvar, :calculator),
            :sum,
            s(:lvar, :a),
            s(:lvar, :b),
            s(:lvar, :c),
            s(:splat, s(:lvar, :others))
          )
        )
      end

      context 'with a form passed as a block' do
        should_parse(
          from: '(map users # :name)',
          to: s(:send,
            s(:lvar, :users),
            :map,
            s(:block_pass, s(:sym, :name))
          )
        )

        should_parse(
          from: '(each users # some-proc)',
          to: s(:send,
            s(:lvar, :users),
            :each,
            s(:block_pass, s(:lvar, :some_proc))
          )
        )
      end

      context 'with a block' do
        should_parse(
          from: '(map users #([user] (upcase (name user))))',
          to: s(:block,
            s(:send, s(:lvar, :users), :map),
            s(:args, s(:arg, :user)),
            s(:send, s(:send, s(:lvar, :user), :name), :upcase)
          )
        )

        should_parse(
          from: '(each-slice users 3 #([user] (@p user)))',
          to: s(:block,
            s(:send, s(:lvar, :users), :each_slice, s(:int, 3)),
            s(:args, s(:arg, :user)),
            s(:send, nil, :p, s(:lvar, :user))
          )
        )

        should_parse(
          from: '(each-slice users 3 #([user] (@p (name user)) (@p (email user))))',
          to: s(:block,
            s(:send, s(:lvar, :users), :each_slice, s(:int, 3)),
            s(:args, s(:arg, :user)),
            s(:begin,
              s(:send, nil, :p, s(:send, s(:lvar, :user), :name)),
              s(:send, nil, :p, s(:send, s(:lvar, :user), :email))
            )
          )
        )

        should_parse(
          from: '(times 5 #(@puts "Hello"))',
          to: s(:block,
            s(:send, s(:int, 5), :times),
            s(:args),
            s(:send, nil, :puts, s(:str, 'Hello'))
          )
        )

        should_parse(
          from: '(times 5 #((@puts "Hello") (@puts "Goodbye")))',
          to: s(:block,
            s(:send, s(:int, 5), :times),
            s(:args),
            s(:begin,
              s(:send, nil, :puts, s(:str, 'Hello')),
              s(:send, nil, :puts, s(:str, 'Goodbye'))
            )
          )
        )
      end

      context 'with two blocks' do
        it 'raises a FormatError' do
          expect {
            subject.parse('(map users #([user] (name user)) #([user] (email user)))')
          }.to raise_error(Carbonate::Parser::FormatError)
        end
      end

      context 'with block in non-last position' do
        it 'raises a FormatError' do
          expect {
            subject.parse('(map users #([user] (name user)) 5)')
          }.to raise_error(Carbonate::Parser::FormatError)
        end
      end
    end

    context 'of a class method' do
      should_parse(
        from: '(User/find-by {:first-name "John"})',
        to: s(:send,
          s(:const, nil, :User),
          :find_by,
          s(:hash,
            s(:pair,
              s(:sym, :first_name),
              s(:str, 'John')
            )
          )
        )
      )

      should_parse(
        from: '(User/count)',
        to: s(:send, s(:const, nil, :User), :count)
      )

      context 'ending with a special char' do
        should_parse(
          from: '(User/exists? {:name "Jack"})',
          to: s(:send,
            s(:const, nil, :User),
            :exists?,
            s(:hash,
              s(:pair,
                s(:sym, :name),
                s(:str, 'Jack')
              )
            )
          )
        )
      end

      context 'with a splat argument' do
        should_parse(
          from: '(Calculator/sum & numbers)',
          to: s(:send, s(:const, nil, :Calculator), :sum, s(:splat, s(:lvar, :numbers)))
        )

        should_parse(
          from: '(Calculator/sum a b c & others)',
          to: s(:send,
            s(:const, nil, :Calculator),
            :sum,
            s(:lvar, :a),
            s(:lvar, :b),
            s(:lvar, :c),
            s(:splat, s(:lvar, :others))
          )
        )
      end

      context 'with a block' do
        should_parse(
          from: '(User/find-each #([user] (@p (name user))))',
          to: s(:block,
            s(:send, s(:const, nil, :User), :find_each),
            s(:args, s(:arg, :user)),
            s(:send, nil, :p, s(:send, s(:lvar, :user), :name))
          )
        )
      end
    end

    context 'without an explicit receiver' do
      should_parse(
        from: '(@attr-reader :first-name)',
        to: s(:send,
          nil,
          :attr_reader,
          s(:sym, :first_name)
        )
      )

      should_parse(
        from: '(def count (@get-count))',
        to: s(:lvasgn, :count, s(:send, nil, :get_count))
      )

      context 'ending with a special char' do
        should_parse(
          from: '(@valid?)',
          to: s(:send,
            nil,
            :valid?
          )
        )
      end

      context 'with a splat argument' do
        should_parse(
          from: '(@sum & numbers)',
          to: s(:send, nil, :sum, s(:splat, s(:lvar, :numbers)))
        )

        should_parse(
          from: '(@sum a b c & others)',
          to: s(:send,
            nil,
            :sum,
            s(:lvar, :a),
            s(:lvar, :b),
            s(:lvar, :c),
            s(:splat, s(:lvar, :others))
          )
        )
      end

      context 'with a block' do
        should_parse(
          from: '(@each #([element] (@p element)))',
          to: s(:block,
            s(:send, nil, :each),
            s(:args, s(:arg, :element)),
            s(:send, nil, :p, s(:lvar, :element))
          )
        )
      end
    end

    context 'of a class constructor' do
      should_parse(from: '(User.)', to: s(:send, s(:const, nil, :User), :new))

      should_parse(
        from: '(User. {:name "John"})',
        to: s(:send,
          s(:const, nil, :User),
          :new,
          s(:hash,
            s(:pair,
              s(:sym, :name),
              s(:str, 'John')
            )
          )
        )
      )

      context 'with a block' do
        should_parse(
          from: '(User. #([user] (def user.name "John")))',
          to: s(:block,
            s(:send, s(:const, nil, :User), :new),
            s(:args, s(:arg, :user)),
            s(:send, s(:lvar, :user), :name=, s(:str, 'John'))
          )
        )
      end
    end
  end

  context 'with a super call' do
    context 'with explicit parameters' do
      should_parse(from: '(super)', to: s(:super))
      should_parse(from: '(super "parameter")', to: s(:super, s(:str, 'parameter')))

      context 'with a block' do
        should_parse(
          from: '(super 123 #([object] (@puts object)))',
          to: s(:block,
            s(:super, s(:int, 123)),
            s(:args, s(:arg, :object)),
            s(:send, nil, :puts, s(:lvar, :object))
          )
        )
      end
    end

    context 'with implicit parameters' do
      should_parse(from: '(zsuper)', to: s(:zsuper))

      context 'with a block' do
        should_parse(
          from: '(zsuper #([object] (@puts object)))',
          to: s(:block,
            s(:zsuper),
            s(:args, s(:arg, :object)),
            s(:send, nil, :puts, s(:lvar, :object))
          )
        )
      end
    end
  end

  context 'with a method definition' do
    should_parse(
      from: (<<-CRB),
(defmethod full-name []
  (join [first-name last-name]))
      CRB
      to: s(:def,
        :full_name,
        s(:args),
        s(:send,
          s(:array,
            s(:lvar, :first_name),
            s(:lvar, :last_name)
          ),
          :join
        )
      )
    )

    should_parse(
      from: '(defmethod unnamed? [] (nil? @name))',
      to: s(:def,
        :unnamed?,
        s(:args),
        s(:send,
          s(:ivar, :@name),
          :nil?
        )
      )
    )

    should_parse(
      from: '(defmethod sum [& numbers] (reduce numbers :+))',
      to: s(:def,
        :sum,
        s(:args, s(:restarg, :numbers)),
        s(:send, s(:lvar, :numbers), :reduce, s(:sym, :+))
      )
    )

    should_parse(
      from: '(defmethod int-to-string [int [base 10]] (to-s int base))',
      to: s(:def,
        :int_to_string,
        s(:args,
          s(:arg, :int),
          s(:optarg, :base, s(:int, 10)),
        ),
        s(:send,
          s(:lvar, :int),
          :to_s,
          s(:lvar, :base)
        )
      )
    )

    should_parse(
      from: '(defmethod output [a b c & d] [a b c d])',
      to: s(:def,
        :output,
        s(:args, s(:arg, :a), s(:arg, :b), s(:arg, :c), s(:restarg, :d)),
        s(:array, s(:lvar, :a), s(:lvar, :b), s(:lvar, :c), s(:lvar, :d))
      )
    )

    should_parse(
      from: (<<-CRB),
(defmethod each [# block]
  (@yield 1)
  (@yield 2)
  (@yield 3))
      CRB
      to: s(:def,
        :each,
        s(:args, s(:blockarg, :block)),
        s(:begin,
          s(:send, nil, :yield, s(:int, 1)),
          s(:send, nil, :yield, s(:int, 2)),
          s(:send, nil, :yield, s(:int, 3))
        )
      )
    )

    context 'with a rescue clause' do
      should_parse(
        from: '(defmethod read-file [path] (File/read path) (rescue Errno.ENOENT e (@puts "No file found.")))',
        to: s(:def,
          :read_file,
          s(:args, s(:arg, :path)),
          s(:rescue,
            s(:send, s(:const, nil, :File), :read, s(:lvar, :path)),
            s(:resbody,
              s(:array,
                s(:const, s(:const, nil, :Errno), :ENOENT)
              ),
              s(:lvasgn, :e),
              s(:send, nil, :puts, s(:str, 'No file found.'))
            ),
            nil
          )
        )
      )
    end

    context 'with an ensure clause' do
      should_parse(
        from: '(defmethod read-file [path] (File/read path) (ensure (@puts "Tried to read a file.")))',
        to: s(:def,
          :read_file,
          s(:args, s(:arg, :path)),
          s(:ensure,
            s(:send, s(:const, nil, :File), :read, s(:lvar, :path)),
            s(:send, nil, :puts, s(:str, 'Tried to read a file.'))
          )
        )
      )
    end

    context 'with rescue and ensure clauses' do
      should_parse(
        from: <<-CRB,
(defmethod read-file [path]
  (File/read path)
  (rescue Errno.ENOENT e (@puts "No file found."))
  (ensure (@puts "Tried to read a file.")))
      CRB
        to: s(:def,
          :read_file,
          s(:args, s(:arg, :path)),
          s(:ensure,
            s(:rescue,
              s(:send, s(:const, nil, :File), :read, s(:lvar, :path)),
              s(:resbody,
                s(:array,
                  s(:const, s(:const, nil, :Errno), :ENOENT)
                ),
                s(:lvasgn, :e),
                s(:send, nil, :puts, s(:str, 'No file found.'))
              ),
              nil
            ),
            s(:send, nil, :puts, s(:str, 'Tried to read a file.'))
          )
        )
      )
    end
  end

  context 'with a lambda definition' do
    should_parse(
      from: '(def my-lambda (-> [user] (email user)))',
      to: s(:lvasgn,
        :my_lambda,
        s(:block,
          s(:send, nil, :lambda),
          s(:args, s(:arg, :user)),
          s(:send, s(:lvar, :user), :email)
        )
      )
    )

    should_parse(
      from: '(-> [user] (@p user) (save user))',
      to: s(:block,
        s(:send, nil, :lambda),
        s(:args, s(:arg, :user)),
        s(:begin,
          s(:send, nil, :p, s(:lvar, :user)),
          s(:send, s(:lvar, :user), :save)
        )
      )
    )

    should_parse(
      from: '(-> (@puts "Hello world!"))',
      to: s(:block,
        s(:send, nil, :lambda),
        s(:args),
        s(:send, nil, :puts, s(:str, 'Hello world!'))
      )
    )
  end

  context 'with a return statement' do
    context 'without arguments' do
      should_parse(
        from: '(defmethod nothing [] (return))',
        to: s(:def, :nothing, s(:args), s(:return))
      )
    end

    context 'with arguments' do
      should_parse(
        from: '(defmethod name [] (return "John"))',
        to: s(:def,
          :name,
          s(:args),
          s(:return,
            s(:str, 'John')
          )
        )
      )
    end
  end

  context 'with a class definition' do
    context 'without a parent class' do
      should_parse(
        from: (<<-CRB),
(defclass User
  (defmethod initialize [first-name last-name]
    (def @first-name first-name)
    (def @last-name last-name))
  (defmethod full-name []
    (join [@first-name @last-name])))
        CRB
        to: s(:class,
          s(:const, nil, :User),
          nil,
          s(:begin,
            s(:def,
              :initialize,
              s(:args,
                s(:arg, :first_name),
                s(:arg, :last_name)
              ),
              s(:begin,
                s(:ivasgn, :@first_name, s(:lvar, :first_name)),
                s(:ivasgn, :@last_name, s(:lvar, :last_name))
              )
            ),
            s(:def,
              :full_name,
              s(:args),
              s(:send,
                s(:array,
                  s(:ivar, :@first_name),
                  s(:ivar, :@last_name),
                ),
                :join
              )
            )
          )
        )
      )
    end

    context 'with a parent class' do
      should_parse(
        from: (<<-CRB),
(defclass User < Base
  (@include Naming))
        CRB
        to: s(:class,
          s(:const, nil, :User),
          s(:const, nil, :Base),
          s(:send, nil, :include, s(:const, nil, :Naming))
        )
      )
    end
  end

  context 'with a module definition' do
    should_parse(
      from: (<<-CRB),
(defmodule Naming
  (@attr-reader :first-name :last-name)
  (defmethod full-name []
    (join [first-name last-name])))
      CRB
      to: s(:module,
        s(:const, nil, :Naming),
        s(:begin,
          s(:send,
            nil,
            :attr_reader,
            s(:sym, :first_name),
            s(:sym, :last_name)
          ),
          s(:def,
            :full_name,
            s(:args),
            s(:send,
              s(:array,
                s(:lvar, :first_name),
                s(:lvar, :last_name)
              ),
              :join
            )
          )
        )
      )
    )
  end

  context 'with a singleton class definition' do
    should_parse(
      from: (<<-CRB),
(<<- user
    (defmethod name []
      @name))
      CRB
      to: s(:sclass,
        s(:lvar, :user),
        s(:def,
          :name,
          s(:args),
          s(:ivar, :@name)
        )
      )
    )
  end

  context 'with a tokenizing error' do
    it 'raises a FormatError' do
      expect {
        subject.parse('(@puts a;)')
      }.to raise_error("Unknown character ';' at line 1")
    end
  end

  context 'with a parsing error' do
    it 'raises a FormatError' do
      expected_message = "Unexpected token 'w' at line 1"

      expect {
        subject.parse('(- 1 2w)')
      }.to raise_error(Carbonate::Parser::FormatError, expected_message)
    end

    context 'at the end of the input' do
      it 'raises a FormatError' do
        expect {
          subject.parse('(save user')
        }.to raise_error(Carbonate::Parser::FormatError, 'Input ends unexpectedly')
      end
    end
  end
end
