require 'spec_helper'

RSpec.describe Carbonate::Parser do
  class << self
    def s(type, *children)
      Parser::AST::Node.new(type, children)
    end

    def should_parse(from:, to:, debug: false)
      it 'parses the source into AST', focus: debug do
        expect(subject.parse(from, debug)).to eq(to)
      end
    end
  end

  context 'with numbers' do
    context 'integer' do
      should_parse(
        from: '1',
        to: s(:int, 1)
      )
    end

    context 'float' do
      should_parse(
        from: '3.14',
        to: s(:float, 3.14)
      )
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

  context 'with basic arithmetic' do
    should_parse(
      from: '(+ 2 2)',
      to: s(:send,
        s(:int, 2),
        :+,
        s(:int, 2)
      )
    )

    should_parse(
      from: '(+ 2 (* 3 4))',
      to: s(:send,
        s(:int, 2),
        :+,
        s(:send,
          s(:int, 3),
          :*,
          s(:int, 4)
        )
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
    context 'defined at top level' do
      should_parse(
        from: 'User',
        to: s(:const, nil, :User)
      )
    end

    context 'nested in a namespace' do
      should_parse(
        from: 'Carbonate.Parser',
        to: s(:const, s(:const, nil, :Carbonate), :Parser)
      )
    end
  end

  context 'with a variable assignment' do
    context 'of a local variable' do
      should_parse(
        from: '(def username "7even")',
        to: s(:lvasgn, :username, s(:str, '7even'))
      )
    end

    context 'of an instance variable' do
      should_parse(
        from: '(def @first-name "John")',
        to: s(:ivasgn, :@first_name, s(:str, 'John'))
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
    end

    context 'of a class constructor' do
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
(<< user
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
end
