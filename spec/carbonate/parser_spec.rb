require 'spec_helper'

RSpec.describe Carbonate::Parser do
  def s(type, *children)
    Parser::AST::Node.new(type, children)
  end

  context 'with numbers' do
    context 'integer' do
      let(:source) { '1' }

      it 'parses the source into AST' do
        expect(subject.parse(source)).to eq(s(:int, 1))
      end
    end

    context 'float' do
      let(:source) { '3.14' }

      it 'parses the source into AST' do
        expect(subject.parse(source)).to eq(s(:float, 3.14))
      end
    end
  end

  context 'with strings' do
    context 'without escaped characters' do
      let(:source) { '"some string"' }

      it 'parses the source into AST' do
        expect(subject.parse(source)).to eq(s(:str, 'some string'))
      end
    end

    context 'with escaped characters' do
      let(:source) { '"line1\nline2\rline3\tline4 \'single-quoted text\' \"double-quoted text\""' }

      it 'parses the source into AST' do
        expect(subject.parse(source)).to eq(
          s(:str,
            "line1\nline2\rline3\tline4 'single-quoted text' \"double-quoted text\""
          )
        )
      end
    end
  end

  context 'with symbols' do
    let(:source) { ':some-symbol' }

    it 'parses the source into AST' do
      expect(subject.parse(source)).to eq(s(:sym, :some_symbol))
    end
  end

  context 'with regexps' do
    let(:source) { '#"[A-Za-z]+"' }

    it 'parses the source into AST' do
      expect(subject.parse(source)).to eq(
        s(:regexp,
          s(:str, '[A-Za-z]+'),
          s(:regopt)
        )
      )
    end
  end

  context 'with arrays' do
    let(:source) { '[1 2 3]' }

    it 'parses the source into AST' do
      expect(subject.parse(source)).to eq(
        s(:array,
          s(:int, 1),
          s(:int, 2),
          s(:int, 3)
        )
      )
    end
  end

  context 'with hashes' do
    let(:source) { '{:first-name "Rich", :last-name "Hickey"}' }

    it 'parses the source into AST' do
      expect(subject.parse(source)).to eq(
        s(:hash,
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
    end

    context 'with odd number of elements' do
      let(:source) { '{:key1 :value1 :key2}' }

      it 'raises a FormatError' do
        expect {
          subject.parse(source)
        }.to raise_error(Carbonate::Parser::FormatError)
      end
    end
  end

  context 'with sets' do
    let(:source) { '#{123 "string" :symbol}' }

    it 'parses the source into AST' do
      expect(subject.parse(source)).to eq(
        s(:send,
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
  end

  context 'with basic arithmetic' do
    let(:basic)  { '(+ 2 2)' }
    let(:nested) { '(+ 2 (* 3 4))' }

    it 'parses the source into AST' do
      expect(subject.parse(basic)).to eq(
        s(:send,
          s(:int, 2),
          :+,
          s(:int, 2)
        )
      )

      expect(subject.parse(nested)).to eq(
        s(:send,
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
  end

  context 'with a variable assignment' do
    context 'of a local variable' do
      let(:source) { '(def username "7even")' }

      it 'parses the source into AST' do
        expect(subject.parse(source)).to eq(s(:lvasgn, :username, s(:str, '7even')))
      end
    end

    context 'of an instance variable' do
      let(:source) { '(def @first-name "John")' }

      it 'parses the source into AST' do
        expect(subject.parse(source)).to eq(s(:ivasgn, :@first_name, s(:str, 'John')))
      end
    end
  end

  context 'with a method call' do
    context 'of an instance method' do
      let(:source) { '(name user)' }

      it 'parses the source into AST' do
        expect(subject.parse(source)).to eq(s(:send, s(:lvar, :user), :name))
      end
    end

    context 'of a class method' do
      let(:source) { '(User/find-by {:first-name "John"})' }

      it 'parses the source into AST' do
        expect(subject.parse(source)).to eq(
          s(:send,
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
      end
    end

    context 'without an explicit receiver' do
      let(:source) { '(@attr-reader :first-name)' }

      it 'parses the source into AST' do
        expect(subject.parse(source)).to eq(
          s(:send,
            nil,
            :attr_reader,
            s(:sym, :first_name)
          )
        )
      end
    end
  end

  context 'with a method definition' do
    let(:source) do
      <<-CRB
(defmethod full-name []
  (join [first-name last-name]))
      CRB
    end

    it 'parses the source into AST' do
      expect(subject.parse(source)).to eq(
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
    end
  end

  context 'with a class definition' do
    let(:source) do
      <<-CRB
(defclass User
  (defmethod initialize [first-name last-name]
    (def @first-name first-name)
    (def @last-name last-name))
  (defmethod full-name []
    (join [@first-name @last-name])))
      CRB
    end

    it 'parses the source into AST' do
      expect(subject.parse(source)).to eq(
        s(:class,
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
  end

  context 'with a module definition' do
    let(:source) do
      <<-CRB
(defmodule Naming
  (@attr-reader :first-name :last-name)
  (defmethod full-name []
    (join [first-name last-name])))
      CRB
    end

    it 'parses the source into AST' do
      expect(subject.parse(source)).to eq(
        s(:module,
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
  end

  context 'with a singleton class definition' do
    let(:source) do
      <<-CRB
(<< user
    (defmethod name []
      @name))
      CRB
    end

    it 'parses the source into AST' do
      expect(subject.parse(source)).to eq(
        s(:sclass,
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
end
