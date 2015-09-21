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
          s(:begin,
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
    end
  end

  context 'with a class definition' do
    let(:source) do
      <<-CRB
(defclass User
  (defmethod initialize [first-name last-name]
    (def @first-name first-name)
    (def @last-name last-name)))
      CRB
    end

    it 'parses the source into AST' do
      expect(subject.parse(source)).to eq(
        s(:class,
          s(:const, nil, :User),
          nil,
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
          )
        )
      )
    end
  end
end
