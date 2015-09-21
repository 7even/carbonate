require 'spec_helper'

RSpec.describe Carbonate::Parser do
  def s(type, *children)
    Parser::AST::Node.new(type, children)
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
