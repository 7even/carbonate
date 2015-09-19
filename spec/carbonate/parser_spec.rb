require 'spec_helper'

RSpec.describe Carbonate::Parser do
  def s(type, *children)
    Parser::AST::Node.new(type, children)
  end

  let(:source) { '(+ 2 (* 3 4))' }

  it 'parses the source into AST' do
    expect(subject.parse(source)).to eq(
      s(
        :send,
        s(:int, 2),
        :+,
        s(
          :send,
          s(:int, 3),
          :*,
          s(:int, 4)
        )
      )
    )
  end
end
