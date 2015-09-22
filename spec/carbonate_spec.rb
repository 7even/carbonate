require 'spec_helper'

describe Carbonate do
  it 'has a version number' do
    expect(Carbonate::VERSION).not_to be nil
  end

  describe '.process' do
    let(:source) do
      <<-CRB
(defclass User
  (defmethod initialize [first-name last-name age]
    (def @first-name first-name)
    (def @last-name last-name)
    (def @age age))
  (defmethod full-name []
    (join [@first-name @last-name]))
  (defmethod age []
    (to-i @age)))
      CRB
    end

    let(:result) do
      <<-RB
class User
  def initialize(first_name, last_name, age)
    @first_name = first_name
    @last_name = last_name
    @age = age
  end
  def full_name
    [@first_name, @last_name].join
  end
  def age
    @age.to_i
  end
end
      RB
    end

    it 'takes a carbonate source and returns a ruby source' do
      expect(subject.process(source)).to eq(result)
    end
  end
end
