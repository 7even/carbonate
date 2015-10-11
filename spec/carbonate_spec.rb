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

    context 'with tokenizing errors' do
      let(:source) { '(def a 1;)' }

      before(:each) do
        allow(STDERR).to receive(:puts)
        allow(Carbonate).to receive(:exit)
      end

      it 'writes the error message to STDERR' do
        expect(STDERR).to receive(:puts).with("Unknown character ';' at line 1")

        subject.process(source)
      end

      it 'exits with a status of 1' do
        expect(Carbonate).to receive(:exit).with(1)

        subject.process(source)
      end
    end

    context 'with parsing errors' do
      let(:source) { '(def a 1' }

      before(:each) do
        allow(STDERR).to receive(:puts)
        allow(Carbonate).to receive(:exit)
      end

      it 'writes the error message to STDERR' do
        expect(STDERR).to receive(:puts).with('Input ends unexpectedly')

        subject.process(source)
      end

      it 'exits with a status of 1' do
        expect(Carbonate).to receive(:exit).with(1)

        subject.process(source)
      end
    end
  end
end
