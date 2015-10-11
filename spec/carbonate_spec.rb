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

  describe '.require' do
    before(:each) do
      @path = Pathname.pwd.join('fixtures/user.crb')
      @path.dirname.mkpath
      @path.write('(defclass User (defmethod name "John"))' + ?\n)
    end

    context 'with a filename starting with a dot' do
      it 'searches for the file in current working directory' do
        Carbonate.require './fixtures/user'

        expect(User).to be_a(Class)
        expect(User.instance_methods).to include(:name)
      end

      context 'with a wrong path' do
        it 'raises a LoadError' do
          expect {
            Carbonate.require './abc'
          }.to raise_error(LoadError, 'cannot load such file -- ./abc')
        end
      end
    end

    after(:each) do
      @path.delete
      @path.dirname.delete
    end
  end
end
