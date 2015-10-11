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

  describe '.require' do
    context 'with an existing file' do
      before(:each) do
        @path = Pathname.pwd.join('fixtures/user.crb')
        @path.dirname.mkpath
        @path.write('(defclass User (defmethod name "John"))' + ?\n)
      end

      it 'searches for the file in current working directory' do
        Carbonate.require './fixtures/user'

        expect(User).to be_a(Class)
        expect(User.instance_methods).to include(:name)
      end

      it 'adds the loaded file path to $LOADED_FEATURES' do
        Carbonate.require './fixtures/user'

        absolute_path = Pathname.pwd + 'fixtures/user.crb'
        expect($LOADED_FEATURES).to include(absolute_path.to_s)
      end

      it 'returns true' do
        expect(Carbonate.require('./fixtures/user')).to eq(true)
      end

      context 'with an absolute path' do
        it 'loads the file' do
          Carbonate.require @path

          expect(User).to be_a(Class)
          expect(User.instance_methods).to include(:name)
        end
      end

      context 'located in a directory from $LOAD_PATH' do
        before(:each) do
          $LOAD_PATH.unshift(@path.dirname.to_s)
        end

        it 'loads the file' do
          Carbonate.require @path.basename

          expect(User).to be_a(Class)
          expect(User.instance_methods).to include(:name)
        end

        after(:each) do
          $LOAD_PATH.delete(@path.dirname.to_s)
        end
      end

      context 'when a file is already loaded' do
        before(:each) do
          Carbonate.require './fixtures/user'
          @path.write('(defclass Post (defmethod title @title))')
        end

        it "won't load it again" do
          expect {
            Carbonate.require './fixtures/user'
          }.not_to change { $LOADED_FEATURES }

          expect(Object.const_defined?(:Post)).to be_falsy
        end

        it 'returns false' do
          expect(Carbonate.require('./fixtures/user')).to eq(false)
        end
      end

      context 'when a file has syntax errors' do
        before(:each) do
          @path.write('(def age 31')
        end

        it 'does not add the file path to $LOADED_FEATURES' do
          expect {
            begin
              Carbonate.require './fixtures/user'
            rescue Carbonate::Parser::FormatError
            end
          }.not_to change { $LOADED_FEATURES }
        end
      end

      after(:each) do
        @path.delete
        @path.dirname.delete
        $LOADED_FEATURES.delete(@path.to_s)
      end
    end

    context 'with a wrong path' do
      it 'raises a LoadError' do
        expect {
          Carbonate.require './abc'
        }.to raise_error(LoadError, 'cannot load such file -- ./abc')
      end

      it 'does not add the file path to $LOADED_FEATURES' do
        expect {
          begin
            Carbonate.require './abc'
          rescue LoadError
          end
        }.not_to change { $LOADED_FEATURES }
      end
    end
  end
end
