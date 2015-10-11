require 'carbonate/parser'
require 'unparser'
require 'carbonate/version'

module Carbonate
  class << self
    def process(source)
      ast = Parser.new.parse(source)
      with_trailing_newline(Unparser.unparse(ast))
    end

    def require(filename)
      absolute_path = find_required_file(filename)
      fail LoadError, "cannot load such file -- #{filename}" if absolute_path.nil?
      return false if $LOADED_FEATURES.include?(absolute_path.to_s)

      ruby_code = process(absolute_path.read)
      Object.class_eval(ruby_code)

      $LOADED_FEATURES.push(absolute_path.to_s)
      true
    end

    def require_relative(relative_path)
      dir = Pathname.new(caller_locations(1, 1).first.path).dirname
      absolute_path = dir + relative_path

      require(absolute_path.to_s)
    end

  private
    def with_trailing_newline(text)
      if text.end_with?(?\n)
        text
      else
        text + ?\n
      end
    end

    def find_required_file(filename)
      ([Dir.pwd] + $LOAD_PATH).each do |path|
        absolute_path = Pathname.new(path).join(append_extension(filename.to_s))
        return absolute_path if absolute_path.exist?
      end

      nil
    end

    def append_extension(filename)
      if filename.end_with?('.crb')
        filename
      else
        "#{filename}.crb"
      end
    end
  end
end
