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
      absolute_path = if filename.start_with?('.')
        Pathname.pwd + "#{filename}.crb"
      else
        fail NotImplementedError
      end
      return false if $LOADED_FEATURES.include?(absolute_path.to_s)

      unless absolute_path.exist?
        fail LoadError, "cannot load such file -- #{filename}"
      end

      $LOADED_FEATURES.push(absolute_path.to_s)

      ruby_code = process(absolute_path.read)
      Object.class_eval(ruby_code)
    end

  private
    def with_trailing_newline(text)
      if text.end_with?(?\n)
        text
      else
        text + ?\n
      end
    end
  end
end
