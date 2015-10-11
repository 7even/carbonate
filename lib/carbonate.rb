require 'carbonate/parser'
require 'unparser'
require 'carbonate/version'

module Carbonate
  class << self
    def process(source)
      ast = Parser.new.parse(source)
      with_trailing_newline(Unparser.unparse(ast))
    rescue Parser::FormatError => e
      STDERR.puts e.message
      exit 1
    end

    def require(filename)
      absolute_path = if filename.start_with?('.')
        Pathname.pwd + "#{filename}.crb"
      else
        fail NotImplementedError
      end

      unless absolute_path.exist?
        fail LoadError, "cannot load such file -- #{filename}"
      end

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
