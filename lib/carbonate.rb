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
