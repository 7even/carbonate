require 'rly'
require 'ast'
require 'parser/ast/node'

module Carbonate
  class Parser < Rly::Yacc
    def s(type, children)
      ::Parser::AST::Node.new(type, children)
    end

    lexer do
      literals '+-*/()'

      token :NUMBER, /\d+/ do |t|
        t.value = t.value.to_i
        t
      end

      token :WHITESPACE, /\s+/

      on_error do |t|
        puts "Illegal character #{t.value.inspect} at line #{t.lexer.lineno + 1}"
        t.lexer.pos += 1
        nil
      end
    end

    rule 'sexp : "(" func WHITESPACE args ")"' do |sexp, _, func, _, args, _|
      sexp.value = s(:send, [args.value.first, func.value, *args.value[1..-1]])
    end

    rule 'func : "+" | "-" | "*" | "/"' do |func, operator|
      func.value = operator.value.to_sym
    end

    rule 'args : args WHITESPACE arg | arg' do |args, *args_array|
      args.value = args_array.reject do |arg|
        arg.type == :WHITESPACE
      end.map(&:value).flatten
    end

    rule 'arg : NUMBER' do |arg, number|
      arg.value = s(:int, [number.value])
    end

    rule 'arg : sexp' do |arg, sexp|
      arg.value = sexp.value
    end
  end
end
