require 'rly'
require 'ast'
require 'parser/ast/node'

module Carbonate
  class Parser < Rly::Yacc
    def without_spaces(elements)
      elements.reject do |element|
        element.type == :S
      end
    end

    def wrap_in_begin(nodes)
      if nodes.one?
        nodes.first
      else
        s(:begin, nodes)
      end
    end

    def s(type, children)
      self.class.s(type, children)
    end

    def self.s(type, children = [])
      ::Parser::AST::Node.new(type, children)
    end

    def self.handle_string(string, range)
      replacements = {
        '\\n' => "\n",
        '\\r' => "\r",
        '\\t' => "\t",
        '\\"' => '"'
      }
      string[range].gsub(/\\[nrt"]/, replacements)
    end

    lexer do
      literals '+-*/()[]{}#<@.'

      token :FLOAT, /\d+\.\d+/ do |t|
        t.value = Parser.s(:float, [t.value.to_f])
        t
      end

      token :INTEGER, /\d+/ do |t|
        t.value = Parser.s(:int, [t.value.to_i])
        t
      end

      token :SYMBOL, /:[A-Za-z0-9_-]+/ do |t|
        t.value = Parser.s(:sym, [t.value[1..-1].gsub('-', '_').to_sym])
        t
      end

      token :REGEXP, /#"([^"\\]|\\[nrt"])*"/ do |t|
        t.value = Parser.s(:regexp,
          [
            Parser.s(:str, [Parser.handle_string(t.value, 2..-2)]),
            Parser.s(:regopt)
          ]
        )
        t
      end

      token :STRING, /"([^"\\]|\\[nrt"])*"/ do |t|
        t.value = Parser.s(:str, [Parser.handle_string(t.value, 1..-2)])
        t
      end

      # characters treated as whitespace
      token :S, /[\s,]+/

      token :DEFCLASS,  /defclass/
      token :DEFMODULE, /defmodule/
      token :DEFMETHOD, /defmethod/
      token :DEF,       /def/

      token :CONST, /[A-Z][A-Za-z0-9]*/ do |t|
        t.value = Parser.s(:const, [nil, t.value.to_sym])
        t
      end

      token :LVAR, /[a-z][A-Za-z0-9_-]*/ do |t|
        t.value = t.value.gsub('-', '_').to_sym
        t
      end

      token :IVAR, /@[a-z][A-Za-z0-9_-]*/ do |t|
        t.value = t.value.gsub('-', '_').to_sym
        t
      end

      on_error do |t|
        STDERR.puts "Illegal character #{t.value.inspect} at line #{t.lexer.lineno + 1}"
        t.lexer.pos += 1
        nil
      end
    end

    # outermost scope
    rule 'source : forms S | forms' do |source, forms|
      source.value = wrap_in_begin(forms.value)
    end

    # multiple forms
    rule 'forms : forms S form | form' do |forms, *forms_array|
      forms.value = without_spaces(forms_array).flat_map(&:value)
    end

    # form can be a local variable
    # user
    rule 'form : LVAR' do |form, lvar|
      form.value = s(:lvar, [lvar.value])
    end

    # form can be an instance variable
    # @user
    rule 'form : IVAR' do |form, ivar|
      form.value = s(:ivar, [ivar.value])
    end

    # form can also be an S-expression, a literal value, self or a collection of forms
    rule 'form : INTEGER | FLOAT | STRING | SYMBOL | REGEXP | array | hash | set | self | sexp' do |form, element|
      form.value = element.value
    end

    # array
    # [1 2 3]
    rule 'array : "[" forms "]"' do |array, _, sexps, _|
      array.value = s(:array, sexps.value)
    end

    # hash
    # {:first-name "Rich", :last-name "Hickey"}
    # commas are optional, elements count must be even
    rule 'hash : "{" forms "}"' do |hash, _, forms, _|
      raise FormatError.new('Odd number of elements in a hash') if forms.value.count.odd?

      pairs = forms.value.each_slice(2).map { |pair| s(:pair, pair) }
      hash.value = s(:hash, pairs)
    end

    # set
    # #{123 "string" :symbol}
    rule 'set : "#" "{" forms "}"' do |set, _, _, forms, _|
      set.value = s(:send,
        [
          s(:const, [nil, :Set]),
          :new,
          s(:array, forms.value)
        ]
      )
    end

    # self
    # @
    rule 'self : "@"' do |self_node, _|
      self_node.value = s(:self, [])
    end

    # multiple S-expressions
    rule 'sexps : sexps S sexp | sexp' do |sexps, *sexps_array|
      sexps.value = without_spaces(sexps_array).flat_map(&:value)
    end

    # instance method call with an explicit receiver
    # (+ 2 2)
    rule 'sexp : "(" func S forms ")"' do |sexp, _, func, _, forms, _|
      sexp.value = s(:send, [forms.value.first, func.value, *forms.value[1..-1]])
    end

    # class method call with an explicit receiver
    # (User/find-by {:first-name "John"})
    rule 'sexp : "(" CONST "/" func S forms ")"' do |sexp, _, const, _, func, _, forms, _|
      sexp.value = s(:send, [const.value, func.value, *forms.value])
    end

    # method call with an implicit receiver
    # (@attr-reader :first-name)
    rule 'sexp : "(" IVAR S forms ")"' do |sexp, _, ivar, _, forms, _|
      sexp.value = s(:send, [nil, ivar.value[1..-1].to_sym, *forms.value])
    end

    # class constructor call
    # (User. {:name "John"})
    rule 'sexp : "(" CONST "." S forms ")"' do |sexp, _, const, _, _, forms|
      sexp.value = s(:send, [const.value, :new, *forms.value])
    end

    # class definition
    # (defclass User (class body))
    rule 'sexp : "(" DEFCLASS S CONST S forms ")"' do |sexp, _, _, _, const, _, forms, _|
      class_body = wrap_in_begin(forms.value)
      sexp.value = s(:class, [const.value, nil, class_body])
    end

    # module definition
    # (defmodule Enumerable (module body ...))
    rule 'sexp : "(" DEFMODULE S CONST S forms ")"' do |sexp, _, _, _, const, _, forms, _|
      module_body = wrap_in_begin(forms.value)
      sexp.value = s(:module, [const.value, module_body])
    end

    # singleton class definition
    # (<< user (defmethod name [] @name)
    rule 'sexp : "(" "<" "<" S form S forms ")"' do |sexp, _, _, _, _, form, _, forms, _|
      singleton_body = wrap_in_begin(forms.value)
      sexp.value = s(:sclass, [form.value, singleton_body])
    end

    # method defition
    # (defmethod full-name (join [first-name last-name]))
    rule 'sexp : "(" DEFMETHOD S LVAR S arguments_list S forms ")"' do |sexp, _, _, _, method_name, _, args, _, forms, _|
      method_body = wrap_in_begin(forms.value)
      sexp.value = s(:def, [method_name.value.to_sym, args.value, method_body])
    end

    # local variable assignment
    # (def username "7even")
    rule 'sexp : "(" DEF S LVAR S form ")"' do |sexp, _, _, _, var_name, _, form, _|
      sexp.value = s(:lvasgn, [var_name.value, form.value])
    end

    # instance variable assignment
    # (def @age 30)
    rule 'sexp : "(" DEF S IVAR S form ")"' do |sexp, _, _, _, var_name, _, form, _|
      sexp.value = s(:ivasgn, [var_name.value, form.value])
    end

    # object attribute assignment
    # (def user.name "John")
    rule 'sexp : "(" DEF S form "." LVAR S form ")"' do |sexp, _, _, _, object, _, lvar, _, form, _|
      method_name = "#{lvar}=".to_sym
      sexp.value = s(:send, [object.value, method_name, form.value])
    end

    # method arguments list (in method definition)
    # [a b c]
    rule 'arguments_list : "[" arguments "]"' do |arguments_list, _, arguments, _|
      arguments_list.value = s(:args, arguments.value)
    end

    # multiple arguments
    rule 'arguments : arguments S argument | argument | empty' do |arguments, *arguments_array|
      arguments.value = without_spaces(arguments_array).flat_map(&:value)
    end

    # an argument can be a local variable
    rule 'argument : LVAR' do |argument, identifier|
      argument.value = s(:arg, [identifier.value])
    end

    # empty rule
    rule('empty :') do |empty|
      empty.value = []
    end

    # function/method (may be operator)
    rule 'func : "+" | "-" | "*" | "/" | LVAR' do |func, function|
      func.value = function.value.to_sym
    end

    class FormatError < RuntimeError; end
  end
end
