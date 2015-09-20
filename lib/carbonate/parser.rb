require 'rly'
require 'ast'
require 'parser/ast/node'

module Carbonate
  class Parser < Rly::Yacc
    def without_spaces(elements)
      elements.reject do |arg|
        arg.type == :S
      end
    end

    def s(type, children)
      self.class.s(type, children)
    end

    def self.s(type, children)
      ::Parser::AST::Node.new(type, children)
    end

    lexer do
      literals '+-*/()[]'

      token :NUMBER, /\d+/ do |t|
        t.value = Parser.s(:int, [t.value.to_i])
        t
      end

      # whitespace
      token :S, /\s+/

      token :DEFCLASS,  /defclass/
      token :DEFMETHOD, /defmethod/
      token :DEF,       /def/

      token :CONST, /[A-Z][A-Za-z0-9]*/ do |t|
        t.value = Parser.s(:const, [nil, t.value.to_sym])
        t
      end

      token :LVAR, /[a-z][A-Za-z0-9_-]*/ do |t|
        t.value = t.value.gsub(/-/, '_').to_sym
        t
      end

      token :IVAR, /@[a-z][A-Za-z0-9_-]*/ do |t|
        t.value = t.value.gsub(/-/, '_').to_sym
        t
      end

      on_error do |t|
        puts "Illegal character #{t.value.inspect} at line #{t.lexer.lineno + 1}"
        t.lexer.pos += 1
        nil
      end
    end

    # outermost scope
    rule 'source : sexps S | sexps' do |source, sexps|
      source.value = s(:begin, sexps.value)
    end

    # multiple S-expressions
    rule 'sexps : sexps S sexp | sexp' do |sexps, *sexps_array|
      sexps.value = without_spaces(sexps_array).flat_map(&:value)
    end

    # function/method call
    # (+ 2 2)
    rule 'sexp : "(" func S forms ")"' do |sexp, _, func, _, forms, _|
      sexp.value = s(:send, [forms.value.first, func.value, *forms.value[1..-1]])
    end

    # class definition
    # (defclass User (class body))
    rule 'sexp : "(" DEFCLASS S CONST S sexps ")"' do |sexp, _, _, _, const, _, sexps, _|
      sexp.value = s(:class, [const.value, nil, *sexps.value])
    end

    # method defition
    # (defmethod full-name (join [first-name last-name]))
    rule 'sexp : "(" DEFMETHOD S LVAR S arguments_list S sexps ")"' do |sexp, _, _, _, method_name, _, args, _, sexps, _|
      method_body = s(:begin, sexps.value)
      sexp.value = s(:def, [method_name.value.to_sym, args.value, method_body])
    end

    # assignment
    # (def age 30)
    rule 'sexp : "(" DEF S IVAR S form ")"' do |sexp, _, _, _, var_name, _, form, _|
      sexp.value = s(:ivasgn, [var_name.value, form.value])
    end

    # multiple forms
    rule 'forms : forms S form | form' do |forms, *forms_array|
      forms.value = without_spaces(forms_array).flat_map(&:value)
    end

    # form can be a local variable
    rule 'form : LVAR' do |form, lvar|
      form.value = s(:lvar, [lvar.value])
    end

    # form can also be an instance variable, a number or an S-expression
    rule 'form : IVAR | NUMBER | sexp' do |form, element|
      form.value = element.value
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
    rule('empty :') {}

    # function/method (may be operator)
    rule 'func : "+" | "-" | "*" | "/"' do |func, operator|
      func.value = operator.value.to_sym
    end
  end
end
