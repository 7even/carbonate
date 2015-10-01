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

    class << self
      def s(type, children = [])
        ::Parser::AST::Node.new(type, children)
      end

      def identifier(value)
        value.gsub('-', '_').to_sym
      end

      def handle_string(string, range)
        replacements = {
          '\\n' => "\n",
          '\\r' => "\r",
          '\\t' => "\t",
          '\\"' => '"'
        }
        string[range].gsub(/\\[nrt"]/, replacements)
      end
    end

    lexer do
      literals '+-*/%=<>&|^~!()[]{}#@.'

      token :FLOAT, /\d+\.\d+/ do |t|
        t.value = Parser.s(:float, [t.value.to_f])
        t
      end

      token :INTEGER, /\d+/ do |t|
        t.value = Parser.s(:int, [t.value.to_i])
        t
      end

      token :SYMBOL, /:([A-Za-z0-9_-]+[!?=]?|[+\-*\/])/ do |t|
        t.value = Parser.s(:sym, [Parser.identifier(t.value[1..-1])])
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

      token :TRUE, /true/ do |t|
        t.value = Parser.s(:true)
        t
      end

      token :FALSE, /false/ do |t|
        t.value = Parser.s(:false)
        t
      end

      # nil (without special characters at the end)
      token :NIL, /nil(?![!?=])/

      # characters treated as whitespace
      token :S, /[\s,]+/

      # keywords
      token :DEFCLASS,  /defclass/
      token :DEFMODULE, /defmodule/
      token :DEFMETHOD, /defmethod/
      token :DEF,       /def/
      token :RETURN,    /return/
      token :SUPER,     /super/
      token :ZSUPER,    /zsuper/

      token :IF,     /if/
      token :UNLESS, /unless/
      token :CASE,   /case/
      token :WHILE,  /while/
      token :UNTIL,  /until/

      token :CONST, /([A-Z][A-Za-z0-9]+\.)*[A-Z][A-Za-z0-9]+/ do |t|
        t.value = t.value.split('.').inject(nil) do |namespace, part|
          Parser.s(:const, [namespace, part.to_sym])
        end
        t
      end

      # method name with '!', '?' or '=' at the end
      token :METHOD_NAME, /[a-z][A-Za-z0-9_-]*[!?=]/ do |t|
        t.value = Parser.identifier(t.value)
        t
      end

      # local variable or method name without special chars at the end
      token :LVAR, /[a-z][A-Za-z0-9_-]*/ do |t|
        t.value = Parser.identifier(t.value)
        t
      end

      # method name with '!', '?' or '=' at the end, called without an explicit receiver
      token :SELF_METHOD_NAME, /@[a-z][A-Za-z0-9_-]*[!?=]/ do |t|
        t.value = Parser.identifier(t.value)
        t
      end

      # instance variable or method name without special chars, called without an explicit receiver
      token :IVAR, /@[a-z][A-Za-z0-9_-]*/ do |t|
        t.value = Parser.identifier(t.value)
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

    # form can also be an S-expression, a literal value, a constant, self or a collection of forms
    rule 'form : INTEGER
               | FLOAT
               | STRING
               | SYMBOL
               | REGEXP
               | TRUE
               | FALSE
               | nil
               | array
               | hash
               | set
               | CONST
               | self
               | sexp' do |form, element|
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

    rule 'nil : NIL' do |nil_node, _|
      nil_node.value = s(:nil, [])
    end

    # multiple S-expressions
    rule 'sexps : sexps S sexp | sexp' do |sexps, *sexps_array|
      sexps.value = without_spaces(sexps_array).flat_map(&:value)
    end

    # if statement
    # can have only "then" clause:
    #   (if (valid? user) (save user))
    # or both "then" and "else" clauses:
    #   (if (> a b) a b)
    rule 'sexp : "(" IF S form S form ")"
               | "(" IF S form S form S form ")"' do |sexp, _, _, _, condition, _, then_clause, _, else_clause, _|
      sexp.value = s(:if, [condition.value, then_clause.value, else_clause && else_clause.value])
    end

    # unless statement
    #   (unless (persisted? user) (save user))
    rule 'sexp : "(" UNLESS S form S form ")"' do |sexp, _, _, _, condition, _, then_clause, _|
      sexp.value = s(:if, [condition.value, nil, then_clause.value])
    end

    # case statement
    #   (case x
    #     1 "one"
    #     2 "two")
    # can have an "else" clause at the end:
    #   (case lang
    #     "clojure" "great!"
    #     "ruby" "cool"
    #     "crap")
    rule 'sexp : "(" CASE S form S forms ")"' do |sexp, _, _, _, form, _, forms, _|
      elements = if forms.value.count.even?
        [*forms.value, nil]
      else
        forms.value
      end

      when_clauses = elements[0..-2].each_slice(2).map do |value, expr|
        s(:when, [value, expr])
      end

      sexp.value = s(:case, [form.value, *when_clauses, elements.last])
    end

    # while loop
    #   (while (< x 5) (def x (+ x 1)))
    rule 'sexp : "(" WHILE S form S form ")"' do |sexp, _, _, _, condition, _, body, _|
      sexp.value = s(:while, [condition.value, body.value])
    end

    # until loop
    #   (until (>= x 5) (def x (+ x 1)))
    rule 'sexp : "(" UNTIL S form S form ")"' do |sexp, _, _, _, condition, _, body, _|
      sexp.value = s(:until, [condition.value, body.value])
    end

    # instance method call with an explicit receiver
    # (+ 2 2)
    rule 'sexp : "(" func S forms ")"' do |sexp, _, func, _, forms, _|
      sexp.value = s(:send, [forms.value.first, func.value, *forms.value[1..-1]])
    end

    # class method call with an explicit receiver
    # (User/find-by {:first-name "John"})
    rule 'sexp : "(" CONST "/" func S forms ")"
               | "(" CONST "/" func ")"' do |sexp, _, const, _, func, _, forms, _|
      arguments = forms && forms.value || []
      sexp.value = s(:send, [const.value, func.value, *arguments])
    end

    # method call with an implicit receiver
    # (@attr-reader :first-name)
    rule 'sexp : "(" IVAR S forms ")"
               | "(" IVAR ")"
               | "(" SELF_METHOD_NAME S forms ")"
               | "(" SELF_METHOD_NAME ")"' do |sexp, _, method_name, _, forms, _|
      arguments = forms && forms.value || []
      sexp.value = s(:send, [nil, method_name.value[1..-1].to_sym, *arguments])
    end

    # return statement without parameters
    # (return)
    rule 'sexp : "(" RETURN ")"' do |sexp, _, _, _|
      sexp.value = s(:return, [])
    end

    # return statement with parameters
    # (return 1)
    rule 'sexp : "(" RETURN S forms ")"' do |sexp, _, _, _, forms, _|
      sexp.value = s(:return, forms.value)
    end

    # call to super with explicit parameters
    #   (super)
    #   (super "parameter")
    rule 'sexp : "(" SUPER ")"
               | "(" SUPER S forms ")"' do |sexp, _, _, _, forms, _|
      parameters = forms ? forms.value : []
      sexp.value = s(:super, parameters)
    end

    # call to super with implicit parameters
    #   (zsuper)
    rule 'sexp : "(" ZSUPER ")"' do |sexp, _, _, _|
      sexp.value = s(:zsuper, [])
    end

    # class constructor call
    # (User. {:name "John"})
    rule 'sexp : "(" CONST "." S forms ")"' do |sexp, _, const, _, _, forms|
      sexp.value = s(:send, [const.value, :new, *forms.value])
    end

    # class definition w/o a parent class
    # (defclass User (class body))
    rule 'sexp : "(" DEFCLASS S CONST S forms ")"' do |sexp, _, _, _, const, _, forms, _|
      class_body = wrap_in_begin(forms.value)
      sexp.value = s(:class, [const.value, nil, class_body])
    end

    # class definition with a parent class
    # (defclass User < Base (class body))
    rule 'sexp : "(" DEFCLASS S CONST S "<" S form S forms ")"' do |sexp, _, _, _, const, _, _, _, form, _, forms|
      class_body = wrap_in_begin(forms.value)
      sexp.value = s(:class, [const.value, form.value, class_body])
    end

    # module definition
    # (defmodule Enumerable (module body ...))
    rule 'sexp : "(" DEFMODULE S CONST S forms ")"' do |sexp, _, _, _, const, _, forms, _|
      module_body = wrap_in_begin(forms.value)
      sexp.value = s(:module, [const.value, module_body])
    end

    # singleton class definition
    # (<<- user (defmethod name [] @name)
    rule 'sexp : "(" "<" "<" "-" S form S forms ")"' do |sexp, _, _, _, _, _, form, _, forms, _|
      singleton_body = wrap_in_begin(forms.value)
      sexp.value = s(:sclass, [form.value, singleton_body])
    end

    # method defition
    # (defmethod full-name (join [first-name last-name]))
    rule 'sexp : "(" DEFMETHOD S LVAR S arguments_list S forms ")"
               | "(" DEFMETHOD S METHOD_NAME S arguments_list S forms ")"' do |sexp, _, _, _, method_name, _, args, _, forms, _|
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

    # method arguments list with a splat argument at the end
    #   [a b c & d]
    rule 'arguments_list : "[" arguments S restargument "]"' do |arguments_list, _, args, _, restargument, _|
      arguments_list.value = s(:args, [*args.value, restargument.value])
    end

    # method arguments list consisting of one splat argument
    #   [& arguments]
    rule 'arguments_list : "[" restargument "]"' do |arguments_list, _, restargument, _|
      arguments_list.value = s(:args, [restargument.value])
    end

    # multiple arguments
    rule 'arguments : arguments S argument | argument | empty' do |arguments, *arguments_array|
      arguments.value = without_spaces(arguments_array).flat_map(&:value)
    end

    # an argument can be a local variable
    rule 'argument : LVAR' do |argument, identifier|
      argument.value = s(:arg, [identifier.value])
    end

    # an argument can be a splat
    rule 'restargument : "&" S LVAR' do |restargument, _, _, identifier|
      restargument.value = s(:restarg, [identifier.value])
    end

    # empty rule
    rule('empty :') do |empty|
      empty.value = []
    end

    # method name (may be operator)
    rule 'func : operator | METHOD_NAME | LVAR' do |func, function|
      func.value = function.value
    end

    # operator
    rule 'operator : "+" | "-" | "*" | "/" | "%" | "*" "*"
                   | "=" | "!" "=" | "<" | ">" | "<" "=" | ">" "=" | "<" "=" ">" | "=" "=" "="
                   | "&" | "|" | "^" | "~" | "<" "<" | ">" ">"
                   | "&" "&" | "|" "|" | "!"' do |operator, *operator_chars|
      operator_name = operator_chars.map(&:value).join

      operator.value = case operator_name
      when '='  then :==
      when '&&' then :and
      when '||' then :or
      else           operator_name.to_sym
      end
    end

    class FormatError < RuntimeError; end
  end
end
