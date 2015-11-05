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

    def destructure_arguments(arguments)
      if arguments[0..-2].any? { |arg| [:pseudo_block, :block_pass].include?(arg.type) }
        raise FormatError, 'You can specify only one block per method call (as the last argument)'
      end

      if !arguments.empty? && arguments.last.type == :pseudo_block
        [arguments[0..-2], arguments.last]
      else
        [arguments, nil]
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

      token :FLOAT, /-?\d+\.\d+/ do |t|
        t.value = Parser.s(:float, [t.value.to_f])
        t
      end

      token :INTEGER, /-?\d+/ do |t|
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
      token :S, /[\s,]+/ do |t|
        t.lexer.lineno += t.value.count("\n")
        t
      end

      # keywords
      token :DEFCLASS,  /defclass/
      token :DEFMODULE, /defmodule/
      token :DEFMETHOD, /defmethod/
      token :DEF_OR,    /def-or/
      token :DEF,       /def/
      token :RETURN,    /return/
      token :SUPER,     /super/
      token :ZSUPER,    /zsuper/
      token :TRY,       /try/
      token :RESCUE,    /rescue/
      token :ENSURE,    /ensure/

      token :IF,     /if/
      token :UNLESS, /unless/
      token :CASE,   /case/
      token :WHILE,  /while/
      token :UNTIL,  /until/

      token :CONST, /\.?([A-Z][A-Za-z0-9_-]+\.)*[A-Z][A-Za-z0-9_-]+/ do |t|
        parts = t.value.split('.').reject(&:empty?)
        top_namespace = Parser.s(:cbase) if t.value.start_with?('.')

        t.value = parts.inject(top_namespace) do |namespace, part|
          Parser.s(:const, [namespace, Parser.identifier(part)])
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
        fail FormatError, "Unknown character '#{t.value}' at line #{t.lexer.lineno.succ}"
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
    #   user
    rule 'form : LVAR' do |form, lvar|
      form.value = s(:lvar, [lvar.value])
    end

    # form can be an instance variable
    #   @user
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
               | irange
               | erange
               | CONST
               | self
               | sexp' do |form, element|
      form.value = element.value
    end

    # array
    #   [1 2 3]
    rule 'array : "[" forms "]"' do |array, _, sexps, _|
      array.value = s(:array, sexps.value)
    end

    # empty array
    #   []
    rule 'array : "[" "]"' do |array, _, _|
      array.value = s(:array, [])
    end

    # hash
    #   {:first-name "Rich", :last-name "Hickey"}
    # commas are optional, elements count must be even
    rule 'hash : "{" forms "}"' do |hash, _, forms, _|
      raise FormatError.new('Odd number of elements in a hash') if forms.value.count.odd?

      pairs = forms.value.each_slice(2).map { |pair| s(:pair, pair) }
      hash.value = s(:hash, pairs)
    end

    # empty hash
    #   {}
    rule 'hash : "{" "}"' do |hash, _, _|
      hash.value = s(:hash, [])
    end

    # set
    #   #{123 "string" :symbol}
    rule 'set : "#" "{" forms "}"' do |set, _, _, forms, _|
      set.value = s(:send,
        [
          s(:const, [nil, :Set]),
          :new,
          s(:array, forms.value)
        ]
      )
    end

    # inclusive range
    #   1..10
    rule 'irange : form "." "." form' do |range, first, _, _, last|
      range.value = s(:irange, [first.value, last.value])
    end

    # exclusive range
    #   1...11
    rule 'erange : form "." "." "." form' do |range, first, _, _, _, last|
      range.value = s(:erange, [first.value, last.value])
    end

    # self
    #   @
    rule 'self : "@"' do |self_node, _|
      self_node.value = s(:self, [])
    end

    # nil
    #   nil
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
    #   (+ 2 2)
    rule 'sexp : "(" func S form ")"
               | "(" func S form S arguments_list ")"' do |sexp, _, func, _, receiver, _, arguments_list, _|
      arguments, block = destructure_arguments(arguments_list && arguments_list.value || [])
      send_node = s(:send, [receiver.value, func.value, *arguments])

      sexp.value = if block.nil?
        send_node
      else
        s(:block, [send_node, *block.children])
      end
    end

    # class method call with an explicit receiver
    #   (User/find-by {:first-name "John"})
    rule 'sexp : "(" CONST "/" func ")"
               | "(" CONST "/" func S arguments_list ")"' do |sexp, _, const, _, func, _, arguments_list, _|
      arguments, block = destructure_arguments(arguments_list && arguments_list.value || [])
      send_node = s(:send, [const.value, func.value, *arguments])

      sexp.value = if block.nil?
        send_node
      else
        s(:block, [send_node, *block.children])
      end
    end

    # method call with an implicit receiver
    #   (@attr-reader :first-name)
    rule 'sexp : "(" IVAR ")"
               | "(" IVAR S arguments_list ")"
               | "(" SELF_METHOD_NAME ")"
               | "(" SELF_METHOD_NAME S arguments_list ")"' do |sexp, _, method_name, _, arguments_list, _|
      arguments, block = destructure_arguments(arguments_list && arguments_list.value || [])
      send_node = s(:send, [nil, method_name.value[1..-1].to_sym, *arguments])

      sexp.value = if block.nil?
        send_node
      else
        s(:block, [send_node, *block.children])
      end
    end

    # class constructor call
    #   (User. {:name "John"})
    rule 'sexp : "(" CONST "." ")"
               | "(" CONST "." S arguments_list ")"' do |sexp, _, const, _, _, arguments_list, _|
      arguments, block = destructure_arguments(arguments_list && arguments_list.value || [])
      send_node = s(:send, [const.value, :new, *arguments])

      sexp.value = if block.nil?
        send_node
      else
        s(:block, [send_node, *block.children])
      end
    end

    # call to super with explicit parameters
    #   (super)
    #   (super "argument")
    rule 'sexp : "(" SUPER ")"
               | "(" SUPER S arguments_list ")"' do |sexp, _, _, _, arguments_list, _|
      arguments, block = destructure_arguments(arguments_list && arguments_list.value || [])
      send_node = s(:super, arguments)

      sexp.value = if block.nil?
        send_node
      else
        s(:block, [send_node, *block.children])
      end
    end

    # call to super with implicit parameters
    #   (zsuper)
    rule 'sexp : "(" ZSUPER ")"
               | "(" ZSUPER S arguments_list ")"' do |sexp, _, _, _, arguments_list, _|
      arguments, block = destructure_arguments(arguments_list && arguments_list.value || [])
      send_node = s(:zsuper, [])

      sexp.value = if block.nil?
        send_node
      else
        s(:block, [send_node, *block.children])
      end
    end

    # multiple arguments (in method invocation)
    rule 'arguments_list : arguments_list S argument | argument | empty' do |arguments_list, *arguments|
      arguments_list.value = without_spaces(arguments).flat_map(&:value)
    end

    # an argument can be a simple form
    rule 'argument : form' do |argument, form|
      argument.value = form.value
    end

    # an argument can be splat
    rule 'argument : "&" S form' do |argument, _, _, form|
      argument.value = s(:splat, [form.value])
    end

    # the last argument can be passed as a block
    rule 'argument : "%" S form' do |argument, _, _, form|
      argument.value = s(:block_pass, [form.value])
    end

    # the last argument can be an inline block
    rule 'argument : "%" "(" parameters_list S forms ")"' do |argument, _, _, params, _, forms, _|
      block_body = wrap_in_begin(forms.value)
      argument.value = s(:pseudo_block, [params.value, block_body])
    end

    # the last argument can also be an inline block without parameters
    rule 'argument : "%" form
                   | "%" "(" forms ")"' do |argument, _, *forms_array|
      body = forms_array.reject do |element|
        ['(', ')'].include?(element.type)
      end.flat_map(&:value)

      argument.value = s(:pseudo_block, [s(:args, []), wrap_in_begin(body)])
    end

    # return statement without parameters
    #   (return)
    rule 'sexp : "(" RETURN ")"' do |sexp, _, _, _|
      sexp.value = s(:return, [])
    end

    # return statement with parameters
    #   (return 1)
    rule 'sexp : "(" RETURN S forms ")"' do |sexp, _, _, _, forms, _|
      sexp.value = s(:return, forms.value)
    end

    # try statement with a rescue clause
    #   (try (File/read path)
    #        (rescue Errno.ENOENT e
    #                (@puts (message e))))
    rule 'sexp : "(" TRY S forms S rescues ")"' do |sexp, _, _, _, forms, _, rescue_clauses, _|
      sexp.value = s(:kwbegin, [s(:rescue, [wrap_in_begin(forms.value), *rescue_clauses.value, nil])])
    end

    # try statement with an ensure clause
    #   (try (File/read path)
    #        (ensure (@puts "Tries to read a file.")))
    rule 'sexp : "(" TRY S forms S ensure ")"' do |sexp, _, _, _, forms, _, ensure_clause|
      sexp.value = s(:kwbegin, [s(:ensure, [wrap_in_begin(forms.value), ensure_clause.value.children.first])])
    end

    # multiple rescue clauses
    rule 'rescues : rescues S rescue | rescue' do |rescues, *rescues_array|
      rescues.value = without_spaces(rescues_array).flat_map(&:value)
    end

    # rescue clause
    rule 'rescue : "(" RESCUE S CONST S LVAR S forms ")"' do |rescue_clause, _, _, _, const, _, lvar, _, forms, _|
      rescue_clause.value = s(:resbody,
        [
          s(:array, [const.value]),
          s(:lvasgn, [lvar.value]),
          wrap_in_begin(forms.value)
        ]
      )
    end

    # ensure clause
    rule 'ensure : "(" ENSURE S forms ")"' do |ensure_clause, _, _, _, forms, _|
      ensure_clause.value = s(:pseudo_ensure, [wrap_in_begin(forms.value)])
    end

    # class definition w/o a parent class
    #   (defclass User (class body))
    rule 'sexp : "(" DEFCLASS S CONST S forms ")"' do |sexp, _, _, _, const, _, forms, _|
      class_body = wrap_in_begin(forms.value)
      sexp.value = s(:class, [const.value, nil, class_body])
    end

    # class definition with a parent class
    #   (defclass User < Base (class body))
    rule 'sexp : "(" DEFCLASS S CONST S "<" S form S forms ")"' do |sexp, _, _, _, const, _, _, _, form, _, forms|
      class_body = wrap_in_begin(forms.value)
      sexp.value = s(:class, [const.value, form.value, class_body])
    end

    # module definition
    #   (defmodule Enumerable (module body ...))
    rule 'sexp : "(" DEFMODULE S CONST S forms ")"' do |sexp, _, _, _, const, _, forms, _|
      module_body = wrap_in_begin(forms.value)
      sexp.value = s(:module, [const.value, module_body])
    end

    # singleton class definition
    #   (<<- user (defmethod name [] @name)
    rule 'sexp : "(" "<" "<" "-" S form S forms ")"' do |sexp, _, _, _, _, _, form, _, forms, _|
      singleton_body = wrap_in_begin(forms.value)
      sexp.value = s(:sclass, [form.value, singleton_body])
    end

    # method defition
    #   (defmethod full-name [] (join [first-name last-name]))
    rule 'sexp : "(" DEFMETHOD S LVAR S parameters_list S forms ")"
               | "(" DEFMETHOD S METHOD_NAME S parameters_list S forms ")"' do |sexp, _, _, _, method_name, _, params, _, forms, _|
      method_body = wrap_in_begin(forms.value)
      sexp.value = s(:def, [method_name.value.to_sym, params.value, method_body])
    end

    # lambda definition
    #   (-> [user] (email user))
    rule 'sexp : "(" "-" ">" S parameters_list S forms ")"' do |sexp, _, _, _, _, params, _, forms, _|
      lambda_body = wrap_in_begin(forms.value)
      sexp.value = s(:block, [s(:send, [nil, :lambda]), params.value, lambda_body])
    end

    # lambda definition without parameters
    #   (-> (@puts "Hello world!"))
    rule 'sexp : "(" "-" ">" S forms ")"' do |sexp, _, _, _, _, forms, _|
      lambda_body = wrap_in_begin(forms.value)
      sexp.value = s(:block, [s(:send, [nil, :lambda]), s(:args, []), lambda_body])
    end

    # local variable assignment
    #   (def username "7even")
    rule 'sexp : "(" DEF S LVAR S form ")"' do |sexp, _, _, _, var_name, _, form, _|
      sexp.value = s(:lvasgn, [var_name.value, form.value])
    end

    # conditional local variable assignment
    #   (def-or username "7even")
    rule 'sexp : "(" DEF_OR S LVAR S form ")"' do |sexp, _, _, _, var_name, _, form, _|
      sexp.value = s(:or_asgn, [s(:lvasgn, [var_name.value]), form.value])
    end

    # instance variable assignment
    #   (def @age 30)
    rule 'sexp : "(" DEF S IVAR S form ")"' do |sexp, _, _, _, var_name, _, form, _|
      sexp.value = s(:ivasgn, [var_name.value, form.value])
    end

    # conditional instance variable assignment
    #   (def-or @age 30)
    rule 'sexp : "(" DEF_OR S IVAR S form ")"' do |sexp, _, _, _, var_name, _, form, _|
      sexp.value = s(:or_asgn, [s(:ivasgn, [var_name.value]), form.value])
    end

    # object attribute assignment
    #   (def user.name "John")
    rule 'sexp : "(" DEF S form "." LVAR S form ")"' do |sexp, _, _, _, object, _, lvar, _, form, _|
      method_name = "#{lvar}=".to_sym
      sexp.value = s(:send, [object.value, method_name, form.value])
    end

    # conditional object attribute assignment
    #   (def-or user.name "John")
    rule 'sexp : "(" DEF_OR S form "." LVAR S form ")"' do |sexp, _, _, _, object, _, lvar, _, form, _|
      sexp.value = s(:or_asgn, [s(:send, [object.value, lvar.value]), form.value])
    end

    # constant assignment
    #   (def DAYS-IN-WEEK 7)
    rule 'sexp : "(" DEF S CONST S form ")"' do |sexp, _, _, _, const, _, form, _|
      sexp.value = s(:casgn, [*const.value.children, form.value])
    end

    # method parameters list (in method definition)
    #   [a b c]
    rule 'parameters_list : "[" parameters "]"' do |parameters_list, _, parameters, _|
      parameters_list.value = s(:args, parameters.value)
    end

    # method parameters list with a block parameter at the end
    #   [a b c % d]
    rule 'parameters_list : "[" parameters S block_parameter "]"' do |parameters_list, _, args, _, block_parameter, _|
      parameters_list.value = s(:args, [*args.value, block_parameter.value])
    end

    # method parameters list consisting of one block parameter
    #   [% parameters]
    rule 'parameters_list : "[" block_parameter "]"' do |parameters_list, _, block_parameter, _|
      parameters_list.value = s(:args, [block_parameter.value])
    end

    # multiple parameters
    rule 'parameters : parameters S parameter | parameter | empty' do |parameters, *parameters_array|
      parameters.value = without_spaces(parameters_array).flat_map(&:value)
    end

    # a parameter can be a local variable
    rule 'parameter : LVAR' do |parameter, identifier|
parameter.value = s(:arg, [identifier.value])
    end

    # a parameter can have a default value
    #   [base 10]
    rule 'parameter : "[" LVAR S form "]"' do |parameter, _, identifier, _, default_value, _|
      parameter.value = s(:optarg, [identifier.value, default_value.value])
    end

    # a parameter can be a splat
    rule 'parameter : "&" S LVAR' do |parameter, _, _, identifier|
      parameter.value = s(:restarg, [identifier.value])
    end

    # the last parameter can be a block
    rule 'block_parameter : "%" S LVAR' do |block_parameter, _, _, identifier|
      block_parameter.value = s(:blockarg, [identifier.value])
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

    # collection member reader
    #   hash[:key]
    #   array[1, 5]
    rule 'sexp : form "[" forms "]"' do |sexp, collection, _, arguments, _|
      sexp.value = s(:send, [collection.value, :[], *arguments.value])
    end

    # collection member writer
    #   (def hash[:key] value)
    #   (def array[1 2] 3)
    rule 'sexp : "(" DEF S form "[" forms "]" S form ")"' do |sexp, _, _, _, collection, _, arguments, _, _, value, _|
      sexp.value = s(:send, [collection.value, :[]=, *arguments.value, value.value])
    end

    # conditional collection member writer
    #   (def-or hash[:key] value)
    rule 'sexp : "(" DEF_OR S form "[" forms "]" S form ")"' do |sexp, _, _, _, collection, _, arguments, _, _, value, _|
      sexp.value = s(:or_asgn, [s(:send, [collection.value, :[], *arguments.value]), value.value])
    end

    on_error -> (token) do
      if token.nil?
        fail FormatError, 'Input ends unexpectedly'
      else
        token_string = if token.value.respond_to?(:children)
          token.value.children.first
        else
          token.value
        end

        message = "Unexpected token '#{token_string}'"
        message << " at line #{token.location_info[:lineno].succ}"

        fail FormatError, message
      end
    end

    class FormatError < RuntimeError; end
  end
end
