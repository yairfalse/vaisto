defmodule Vaisto.CoreEmitter do
  @moduledoc """
  Emits Core Erlang from typed Vaisto AST.
  
  Core Erlang is the intermediate representation that
  the BEAM compiler understands. By targeting Core Erlang,
  Vaisto becomes a native BEAM language.
  """

  @doc """
  Emit Core Erlang source from a typed AST.
  """
  def emit(typed_ast) do
    case typed_ast do
      {:process, name, init, handlers, _type} ->
        emit_process(name, init, handlers)
      
      {:supervise, strategy, children} ->
        emit_supervisor(strategy, children)
      
      {:call, func, args, _type} ->
        emit_call(func, args)
      
      {:lit, _type, value} ->
        emit_literal(value)
      
      other ->
        emit_expr(other)
    end
  end

  @doc """
  Emit and compile to BEAM binary.
  """
  def emit_to_beam(typed_ast) do
    # For now, return the Core Erlang source
    # Full compilation requires parsing Core Erlang
    emit(typed_ast)
  end

  # Process emission
  defp emit_process(name, initial_state, handlers) do
    """
    module '#{name}' ['start_link'/1, 'loop'/1]
    attributes []

    'start_link'/1 = 
      fun (InitState) ->
        apply 'erlang':'spawn'('#{name}', 'loop', [InitState])

    'loop'/1 = 
      fun (State) ->
        receive
          #{emit_handlers(handlers, name)}
        end
    """
  end

  defp emit_handlers(handlers, module_name) do
    handlers
    |> Enum.map(fn {msg, body, _type} ->
      """
          <{'#{msg}', From}> when 'true' ->
            let <NewState> = #{emit_expr(body)} in
            do apply 'erlang':'!'(From, NewState)
            apply '#{module_name}':'loop'(NewState)
      """
    end)
    |> Enum.join("\n")
  end

  # Supervisor emission
  defp emit_supervisor(strategy, children) do
    child_specs = Enum.map(children, &emit_child_spec/1) |> Enum.join(", ")
    
    """
    module 'vaisto_sup' ['start_link'/0, 'init'/1]
    attributes []

    'start_link'/0 = 
      fun () ->
        apply 'supervisor':'start_link'({'local', 'vaisto_sup'}, 'vaisto_sup', [])

    'init'/1 = 
      fun (_Args) ->
        let <Flags> = {'#{strategy}', 5, 10} in
        let <Specs> = [#{child_specs}] in
        {'ok', {Flags, Specs}}
    """
  end

  defp emit_child_spec({:call, name, [init_val], _type}) do
    """
    {'#{name}', 
     {'#{name}', 'start_link', [#{emit_expr(init_val)}]}, 
     'permanent', 5000, 'worker', ['#{name}']}
    """
  end

  defp emit_child_spec({name, init_val}) when is_atom(name) do
    """
    {'#{name}', 
     {'#{name}', 'start_link', [#{emit_expr(init_val)}]}, 
     'permanent', 5000, 'worker', ['#{name}']}
    """
  end

  # Expression emission
  defp emit_expr({:lit, :int, n}), do: "#{n}"
  defp emit_expr({:lit, :float, f}), do: "#{f}"
  defp emit_expr({:lit, :atom, a}), do: "'#{a}'"
  defp emit_expr({:lit, :bool, b}), do: "'#{b}'"

  defp emit_expr({:var, name, _type}), do: "#{camelize(name)}"

  defp emit_expr({:call, :+, [a, b], _type}) do
    "apply 'erlang':'+'(#{emit_expr(a)}, #{emit_expr(b)})"
  end

  defp emit_expr({:call, :-, [a, b], _type}) do
    "apply 'erlang':'-'(#{emit_expr(a)}, #{emit_expr(b)})"
  end

  defp emit_expr({:call, :*, [a, b], _type}) do
    "apply 'erlang':'*'(#{emit_expr(a)}, #{emit_expr(b)})"
  end

  defp emit_expr({:call, func, args, _type}) do
    arg_str = args |> Enum.map(&emit_expr/1) |> Enum.join(", ")
    "apply '#{func}'(#{arg_str})"
  end

  # Fallback for simple values
  defp emit_expr(n) when is_integer(n), do: "#{n}"
  defp emit_expr(f) when is_float(f), do: "#{f}"
  defp emit_expr(a) when is_atom(a), do: "'#{a}'"
  defp emit_expr(:state), do: "State"

  defp emit_literal(n) when is_integer(n), do: "#{n}"
  defp emit_literal(f) when is_float(f), do: "#{f}"
  defp emit_literal(a) when is_atom(a), do: "'#{a}'"
  defp emit_literal(b) when is_boolean(b), do: "'#{b}'"

  defp emit_call(func, args) do
    arg_str = args |> Enum.map(&emit/1) |> Enum.join(", ")
    "apply 'erlang':'#{func}'(#{arg_str})"
  end

  defp camelize(atom) when is_atom(atom) do
    atom
    |> Atom.to_string()
    |> String.split("_")
    |> Enum.map(&String.capitalize/1)
    |> Enum.join()
  end
end
