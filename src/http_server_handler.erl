-module(http_server_handler).

-export([process_request/1]).

decode_tasks([{<<"tasks">>, TaskList}]) ->
    decode_tasks(TaskList, {digraph:new(), []}).

decode_tasks([], {TaskGraph, Commands}) ->
    {TaskGraph, Commands};
decode_tasks([Task|Rest], {TaskGraph, Commands}) ->
    {struct, Parameters} = Task,
    TaskName = proplists:get_value(<<"name">>, Parameters),
    digraph:add_vertex(TaskGraph, TaskName),
    case proplists:get_value(<<"requires">>, Parameters) of
        undefined ->
            ok;
        Requirements ->
            lists:foreach(
                fun(RequiredTaskName) ->
                    digraph:add_vertex(TaskGraph, RequiredTaskName),
                    digraph:add_edge(TaskGraph, RequiredTaskName, TaskName)
                end, Requirements)
    end,
    decode_tasks(Rest, {TaskGraph, [{TaskName, proplists:get_value(<<"command">>, Parameters)}] ++ Commands}).

fmt(Commands, Order) ->
    Response =
        lists:foldr(
            fun(Task, Acc) ->
                Command = proplists:get_value(Task, Commands),
                [{struct, [{<<"name">>, Task}, {<<"command">>, Command}]}] ++ Acc
            end, [], Order),
    [{<<"tasks">>, Response}].

process_request(Req) ->
    Body = mochiweb_request:recv_body(Req),
    {struct, Tasks} = mochijson2:decode(Body),
    {TaskGraph, Commands} = decode_tasks(Tasks),
    Order = digraph_utils:topsort(TaskGraph),
    write_script(Commands, Order),
    fmt(Commands, Order).

write_script(Commands, Order) ->
    {ok, S} = file:open("script.sh", [write]),
    file:write(S, "#!/usr/bin/env bash\n"),
    lists:foreach(
        fun(Task) ->
            Command = proplists:get_value(Task, Commands),
            file:write(S, binary_to_list(Command) ++ "\n")
        end, Order).
