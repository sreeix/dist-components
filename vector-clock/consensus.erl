-module(consensus).
-export([run/0, loop/1, printer_loop/0, print_end/1]).

loop(VC) ->
    Queue = [],
    receive
	{printer_request, Process, _} ->
	    io:format("Got printer requested ~p: ~n ", [Process]),
	    Process ! {printer_request_ack, self()},
	    loop(VC);
	{printer_release, From, _} ->
	    io:format("Releasing the printer ~p ~n", [From]),
	    loop(VC)
    end.

printer_loop() ->
    receive
	{print, From, For, Text} ->
	    io:format("Got print request from ~p:~p ~n", [From, Text]),
	    timer:sleep(round(random:uniform() * 10000)),
	    From ! {print_success, For},
	    printer_loop()
    end.

print_end(Processes) ->
    receive
	{print_success, PrintRequester} ->
	    io:format("got success from printer~n"),
	    [Proc ! {printer_release, PrintRequester, no_clock} || Proc <- Processes]
	end.

print(PrintRequester, Processes, Content) ->
    PrintMediator = spawn(consensus,print_end, [Processes]),
    printer ! {print, PrintMediator, PrintRequester, Content},
    [Proc ! {printer_request, PrintRequester, no_clock} || Proc <- Processes, Proc =/= PrintRequester].

run() ->
    Processes = [spawn(consensus, loop,[vc:new()]), spawn(consensus, loop,[vc:new()]),spawn(consensus, loop,[vc:new()])],
    register(printer, spawn(consensus, printer_loop, [])),
    do_run(Processes).

do_run(Processes) ->
    PrintRequester = lists:nth(random:uniform(length(Processes)), Processes),
    print(PrintRequester, Processes, "foobarbaz"),
    io:format("Wating for 5 seconds~n"),
    timer:sleep(5000),
    do_run(Processes).
