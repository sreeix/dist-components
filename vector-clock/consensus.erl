-module(consensus).
-export([run/0, loop/0, printer_loop/0, print_end/1]).

loop() ->
    receive
	{printer_request, Process, _} ->
	    io:format("Got printer requested ~p: ~n ", [Process]),
	    loop();
	{printer_release, From, _} ->
	    io:format("Releasing the printer ~p ~n", [From]),
	    loop()
    end.

printer_loop() ->
    receive
	{print, From, For, Text} ->
	    io:format("Got print request from ~p:~p ~n", [From, Text]),
	    timer:sleep(round(random:uniform() * 10000)),
	    io:format("sending response to the print caller~n"),
	    From ! {print_success, For},
	    io:format("printer sent message~n"),
	    printer_loop()
    end.

print_end(Processes) ->
    receive
	{print_success, PrintRequester} ->
	    debugger:start(),
	    io:format("got success from printer~n"),
	    [Proc ! {printer_release, PrintRequester, no_clock} || Proc <- Processes]
	end.

print(PrintRequester, Processes, Content) ->
    PrintMediator = spawn(consensus,print_end, [Processes]),
    whereis('printer') ! {print, PrintMediator, PrintRequester, Content},
    [Proc ! {printer_request, PrintRequester, no_clock} || Proc <- Processes, Proc =/= PrintRequester].

run() ->
    Processes = [spawn(consensus, loop,[]), spawn(consensus, loop,[]),spawn(consensus, loop,[])],
    io:format("Processes: ~w~n", [Processes]),
    register(printer, spawn(consensus, printer_loop, [])),
    PrintRequester = lists:nth(round(random:uniform() * length(Processes)) + 1, Processes),
    print(PrintRequester, Processes, "foobarbaz").
