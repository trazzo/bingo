-module(bingo_communication).

-export([send_greet/2, send_log/2, send_name/2, send_display/2, send_card/2]).


-spec send_greet(pid(), iolist()) -> ok.
send_greet(Pid, Greet) ->
    Pid ! {[{type, <<"greet">>}, {content, iolist_to_binary(Greet)}]}.
    
-spec send_log(pid(), iolist()) -> ok.
send_log(Pid, Message) ->
    Pid ! {[{type, <<"notification">>}, {content, iolist_to_binary(Message)}]}.
    
-spec send_name(pid(), iolist()) -> ok.
send_name(Pid, Name) ->
    Pid ! {[{type, <<"name">>}, {content, iolist_to_binary(Name)}]}.    

-spec send_display(pid(), pos_integer()) -> ok.
send_display(Pid, Number) ->
    Pid ! {[{type, <<"number">>}, {content, Number}]}.

-spec send_card(pid(), bingo:card()) -> ok.
send_card(Pid, Card) ->
    JsonCard = bingo:card2json(Card),
    Pid ! {[{type, <<"card">>}, {content, JsonCard}]}.


    %% NIN PUTO CASO POR AGORA
