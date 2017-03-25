/* File irc_chat_client_foofy.pl   :- pack_install(eggdrop). */
:- module(foofy_bot,[go/0,wave/0]).

:- use_module(library(eggdrop))).
:- set_irc_nick("Foofy").
:- set_irc_serv("irc.dal.net",6668).

install_hello:- 
    reg_irc_hook(on_irc_msg(Channel,Nick,"hello"), 
              ( downcase_atom(Nick,DCNick), greet(DCNick,Channel) )).

greet(Nick,Channel):- say(["hello",Nick,"welcome to",Channel]).

irc_hooks:on_irc_msg(_, _,"bye"):- wave.  % confirm wave/0 is usable 

wave:- action("waves").

irc_hooks:on_irc_msg(_, _,"foofy"):- say("That is my name!").

:- reg_irc_hook((
                 on_irc_connect(_):- 
                   join("#foof_fun"),
                   say("I have arrived")
                )).

:- ensure_loaded(eggdrop_fun(jokes)). 
go:- irc_connect,install_hello.





