/* File irc_chat_client_foofy.pl   

:- pack_install(eggdrop). 

% Read README @ https://github.com/TeamSPoon/eggdrop

:- use_module(pack('eggdrop/t/foofy_bot')).

*/

:- module(foofy_bot,[go/0,wave/0]).

:- use_module(library(eggdrop))).

:- X is random(666),atom_concat('foof_',X,Nick),set_irc_nick(Nick).

:- set_irc_serv("irc.freenode.net":6667).

install_hello:- 
    reg_irc_hook(on_irc_msg(Channel,Nick,"hello"), 
              ( downcase_atom(Nick,DCNick), greet(DCNick,Channel) )).

greet(Nick,Channel):- say(["hello",Nick,"welcome to",Channel]).

irc_hooks:on_irc_msg(_, _,"bye"):- wave.  % confirm wave/0 is usable 

wave:- irc_action("waves").

irc_hooks:on_irc_msg(_, _,"foofy"):- say("That is my name!").

:- reg_irc_hook((
                 on_irc_connect(_):- 
                   join("#foof_fun"),
                   say("I have arrived")
                )).

:- if(source_exists(eggdrop_fun(jokes)).
:- ensure_loaded(eggdrop_fun(jokes)).
:- endif.

go:- irc_connect,install_hello.





