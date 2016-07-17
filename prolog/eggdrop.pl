:- if(exists_source(library(logicmoo_utils))).
:- if(( system:use_module(system:library('logicmoo/util/logicmoo_util_clause_expansion.pl')), push_modules)). 
:- endif.
:- endif.
:- module(eggdrop, [
add_maybe_static/2,bot_nick/1,call_in_thread/1,call_with_results/2,check_put_server_count/1,cit/0,close_ioe/3,consultation_codes/3,
  consultation_thread/2,ctcp/6,ctrl_nick/1,ctrl_pass/1,ctrl_port/1,ctrl_server/1,deregister_unsafe_preds/0,egg_go/0,
  egg_go_fg/0,eggdropConnect/0,eggdropConnect/2,eggdropConnect/4,eggdrop_bind_user_streams/0,escape_quotes/2,flush_all_output/0,flushed_privmsg/4,
  format_nv/2,get2react/1,get_session_prefix/1,ignore_catch/1,ignored_source/1,ircEvent/3,ircEvent_call/4,ircEvent_call_filtered/4,
  irc_receive/5,is_callable_egg/1,is_empty/1,is_lisp_call_functor/1,join/4,list_replace_egg/4,msgm/5,my_wdmsg/1,
  part/5,privmsg/3,privmsg_session/3,pubm/5,putnotice/3,read_codes_and_send/2,read_each_term_egg/3,read_from_agent_and_send/2,
  read_one_term_egg/3,recordlast/3,remove_anons/2,remove_pred_egg/3,say/1,say/2,say/3,say_list/3,
  say_list/4,say_prefixed/3,sayq/1,show_thread_exit/0,to_egg/1,to_egg/2,unreadable/1,unsafe_preds_egg/3,
  update_changed_files_eggdrop/0,vars_as_comma/0,vars_as_list/0,with_error_channel/2,with_error_to_output/1,with_input_channel_user/3,with_io/1,with_no_input/1,
  with_output_channel/2,with_resource_limit/1,egg:stdio/3,egg:vars_as/1,ignored_channel/1,chat_config:chat_isWith/2,chat_config:chat_isRegistered/3,last_read_from/3,
  chat_config:chat_isChannelUserAct/4,
  attvar_to_dict_egg/2,
  % dict_to_attvar_egg/2,
  format_nv/2
  % eggdrop_e:stream_close/1,eggdrop_e:stream_read/2,eggdrop_e:stream_write/2,eggdrop_io:stream_close/1,eggdrop_io:stream_read/2,
  % eggdrop_io:stream_write/2,t_l:put_server_count/1,t_l:put_server_no_max/0,t_l:session_id/1
  ]).
% restore entry state
:- reset_modules.

:- if((fail,exists_source(library(atts)))).
:- set_prolog_flag(metaterm,true).
:- use_module(library(atts)).
:- endif.

:- if(exists_source(library(logicmoo_utils))).
:- system:use_module(library(logicmoo_utils)).
:- endif.

:- set_prolog_flag(dialect_pfc,false).

:- meta_predicate 
        call_in_thread(0),
        with_dmsg_to_main_err(0),
        call_with_results_0(0, ?),
        call_with_results_2(0, ?),
        call_with_results_3(0, ?),
        ignore_catch(0),
        with_error_channel(+, 0),
        with_error_to_output(0),
        with_input_channel_user(+, +, 0),
        with_io(0),
        with_no_input(0),
        with_output_channel(+, 0),
        with_resource_limit(0).


/*:-module(eggdrop,[egg_go/0,ircEvent/3,call_with_results/2,close_ioe/3,
         with_error_channel/2,egg_go_fg/0,
         ignore_catch/1,
         call_in_thread/1,
         with_no_input/1,
         with_output_channel/2,
         with_error_channel/2,
         with_input_channel_user/3,
         say_prefixed/3,
         say/1,
   chat_config:chat_isRegistered/3]).*/


%% my_wdmsg( ?Msg) is semidet.
%
% My Wdmsg.
%
my_wdmsg(Msg):- string(Msg),format(user_error,'~N% ~s~N',[Msg]),flush_output(user_error),!.
my_wdmsg(Msg):- current_predicate(logicmoo_bugger_loaded/0),catch((cnotrace((get_main_error_stream(ERR), format(ERR,'~N% ~q.~N',[Msg]),flush_output(ERR)))),_,fail),!.
my_wdmsg(Msg):- format(user_error,'~N% ~q.~n',[Msg]),flush_output(user_error),!.

:-dynamic(chat_config:chat_isWith/2).
:-dynamic(chat_config:chat_isRegistered/3).
:- thread_local(t_l:(disable_px)).

:- my_wdmsg("HI there").

% :- autoload([verbose(false)]).

/*
:- (multifile t_l:put_server_count/1, t_l:put_server_no_max/0, t_l:session_id/1, lmconf:irc_event_hooks/3).
:- module_transparent ((add_maybe_static/2,bot_nick/1,call_in_thread/1,call_with_results/2,call_with_results_0/2,call_with_results_2/2,call_with_results_3/2,
  check_put_server_count/1,cit/0,cit2/0,cit3/0,close_ioe/3,consultation_codes/3,consultation_thread/2,ctcp/6,
  ctrl_nick/1,ctrl_pass/1,ctrl_port/1,ctrl_server/1,deregister_unsafe_preds/0,egg_go/0,egg_go_fg/0,eggdropConnect/0,
  eggdropConnect/2,eggdropConnect/4,eggdrop_bind_user_streams/0,escape_quotes/2,flush_all_output/0,flushed_privmsg/4,format_nv/2,get2react/1,
  get_session_prefix/1,ignore_catch/1,ignored_source/1,ircEvent/3,ircEvent_call/4,ircEvent_call_filtered/4,irc_receive/5,is_callable_egg/1,
  is_empty/1,is_lisp_call_functor/1,join/4,list_replace_egg/4,msgm/5,my_wdmsg/1,part/5,privmsg/3,
  privmsg0/3,privmsg1/3,privmsg2/3,privmsg_session/3,pubm/5,putnotice/3,read_codes_and_send/2,read_each_term_egg/3,
  read_from_agent_and_send/2,read_one_term_egg/3,recordlast/3,remove_anons/2,remove_pred_egg/3,say/1,say/2,say/3,
  say_list/3,say_list/4,say_prefixed/3,sayq/1,show_thread_exit/0,to_egg/1,to_egg/2,unreadable/1,
  unsafe_preds_egg/3,update_changed_files_eggdrop/0,vars_as_comma/0,vars_as_list/0,with_error_channel/2,with_error_to_output/1,with_input_channel_user/3,with_io/1,
  with_no_input/1,with_output_channel/2,with_resource_limit/1,write_varcommas2/1,write_varcommas3/1,write_varvalues2/1,write_varvalues3/1,ignored_channel/1)).



:- thread_local t_l: put_server_count/1, t_l:put_server_no_max/0, t_l:session_id/1.
:- export((call_with_results_2/2,call_with_results_3/2,cit2/0,cit3/0,privmsg0/3,privmsg1/3,
  privmsg2/3,write_varcommas2/1,write_varcommas3/1,write_varvalues2/1,write_varvalues3/1 ,call_with_results_0/2,call_with_results_2/2,call_with_results_3/2,
  cit2/0,cit3/0,privmsg0/3,privmsg1/3,privmsg2/3,write_varcommas2/1,write_varcommas3/1,write_varvalues2/1,write_varvalues3/1 )).
:- dynamic (( egg:stdio/3,egg:vars_as/1,ignored_channel/1,chat_config:chat_isWith/2,chat_config:chat_isRegistered/3,last_read_from/3,chat_config:chat_isChannelUserAct/4,lmconf:irc_event_hooks/3  )).
:- multifile (( egg:stdio/3,egg:vars_as/1,ignored_channel/1,chat_config:chat_isWith/2,chat_config:chat_isRegistered/3,last_read_from/3,chat_config:chat_isChannelUserAct/4,lmconf:irc_event_hooks/3  )).
:- (volatile egg:stdio/3, t_l:put_server_count/1, t_l:put_server_no_max/0, t_l:session_id/1).
*/

% ===================================================================
% IRC CONFIG
% ===================================================================

:- if(exists_file('.ircbot.pl')).
:- include('.ircbot.pl').
:- else.

%= 	 	 

%% bot_nick( ?PrologMUD1) is semidet.
%
% Bot Nick.
%
bot_nick("PrologMUD").

%= 	 	 

%% ctrl_server( ?Localhost1) is semidet.
%
% Ctrl Server.
%
ctrl_server(localhost).

%= 	 	 

%% ctrl_nick( ?Swipl1) is semidet.
%
% Ctrl Nick.
%
ctrl_nick("swipl").

%= 	 	 

%% ctrl_pass( ?Top5ecret1) is semidet.
%
% Ctrl Pass.
%
ctrl_pass("top5ecret").

%= 	 	 

%% ctrl_port( ?Port) is semidet.
%
% Ctrl Port.
%
ctrl_port(3334).

:- endif.



 :- meta_predicate call_with_results_3(0,*).
 :- meta_predicate call_with_results_2(0,*).
 :- meta_predicate call_with_results_0(0,*),with_resource_limit(0).

% :- use_module(library(logicmoo/util/logicmoo_util_prolog_streams),[with_output_to_stream_pred/4]).

:- module_transparent(ircEvent/3).
% from https://github.com/TeamSPoon/PrologMUD/tree/master/src_lib/logicmoo_util
% supplies w_tl/2,atom_concats/2, dmsg/1, my_wdmsg/1, must/1, if_startup_script/0
:- ensure_loaded(library(logicmoo_utils)).
:- use_module(library(resource_bounds)).
/*
my_wdmsg(List):-is_list(List),text_to_string(List,CMD),!,format(user_error,'~q~n',[CMD]),flush_output(user_error),!.
my_wdmsg(CMD):-format(user_error,'~q~n',[CMD]),flush_output(user_error),!.
*/
/*
TODO

* one may consider letting it wait until a query is completed with a `.'

* consider using numbervars/[3,4] <http://www.swi-prolog.org/pldoc/man?section=manipterm#numbervars/3>,
  <http://www.swi-prolog.org/pldoc/man?section=termrw#write_term/2> (option numbervars) 
   for printing the variables (named ones in query, and freshly generated ones)

* skip printing toplevel vars marked with "_" in  findall(_X, fact(_X), Xs).

* perhaps use "commalists" instead of ordinary lists for each solution ? (makes it look more like a traditional interactor reply, and looks more sensible, logically)

*/


%= 	 	 

%% chat_config:chat_isRegistered( ?Channel, ?Agent, ?Execute3) is semidet.
%
% If Is A Registered.
%
chat_config:chat_isRegistered(Channel,Agent,kifbot):-chat_config:chat_isWith(Channel,Agent).
chat_config:chat_isRegistered(_,"someluser",execute):-!,fail.
chat_config:chat_isRegistered("#ai",_,execute):-ignore(fail).
chat_config:chat_isRegistered("#pigface",_,execute):-ignore(fail).  % havent been there since 2001
chat_config:chat_isRegistered("#logicmoo",_,execute):-ignore(fail).
chat_config:chat_isRegistered("#kif",_,execute):-ignore(fail).
chat_config:chat_isRegistered("#rdfig",_,execute):-ignore(fail).
chat_config:chat_isRegistered("##prolog",_,execute):-!.
% all may execture since they are using ?-
chat_config:chat_isRegistered(_,_,execute):-!.

:- export((egg_go/0,ircEvent/3,call_with_results/2,chat_config:chat_isRegistered/3)).
% ===================================================================
% Deregister unsafe preds
% ===================================================================
:-use_module(library(process)).


%= 	 	 

%% unsafe_preds_egg( ?M, ?F, ?A) is semidet.
%
% Unsafe Predicates Egg.
%
unsafe_preds_egg(M,F,A):-M=files_ex,current_predicate(M:F/A),member(X,[delete,copy]),atom_contains(F,X).
%unsafe_preds_egg(M,F,A):-M=process,current_predicate(M:F/A),member(X,[kill,create]),atom_contains(F,X).
unsafe_preds_egg(M,F,A):-M=system,member(F,[shell,halt]),current_predicate(M:F/A).

:- export(remove_pred_egg/3).

%= 	 	 

%% remove_pred_egg( ?M, ?F, ?A) is semidet.
%
% Remove Predicate Egg.
%
remove_pred_egg(_,F,A):-member(_:F/A,[_:delete_common_prefix/4]),!.
remove_pred_egg(M,F,A):- functor(P,F,A),
  (current_predicate(M:F/A) -> ignore((catch(redefine_system_predicate(M:P),_,true),abolish(M:F,A)));true),
  M:asserta((P:-(my_wdmsg(error(call(P))),throw(permission_error(M:F/A))))).

% :-use_module(library(uid)).
% only if root

%= 	 	 

%% deregister_unsafe_preds is semidet.
%
% Deregister Unsafe Predicates.
%
deregister_unsafe_preds:-!.
deregister_unsafe_preds:-if_defined(getuid(0),true),forall(unsafe_preds_egg(M,F,A),remove_pred_egg(M,F,A)).
deregister_unsafe_preds:-!.

% [Optionaly] Solve the Halting problem
:-redefine_system_predicate(system:halt).
:-abolish(system:halt,0).

%= 	 	 

%% halt is semidet.
%
% Hook To [system:halt/0] For Module Eggdrop.
% Halt.
%
system:halt:- format('the halting problem is now solved!').


% :- deregister_unsafe_preds.
% ===================================================================
% Eggrop interaction
% ===================================================================

:- use_module(library(socket)).
:- volatile(egg:stdio/3).
:- dynamic(egg:stdio/3).

%= 	 	 

%% eggdropConnect is semidet.
%
% Eggdrop Connect.
%
eggdropConnect:- eggdropConnect(_Host,_Port,_CtrlNick,_Pass).

%= 	 	 

%% eggdropConnect( ?CtrlNick, ?Port) is semidet.
%
% Eggdrop Connect.
%
eggdropConnect(CtrlNick,Port):-eggdropConnect(_Host,Port,CtrlNick,_Pass).

%= 	 	 

%% eggdropConnect( ?Host, ?Port, ?CtrlNick, ?Pass) is semidet.
%
% Eggdrop Connect.
%
eggdropConnect(Host,Port,CtrlNick,Pass):-
       ignore(ctrl_server(Host)),
       ignore(ctrl_port(Port)),
       ignore(ctrl_nick(CtrlNick)),
       ignore(ctrl_pass(Pass)),   
       tcp_socket(SocketId),
       my_wdmsg(tcp_connect(SocketId,Host:Port)),
       tcp_connect(SocketId,Host:Port),
       tcp_open_socket(SocketId, IN, OutStream),
       format(OutStream,'~w\n',[CtrlNick]),flush_output(OutStream),
       format(OutStream,'~w\n',[Pass]),flush_output(OutStream),
       retractall(egg:stdio(CtrlNick,_,_)),
       asserta((egg:stdio(CtrlNick,IN,OutStream))),!.

:-export(consultation_thread/2).

%= 	 	 

%% consultation_thread( ?CtrlNick, ?Port) is semidet.
%
% Consultation Thread.
%
consultation_thread(CtrlNick,Port):- 
      eggdropConnect(CtrlNick,Port),
      to_egg('.echo off\n'),
      to_egg('.console ~w ""\n',[CtrlNick]),
      must(bot_nick(PrologMUDNick)),
      to_egg('.set nick ~w\n',[PrologMUDNick]),
      must(egg:stdio(CtrlNick,IN,_)),!,
      % loop
      repeat,
         % update_changed_files_eggdrop,
         catch(clpfd:read_line_to_codes(IN,Codes),_,Codes=end_of_file),    
         Codes\=end_of_file,
         once(consultation_codes(CtrlNick,Port,Codes)),
         fail.


%= 	 	 

%% is_callable_egg( ?CMD) is semidet.
%
% If Is A Callable Egg.
%
is_callable_egg(CMD):-var(CMD),!,fail.
is_callable_egg((A,B)):-!,is_callable_egg(A),is_callable_egg(B).
is_callable_egg((A;B)):-!,is_callable_egg(A),is_callable_egg(B).
is_callable_egg(CMD):- callable(CMD),
          functor(CMD,F,A),
          current_predicate(F/A),!.

%:-meta_predicate(module_call(+,0)).
%module_call(M,CMD):- CALL=M:call(CMD), '@'(catch(CALL,E,(my_wdmsg(E:CALL),throw(E))),M).
%:-meta_predicate(user_call(0)).
%user_call(M:CMD):-!,show_call(module_call(M,CMD)).
%user_call(CMD):-module_call('user',CMD).


%= 	 	 

%% consultation_codes( ?CtrlNick, ?Port, ?Codes) is semidet.
%
% Consultation Codes.
%
consultation_codes(CtrlNick,Port,end_of_file):-!,consultation_thread(CtrlNick,Port).
consultation_codes(_BotNick,_Port,Codes):-
      text_to_string(Codes,String),
      catch(read_term_from_atom(String,CMD,[]),_E,(my_wdmsg(String),!,fail)),!,      
      is_callable_egg(CMD),
      my_wdmsg(maybe_call(CMD)),!,
      catch(CMD,E,my_wdmsg(E:CMD)).


% IRC EVENTS Bubble from here
:- export(get2react/1).

%= 	 	 

%% get2react( ?ARG1) is semidet.
%
% Get2react.
%
get2react([L|IST1]):- CALL =.. [L|IST1],functor(CALL,F,A),show_failure((current_predicate(F/A),CALL)).

:- thread_local(t_l: session_id/1).

% ===================================================================
% IRC interaction
% ===================================================================
:- thread_local t_l:default_channel/1, t_l:default_user/1, t_l:current_irc_receive/5.
% IRC EVENTS FROM CALL

%= 	 	 

%% part( ?USER, ?HOSTMASK, ?TYPE, ?DEST, ?MESSAGE) is semidet.
%
% Part.
%
part(USER, HOSTMASK,TYPE,DEST,MESSAGE):- irc_receive(USER, HOSTMASK,TYPE,DEST,part(USER, HOSTMASK,TYPE,DEST,MESSAGE)).

%= 	 	 

%% join( ?USER, ?HOSTMASK, ?TYPE, ?DEST) is semidet.
%
% Join.
%
join(USER, HOSTMASK,TYPE,DEST):- irc_receive(USER, HOSTMASK,TYPE,DEST,join(USER, HOSTMASK,TYPE,DEST)).

%= 	 	 

%% msgm( ?USER, ?HOSTMASK, ?TYPE, ?DEST, ?MESSAGE) is semidet.
%
% Msgm.
%
msgm(USER, HOSTMASK,TYPE,DEST,MESSAGE):- irc_receive(USER, HOSTMASK,TYPE,DEST,say(MESSAGE)).

%= 	 	 

%% ctcp( ?USER, ?HOSTMASK, ?FROM, ?DEST, ?TYPE, ?MESSAGE) is semidet.
%
% Ctcp.
%
ctcp(USER, HOSTMASK,_FROM,DEST,TYPE,MESSAGE):- irc_receive(USER, HOSTMASK,TYPE,DEST,ctcp(TYPE,MESSAGE)).

%= 	 	 

%% pubm( ?USER, ?HOSTMASK, ?TYPE, ?DEST, ?MESSAGE) is semidet.
%
% Pubm.
%
pubm(USER, HOSTMASK,TYPE,DEST,MESSAGE):- irc_receive(USER, HOSTMASK,TYPE,DEST,say(MESSAGE)).



%= 	 	 

%% irc_receive( ?USER, ?HOSTMASK, ?TYPE, ?DEST, ?MESSAGE) is semidet.
%
% Irc Receive.
%
irc_receive(USER,HOSTMASK,TYPE,DEST,MESSAGE):- 
 my_wdmsg(irc_receive(USER,HOSTMASK,TYPE,DEST,MESSAGE)),!,
   string_to_atom(USER,ID),
   (call_in_thread((      
     w_tl([
       t_l:put_server_count(0),
       t_l:default_channel(DEST),       
       t_l:default_user(USER),
       t_l:session_id(ID),       
       t_l:current_irc_receive(USER, HOSTMASK,TYPE,DEST,MESSAGE)],
        with_resource_limit((eggdrop_bind_user_streams, ircEvent(DEST,USER,MESSAGE))))))).
       

%= 	 	 

%% with_resource_limit( :GoalCall) is semidet.
%
% Using Resource Limit.
%
with_resource_limit(Call):- thread_self(main),!,rtrace((guitracer,trace,Call)).
with_resource_limit(Call):- !,nodebugx(Call).
% with_resource_limit(Goal):- show_call(eggdrop,nodebugx(resource_bounded_call(Goal, 1000.0, _Status, []))).
with_resource_limit(Call):- nodebugx(call_with_time_limit(30,Call)).

% ===================================================================
% IRC EVENTS
% ===================================================================

:- dynamic(last_read_from/3).

% convert all to strings

%= 	 	 

%% ignored_source( ?From) is semidet.
%
% Ignored Source.
%
ignored_source(From):-var(From),!,fail.
ignored_source("yesbot").
ignored_source(From):-not(string(From)),!,text_to_string(From,String),!,ignored_source(String).
%  from bot telnet
ignored_source(From):- atom(From),atom_length(From,1).
% from the process or bot
ignored_source(From):-
 bot_nick(BotNick),ctrl_nick(CtrlNick),arg(_,vv(BotNick,CtrlNick),Ignore),atom_contains(From,Ignore),!.

:-dynamic(chat_config:chat_isChannelUserAct/4).
:-dynamic(ignored_channel/1).
:-dynamic(lmconf:irc_event_hooks/3).
:-multifile(lmconf:irc_event_hooks/3).


%= 	 	 

%% lmconf:irc_event_hooks( ?Channel, ?User, ?Stuff) is semidet.
%
% Hook To [lmconf:irc_event_hooks/3] For Module Eggdrop.
% Irc Event Hooks.
%
lmconf:irc_event_hooks(_Channel,_User,_Stuff):-fail.

with_dmsg_to_main_err(G):-!,call(G).
with_dmsg_to_main_err(G):-with_dmsg_to_main(G).


%= 	 	 

%% recordlast( ?Channel, ?User, ?What) is semidet.
%
% Recordlast.
%
recordlast(Channel,User,say(What)):-!,retractall(chat_config:chat_isChannelUserAct(Channel,User,say,_)),asserta(chat_config:chat_isChannelUserAct(Channel,User,say,What)),!.
recordlast(Channel,User,What):-functor(What,F,_),retractall(chat_config:chat_isChannelUserAct(Channel,User,F,_)),asserta(chat_config:chat_isChannelUserAct(Channel,User,F,What)),!.

% awaiting some inputs

%= 	 	 

%% ircEvent( ?DEST, ?User, ?Event) is semidet.
%
% Irc Event.
%
ircEvent(DEST,User,say(W)):- 
 term_to_atom(cu(DEST,User),QUEUE),
   message_queue_property(_, alias(QUEUE)),
     show_call(ircEvent,thread_send_message(QUEUE,W)).

% ignore some inputs
ircEvent(Channel,Agent,_):- (ignored_channel(Channel) ; ignored_source(Agent)) ,!.

% attention (notice the fail to disable)
ircEvent(Channel,Agent,say(W)):- fail,
               atom_contains(W,"goodbye"),!,retractall(chat_config:chat_isWith(Channel,Agent)).
ircEvent(Channel,Agent,say(W)):- fail,
               (bot_nick(BotNick),atom_contains(W,BotNick)),
		retractall(chat_config:chat_isWith(Channel,Agent)),!,
		asserta(chat_config:chat_isWith(Channel,Agent)),!,
		say(Channel,[hi,Agent,'I will answer you in',Channel,'until you say "goodbye"']).


ircEvent(Channel,Agent,Event):-doall(call_no_cuts(lmconf:irc_event_hooks(Channel,Agent,Event))),fail.

% Say -> Call
ircEvent(Channel,Agent,say(W)):- 
 with_dmsg_to_main_err((
   forall(eggdrop:read_each_term_egg(W,CMD,Vs),ircEvent(Channel,Agent,call(CMD,Vs))))),!.

% Call -> call_with_results
ircEvent(Channel,Agent,call(CALL,Vs)):- 
 with_dmsg_to_main_err((
  thread_self(Self),tnodebug(Self),
  use_agent_module(Agent),!,
  hotrace(ircEvent_call_filtered(Channel,Agent,CALL,Vs)),
  save_agent_module(Agent))),!.

ircEvent(Channel,User,Method):-recordlast(Channel,User,Method), my_wdmsg(unused(ircEvent(Channel,User,Method))).

:- dynamic(chat_config:chat_isModule/3).

:- module_transparent(use_agent_module/1).
:- module_transparent(save_agent_module/1).
use_agent_module(AgentS):- any_to_atom(AgentS,Agent),source_and_module_for_agent(Agent,Module,CallModule),!,'$set_source_module'(Module),'$set_typein_module'(CallModule).
save_agent_module(AgentS):- any_to_atom(AgentS,Agent), retractall(chat_config:chat_isModule(Agent,_)), '$set_source_module'(Next,Next),'$module'(CallModule,CallModule),asserta(chat_config:chat_isModule(Agent,Next,CallModule)).
source_and_module_for_agent(Agent,Module,CallModule):- chat_config:chat_isModule(Agent,Module,CallModule),!.
source_and_module_for_agent(Agent,Agent,user):- maybe_add_import_module(Agent,user,end), maybe_add_import_module(Agent,eggdrop,end).


:-export(unreadable/1).

%= 	 	 

%% unreadable( ?UR) is semidet.
%
% Unreadable.
%
unreadable(UR):-my_wdmsg(unreadable(UR)).

:-export(read_each_term_egg/3).
:-module_transparent(read_each_term_egg/3).

%= 	 	 

%% read_each_term_egg( ?S, ?CMD, ?Vs) is semidet.
%
% Read Each Term Egg.
%
read_each_term_egg(S,CMD,Vs):-   
  show_failure(( l_open_input(S,Stream),  
      findall(CMD-Vs,(
       repeat,
       read_one_term_egg(Stream,CMD,Vs),
       (CMD==end_of_file->!;true)),Results),!,
  ((member(CMD-Vs,Results),CMD\==end_of_file)*->true;read_one_term_egg(S,CMD,Vs)))).

%:- ensure_loaded(library(logicmoo/snark/common_logic_sexpr)).
%:- set_file_lang(pl).
:- user:ensure_loaded(library(clpfd)).




:-export(eggdrop_bind_user_streams/0).


%= 	 	 

%% eggdrop_bind_user_streams is semidet.
%
% Eggdrop Bind User Streams.
%
eggdrop_bind_user_streams :- thread_self(main),!.
eggdrop_bind_user_streams :-
  user:((	
	user:open_prolog_stream(eggdrop_io, write, Out, []),
        user:open_prolog_stream(eggdrop_e, write, Err, []),
	set_stream(Out, buffer(line)),
        set_stream(Err, buffer(line)),
	open_prolog_stream(eggdrop_io, read,  In, []),
        set_input(In),
        set_output(Out),
/*        set_stream(In,  alias(user_input)),
        set_stream(Out, alias(user_output)),
        set_stream(Err, alias(user_error)),
	set_stream(In,  alias(current_input)),
        set_stream(Out, alias(current_output)),
        set_stream(Err, alias(current_error)),
  */      
	thread_at_exit(eggdrop:close_ioe(In, Out, Err)))).

:- use_module(library(pengines)).


:- meta_predicate with_error_channel(+,0).
:- meta_predicate ignore_catch(0).
:- meta_predicate call_in_thread(0).
:- meta_predicate with_no_input(0).
:- meta_predicate with_output_channel(+,0).
:- meta_predicate with_input_channel_user(+,+,0).




%= 	 	 

%% stream_write( ?Stream, ?Out) is semidet.
%
% Hook To [eggdrop_e:stream_write/2] For Module Eggdrop.
% Stream Write.
%
eggdrop_io:stream_write(_Stream, Out) :- t_l:default_channel(RETURN),say(RETURN,Out).

%= 	 	 

%% stream_read( ?Stream, ?Data) is semidet.
%
% Hook To [eggdrop_e:stream_read/2] For Module Eggdrop.
% Stream Read.
%
eggdrop_io:stream_read(_Stream, "") :- !.
eggdrop_io:stream_read(_Stream, Data) :- prompt(Prompt, Prompt), pengines:pengine_input(_{type:console, prompt:Prompt}, Data).

%= 	 	 

%% stream_close( ?Stream) is semidet.
%
% Hook To [eggdrop_e:stream_close/1] For Module Eggdrop.
% Stream Close.
%
eggdrop_io:stream_close(_Stream).

eggdrop_e:stream_write(_Stream, Out) :- t_l:default_channel(RETURN),say(RETURN,Out).
eggdrop_e:stream_read(_Stream, "") :- !.
eggdrop_e:stream_read(_Stream, Data) :- prompt(Prompt, Prompt), pengines:pengine_input(_{type:console, prompt:Prompt}, Data).
eggdrop_e:stream_close(_Stream).


%= 	 	 

%% close_ioe( ?In, ?Out, ?Err) is semidet.
%
% Close Ioe.
%
close_ioe(In, Out, Err) :-
	close(In, [force(true)]),
        close(Err, [force(true)]),
	close(Out, [force(true)]).




:-export(read_one_term_egg/3).
:-module_transparent(read_one_term_egg/3).

%= 	 	 

%% read_one_term_egg( ?Stream, ?CMD, ?Vs) is semidet.
%
% Read One Term Egg.
%
read_one_term_egg(Stream,CMD,Vs):- \+ is_stream(Stream),l_open_input(Stream,InStream),!, 
       with_stream_pos(InStream,show_entry(read_one_term_egg(InStream,CMD,Vs))).
read_one_term_egg(Stream,CMD,_ ):- at_end_of_stream(Stream),!,CMD=end_of_file,!.
% read_one_term_egg(Stream,CMD,Vs):- catch((input_to_forms(Stream,CMD,Vs)),_,fail),CMD\==end_of_file,!.
read_one_term_egg(Stream,CMD,Vs):- catch((read_term(Stream,CMD,[double_quotes(string),module(clpfd),variable_names(Vs)])),_,fail),CMD\==end_of_file,!.
read_one_term_egg(Stream,unreadable(String),_):-catch((read_stream_to_codes(Stream,Codes),string_codes(String,Codes)),_,fail),!.
read_one_term_egg(Stream,unreadable(String),_):-catch((read_pending_input(Stream,Codes,[]),string_codes(String,Codes)),_,fail),!.


:-export(add_maybe_static/2).

%= 	 	 

%% add_maybe_static( ?H, ?Vs) is semidet.
%
% Add Maybe Static.
%
add_maybe_static( H,Vs):- H \= (_:-_), !,add_maybe_static((H:-true),Vs).
add_maybe_static((H:-B),_Vs):- predicate_property(H,dynamic),!,assertz(((H:-B))).
add_maybe_static((H:-B),_Vs):- must_det_l((convert_to_dynamic(H),assertz(((H:-B))),functor(H,F,A),compile_predicates([F/A]))).

% ===================================================================
% IRC CALL/1
% ===================================================================
:-module_transparent(ircEvent_call_filtered/4).
:-export(ircEvent_call_filtered/4).

%= 	 	 

%% ircEvent_call_filtered( ?Channel, ?Agent, ?CALL, ?Vs) is semidet.
%
% Irc Event Call Filtered.
%
ircEvent_call_filtered(_Channel,_Agent,CALL,_Vs):-var(CALL),!.
ircEvent_call_filtered(_Channel,_Agent,end_of_file,_Vs):-!.
ircEvent_call_filtered(_Channel,_Agent,(H :- B ),Vs):- add_maybe_static((H :- B),Vs),!.
ircEvent_call_filtered(Channel,Agent,((=>(H)) :- B ),Vs):- 
  ((=>(H :- B)) \== ((=>(H)) :- B )),!,ircEvent_call_filtered(Channel,Agent,(=>(H :- B)),Vs).
ircEvent_call_filtered(Channel,Agent,'?-'(CALL),Vs):- nonvar(CALL),!,ircEvent_call(Channel,Agent,CALL,Vs),!.
ircEvent_call_filtered(Channel,Agent,'=>'(CALL),Vs):- nonvar(CALL),!,ircEvent_call(Channel,Agent,ain(CALL),Vs),!.
ircEvent_call_filtered(Channel,Agent,[S|TERM],Vs):- is_list([S|TERM]),is_lisp_call_functor(S),!,
   (current_predicate(lisp_call/3) -> ircEvent_call(Channel,Agent,lisp_call([S|TERM],Vs,R),['Result'=R|Vs]);
     my_wdmsg(cant_ircEvent_call_filtered(Channel,Agent,[S|TERM],Vs))).
ircEvent_call_filtered(Channel,Agent,CALL,Vs):- chat_config:chat_isRegistered(Channel,Agent,executeAll),!,ircEvent_call(Channel,Agent,CALL,Vs),!.
ircEvent_call_filtered(Channel,Agent,CALL,Vs):- my_wdmsg(unused_ircEvent_call_filtered(Channel,Agent,CALL,Vs)),!. 


%= 	 	 

%% is_lisp_call_functor( ?FUNCTOR) is semidet.
%
% If Is A Lisp Call Functor.
%
is_lisp_call_functor('?-').
is_lisp_call_functor('?>').


:-module_transparent(ircEvent_call/4).
:-export(ircEvent_call/4).

%= 	 	 

%% ircEvent_call( ?Channel, ?Agent, ?CALL, ?Vs) is semidet.
%
% Irc Event Call.
%
ircEvent_call(Channel,Agent,CALL,Vs):-  fail,
 my_wdmsg(cdo_ircEvent_call(Channel,Agent,CALL,Vs)),
 call_cleanup(call_with_results(CALL,Vs),flush_output).

ircEvent_call(Channel,Agent,CALL,Vs):-  
 my_wdmsg(do_ircEvent_call(Channel,Agent,CALL,Vs)),
  % debug(_),
  % gtrace,  
  with_output_channel(Channel,
     with_error_channel(Channel,
       ((stream_property(X,alias(current_output)),set_stream(X,alias(user_output))),
         with_no_input(catch(call_with_results(CALL,Vs),E,(((say(Agent,[Channel,': ',E])),fail))))))),!.


%= 	 	 

%% cit is semidet.
%
% Cit.
%
cit:- get_time(HH), call_in_thread(with_error_channel(dmiles:err,writeln(user_error,HH))).

%= 	 	 

%% cit2 is semidet.
%
% Cit Extended Helper.
%
cit2:- get_time(HH), rtrace(with_error_channel(dmiles:err,writeln(user_error,HH))).

%= 	 	 

%% cit3 is semidet.
%
% Cit3.
%
cit3:- get_time(HH), writeln(user_error,HH).




%= 	 	 

%% call_in_thread( :GoalCMD) is semidet.
%
% Call In Thread.
%
call_in_thread(CMD):- thread_self(main),!,CMD.
% call_in_thread(CMD):- !,CMD.
call_in_thread(CMD):- thread_create(CMD,_,[detached(true)]).
call_in_thread(CMD):- thread_self(Self),thread_create(CMD,_,[detached(true),inherit_from(Self)]).


:- dynamic(egg:vars_as/1).
% :- thread_local egg:vars_as/1.

%= 	 	 

%% vars_as( ?VarType) is semidet.
%
% Hook To [egg:vars_as/1] For Module Eggdrop.
% Variables Converted To.
%
egg:vars_as(comma).

:-export(flush_all_output/0).

%= 	 	 

%% flush_all_output is semidet.
%
% Flush All Output.
%
flush_all_output:- flush_output(user_error),flush_output.

:-export(vars_as_list/0).

%= 	 	 

%% vars_as_list is semidet.
%
% Variables Converted To List.
%
vars_as_list :- retractall(egg:vars_as(_)),asserta(egg:vars_as(list)).
:-export(vars_as_comma/0).

%= 	 	 

%% vars_as_comma is semidet.
%
% Variables Converted To Comma.
%
vars_as_comma :- retractall(egg:vars_as(_)),asserta(egg:vars_as(comma)).


attvar_to_dict_egg(AttVar,Dict):-
   get_attrs(AttVar,Att3s),
   attrs_to_pairs(Att3s,DictPairs),
   dict_create(Dict,AttVar,DictPairs).

attrs_to_pairs(att(N,V,Att3s),[N=V|DictPairs]):-!,attrs_to_pairs(Att3s,DictPairs).
attrs_to_pairs(DictPairs,DictPairs).
/*
dict_to_attvar_egg(Dict):- dict_to_attvar_egg(Dict,_),!.
dict_to_attvar_egg(_:Dict,Out):- \+ compound(Dict),!,Out=Dict.
dict_to_attvar_egg(Mod:Dict,Out):- 
   is_dict(Dict),dict_pairs(Dict,M,Pairs),
   (atom(M)->atts_put(+,Out,M,Pairs);
   (var(M)-> (M=Out,put_atts(Out,Mod:Pairs)))),!.
dict_to_attvar_egg(Mod:Dict,Out):- 
  compound_name_arguments(Dict,F,Args),
   maplist(Mod:dict_to_attvar_egg,Args,ArgsO),!,
   compound_name_arguments(Out,F,ArgsO).
*/

%= 	 	 

%% format_nv( ?N, ?V) is semidet.
%
% Format Nv.
%
format_nv(N,V):- format('~w=',[N]),write_v(V).

write_v(V):- attvar(V),if_defined(attvar_to_dict_egg(V,Dict),fail),writeq(Dict),!.
write_v(V):- var(V),(var_property(V,name(EN))->write(EN);writeq(V)),!.
write_v(V):- writeq(V).

:-export(write_varvalues2/1).

%= 	 	 

%% write_varvalues2( ?Vs) is semidet.
%
% Write Varvalues Extended Helper.
%
write_varvalues2(Vs):-egg:vars_as(comma),!,write_varcommas2(Vs).
write_varvalues2(Vs):-write('['),copy_term(Vs,CVs),numbervars(CVs,6667,_,[singletons(true),attvar(skip)]),write_varvalues3(CVs).

%= 	 	 

%% write_varvalues3( ?ARG1) is semidet.
%
% Write Varvalues3.
%
write_varvalues3([N=V]):- format_nv(N,V), write(']'),!,flush_all_output.
write_varvalues3([N=V|Vs]):-format_nv(N,V),write(','),write_varvalues3(Vs),!,flush_all_output.



%= 	 	 

%% write_varcommas2( ?Vs) is semidet.
%
% Write Varcommas Extended Helper.
%
write_varcommas2(Vs):- copy_term(Vs,CVs),numbervars(CVs,6667,_,[singletons(true),attvar(skip)]),write_varcommas3(CVs).

%= 	 	 

%% write_varcommas3( ?ARG1) is semidet.
%
% Write Varcommas3.
%
write_varcommas3([N=V]):-format_nv(N,V),!.
write_varcommas3([N=V|Vs]):-format_nv(N,V), write(','),!,write_varcommas3(Vs),!.



:-export(remove_anons/2).

%= 	 	 

%% remove_anons( ?ARG1, ?VsRA) is semidet.
%
% Remove Anons.
%
remove_anons([],[]).
remove_anons([N=_|Vs],VsRA):-atom_concat('_',_,N),!,remove_anons(Vs,VsRA).
remove_anons([N=V|Vs],[N=V|VsRA]):-remove_anons(Vs,VsRA).

:-module_transparent(call_with_results/2).
:-export(call_with_results/2).

%= 	 	 

%% call_with_results( ?CMDI, ?Vs) is semidet.
%
% Call Using Results.
%
call_with_results(CMDI,Vs):- remove_anons(Vs,VsRA),!,
 ignore((
 nodebugx((w_tl(t_l:disable_px,((
  user:expand_term(CMDI,CMDG),
   user:expand_goal(CMDG,CMD)))),
    (CMD==CMDI->true;my_wdmsg(call_with_results(CMDI->CMD))),
    show_call(call_with_results_0(CMD,VsRA)))))),!.

:-module_transparent(call_with_results_0/2).
:-export(call_with_results_0/2).

%= 	 	 

%% call_with_results_0( :GoalCMD, ?Vs) is semidet.
%
% call Using results  Primary Helper.
%
call_with_results_0(CMD,Vs):- 
 set_varname_list( Vs),
 flag(num_sols,_,0),
 (call_with_results_2(CMD,Vs) *-> 
  (deterministic(X),flag(num_sols,N,0),(N\==0->YN='Yes';YN='No'), write(' '),(X=true->write(det(YN,N));write(nondet(YN,N)))) ;
     (deterministic(X),flag(num_sols,N,0),(N\==0->YN='Yes';YN='No'),write(' '),(X=true->write(det(YN,N));write(nondet(YN,N))))).



:-module_transparent(call_with_results_2/2).
:-export(call_with_results_2/2).

%= 	 	 

%% call_with_results_2( :GoalCMDIN, ?Vs) is semidet.
%
% call Using results  Extended Helper.
%
call_with_results_2(CMDIN,Vs):-  
   CMDIN = CMD,functor_h(CMD,F,A),A2 is A+1,CMD=..[F|ARGS],atom_concat(F,'_with_vars',FF),
   (current_predicate(FF/A2)-> (CMDWV=..[FF|ARGS],append_term(CMDWV,Vs,CCMD)); CCMD=CMD),!,
   call_with_results_3(CCMD,Vs).
call_with_results_2(CCMD,Vs):- call_with_results_3(CCMD,Vs).

:-module_transparent(call_with_results_3/2).
:-export(call_with_results_3/2).

%= 	 	 

%% call_with_results_3( :GoalCCMD, ?Vs) is semidet.
%
% Call Using Results Helper Number 3..
%
call_with_results_3(CCMD,Vs):- 
   show_call((user:CCMD,flush_output)), flag(num_sols,N,N+1), deterministic(Done),
     (once((Done==true -> (once(write_varvalues2(Vs)),write('. ')) ; (once(write_varvalues2(Vs)),write('; '),N>28)))).

:-export(with_output_channel/2).
:-module_transparent(with_output_channel(+,0)).
% with_output_channel(Channel,CMD):- CMD.

%= 	 	 

%% with_output_channel( +Channel, :GoalCMD) is semidet.
%
% Using Output Channel.
%
with_output_channel(Channel,CMD):- 
  with_output_to_pred(say(Channel),CMD).



%= 	 	 

%% with_input_channel_user( +Channel, +User, :GoalCMD) is semidet.
%
% Using Input Channel User.
%
with_input_channel_user(_,_,CMD):- !, with_no_input(CMD).
with_input_channel_user(Channel,User,CMD):- 
  with_input_from_pred(last_read_from(Channel,User),CMD).

:-export(with_io/1).
:-meta_predicate(with_io(0)).

%= 	 	 

%% with_io( :GoalCMD) is semidet.
%
% Using Input/output.
%
with_io(CMD):-
 with_dmsg_to_main_err((
  current_input(IN),current_output(OUT),get_thread_current_error(Err),  
  setup_call_cleanup(set_prolog_IO(IN,OUT,Err),CMD,(set_input(IN),set_output(OUT),set_error_stream(Err))))).

%with_no_input(CMD):-  current_input(Prev), open_chars_stream([e,n,d,'_',o,f,'_',f,i,l,e,'.'],In),set_input(In),!,call_cleanup(CMD,set_input(Prev)).
% with_no_input(CMD):- open_chars_stream([e,n,d,'_',o,f,'_',f,i,l,e,'.'],In),current_output(OUT), set_prolog_IO(In,OUT,user_error ),CMD.

%= 	 	 

%% with_no_input( :GoalCMD) is semidet.
%
% Using No Input.
%
with_no_input(CMD):- CMD.




%= 	 	 

%% ignore_catch( :GoalCALL) is semidet.
%
% Ignore Catch.
%
ignore_catch(CALL):-ignore(catch(CALL,E,my_wdmsg(E:CALL))).


:- meta_predicate with_error_to_output(0).

%= 	 	 

%% with_error_to_output( :GoalCMD) is semidet.
%
% Using Error Converted To Output.
%
with_error_to_output(CMD):- !, CMD.
with_error_to_output(CMD):- 
   current_input(IN),current_output(OUT),!,
   with_io((set_prolog_IO(IN,OUT,OUT), CMD)).

:- export(with_error_channel/2).
:- module_transparent(with_error_channel/2).


%= 	 	 

%% with_error_channel( +Agent, :GoalCMD) is semidet.
%
% Using Error Channel.
%
/*
with_error_channel(_Agent, CMD):-  !, CMD.
with_error_channel(Agent,CMD):- fail,
   current_input(IN),current_output(OUT),
   get_main_error_stream(MAINERROR),
   set_prolog_IO(IN,OUT,MAINERROR),
   new_memory_file(MF),
   open_memory_file(MF, write, ERR), 
   set_prolog_IO(IN,OUT,ERR),!,
   setup_call_cleanup_each(CMD,(ignore_catch(flush_output(ERR)),ignore_catch(close(ERR)),read_from_agent_and_send(Agent,MF))).
*/
with_error_channel(Agent, CMD):- !,  with_err_to_pred(say(Agent),CMD).
with_error_channel(_Agent, CMD):-  !, call(CMD).
with_error_channel(_Agent,CMD):- !, with_error_to_output(CMD).

          	


%= 	 	 

%% read_from_agent_and_send( ?Agent, ?MF) is semidet.
%
% Read Converted From Agent And Send.
%
read_from_agent_and_send(Agent,MF):- open_memory_file(MF, read, Stream,[ free_on_close(true)]),ignore_catch(read_codes_and_send(Stream,Agent)),ignore_catch(close(Stream)).


%= 	 	 

%% read_codes_and_send( ?IN, ?Agent) is semidet.
%
% Read Codes And Send.
%
read_codes_and_send(IN,Agent):- at_end_of_stream(IN),!,my_wdmsg(say(Agent,done)).
read_codes_and_send(IN,Agent):- repeat,read_line_to_string(IN,Codes),say(Agent,Codes),at_end_of_stream(IN),!.

%:-servantProcessCreate(killable,'Consultation Mode Test (KIFBOT!) OPN Server',consultation_thread(swipl,3334),Id,[]).


%= 	 	 

%% update_changed_files_eggdrop is semidet.
%
% Update Changed Files Eggdrop.
%
update_changed_files_eggdrop :-
 with_dmsg_to_main_err(( with_no_dmsg((
        set_prolog_flag(verbose_load,true),
        ensure_loaded(library(make)),
	findall(File, make:modified_file(File), Reload0),
	list_to_set(Reload0, Reload),
	(   prolog:make_hook(before, Reload)
	->  true
	;   true
	),
	print_message(silent, make(reload(Reload))),
	maplist(make:reload_file, Reload),
	print_message(silent, make(done(Reload))),
	(   prolog:make_hook(after, Reload)
	->  true
	;   
           true %list_undefined,list_void_declarations
	))))),!.

  
% ===================================================================
% IRC OUTPUT
% ===================================================================
:-export(sayq/1).

%= 	 	 

%% sayq( ?D) is semidet.
%
% Sayq.
%
sayq(D):-sformat(S,'~q',[D]),!,say(S),!.

:-export(say/1).

%= 	 	 

%% say( ?D) is semidet.
%
% Say.
%
say(D):- t_l:default_channel(C),say(C,D),!.
say(D):- say("#logicmoo",D),!.


%= 	 	 

%% say_prefixed( ?Agent, ?Agent, ?Out) is semidet.
%
% Say Prefixed.
%
say_prefixed(Agent,Agent,Out):- say(Agent,Out),!.
say_prefixed(Agent,Prefix,Out):-sformat(SF,'~p',[Out]), say(Agent:Prefix,SF),!.
say_prefixed(Agent,Prefix,Out):-sformat(SF,'~w: ~p',[Prefix,Out]), say(Agent,SF),!.

:- export(say/2).


%= 	 	 

%% say( ?Channel, ?Data) is semidet.
%
% Say.
%
say(Channel,[Channel,': '|Data]):-nonvar(Data),say(Channel,Data),!.
%say(C:C,Text):-nonvar(C),!,say(C,Text).
%say(C:A,Text):-!,say_prefixed(C,A,Text).
say(Channel,Data):-
	once(egg:stdio(_Agent,_InStream,OutStream);current_output(OutStream)),
	say(OutStream,Channel,Data),!.

:-export(say/3).

%= 	 	 

%% say( ?OutStream, ?NonList, ?Data) is semidet.
%
% Say.
%
say(_,NonList,Data):-is_stream(NonList),!,say(NonList,"console",Data),!.

%say(_OutStream,Channel,Text):- my_wdmsg(will_say(Channel,Text)),fail.
say(OutStream,NonList,Data):- \+(is_list(NonList)),text_to_string_safe(NonList, S),string_codes(S,Codes),!,say(OutStream,Codes,Data),!.
say(OutStream,Channel,Text):-
   any_to_string(Text,Data),
	concat_atom(List,'\n',Data),
	say_list(OutStream,Channel,List),!.

say(OutStream,Channel,Data):-
	say_list(OutStream,Channel,[Data]),!.

say(OutStream,Channel,Data):-my_wdmsg(say(OutStream,Channel,Data)),!.


%= 	 	 

%% get_session_prefix( ?ID) is semidet.
%
% Get Session Prefix.
%
get_session_prefix(ID):-t_l:session_id(ID),!.
get_session_prefix(ID):-t_l:default_user(ID),!.
get_session_prefix('').

% say_list(_OutStream,Channel,Text):-my_wdmsg(say_list(Channel,Text)),fail.

%= 	 	 

%% say_list( ?OutStream, ?Channel, ?List) is semidet.
%
% Say List.
%
say_list(OutStream,Channel,List):-
  get_session_prefix(Prefix),!,
  say_list(OutStream,Channel,Prefix,List),!.


%= 	 	 

%% is_empty( ?A) is semidet.
%
% If Is A Empty.
%
is_empty(A):-any_to_string(A,S),string_length(S,0).


%= 	 	 

%% flushed_privmsg( ?OutStream, ?Channel, ?Fmt, ?Args) is semidet.
%
% Flushed Privmsg.
%
flushed_privmsg(OutStream,Channel,Fmt,Args):- 
  format(string(NS),Fmt,Args),
  privmsg(OutStream,Channel,NS),!,
  catch(flush_output(OutStream),_,true).


%= 	 	 

%% say_list( ?OutStream, ?Channel, ?ID, ?List) is semidet.
%
% Say List.
%
say_list(OutStream,Channel:Prefix,_ID,List):-nonvar(Channel),!,say_list(OutStream,Channel,Prefix,List).


say_list(_OutStream,_Channel,_Prefix,[]).

say_list(_OutStream,_Channel,_Prefix,['']):- !.
say_list(OutStream,Channel,Prefix,[S,'']):-!, say_list(OutStream,Channel,Prefix,[S]).
% say_list(OutStream,Channel,Prefix,[S|L]):- my_wdmsg(say_list(OutStream,Channel,Prefix,[S|L])),fail.
say_list(OutStream,Channel,Prefix,[S|L]):- atom_string(N,S),atom_concat('\t',Front,N),atom_concat('   ',Front,NEW),!,say_list(OutStream,Channel,Prefix,[NEW|L]).

say_list(OutStream,Channel,Prefix,[N|L]):- any_to_string(Prefix,S),any_to_string(Channel,S),!,
        flushed_privmsg(OutStream,Channel,'~w',[N]),        
	say_list(OutStream,Channel,Prefix,L),!.
say_list(OutStream,Channel,Prefix,[N|L]):-!,
   flushed_privmsg(OutStream,Channel,'~w: ~w',[Prefix,N]),
	say_list(OutStream,Channel,Prefix,L),!.

:-thread_local t_l: put_server_count/1.
:-thread_local t_l: put_server_no_max/0.


%= 	 	 

%% put_server_count( :GoalGOAL1) is semidet.
%
% Hook To [t_l:put_server_count/1] For Module Eggdrop.
% Put Server Count.
%
t_l:put_server_count(0).


%= 	 	 

%% check_put_server_count( ?Max) is semidet.
%
% Check Put Server Count.
%
check_put_server_count(0):- if_defined(t_l:put_server_no_max),retractall(t_l:put_server_count(_)),asserta(t_l:put_server_count(0)).
check_put_server_count(Max):-retract(t_l:put_server_count(Was)),Is is Was+1,asserta(t_l:put_server_count(Is)),!,Is =< Max.
% 

%= 	 	 

%% privmsg( ?OutStream, ?Channel, ?Text) is semidet.
%
% Privmsg.
%


privmsg(OutStream,Channel,Text):- string_codes(Text,Codes),privmsg0(OutStream,Channel,Codes).


%= 	 	 

%% privmsg0( ?OutStream, ?Channel, ?Codes) is semidet.
%
% Privmsg Primary Helper.
%
privmsg0(OutStream,Channel,Codes):-length(Codes,Len),Len>430,length(LCodes,430),append(LCodes,RCodes,Codes),!,
   privmsg1(OutStream,Channel,LCodes),!,privmsg0(OutStream,Channel,RCodes).
privmsg0(OutStream,Channel,Codes):-privmsg1(OutStream,Channel,Codes).


%= 	 	 

%% privmsg1( ?OutStream, ?Channel, ?Text) is semidet.
%
% Privmsg Secondary Helper.
%
privmsg1(OutStream,Channel,Text):-check_put_server_count(30)->privmsg2(OutStream,Channel,Text);ignore(check_put_server_count(100)->privmsg_session(OutStream,Channel,Text);true).



%= 	 	 

%% privmsg2( ?OutStream, ?Channel, ?Text) is semidet.
%
% Privmsg Extended Helper.
%
privmsg2(OutStream,Channel:_,Text):-nonvar(Channel),!,privmsg2(OutStream,Channel,Text).
privmsg2(OutStream,_:Channel,Text):-nonvar(Channel),!,privmsg2(OutStream,Channel,Text).
privmsg2(OutStream,Channel,Text):- sleep(0.2),escape_quotes(Text,N),!,show_call(on_f_log_ignore(format(OutStream,'\n.tcl putquick "PRIVMSG ~s :~s"\n',[Channel,N]))),!.
% privmsg2(OutStream,Channel,Text):-on_f_log_ignore(format(OutStream,'\n.msg ~s ~s\n',[Channel,Text])).

% privmsg2(OutStream,Channel,Text):- escape_quotes(Text,N),ignore(catch(format(OutStream,'\n.tcl putserv "PRIVMSG ~s :~s" ;  return "noerror ."\n',[Channel,N]),_,fail)),!.


%= 	 	 

%% putnotice( ?OutStream, ?Channel, ?Text) is semidet.
%
% Putnotice.
%
putnotice(OutStream,Channel,Text):-escape_quotes(Text,N),ignore(catch(format(OutStream,'\n.tcl putserv "NOTICE ~s :~w"\n',[Channel,N]),_,fail)),!.


%= 	 	 

%% privmsg_session( ?OutStream, ?Channel, ?Text) is semidet.
%
% Privmsg Session.
%
privmsg_session(OutStream,Channel,Text):- t_l:session_id(ID),(ID==Channel->privmsg2(OutStream,Channel,Text);privmsg2(OutStream,ID,Text)).



%= 	 	 

%% to_egg( ?X) is semidet.
%
% Converted To Egg.
%
to_egg(X):-to_egg('~w',[X]),!.

%= 	 	 

%% to_egg( ?X, ?Y) is semidet.
%
% Converted To Egg.
%
to_egg(X,Y):-once(egg:stdio(_Agent,_InStream,OutStream)),once((sformat(S,X,Y),format(OutStream,'~s\n',[S]),!,flush_output(OutStream))).



%= 	 	 

%% escape_quotes( ?LIST, ?ISO) is semidet.
%
% Escape Quotes.
%
escape_quotes(LIST,ISO):-
           %dmsg(q(I)),
            %    term_string(I,IS),!,
		%string_to_list(IS,LIST),!,
		list_replace_egg(LIST,92,[92,92],LISTM),
		list_replace_egg(LISTM,34,[92,34],LISTM2),
                list_replace_egg(LISTM2,91,[92,91],LIST3),
                list_replace_egg(LIST3,36,[92,36],LISTO),
                =(LISTO,ISO),!.
		% text_to_string(LISTO,ISO),!.


%= 	 	 

%% list_replace_egg( ?List, ?Char, ?Replace, ?NewList) is semidet.
%
% List Replace Egg.
%
list_replace_egg(List,Char,Replace,NewList):-
	append(Left,[Char|Right],List),
	append(Left,Replace,NewLeft),
	list_replace_egg(Right,Char,Replace,NewRight),
	append(NewLeft,NewRight,NewList),!.
list_replace_egg(List,_Char,_Replace,List):-!.


% ===================================================================
% Startup
% ===================================================================


%= 	 	 

%% show_thread_exit is semidet.
%
% Show Thread Exit.
%
show_thread_exit:- my_wdmsg(warn(eggdrop_show_thread_exit)).


%= 	 	 

%% egg_go_fg is semidet.
%
% Egg Go Fg.
%
egg_go_fg:-consultation_thread(swipl,3334).


%= 	 	 

%% egg_go is semidet.
%
% Egg Go.
%
egg_go:- 
 deregister_unsafe_preds,
 (thread_property(_,alias(egg_go)) -> 
         true; 
         thread_create(egg_go_fg,_,[alias(egg_go),detached(true),an_exit(show_thread_exit)])).

/*
:- source_location(S,_),forall(source_file(H,S),ignore(( ( \+predicate_property(H,PP),member(PP,[(multifile),built_in]) ),  
 functor(H,F,A),module_transparent(F/A),export(F/A),user:import(H)))).
*/


  
% :-asserta(lmconf:irc_user_plays(_,dmiles,dmiles)).

% :- if_startup_script -> egg_go ; true.



:- module_transparent((egg_go)/0).
:- module_transparent((egg_go_fg)/0).
:- module_transparent((show_thread_exit)/0).
:- module_transparent((list_replace_egg)/4).
:- module_transparent((escape_quotes)/2).
:- module_transparent((to_egg)/2).
:- module_transparent((to_egg)/1).
:- module_transparent((privmsg_session)/3).
:- module_transparent((putnotice)/3).
:- module_transparent((privmsg2)/3).
:- module_transparent((privmsg1)/3).
:- module_transparent((privmsg0)/3).
:- module_transparent((privmsg)/3).
:- module_transparent((check_put_server_count)/1).
:- module_transparent((say_list)/4).
:- module_transparent((flushed_privmsg)/4).
:- module_transparent((is_empty)/1).
:- module_transparent((say_list)/3).
:- module_transparent((get_session_prefix)/1).
:- module_transparent((say)/3).
:- module_transparent((say)/2).
:- module_transparent((say_prefixed)/3).
:- module_transparent((say)/1).
:- module_transparent((sayq)/1).
:- module_transparent((update_changed_files_eggdrop)/0).
:- module_transparent((read_codes_and_send)/2).
:- module_transparent((read_from_agent_and_send)/2).
:- module_transparent((remove_anons)/2).
:- module_transparent((write_varcommas3)/1).
:- module_transparent((write_varcommas2)/1).
:- module_transparent((write_varvalues3)/1).
:- module_transparent((write_varvalues2)/1).
:- module_transparent((format_nv)/2).
:- module_transparent((vars_as_comma)/0).
:- module_transparent((vars_as_list)/0).
:- module_transparent((flush_all_output)/0).
:- module_transparent((cit3)/0).
:- module_transparent((cit2)/0).
:- module_transparent((cit)/0).
:- module_transparent((is_lisp_call_functor)/1).
:- module_transparent((add_maybe_static)/2).
:- module_transparent((close_ioe)/3).
:- module_transparent((eggdrop_bind_user_streams)/0).
:- module_transparent((unreadable)/1).
:- module_transparent((recordlast)/3).
:- module_transparent((ignored_channel)/1).
:- module_transparent((ignored_source)/1).
:- module_transparent((last_read_from)/3).
:- module_transparent((irc_receive)/5).
:- module_transparent((pubm)/5).
:- module_transparent((ctcp)/6).
:- module_transparent((msgm)/5).
:- module_transparent((join)/4).
:- module_transparent((part)/5).
:- module_transparent((get2react)/1).
:- module_transparent((consultation_codes)/3).
:- module_transparent((is_callable_egg)/1).
:- module_transparent((consultation_thread)/2).
:- module_transparent((eggdropConnect)/4).
:- module_transparent((eggdropConnect)/2).
:- module_transparent((eggdropConnect)/0).
:- module_transparent((deregister_unsafe_preds)/0).
:- module_transparent((remove_pred_egg)/3).
:- module_transparent((unsafe_preds_egg)/3).
:- module_transparent((my_wdmsg)/1).
:- module_transparent((ctrl_port)/1).
:- module_transparent((ctrl_pass)/1).
:- module_transparent((ctrl_nick)/1).
:- module_transparent((ctrl_server)/1).
:- module_transparent((bot_nick)/1).
:- module_transparent((write_v)/1).
:- module_transparent((source_and_module_for_agent)/3).
:- module_transparent((with_resource_limit)/1).
:- module_transparent((with_no_input)/1).
:- module_transparent((with_io)/1).
:- module_transparent((with_input_channel_user)/3).
:- module_transparent((with_error_to_output)/1).
:- module_transparent((ignore_catch)/1).
:- module_transparent((call_in_thread)/1).

:- source_location(S,_),prolog_load_context(module,M),
 forall(source_file(M:H,S),ignore((functor(H,F,A),
   nop((\+ mpred_database_term(F,A,_))),
   F\=='$mode',
   F\=='$pldoc',
   ignore(((\+ atom_concat('$',_,F),export(F/A)))),
   \+ predicate_property(M:H,transparent),
   ignore(((\+ atom_concat('__aux',_,F),format('~N:- module_transparent((~q)/~q).~n',[F,A])))),
   M:multifile(M:F/A),
   M:module_transparent(M:F/A)))).

% :- ircEvent("dmiles","dmiles",say("(?- (a b c))")).

