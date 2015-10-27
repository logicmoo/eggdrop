:-module(eggdrop,[egg_go/0,ircEvent/3,call_with_results/2,close_ioe/3,
         while_sending_error/2,egg_go_fg/0,
         ignore_catch/1,
         call_in_thread/1,
         with_no_input/1,
         with_output_channel/2,
         with_input_channel_user/3,
   isRegistered/3]).


 :- meta_predicate call_with_results_3(0,*).
 :- meta_predicate call_with_results_2(0,*).
 :- meta_predicate call_with_results_0(0,*),with_resource_limit(0).

% :- use_module(library(logicmoo/util/logicmoo_util_prolog_streams),[with_output_to_stream_pred/4]).

:-module_transparent(ircEvent/3).
% from https://github.com/TeamSPoon/PrologMUD/tree/master/src_lib/logicmoo_util
% supplies w_tl/2,atom_concats/2, dmsg/1, my_wdmsg/1, must/1, if_startup_script/0
:- ensure_loaded(library(logicmoo/logicmoo_utils)).
:- use_module(logicmoo(util/logicmoo_util_strings)).
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

% ===================================================================
% IRC CONFIG
% ===================================================================
bot_nick("PrologMUD").
ctrl_nick("swipl").
ctrl_port(3334).

my_wdmsg(Msg):- string(Msg),format(user_error,'~N% ~s~N',[Msg]),flush_output(user_error),!.
my_wdmsg(Msg):- current_predicate(logicmoo_bugger_loaded/0),catch((notrace((current_main_error_stream(ERR), format(ERR,'~N% ~q.~N',[Msg]),flush_output(ERR)))),_,fail),!.
my_wdmsg(Msg):- format(user_error,'~N% ~q.~n',[Msg]),flush_output(user_error),!.

:-dynamic(isChattingWith/2).
:-dynamic(isRegistered/3).
:-thread_local thlocal:disable_mpred_term_expansions_locally.

isRegistered(Channel,Agent,kifbot):-isChattingWith(Channel,Agent).
isRegistered(_,"someluser",execute):-!,fail.
isRegistered("#ai",_,execute):-ignore(fail).
isRegistered("#pigface",_,execute):-ignore(fail).  % havent been there since 2001
isRegistered("#logicmoo",_,execute):-ignore(fail).
isRegistered("#kif",_,execute):-ignore(fail).
isRegistered("#rdfig",_,execute):-ignore(fail).
isRegistered("##prolog",_,execute):-!.
% all may execture since they are using ?-
isRegistered(_,_,execute):-!.

:- export((egg_go/0,ircEvent/3,call_with_results/2,isRegistered/3)).
% ===================================================================
% Deregister unsafe preds
% ===================================================================
:-use_module(library(process)).

unsafe_preds_egg(M,F,A):-M=files_ex,current_predicate(M:F/A),member(X,[delete,copy]),atom_contains(F,X).
%unsafe_preds_egg(M,F,A):-M=process,current_predicate(M:F/A),member(X,[kill,create]),atom_contains(F,X).
unsafe_preds_egg(M,F,A):-M=system,member(F,[shell,halt]),current_predicate(M:F/A).

:- export(remove_pred_egg/3).
remove_pred_egg(_,F,A):-member(_:F/A,[_:delete_common_prefix/4]),!.
remove_pred_egg(M,F,A):- functor(P,F,A),
  (current_predicate(M:F/A) -> ignore((catch(redefine_system_predicate(M:P),_,true),abolish(M:F,A)));true),
  M:asserta((P:-(my_wdmsg(error(call(P))),throw(permission_error(M:F/A))))).

% :-use_module(library(uid)).
% only if root
deregister_unsafe_preds:-!.
deregister_unsafe_preds:-if_defined(getuid(0),true),forall(unsafe_preds_egg(M,F,A),remove_pred_egg(M,F,A)).
deregister_unsafe_preds:-!.

% [Optionaly] Solve the Halting problem
:-redefine_system_predicate(system:halt).
:-abolish(system:halt,0).
system:halt:- format('the halting problem is now solved!').


% :- deregister_unsafe_preds.
% ===================================================================
% Eggrop interaction
% ===================================================================

:- use_module(library(socket)).
:- volatile(egg:stdio/3).
:- dynamic(egg:stdio/3).
eggdropConnect:- ctrl_nick(SWIPL),ctrl_port(PORT),eggdropConnect(SWIPL,PORT).
eggdropConnect(CtrlNick,Port):-eggdropConnect('10.10.10.198',Port,CtrlNick,logicmoo).
eggdropConnect(Host,Port,CtrlNick,Pass):-
       tcp_socket(SocketId),
       tcp_connect(SocketId,Host:Port),
       tcp_open_socket(SocketId, IN, OutStream),
       format(OutStream,'~w\n',[CtrlNick]),flush_output(OutStream),
       format(OutStream,'~w\n',[Pass]),flush_output(OutStream),
       retractall(egg:stdio(CtrlNick,_,_)),
       asserta((egg:stdio(CtrlNick,IN,OutStream))),!.

:-export(consultation_thread/2).
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

consultation_codes(CtrlNick,Port,end_of_file):-!,consultation_thread(CtrlNick,Port).
consultation_codes(_BotNick,_Port,Codes):-
      text_to_string(Codes,String),
      catch(read_term_from_atom(String,CMD,[]),_E,(my_wdmsg(String),!,fail)),!,      
      is_callable_egg(CMD),
      my_wdmsg(maybe_call(CMD)),!,
      catch(CMD,E,my_wdmsg(E:CMD)).


% IRC EVENTS Bubble from here
:- export(get2react/1).
get2react([L|IST1]):- CALL =.. [L|IST1],functor(CALL,F,A),show_failure((current_predicate(F/A),CALL)).

:-thread_local(thlocal:session_id/1).
:-multifile(thlocal:session_id/1).

% ===================================================================
% IRC interaction
% ===================================================================
:- thread_local((thlocal:default_channel/1, thlocal:default_user/1, thlocal:current_irc_receive/5 )).
% IRC EVENTS FROM CALL
part(USER, HOSTMASK,TYPE,DEST,MESSAGE):- irc_receive(USER, HOSTMASK,TYPE,DEST,part(USER, HOSTMASK,TYPE,DEST,MESSAGE)).
join(USER, HOSTMASK,TYPE,DEST):- irc_receive(USER, HOSTMASK,TYPE,DEST,join(USER, HOSTMASK,TYPE,DEST)).
msgm(USER, HOSTMASK,TYPE,DEST,MESSAGE):- irc_receive(USER, HOSTMASK,TYPE,DEST,say(MESSAGE)).
ctcp(USER, HOSTMASK,_FROM,DEST,TYPE,MESSAGE):- irc_receive(USER, HOSTMASK,TYPE,DEST,ctcp(TYPE,MESSAGE)).
pubm(USER, HOSTMASK,TYPE,DEST,MESSAGE):- irc_receive(USER, HOSTMASK,TYPE,DEST,say(MESSAGE)).


irc_receive(USER,HOSTMASK,TYPE,DEST,MESSAGE):- 
 my_wdmsg(irc_receive(USER,HOSTMASK,TYPE,DEST,MESSAGE)),!,
   string_to_atom(USER,ID),
   call_in_thread((    
     w_tl([
       thlocal:put_server_count(0),
       thlocal:default_channel(DEST),       
       thlocal:default_user(USER),
       thlocal:session_id(ID),       
       thlocal:current_irc_receive(USER, HOSTMASK,TYPE,DEST,MESSAGE)],
        with_resource_limit( (eggdrop_bind_user_streams,
           ircEvent(DEST,USER,MESSAGE)))))).
       
with_resource_limit(Call):- thread_self(main),!,rtrace((guitracer,trace,Call)).
with_resource_limit(Call):- !,Call.
with_resource_limit(Call):- nodebugx(call_with_time_limit(30,Call)).

% ===================================================================
% IRC EVENTS
% ===================================================================

:- dynamic(last_read_from/3).

% convert all to strings
ignored_source(From):-var(From),!,fail.
ignored_source("yesbot").
ignored_source(From):-not(string(From)),!,text_to_string(From,String),!,ignored_source(String).
%  from bot telnet
ignored_source(From):- atom_length(From,1).
% from the process or bot
ignored_source(From):-
 bot_nick(BotNick),ctrl_nick(CtrlNick),arg(_,vv(BotNick,CtrlNick),Ignore),atom_contains(From,Ignore),!.

:-dynamic(last_read_from_saved/4).
:-dynamic(ignored_channel/1).
:-dynamic(user:irc_event_hooks/3).
:-multifile(user:irc_event_hooks/3).

user:irc_event_hooks(_Channel,_User,_Stuff):-fail.



recordlast(Channel,User,say(What)):-!,retractall(last_read_from_saved(Channel,User,say,_)),asserta(last_read_from_saved(Channel,User,say,What)),!.
recordlast(Channel,User,What):-functor(What,F,_),retractall(last_read_from_saved(Channel,User,F,_)),asserta(last_read_from_saved(Channel,User,F,What)),!.

% awaiting some inputs
ircEvent(DEST,User,say(W)):- 
 term_to_atom(cu(DEST,User),QUEUE),
   message_queue_property(_, alias(QUEUE)),
     show_call(ircEvent,thread_send_message(QUEUE,W)).

% ignore some inputs
ircEvent(Channel,Agent,_):- (ignored_channel(Channel) ; ignored_source(Agent)) ,!.

% attention (notice the fail to disable)
ircEvent(Channel,Agent,say(W)):- fail,
               atom_contains(W,"goodbye"),!,retractall(isChattingWith(Channel,Agent)).
ircEvent(Channel,Agent,say(W)):- fail,
               (bot_nick(BotNick),atom_contains(W,BotNick)),
		retractall(isChattingWith(Channel,Agent)),!,
		asserta(isChattingWith(Channel,Agent)),!,
		say(Channel,[hi,Agent,'I will answer you in',Channel,'until you say "goodbye"']).


ircEvent(Channel,Agent,Event):-doall(call_no_cuts(user:irc_event_hooks(Channel,Agent,Event))),fail.

% Say -> Call
ircEvent(Channel,Agent,say(W)):- 
   forall(eggdrop:read_each_term_egg(W,CMD,Vs),ircEvent(Channel,Agent,call(CMD,Vs))).

% Call -> call_with_results
ircEvent(Channel,Agent,call(CALL,Vs)):- ircEvent_call_filtered(Channel,Agent,CALL,Vs).

ircEvent(Channel,User,Method):-recordlast(Channel,User,Method), my_wdmsg(unused(ircEvent(Channel,User,Method))).




:-export(unreadable/1).
unreadable(UR):-my_wdmsg(unreadable(UR)).

:-export(read_each_term_egg/3).
:-module_transparent(read_each_term_egg/3).
read_each_term_egg(S,CMD,Vs):-   
  show_failure(( l_open_input(S,Stream),  
      findall(CMD-Vs,(
       repeat,
       read_one_term_egg(Stream,CMD,Vs),
       (CMD==end_of_file->!;true)),Results),!,
  ((member(CMD-Vs,Results),CMD\==end_of_file)*->true;read_one_term_egg(S,CMD,Vs)))).

%:- ensure_loaded(library(logicmoo/snark/common_logic_sexpr)).
%:- ensure_loaded(library(clpfd)).




:-export(eggdrop_bind_user_streams/0).
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
	thread_at_exit(close_ioe(In, Out, Err)))).

:- use_module(library(pengines)).

:- meta_predicate while_sending_error(?,0).
:- meta_predicate ignore_catch(0).
:- meta_predicate call_in_thread(0).
:- meta_predicate with_no_input(0).
:- meta_predicate with_output_channel(?,0).
:- meta_predicate with_input_channel_user(?,?,0).



eggdrop_io:stream_write(_Stream, Out) :- thlocal:default_channel(RETURN),say(RETURN,Out).
eggdrop_io:stream_read(_Stream, "") :- !.
eggdrop_io:stream_read(_Stream, Data) :- prompt(Prompt, Prompt), pengines:pengine_input(_{type:console, prompt:Prompt}, Data).
eggdrop_io:stream_close(_Stream).

eggdrop_e:stream_write(_Stream, Out) :- thlocal:default_channel(RETURN),say(RETURN,Out).
eggdrop_e:stream_read(_Stream, "") :- !.
eggdrop_e:stream_read(_Stream, Data) :- prompt(Prompt, Prompt), pengines:pengine_input(_{type:console, prompt:Prompt}, Data).
eggdrop_e:stream_close(_Stream).

close_ioe(In, Out, Err) :-
	close(In, [force(true)]),
        close(Err, [force(true)]),
	close(Out, [force(true)]).




:-export(read_one_term_egg/3).
:-module_transparent(read_one_term_egg/3).
read_one_term_egg(Stream,CMD,Vs):- \+ is_stream(Stream),l_open_input(Stream,InStream),!, 
       with_stream_pos(InStream,show_entry(read_one_term_egg(InStream,CMD,Vs))).
read_one_term_egg(Stream,CMD,_ ):- at_end_of_stream(Stream),!,CMD=end_of_file,!.
read_one_term_egg(Stream,CMD,Vs):- catch((logicmoo_i_sexp_reader:input_to_forms(Stream,CMD,Vs)),_,fail),CMD\==end_of_file,!.
read_one_term_egg(Stream,CMD,Vs):- catch((read_term(Stream,CMD,[double_quotes(string),module(clpfd),variable_names(Vs)])),_,fail),CMD\==end_of_file,!.
read_one_term_egg(Stream,unreadable(String),_):-catch((read_stream_to_codes(Stream,Codes),string_codes(String,Codes)),_,fail),!.
read_one_term_egg(Stream,unreadable(String),_):-catch((read_pending_input(Stream,Codes,[]),string_codes(String,Codes)),_,fail),!.


:-export(add_maybe_static/2).
add_maybe_static( H,Vs):- H \= (_:-_), !,add_maybe_static((H:-true),Vs).
add_maybe_static((H:-B),_Vs):- predicate_property(H,dynamic),!,assertz(((H:-B))).
add_maybe_static((H:-B),_Vs):- must_det_l((convert_to_dynamic(H),assertz(((H:-B))),functor(H,F,A),compile_predicates([F/A]))).

% ===================================================================
% IRC CALL/1
% ===================================================================
:-module_transparent(ircEvent_call_filtered/4).
:-export(ircEvent_call_filtered/4).
ircEvent_call_filtered(_Channel,_Agent,CALL,_Vs):-var(CALL),!.
ircEvent_call_filtered(_Channel,_Agent,end_of_file,_Vs):-!.
ircEvent_call_filtered(_Channel,_Agent,(H :- B ),Vs):- user:add_maybe_static((H :- B),Vs),!.
ircEvent_call_filtered(Channel,Agent,((=>(H)) :- B ),Vs):- ((=>(H :- B)) \== ((=>(H)) :- B )),!,ircEvent_call_filtered(Channel,Agent,(=>(H :- B)),Vs).
ircEvent_call_filtered(Channel,Agent,'?-'(CALL),Vs):- nonvar(CALL),!,ircEvent_call(Channel,Agent,CALL,Vs),!.
ircEvent_call_filtered(Channel,Agent,'=>'(CALL),Vs):- nonvar(CALL),!,ircEvent_call(Channel,Agent,pfc_add(CALL),Vs),!.
ircEvent_call_filtered(Channel,Agent,[S|TERM],Vs):- is_list([S|TERM]),is_lisp_call_functor(S),!,
   (current_predicate(lisp_call/3) -> ircEvent_call(Channel,Agent,lisp_call([S|TERM],Vs,R),['Result'=R|Vs]);
     my_wdmsg(cant_ircEvent_call_filtered(Channel,Agent,[S|TERM],Vs))).
ircEvent_call_filtered(Channel,Agent,CALL,Vs):- isRegistered(Channel,Agent,executeAll),!,ircEvent_call(Channel,Agent,CALL,Vs),!.
ircEvent_call_filtered(Channel,Agent,CALL,Vs):- my_wdmsg(unused_ircEvent_call_filtered(Channel,Agent,CALL,Vs)),!. 

is_lisp_call_functor('?-').
is_lisp_call_functor('?>').


:-module_transparent(ircEvent_call/4).
:-export(ircEvent_call/4).
ircEvent_call(Channel,Agent,CALL,Vs):-
  my_wdmsg(do_ircEvent_call(Channel,Agent,CALL,Vs)),
  show_failure((with_no_input(while_sending_error(Channel,with_output_channel(Channel,
    '@'(catch(once(show_call(call_with_results(CALL,Vs))),E,((say(Channel,[Agent,': ',E]),fail))),user)))))),!.


% call_in_thread(CMD):- !,CMD.
call_in_thread(CMD):- thread_self(main),!,CMD.
call_in_thread(CMD):- thread_self(Self),thread_create(CMD,_,[detached(true),inherit_from(Self)]).


:- dynamic(egg:vars_as/1).
% :- thread_local egg:vars_as/1.
egg:vars_as(comma).



:-export(vars_as_list/0).
vars_as_list :- retractall(egg:vars_as(_)),asserta(egg:vars_as(list)).
:-export(vars_as_comma/0).
vars_as_comma :- retractall(egg:vars_as(_)),asserta(egg:vars_as(comma)).

format_nv(N,V):- format('~w=',[N]),((var(V),var_property(V,name(EN))->write(EN);writeq(V))).

:-export(write_varvalues2/1).
write_varvalues2(Vs):-egg:vars_as(comma),!,write_varcommas2(Vs).
write_varvalues2(Vs):-write('['),copy_term(Vs,CVs),numbervars(CVs,6667,_,[singletons(true),attvar(skip)]),write_varvalues3(CVs).
write_varvalues3([N=V]):- format_nv(N,V), write(']'),!.
write_varvalues3([N=V|Vs]):-format_nv(N,V),write(','),write_varvalues3(Vs),!.

write_varcommas2(Vs):- copy_term(Vs,CVs),numbervars(CVs,6667,_,[singletons(true),attvar(skip)]),write_varcommas3(CVs).
write_varcommas3([N=V]):-format_nv(N,V),!.
write_varcommas3([N=V|Vs]):-format_nv(N,V), write(','),!,write_varcommas3(Vs),!.



:-export(remove_anons/2).
remove_anons([],[]).
remove_anons([N=_|Vs],VsRA):-atom_concat('_',_,N),!,remove_anons(Vs,VsRA).
remove_anons([N=V|Vs],[N=V|VsRA]):-remove_anons(Vs,VsRA).

:-module_transparent(call_with_results/2).
:-export(call_with_results/2).
call_with_results(CMDI,Vs):- remove_anons(Vs,VsRA),!,
 w_tl(thlocal:disable_mpred_term_expansions_locally, 
  expand_term(CMDI,CMDG)),
   expand_goal(CMDG,CMD),
    show_call(call_with_results_0(CMD,VsRA)).

:-module_transparent(call_with_results_0/2).
:-export(call_with_results_0/2).
call_with_results_0(CMD,Vs):- 
 b_setval('$variable_names', Vs),
 flag(num_sols,_,0),
 (call_with_results_2(CMD,Vs) *-> 
  (deterministic(X),flag(num_sols,N,0),(N\==0->YN='Yes';YN='No'), write(' '),(X=true->write(det(YN,N));write(nondet(YN,N)))) ;
     (deterministic(X),flag(num_sols,N,0),(N\==0->YN='Yes';YN='No'),write(' '),(X=true->write(det(YN,N));write(nondet(YN,N))))).



:-module_transparent(call_with_results_2/2).
:-export(call_with_results_2/2).
call_with_results_2(CMDIN,Vs):-  
   CMDIN = CMD,functor_h(CMD,F,A),A2 is A+1,CMD=..[F|ARGS],atom_concat(F,'_with_vars',FF),
   (current_predicate(FF/A2)-> (CMDWV=..[FF|ARGS],append_term(CMDWV,Vs,CCMD)); CCMD=CMD),!,
   call_with_results_3(CCMD,Vs).
call_with_results_2(CCMD,Vs):- call_with_results_3(CCMD,Vs).

:-module_transparent(call_with_results_3/2).
:-export(call_with_results_3/2).
call_with_results_3(CCMD,Vs):-
   show_call(CCMD), flag(num_sols,N,N+1), deterministic(Done),
     (once((Done==true -> (once(write_varvalues2(Vs)),write('. ')) ; (once(write_varvalues2(Vs)),write('; '),N>28)))).

:-export(with_output_channel/2).
:-module_transparent(with_output_channel(+,0)).
with_output_channel(Channel,CMD):- 
  with_output_to_pred(say(Channel),CMD).


with_input_channel_user(_,_,CMD):- !, with_no_input(CMD).
with_input_channel_user(Channel,User,CMD):- 
  with_input_from_pred(last_read_from(Channel,User),CMD).

:-export(with_io/1).
:-meta_predicate(with_io(0)).
with_io(CMD):-
  current_input(IN),
  current_output(OUT),
  set_prolog_IO(IN,OUT,OUT),
   call_cleanup(true,CMD,(set_input(IN),set_output(OUT))).

with_no_input(CMD):-  !,CMD.
with_no_input(CMD):-  open_chars_stream([e,n,d,'_',o,f,'_',f,i,l,e,'.'],In),set_input(In),!,CMD.
with_no_input(CMD):- open_chars_stream([e,n,d,'_',o,f,'_',f,i,l,e,'.'],In),current_output(OUT), set_prolog_IO(In,OUT,user_error ),CMD.



ignore_catch(CALL):-ignore(catch(CALL,E,my_wdmsg(E:CALL))).

while_sending_error(_Agent,CMD):- !, CMD.

while_sending_error(Agent,CMD):- % current_input(IN),current_output(OUT),show_call( set_prolog_IO(IN,OUT,OUT)),!,
  with_output_to_pred(say(Agent),CMD).
/*
while_sending_error(Agent:PREFIX,CMD):-!,while_sending_error(Agent,PREFIX,CMD).
while_sending_error(Agent,CMD):-while_sending_error(Agent,[],CMD).
while_sending_error(Agent,_PREFIX,CMD):-  new_memory_file(MF),
 open_memory_file(MF, write, ERR),
   current_input(IN),current_output(OUT),
   my_wdmsg(set_prolog_IO(IN,OUT,ERR)),
     set_prolog_IO(IN,OUT,OUT),     
     my_wdmsg(set_prolog_IO_done(IN,OUT,ERR)),!,
      call_cleanup(true, CMD,
        (ignore_catch(flush_output(ERR)),ignore_catch(close(ERR)),
          	open_memory_file(MF, read, Stream,[ free_on_close(true)]),
                ignore_catch(read_codes_and_send(Stream,Agent)),
                ignore_catch(close(Stream)))).
*/

read_codes_and_send(IN,Agent):- at_end_of_stream(IN),!,my_wdmsg(say(Agent,done)).
read_codes_and_send(IN,Agent):- repeat,read_line_to_string(IN,Codes),say(Agent,Codes),at_end_of_stream(IN),!.

%:-servantProcessCreate(killable,'Consultation Mode Test (KIFBOT!) OPN Server',consultation_thread(swipl,3334),Id,[]).

update_changed_files_eggdrop :-
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
	).

  
% ===================================================================
% IRC OUTPUT
% ===================================================================
:-export(sayq/1).
sayq(D):-sformat(S,'~q',[D]),!,say(S),!.

:-export(say/1).
say(D):- thlocal:default_channel(C),say(C,D),!.
say(D):- say("#logicmoo",D),!.

:- export(say/2).
say(Channel,[Channel,': '|Data]):-nonvar(Data),say(Channel,Data),!.
say(Channel,Data):-
	once(egg:stdio(_Agent,_InStream,OutStream);current_output(OutStream)),
	say(OutStream,Channel,Data),!.

:-export(say/3).
say(_,NonList,Data):-is_stream(NonList),!,say(NonList,"console",Data),!.

say(_OutStream,Channel,Text):-my_wdmsg(will_say(Channel,Text)),fail.
say(OutStream,NonList,Data):- \+(is_list(NonList)),text_to_string_safe(NonList, S),string_codes(S,Codes),!,say(OutStream,Codes,Data),!.
say(OutStream,Channel,Text):-
   any_to_string(Text,Data),
	concat_atom(List,'\n',Data),
	say_list(OutStream,Channel,List),!.

say(OutStream,Channel,Data):-
	say_list(OutStream,Channel,[Data]),!.

say(OutStream,Channel,Data):-my_wdmsg(say(OutStream,Channel,Data)),!.

get_session_prefix(ID):-thlocal:session_id(ID),!.
get_session_prefix(ID):-thlocal:default_user(ID),!.
get_session_prefix('').

% say_list(_OutStream,Channel,Text):-my_wdmsg(say_list(Channel,Text)),fail.
say_list(OutStream,Channel,List):-
  get_session_prefix(Prefix),!,
  say_list(OutStream,Channel,Prefix,List),!.

is_empty(A):-any_to_string(A,S),string_length(S,0).

flushed_privmsg(OutStream,Channel,Fmt,Args):- 
  format(string(NS),Fmt,Args),
  privmsg(OutStream,Channel,NS),!,
  catch(flush_output(OutStream),_,true).

say_list(OutStream,Channel:Prefix,_ID,List):-nonvar(Channel),!,say_list(OutStream,Channel,Prefix,List).
say_list(_OutStream,_Channel,_Prefix,[]).

say_list(OutStream,Channel,Prefix,[S|L]):- atom_string(N,S),atom_concat('\t',Front,N),atom_concat('   ',Front,NEW),!,say_list(OutStream,Channel,Prefix,[NEW|L]).

say_list(OutStream,Channel,Prefix,[N|L]):- any_to_string(Prefix,S),any_to_string(Channel,S),!,
        flushed_privmsg(OutStream,Channel,'~w',[N]),        
	say_list(OutStream,Channel,Prefix,L),!.
say_list(OutStream,Channel,Prefix,[N|L]):-!,
   flushed_privmsg(OutStream,Channel,'~w: ~w',[Prefix,N]),
	say_list(OutStream,Channel,Prefix,L),!.

:-thread_local thlocal:put_server_count/1.
:-multifile thlocal:put_server_count/1.
:-thread_local thlocal:put_server_no_max/0.
:-multifile thlocal:put_server_no_max/0.

thlocal:put_server_count(0).

check_put_server_count(0):- if_defined(thlocal:put_server_no_max),retractall(thlocal:put_server_count(_)),asserta(thlocal:put_server_count(0)).
check_put_server_count(Max):-retract(thlocal:put_server_count(Was)),Is is Was+1,asserta(thlocal:put_server_count(Is)),!,Is =< Max.
% 
privmsg(OutStream,Channel,Text):- string_codes(Text,Codes),privmsg0(OutStream,Channel,Codes).

privmsg0(OutStream,Channel,Codes):-length(Codes,Len),Len>430,length(LCodes,430),append(LCodes,RCodes,Codes),!,
   privmsg1(OutStream,Channel,LCodes),!,privmsg0(OutStream,Channel,RCodes).
privmsg0(OutStream,Channel,Codes):-privmsg1(OutStream,Channel,Codes).

privmsg1(OutStream,Channel,Text):-check_put_server_count(30)->privmsg2(OutStream,Channel,Text);ignore(check_put_server_count(100)->privmsg_session(OutStream,Channel,Text);true).


privmsg2(OutStream,Channel:_,Text):-nonvar(Channel),!,privmsg2(OutStream,Channel,Text).
privmsg2(OutStream,_:Channel,Text):-nonvar(Channel),!,privmsg2(OutStream,Channel,Text).
privmsg2(OutStream,Channel,Text):- sleep(0.2),escape_quotes(Text,N),!,on_f_log_ignore(format(OutStream,'\n.tcl putquick "PRIVMSG ~s :~s"\n',[Channel,N])),!.
% privmsg2(OutStream,Channel,Text):-on_f_log_ignore(format(OutStream,'\n.msg ~s ~s\n',[Channel,Text])).

% privmsg2(OutStream,Channel,Text):- escape_quotes(Text,N),ignore(catch(format(OutStream,'\n.tcl putserv "PRIVMSG ~s :~s" ;  return "noerror ."\n',[Channel,N]),_,fail)),!.

putnotice(OutStream,Channel,Text):-escape_quotes(Text,N),ignore(catch(format(OutStream,'\n.tcl putserv "NOTICE ~s :~w"\n',[Channel,N]),_,fail)),!.

privmsg_session(OutStream,Channel,Text):- thlocal:session_id(ID),(ID==Channel->privmsg2(OutStream,Channel,Text);privmsg2(OutStream,ID,Text)).


to_egg(X):-to_egg('~w',[X]),!.
to_egg(X,Y):-once(egg:stdio(_Agent,_InStream,OutStream)),once((sformat(S,X,Y),format(OutStream,'~s\n',[S]),!,flush_output(OutStream))).


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

list_replace_egg(List,Char,Replace,NewList):-
	append(Left,[Char|Right],List),
	append(Left,Replace,NewLeft),
	list_replace_egg(Right,Char,Replace,NewRight),
	append(NewLeft,NewRight,NewList),!.
list_replace_egg(List,_Char,_Replace,List):-!.


% ===================================================================
% Startup
% ===================================================================

show_thread_exit:- my_wdmsg(warn(eggdrop_show_thread_exit)).

egg_go_fg:-consultation_thread(swipl,3334).

egg_go:- 
 deregister_unsafe_preds,
 (thread_property(_,alias(egg_go)) -> 
         true; 
         thread_create(egg_go_fg,_,[alias(egg_go),detached(true),an_exit(show_thread_exit)])).

/*
:- source_location(S,_),forall(source_file(H,S),ignore(( ( \+predicate_property(H,PP),member(PP,[(multifile),built_in]) ),  
 functor(H,F,A),module_transparent(F/A),export(F/A),user:import(H)))).
*/


  
% :-asserta(user:irc_user_plays(_,dmiles,dmiles)).

% :- if_startup_script -> egg_go ; true.

:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).

% :- ircEvent("dmiles_afk","dmiles_afk",say("(?- (a b c))")).

