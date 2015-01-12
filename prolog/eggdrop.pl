:-module(eggdrop,[egg_go/0,user_call/1]).

% from https://github.com/TeamSPoon/PrologMUD/tree/master/src_lib/logicmoo_util
% supplies with_assertions/2,atom_concats/2, dmsg/1, wdmsg/1, must/1, if_startup_script/0
:- '@'(ensure_loaded('../../../src_lib/logicmoo_util/logicmoo_util_all'),user).
/*
wdmsg(List):-is_list(List),text_to_string(List,CMD),!,format(user_error,'~q~n',[CMD]),flush_output(user_error),!.
wdmsg(CMD):-format(user_error,'~q~n',[CMD]),flush_output(user_error),!.
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

:-dynamic(isChattingWith/2).
:-dynamic(isRegistered/3).

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


% ===================================================================
% Deregister unsafe preds
% ===================================================================
:-use_module(library(process)).
/*
unsafe_preds(M,F,A):-M=files_ex,current_predicate(M:F/A),member(X,[delete,copy]),atom_contains(F,X).
unsafe_preds(M,F,A):-M=process,current_predicate(M:F/A),member(X,[kill,create]),atom_contains(F,X).
unsafe_preds(M,F,A):-M=system,member(F,[shell,halt]),current_predicate(M:F/A).

remove_pred(_,F,A):-member(_:F/A,[_:delete_common_prefix/4]),!.
remove_pred(M,F,A):- functor(P,F,A),
  (current_predicate(M:F/A) -> ignore((catch(redefine_system_predicate(M:P),_,true),abolish(M:F,A)));true),
  M:asserta((P:-(wdmsg(error(call(P))),throw(permission_error(M:F/A))))).

*/
deregister_unsafe_preds:-!.
deregister_unsafe_preds:-forall(unsafe_preds(M,F,A),bugger:remove_pred(M,F,A)).


% [Optionaly] Solve the Halting problem
:-redefine_system_predicate(system:halt).
:-abolish(system:halt,0).
system:halt:- format('the halting problem is now solved!').

% ===================================================================
% Eggrop interaction
% ===================================================================

:-use_module(library(socket)).
:- dynamic(stdio/3).
eggdropConnect:- ctrl_nick(SWIPL),ctrl_port(PORT),eggdropConnect(SWIPL,PORT).
eggdropConnect(CtrlNick,Port):-eggdropConnect('127.0.0.1',Port,CtrlNick,logicmoo).
eggdropConnect(Host,Port,CtrlNick,Pass):-
       tcp_socket(SocketId),
       tcp_connect(SocketId,Host:Port),
       tcp_open_socket(SocketId, IN, OutStream),
       format(OutStream,'~w\n',[CtrlNick]),flush_output(OutStream),
       format(OutStream,'~w\n',[Pass]),flush_output(OutStream),
       retractall(stdio(CtrlNick,_,_)),
       asserta((stdio(CtrlNick,IN,OutStream))),!.
		
consultation_thread(CtrlNick,Port):- 
      eggdropConnect(CtrlNick,Port),
      to_egg('.echo off\n'),
      to_egg('.console ~w ""\n',[CtrlNick]),
      must(bot_nick(PrologMUDNick)),
      to_egg('.set nick ~w\n',[PrologMUDNick]),
      must(stdio(CtrlNick,IN,_)),!,
      % loop
      repeat,
         update_changed_files_eggdrop,
         catch(read_line_to_codes(IN,Codes),_,Codes=end_of_file),    
         Codes\=end_of_file,
         once(consultation_codes(CtrlNick,Port,Codes)),
         fail.

is_callable(CMD):-var(CMD),!,fail.
is_callable((A,B)):-!,is_callable(A),is_callable(B).
is_callable((A;B)):-!,is_callable(A),is_callable(B).
is_callable(CMD):- callable(CMD),
          functor(CMD,F,A),
          current_predicate(F/A),!.

:-meta_predicate(module_call(+,0)).
module_call(M,CMD):-CALL=M:call(CMD), '@'(catch(CALL,E,(wdmsg(E:CALL),throw(E))),M).
:-meta_predicate(user_call(0)).
user_call(M:CMD):-!,show_call(module_call(M,CMD)).
user_call(CMD):-module_call('user',CMD).

consultation_codes(CtrlNick,Port,end_of_file):-!,consultation_thread(CtrlNick,Port).
consultation_codes(_BotNick,_Port,Codes):-
      text_to_string(Codes,String),
      catch(read_term_from_atom(String,CMD,[]),_E,(wdmsg(String),!,fail)),!,      
      is_callable(CMD),
      wdmsg(maybe_call(CMD)),!,
      catch(module_call('eggdrop',CMD),E,wdmsg(E:CMD)).


% IRC EVENTS Bubble from here
:-export(get2react/1).
get2react([L|IST1]):- CALL =.. [L|IST1],functor(CALL,F,A),current_predicate(F/A),module_call('eggdrop',CALL).

% ===================================================================
% IRC interaction
% ===================================================================
:- thread_local((thlocal:default_channel/1, thlocal:default_user/1)).
% IRC EVENTS FROM CALL
part(USER, HOSTAMSK,TYPE,DEST,MESSAGE):- wdmsg(notice(part(USER, HOSTAMSK,TYPE,DEST,MESSAGE))).
join(USER, HOSTAMSK,TYPE,DEST):- wdmsg(notice(join(USER, HOSTAMSK,TYPE,DEST))).
msgm(USER, HOSTAMSK,TYPE,DEST,MESSAGE):-pubm(USER, HOSTAMSK,TYPE,DEST,MESSAGE).
pubm(USER, HOSTAMSK,TYPE,DEST,MESSAGE):- 
 wdmsg(pubm(USER, HOSTAMSK,TYPE,DEST,MESSAGE)), !,
  call_in_thread((with_assertions(thlocal:default_channel(DEST),
   with_assertions(thlocal:default_user(USER),
     ircEvent(DEST,USER,say(MESSAGE)))))).



% ===================================================================
% IRC EVENTS
% ===================================================================


% convert all to strings
ignored_source(From):-var(From),!,fail.
ignored_source(From):-not(string(From)),!,text_to_string(From,String),!,ignored_source(String).
%  from bot telnet
ignored_source(From):- atom_length(From,1).
% from the process or bot
ignored_source(From):-
 bot_nick(BotNick),ctrl_nick(CtrlNick),arg(_,vv(BotNick,CtrlNick),Ignore),atom_contains(From,Ignore),!.



% ignore some inputs
ircEvent(Channel,Agent,_):- (ignored_source(Channel) ; ignored_source(Agent)) ,!.

% attention (notice the fail to disable)
ircEvent(Channel,Agent,say(W)):- fail,
               atom_contains(W,"goodbye"),!,retractall(isChattingWith(Channel,Agent)).
ircEvent(Channel,Agent,say(W)):- fail,
               (bot_nick(BotNick),atom_contains(W,BotNick)),
		retractall(isChattingWith(Channel,Agent)),!,
		asserta(isChattingWith(Channel,Agent)),!,
		say(Channel,[hi,Agent,'I will answer you in',Channel,'until you say "goodbye"']).

% Say -> Call
ircEvent(Channel,Agent,say(W)):- 
     catch(read_term_from_atom(W,CMD,[double_quotes(string),variable_names(Vs)]),_,fail),
     ircEvent(Channel,Agent,call(CMD,Vs)),!.     

% ?- Call -> call_with_results
ircEvent(Channel,Agent,call(CALL,Vs)):- nonvar(CALL),CALL = '?-'(CMD),
  show_call_failure(isRegistered(Channel,Agent,execute)), !,  
  with_no_input(while_sending_error(Channel,with_output_channel(Channel,
    catch(once(call_with_results(CMD,Vs)),E,((say(Channel,E),fail)))))),!.

% Call -> call_with_results
ircEvent(Channel,Agent,call(CMD,Vs)):- isRegistered(Channel,Agent,executeAll),
   while_sending_error(Channel,(catch((with_output_channel(Channel,
     with_no_input((CMD*->true;say(failed(CMD:Vs)))))),E,((compound(CMD),say(Agent,E),fail))))),!.

ircEvent(Channel,Agent,Method):- wdmsg(unused(ircEvent(Channel,Agent,Method))).


% ===================================================================
% IRC CALL/1
% ===================================================================
call_in_thread(CMD):- thread_self(Self),thread_create((asserta(put_server_count(0)),call_with_time_limit(10,CMD)),_,[detached(true),inherit_from(Self)]).

:- dynamic vars_as/1.
% :- thread_local vars_as/1.
vars_as(comma).

vars_as_list :- retractall(vars_as(_)),asserta(vars_as(list)).
vars_as_comma :- retractall(vars_as(_)),asserta(vars_as(comma)).

write_varvalues2(Vs):-vars_as(comma),!,write_varcommas2(Vs).
write_varvalues2(Vs):-write('['),copy_term(Vs,CVs),numbervars(CVs,6667,_,[singletons(true),attvar(skip)]),write_varvalues3(CVs).
write_varvalues3([N=V]):-format('~w=~q]',[N,V]),!.
write_varvalues3([N=V|Vs]):-format('~w=~q,',[N,V]),write_varvalues3(Vs),!.

write_varcommas2(Vs):- copy_term(Vs,CVs),numbervars(CVs,6667,_,[singletons(true),attvar(skip)]),write_varcommas3(CVs).
write_varcommas3([N=V]):-format('~w=~q',[N,V]),!.
write_varcommas3([N=V|Vs]):-format('~w=~q,',[N,V]),write_varcommas3(Vs),!.

remove_anons([],[]).
remove_anons([N=_|Vs],VsRA):-atom_concat('_',_,N),!,remove_anons(Vs,VsRA).
remove_anons([N=V|Vs],[N=V|VsRA]):-remove_anons(Vs,VsRA).

call_with_results(CMD,Vs):-remove_anons(Vs,VsRA),!,call_with_results_0(CMD,VsRA).

call_with_results_0(CMD,[]):-!, CMD *-> write(" Yes. ") ; write(" No. ").
call_with_results_0(CMD,Vs):- call_with_results_1(CMD,Vs).

call_with_results_1(CMD,Vs):- call_with_results_2(CMD,Vs) *-> 
  (deterministic(X),flag(num_sols,N,0),write(' '),write(det(X,N)),write(' Yes. ')) ;
     (deterministic(X),flag(num_sols,N,0),write(' '),write(det(X,N)),write(' No. ')).

call_with_results_2(CMDIN,Vs):- 
 b_setval('$variable_names', Vs),
   with_assertions(toplevel_variables(Vs),call_with_results_3(CMDIN,Vs)).

call_with_results_2(CMDIN,Vs):- flag(num_sols,_,0),
   strip_module(CMDIN,M,CMD),
   functor_h(CMD,F,A),A2 is A+1,
   CMD=..[F|ARGS],atom_concat(F,'_with_vars',FF),
   (M:current_predicate(FF/A2)-> (CMDWV=..[FF|ARGS],append_term(CMDWV,Vs,CCMD)); CCMD=CMD),
   M:CCMD, flag(num_sols,N,N+1), deterministic(Done),once(Done==true -> (once(write_varvalues2(Vs)),write('. ')) ; (once(write_varvalues2(Vs)),write('; '),N>28)).

with_output_channel(Channel,CMD):-  
  with_output_to_chars((CMD),Chars),!,
  % text_to_string(Chars,S),
  say(Channel,Chars).

with_io(CMD):-
  current_input(IN),
  current_output(OUT),
   call_cleanup(true,CMD,(set_input(IN),set_output(OUT))).


with_no_input(CMD):- !,
 open_chars_stream([e,n,d,'_',o,f,'_',f,i,l,e,'.'],In),set_input(In),CMD.
with_no_input(CMD):- 
 open_chars_stream([e,n,d,'_',o,f,'_',f,i,l,e,'.'],In),current_output(OUT), set_prolog_IO(In,OUT,user_error ),CMD.

ignore_catch(CALL):-ignore(catch(CALL,E,wdmsg(E:CALL))).

% while_sending_error(_Agent,CMD):- !, CMD.

while_sending_error(_Agent,CMD):- current_input(IN),current_output(OUT),show_call( set_prolog_IO(IN,OUT,OUT)),!,CMD.

while_sending_error(Agent,CMD):-  new_memory_file(MF),
 open_memory_file(MF, write, ERR),
   current_input(IN),current_output(OUT),
   wdmsg(set_prolog_IO(IN,OUT,ERR)),
     set_prolog_IO(IN,OUT,OUT),     
     wdmsg(set_prolog_IO_done(IN,OUT,ERR)),
      call_cleanup(true, CMD,
        (ignore_catch(flush_output(ERR)),ignore_catch(close(ERR)),
          	open_memory_file(MF, read, Stream,[ free_on_close(true)]),
                ignore_catch(read_codes_and_send(Stream,Agent)),
                ignore_catch(close(Stream)))).


read_codes_and_send(IN,Agent):- at_end_of_stream(IN),!,wdmsg(say(Agent,done)).
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
sayq(D):-sformat(S,'~q',[D]),!,say(S),!.

say(D):- thlocal:default_channel(C),!,say(C,D),!.
say(D):- say("#PrologMUD",D),!.

say(Channel,Data):-
	once(stdio(_Agent,_InStream,OutStream);current_output(OutStream)),
	say(OutStream,Channel,Data).

say(_,NonList,Data):-is_stream(NonList),!,say(NonList,"console",Data),!.
say(OutStream,NonList,Data):-not(is_list(NonList)),text_to_string(NonList, S),string_codes(S,Codes),!,say(OutStream,Codes,Data).
say(OutStream,Channel,Text):-any_to_string(Text,Data),
	concat_atom(List,'\n',Data),
	say_list(OutStream,Channel,List),!.

say(OutStream,Channel,Data):-
	say_list(OutStream,Channel,[Data]),!.

say(OutStream,Channel,Data):-wdmsg(say(OutStream,Channel,Data)).

say_list(_OutStream,_Channel,[]).
say_list(OutStream,Channel,[N|L]):-	
	privmsg(OutStream,Channel,N),
        flush_output(OutStream),
	say_list(OutStream,Channel,L),!.

:-thread_local put_server_count/1.
:-multifile put_server_count/1.

put_server_count(0).

check_put_server_count(Max):-retract(put_server_count(Was)),Is is Was+1,asserta(put_server_count(Is)),!,Is =< Max.
% 
privmsg(OutStream,Channel,Text):-string_codes(Text,Codes),privmsg0(OutStream,Channel,Codes).

privmsg0(OutStream,Channel,Codes):-length(Codes,Len),Len>430,length(LCodes,430),append(LCodes,RCodes,Codes),!,
   privmsg1(OutStream,Channel,LCodes),!,privmsg0(OutStream,Channel,RCodes).
privmsg0(OutStream,Channel,Codes):-privmsg1(OutStream,Channel,Codes).

privmsg1(OutStream,Channel,Text):-check_put_server_count(10)->privmsg2(OutStream,Channel,Text);true.
privmsg2(OutStream,Channel,Text):-ignore(catch(format(OutStream,'\n.msg ~s ~s\n',[Channel,Text]),_,fail)).
% privmsg0(OutStream,Channel,Text):- escape_quotes(Text,N),ignore(catch(format(OutStream,'\n.tcl putserv "PRIVMSG ~s :~s" ;  return "noerror ."\n',[Channel,N]),_,fail)),!.
putnotice(OutStream,Channel,Text):-escape_quotes(Text,N),ignore(catch(format(OutStream,'\n.tcl putserv "NOTICE ~s :~w" ;  return "noerror ."\n',[Channel,N]),_,fail)),!.

to_egg(X):-to_egg('~w',[X]),!.
to_egg(X,Y):-once(stdio(_Agent,_InStream,OutStream)),once((sformat(S,X,Y),format(OutStream,'~s\n',[S]),!,flush_output(OutStream))).


escape_quotes(I,ISO):-
                term_string(I,IS),!,
		string_to_list(IS,LIST),!,
		list_replace(LIST,92,[92,92],LISTM),
		list_replace(LISTM,34,[92,34],LISTM2),
                list_replace(LISTM2,91,[92,91],LISTO),
		text_to_string(LISTO,ISO),!.

list_replace(List,Char,Replace,NewList):-
	append(Left,[Char|Right],List),
	append(Left,Replace,NewLeft),
	list_replace(Right,Char,Replace,NewRight),
	append(NewLeft,NewRight,NewList),!.
list_replace(List,_Char,_Replace,List):-!.


% ===================================================================
% Startup
% ===================================================================

show_thread_exit:- bugger:wdmsg(warn(show_thread_exit)).

egg_go:- 
 deregister_unsafe_preds,
 (thread_property(_,alias(egg_go)) -> 
         true; 
         thread_create(consultation_thread(swipl,3334),_,[alias(egg_go),detached(true),an_exit(show_thread_exit)])).

:- if_startup_script -> egg_go ; true.
