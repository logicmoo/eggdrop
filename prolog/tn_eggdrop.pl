
end_of_file.


telnet_server_io(_Port, _Options) :- thread_property(X, status(running)),X=telnet_server_io,!.
telnet_server_io(Port, Options) :-
	tcp_socket(ServerSocket),
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5),
	thread_create(server_loop_io(ServerSocket, Options), _,
		      [ alias(telnet_server_io)
		      ]).

server_loop_io(ServerSocket, Options) :-
	tcp_accept(ServerSocket, Slave, Peer),
	tcp_open_socket(Slave, In, Out),
	set_stream(In, close_on_abort(false)),
	set_stream(Out, close_on_abort(false)),
	catch(tcp_host_to_address(Host, Peer),_,Host = Peer),
	atomic_list_concat(['client@', Host,'-'], AliasH),
        gensym(AliasH,Alias),
	catch(thread_create(
		  service_client_io(Slave, In, Out, Host, Peer, Options),
		  ThreadID,
		  [ 
                      alias(Alias)
                     % detached(true)
		  ]),
	      error(permission_error(create, thread, Alias/ThreadID), _),
	      fail), !,
	server_loop_io(ServerSocket, Options).


call_close_and_detatch_io(In, Out, Id, Call):-         
               setup_streams_io(In, Out),
                 call_cleanup(call(Call),
		     ( close_connection_io(In, Out),
		       ignore(thread_detach(Id))
                       )),!.
 
read_call:-repeat,catch(read_term(X,[]),E,dmsg(E)),(X=end_of_file->true;(catch(call(X),E,dmsg(E:X)),format('~q.~n',[X]),flush_output)),X=end_of_file.

close_connection_io(In, Out) :-
        retractall(thread_util:has_console(_,In,Out,_)),
        ignore(catchv(close(In, [force(true)]),_,true)),
        ignore(catchv(close(Out, [force(true)]),_,true)).

setup_streams_io(In, Out):-
     set_prolog_IO(In, Out, Out),
        set_stream(In, tty(true)),
        set_stream(Out, tty(true)),
        set_prolog_flag(tty_control, true),       
	current_prolog_flag(encoding, Enc),
	set_stream(user_input, encoding(Enc)),
	set_stream(user_output, encoding(Enc)),
	set_stream(user_error, encoding(Enc)),
	set_stream(user_input, newline(detect)),
	set_stream(user_output, newline(dos)),
	set_stream(user_error, newline(dos)),!.
  

service_client_io(_Slave, In, Out, Host, Peer, Options) :-
	allow_io(Peer, Options), !,
         call_pred_io(Call, Options), !,
         setup_streams_io(In, Out),
         thread_self(Id),
     %    'format'(Out,'% Welcome ~q to the SWI-Prolog LogicMOO server on thread ~w~n~n', [Peer,service_client_io(Slave, In, Out, Host, Peer, Options)]),
	call_close_and_detatch_io(In, Out, Id, call_close_and_detatch_io(In, Out, Host:Peer, Call)),!.

service_client_io(_Slave, In, Out, Host, Peer, _Options):- 
   thread_self(Id),
    call_close_and_detatch_io(In, Out, Id,'format'(Out, 'Go away ~q!!~n', [Host:Peer])).

allow_io(Peer, Options) :-
	(   member(allow_io(Allow), Options)
	*-> Peer = Allow,
	    !
	;   Peer = ip(127,0,0,1)
	).

call_pred_io(Call, Options) :-
	(   member(call_pred_io(Allow), Options)
	*-> Call = Allow,
	    !
	;   Call = prolog
	).

:- telnet_server_io(33341, [call_pred_io(read_call)]).


