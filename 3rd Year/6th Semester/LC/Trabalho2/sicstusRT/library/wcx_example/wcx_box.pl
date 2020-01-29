
%% See Section "A Sample WCX Box" in the SICStus Prolog Manual

foreign(wcx_set_encoding, wcx_set_encoding(+integer, +atom)).
foreign_resource(wcx_box, [init(wcx_init),deinit(wcx_deinit), wcx_set_encoding]).

:- load_foreign_resource(wcx_box).

set_encoding(Stream, Enc) :-
	stream_code(Stream, StreamCode),
	wcx_set_encoding(StreamCode, Enc).
