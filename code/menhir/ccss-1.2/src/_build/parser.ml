exception Error

type token = 
  | VAR of (string)
  | URI
  | TILDE
  | TERM_FUNC of (string)
  | STRING of (string)
  | SLASH
  | SEMICOLON
  | SEL_FUNC of (string)
  | S
  | QUOTIENT
  | QUANTITY of (Ast.quantity_t)
  | PLUS
  | PERIOD
  | PAGE
  | OPEN_SQUARE
  | OPEN_ROUND
  | OPEN_CURLY
  | NTH of (string)
  | MINUS
  | MEDIA
  | IMPORTANT
  | IMPORT
  | IDENT of (string)
  | HASH of (string)
  | GT
  | FONTFACE
  | EOF
  | DOUBLE_COLON
  | COMMA
  | COLON
  | CLOSE_SQUARE
  | CLOSE_ROUND
  | CLOSE_CURLY
  | CHARSET
  | ATTR_SUFFIX
  | ATTR_SUBSTRING
  | ATTR_PREFIX
  | ATTR_INCLUDES
  | ATTR_EQUALS
  | ATTR_DASHMATCH
  | ASTERISK

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState143
  | MenhirState141
  | MenhirState137
  | MenhirState136
  | MenhirState127
  | MenhirState125
  | MenhirState122
  | MenhirState119
  | MenhirState116
  | MenhirState115
  | MenhirState112
  | MenhirState106
  | MenhirState103
  | MenhirState100
  | MenhirState96
  | MenhirState93
  | MenhirState86
  | MenhirState84
  | MenhirState83
  | MenhirState74
  | MenhirState63
  | MenhirState61
  | MenhirState59
  | MenhirState57
  | MenhirState55
  | MenhirState51
  | MenhirState46
  | MenhirState40
  | MenhirState34
  | MenhirState32
  | MenhirState27
  | MenhirState25
  | MenhirState23
  | MenhirState21
  | MenhirState19
  | MenhirState15
  | MenhirState10
  | MenhirState8
  | MenhirState1
  | MenhirState0

  
let nelist = function
	| hd :: tl -> (hd, tl)
	| []	   -> failwith "nelist"
let _eRR =
  Error

let rec _menhir_goto_nonempty_list_rule_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.rule_t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.rule_t list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_rule_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSE_CURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, _2), _, _4) = _menhir_stack in
            let _v : (Ast.statement_t) =                                                    (`Media (_2, _4)) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.declaration_t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSE_CURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : (Ast.declaration_t list) =                                           (_2) in
            (match _menhir_s with
            | MenhirState83 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _3 = _v in
                let ((_menhir_stack, _menhir_s), _2) = _menhir_stack in
                let _v : (Ast.statement_t) =                                           (`Page (_2, _3)) in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | MenhirState122 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _2 = _v in
                let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
                let _v : (Ast.rule_t) =                                       ((_1, _2)) in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                (match _menhir_s with
                | MenhirState127 | MenhirState103 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ASTERISK ->
                        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
                    | COLON ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                    | DOUBLE_COLON ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                    | HASH _v ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
                    | IDENT _v ->
                        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
                    | OPEN_SQUARE ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                    | PERIOD ->
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                    | SEL_FUNC _v ->
                        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
                    | CLOSE_CURLY ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                        let _v : (Ast.rule_t list) =     ( [ x ] ) in
                        _menhir_goto_nonempty_list_rule_ _menhir_env _menhir_stack _menhir_s _v
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
                | MenhirState8 | MenhirState143 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
                    let _v : (Ast.statement_t) =                (`Rule _1) in
                    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    _menhir_fail ())
            | MenhirState141 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _2 = _v in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : (Ast.statement_t) =                                   (`Fontface _2) in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.declaration_t list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_declaration_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_selector_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.selector_t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState8 | MenhirState143 | MenhirState127 | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _v : (Ast.selector_t list) =                                               (_1) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | OPEN_CURLY ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.selector_t list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_selector_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_IMPORTANT_ : _menhir_env -> 'ttv_tail -> (Ast.important_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _1), _, _3), _4) = _menhir_stack in
        let _v : (Ast.declaration_t) =                                                    ((_1, _3, _4)) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | CLOSE_CURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Ast.declaration_t list) =     ( [ x ] ) in
            _menhir_goto_nonempty_list_declaration_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_combination_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.combinator_t * Ast.simplesel_t) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (Ast.selector_t) =                                     ((_1, _2)) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ASTERISK ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_startp
            | COLON ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | DOUBLE_COLON ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | HASH _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
            | IDENT _v ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
            | OPEN_SQUARE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | PERIOD ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | SEL_FUNC _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
        | OPEN_CURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Ast.selector_t list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_selector_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : ((Ast.combinator_t * Ast.simplesel_t) list) =     ( x :: xs ) in
        _menhir_goto_list_combination_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_combinator : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.combinator_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASTERISK ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | COLON ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | DOUBLE_COLON ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | HASH _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | IDENT _v ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | OPEN_SQUARE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | PERIOD ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | SEL_FUNC _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112

and _menhir_goto_separated_nonempty_list_COMMA_sentence_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expression_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState86 | MenhirState10 | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _v : (Ast.expression_t) =                                               (_1) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState15 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CLOSE_ROUND ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
                let _v : (Ast.term_t) =                                   (`Term_func (_1, _2)) in
                _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState10 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _1, _startpos__1_), _, _3) = _menhir_stack in
                let _v : (Ast.statement_t) =                                 (`Vardecl (_startpos__1_, _1, _3)) in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState86 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IMPORTANT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (Ast.important_t) =     ( true ) in
                _menhir_goto_boption_IMPORTANT_ _menhir_env _menhir_stack _v
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (Ast.important_t) =     ( false ) in
                _menhir_goto_boption_IMPORTANT_ _menhir_env _menhir_stack _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.expression_t) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_sentence_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.statement_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASTERISK ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
    | COLON ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | DOUBLE_COLON ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | FONTFACE ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | HASH _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | IDENT _v ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | IMPORT ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | MEDIA ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | OPEN_SQUARE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | PAGE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | PERIOD ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | SEL_FUNC _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | VAR _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_startp
    | EOF ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Ast.combinator_t * Ast.simplesel_t) list) =     ( [] ) in
    _menhir_goto_list_combination_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run107 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.combinator_t) =                 (`General_sibling) in
    _menhir_goto_combinator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run108 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.combinator_t) =             (`Descendant) in
    _menhir_goto_combinator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run109 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _v : (Ast.combinator_t) =                (`Adjacent_sibling) in
    _menhir_goto_combinator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run110 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.combinator_t) =              (`Child) in
    _menhir_goto_combinator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.calc_t) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OPEN_ROUND ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | QUANTITY _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | VAR _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.calc_t) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OPEN_ROUND ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | QUANTITY _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | VAR _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.calc_t) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OPEN_ROUND ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | QUANTITY _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | VAR _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.calc_t) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OPEN_ROUND ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | QUANTITY _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | VAR _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_goto_separated_nonempty_list_option_S__term_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.sentence_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let ((_menhir_stack, _menhir_s, x), _, _) = _menhir_stack in
        let _v : (Ast.sentence_t) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_option_S__term_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState86 | MenhirState10 | MenhirState40 | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _v : (Ast.sentence_t) =                                          (_1) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | HASH _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | IDENT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | OPEN_ROUND ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | QUANTITY _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | SLASH ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | STRING _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | TERM_FUNC _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | URI ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | VAR _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
        | CLOSE_ROUND | IMPORTANT | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Ast.expression_t) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_sentence_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_media_list_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.medium_t list option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _2), _, _), _, _4) = _menhir_stack in
        let _v : (Ast.statement_t) =                                             (`Import (_2, _4)) in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_S_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | HASH _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | IDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | OPEN_ROUND ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | QUANTITY _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | SLASH ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | STRING _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | TERM_FUNC _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | URI ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | VAR _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState137 in
            let _v : (Ast.medium_t list option) =     ( None ) in
            _menhir_goto_option_media_list_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
    | _ ->
        _menhir_fail ()

and _menhir_goto_simple_selector : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.simplesel_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 | MenhirState143 | MenhirState127 | MenhirState125 | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | PLUS ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
        | S ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | TILDE ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | COMMA | OPEN_CURLY ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : (Ast.combinator_t * Ast.simplesel_t) =                                   ((_1, _2)) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | PLUS ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_startp
        | S ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | TILDE ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | COMMA | OPEN_CURLY ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_qualifier_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.qualifier_t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.qualifier_t list) =     ( x :: xs ) in
        _menhir_goto_list_qualifier_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (Ast.simplesel_t) =                            (`Explicit (_1, _2)) in
        _menhir_goto_simple_selector _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_calc : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.calc_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASTERISK ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | CLOSE_ROUND ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : (Ast.calc_t) =                                    (_2) in
            _menhir_goto_calc _menhir_env _menhir_stack _menhir_s _v
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | QUOTIENT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _1), _startpos__2_), _, _3) = _menhir_stack in
        let _v : (Ast.calc_t) =                            (`Div (_startpos__2_, _1, _3)) in
        _menhir_goto_calc _menhir_env _menhir_stack _menhir_s _v
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASTERISK ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | QUOTIENT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | CLOSE_ROUND | COMMA | HASH _ | IDENT _ | IMPORTANT | MINUS | OPEN_ROUND | PLUS | QUANTITY _ | S | SEMICOLON | SLASH | STRING _ | TERM_FUNC _ | URI | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _1), _startpos__2_), _, _3) = _menhir_stack in
            let _v : (Ast.calc_t) =                        (`Sum (_startpos__2_, _1, _3)) in
            _menhir_goto_calc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _1), _startpos__2_), _, _3) = _menhir_stack in
        let _v : (Ast.calc_t) =                            (`Mul (_startpos__2_, _1, _3)) in
        _menhir_goto_calc _menhir_env _menhir_stack _menhir_s _v
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASTERISK ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | QUOTIENT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | CLOSE_ROUND | COMMA | HASH _ | IDENT _ | IMPORTANT | MINUS | OPEN_ROUND | PLUS | QUANTITY _ | S | SEMICOLON | SLASH | STRING _ | TERM_FUNC _ | URI | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _1), _startpos__2_), _, _3) = _menhir_stack in
            let _v : (Ast.calc_t) =                         (`Sub (_startpos__2_, _1, _3)) in
            _menhir_goto_calc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState86 | MenhirState10 | MenhirState15 | MenhirState40 | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASTERISK ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | QUOTIENT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | CLOSE_ROUND | COMMA | HASH _ | IDENT _ | IMPORTANT | OPEN_ROUND | QUANTITY _ | S | SEMICOLON | SLASH | STRING _ | TERM_FUNC _ | URI | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (Ast.term_t) =                (`Calc _1) in
            _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.term_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | CLOSE_ROUND | COMMA | IMPORTANT | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.sentence_t) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_option_S__term_ _menhir_env _menhir_stack _menhir_s _v
    | HASH _ | IDENT _ | OPEN_ROUND | QUANTITY _ | SLASH | STRING _ | TERM_FUNC _ | URI | VAR _ ->
        _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_goto_attr_operand : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let _v : (Ast.attr_t) =                                 (`Attr_suffix _2) in
        _menhir_goto_attr_operation _menhir_env _menhir_stack _v
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let _v : (Ast.attr_t) =                                    (`Attr_substring _2) in
        _menhir_goto_attr_operation _menhir_env _menhir_stack _v
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let _v : (Ast.attr_t) =                                 (`Attr_prefix _2) in
        _menhir_goto_attr_operation _menhir_env _menhir_stack _v
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let _v : (Ast.attr_t) =                                   (`Attr_includes _2) in
        _menhir_goto_attr_operation _menhir_env _menhir_stack _v
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let _v : (Ast.attr_t) =                                 (`Attr_equals _2) in
        _menhir_goto_attr_operation _menhir_env _menhir_stack _v
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let _v : (Ast.attr_t) =                                    (`Attr_dashmatch _2) in
        _menhir_goto_attr_operation _menhir_env _menhir_stack _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_medium_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.medium_t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState137 | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _v : (Ast.medium_t list) =                                             (_1) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState96 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | OPEN_CURLY ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | ASTERISK ->
                    _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
                | COLON ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | DOUBLE_COLON ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | HASH _v ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
                | IDENT _v ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
                | OPEN_SQUARE ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | PERIOD ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | SEL_FUNC _v ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState137 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Ast.medium_t list option) =     ( Some x ) in
            _menhir_goto_option_media_list_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.medium_t list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_medium_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_S_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) =     ( Some x ) in
    _menhir_goto_option_S_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run85 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | HASH _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | IDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | OPEN_ROUND ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | QUANTITY _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | SLASH ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | STRING _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | TERM_FUNC _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | URI ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | VAR _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_nonempty_list_qualifier_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.qualifier_t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.qualifier_t list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_qualifier_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _v : (Ast.function_t) =                     (`Qualified _1) in
        _menhir_goto_function_args _menhir_env _menhir_stack _menhir_s _v
    | MenhirState8 | MenhirState143 | MenhirState103 | MenhirState127 | MenhirState125 | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _v : (Ast.simplesel_t) =                     (`Generic (nelist _1)) in
        _menhir_goto_simple_selector _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.qualifier_t list) =     ( [] ) in
    _menhir_goto_list_qualifier_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_statement_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.statement_t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.statement_t list) =     ( x :: xs ) in
        _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _), _2), _, _3) = _menhir_stack in
            let _v : (Ast.t) =                                   ((_2, _3)) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _startpos__1_ = _startpos in
    let _v : (Ast.calc_t) =               (`Varref (_startpos__1_, _1)) in
    _menhir_goto_calc _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | STRING _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | CLOSE_ROUND ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _2) = _menhir_stack in
            let _v : (Ast.term_t) =                               (`Uri _2) in
            _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | HASH _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | IDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | OPEN_ROUND ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | QUANTITY _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | SLASH ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | STRING _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | TERM_FUNC _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | URI ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | VAR _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Ast.term_t) =                 (`String _1) in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.term_t) =                 (`Slash) in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.quantity_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Ast.calc_t) =                   (`Quantity _1) in
    _menhir_goto_calc _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OPEN_ROUND ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | QUANTITY _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | VAR _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Ast.term_t) =                 (`Ident _1) in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Ast.term_t) =                (`Hash _1) in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_function_args : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.function_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLOSE_ROUND ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : (Ast.qualifier_t) =                                          (`Sel_func (_1, _2)) in
        _menhir_goto_qualifier _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_pseudo_page_ : _menhir_env -> 'ttv_tail -> (Ast.pseudo_page_t option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OPEN_CURLY ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_goto_attr_operation : _menhir_env -> 'ttv_tail -> (Ast.attr_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLOSE_SQUARE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _2), _3) = _menhir_stack in
        let _v : (Ast.qualifier_t) =                                                    (`Attr (_2, _3)) in
        _menhir_goto_qualifier _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (string) =                 (_1) in
    _menhir_goto_attr_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (string) =                 (_1) in
    _menhir_goto_attr_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Ast.medium_t) =                 (_1) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | IDENT _v ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | OPEN_CURLY | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.medium_t list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_medium_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_source : _menhir_env -> 'ttv_tail -> (Ast.source_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | IDENT _ | SEMICOLON ->
        _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_goto_qualifier : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.qualifier_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 | MenhirState143 | MenhirState103 | MenhirState127 | MenhirState125 | MenhirState112 | MenhirState74 | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | DOUBLE_COLON ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | HASH _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | OPEN_SQUARE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PERIOD ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | SEL_FUNC _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | CLOSE_ROUND | COMMA | GT | OPEN_CURLY | PLUS | S | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Ast.qualifier_t list) =     ( [ x ] ) in
            _menhir_goto_nonempty_list_qualifier_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState116 | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | DOUBLE_COLON ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | HASH _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | OPEN_SQUARE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | PERIOD ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | SEL_FUNC _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | COMMA | GT | OPEN_CURLY | PLUS | S | TILDE ->
            _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | _ ->
        _menhir_fail ()

and _menhir_goto_element : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.element_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | DOUBLE_COLON ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | HASH _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | OPEN_SQUARE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | PERIOD ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | SEL_FUNC _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | COMMA | GT | OPEN_CURLY | PLUS | S | TILDE ->
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.statement_t list) =     ( [] ) in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | HASH _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | IDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | OPEN_ROUND ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | QUANTITY _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | SLASH ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | STRING _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | TERM_FUNC _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | URI ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | VAR _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | DOUBLE_COLON ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | HASH _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState46 in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _v : (Ast.function_t) =                 (`Nth _1) in
        _menhir_goto_function_args _menhir_env _menhir_stack _menhir_s _v
    | NTH _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState46 in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _v : (Ast.function_t) =               (`Nth _1) in
        _menhir_goto_function_args _menhir_env _menhir_stack _menhir_s _v
    | OPEN_SQUARE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | PERIOD ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | SEL_FUNC _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.qualifier_t) =                       (`Class _2) in
        _menhir_goto_qualifier _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _2 = _v in
            let _v : (Ast.pseudo_page_t) =                      (_2) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let x = _v in
            let _v : (Ast.pseudo_page_t option) =     ( Some x ) in
            _menhir_goto_option_pseudo_page_ _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | OPEN_CURLY ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (Ast.pseudo_page_t option) =     ( None ) in
        _menhir_goto_option_pseudo_page_ _menhir_env _menhir_stack _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATTR_DASHMATCH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | STRING _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | ATTR_EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | STRING _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
        | ATTR_INCLUDES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | STRING _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
        | ATTR_PREFIX ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | STRING _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
        | ATTR_SUBSTRING ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | STRING _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
        | ATTR_SUFFIX ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | STRING _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
        | CLOSE_SQUARE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (Ast.attr_t) =                      (`Attr_exists) in
            _menhir_goto_attr_operation _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run96 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | STRING _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _v : (Ast.source_t) =                 (`String _1) in
        _menhir_goto_source _menhir_env _menhir_stack _v
    | URI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | STRING _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | CLOSE_ROUND ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _2) = _menhir_stack in
                let _v : (Ast.source_t) =                               (`Uri _2) in
                _menhir_goto_source _menhir_env _menhir_stack _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run104 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Ast.element_t) =                 (`Tag _1) in
    _menhir_goto_element _menhir_env _menhir_stack _menhir_s _v

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Ast.qualifier_t) =                (`Id _1) in
    _menhir_goto_qualifier _menhir_env _menhir_stack _menhir_s _v

and _menhir_run141 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OPEN_CURLY ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.qualifier_t) =                            (`Pseudo_element _2) in
        _menhir_goto_qualifier _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.qualifier_t) =                      (`Pseudo_class _2) in
        _menhir_goto_qualifier _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run105 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _v : (Ast.element_t) =                   (`Universal) in
    _menhir_goto_element _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_option_charset_ : _menhir_env -> 'ttv_tail -> (Ast.charset_t option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASTERISK ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | COLON ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | DOUBLE_COLON ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FONTFACE ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | HASH _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | IDENT _v ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | IMPORT ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | MEDIA ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | OPEN_SQUARE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | PAGE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | PERIOD ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | SEL_FUNC _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | VAR _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v _menhir_env._menhir_startp
    | EOF ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_goto_list_S_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, xs) = _menhir_stack in
        let x = () in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_S_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CHARSET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | STRING _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | SEMICOLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _ = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _2) = _menhir_stack in
                    let _v : (Ast.charset_t) =                                 (_2) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let x = _v in
                    let _v : (Ast.charset_t option) =     ( Some x ) in
                    _menhir_goto_option_charset_ _menhir_env _menhir_stack _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | ASTERISK | COLON | DOUBLE_COLON | EOF | FONTFACE | HASH _ | IDENT _ | IMPORT | MEDIA | OPEN_SQUARE | PAGE | PERIOD | SEL_FUNC _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (Ast.charset_t option) =     ( None ) in
            _menhir_goto_option_charset_ _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_S_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | ASTERISK | CHARSET | COLON | DOUBLE_COLON | EOF | FONTFACE | HASH _ | IDENT _ | IMPORT | MEDIA | OPEN_SQUARE | PAGE | PERIOD | SEL_FUNC _ | VAR _ ->
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and stylesheet : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.t) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = 4611686018427387903;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | S ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ASTERISK | CHARSET | COLON | DOUBLE_COLON | EOF | FONTFACE | HASH _ | IDENT _ | IMPORT | MEDIA | OPEN_SQUARE | PAGE | PERIOD | SEL_FUNC _ | VAR _ ->
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



